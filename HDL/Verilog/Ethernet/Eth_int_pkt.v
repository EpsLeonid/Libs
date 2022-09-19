//-----------------------------------------------------------------------------
// Title       : Eth_int_pkt
//-----------------------------------------------------------------------------
// File        : Eth_int_pkt.v
// Company     : INP SB RAS
// Created     : 12/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_int_pkt
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_int_pkt (
//-----------------------------------------------------------------------------
// Libraries
//-----------------------------------------------------------------------------
//`include "Eth_parameters.v"
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire			Clock, //-- System Clock, really Bus_Clock

	input		wire			Int_Start, // -- start parcer
	output	wire			Eth_Tx_End, // -- ????? ???????? ?? ?????? ????????

	output	wire			Eth_RxTx_In_Progress,

	output	wire [7:0]	Tx_Data_for_FIFO,
	output	reg			Tx_Strobe_for_FIFO,
	output	reg			Tx_End_Pkt_for_FIFO,
	output	wire			Tx_packet_RQ,
	input		wire			Tx_packet_AG,

	input		wire			Out_FIFO_full,

	//-- Standard bus connections
	input		wire			BUS_Clock,
	input		wire [15:0]	DataBus_In,
	output	reg [15:0]	DataBusOut,

	input		wire			DataBusStrobe,
	input		wire			Select_i,
	input		wire			DirectIn,
	input		wire [12:0]	AddrBus_In,
	input		wire			Reset,
	//-- Master Mode Signals 
	output	wire			AccessRequest,
	input		wire			AccessGranted,
	output	wire			DirectOut,
	output	wire [15:0]	AddrBusOut
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------
//	`define CLOG2_CORE(N)  \
//		((N)&'h8000_0000 ?32:((N)&'h4000_0000 ?31:((N)&'h2000_0000 ?30:((N)&'h1000_0000 ?29: \
//		((N)&'h800_0000 ?28:((N)&'h400_0000 ?27:((N)&'h200_0000 ?26:((N)&'h100_0000 ?25: \
//		((N)&'h80_0000 ?24:((N)&'h40_0000 ?23:((N)&'h20_0000 ?22:((N)&'h10_0000 ?21: \
//		((N)&'h8_0000 ?20:((N)&'h4_0000 ?19:((N)&'h2_0000 ?18:((N)&'h1_0000 ?17: \
//		((N)&'h8000 ?16:((N)&'h4000 ?15:((N)&'h2000 ?14:((N)&'h1000 ?13: \
//		((N)&'h800 ?12:((N)&'h400 ?11:((N)&'h200 ?10:((N)&'h100 ?9: \
//		((N)&'h80 ?8:((N)&'h40 ?7:((N)&'h20 ?6:((N)&'h10 ?5: \
//		((N)&'h8 ?4:((N)&'h4 ?3:((N)&'h2 ?2: \
//		((N)&'h1))))))))))))))))))))))))))))))))	//"Core Ceil(LOG2(N+1))" for correctly defined 
//						//values (<= 32 bits). Both for synthesis and not; bit selection is not
//						//used since N could be an expression.
//	`define Clog2(N)  ((!`ISDEF(N) || (N) <= 0) ? 'hx : `CLOG2_CORE((N)-1)) //"Ceil(LOG2(N))"
//						//ONLY FOR ARGUMENTS <= 32 BITS! Ceil (nearest greater or equal integer) of
//						//binary logarithm.	parameter PacketLenghts_at_signaling_layer_ext = 4096;//--2048;-- maximum length value in bytes

	parameter PacketLenghts_at_signaling_layer_int = 4096;//--2048;-- maximum length value in bytes
	parameter RxByte_Cnt_Width_int = 12;//clog2(PacketLenghts_at_signaling_layer_ext);

	parameter Eth_WORD_WIDTH		 = 16; 

	parameter ETH_HEADER_LENGTH	 = 14;
	parameter IP_HEADER_LENGTH		 = 20;
	parameter UDP_HEADER_LENGTH	 = 8;
	parameter OPCODE_LENGTH			 = 2;

	parameter KLUKVA_DATA_LENGTH	 = ((32+512+32)*2)*2; //--2304;
	parameter HEADER_LENGTH_BYTES	 = ETH_HEADER_LENGTH + IP_HEADER_LENGTH + UDP_HEADER_LENGTH + OPCODE_LENGTH;
	parameter HEADER_LENGTH_WORDS	 = HEADER_LENGTH_BYTES / 2;
	parameter MASS_RAM_BYTE_Tx_Num = HEADER_LENGTH_BYTES + KLUKVA_DATA_LENGTH;
//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------
	wire 				 RxReadyToRecive_o;
	wire 				 SetRxReadyToRecive;
	reg 				 SetRxReadyToRecive_Sc_Bus;
	wire 				 SetRxReadyToRecive_Parcer;
	
	wire [7:0]		 RxRAM_q_a;
	wire [15:0]		 RxRAM_q_b;
	reg 				 RxRAM_CS;
	
	wire 				 ParcerCycle_SRFF_o;
	
	wire [15:0]		 Buffer_RAM_Data;
	
	reg [15:0]		 DataBusOut_tmp;
	
	wire 				 Tx_Packet_Cnt_o;
	reg 				 Tx_Packet_Cnt_Reset;
	
	wire 				 Tx_packet_ready;
	
	reg 				 TxRAM_wren;
	wire 				 TxRAM_CS;
	
	wire 				 Status_REG_o;
	reg 				 Status_REG_CS;
	wire 				 Status_REG_ES_o;
	
	wire [2:0]		 Module_MAC_Reg_o[15:0];
	reg [2:0]		 Module_MAC_Reg_CS;
	wire [1:0]		 Module_IP_Reg_o[15:0];
	reg [1:0]		 Module_IP_Reg_CS;
	
	reg [Eth_WORD_WIDTH-1:0]			Port_Reg_o;
	reg 				 Port_Reg_CS;
	
	reg [RxByte_Cnt_Width_int-2:0]	Pkt_parcer_Rx_Addr_o;
	reg [RxByte_Cnt_Width_int-2:0]	Pkt_parcer_Tx_Addr;
	reg [Eth_WORD_WIDTH-1:0]			Pkt_parcer_Tx_Data;
	wire 										Pkt_parcer_Tx_Strobe;
	reg [15:0]								Pkt_parcer_DataBusOut;
	reg [RxByte_Cnt_Width_int-2:0]	Tx_RAM_Address_Bus;
	reg [RxByte_Cnt_Width_int-2:0]	Rx_RAM_Address_Bus;
	reg [Eth_WORD_WIDTH-1:0]			Tx_RAM_Data_Bus;
	
	reg [Eth_WORD_WIDTH-1:0]			PacketLenghts_DataBus;
	reg 				 PacketLenghts_to_be_transmitted_Reg_EN;
	reg [15:0]		 PacketLenghts_to_be_transmitted_Reg_o;
	reg 				 PacketLenghts_to_be_transmitted_Reg_CS;
	
	wire 				 Test_Reg_o;
	reg 				 Test_Reg_CS;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
//	-------------------------------------------- Rx section --------------------------------------------
//--	RxReadyToRecive.(S,clk,R) = (SetRxReadyToRecive, Clock, RAM_Ptr_Packet_End | RxIntStart_ES.q);
	SRFF RxReadyToRecive_SR
	 (
		.S		(SetRxReadyToRecive),
		.CLK	(Clock),
		.R		(RAM_Ptr_Packet_End | RxIntStart_ES_o),
		.q		(RxReadyToRecive_o)
		);

	assign SetRxReadyToRecive = SetRxReadyToRecive_Sc_Bus | SetRxReadyToRecive_Parcer;
	assign Eth_RxTx_In_Progress = (!RxReadyToRecive_o);

	assign RAM_Ptr_Packet_End = Int_Start;

	RAM2048_2p RxRAM
	(
		.dina		({8{1'b0}}),
		.addra	({11{1'b0}}),
		.clka		(Clock),
		.wea		(1'b0),
		.douta	(RxRAM_q_a),
		
		.dinb		({DataBus_In[7:0],DataBus_In[15:8]}),
		.addrb	(Rx_RAM_Address_Bus),
		.clkb		(BUS_Clock),
		.web		(RxRAM_CS & DataBusStrobe & DirectIn & Select_i),
		.doutb	(RxRAM_q_b)
		);
	
// ------------------------------------------- Tx section -------------------------------------------------   
	// condition to start sending a response from the Tx buffer
	assign Tx_packet_RQ  = Tx_packet_ready;
//	assign AnswerTxStart = Tx_packet_AG;
	
	SRFF TxRQ_SRFF
	(
		.S		(Tx_packet_AG),
		.CLK	(Clock),
		.R		(TxRQ_Reset_ES_o | Reset),
		.q		(TxRQ_SRFF_o)
		);
	
	V_Counter
	#(
		.width (RxByte_Cnt_Width_int+1)
	) TxByte_Cnt
	(
		.clock	(Clock),
		.clk_en	(1'b1),
		.cnt_en	(TxRQ_SRFF_o & !Out_FIFO_f),
		.sclr		(Tx_packet_AG),
		.q			(TxByte_Cnt_o)
		);
	assign TxRQ_Reset = ((TxByte_Cnt_o >= PacketLenghts_at_signaling_layer_int-1) | (TxByte_Cnt_o >= PacketLenghts_to_be_transmitted_Reg_o[RxByte_Cnt_Width_int:0])) ? 1'b1 : 1'b0;

//--	TxRQ_Reset_ES.(d,clk) = (TxRQ_Reset,Clock);
	Edge_Sensing TxRQ_Reset_ES
	(
		.clk	(BUS_Clock),
		.d		(TxRQ_Reset),
		.q		(TxRQ_Reset_ES_o)
		);

//--	TxRAM.data_a(7:0) = GND;  
//--	TxRAM.(address_a(RxByte_Cnt_Width_int-1:0)   , clock_a, wren_a) =
//--		(TxByte_Cnt.q(RxByte_Cnt_Width_int-1:0], Clock  , GND   ); 
//--
//--	TxRAM.(address_b(RxByte_Cnt_Width_int-2:0)         , clock_b  , data_b()         , wren_b  ) =
//--		(Tx_RAM_Address_Bus(RxByte_Cnt_Width_int-2:0], BUS_Clock, Tx_RAM_Data_Bus[], TxRAM_wren);
	EthBufferRAM2048 TxRAM
	(
		dina		({8{1'b0}}),
		addra		(TxByte_Cnt_o[RxByte_Cnt_Width_int-1:0]),
		clka		(Clock),
		wea		(1'b0),
		douta		(Tx_Data_for_FIFO),
		
		dinb		(Tx_RAM_Data_Bus),
		addrb		(Tx_RAM_Address_Bus[RxByte_Cnt_Width_int-2:0]),
		clkb		(BUS_Clock),
		web		(TxRAM_wren),
		doutb		(TxRAM_q_b)
		);
	
	assign Eth_Tx_End = TxRQ_Reset_ES_o; 
	
	always @(posedge Clock)
	begin
		Tx_Strobe_for_FIFO <= TxRQ_SRFF_o;
		Tx_End_Pkt_for_FIFO <= TxRQ_Reset_ES_o;
	end
	
	// -------------------------------------------------------------------------------------------------------
	// ------------------- data parcer
	Eth_Pkt_Parcer Pkt_parcer  
	(
		.Clock				(Clock),
		
		.Rx_Data				(Buffer_RAM_Data),
		.Rx_Addr_o			(Pkt_parcer_Rx_Addr_o),
		.Rx_Parcer_RQ		(RAM_Ptr_Packet_End & RxReadyToRecive_o),
		.Rx_NUM_Data		(MASS_RAM_WORD_Tx_Num-1), //--(RxWordRecive_Reg.q[10..0]-2);
		
		.Tx_Data				(Pkt_parcer_Tx_Data),
		.Tx_Addr				(Pkt_parcer_Tx_Addr),
		.Tx_Word_Strobe	(Pkt_parcer_Tx_Strobe),
		
		.Module_MAC			({Module_MAC_Reg_o[2], Module_MAC_Reg_o[1], Module_MAC_Reg_o[0]}),
		.Module_IP			({Module_IP_Reg_o[1], Module_IP_Reg_o[0]}),

		.Module_Port		(Port_Reg_o),
		
		.Reset				(SetRxReadyToRecive),
		.Progress_Flag		(Data_Flow_parcing),
		.Tx_Start			(Tx_packet_ready),
		.CycleEndErr		(CycleEndErr),
		
		.Identification	(Identification),
		.MAC_recognized	(MAC_recognized),
		
	// -- Skeleton bus control signals 
	// Conected to the module "Pkt_parcer" directly
		.AccessRequest		(AccessRequest),
		.AccessGranted		(AccessGranted),
		.DirectOut			(DirectOut),
		.AddrBusOut			(AddrBusOut),
		.DataBus_In			(DataBus_In),
		.DataBusOut			(Pkt_parcer_DataBusOut),
		.DataBusStrobe		(DataBusStrobe)
		);
	assign SetRxReadyToRecive_Parcer = CycleEndErr | TxRQ_Reset_ES_o;
	
	always @*
	begin
		if(Data_Flow_parcing == 1'b1) begin: Data_parcing_1
			Tx_RAM_Address_Bus	= Pkt_parcer_Tx_Addr[RxByte_Cnt_Width_int-2:0];
			Rx_RAM_Address_Bus	= Pkt_parcer_Rx_Addr_o[RxByte_Cnt_Width_int-2:0];
			Tx_RAM_Data_Bus		= Pkt_parcer_Tx_Data;
			TxRAM_wren				= Pkt_parcer_Tx_Strobe;
		end else begin: Data_parcing_0
			Tx_RAM_Address_Bus[RxByte_Cnt_Width_int-2:0] = AddrBus_In[RxByte_Cnt_Width_int-2:0];
			Tx_RAM_Data_Bus									= ({DataBus_In[7:0], DataBus_In[15:8]});
			TxRAM_wren											= TxRAM_CS & DataBusStrobe & DirectIn & Select_i;
			Rx_RAM_Address_Bus								= AddrBus_In[RxByte_Cnt_Width_int-2:0];
		end
	end
	assign TxRAM_CS = 1'b0;

// --------------------------------------- BUS Section ---------------------------------------------------   
// --************************************************************************* 
// -- module in master mode: 

	generate
		always @*
		begin
			case ({Select_i, DirectOut, ParcerCycle_SRFF_o})
				3'b100: begin: Sel
								DataBusOut[15:0] = DataBusOut_tmp;// -- read from skeleton bus
						  end
				3'b110: begin: Sel_Dir
								DataBusOut[15:0] = DataBusOut_tmp;// -- write from skeleton bus
							end
				3'b101: begin: Sel_Par
								DataBusOut[15:0] = DataBusOut_tmp;// -- ???? ?????? ?? ????????? ????
							end
				3'b011: begin: Dir_Par
								DataBusOut[15:0] = (Pkt_parcer_DataBusOut);// -- ???? ?????? ???????????? ??????? ?? Ethernet
							end
				3'b111: begin: Sel_Dir_Par
								DataBusOut[15:0] = (Pkt_parcer_DataBusOut);// -- ???? ?????? ???????????? ??????? ?? Ethernet
							end
				default: begin: Def
								DataBusOut[15:0] = {16{1'b0}};
							end
			endcase
		end
	endgenerate

	always @*
	begin
		if ((AddrBus_In >= 0) & (AddrBus_In < 2048)) begin 
			DataBusOut_tmp = ({Buffer_RAM_Data[7:0],Buffer_RAM_Data[15:8]}); // --DataBusOut() = RxRAM.q_b(); 
			RxRAM_CS = 1'b1;
		end
			else begin RxRAM_CS = 1'b0; 
		end

		if (AddrBus_In == 2048) begin 
			DataBusOut_tmp = Status_REG_o;
			Status_REG_CS = 1'b1;
		end
			else begin Status_REG_CS = 1'b0; 
		end

		if (AddrBus_In == 2049) begin 
			SetRxReadyToRecive_Sc_Bus = DataBusStrobe; 
		end
			else begin SetRxReadyToRecive_Sc_Bus = 1'b0; 
		end

	//	if (AddrBus_In == 2050) begin 
	//		assign InternalTxStart = DataBusStrobe;
	//	end
	//		else begin assign InternalTxStart = 1'b0;
	//	end

		if (AddrBus_In == 2051) begin 
			DataBusOut_tmp = PacketLenghts_to_be_transmitted_Reg_o;
			PacketLenghts_to_be_transmitted_Reg_CS = 1'b1;
		end
			else begin PacketLenghts_to_be_transmitted_Reg_CS = 1'b0;
		end
	end
//	if (AddrBus_In == 2052) begin 
//		assign DataBusOut_tmp = RxLostPacket_Cnt_REG_o; //-- ????? ?? ???????? ??????? ?? ????? ????????? ????????
//		assign RxLostPacket_Cnt_REG_CS = 1'b1;
//	end
//		else begin assign RxLostPacket_Cnt_REG_CS = 1'b0;
//	end
	
//	-- MAC-address and IP-address
	genvar i;
	generate
		for (i=0; i <= 2; i=i+1) //-- MAC-address
		begin: Module_MAC_Reg_i
			always @*
			begin
				if (AddrBus_In == 2053+i) begin 
					DataBusOut_tmp = Module_MAC_Reg_o[i];
					Module_MAC_Reg_CS[i] = 1'b1;
				end
				else begin 
					Module_MAC_Reg_CS[i] = 1'b0;
				end
			end
		end
	endgenerate
	
	generate
		for (i=0; i <= 1; i=i+1) // --IP-address 
		begin: Module_IP_Reg_i
			always @*
			begin
				if (AddrBus_In == 2056+i) begin 
					DataBusOut_tmp = Module_IP_Reg_o[i];
					Module_IP_Reg_CS[i] = 1'b1;
				end
				else begin 
					Module_IP_Reg_CS[i] = 1'b0;
				end
			end
		end
	endgenerate

// -- MAC-????? ?????????? ???????????? ??????? ??????????
//	if (AddrBus_In == 2058) begin assign DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[0][7:0],Source_MAC_Reg_o[0][15:8]}); end
//	if (AddrBus_In == 2059) begin assign DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[1][7:0],Source_MAC_Reg_o[1][15:8]}); end
//	if (AddrBus_In == 2060) begin assign DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[2][7:0],Source_MAC_Reg_o[2][15:8]}); end

//		-- Port 
	always @*
	begin
		if (AddrBus_In == 2061) begin 
			DataBusOut_tmp = Port_Reg_o;//--DataBusOut(7:0) = Port_Reg.q(15:8); DataBusOut(15:8) = Port_Reg.q(7:0);
			Port_Reg_CS = 1'b1;
		end
			else begin Port_Reg_CS = 1'b0;
		end

		if (AddrBus_In == 2062) begin 
			DataBusOut_tmp = Test_Reg_o;
			Test_Reg_CS = 1'b1;
		end
			else begin Test_Reg_CS = 1'b0;
		end

		if (AddrBus_In == 2063) begin DataBusOut_tmp = Identification; end
	//	if (AddrBus_In == 2064) begin assign DataBusOut_tmp = Raw_Decoder_Identification;  end

	//	if (AddrBus_In == 2065) begin assign DataBusOut_tmp = Rx_Packet_Cnt_o; end
		if (AddrBus_In == 2066) begin DataBusOut_tmp = Tx_Packet_Cnt_o; end
	//	if (AddrBus_In == 2067) begin assign DataBusOut_tmp = {16{1'b0}}; 
	//											assign Rx_Packet_Cnt_Reset = DataBusStrobe & DirectIn & Select_i; 
	//	end
	//							 else begin assign Rx_Packet_Cnt_Reset = 1'b0;
	//	end
		if (AddrBus_In == 2068) begin 
			DataBusOut_tmp = {16{1'b0}}; 
			Tx_Packet_Cnt_Reset = DataBusStrobe & DirectIn & Select_i;
		end
		else begin 
			Tx_Packet_Cnt_Reset = 1'b0;
		end
	end
//		-- ????? ???? ???????????? ?? ???????? ??????
//	if (AddrBus_In == 2069) begin assign DataBusOut_tmp = MASS_RAM_BYTE_Tx_Num; end
//
//	if ( (AddrBus_In >= 4096) & (AddrBus_In <= 6143) ) begin 
//		assign DataBusOut_tmp[15:0] = ({RxIntRAM_q_b[7:0],RxIntRAM_q_b[15:8]});
//		assign RxIntRAM_CS = 1'b1;
//	end
//	else begin 
//		assign RxIntRAM_CS = 1'b0;     
//	end

//	--***************************************************************************

//	-- MAC-address and IP-address 
//		FOR i IN 0 TO 2 GENERATE  
//--	Module_MAC_Reg(i).(data[],clock, load, enable) = (DataBus_In,Clock,1'b1, Module_MAC_Reg_CS(i) & DataBusStrobe & DirectIn & Select_i);
	generate
		for (i=0; i <= 2; i=i+1) 
		begin: Module_MAC_Reg
			ShiftReg
			#(
				.WIDTH	(16)
			) Module_MAC_Reg_i
			(
				.clock	(Clock),
				.data		(DataBus_In),
				.enable	(Module_MAC_Reg_CS[i] & DataBusStrobe & DirectIn & Select_i),
				.q			(Module_MAC_Reg_o[i])
				);
		end
	endgenerate
	
//	FOR i IN 0 TO 1 GENERATE  
//--	Module_IP_Reg(i).(data[],clock, load, enable) = (DataBus_In,Clock,1'b1, Module_IP_Reg_CS(i) & DataBusStrobe & DirectIn & Select_i);
	generate
		for (i=0; i <= 1; i=i+1) 
		begin: Module_IP_Reg
			ShiftReg 
			#(
				.WIDTH	(16)
			) Module_IP_Reg_i
			(
				.clock		(Clock),
				.data		(DataBus_In),
				.enable	(Module_IP_Reg_CS[i] & DataBusStrobe & DirectIn & Select_i),
				.q			(Module_IP_Reg_o[i])
				);
		end
	endgenerate
//--	Port_Reg.(data[],clock, load, enable) = (DataBus_In[],Clock,VCC, Port_Reg_CS & DataBusStrobe & DirectIn & Select);
	ShiftReg
	#(
		.WIDTH	(16)
	) Port_Reg
	(
		.clock	(Clock),
		.data		(DataBus_In),
		.enable	(Port_Reg_CS & DataBusStrobe & DirectIn & Select_i),
		.q			(Port_Reg_o)
		);
//--	Test_Reg.(data[],clock, load, enable) = (DataBus_In[],Clock,VCC, Test_Reg_CS & DataBusStrobe & DirectIn & Select);
	ShiftReg
	#(
		.WIDTH	(16)
	) Test_Reg
	(
		.clock	(Clock),
		.data		(DataBus_In),
		.enable	(Test_Reg_CS & DataBusStrobe & DirectIn & Select_i),
		.q			(Test_Reg_o)
		);

//--	Status_REG_ES.(d,clk) 						= (Status_REG_CS & Select, Clock);
	Edge_Sensing Status_REG_ES
	(
		.clk	(Clock),
		.d		(Status_REG_CS & Select_i),
		.q		(Status_REG_ES_o)
		);
//--	Status_REG.(clock, load, enable)			= (BUS_Clock, VCC,  Status_REG_ES.q);
//--	Status_REG.data(12:0)						= GND; --RxByte_Cnt.q();
//--	Status_REG.data(13)							= RxReadyToRecive.q;
//--	Status_REG.data(14)							= 1'b0;--IPv4_Decoder.Rx_NOT_RQ;--RxPacket_in_progress;
//--	Status_REG.data(15)							= VCC;--Raw_Decoder.Tx_Start;--ARP_Decoder.Test;--ARP_Decoder.Rx_Error;%
	ShiftReg
	#(
		.WIDTH	(16)
	) Status_REG
	(
		.clock	(BUS_Clock),
		.data		({1'b1, 1'b0, RxReadyToRecive_o, {13{1'b0}}}),
		.enable	(Status_REG_ES_o),
		.q			(Status_REG_o)
		);

//	-- ?????????? ????? ???????????? ????
//--	PacketLenghts_to_be_transmitted_Reg.(data[],clock, load, enable) = (PacketLenghts_DataBus[], BUS_Clock, VCC, PacketLenghts_to_be_transmitted_Reg_EN);
	ShiftReg 
	#(
		.WIDTH	(16) 
	) PacketLenghts_to_be_transmitted_Reg
	(
		.clock	(BUS_Clock),
		.data		(PacketLenghts_DataBus),
		.enable	(PacketLenghts_to_be_transmitted_Reg_EN),
		.q			(PacketLenghts_to_be_transmitted_Reg_o)
		);
	generate
		always @*
		begin
			if(Select_i == 1'b1) begin: PacketLenghts_1
				PacketLenghts_DataBus = DataBus_In;
				PacketLenghts_to_be_transmitted_Reg_EN = (PacketLenghts_to_be_transmitted_Reg_CS & DataBusStrobe & DirectIn & Select_i);
			end
			else begin: PacketLenghts_2
				PacketLenghts_DataBus = ({{(16 - RxByte_Cnt_Width_int+1){1'b0}},MASS_RAM_BYTE_Tx_Num-1});
				PacketLenghts_to_be_transmitted_Reg_EN = Tx_packet_AG;
			end 
		end
	endgenerate

//--	Tx_Packet_Cnt.(clock,cnt_en,sclr) = (BUS_Clock,Edge_Sensing_Sync(.d=TxStart,.clk=BUS_Clock),Tx_Packet_Cnt_Reset);  
	V_Counter 
	#(
		.width	(16)
	) Tx_Packet_Cnt
	(
		.clock	(BUS_Clock),//--Quarts,-
		.clk_en	(1'b1),
		.cnt_en	(TxStart_ES_o),
		.sclr		(Tx_Packet_Cnt_Reset),
		.q			(Tx_Packet_Cnt_o)
		);
	Edge_Sensing TxStart_ES
	(
		.clk	(Clock),
		.d		(TxStart),
		.q		(TxStart_ES_o)
		);

endmodule
