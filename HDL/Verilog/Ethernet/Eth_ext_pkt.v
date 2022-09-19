//-----------------------------------------------------------------------------
// Title       : Eth_ext_pkt
//-----------------------------------------------------------------------------
// File        : Eth_ext_pkt.v
// Company     : INP SB RAS
// Created     : 12/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_ext_pkt
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_ext_pkt (
//-----------------------------------------------------------------------------
// Libraries
//-----------------------------------------------------------------------------
//`include "Eth_parameters.v"
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire			Clock, //-- System Clock, really Bus_Clock
	//-- PHY Ethernet I/O
	//-- Rx section    --Preambula, SOF & CRC are cutted out
	input		wire			Byte_Strobe_Rx,
	input		wire [7:0]	Rx_Data,
	input		wire			RxPacket_in_progress,
	input		wire			RxPacket_End,
	input		wire			Packet_Good_End, //-- CRC result
	input		wire			Packet_bad_End, //-- CRC result

	output	wire			Packet_Decode_Error, //--next packet is detected while current packet is in processing

	//-- Tx section
	output	wire			Eth_Tx_End,

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
	parameter PacketLenghts_at_signaling_layer_ext = 2048;//--2048;-- maximum length value in bytes
	parameter RxByte_Cnt_Width_ext = 11;//clog2(PacketLenghts_at_signaling_layer_ext);

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
	wire 									 RxByte_Cnt_sclr;
	wire [RxByte_Cnt_Width_ext:0]	 RxByte_Cnt_o;
	wire [Eth_WORD_WIDTH-1:0]		 RxByte_Cnt_Reg_o;
	wire 									 RAM_Overflow;
	wire 									 RAM_Overflow_SRFF_o;
	wire 									 Cnt_Overflow_Edge_o;
	wire 									 RxReadyToRecive_o;
	wire 									 RxReadyToRecive;
	wire 									 SetRxReadyToRecive;
	wire 									 SetRxReadyToRecive_Parcer_o;
	reg 									 SetRxReadyToRecive_Sc_Bus;
	wire [7:0]							 RxRAM_q_a;
	wire [15:0]							 RxRAM_q_b;
	reg 									 RxRAM_CS;
	reg [RxByte_Cnt_Width_ext-2:0]	 Rx_RAM_Address_Bus;

	wire [Eth_WORD_WIDTH-1:0]		 Buffer_RAM_Data;

	wire [Eth_WORD_WIDTH-1:0]		 Status_REG_o;//-- ?????? ???????????? (???? ????? ??? ????????)
	reg 									 Status_REG_CS;
	wire 									 Status_REG_ES_o;

	wire 									 RxLostPackcet_ES_o;
	wire 									 RxLostPacket_node;
	wire [Eth_WORD_WIDTH-1:0]		 RxLostPacket_Cnt_o;
	wire [Eth_WORD_WIDTH-1:0]		 RxLostPacket_Cnt_REG_o;
	reg 									 RxLostPacket_Cnt_REG_CS;
	wire 									 RxLostPacket_Cnt_REG_ES_o;

	wire [RxByte_Cnt_Width_ext:0]	 TxByte_Cnt_o;
	wire [7:0] 							 TxRAM_q_a;
	wire [15:0]							 TxRAM_q_b;
	reg 									 TxRAM_wren;
	reg [RxByte_Cnt_Width_ext-2:0]	 Tx_RAM_Address_Bus;
	reg [Eth_WORD_WIDTH-1:0]		 Tx_RAM_Data_Bus;
	reg 									 TxRAM_CS;
	wire 									 TxRQ_Reset;
	wire 									 TxRQ_Reset_ES_o;
	wire 									 TxRQ_SRFF_o;
//	wire 									 InternalTxStart;
	wire 									 AnswerTxStart_ES_o;

	reg [RxByte_Cnt_Width_ext-2:0]	Pkt_parcer_Rx_Addr_o;
	reg [RxByte_Cnt_Width_ext-2:0]	Pkt_parcer_Tx_Addr;
	reg [Eth_WORD_WIDTH-1:0]			Pkt_parcer_Tx_Data;
	wire 										Pkt_parcer_Tx_Strobe;
	reg [15:0]								Pkt_parcer_DataBusOut;
	wire [Eth_WORD_WIDTH-1:0]		 PacketLenghts_to_be_transmitted_Reg_o; 
	reg 									 PacketLenghts_to_be_transmitted_Reg_CS;
	reg [Eth_WORD_WIDTH-1:0]		 PacketLenghts_DataBus;
	reg 									 PacketLenghts_to_be_transmitted_Reg_EN;

	wire [2:0]							 Module_MAC_Reg_o [Eth_WORD_WIDTH-1:0];
	reg [2:0]							 Module_MAC_Reg_CS;
	wire [1:0]							 Module_IP_Reg_o [Eth_WORD_WIDTH-1:0];
	reg [1:0]							 Module_IP_Reg_CS;
	wire [Eth_WORD_WIDTH-1:0]		 Port_Reg_o;
	reg 									 Port_Reg_CS;
	
	reg 									 Identification;
	reg 									 CycleEndErr;

	wire [Eth_WORD_WIDTH-1:0]		 RxWordRecive_Reg_o;
	wire [Eth_WORD_WIDTH-1:0]		 Rx_Packet_Lenght_Reg_o;
	wire [Eth_WORD_WIDTH-1:0]		 Rx_Packet_Lenght_Reg_i;

	wire [2:0]							 Source_MAC_Reg_en;
	wire [2:0]							 Source_MAC_Reg_o [Eth_WORD_WIDTH-1:0];
	
	wire [Eth_WORD_WIDTH-1:0]		 Test_Reg_o;
	reg 									 Test_Reg_CS;

	reg [15:0]							 DataBusOut_tmp;

	reg [15:0]							 Rx_Packet_Cnt_o;
	reg [15:0]							 Tx_Packet_Cnt_o;
	reg 									 Rx_Packet_Cnt_Reset;
	reg 									 Tx_Packet_Cnt_Reset;

	wire 									 RxTxCycle_IntStart_o;

	wire 									 In_FIFO_e;
	wire [9:0]							 In_FIFO_o;
	wire 									 Copy_Byte_Strobe;
	wire 									 FIFO_to_RxRAM_Copy;// -- ?????? ?????????? ????????????? ?????? ????????? ??????? ???????? ?????? ? ?????? ?????? ? ???????? ??????
	wire 									 FIFO_to_RxRAM_Copy_ES_o;
	wire 									 RAM_Ptr_Packet_End;// -- ?????? ????????? ??????????? ?????? ?? FIFO ? ???????? ?????? ? ??????? ????????? ???????????? ??????

	wire 									 Packet_Good_End_ES_o;// -- ????????? ????????? ???????? ?????? (CRC ???????)
	wire 									 Packet_bad_End_ES_o;// -- ????????? ????????? ??????? ?????? (CRC ?????????)

	wire 									 Data_Flow_parcing;
	wire 									 Tx_packet_ready;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------

//-------------------------------------------- Input FIFO --------------------------------------------
//--	RxIntStart_out = RxIntStart_ES.q;--RxIntStart;
//	assign RxIntStart_out = RxIntStart_ES_o;
	
	Eth_In_FIFO4kb In_FIFO
	(
		.din		({Packet_Good_End, Packet_bad_End, Rx_Data}),
		.clk		(Clock),
		.wr_en	(Byte_Strobe_Rx | RxPacket_End),
		.rd_en	((!In_FIFO_e) & Copy_Byte_Strobe & RxReadyToRecive_o),
		
		.empty	(In_FIFO_e),
		.dout		(In_FIFO_o)
		);

	assign FIFO_to_RxRAM_Copy	= (!In_FIFO_e) & Copy_Byte_Strobe & RxReadyToRecive_o;
	assign Copy_Byte_Strobe		= 1'b1;

//--	Packet_Good_End_ES.(d,clk) = (In_FIFO.q(9],Clock);
	Edge_Sensing Packet_Good_End_ES
	(
		.clk	(Clock),
		.d		(In_FIFO_o[9]),
		.q		(Packet_Good_End_ES_o)
		);

//--	Packet_bad_End_ES.(d,clk)  = (In_FIFO.q(8],Clock);
	Edge_Sensing Packet_bad_End_ES
	(
		clk	(Clock),
		d		(In_FIFO_o[8]), 
		q		(Packet_bad_End_ES_o)
		);

//	-------------------------------------------- Rx section --------------------------------------------
//--	RxReadyToRecive.(S,clk,R) = (SetRxReadyToRecive, Clock, RAM_Ptr_Packet_End | RxIntStart_ES.q);
	SRFF RxReadyToRecive_SR
	 (
		.S		(SetRxReadyToRecive),
		.CLK	(Clock),
		.R		(RAM_Ptr_Packet_End),
		.q		(RxReadyToRecive_o)
		);

	assign SetRxReadyToRecive = SetRxReadyToRecive_Sc_Bus | SetRxReadyToRecive_Parcer_o;
	assign Eth_RxTx_In_Progress = (!RxReadyToRecive_o);

//--	RxByte_Cnt.(clock,clk_en,cnt_en,sclr) = (Clock,VCC, FIFO_to_RxRAM_Copy , --Enable to count actual length
//--													SetRxReadyToRecive | Packet_bad_End_ES.q    --Not to clear while processing in progress to know actual length
//--														); 
//--	RxByte_Cnt.data() = MASS_RAM_BYTE_Tx_Num-1;  
//--	RxByte_Cnt.sload  = RxIntStart_ES.q;
	V_Counter 
	#(
		.WIDTH (RxByte_Cnt_Width_ext+1)
	) RxByte_Cnt
	(
		.clock 	(clock),//--Quarts,--
		.cnt_en	(FIFO_to_RxRAM_Copy),
		.sload	(RxIntStart_ES_o),
//		.data		(MASS_RAM_BYTE_Tx_Num - 1),
		.sclr		(SetRxReadyToRecive | Packet_bad_End_ES_o),
		.q			(RxByte_Cnt_o)
		);

//--	IF 	(RxByte_Cnt.q[] == PacketLenghts_at_signaling_layer-1) --Check to not overwrite RAM at ubnormal length packets
//--		THEN RAM_Overflow	= VCC;
//--		ELSE RAM_Overflow	= GND;
//--	end
	assign RAM_Overflow = (RxByte_Cnt_o == (PacketLenghts_at_signaling_layer_ext - 1)) ? 1'b1 : 1'b0;

//--	RAM_Overflow_SRFF.(S,clk,R) = (Cnt_Overflow_Edge.q,Clock,SetRxReadyToRecive);
	SRFF RAM_Overflow_SRFF
	(
		.S		(Cnt_Overflow_Edge_o),
		.CLK	(Clock),
		.R		(SetRxReadyToRecive),
		.q		(RAM_Overflow_SRFF_o)
		);
		
//--	Cnt_Overflow_Edge.(clk,d)	=	(Clock, RAM_Overflow); -- 
	Edge_Sensing Cnt_Overflow_Edge
	(
		.clk	(Clock),
		.d		(RAM_Overflow),
		.q		(Cnt_Overflow_Edge_o)
		);

	assign Packet_Decode_Error = Cnt_Overflow_Edge_o;

	assign RAM_Ptr_Packet_End = Packet_Good_End_ES_o;


//--	IF((RxPacket_in_progress == VCC) & (RxReadyToRecive.q == GND)) THEN RxLostPacket_node = VCC; -- 
//--																 ELSE RxLostPacket_node = GND; -- 
//--	end
	assign RxLostPacket_node = ((RxPacket_in_progress == 1'b1) & (RxReadyToRecive == 1'b0)) ? 1'b1 : 1'b0;

//--	RxRAM.data_a(7:0) = In_FIFO_o(7:0);  -- 
//--	RxRAM.(address_a(RxByte_Cnt_Width_ext-1:0)   , clock_a, wren_a                       ) =
//--		(RxByte_Cnt.q(RxByte_Cnt_Width_ext-1:0], Clock  , FIFO_to_RxRAM_Copy & !RAM_Overflow_SRFF.q); 
//--
//--	RxRAM.(address_b(RxByte_Cnt_Width_ext-2:0)     , clock_b  , data_b(15:8)   , data_b(7:0)     , wren_b  ) =
//--		(Rx_RAM_Address_Bus[], BUS_Clock, DataBus_In(7:0], DataBus_In(15:8], RxRAM_CS & DataBusStrobe & DirectIn & Select); 
	EthBufferRAM2048 RxRAM
	(
		.dina		(In_FIFO_o),
		.addra	(RxByte_Cnt_o[RxByte_Cnt_Width_ext-1:0]),
		.clka		(Clock),
		.wea		(FIFO_to_RxRAM_Copy & (!RAM_Overflow_SRFF_o)),
		.douta	(RxRAM_q_a),
		
		.dinb		({DataBus_In[7:0],DataBus_In[15:8]}),
		.addrb	(Rx_RAM_Address_Bus),
		.clkb		(BUS_Clock),
		.web		(RxRAM_CS & DataBusStrobe & DirectIn & Select_i),
		.doutb	(RxRAM_q_b)
		);

//--	RxLostPackcet_ES.(d,clk)                            = (RxLostPacket_node, Clock);
	Edge_Sensing RxLostPackcet_ES
	(
		.clk	(Clock),
		.d		(RxLostPacket_node),
		.q		(RxLostPackcet_ES_o)
		);

//--	RxLostPacket_Cnt.(clock,clk_en,cnt_en,sclr)        = (Clock, VCC, RxLostPackcet_ES.q, GND%RxPacketError_Cnt_REG_ES.q%);
	V_Counter
	#(
		.width (26)
	) RxLostPacket_Cnt
	(
		clock		(Clock),//--Quarts,--
		clk_en	(1'b1),
		cnt_en	(RxLostPackcet_ES_o),
		sclr		(1'b0),
		q			(RxLostPacket_Cnt_o)
		);

//--	RxLostPacket_Cnt_REG.(data[],clock, load, enable)  = (RxLostPacket_Cnt.q[], BUS_Clock, VCC, VCC%RxPacketError_Cnt_REG_ES.q%);
	ShiftReg
	#(
		.WIDTH (Eth_WORD_WIDTH) 
	) RxLostPacket_Cnt_REG
	(
		.clock	(BUS_Clock),
		.data		(RxLostPacket_Cnt_o),
		.load		(1'b1),
		.enable	(1'b1),
		.q			(RxLostPacket_Cnt_REG_o)
	);

//--	RxLostPacket_Cnt_REG_ES.(d,clk)                    = (RxLostPacket_Cnt_REG_CS & Select & DataBusStrobe, BUS_Clock);
	Edge_Sensing RxLostPacket_Cnt_REG_ES
	(
		.clk	(BUS_Clock),
		.d		(RxLostPacket_Cnt_REG_CS & Select_i & DataBusStrobe),
		.q		(RxLostPacket_Cnt_REG_ES_o)
		);

//	------------------------------------------- Tx section ------------------------------------------------- 
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
		.width (RxByte_Cnt_Width_ext+1)
	) TxByte_Cnt
	(
		.clock	(Clock),
		.clk_en	(1'b1),
		.cnt_en	(TxRQ_SRFF_o & !Out_FIFO_f),
		.sclr		(Tx_packet_AG),
		.q			(TxByte_Cnt_o)
		);
	assign TxRQ_Reset = ((TxByte_Cnt_o >= PacketLenghts_at_signaling_layer_ext-1) | (TxByte_Cnt_o >= PacketLenghts_to_be_transmitted_Reg_o[RxByte_Cnt_Width_ext:0])) ? 1'b1 : 1'b0;

//--	TxRQ_Reset_ES.(d,clk) = (TxRQ_Reset,Clock);
	Edge_Sensing TxRQ_Reset_ES
	(
		.clk	(BUS_Clock),
		.d		(TxRQ_Reset),
		.q		(TxRQ_Reset_ES_o)
		);

//--	TxRAM.data_a(7:0) = GND;  
//--	TxRAM.(address_a(RxByte_Cnt_Width_ext-1:0)   , clock_a, wren_a) =
//--		(TxByte_Cnt.q(RxByte_Cnt_Width_ext-1:0], Clock  , GND   ); 
//--
//--	TxRAM.(address_b(RxByte_Cnt_Width_ext-2:0)         , clock_b  , data_b()         , wren_b  ) =
//--		(Tx_RAM_Address_Bus(RxByte_Cnt_Width_ext-2:0], BUS_Clock, Tx_RAM_Data_Bus[], TxRAM_wren);
	EthBufferRAM2048 TxRAM
	(
		dina		({8{1'b0}}),
		addra		(TxByte_Cnt_o[RxByte_Cnt_Width_ext-1:0]),
		clka		(Clock),
		wea		(1'b0),
		douta		(Tx_Data_for_FIFO),
		
		dinb		(Tx_RAM_Data_Bus),
		addrb		(Tx_RAM_Address_Bus[RxByte_Cnt_Width_ext-2:0]),
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
		.Reset				(SetRxReadyToRecive),
		
		.Rx_Data				(Buffer_RAM_Data),
		.Rx_Addr_o			(Pkt_parcer_Rx_Addr_o),
		.Rx_Parcer_RQ		(RAM_Ptr_Packet_End & RxReadyToRecive_o),
		.Rx_NUM_Data		({1'b0, (RxWordRecive_Reg_o[RxByte_Cnt_Width_ext-2:0]-2)}), //--(RxWordRecive_Reg.q[10..0]-2);
		
		.Tx_Data				(Pkt_parcer_Tx_Data),
		.Tx_Addr				(Pkt_parcer_Tx_Addr),
		.Tx_Word_Strobe	(Pkt_parcer_Tx_Strobe),
		
		.Module_MAC			({Module_MAC_Reg_o[2], Module_MAC_Reg_o[1], Module_MAC_Reg_o[0]}),
		.Module_IP			({Module_IP_Reg_o[1], Module_IP_Reg_o[0]}),

		.Module_Port		(Port_Reg_o),
		
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
			Tx_RAM_Address_Bus	= Pkt_parcer_Tx_Addr[RxByte_Cnt_Width_ext-2:0];
			Rx_RAM_Address_Bus	= Pkt_parcer_Rx_Addr_o[RxByte_Cnt_Width_ext-2:0];
			Tx_RAM_Data_Bus		= Pkt_parcer_Tx_Data;
			TxRAM_wren				= Pkt_parcer_Tx_Strobe;
		end else begin: Data_parcing_0
			Tx_RAM_Address_Bus[RxByte_Cnt_Width_ext-2:0] = AddrBus_In[RxByte_Cnt_Width_ext-2:0];
			Tx_RAM_Data_Bus									= ({DataBus_In[7:0], DataBus_In[15:8]});
			TxRAM_wren											= TxRAM_CS & DataBusStrobe & DirectIn & Select_i;
			Rx_RAM_Address_Bus								= AddrBus_In[RxByte_Cnt_Width_ext-2:0];
		end
	end
//	assign TxRAM_CS = 1'b0;

//	-- source MAC-address buffer
	genvar i;
	generate
		for (i=0; i <= 2; i=i+1) 
		begin: Source_MAC_Reg_i
			assign Source_MAC_Reg_en[i] = (Rx_RAM_Address_Bus == 3+i) ? 1'b1 : 1'b0;
			ShiftReg
			#(
				.WIDTH (Eth_WORD_WIDTH) 
			) Source_MAC_Reg
			(
				.clock	(Clock),
				.data		(RxRAM_q_b),
				.enable	(Source_MAC_Reg_en[i]),
				.q			(Source_MAC_Reg_o[i])
			);
		end
	endgenerate
//	-- latch incoming number bytes and convert to 16-bit words
//	Rx_Packet_Lenght_Reg.data[15..RxByte_Cnt_Width]   = GND;
//	Rx_Packet_Lenght_Reg.data[RxByte_Cnt_Width-1..0]  = RxByte_Cnt.q[RxByte_Cnt_Width..1]-(HEADER_LENGTH_WORDS-1)-3;
//	Rx_Packet_Lenght_Reg.(clock,enable,load) = (Clock,RAM_Ptr_Packet_End,VCC); 
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Rx_Packet_Lenght_Reg
		(
			.clock	(Clock),
			.data		({{(Eth_WORD_WIDTH-1-RxByte_Cnt_Width_ext){1'b0}}, (RxByte_Cnt_o[RxByte_Cnt_Width_ext:1]-(HEADER_LENGTH_WORDS-1)-3)}),
			.enable	(RAM_Ptr_Packet_End),
			.q			(Rx_Packet_Lenght_Reg_o)
		);

//	RxWordRecive_Reg.(data[15..RxByte_Cnt_Width],data[RxByte_Cnt_Width-1..0]) = (B"00000",RxByte_Cnt.q[RxByte_Cnt_Width..1]);
//	RxWordRecive_Reg.(clock,enable,load) = (Clock,RAM_Ptr_Packet_End,VCC); 
	ShiftReg
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) RxWordRecive_Reg
		(
			.clock	(Clock),
			.data		({4'b0000,RxByte_Cnt_o[RxByte_Cnt_Width_ext:1]}),
			.enable	(RAM_Ptr_Packet_End),
			.q			(RxWordRecive_Reg_o)
		);

// --------------------------------------- BUS Section ---------------------------------------------------   
// --************************************************************************* 
// -- module in master mode: 

	generate
		always @*
		begin
			case ({Select_i, DirectOut, Data_Flow_parcing})
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
		if ((AddrBus_In >= 0) & (AddrBus_In < 1024)) begin 
			DataBusOut_tmp = ({Buffer_RAM_Data[7:0],Buffer_RAM_Data[15:8]}); // --DataBusOut() = RxRAM.q_b(); 
			RxRAM_CS = 1'b1;
		end
			else begin RxRAM_CS = 1'b0; 
		end
		if ((AddrBus_In >= 1024) & (AddrBus_In < 2048)) begin 
			DataBusOut_tmp = ({TxRAM_q_b[7:0],TxRAM_q_b[15:8]}); // --DataBusOut() = RxRAM.q_b(); 
			TxRAM_CS = 1'b1;
		end
			else begin TxRAM_CS = 1'b0; 
		end

		if (AddrBus_In == 2048) begin 
			DataBusOut_tmp = Status_REG_o;
			Status_REG_CS = 1'b1;
		end
			else begin Status_REG_CS = 1'b0; 
		end

		if (AddrBus_In == 2049) begin 
			assign SetRxReadyToRecive_Sc_Bus = DataBusStrobe; 
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
		if (AddrBus_In == 2052) begin 
			DataBusOut_tmp = RxLostPacket_Cnt_REG_o; //-- ????? ?? ???????? ??????? ?? ????? ????????? ????????
			RxLostPacket_Cnt_REG_CS = 1'b1;
		end
			else begin RxLostPacket_Cnt_REG_CS = 1'b0;
		end
	end
	
//	-- MAC-address and IP-address
//	genvar i;
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

	always @*
	begin
// -- MAC-????? ?????????? ???????????? ??????? ??????????
		if (AddrBus_In == 2058) begin DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[0][7:0],Source_MAC_Reg_o[0][15:8]}); end
		if (AddrBus_In == 2059) begin DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[1][7:0],Source_MAC_Reg_o[1][15:8]}); end
		if (AddrBus_In == 2060) begin DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[2][7:0],Source_MAC_Reg_o[2][15:8]}); end

//		-- Port 
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
		if (AddrBus_In == 2064) begin DataBusOut_tmp = {16{1'b0}}; end

		if (AddrBus_In == 2065) begin DataBusOut_tmp = Rx_Packet_Cnt_o; end
		if (AddrBus_In == 2066) begin DataBusOut_tmp = Tx_Packet_Cnt_o; end
		if (AddrBus_In == 2067) begin DataBusOut_tmp = {16{1'b0}}; 
												Rx_Packet_Cnt_Reset = DataBusStrobe & DirectIn & Select_i; 
		end
								 else begin Rx_Packet_Cnt_Reset = 1'b0;
		end
		if (AddrBus_In == 2068) begin 
			DataBusOut_tmp = {16{1'b0}}; 
			Tx_Packet_Cnt_Reset = DataBusStrobe & DirectIn & Select_i;
		end
		else begin 
			Tx_Packet_Cnt_Reset = 1'b0;
		end
	end

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
				PacketLenghts_DataBus = ({{(16 - RxByte_Cnt_Width_ext+1){1'b0}},(RxByte_Cnt_o-7)});
				PacketLenghts_to_be_transmitted_Reg_EN = Tx_packet_AG;
			end 
		end
	endgenerate

//	RxByte_Cnt_Reg.data[RxByte_Cnt_Width..0]  = (RxByte_Cnt.q[]-1);
//	RxByte_Cnt_Reg.data[15..RxByte_Cnt_Width+1] = GND;
//	RxByte_Cnt_Reg.(clock, load, enable) = (Clock,VCC,Edge_Sensing_Sync(.d=FIFO_to_RxRAM_Copy,.clk=Clock));
	ShiftReg 
	#(
		.WIDTH	(16)
	) RxByte_Cnt_Reg
	(
		.clock	(Clock),
		.data		({{(16 - RxByte_Cnt_Width_ext+1){1'b0}},(RxByte_Cnt_o-1)}),
		.enable	(RxByte_Cnt_Reg_en),
		.q			(RxByte_Cnt_Reg_o)
		);
	Edge_Sensing RxByte_Cnt_Reg_ES
	(
		.clk	(Clock),
		.d		(FIFO_to_RxRAM_Copy),
		.q		(RxByte_Cnt_Reg_en)
		);

//	------------ ???????? ?????????(????????? MAC-???????) ? ???????????? ???????
//	Rx_Packet_Cnt.(clock,cnt_en,sclr) = (BUS_Clock,Edge_Sensing_Sync(.d=Pkt_parcer.MAC_recognized,.clk=BUS_Clock),Rx_Packet_Cnt_Reset);  
	V_Counter 
	#(
		.width	(16)
	) Rx_Packet_Cnt
	(
		.clock	(BUS_Clock),//--Quarts,-
		.clk_en	(1'b1),
		.cnt_en	(Rx_Packet_Cnt_en),
		.sclr		(Tx_Packet_Cnt_Reset),
		.q			(Tx_Packet_Cnt_o)
		);
	Edge_Sensing Rx_Packet_Cnt_ES
	(
		.clk	(Clock),
		.d		(MAC_recognized),
		.q		(Rx_Packet_Cnt_en)
		);
//	Tx_Packet_Cnt.(clock,cnt_en,sclr) = (BUS_Clock,Edge_Sensing_Sync(.d=AnswerTxStart,.clk=BUS_Clock),Tx_Packet_Cnt_Reset);  
	V_Counter 
	#(
		.width	(16)
	) Tx_Packet_Cnt
	(
		.clock	(BUS_Clock),//--Quarts,-
		.clk_en	(1'b1),
		.cnt_en	(Tx_Packet_Cnt_en),
		.sclr		(Tx_Packet_Cnt_Reset),
		.q			(Tx_Packet_Cnt_o)
		);
	Edge_Sensing Tx_Packet_Cnt_ES
	(
		.clk	(Clock),
		.d		(Tx_packet_AG),
		.q		(Tx_Packet_Cnt_en)
		);

endmodule
