//-----------------------------------------------------------------------------
// Title    : Ethernet IPv4 decoder
//-----------------------------------------------------------------------------
// File    : Eth_IPv4_Decoder_v2.v
// Company   : INP SB RAS
// Created   : 15/09/2022
// Created by : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_IPv4_Decoder
//-----------------------------------------------------------------------------
// Revision  : 2.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_IPv4_Decoder_v2 #(
//-----------------------------------------------------------------------------
// Libraries
//-----------------------------------------------------------------------------
//`include "Eth_parameters.v"
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Input Parameters
//-----------------------------------------------------------------------------
	parameter Eth_WORD_WIDTH	= 16
	)
	(
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire			Clock, //-- System Clock, really Bus_Clock

	input		wire [Eth_WORD_WIDTH-1:0]	Rx_Data,
	output	reg [10:0]						Rx_Addr,
	input		wire								Rx_Parcer_RQ,
	input		wire [10:0]						Rx_NUM_Data,

	output	reg [10:0]						Tx_Addr,
	output	reg [Eth_WORD_WIDTH-1:0]	Tx_Data,
	output	wire								Tx_Word_Strobe,

	input		wire								Reset,

	output	wire								Rx_Error_IP,
	output	wire								Tx_Start,
	output	wire								Rx_Parcer_in_progress,
	output	wire								Rx_TRUE_RQ,
	output	wire								Rx_NOT_RQ,

	input		wire [Eth_WORD_WIDTH-1:0]	IP_Addr0_i, //-- IP address
	input		wire [Eth_WORD_WIDTH-1:0]	IP_Addr1_i, //-- IP address
	input		wire [Eth_WORD_WIDTH-1:0]	Port_i, //-- Port number

	output	wire [Eth_WORD_WIDTH-1:0]	Identification,

	//-- Standard bus connections
	output	wire								AccessRequest,
	input		wire								AccessGranted,
	output	wire								DirectOut,
	output	reg [15:0]						AddrBusOut,
	output	reg [15:0]						DataBusOut,
	input		wire [15:0]						DataBus_In,
	input		wire								DataBusStrobe,

	output	wire								Test
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

	parameter PARCER_CYLCLE_WIDTH			= 2;// --20
	parameter PARCER_CYLCLE_CNT_WIDTH	= 1+1; //Ceil(log2(PARCER_CYLCLE_WIDTH))

	parameter PacketLenghts_at_signaling_layer = 4096;//--2048;-- maximum length value in bytes
	parameter RxByte_Cnt_Width = 12-1; //Ceil( LOG2(PacketLenghts_at_signaling_layer))-1;

	parameter IPv4_type				= 16'h0008;

	parameter HeaderFrame_Type		= 6; //-- 2 ???? ??? ??????? (ARP ??? IPv4 ??? ...)
//--	parameter IP_Header			= 7; //-- 20 ???? (10 ?????) ??? IP ?????????
//--	parameter UDP_Header			= 17; //-- 8 ???? (4 ?????) ??? UDP ?????????
	parameter RxCommandPath			= 21; //-- ????? ????? ??????????? ????????
	parameter RxDataPath				= 22; //-- ????? ????? ??????????? ??? ????? ??? ?????? ? ??????????? ?? ???????
//--	parameter RxDataWordLenght	= 500;//--(PacketLenghts_at_signaling_layer / 2)-RxCommandPath-1;
	parameter RxByteLenght			= (PacketLenghts_at_signaling_layer / 2)-1;

//-- ???????????? ???????
	parameter CMD_PING				= 1; //-- ??????? ???
	parameter CMD_READ_AD			= 2; //-- ??????? ?????? ?????,??????,?????,??????...
	parameter CMD_WRITE_AD			= 3; //-- ??????? ?????? ?????,??????,?????,??????...
	parameter CMD_READ_AA			= 4; //-- ??????? ?????? ?????,?????,?????,?????... ? ?????? ??????,??????,?????...
	parameter CMD_READ_BLOCK_16	= 5; //-- ??????? ?????? ?????, 16 ???? ?????? ? ??????????????? ??????, ?????, 16 ???? ??????... 
	parameter CMD_WRITE_BLOCK_16	= 6; //-- ??????? ?????? ?????, 16 ???? ?????? ? ??????????????? ??????, ?????, 16 ???? ??????... 

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------

	wire 											 Rx_Parcer_RQ_ES_o;
	wire 											 RxParcerActive_o;
	wire 											 Tx_Start_Pulse_Flag_o;
	reg 											 Tx_Strobe;

	wire [RxByte_Cnt_Width-1:0]			 ParcerCnt_o;
	reg 											 ParcerCnt_Inc_s;

//	wire [PARCER_CYLCLE_CNT_WIDTH-:0]	 Prescaler_o;
	wire 											 Pascer_Sample_Enable;

	reg 											 IPv4_type_True;
	wire 											 IPv4_type_Flag_o;
	wire [1:0]									 IP_address_check_Flag_o;
	reg [1:0]									 IP_True;
	reg 											 Wrong_IP_address;

	reg 											 ParcerEndCycle;

	reg 											 BUS_Direct; //-- bus operation type (direction), Write - HI, Read - LOW
	wire [Eth_WORD_WIDTH-1:0]				 Target_Command_Reg_o;
	reg 											 CMD_CS; //-- ???? ??????? ARP ?????? ?? ??????????? ???????, ??????? ???? ???????? ?????? ?? Tx ???????
	wire [Eth_WORD_WIDTH-1:0]				 Target_Address_Reg_o;
	reg 											 ADDR_CS;
	reg 											 DATA_CS;
	reg 											 DATA_CS_del;
	reg 											 TA_REG_Load;
	wire [Eth_WORD_WIDTH-1:0]				 Target_Data_Reg_o;
	wire 											 DataOutReg_en;

	wire 											 Access_Request_o;
	wire 											 Data_Sent_OK;
	reg 											 Requiest_Enable;
	reg 											 Tx_Word_Strobe_s;

	reg 											 Tx_Start_Pulse;
	wire 											 Tx_Start_Pulse_EN;

	wire [Eth_WORD_WIDTH-1:0]				 Identification_Cnt_o;
	wire											 Identification_Cnt_en;

	wire [Eth_WORD_WIDTH-1:0]				 Rx_Data_tmp;

	wire [Eth_WORD_WIDTH-1:0]				 Src_IP_Addr0_reg_o;
	reg 											 Src_IP_Addr0_en;
	wire [Eth_WORD_WIDTH-1:0]				 Src_IP_Addr1_reg_o;
	reg 											 Src_IP_Addr1_en;
	wire [Eth_WORD_WIDTH-1:0]				 Src_Port_reg_o;
	reg 											 Src_Port_en;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------
	Edge_Sensing Rx_Parcer_RQ_ES
	(
		.clk	(Clock),
		.d		(Rx_Parcer_RQ),
		.q		(Rx_Parcer_RQ_ES_o)
		);

	SRFF RxParcerActive
	(
		.S		(Rx_Parcer_RQ_ES_o),
		.CLK	(Clock),
		.R		(ParcerEndCycle | Reset),
		.q		(RxParcerActive_o)
		);

	V_Counter
	#(
		.width (RxByte_Cnt_Width)
	) ParcerCnt
	(
		.data		(HeaderFrame_Type-1),
		.clock	(Clock),
		.clk_en	(1'b1),
		.cnt_en	(RxParcerActive_o & Pascer_Sample_Enable),
		.sload	(Rx_Parcer_RQ_ES_o | ParcerEndCycle),
		.q			(ParcerCnt_o)
		);

	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Src_IP_Addr0_reg
		(
			.clock	(Clock),
			.data		(Rx_Data),
			.enable	(Src_IP_Addr0_en),
			.q			(Src_IP_Addr0_reg_o)
		);
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Src_IP_Addr1_reg
		(
			.clock	(Clock),
			.data		(Rx_Data),
			.enable	(Src_IP_Addr1_en),
			.q			(Src_IP_Addr1_reg_o)
		);
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Src_Port_reg
		(
			.clock	(Clock),
			.data		(Rx_Data),
			.enable	(Src_Port_en),
			.q			(Src_Port_reg_o)
		);

	V_Counter
	#(
		.width (Eth_WORD_WIDTH)
	) Identification_Cnt
	(
		.clock	(Clock),
		.clk_en	(1'b1),
		.cnt_en	(Identification_Cnt_en),
		.q			(Identification)
		);
	Edge_Sensing Identification_Cnt_ES
	(
		.clk	(Clock),
		.d		(IPv4_type_True),
		.q		(Identification_Cnt_en)
		);

	// -- Master Access Control: send request to internal skeleton bus
	SRFF Access_Request
	(
		.S		(Requiest_Enable),
		.CLK	(Clock),
		.R		(Data_Sent_OK),
		.q		(Access_Request_o)
		);

	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Target_Command_Reg
		(
			.clock	(Clock),
			.data		({Rx_Data[7:0],Rx_Data[15:8]}),
			.enable	((CMD_CS & Pascer_Sample_Enable) | Rx_Parcer_RQ_ES_o | Reset),
			.sclr		(Rx_Parcer_RQ_ES_o | Reset),
			.q			(Target_Command_Reg_o)
		);
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Target_Address_Reg
		(
			.clock	(Clock),
			.data		({Rx_Data[7:0],Rx_Data[15:8]}),
			.enable	(TA_REG_Load | Rx_Parcer_RQ_ES_o | Reset),
			.sclr		(Rx_Parcer_RQ_ES_o | Reset),
			.q			(Target_Address_Reg_o)
		);
	// -- generate a single-cycle pulse to latch data
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) Target_Data_Reg
		(
			.clock	(Clock),
			.data		({Rx_Data[7:0],Rx_Data[15:8]}),
			.enable	((DATA_CS & !DATA_CS_del) | ParcerEndCycle | Reset),
			.sclr		(ParcerEndCycle | Reset),
			.q			(Target_Data_Reg_o)
		);

//	-- Flags
	SRFF IPv4_type_Flag
	(
		.S		(IPv4_type_True),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(IPv4_type_Flag_o)
		);

	SRFF Tx_Start_Pulse_Flag
	(
		.S		(Tx_Start_Pulse_EN & Tx_Start_Pulse),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset | ParcerEndCycle),
		.q		(Tx_Start_Pulse_Flag_o)
		);

	SRFF IP_address_check_Flag_0
	(
		.S		(IP_True[0]),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(IP_address_check_Flag_o[0])
		);

	SRFF IP_address_check_Flag_1
	(
		.S		(IP_True[1]),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(IP_address_check_Flag_o[1])
		);

//	-- outputs
	Edge_Sensing Rx_Error_IP_ES
	(
		.clk	(Clock),
		.d		(Wrong_IP_address),
		.q		(Rx_Error_IP)
		);

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
	assign Pascer_Sample_Enable = 1'b1; // For what???

//	-------------------------------------------- Rx section --------------------------------------------
//	Rx_Data_tmp[] = Rx_Data[]; // For what???

	always @*
	begin
		if (RxParcerActive_o == 1'b1) begin
			Tx_Addr = ParcerCnt_o;
			
			case (ParcerCnt_o)
				5: begin		Rx_Addr = 6 ; end
				6: begin		Rx_Addr = 7 ; Tx_Data = Rx_Data; end // -- Packet header type
				7: begin		Rx_Addr = 8 ; Tx_Data = Rx_Data; end 
				8: begin		Rx_Addr = 17; Tx_Data = Rx_Data; end
				9: begin		Rx_Addr = 10; Tx_Data = Identification; Src_Port_en = 1'b1;  end
				10: begin	Rx_Addr = 11; Tx_Data = Rx_Data; end
				11: begin	Rx_Addr = 12; Tx_Data = Rx_Data; end
				12: begin	Rx_Addr = 13; Tx_Data = Rx_Data; end
				13: begin	Rx_Addr = 14; Tx_Data = IP_Addr0_i;			   Src_IP_Addr0_en = 1'b1; end
				14: begin	Rx_Addr = 15; Tx_Data = IP_Addr1_i;			   Src_IP_Addr1_en = 1'b1; end
				15: begin	Rx_Addr = 16; Tx_Data = Src_IP_Addr0_reg_o; end
				16: begin	Rx_Addr = 21; Tx_Data = Src_IP_Addr1_reg_o; end
				17: begin	Rx_Addr = 18; Tx_Data = Port_i;				   CMD_CS = 1'b1; end //-- latch module port number to TX buffer, latch operation type	 			
				18: begin	Rx_Addr = 19; Tx_Data = Src_Port_reg_o; end //-- latch incoming packet port number to TX buffer
				19: begin	Rx_Addr = 20; Tx_Data = Rx_Data; end
				20: begin	Rx_Addr = 21; Tx_Data = 1'b0; end //-- UDP checksum (not analize)
				21: begin	Rx_Addr = 22; Tx_Data = (Rx_Data | 16'h0002); Tx_Start_Pulse = 1'b1; end //-- latch answer command, if IP and UDP fields is correct then enable transmite Tx data	 
				default: begin Rx_Addr = ParcerCnt_o+1; CMD_CS = 1'b0; 
									// -- encoding address and data from packet
									if (ParcerCnt_o[0] == 1'b0) begin ADDR_CS = 1'b1; DATA_CS = 1'b0; Tx_Data = Rx_Data; end
																  else begin ADDR_CS = 1'b0; DATA_CS = 1'b1; Tx_Data =({DataBus_In[7:0], DataBus_In[15:8]});
									end
						  end
			endcase

			case (ParcerCnt_o)
				6:  begin	if(Rx_Data == IPv4_type) begin IPv4_type_True = 1'b1; end end
				7:  begin	if(IPv4_type_Flag_o == 1'b0) begin ParcerEndCycle = 1'b1; end end
				15: begin	if(Rx_Data == IP_Addr0_i) begin IP_True[0] = 1'b1; end end
				16: begin	if(Rx_Data == IP_Addr1_i) begin IP_True[1] = 1'b1; end end
				17: begin	if((IP_address_check_Flag_o[0] & IP_address_check_Flag_o[1]) == 1'b0) begin 
									ParcerEndCycle = 1'b1; 
									Wrong_IP_address = 1'b1; 
								end
					 end
			endcase
			
			if (ParcerCnt_o >= Rx_NUM_Data) begin ParcerEndCycle = 1'b1; end // -- End IPv4 parcer cycle
			
			Requiest_Enable = (Pascer_Sample_Enable & ADDR_CS);
			TA_REG_Load	  = (Pascer_Sample_Enable & ADDR_CS);

			if (DATA_CS == 1'b1) begin Tx_Word_Strobe_s = Data_Sent_OK; ParcerCnt_Inc_s = Data_Sent_OK; end 
								 else begin Tx_Word_Strobe_s = Pascer_Sample_Enable & (ParcerCnt_o >= HeaderFrame_Type); ParcerCnt_Inc_s = Pascer_Sample_Enable;
			end

			AddrBusOut = Target_Address_Reg_o;
			DataBusOut = Target_Data_Reg_o;

		end else begin 
			IPv4_type_True		= 1'b0;
			Rx_Addr					= {11{1'b0}};
			Tx_Addr					= {11{1'b0}};
			Tx_Data					= {Eth_WORD_WIDTH{1'b0}}; 
			ParcerEndCycle		= 1'b0;
			CMD_CS					= 1'b0;
			ADDR_CS					= 1'b0;
			DATA_CS					= 1'b0;
			AddrBusOut				= {16{1'b0}};
			DataBusOut				= {16{1'b0}};
			Requiest_Enable		= 1'b0;
			TA_REG_Load			= 1'b0;
			Tx_Word_Strobe_s		= 1'b0;
			IP_True[0]				= 1'b0;
			IP_True[1]				= 1'b0;
			Wrong_IP_address		= 1'b0;
			Src_IP_Addr0_en		= 1'b0;
			Src_IP_Addr1_en		= 1'b0;
			Src_Port_en			= 1'b0;
		end
	end

	assign AccessRequest	= Access_Request_o;
	assign DirectOut		= BUS_Direct;

	assign Data_Sent_OK	= DataBusStrobe & AccessGranted;

	always @(Clock)
	begin 
		DATA_CS_del <= DATA_CS;
	end

	always @*
	begin
		case (Target_Command_Reg_o & 16'h00FF) // -- analize LSB, define command type (Read/Write)
				CMD_READ_AD:	begin	BUS_Direct = 1'b0; end 
				CMD_WRITE_AD:	begin	BUS_Direct = 1'b1; end 
				default: begin			BUS_Direct = 1'b0; end 
		endcase
	end
	assign Tx_Start_Pulse_EN = (Target_Command_Reg_o[15:8] == 2) ? 1'b0 : 1'b1; 
																					//-- analize MSB, ?????????? ????????? ??? ???????????? ???????
																								//-- check packet payload: incoming or returned

//	-- outputs
//	assign Rx_Error_IP				= Edge_Sensing_Sync(.d=Wrong_IP_address,.clk=Clock);
	assign Rx_NOT_RQ					= (!IPv4_type_Flag_o) & ParcerEndCycle;
	assign Tx_Start					= Tx_Start_Pulse_Flag_o & ParcerEndCycle;
	assign Rx_Parcer_in_progress	= RxParcerActive_o;
	assign Tx_Word_Strobe			= (Tx_Word_Strobe_s & RxParcerActive_o); // -- ????? ???????????? ? Tx ??????
	assign Rx_TRUE_RQ					= IPv4_type_Flag_o & ParcerEndCycle;

	assign Test							= IPv4_type_Flag_o;

endmodule
