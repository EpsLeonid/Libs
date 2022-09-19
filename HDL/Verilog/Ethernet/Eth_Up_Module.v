//-----------------------------------------------------------------------------
// Title       : Eth_Up_Module
//-----------------------------------------------------------------------------
// File        : Eth_Up_Module.v
// Company     : INP SB RAS
// Created     : 08/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_Up_Module
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_Up_Module (
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

	input		wire			RxIntStart,
	output	wire			RxIntStart_out,

	output	wire			Packet_Decode_Error, //--next packet is detected while current packet is in processing

	//-- Tx section
	input		wire			Byte_Strobe_Tx,
	output	wire [7:0]	Tx_Data,
	output	wire			Transmit_of_Data_RQ,
	input		wire			Eth_Tx_In_Progress, //-- Transmittion is in progress(flag), Phy is busy
	output	wire			Eth_Tx_End,

	output	wire			Eth_RxTx_In_Progress,


	//-- Standard bus connections
	input		wire			BUS_Clock,
	input		wire [15:0]	DataBus_In,
	output	wire [15:0]	DataBusOut,

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
	parameter PacketLenghts_at_signaling_layer = 4096;//--2048;-- maximum length value in bytes
	parameter RxByte_Cnt_Width = Ceil( log2(PacketLenghts_at_signaling_layer));

	parameter WORD_WIDTH				 = 16; 

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
	wire [RxByte_Cnt_Width:0]		 RxByte_Cnt_o;
	wire [WORD_WIDTH-1:0]			 RxByte_Cnt_Reg_o;
	wire 									 RAM_Overflow;
	wire 									 RAM_Overflow_SRFF_o;
	wire 									 Cnt_Overflow_Edge_o;
	wire 									 RxReadyToRecive_r;
	wire 									 RxReadyToRecive_o;
	wire 									 RxReadyToRecive;
	wire 									 SetRxReadyToRecive;
	wire 									 SetRxReadyToRecive_Parcer_d;
	wire 									 SetRxReadyToRecive_Parcer_o;
	wire 									 SetRxReadyToRecive_Sc_Bus;
	wire 									 RxRAM_wea;
	wire 									 RxRAM_web;
	wire [7:0]							 RxRAM_q_a;
	wire [15:0]							 RxRAM_q_b;
	wire 									 RxRAM_CS;
	wire [RxByte_Cnt_Width-2:0]	 Rx_RAM_Address_Bus;

	wire 									 RxIntRAM_web;
	wire [15:0]							 RxIntRAM_q_a;
	wire [15:0]							 RxIntRAM_q_b;
	wire 									 RxIntRAM_CS;
	wire 									 RxIntStart_ES_d;
	wire 									 RxIntStart_ES_o;
	wire 									 RxIntStart_node;
	wire [WORD_WIDTH-1:0]			 Buffer_RAM_Data;
	wire 									 ParcerCycle_IntStart_o;

	wire [WORD_WIDTH-1:0]			 Status_REG_o;//-- ?????? ???????????? (???? ????? ??? ????????)
	wire 									 Status_REG_CS;
	wire 									 Status_REG_ES_d;
	wire 									 Status_REG_ES_o;

	wire 									 RxLostPackcet_ES_o;
	wire 									 RxLostPacket_node;
	wire [WORD_WIDTH-1:0]			 RxLostPacket_Cnt_o;
	wire [WORD_WIDTH-1:0]			 RxLostPacket_Cnt_REG_o;
	wire 									 RxLostPacket_Cnt_REG_CS;
	wire 									 RxLostPacket_Cnt_REG_ES_d;
	wire 									 RxLostPacket_Cnt_REG_ES_o;

	wire 									 TxByte_Cnt_cnt_en;
	wire [RxByte_Cnt_Width:0]		 TxByte_Cnt_o;
	wire [7:0] 							 TxRAM_q_a;
	wire [15:0]							 TxRAM_q_b;
	wire [RxByte_Cnt_Width-2:0]	 Tx_RAM_Address_Bus;
	wire [WORD_WIDTH-1:0]			 Tx_RAM_Data_Bus;
	wire 									 TxRAM_CS;
	wire 									 TxRQ_Reset;
	wire 									 TxRQ_Reset_ES_o;
	wire 									 TxRQ_SRFF_r;
	wire 									 TxRQ_SRFF_o;
	wire 									 InternalTxStart;
	wire 									 AnswerTxStart;
	wire 									 AnswerTxStart_tmp_d;
	wire 									 AnswerTxStart_tmp_o;
	wire 									 AnswerTxStart_SR_o;
	wire 									 AnswerTxStart_ES_o;
	wire 									 TxStart;
	wire 									 TxStart_ES_o;

	wire [WORD_WIDTH-1:0]			 PacketLenghts_to_be_transmitted_Reg_o; 
	wire 									 PacketLenghts_to_be_transmitted_Reg_CS;
	wire [WORD_WIDTH-1:0]			 PacketLenghts_DataBus;
	wire 									 PacketLenghts_to_be_transmitted_Reg_EN;

	wire 									 TxRAM_wren;

//	type Module_MAC is array (0 to 2) of std_logic_vector (WORD_WIDTH-1:0)
//	type Module_IP is array (0 to 1) of std_logic_vector (WORD_WIDTH-1:0)

	wire [2:0]							 Module_MAC_Reg_en;
	wire [2:0]							 Module_MAC_Reg_o [WORD_WIDTH-1:0];
	wire [2:0]							 Module_MAC_Reg_CS;
	wire 									 Module_IP_Reg_en;
	wire [1:0]							 Module_IP_Reg_o [WORD_WIDTH-1:0];
	wire [1:0]							 Module_IP_Reg_CS;
	wire 									 Port_Reg_en;
	wire [WORD_WIDTH-1:0]			 Port_Reg_o;
	wire 									 Port_Reg_CS;

	wire 									 RxWordRecive_Reg_en;
	wire [WORD_WIDTH-1:0]			 RxWordRecive_Reg_o;
	wire 									 Rx_Packet_Lenght_Reg_en;
	wire [WORD_WIDTH-1:0]			 Rx_Packet_Lenght_Reg_o;
	wire [WORD_WIDTH-1:0]			 Rx_Packet_Lenght_Reg_i;
	wire 									 ParcerCycle_SRFF_s;
	wire 									 ParcerCycle_SRFF_o;
	wire 									 ParcerCycleEnd_d;
	wire 									 ParcerCycleEnd_o;

//--	wire 								 MAC_Decoder; Eth_MAC_Decoder
	wire [10:0]							 MAC_Decoder_Rx_Addr;
	wire [10:0]							 MAC_Decoder_Tx_Addr;
	wire [WORD_WIDTH-1:0]			 MAC_Decoder_Tx_Data;
	wire 									 MAC_Decoder_Tx_Word_Strobe;
	wire 									 MAC_Decoder_Rx_Parcer_RQ;
	
	wire 									 MAC_Decoder_Rx_Error_MAC;
	wire 									 MAC_Decoder_Next_Parcer;
	wire 									 MAC_Decoder_Next_Parcer_ES_o;
	wire 									 MAC_Rx_Parcer_in_progress;

	wire [2:0]							 Source_MAC_Reg_en;
	wire [2:0]							 Source_MAC_Reg_o [WORD_WIDTH-1:0];
	
//--	wire 									 ARP_Decoder;Eth_ARP_Decoder
	wire [10:0]							 ARP_Decoder_Rx_Addr;
	wire [10:0]							 ARP_Decoder_Tx_Addr;
	wire [WORD_WIDTH-1:0]			 ARP_Decoder_Tx_Data;
	wire 									 ARP_Decoder_Tx_Word_Strobe;
	
	wire 									 ARP_Decoder_Rx_IP_Error;
	wire 									 ARP_Decoder_Rx_NOT_RQ;
	wire 									 ARP_Decoder_Tx_Start;
	wire 									 ARP_Decoder_Rx_TRUE_RQ;
	wire 									 ARP_Decoder_Rx_Parcer_in_progress;
	
	wire 									 ARP_Decoder_Test;

//--	wire 									 IPv4_Decoder; Eth_IPv4_Decoder
	wire [10:0]							 IPv4_Decoder_Rx_Addr;
	wire [10:0]							 IPv4_Decoder_Tx_Addr;
	wire [WORD_WIDTH-1:0]			 IPv4_Decoder_Tx_Data;
	wire 									 IPv4_Decoder_Tx_Word_Strobe;
	
	wire [15:0]							 IPv4_Decoder_Identification;
	wire 									 IPv4_Decoder_Rx_Error_IP;
	wire 									 IPv4_Decoder_Rx_NOT_RQ;
	wire 									 IPv4_Decoder_Tx_Start;
	wire 									 IPv4_Decoder_Rx_TRUE_RQ;
	wire 									 IPv4_Decoder_Rx_Parcer_in_progress;
	
	wire 									 IPv4_Decoder_AccessRequest;
	wire 									 IPv4_Decoder_DirectOut;
	wire [15:0]							 IPv4_Decoder_AddrBusOut;
	
	wire 									 IPv4_Decoder_test;
	
//--	wire 									 Raw_Decoder; Eth_Raw_Decoder
	wire [10:0]							 Raw_Decoder_Rx_Addr;
	wire [10:0]							 Raw_Decoder_Tx_Addr;
	wire [WORD_WIDTH-1:0]			 Raw_Decoder_Tx_Data;
	wire 									 Raw_Decoder_Tx_Word_Strobe;

	wire 									 Raw_Decoder_Rx_Error_IP;
	wire 									 Raw_Decoder_Tx_Start;
	wire 									 Raw_Decoder_Rx_Parcer_in_progress;
	wire 									 Raw_Decoder_Rx_TRUE_RQ;
	wire 									 Raw_Decoder_Rx_NOT_RQ;
	wire [WORD_WIDTH-1:0]			 Raw_Decoder_Identification;
	
	wire 									 Raw_Decoder_AccessRequest;
	wire 									 Raw_Decoder_DirectOut;
	wire [15:0]							 Raw_Decoder_AddrBusOut;
	wire 									 Raw_Decoder_test;
	
//--	wire 									 CCCD_Decoder: Eth_CCCD_Decoder
	wire [10:0]							 CCCD_Decoder_Rx_Addr;
	wire [10:0]							 CCCD_Decoder_Tx_Addr;
	wire [WORD_WIDTH-1:0]			 CCCD_Decoder_Tx_Data;
	wire 									 CCCD_Decoder_Tx_Word_Strobe;

	wire 									 CCCD_Decoder_Rx_Error_IP;
	wire 									 CCCD_Decoder_Tx_Start;
	wire 									 CCCD_Decoder_Rx_Parcer_in_progress;
	wire 									 CCCD_Decoder_Rx_TRUE_RQ;
	wire 									 CCCD_Decoder_Rx_NOT_RQ;
	wire [WORD_WIDTH-1:0]			 CCCD_Decoder_Identification;

	wire 									 CCCD_Decoder_AccessRequest;
	wire 									 CCCD_Decoder_DirectOut;
	wire [15:0]							 CCCD_Decoder_AddrBusOut;
	wire 									 CCCD_Decoder_test;

	wire 									 DirectOut;
	wire [2:0]							 SelDirPar;
	
	wire 									 Test_Reg_en;
	wire [WORD_WIDTH-1:0]			 Test_Reg_o;
	wire 									 Test_Reg_CS;

	wire [15:0]							 DataBusOut_tmp;

//--	wire 									 IPv4_CheckSum_Ctrl				: IPv4_checkSum;
	wire [10:0]							 IPv4_CheckSum_Ctrl_Rx_Addr;
	wire [10:0]							 IPv4_CheckSum_Ctrl_Tx_Addr;
	wire [WORD_WIDTH-1:0]			 IPv4_CheckSum_Ctrl_Tx_Data;
	wire 									 IPv4_CheckSum_Ctrl_Tx_Word_Strobe;
	wire 									 IPv4_CheckSum_Ctrl_Complete;
	
	wire [15:0]							 Rx_Packet_Cnt_o;
	wire [15:0]							 Tx_Packet_Cnt_o;
	wire 									 Rx_Packet_Cnt_Reset;
	wire 									 Tx_Packet_Cnt_Reset;

	wire 									 RxTxCycle_IntStart_o;

	wire 									 In_FIFO_e;
	wire 									 In_FIFO_wr_en;
	wire 									 In_FIFO_rd_en;
	wire [9:0]							 In_FIFO_o;
	wire 									 Copy_Byte_Strobe;
	wire 									 FIFO_to_RxRAM_Copy;// -- ?????? ?????????? ????????????? ?????? ????????? ??????? ???????? ?????? ? ?????? ?????? ? ???????? ??????
	wire 									 FIFO_to_RxRAM_Copy_ES_o;
	wire 									 RAM_Ptr_Packet_End;// -- ?????? ????????? ??????????? ?????? ?? FIFO ? ???????? ?????? ? ??????? ????????? ???????????? ??????

	wire 									 Packet_Good_End_ES_o;// -- ????????? ????????? ???????? ?????? (CRC ???????)
	wire 									 Packet_bad_End_ES_o;// -- ????????? ????????? ??????? ?????? (CRC ?????????)

	wire 									 Out_FIFO_e;
	wire 									 Out_FIFO_f;
	wire [9:0]							 Out_FIFO_o;
	wire 									 Tx_FIFO_RQ_s;
	wire 									 Tx_FIFO_RQ_r;
	wire 									 Tx_FIFO_RQ_o;
//	-- ???????? ?????????? ????? ????????; ????????????? ??????
	wire [7:0]							 GuardTime_Cnt_o;
	wire 									 GuardTime_Cnt_Rst;
	wire 									 GuardTime_s;
	wire 									 GuardTime_o;
	wire 									 Eth_Tx_In_Progress_del;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------

//-------------------------------------------- Input FIFO --------------------------------------------
//--	RxIntStart_out = RxIntStart_ES.q;--RxIntStart;
	assign RxIntStart_out = RxIntStart_ES_o;
	
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
		.R		(RAM_Ptr_Packet_End | RxIntStart_ES_o),
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
		.WIDTH (RxByte_Cnt_Width+1)
	) RxByte_Cnt
	(
		.clock 	(clock),//--Quarts,--
		.cnt_en	(FIFO_to_RxRAM_Copy),
		.sload	(RxIntStart_ES_o),
		.data		(MASS_RAM_BYTE_Tx_Num - 1),
		.sclr		(SetRxReadyToRecive | Packet_bad_End_ES_o),
		.q			(RxByte_Cnt_o)
		);

//--	IF 	(RxByte_Cnt.q[] == PacketLenghts_at_signaling_layer-1) --Check to not overwrite RAM at ubnormal length packets
//--		THEN RAM_Overflow	= VCC;
//--		ELSE RAM_Overflow	= GND;
//--	end
	assign RAM_Overflow = (RxByte_Cnt_o == (PacketLenghts_at_signaling_layer - 1)) ? 1'b1 : 1'b0;

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
	always @(posedge System_Clock)
	begin
		RxReadyToRecive <= RxReadyToRecive_o;
	end

//	--RxIntStart_ES.(d,clk)     = (RxIntStart & DFF(.d=RxReadyToRecive.q,.clk=Clock), Clock);
	Edge_Sensing RxIntStart_ES
	(
		.clk	(Clock),
		.d		(RxIntStart & RxReadyToRecive), 
		.q		(RxIntStart_ES_o)
		);

//--	RxIntStart_node = DFF(.d=RxIntStart_ES.q,.clk=Clock);
	always @(posedge System_Clock)
	begin
		RxIntStart_node <= RxIntStart_ES_o;
	end

//--	IF((RxPacket_in_progress == VCC) & (RxReadyToRecive.q == GND)) THEN RxLostPacket_node = VCC; -- 
//--																 ELSE RxLostPacket_node = GND; -- 
//--	end
	assign RAM_Overflow = ((RxPacket_in_progress = 1'b1) & (RxReadyToRecive = 1'b0)) ? 1'b1 : 1'b0;

//--	RxRAM.data_a(7:0) = In_FIFO_o(7:0);  -- 
//--	RxRAM.(address_a(RxByte_Cnt_Width-1:0)   , clock_a, wren_a                       ) =
//--		(RxByte_Cnt.q(RxByte_Cnt_Width-1:0], Clock  , FIFO_to_RxRAM_Copy & !RAM_Overflow_SRFF.q); 
//--
//--	RxRAM.(address_b(RxByte_Cnt_Width-2:0)     , clock_b  , data_b(15:8)   , data_b(7:0)     , wren_b  ) =
//--		(Rx_RAM_Address_Bus[], BUS_Clock, DataBus_In(7:0], DataBus_In(15:8], RxRAM_CS & DataBusStrobe & DirectIn & Select); 
	EthBufferRAM2048 RxRAM
	(
		.dina		(In_FIFO_o),
		.addra	(RxByte_Cnt_o[RxByte_Cnt_Width-1:0]),
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
		.WIDTH (WORD_WIDTH) 
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

//	-------------------------------------------- Output FIFO --------------------------------------------
//--	Out_FIFO.data(7:0) 	=	TxRAM.q_a();
//--
//--	Out_FIFO.data(8) 		=	GND; 
//--	Out_FIFO.data(9) 		=	DFF(.d=TxRQ_Reset_ES.q,.clk=Clock); -- ?????? ????????? ??????
//--
//--	Out_FIFO.clock			=	Clock;
//--	Out_FIFO.wrreq  		=	DFF(.d=TxRQ_SRFF.q,.clk=Clock);
//--	Out_FIFO.rdreq  		=	Byte_Strobe_Tx;    

	Eth_In_FIFO4kb Out_FIFO
	(
		.din		({TxRQ_Reset_ES_o, 1'b0, TxRAM_q_a}), //--DFF(.d=TxRQ_Reset_ES.q,.clk=Clock);
		.clk		(Clock),
		.wr_en	(TxRQ_SRFF_o), //--DFF(.d=TxRQ_SRFF.q,.clk=Clock);
		.rd_en	(Byte_Strobe_Tx),
		.dout		(Out_FIFO_o),
		.full		(Out_FIFO_f),
		.empty	(Out_FIFO_e)
		);

//--	Tx_FIFO_RQ				=	SRFF(.S=!Out_FIFO.empty & !Eth_Tx_In_Progress & !GuardTime,.clk=Clock,.R=Out_FIFO.q(9) & Byte_Strobe_Tx);--DFF(.d=!Out_FIFO.empty,.clk=Clock);--SRFF(.S=!Out_FIFO.empty & !Tx_FIFO_RQ,.clk=Clock,.R=Out_FIFO.q(9]); 
	SRFF Tx_FIFO_RQ
	(
		S		((!Out_FIFO_e) & (!Eth_Tx_In_Progress) & (!GuardTime_o)),
		CLK	(Clock),
		R		(Out_FIFO_o[9] & Byte_Strobe_Tx),
		q		(Tx_FIFO_RQ_o)
		);

	assign Transmit_of_Data_RQ	= Tx_FIFO_RQ_o;
	assign Tx_Data					= Out_FIFO_o[7:0];

//	-- ???????? ?????????? ????? ????????
//--	GuardTime_Cnt.(clock,cnt_en,sclr) = (Clock, GuardTime, GuardTime_Cnt_Rst);
	V_Counter
	#(
		.width (8)
	) GuardTime_Cnt
	(
		.clock	(Clock),//--Quarts,--
		.clk_en	(1'b1),
		.cnt_en	(GuardTime_o),
		.sclr		(GuardTime_Cnt_Rst),
		.q			(GuardTime_Cnt_o)
		);
	assign GuardTime_Cnt_Rst = (GuardTime_Cnt_o > 99)? 1'b1 : 1'b0;

//--	GuardTime = SRFF(.S=!Eth_Tx_In_Progress & DFF(.d=Eth_Tx_In_Progress,.clk=Clock),.clk=Clock,.R=GuardTime_Cnt_Rst);
	always @(posedge clock)
	begin
		Eth_Tx_In_Progress_del <= Eth_Tx_In_Progress;
	end
	
	SRFF GuardTime
	(
		.S		((!Eth_Tx_In_Progress) & Eth_Tx_In_Progress_del),
		.CLK	(Clock),
		.R		(GuardTime_Cnt_Rst),
		.q		(GuardTime_o)
		);

//	------------------------------------------- Tx section ------------------------------------------------- 
	assign TxStart = InternalTxStart | AnswerTxStart;
//--	TxRQ_SRFF.(S,clk,R) = (TxStart ,Clock,TxRQ_Reset_ES.q | Reset); 
	SRFF TxRQ_SRFF
	(
		.S		(TxStart),
		.CLK	(Clock),
		.R		(TxRQ_Reset_ES_o | Reset),
		.q		(TxRQ_SRFF_o)
		);
//--	TxByte_Cnt.(clock,clk_en,cnt_en,sclr) = (Clock,VCC, TxRQ_SRFF.q & !Out_FIFO.full, TxStart);
	V_Counter
	#(
		.width (RxByte_Cnt_Width+1)
	) TxByte_Cnt
	(
		.clock	(Clock),//--Quarts,--
		.clk_en	(1'b1),
		.cnt_en	(TxRQ_SRFF_o & !Out_FIFO_f),
		.sclr		(TxStart),
		.q			(TxByte_Cnt_o)
		);
	assign TxRQ_Reset = ((TxByte_Cnt_o >= PacketLenghts_at_signaling_layer-1) | (TxByte_Cnt_o >= PacketLenghts_to_be_transmitted_Reg_o[RxByte_Cnt_Width:0])) ? 1'b1 : 1'b0;

//--	TxRQ_Reset_ES.(d,clk) = (TxRQ_Reset,Clock);
	Edge_Sensing TxRQ_Reset_ES
	(
		.clk	(BUS_Clock),
		.d		(TxRQ_Reset),
		.q		(TxRQ_Reset_ES_o)
		);

//--	TxRAM.data_a(7:0) = GND;  
//--	TxRAM.(address_a(RxByte_Cnt_Width-1:0)   , clock_a, wren_a) =
//--		(TxByte_Cnt.q(RxByte_Cnt_Width-1:0], Clock  , GND   ); 
//--
//--	TxRAM.(address_b(RxByte_Cnt_Width-2:0)         , clock_b  , data_b()         , wren_b  ) =
//--		(Tx_RAM_Address_Bus(RxByte_Cnt_Width-2:0], BUS_Clock, Tx_RAM_Data_Bus[], TxRAM_wren);
	EthBufferRAM2048 TxRAM
	(
		dina		({8{1'b0}}),
		addra		(TxByte_Cnt_o[RxByte_Cnt_Width-1:0]),
		clka		(Clock),
		wea		(1'b0),
		douta		(TxRAM_q_a),
		
		dinb		(Tx_RAM_Data_Bus),
		addrb		(Tx_RAM_Address_Bus[RxByte_Cnt_Width-2:0]),
		clkb		(BUS_Clock),
		web		(TxRAM_wren),
		doutb		(TxRAM_q_b)
		);

//	-------------------------------------------------------------------------------------------------------------------  

//	-----------------------------???????? ?????? ??? ???????? ?????? ?? ???????? ??????--------------------------------  
//--	RxIntRAM.(address_a()     	 , clock_a  , data_a(15:8)   , data_a(7:0)     , wren_a  ) =
//--			(Rx_RAM_Address_Bus[], BUS_Clock  , DataBus_In(7:0], DataBus_In(15:8], GND); 
//--	RxIntRAM.(address_b() 		 			  , clock_b  , data_b(15:8)   , data_b(7:0)     , wren_b  ) =
//--			(AddrBus_In(RxByte_Cnt_Width-2:0], BUS_Clock, DataBus_In(7:0], DataBus_In(15:8], RxIntRAM_CS & DataBusStrobe & DirectIn & Select); 

	RAM2048_2p RxIntRAM
	(
		dina		({DataBus_In[7:0],DataBus_In[15:8]}),
		addra		(Rx_RAM_Address_Bus),
		clka		(BUS_Clock),
		wea		(1'b0),
		douta		(RxIntRAM_q_a),
		
		dinb		({DataBus_In[7:0],DataBus_In[15:8]}),
		addrb		(AddrBus_In[RxByte_Cnt_Width-2:0]),
		clkb		(BUS_Clock),
		web		(RxIntRAM_CS & DataBusStrobe & DirectIn & Select_i),
		doutb		(RxIntRAM_q_b)
		);

//--	ParcerCycle_IntStart.(S,clk,R) = (RxIntStart_ES.q,Clock, ParcerCycleEnd);
	SRFF ParcerCycle_IntStart
	(
		.S		(RxIntStart_ES_o),
		.CLK	(Clock),
		.R		(ParcerCycleEnd_o),
		.q		(ParcerCycle_IntStart_o)
		);
	assign Buffer_RAM_Data = (ParcerCycle_IntStart_o) ? RxIntRAM_q_a : RxRAM_q_b;
//	-------------------------------------------------------------------------------------------------------------------  

//	------------------- ?????? ???????, ?????????? ??????????? ? ??? 
//--	ParcerCycle_SRFF.(S,clk,R) = ((RAM_Ptr_Packet_End & RxReadyToRecive.q) | RxIntStart_ES.q,Clock,ParcerCycleEnd);   -- ???? ?? ???????? ?????????? ?????? ?? ????? ???????? ?????
	SRFF ParcerCycle_SRFF
	(
		.S		((RAM_Ptr_Packet_End & RxReadyToRecive_o) | RxIntStart_ES_o),
		.CLK	(Clock),
		.R		(ParcerCycleEnd_o),
		.q		(ParcerCycle_SRFF_o)
		);
//--	ParcerCycleEnd = Edge_Sensing_Sync(.d=(MAC_Decoder.Rx_Error_MAC | ARP_Decoder.Rx_TRUE_RQ | IPv4_Decoder.Rx_Error_IP | ARP_Decoder.Rx_IP_Error | IPv4_CheckSum_Ctrl.IPv4_CheckSum_Complete | CCCD_Decoder.Rx_TRUE_RQ | Raw_Decoder.Rx_TRUE_RQ | Raw_Decoder.Rx_NOT_RQ),.clk=Clock);
	Edge_Sensing ParcerCycleEnd
	(
		.clk	(Clock),
		.d		(MAC_Decoder_Rx_Error_MAC | ARP_Decoder_Rx_TRUE_RQ | IPv4_Decoder_Rx_Error_IP | ARP_Decoder_Rx_IP_Error | IPv4_CheckSum_Ctrl_Complete | CCCD_Decoder_Rx_TRUE_RQ | Raw_Decoder_Rx_TRUE_RQ | Raw_Decoder_Rx_NOT_RQ),
		.q		(ParcerCycleEnd_o)
		);
//--	SetRxReadyToRecive_Parcer = Edge_Sensing_Sync(.d=TxRQ_Reset_ES.q | MAC_Decoder.Rx_Error_MAC | ARP_Decoder.Rx_IP_Error | IPv4_Decoder.Rx_Error_IP | Raw_Decoder.Rx_NOT_RQ,.clk=Clock);
	Edge_Sensing SetRxReadyToRecive_Parcer
	(
		.clk	(Clock),
		.d		(TxRQ_Reset_ES_o | MAC_Decoder_Rx_Error_MAC | ARP_Decoder_Rx_IP_Error | IPv4_Decoder_Rx_Error_IP | Raw_Decoder_Rx_NOT_RQ),
		.q		(SetRxReadyToRecive_Parcer_o)
		);

//	-- ??????? ?? ?????? ???????? ?????? ?? Tx ??????
//--	AnswerTxStart_ = Edge_Sensing_Sync(.d=ARP_Decoder.Tx_Start | IPv4_CheckSum_Ctrl.IPv4_CheckSum_Complete | CCCD_Decoder.Tx_Start | Raw_Decoder.Tx_Start,.clk=Clock);
	Edge_Sensing AnswerTxStart_tmp
	(
		.clk	(Clock),
		.d		(ARP_Decoder_Tx_Start | IPv4_CheckSum_Ctrl_Complete | CCCD_Decoder_Tx_Start | Raw_Decoder_Tx_Start),
		.q		(AnswerTxStart_tmp_o)
		);
//--	IF(TxRQ_SRFF_o = 1'b1) THEN AnswerTxStart = Edge_Sensing_Sync(.d=!SRFF(.S=AnswerTxStart_,.clk=Clock,.R=TxRQ_Reset),.clk=Clock); -- ????? ?????????, ??????? ????? ??????????? ??????????
//--					ELSE AnswerTxStart = AnswerTxStart_;
//--	end
	SRFF AnswerTxStart_SR
	(
		.S		(AnswerTxStart_tmp_o),
		.CLK	(Clock),
		.R		(TxRQ_Reset),
		.q		(AnswerTxStart_SR_o)
		);
	Edge_Sensing AnswerTxStart_ES
	(
		.clk	(Clock),
		.d		(!AnswerTxStart_SR_o),
		.q		(AnswerTxStart_ES_o)
		);
	assign AnswerTxStart = (TxRQ_SRFF_o) ? AnswerTxStart_ES_o : AnswerTxStart_tmp_o;

	assign Eth_Tx_End = TxRQ_Reset_ES_o & RxTxCycle_IntStart_o;// -- ?????????????? ???????-????????? ??????????? ??????
//--	RxTxCycle_IntStart.(S,clk,R) = (RxIntStart_ES.q,Clock, TxRQ_Reset_ES.q);	-- ?????? ??? ???????? ?????? ?? ???????? ??????
	SRFF RxTxCycle_IntStart
	(
		.S		(RxIntStart_ES_o),
		.CLK	(Clock),
		.R		(TxRQ_Reset_ES_o),
		.q		(RxTxCycle_IntStart_o)
		);

//	-- MAC ????? ???????????? ? ???????????? ?????? ?? ???????? ????????? ?? Ethernet 
//--	MAC_Decoder.Clock = Clock;
//--	MAC_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset) 	 = (Buffer_RAM_Data()		 , RAM_Ptr_Packet_End | RxIntStart_ES.q, SetRxReadyToRecive);
//--	MAC_Decoder.(MAC_Addr0_(15:8], MAC_Addr0_(7:0]) = (Module_MAC_Reg(0].q(7:0], Module_MAC_Reg(0].q(15:8]);
//--	MAC_Decoder.(MAC_Addr1_(15:8], MAC_Addr1_(7:0]) = (Module_MAC_Reg(1].q(7:0], Module_MAC_Reg(1].q(15:8]);
//--	MAC_Decoder.(MAC_Addr2_(15:8], MAC_Addr2_(7:0]) = (Module_MAC_Reg(2].q(7:0], Module_MAC_Reg(2].q(15:8]);

	Eth_MAC_Decoder MAC_Decoder
	(
		.Clock						(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data						(Buffer_RAM_Data),
		.Rx_Addr						(MAC_Decoder_Rx_Addr),
		.Rx_Parcer_RQ				(RAM_Ptr_Packet_End | RxIntStart_ES_o),

		.Tx_Addr						(MAC_Decoder_Tx_Addr),
		.Tx_Data						(MAC_Decoder_Tx_Data),
		.Tx_Word_Strobe			(MAC_Decoder_Tx_Word_Strobe),

		.MAC_Addr0_i				({Module_MAC_Reg_o[0][7:0], Module_MAC_Reg_o[0][15:8]}),
		.MAC_Addr1_i				({Module_MAC_Reg_o[1][7:0], Module_MAC_Reg_o[1][15:8]}),
		.MAC_Addr2_i				({Module_MAC_Reg_o[2][7:0], Module_MAC_Reg_o[2][15:8]}),

		.Reset						(SetRxReadyToRecive),

		.Rx_Error_MAC				(MAC_Decoder_Rx_Error_MAC),
		.Next_Parcer				(MAC_Decoder_Next_Parcer),
		.Rx_Parcer_in_progress	(MAC_Rx_Parcer_in_progress)
		);

//	-- ????? MAC-?????? ?????????? ??????????? ???????
//--	FOR i IN 0 TO 2 GENERATE  
//--		Source_MAC_Reg(i].(data[],clock, load, enable) = (RxRAM.q_b[],Clock,VCC, (Rx_RAM_Address_Bus() == 3+i) & MAC_Decoder_Tx_Word_Strobe);
//--	END GENERATE;
	
	genvar i;
	generate
		for (i=0; i <= 2; i=i+1) 
		begin: Source_MAC_Reg_i
			assign Source_MAC_Reg_en[i] = ((Rx_RAM_Address_Bus == 3+i) & (MAC_Decoder_Tx_Word_Strobe)) ? 1'b1 : 1'b0;
			ShiftReg
			#(
				.WIDTH (WORD_WIDTH) 
			) Source_MAC_Reg
			(
				.clock	(Clock),
				.data		(RxRAM_q_b),
				.enable	(Source_MAC_Reg_en[i]),
				.q			(Source_MAC_Reg_o[i])
			);
		end
	endgenerate
//	-- ??????????? ????? ???????? ????(???????????? 4 ????? CRC) ? ????????????? ? 16?????? ?????
//--	IF RxTxCycle_IntStart.q == VCC THEN
//--		Rx_Packet_Lenght_Reg.(data(15:RxByte_Cnt_Width],data(RxByte_Cnt_Width-1:0]) = (B"0000",RxByte_Cnt.q(RxByte_Cnt_Width:1]-(HEADER_LENGTH_WORDS-1));
//--							  ELSE
//--		Rx_Packet_Lenght_Reg.(data(15:RxByte_Cnt_Width],data(RxByte_Cnt_Width-1:0]) = (B"0000",RxByte_Cnt.q(RxByte_Cnt_Width:1]-(HEADER_LENGTH_WORDS-1)-3);
//--	end
//--	Rx_Packet_Lenght_Reg.(clock,enable,load) = (Clock,RAM_Ptr_Packet_End | RxIntStart_node,VCC); 

	assign Rx_Packet_Lenght_Reg_i = (RxTxCycle_IntStart_o) ? ({4'b0000,RxByte_Cnt_o[RxByte_Cnt_Width:1]-(HEADER_LENGTH_WORDS-1)})
																			 :	({4'b0000,RxByte_Cnt_o[RxByte_Cnt_Width:1]-(HEADER_LENGTH_WORDS-1)-3});
	ShiftReg
		#(
			.WIDTH	(WORD_WIDTH) 
		) Rx_Packet_Lenght_Reg
		(
			.clock	(Clock),
			.data		(Rx_Packet_Lenght_Reg_i),
			.enable	(RAM_Ptr_Packet_End | RxIntStart_node),
			.q			(Rx_Packet_Lenght_Reg_o)
		);

//--	RxWordRecive_Reg.(data(15:RxByte_Cnt_Width],data(RxByte_Cnt_Width-1:0]) = (B"0000",RxByte_Cnt.q(RxByte_Cnt_Width:1]);
//--	RxWordRecive_Reg.(clock,enable,load) = (Clock,RAM_Ptr_Packet_End | RxIntStart_node,VCC); 
	ShiftReg 
		#(
			.WIDTH	(WORD_WIDTH) 
		) RxWordRecive_Reg
		(
			.clock	(Clock),
			.data		({{(WORD_WIDTH-1-RxByte_Cnt_Width){1'b0}}, RxByte_Cnt_o[RxByte_Cnt_Width:1]}),
			.enable	(RAM_Ptr_Packet_End | RxIntStart_node),
			.q			(RxWordRecive_Reg_o)
		);

//--	ARP_Decoder.Clock = Clock;
//--	ARP_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset) = (Buffer_RAM_Data[],  MAC_Decoder.Next_Parcer, SetRxReadyToRecive);
//--	ARP_Decoder.(MAC_Addr0_(15:8], MAC_Addr0_(7:0]) = (Module_MAC_Reg(0].q(7:0], Module_MAC_Reg(0].q(15:8]);
//--	ARP_Decoder.(MAC_Addr1_(15:8], MAC_Addr1_(7:0]) = (Module_MAC_Reg(1].q(7:0], Module_MAC_Reg(1].q(15:8]);
//--	ARP_Decoder.(MAC_Addr2_(15:8], MAC_Addr2_(7:0]) = (Module_MAC_Reg(2].q(7:0], Module_MAC_Reg(2].q(15:8]);
//--	ARP_Decoder.(IP_Addr0_(15:8], IP_Addr0_(7:0]) = (Module_IP_Reg(0].q(7:0], Module_IP_Reg(0].q(15:8]);
//--	ARP_Decoder.(IP_Addr1_(15:8], IP_Addr1_(7:0]) = (Module_IP_Reg(1].q(7:0], Module_IP_Reg(1].q(15:8]);
	Eth_ARP_Decoder ARP_Decoder
	(
		.Clock					(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data					(Buffer_RAM_Data),
		.Rx_Addr					(ARP_Decoder_Rx_Addr),
		.Rx_Parcer_RQ			(MAC_Decoder_Next_Parcer),

		.Tx_Addr					(ARP_Decoder_Tx_Addr),
		.Tx_Data					(ARP_Decoder_Tx_Data),
		.Tx_Word_Strobe		(ARP_Decoder_Tx_Word_Strobe),

		.MAC_Addr0_i			({Module_MAC_Reg_o[0][7:0],Module_MAC_Reg_o[0][15:8]}),
		.MAC_Addr1_i			({Module_MAC_Reg_o[1][7:0],Module_MAC_Reg_o[1][15:8]}),
		.MAC_Addr2_i			({Module_MAC_Reg_o[2][7:0],Module_MAC_Reg_o[2][15:8]}),
		.IP_Addr0_i				({Module_IP_Reg_o[0][7:0],Module_IP_Reg_o[0][15:8]}),
		.IP_Addr1_i				({Module_IP_Reg_o[1][7:0],Module_IP_Reg_o[1][15:8]}),

		.Reset						(SetRxReadyToRecive),

		.Rx_IP_Error				(ARP_Decoder_Rx_IP_Error),
		.Rx_NOT_RQ					(ARP_Decoder_Rx_NOT_RQ),
		.Tx_Start					(ARP_Decoder_Tx_Start),
		.Rx_TRUE_RQ					(ARP_Decoder_Rx_TRUE_RQ),
		.Rx_Parcer_in_progress	(ARP_Decoder_Rx_Parcer_in_progress),
		.Test							(ARP_Decoder_Test)
		);

//--	IPv4_Decoder.Clock = Clock;
//--	IPv4_Decoder.Rx_NUM_Data() = RxWordRecive_Reg.q(RxByte_Cnt_Width-2:0]-2;
//--	IPv4_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[],  ARP_Decoder.Rx_NOT_RQ, SetRxReadyToRecive);
//--	IPv4_Decoder.(IP_Addr0_(15:8], IP_Addr0_(7:0]) = (Module_IP_Reg(0].q(7:0], Module_IP_Reg(0].q(15:8]);
//--	IPv4_Decoder.(IP_Addr1_(15:8], IP_Addr1_(7:0]) = (Module_IP_Reg(1].q(7:0], Module_IP_Reg(1].q(15:8]);
//--	IPv4_Decoder.(Port(15:8], Port(7:0]) 			= (Port_Reg.q(7:0], Port_Reg.q(15:8]);
//--	IPv4_Decoder.DataBus_In() = DataBus_In();
//--	IPv4_Decoder.(AccessGranted, DataBusStrobe) = (AccessGranted, DataBusStrobe);
	Eth_IPv4_Decoder IPv4_Decoder
	(
		.Clock					(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data				(Buffer_RAM_Data),
		.Rx_Addr				(IPv4_Decoder_Rx_Addr),
		.Rx_Parcer_RQ		(ARP_Decoder_Rx_NOT_RQ),
		.Rx_NUM_Data			(RxWordRecive_Reg_o[RxByte_Cnt_Width-2:0]-2),

		.Tx_Addr				(IPv4_Decoder_Tx_Addr),
		.Tx_Data				(IPv4_Decoder_Tx_Data),
		.Tx_Word_Strobe	(IPv4_Decoder_Tx_Word_Strobe),

		.IP_Addr0_i			({Module_IP_Reg_o[0][7:0],Module_IP_Reg_o[0][15:8]}),
		.IP_Addr1_i			({Module_IP_Reg_o[1][7:0],Module_IP_Reg_o[1][15:8]}),
		.Port_i				({Port_i[7:0],Port_i[15:8]}),
		
		.Identification	(IPv4_Decoder_Identification),

		.Reset				(SetRxReadyToRecive),

		.Rx_Error_IP				(IPv4_Decoder_Rx_Error_IP),
		.Rx_NOT_RQ					(IPv4_Decoder_Rx_NOT_RQ),
		.Tx_Start					(IPv4_Decoder_Tx_Start),
		.Rx_TRUE_RQ					(IPv4_Decoder_Rx_TRUE_RQ),
		.Rx_Parcer_in_progress	(IPv4_Decoder_Rx_Parcer_in_progress),
		
		.AccessRequest				(IPv4_Decoder_AccessRequest),
		.AccessGranted				(AccessGranted),
		.DirectOut					(IPv4_Decoder_DirectOut),
		.AddrBusOut					(IPv4_Decoder_AddrBusOut),
		.DataBus_In					(DataBus_In),// -- ???????????? ? ?????????? ???? ? ?????? ??????
		.DataBusStrobe				(DataBusStrobe),// -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)

		.test							(IPv4_Decoder_test)
		);
		
//--	IPv4_CheckSum_Ctrl.Clock = Clock;
//--	IPv4_CheckSum_Ctrl.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[], IPv4_Decoder.Tx_Start, SetRxReadyToRecive);
//--	IPv4_CheckSum_Ctrl.IP_ID() = IPv4_Decoder.Identification();
	IPv4_CheckSum IPv4_CheckSum_Ctrl
	(
		.Clock				(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data				(Buffer_RAM_Data),
		.Rx_Addr				(IPv4_CheckSum_Ctrl_Rx_Addr),
		.Rx_Parcer_RQ		(IPv4_Decoder_Tx_Start),

		.Tx_Addr				(IPv4_CheckSum_Ctrl_Tx_Addr),
		.Tx_Data				(IPv4_CheckSum_Ctrl_Tx_Data),
		.Tx_Word_Strobe	(IPv4_CheckSum_Ctrl_Tx_Word_Strobe),

		.Reset				(SetRxReadyToRecive),

		.IPv4_CheckSum_Complete	(IPv4_CheckSum_Ctrl_Complete),

		.IP_ID						(IPv4_Decoder_Identification)

		//-----------------
//--		Sample_Enable			(IPv4_CheckSum_Ctrl_Sample_Enable,
//--		Sum20_Reg_out			(IPv4_CheckSum_Ctrl_Sum20_Reg_out,
//--		Sum16_Reg_out			(IPv4_CheckSum_Ctrl_Sum16_Reg_out,
//--		RxParcerActive_out	(IPv4_CheckSum_Ctrl_RxParcerActive_out
		);

//--	CCCD_Decoder.Clock = Clock;
//--	CCCD_Decoder.Rx_NUM_Data() = RxWordRecive_Reg.q(RxByte_Cnt_Width-2:0);
//--	CCCD_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[],  IPv4_Decoder.Rx_NOT_RQ, SetRxReadyToRecive);
//--	CCCD_Decoder.Rx_Packet_Lenght()  = Rx_Packet_Lenght_Reg.q[];--(RxWordRecive_Reg.q[]-(HEADER_LENGTH_WORDS-1));--42);
//--	CCCD_Decoder.DataBus_In() = DataBus_In();
//--	CCCD_Decoder.(AccessGranted, DataBusStrobe) = (AccessGranted, DataBusStrobe);
	Eth_CCCD_Decoder CCCD_Decoder
	(
		.Clock							(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data						(Buffer_RAM_Data),
		.Rx_Addr						(CCCD_Decoder_Rx_Addr),
		.Rx_Parcer_RQ				(IPv4_Decoder_Rx_NOT_RQ),
		.Rx_NUM_Data				(RxWordRecive_Reg_o[RxByte_Cnt_Width-2:0]-2),

		.Tx_Addr						(CCCD_Decoder_Tx_Addr),
		.Tx_Data						(CCCD_Decoder_Tx_Data),
		.Tx_Word_Strobe			(CCCD_Decoder_Tx_Word_Strobe),

		.Rx_Error					(CCCD_Decoder_Rx_Error_IP),
		.Tx_Start					(CCCD_Decoder_Tx_Start),
		.Rx_Parcer_in_progress	(CCCD_Decoder_Rx_Parcer_in_progress),
		.Rx_TRUE_RQ					(CCCD_Decoder_Rx_TRUE_RQ),
		.Rx_NOT_RQ					(CCCD_Decoder_Rx_NOT_RQ),
		
		.Identification			(CCCD_Decoder_Identification),
		
		.Rx_Packet_Lenght			(Rx_Packet_Lenght_Reg_o),
		
		.AccessRequest				(CCCD_Decoder_AccessRequest),
		.AccessGranted				(AccessGranted),
		.DirectOut					(CCCD_Decoder_DirectOut),
		.AddrBusOut					(CCCD_Decoder_AddrBusOut),
		.DataBus_In					(DataBus_In),// -- ???????????? ? ?????????? ???? ? ?????? ??????
		.DataBusStrobe				(DataBusStrobe),//  -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)
		
		.Reset						(SetRxReadyToRecive),
		
		.test							(CCCD_Decoder_test)
		);
	
//--	Raw_Decoder.Clock = Clock;
//--	Raw_Decoder.Rx_NUM_Data() = RxWordRecive_Reg.q(RxByte_Cnt_Width-2:0);
//--	Raw_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[], CCCD_Decoder.Rx_NOT_RQ, SetRxReadyToRecive);
//--	Raw_Decoder.Rx_Packet_Lenght()  = Rx_Packet_Lenght_Reg.q[];--(RxWordRecive_Reg.q[]-(HEADER_LENGTH_WORDS-1));--42);
//--	Raw_Decoder.DIMA_Ident() = Test_Reg.q();
//--	Raw_Decoder.DataBus_In() = DataBus_In();
//--	Raw_Decoder.(AccessGranted, DataBusStrobe) = (AccessGranted, DataBusStrobe);
	Eth_Raw_Decoder Raw_Decoder
	(
		.Clock						(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data						(Buffer_RAM_Data),
		.Rx_Addr						(Raw_Decoder_Rx_Addr),
		.Rx_Parcer_RQ				(CCCD_Decoder_Rx_NOT_RQ),
		.Rx_NUM_Data				(RxWordRecive_Reg_o[RxByte_Cnt_Width-2:0]-2),

		.Tx_Addr						(Raw_Decoder_Tx_Addr),
		.Tx_Data						(Raw_Decoder_Tx_Data),
		.Tx_Word_Strobe			(Raw_Decoder_Tx_Word_Strobe),

		.Rx_Error					(Raw_Decoder_Rx_Error_IP),
		.Tx_Start					(Raw_Decoder_Tx_Start),
		.Rx_Parcer_in_progress	(Raw_Decoder_Rx_Parcer_in_progress),
		.Rx_TRUE_RQ					(Raw_Decoder_Rx_TRUE_RQ),
		.Rx_NOT_RQ					(Raw_Decoder_Rx_NOT_RQ),
		
		.Identification			(Raw_Decoder_Identification),
		
		.Rx_Packet_Lenght			(Rx_Packet_Lenght_Reg_o),
		.DIMA_Ident					(Test_Reg_o),
		
		.AccessRequest				(Raw_Decoder_AccessRequest),
		.AccessGranted				(AccessGranted),
		.DirectOut					(Raw_Decoder_DirectOut),
		.AddrBusOut					(Raw_Decoder_AddrBusOut),
		.DataBus_In					(DataBus_In),// -- ???????????? ? ?????????? ???? ? ?????? ??????
		.DataBusStrobe				(DataBusStrobe),// -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)
		
		.Reset						(SetRxReadyToRecive),
		
		.test							(Raw_Decoder_test)
		);

	assign AccessRequest	= IPv4_Decoder_AccessRequest	 | Raw_Decoder_AccessRequest	| CCCD_Decoder_AccessRequest;
	assign DirectOut		= IPv4_Decoder_DirectOut		 | Raw_Decoder_DirectOut		| CCCD_Decoder_DirectOut;
	assign AddrBusOut		= IPv4_Decoder_AddrBusOut		 | Raw_Decoder_AddrBusOut		| CCCD_Decoder_AddrBusOut;

//--	IF(ParcerCycle_SRFF.q == VCC) THEN 
//--		FOR i IN 0 TO RxByte_Cnt_Width-2 GENERATE  
//--			Tx_RAM_Address_Bus(i) = DFF(.d=MAC_Decoder.Tx_Addr(i) | ARP_Decoder.Tx_Addr(i) | IPv4_Decoder.Tx_Addr(i) | Raw_Decoder.Tx_Addr(i) | IPv4_CheckSum_Ctrl.Tx_Addr(i) | CCCD_Decoder.Tx_Addr(i],.clk=Clock);
//--		END GENERATE;
//--		Rx_RAM_Address_Bus() = MAC_Decoder.Rx_Addr() | ARP_Decoder.Rx_Addr() | IPv4_Decoder.Rx_Addr() | Raw_Decoder.Rx_Addr() | IPv4_CheckSum_Ctrl.Rx_Addr() | CCCD_Decoder.Rx_Addr();
//--		FOR i IN 0 TO 15 GENERATE 
//--			Tx_RAM_Data_Bus(i) = DFF(.d=MAC_Decoder.Tx_Data(i) | ARP_Decoder.Tx_Data(i) | IPv4_Decoder.Tx_Data(i) | Raw_Decoder.Tx_Data(i) | IPv4_CheckSum_Ctrl.Tx_Data(i) | CCCD_Decoder.Tx_Data(i],.clk=Clock);
//--		END GENERATE;
//--		TxRAM_wren				= DFF(.d=MAC_Decoder.Tx_Word_Strobe | ARP_Decoder.Tx_Word_Strobe | IPv4_Decoder.Tx_Word_Strobe | Raw_Decoder.Tx_Word_Strobe | IPv4_CheckSum_Ctrl.Tx_Word_Strobe | CCCD_Decoder.Tx_Word_Strobe,.clk=Clock);
//--	ELSE 
//--		Tx_RAM_Address_Bus(RxByte_Cnt_Width-2:0) = AddrBus_In(RxByte_Cnt_Width-2:0);
//--		Tx_RAM_Data_Bus(15:8)	= DataBus_In(7:0); Tx_RAM_Data_Bus(7:0) = DataBus_In(15:8);
//--		TxRAM_wren							= TxRAM_CS & DataBusStrobe & DirectIn & Select;
//--		Rx_RAM_Address_Bus()				= AddrBus_In(RxByte_Cnt_Width-2:0);
//--	end
	genvar i;
	generate
		always @(posedge clock)
		begin
			if(ParcerCycle_SRFF_o == 1'b1) begin
				for (i=0; i <= RxByte_Cnt_Width-2; i=i+1) 
				begin: Tx_RAM_Address_Bus_i
					Tx_RAM_Address_Bus[i] <= MAC_Decoder_Tx_Addr[i] | ARP_Decoder_Tx_Addr[i] | IPv4_Decoder_Tx_Addr[i] | Raw_Decoder_Tx_Addr[i] | IPv4_CheckSum_Ctrl_Tx_Addr[i] | CCCD_Decoder_Tx_Addr[i];
				end
				Rx_RAM_Address_Bus <= MAC_Decoder_Rx_Addr | ARP_Decoder_Rx_Addr | IPv4_Decoder_Rx_Addr | Raw_Decoder_Rx_Addr | IPv4_CheckSum_Ctrl_Rx_Addr | CCCD_Decoder_Rx_Addr;
				for (i=0; i <= 15; i=i+1) 
				begin: Tx_RAM_Data_Bus_i
					Tx_RAM_Data_Bus[i] <= MAC_Decoder_Tx_Data[i] | ARP_Decoder_Tx_Data[i] | IPv4_Decoder_Tx_Data[i] | Raw_Decoder_Tx_Data[i] | IPv4_CheckSum_Ctrl_Tx_Data[i] | CCCD_Decoder_Tx_Data[i];
				end
				TxRAM_wren <= MAC_Decoder_Tx_Word_Strobe | ARP_Decoder_Tx_Word_Strobe | IPv4_Decoder_Tx_Word_Strobe | Raw_Decoder_Tx_Word_Strobe | IPv4_CheckSum_Ctrl_Tx_Word_Strobe | CCCD_Decoder_Tx_Word_Strobe;
			end else begin 
				Tx_RAM_Address_Bus[RxByte_Cnt_Width-2:0] <= AddrBus_In[RxByte_Cnt_Width-2:0];
				Tx_RAM_Data_Bus[15:0] <= ({DataBus_In[7:0],DataBus_In[15:8]});
				TxRAM_wren <= TxRAM_CS & DataBusStrobe & DirectIn & Select_i;
				Rx_RAM_Address_Bus <= AddrBus_In[RxByte_Cnt_Width-2:0];
			end
		end 
	endgenerate
//	--------------------------------------- BUS Section ---------------------------------------------------   

//	--************************************************************************* 
//	-- ?????? ???????? ???????? ? ???? ???????? ?????? ??? ? ?????? ?????????? ?????? ??????
	generate
		case ({Select_i, DirectOut, ParcerCycle_SRFF_o})
			3'b100: begin: Sel
							assign DataBusOut[15:0] = DataBusOut_tmp;// -- ???? ?????? ?? ????????? ????
					  end
			3'b110: begin: Sel_Dir
							assign DataBusOut[15:0] = DataBusOut_tmp;// -- ???? ?????? ?? ????????? ????
						end
			3'b101: begin: Sel_Par
							assign DataBusOut[15:0] = DataBusOut_tmp;// -- ???? ?????? ?? ????????? ????
						end
			3'b011: begin: Dir_Par
							assign DataBusOut[15:0] = ({Buffer_RAM_Data[7:0],Buffer_RAM_Data[15:8]});// -- ???? ?????? ???????????? ??????? ?? Ethernet
						end
			3'b111: begin: Sel_Dir_Par
							assign DataBusOut[15:0] = ({Buffer_RAM_Data[7:0],Buffer_RAM_Data[15:8]});// -- ???? ?????? ???????????? ??????? ?? Ethernet
						end
			default: begin: Def
							assign DataBusOut[15:0] = {16{1'b0}};
						end
		endcase
	endgenerate

	assign TxRAM_CS = 1'b0;

	if ((AddrBus_In >= 0) & (AddrBus_In < 2048)) begin 
		assign DataBusOut_tmp[15:0] = ({RxRAM_q_b[7:0],RxRAM_q_b[15:8]}); // --DataBusOut() = RxRAM.q_b(); 
		assign RxRAM_CS = 1'b1;
	end
		else begin assign RxRAM_CS = 1'b0; 
	end

	if (AddrBus_In == 2048) begin 
		assign DataBusOut_tmp = Status_REG_o;
		assign Status_REG_CS = 1'b1;
	end
		else begin assign Status_REG_CS = 1'b0; 
	end

	if (AddrBus_In == 2049) begin 
		assign SetRxReadyToRecive_Sc_Bus = DataBusStrobe; 
	end
		else begin assign SetRxReadyToRecive_Sc_Bus = 1'b0; 
	end

	if (AddrBus_In == 2050) begin 
		assign InternalTxStart = DataBusStrobe;
	end
		else begin assign InternalTxStart = 1'b0;
	end

	if (AddrBus_In == 2051) begin 
		assign DataBusOut_tmp = RxByte_Cnt_Reg_o;//--PacketLenghts_to_be_transmitted_Reg.q();
		assign PacketLenghts_to_be_transmitted_Reg_CS = 1'b1;
	end
		else begin assign PacketLenghts_to_be_transmitted_Reg_CS = 1'b0;
	end

	if (AddrBus_In == 2052) begin 
		assign DataBusOut_tmp = RxLostPacket_Cnt_REG_o; //-- ????? ?? ???????? ??????? ?? ????? ????????? ????????
		assign RxLostPacket_Cnt_REG_CS = 1'b1;
	end
		else begin assign RxLostPacket_Cnt_REG_CS = 1'b0;
	end
	
//	-- MAC-????? ? IP-????? ????????
	genvar i;
	generate
		for (i=0; i <= 2; i=i+1) //-- MAC-?????
		begin: Module_MAC_Reg_i
			if (AddrBus_In == 2053+i) begin 
				assign DataBusOut_tmp = Module_MAC_Reg_o[i];
				assign Module_MAC_Reg_CS[i] = 1'b1;
			end
			else begin 
				assign Module_MAC_Reg_CS[i] = 1'b0;
			end
		end
	endgenerate;
	
	genvar i;
	generate
		for (i=0; i <= 1; i=i+1) // --IP-????? ????????
		begin: Module_IP_Reg_i
			if (AddrBus_In == 2056+i) begin 
				assign DataBusOut_tmp = Module_IP_Reg_o[i];
				assign Module_IP_Reg_CS[i] = 1'b1;
			end
			else begin 
				assign Module_IP_Reg_CS[i] = 1'b0;
			end
		end
	endgenerate;

// -- MAC-????? ?????????? ???????????? ??????? ??????????
	if (AddrBus_In == 2058) begin assign DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[0][7:0],Source_MAC_Reg_o[0][15:8]}); end
	if (AddrBus_In == 2059) begin assign DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[1][7:0],Source_MAC_Reg_o[1][15:8]}); end
	if (AddrBus_In == 2060) begin assign DataBusOut_tmp[15:0] = ({Source_MAC_Reg_o[2][7:0],Source_MAC_Reg_o[2][15:8]}); end

//		-- Port ????????
	if (AddrBus_In == 2061) begin 
		assign DataBusOut_tmp = Port_Reg_o;//--DataBusOut(7:0) = Port_Reg.q(15:8); DataBusOut(15:8) = Port_Reg.q(7:0);
		assign Port_Reg_CS = 1'b1;
	end
		else begin assign Port_Reg_CS = 1'b0;
	end

	if (AddrBus_In == 2062) begin 
		assign DataBusOut_tmp = Test_Reg_o;
		assign Test_Reg_CS = 1'b1;
	end
		else begin assign Test_Reg_CS = 1'b0;
	end

	if (AddrBus_In == 2063) begin assign DataBusOut_tmp = IPv4_Decoder_Identification; end
	if (AddrBus_In == 2064) begin assign DataBusOut_tmp = Raw_Decoder_Identification;  end

	if (AddrBus_In == 2065) begin assign DataBusOut_tmp = Rx_Packet_Cnt_o; end
	if (AddrBus_In == 2066) begin assign DataBusOut_tmp = Tx_Packet_Cnt_o; end
	if (AddrBus_In == 2067) begin assign DataBusOut_tmp = {16{1'b0}}; 
											assign Rx_Packet_Cnt_Reset = DataBusStrobe & DirectIn & Select_i; 
	end
							 else begin assign Rx_Packet_Cnt_Reset = 1'b0;
	end
	if (AddrBus_In == 2068) begin 
		assign DataBusOut_tmp = {16{1'b0}}; 
		assign Tx_Packet_Cnt_Reset = DataBusStrobe & DirectIn & Select_i;
	end
	else begin 
		assign Tx_Packet_Cnt_Reset = 1'b0;
	end
//		-- ????? ???? ???????????? ?? ???????? ??????
	if (AddrBus_In == 2069) begin assign DataBusOut_tmp = MASS_RAM_BYTE_Tx_Num; end

	if ( (AddrBus_In >= 4096) & (AddrBus_In <= 6143) ) begin 
		assign DataBusOut_tmp[15:0] = ({RxIntRAM_q_b[7:0],RxIntRAM_q_b[15:8]});
		assign RxIntRAM_CS = 1'b1;
	end
	else begin 
		assign RxIntRAM_CS = 1'b0;     
	end

//	--***************************************************************************

//	-- MAC-????? ? IP-????? ????????
//		FOR i IN 0 TO 2 GENERATE  
//--	Module_MAC_Reg(i).(data[],clock, load, enable) = (DataBus_In,Clock,1'b1, Module_MAC_Reg_CS(i) & DataBusStrobe & DirectIn & Select_i);
	genvar i;
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
	genvar i;
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
//--	Status_REG.data(12:0)				= RxByte_Cnt.q();
//--	Status_REG.data(13)							= RxReadyToRecive.q;
//--	Status_REG.data(14)							= 1'b0;--IPv4_Decoder.Rx_NOT_RQ;--RxPacket_in_progress;
//--	Status_REG.data(15)							= VCC;--Raw_Decoder.Tx_Start;--ARP_Decoder.Test;--ARP_Decoder.Rx_Error;%
	ShiftReg
	#(
		.WIDTH	(16)
	) Status_REG
	(
		.clock	(BUS_Clock),
		.data		({1'b1, 1'b0, RxReadyToRecive_o, RxByte_Cnt_o}),
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
		if(Select_i == 1'b1) begin: PacketLenghts_1
			assign PacketLenghts_DataBus = DataBus_In;
			assign PacketLenghts_to_be_transmitted_Reg_EN = (PacketLenghts_to_be_transmitted_Reg_CS & DataBusStrobe & DirectIn & Select_i);
		end
		else begin: PacketLenghts_2
			if (RxTxCycle_IntStart_o == 1'b1) begin: PacketLenghts_3
				assign PacketLenghts_DataBus[RxByte_Cnt_Width:0] = (RxByte_Cnt_o - 1);// -- ???????? 1 ???? ????? ?????? Packet_Good_End ??? Packet_bad_End
			end
			else begin: PacketLenghts_4
				assign PacketLenghts_DataBus[RxByte_Cnt_Width:0] = (RxByte_Cnt_o - 7);// -- ???????? 7 ???? 
			end
			assign PacketLenghts_DataBus[15:RxByte_Cnt_Width+1] = ({(16 - RxByte_Cnt_Width+1){1'b0}});
			assign PacketLenghts_to_be_transmitted_Reg_EN = AnswerTxStart;
		end 
	endgenerate

//--	RxByte_Cnt_Reg.data(RxByte_Cnt_Width:0)  = (RxByte_Cnt.q[]-1);
//--	RxByte_Cnt_Reg.data(15:RxByte_Cnt_Width+1) = GND;
//--	RxByte_Cnt_Reg.(clock, load, enable) = (Clock,1'b1,Edge_Sensing_Sync(.d=FIFO_to_RxRAM_Copy,.clk=Clock));
	ShiftReg 
	#(
		.WIDTH	(16)
	) RxByte_Cnt_Reg
	(
		.clock		(BUS_Clock),
		.data			({({(16 - RxByte_Cnt_Width+1){1'b0}}), (RxByte_Cnt_o - 1)}),
		.enable		(FIFO_to_RxRAM_Copy_ES_o),
		.q				(RxByte_Cnt_Reg_o)
		);
	Edge_Sensing FIFO_to_RxRAM_Copy_ES
	(
		.clk	(Clock),
		.d		(FIFO_to_RxRAM_Copy),
		.q		(FIFO_to_RxRAM_Copy_ES_o)
		);

//	------------ ???????? ?????????(????????? MAC-???????) ? ???????????? ???????
//--	Rx_Packet_Cnt.(clock,cnt_en,sclr) = (BUS_Clock,Edge_Sensing_Sync(.d=MAC_Decoder.Next_Parcer,.clk=BUS_Clock),Rx_Packet_Cnt_Reset);  
	V_Counter 
	#(
		.width	(16)
	) Rx_Packet_Cnt
	(
		.clock	(BUS_Clock),//--Quarts,--
		.clk_en	(1'b1),
		.cnt_en	(MAC_Decoder_Next_Parcer_ES_o),
		.sclr		(Rx_Packet_Cnt_Reset),
		.q			(Rx_Packet_Cnt_o)
		);
	Edge_Sensing MAC_Decoder_Next_Parcer_ES
	(
		.clk	(Clock),
		.d		(MAC_Decoder_Next_Parcer),
		.q		(MAC_Decoder_Next_Parcer_ES_o)
		);

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
