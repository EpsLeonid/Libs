//-----------------------------------------------------------------------------
// Title       : Ethernet MAC decoder
//-----------------------------------------------------------------------------
// File        : Eth_MAC_Decoder_v2.v
// Company     : INP SB RAS
// Created     : 15/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_Pkt_Parcer
//-----------------------------------------------------------------------------
// Revision    : 2.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_MAC_Decoder_v2 #(
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

	output	reg [10:0]						Tx_Addr,
	output	reg [Eth_WORD_WIDTH-1:0]	Tx_Data,
	output	wire								Tx_Word_Strobe,

		
	output	wire								Progress_Flag,
	output	wire								Tx_Start,
	output	wire								CycleEndErr,

	input		wire [Eth_WORD_WIDTH-1:0]	MAC_Addr0_i,
	input		wire [Eth_WORD_WIDTH-1:0]	MAC_Addr1_i,
	input		wire [Eth_WORD_WIDTH-1:0]	MAC_Addr2_i,

	input		wire								Reset,

	output	wire								Rx_Error_MAC,
	output	wire								Next_Parcer,
	output	wire								Rx_Parcer_in_progress
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

	parameter Eth_MAC_Pos        = 0;//--0; --6

	parameter PARCER_CYLCLE_WIDTH = 2; 
	parameter PARCER_CYLCLE_CNT_WIDTH = 1+1; //Ceil(log2(PARCER_CYLCLE_WIDTH))

//	-- project version
	parameter VERSION = 16'h0003;
//	-- unit type code
	parameter UNIT_TYPE = 16'hAAAA;
//	-- MAC address multicast
	parameter MMAC_ADDR_0 = 16'h0355;
	parameter MMAC_ADDR_1 = 16'h5555;
	parameter MMAC_ADDR_2 = 16'h5700;//-- MCHS --H"5600"; -- PVV

//	-- MASTER device number
	parameter NUM_ETH_TX = 2;
	parameter NUM_POSS 	= 1;
	parameter NUM_USB 	= 1;
	parameter NUM_CLINK 	= 1;
	parameter NUM_DEV = NUM_POSS+NUM_USB+NUM_CLINK+NUM_ETH_TX;

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------

	wire 											 Rx_Parcer_RQ_ES_o;
	wire 											 RxParcerActive_o;

	wire [2:0]									 ParcerCnt_o;

	wire [PARCER_CYLCLE_CNT_WIDTH-1:0]	 Prescaler;
	wire 											 Pascer_Sample_Enable;

	wire 											 Rx_MAC_Addr_Err_Flag_o;
	reg [3:0]									 Rx_MAC_Addr_True;
	wire [3:0]									 Rx_MAC_Addr_True_Flag_o;

	reg 											 Tx_Strobe;

	reg 											 ParcerEndCyle;

	wire [Eth_WORD_WIDTH-1:0]				 Rx_Data_tmp;

	wire [15:0]				 incoming_MAC0_Reg_o;
	reg 						 incoming_MAC0_Reg_En;
	wire [15:0]				 incoming_MAC1_Reg_o;
	reg 						 incoming_MAC1_Reg_En;
	wire [15:0]				 incoming_MAC2_Reg_o;
	reg 						 incoming_MAC2_Reg_En;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) incoming_MAC0_Reg
		(
			.clock	(Clock),
			.data		(Rx_Data),
			.enable	(incoming_MAC0_Reg_En),
			.q			(incoming_MAC0_Reg_o)
		);
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) incoming_MAC1_Reg
		(
			.clock	(Clock),
			.data		(Rx_Data),
			.enable	(incoming_MAC1_Reg_En),
			.q			(incoming_MAC1_Reg_o)
		);
	ShiftReg 
		#(
			.WIDTH	(Eth_WORD_WIDTH) 
		) incoming_MAC2_Reg
		(
			.clock	(Clock),
			.data		(Rx_Data),
			.enable	(incoming_MAC2_Reg_En),
			.q			(incoming_MAC2_Reg_o)
		);

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
		.R		(ParcerEndCyle | Reset),
		.q		(RxParcerActive_o)
		);

	V_Counter
	#(
		.width (3)
	) ParcerCnt
	(
		clock		(Clock),
		clk_en	(1'b1),
		cnt_en	(RxParcerActive_o & Pascer_Sample_Enable),
		sclr		(!RxParcerActive_o),
		q			(ParcerCnt_o)
		);

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
	assign Pascer_Sample_Enable = 1'b1; // For what???

//	-------------------------------------------- Rx section --------------------------------------------
//	Rx_Data_[] = Rx_Data[]; // For what???

	always @*
	begin
		if (RxParcerActive_o == 1'b1) begin
			Rx_Addr = ({{8{1'b0}}, ParcerCnt_o});
			if ((ParcerCnt_o == Eth_MAC_Pos+1) & ((Rx_Data == MAC_Addr0_i) /*| (Rx_Data == 4'hFFFF) | (Rx_Data == MMAC_ADDR_0)*/) ) begin Rx_MAC_Addr_True[0] = 1'b1; end else begin Rx_MAC_Addr_True[0] = 1'b0; end
			if ((ParcerCnt_o == Eth_MAC_Pos+2) & ((Rx_Data == MAC_Addr1_i) /*| (Rx_Data == 4'hFFFF) | (Rx_Data == MMAC_ADDR_1)*/) ) begin Rx_MAC_Addr_True[1] = 1'b1; end else begin Rx_MAC_Addr_True[1] = 1'b0; end 
			if ((ParcerCnt_o == Eth_MAC_Pos+3) & ((Rx_Data == MAC_Addr2_i) /*| (Rx_Data == 4'hFFFF) | (Rx_Data == MMAC_ADDR_2)*/) ) begin Rx_MAC_Addr_True[2] = 1'b1; end else begin Rx_MAC_Addr_True[2] = 1'b0; end

//		 --  ????????????? ? ????????? ????? MAC-????? ??????????? ???????
			if (ParcerCnt_o == Eth_MAC_Pos+1) begin Tx_Addr = 3; Tx_Data = MAC_Addr0_i; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_MAC_Pos+2) begin Tx_Addr = 4; Tx_Data = MAC_Addr1_i; Tx_Strobe = 1'b1; end 
			if (ParcerCnt_o == Eth_MAC_Pos+3) begin Tx_Addr[10:0] = 5; Tx_Data = MAC_Addr2_i; Tx_Strobe = 1'b1; end
//		 
//		 --latch incoming MAC address
			if (ParcerCnt_o == Eth_MAC_Pos+4) begin incoming_MAC0_Reg_En = 1'b1; end else begin incoming_MAC0_Reg_En = 1'b0; end
			if (ParcerCnt_o == Eth_MAC_Pos+5) begin incoming_MAC1_Reg_En = 1'b1; end else begin incoming_MAC1_Reg_En = 1'b0; end
			if (ParcerCnt_o == Eth_MAC_Pos+6) begin incoming_MAC2_Reg_En = 1'b1; end else begin incoming_MAC2_Reg_En = 1'b0; end
//		 --  ????????????? ? ????????? ????? MAC-????? ?????????? ??????
			if (ParcerCnt_o == Eth_MAC_Pos+5) begin Tx_Addr = 0; Tx_Data = incoming_MAC0_Reg_o; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_MAC_Pos+6) begin Tx_Addr = 1; Tx_Data = incoming_MAC1_Reg_o; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_MAC_Pos+7) begin Tx_Addr = 2; Tx_Data = incoming_MAC2_Reg_o; Tx_Strobe = 1'b1; end
		
			if (ParcerCnt_o == Eth_MAC_Pos+7) begin ParcerEndCyle = 1'b1; end else begin ParcerEndCyle = 1'b0; end
			 
		end else begin 
			Tx_Addr[10:0] = {11{1'b0}}; 
			Tx_Data = 1'b0; 
			Tx_Strobe = 1'b0; 
			Rx_Addr = 1'b0;
			Rx_MAC_Addr_True[0] = 1'b0; 
			Rx_MAC_Addr_True[1] = 1'b0; 
			Rx_MAC_Addr_True[2] = 1'b0;
			incoming_MAC0_Reg_En = 1'b0;
			incoming_MAC1_Reg_En = 1'b0;
			incoming_MAC2_Reg_En = 1'b0;
		end
	end

	always @(Clock)
	begin
		Rx_MAC_Addr_True[3] <= Rx_MAC_Addr_True_Flag_o[0] & Rx_MAC_Addr_True_Flag_o[1] & Rx_MAC_Addr_True_Flag_o[2];
	end
	
	genvar i;
	generate
		for (i=0; i <= 3; i=i+1) 
		begin: Rx_MAC_Addr_True_Flag_i
			SRFF Rx_MAC_Addr_True_Flag
			(
				.S		(Rx_MAC_Addr_True[i]),
				.CLK	(Clock),
				.R		(Rx_Parcer_RQ_ES_o | ParcerEndCyle | Reset),
				.q		(Rx_MAC_Addr_True_Flag_o[i])
				);
		end
	endgenerate

	SRFF Rx_MAC_Addr_Err_Flag
	(
		.S		((!Rx_MAC_Addr_True_Flag_o[3]) & ParcerEndCyle),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(Rx_MAC_Addr_Err_Flag_o)
		);

//	-- outputs
	assign Rx_Error_MAC 				 = Rx_MAC_Addr_Err_Flag_o; //-- ????????? MAC-????? ????????
	assign Next_Parcer 				 = (Rx_MAC_Addr_True_Flag_o[3] & ParcerEndCyle); //-- MAC-????? ???????, ????????? ? ?????????? ???????
	assign Tx_Word_Strobe 			 = (RxParcerActive_o & Tx_Strobe & Pascer_Sample_Enable); //-- ????? ???????????? ? Tx ?????? ???-????? ??????????, ???-????? ????????
	assign Rx_Parcer_in_progress	 = RxParcerActive_o; //-- ???? ??????? ????????????? MAC-???????

endmodule
