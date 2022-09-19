//-----------------------------------------------------------------------------
// Title       : Ethernet ARP decoder
//-----------------------------------------------------------------------------
// File        : Eth_ARP_Decoder_v2.v
// Company     : INP SB RAS
// Created     : 15/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_ARP_Decoder
//-----------------------------------------------------------------------------
// Revision    : 2.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_ARP_Decoder_v2 #(
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

	input		wire								Reset,

	output	wire								Rx_IP_Error,
	output	wire								Rx_NOT_RQ,
	output	wire								Tx_Start,
	output	wire								Rx_Parcer_in_progress,
	output	wire								Rx_TRUE_RQ,

	input		wire [Eth_WORD_WIDTH-1:0]	MAC_Addr0_i,
	input		wire [Eth_WORD_WIDTH-1:0]	MAC_Addr1_i,
	input		wire [Eth_WORD_WIDTH-1:0]	MAC_Addr2_i,
	input		wire [Eth_WORD_WIDTH-1:0]	IP_Addr0_i,
	input		wire [Eth_WORD_WIDTH-1:0]	IP_Addr1_i,

	output	wire								Test
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

	parameter PARCER_CYLCLE_WIDTH			= 2;// --4
	parameter PARCER_CYLCLE_CNT_WIDTH	= 1+1; //Ceil(log2(PARCER_CYLCLE_WIDTH))

	parameter Eth_type_Pos					= 6;

	parameter ARP_type						= 16'h0608;

	parameter ARP_Hardware_type			= 16'h0100;
	parameter ARP_Protocol_type			= 16'h0008;
	parameter ARP_HP_size					= 16'h0406; //-- ?????? MAC(6) ? IP(4) ??????? ? ??????
	parameter ARP_Opcode_R_ouest			= 16'h0100; //-- ?????? ARP ??????
	parameter ARP_Opcode_Answer			= 16'h0200; //-- ?????? ARP ?????

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------

	wire 											 Rx_Parcer_RQ_ES_o;
	wire 											 RxParcerActive_o;
	reg 											 ARP_R_ouest; // -- ???? ??????? ARP ?????? ? Op?ode==ARP_Opcode_R_ouest ?? ????????? ???????? ?????? ?? Tx ???????
	wire 											 Tx_Start_Pulse_Flag_o;
	reg 											 Tx_Strobe;

	wire [4:0]									 ParcerCnt_o;

//	wire [PARCER_CYLCLE_CNT_WIDTH-1:0]	 Prescaler_o;
	wire 											 Pascer_Sample_Enable;

	reg 											 ARP_type_True;
	wire 											 ARP_type_Flag_o;

	reg 											 ParcerEndCyle;

	reg 											 IP0_True, IP1_True; // -- ???????? ? ARP-??????? IP-?????? ?????.
	wire 											 IP0_Addr_Flag_o, IP1_Addr_Flag_o; // -- ???????? ?????? ???? ?????? IP-????? ?????

	wire [Eth_WORD_WIDTH-1:0]				 Rx_Data_tmp;

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
		.R		(ParcerEndCyle | Reset),
		.q		(RxParcerActive_o)
		);

	V_Counter
	#(
		.width (3)
	) ParcerCnt
	(
		.data		(Eth_type_Pos),
		.clock	(Clock),
		.clk_en	(1'b1),
		.cnt_en	(RxParcerActive_o & Pascer_Sample_Enable),
		.sload	(Rx_Parcer_RQ_ES_o),
		.q			(ParcerCnt_o)
		);
//	-- ?????
	SRFF ARP_type_Flag
	(
		.S		(ARP_type_True),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(ARP_type_Flag_o)
		);

	SRFF IP0_Addr_Flag
	(
		.S		(IP0_True),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(IP0_Addr_Flag_o)
		);

	SRFF IP1_Addr_Flag
	(
		.S		(IP1_True),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(IP1_Addr_Flag_o)
		);

	SRFF Tx_Start_Pulse_Flag
	(
		.S		(ARP_R_ouest),
		.CLK	(Clock),
		.R		(Rx_Parcer_RQ_ES_o | Reset),
		.q		(Tx_Start_Pulse_Flag_o)
		);

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
	assign Pascer_Sample_Enable = 1'b1; // For what???

//	-------------------------------------------- Rx section --------------------------------------------
//	Rx_Data_tmp[] = Rx_Data[];  // For what???

	always @*
	begin
		if (RxParcerActive_o == 1'b1) begin
			assign Rx_Addr = ({{6{1'b0}}, ParcerCnt_o});
			if (ParcerCnt_o == Eth_type_Pos+0) begin Rx_Addr = 6 ; end // -- check packet type (arp type)
			if (ParcerCnt_o == Eth_type_Pos+1) begin Rx_Addr = 19; end // -- check LSB IP-address
			if (ParcerCnt_o == Eth_type_Pos+2) begin Rx_Addr = 20; end // -- check MSB IP-address
			if (ParcerCnt_o == Eth_type_Pos+3) begin Rx_Addr = 10; end // -- check arp r_ouest

			if ((ParcerCnt_o == Eth_type_Pos+1) & (Rx_Data == ARP_type)) begin ARP_type_True = 1'b1; end 
																						else begin ARP_type_True = 1'b0; end
			if ((ParcerCnt_o == Eth_type_Pos+3) & (ARP_type_Flag_o == 1'b0)) begin ParcerEndCyle = 1'b1; end
			  
			if (ParcerCnt_o == Eth_type_Pos+2) begin 
				if (Rx_Data == IP_Addr0_i) begin assign IP0_True = 1'b1; end // -- check LSB IP-address
			end
			if (ParcerCnt_o == Eth_type_Pos+3) begin 
				if (Rx_Data == IP_Addr1_i) begin assign IP1_True = 1'b1; end // -- check MSB IP-address
			end

			if (( ParcerCnt_o == Eth_type_Pos+4) & (Rx_Data == ARP_Opcode_R_ouest)) begin assign ARP_R_ouest = 1'b1; end // -- ?????? ARP ??????, ????????? ?????? ???????? ?????? ?? Tx ???????

			if (ParcerCnt_o == Eth_type_Pos+0)  begin Tx_Data = ARP_type;				Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+1)  begin Tx_Data = ARP_Hardware_type;	Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+2)  begin Tx_Data = ARP_Protocol_type;	Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+3)  begin Tx_Data = ARP_HP_size;			Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+4)  begin Tx_Data = ARP_Opcode_Answer;	Tx_Strobe = 1'b1; end

			if (ParcerCnt_o == Eth_type_Pos+5)  begin Tx_Data = MAC_Addr0_i;	Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+6)  begin Tx_Data = MAC_Addr1_i;	Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+7)  begin Tx_Data = MAC_Addr2_i;	Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+8)  begin Tx_Data = IP_Addr0_i;	Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+9)  begin Rx_Addr[10:0] = 11; 
																	Tx_Data = IP_Addr1_i;	Tx_Strobe = 1'b1; end
			
			if (ParcerCnt_o == Eth_type_Pos+10) begin Rx_Addr[10:0] = 12; Tx_Data = Rx_Data; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+11) begin Rx_Addr[10:0] = 13; Tx_Data = Rx_Data; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+12) begin Rx_Addr[10:0] = 14; Tx_Data = Rx_Data; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+13) begin Rx_Addr[10:0] = 15; Tx_Data = Rx_Data; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+14) begin Tx_Data = Rx_Data; Tx_Strobe = 1'b1; end
			if (ParcerCnt_o == Eth_type_Pos+15) begin ParcerEndCyle = 1'b1; end
			 
		end else begin 
			Tx_Addr = {11{1'b0}}; 
			Tx_Data = 1'b0; 
			Tx_Strobe = 1'b0; 
			Rx_Addr = 1'b0;
			ARP_type_True = 1'b0; 
			ARP_R_ouest = 1'b0; 
			ParcerEndCyle = 1'b0;
			IP0_True = 1'b0;
			IP1_True = 1'b0;
		end
	end

//	-- outputs
	assign Rx_IP_Error				= ARP_type_Flag_o & !(IP0_Addr_Flag_o & IP1_Addr_Flag_o) & ParcerEndCyle;
	assign Rx_NOT_RQ					= (!ARP_type_Flag_o) & ParcerEndCyle;
	assign Tx_Start					= Tx_Start_Pulse_Flag_o & IP0_Addr_Flag_o & IP1_Addr_Flag_o & ParcerEndCyle;
	assign Rx_Parcer_in_progress	= RxParcerActive_o;
	assign Tx_Word_Strobe			= (Tx_Strobe & Pascer_Sample_Enable); // -- ????? ???????????? ? Tx ??????
	assign Rx_TRUE_RQ					= ARP_type_Flag_o & ParcerEndCyle;

	assign Test							= ARP_type_Flag_o;

endmodule
