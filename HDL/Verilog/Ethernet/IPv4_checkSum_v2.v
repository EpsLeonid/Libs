//-----------------------------------------------------------------------------
// Title       : Ethernet IPv4 checksum
//-----------------------------------------------------------------------------
// File        : IPv4_checkSum_v2.v
// Company     : INP SB RAS
// Created     : 15/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : IPv4_checkSum
//-----------------------------------------------------------------------------
// Revision    : 2.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module IPv4_checkSum_v2 #(
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

	output	wire								IPv4_CheckSum_Complete,

	input		wire [Eth_WORD_WIDTH-1:0]	IP_ID,

	//-----------------
	output	wire								Sample_Enable,
	output	wire [19:0]						Sum20_Reg_out,
	output	wire [15:0]						Sum16_Reg_out,
	output	wire								RxParcerActive_out
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

	parameter PARCER_CYLCLE_WIDTH = 2; 
	parameter PARCER_CYLCLE_CNT_WIDTH = 1+1; //Ceil(log2(PARCER_CYLCLE_WIDTH))

	parameter PacketLenghts_at_signaling_layer = 4096;//--2048;-- maximum length value in bytes
	parameter RxByte_Cnt_Width = 12;//clog2(PacketLenghts_at_signaling_layer_ext);

	parameter IP_HEADER_OFFSET	= 7;
	parameter IP_HEADER_SIZE	= 10;
	parameter IP_CHECKSUM_ADDR	= 12;

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------

	wire 											 Rx_Parcer_RQ_ES_o;
	wire 											 RxParcerActive_o;
	reg 											 ParcerEndCycle;

	wire [RxByte_Cnt_Width-1:0]			 ParcerCnt_o;

	wire [PARCER_CYLCLE_CNT_WIDTH-1:0]	 Prescaler_o;
	wire 											 Pascer_Sample_Enable;

	wire [19:0]									 Sum20_Reg_o;
	wire [15:0]									 Sum16_Reg_o;
	
	reg [19:0]									 Adder2x20_data;
	reg 											 Adder2x16_data_cout;
	reg [15:0]									 Adder2x16_data;
	reg [15:0]									 FinAdder2x16_data;

	reg [Eth_WORD_WIDTH-1:0]				 Rx_Data_tmp;

	reg 											 HeaderSum_Complete; 
	reg 											 HeaderSum_with_carry_count;
	reg 											 IP_CheckSum_Complete;
	reg 											 Tx_Word_Strobe_tmp;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------
	V_Counter
	#(
		.width (PARCER_CYLCLE_CNT_WIDTH)
	) Prescaler
	(
		clock		(Clock),
		clk_en	(1'b1),
		cnt_en	(1'b1),
		sclr		(Pascer_Sample_Enable | Rx_Parcer_RQ_ES_o),
		q			(Prescaler_o)
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
		.width (RxByte_Cnt_Width)
	) ParcerCnt
	(
		.data		(IP_HEADER_OFFSET),
		.clock	(Clock),
		.clk_en	(1'b1),
		.cnt_en	(RxParcerActive_o & Pascer_Sample_Enable),
		.sload	(Rx_Parcer_RQ_ES_o),
		.q			(ParcerCnt_o)
		);

	ShiftReg 
		#(
			.WIDTH	(20) 
		) Sum20_Reg
		(
			.clock	(Clock),
			.data		(Adder2x20_data),
			.enable	(Sum20_Reg_en | Rx_Parcer_RQ_ES_o),
			.sclr		(Rx_Parcer_RQ_ES_o),
			.q			(Sum20_Reg_o)
		);
	ShiftReg 
		#(
			.WIDTH	(16) 
		) Sum16_Reg
		(
			.clock	(Clock),
			.data		(FinAdder2x16_data),
			.enable	(Sum16_Reg_en | Rx_Parcer_RQ_ES_o),
			.q			(Sum16_Reg_o)
		);
//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
	assign Pascer_Sample_Enable = (Prescaler_o ==  PARCER_CYLCLE_WIDTH-1) ? 1'b1 : 1'b0; // For what???

//	-------------------------------------------- Rx section --------------------------------------------
//	Rx_Data_[] = Rx_Data[]; // For what???

	always @*
	begin
		if (RxParcerActive_o == 1'b1) begin
			Rx_Addr = ParcerCnt_o;
			if (ParcerCnt_o == IP_HEADER_OFFSET+2) begin Rx_Data_tmp = IP_ID;
				end else if (ParcerCnt_o == IP_HEADER_OFFSET+5) begin Rx_Data_tmp = {Eth_WORD_WIDTH{1'b0}}; 
				end else begin Rx_Data_tmp = Rx_Data;
			end

			if (ParcerCnt_o == IP_HEADER_SIZE+IP_HEADER_OFFSET+0) begin HeaderSum_Complete = 1'b1; end
			if (ParcerCnt_o == IP_HEADER_SIZE+IP_HEADER_OFFSET+1) begin HeaderSum_with_carry_count = 1'b1; end
			if (ParcerCnt_o == IP_HEADER_SIZE+IP_HEADER_OFFSET+2) begin IP_CheckSum_Complete = 1'b1; end
			if (ParcerCnt_o == IP_HEADER_SIZE+IP_HEADER_OFFSET+3) begin Tx_Data = !Sum16_Reg_o; Tx_Addr = IP_CHECKSUM_ADDR; Tx_Word_Strobe_tmp = 1'b1; end
			if (ParcerCnt_o == IP_HEADER_SIZE+IP_HEADER_OFFSET+4) begin ParcerEndCycle = Pascer_Sample_Enable; end
			 
		end else begin 
			Tx_Addr = {11{1'b0}}; 
			Tx_Data = 1'b0; 
			Rx_Addr = 1'b0;
			HeaderSum_Complete = 1'b0; 
			HeaderSum_with_carry_count = 1'b0; 
			IP_CheckSum_Complete = 1'b0;
			ParcerEndCycle = 1'b0;
		end
	end

	always @(Clock)
	begin
		if (RxParcerActive_o & Pascer_Sample_Enable & (ParcerCnt_o>=IP_HEADER_OFFSET) & (ParcerCnt_o < (IP_HEADER_SIZE+IP_HEADER_OFFSET))) begin
			Adder2x20_data <= ({{4{1'b0}},Rx_Data_tmp} + Sum20_Reg_o);
		end
		
		if (HeaderSum_Complete & Pascer_Sample_Enable) begin
			{Adder2x16_data_cout, Adder2x16_data} <= ({{12{1'b0}},Sum20_Reg_o[19:16]} + Sum20_Reg_o[15:0]);
		end
		
		if (HeaderSum_with_carry_count & Pascer_Sample_Enable) begin
			FinAdder2x16_data <= ({{15{1'b0}},Adder2x16_data_cout} + Adder2x16_data);
		end
	end

	assign Tx_Word_Strobe = Tx_Word_Strobe_tmp & Pascer_Sample_Enable; // --Edge_Sensing_Sync(.d=Tx_Word_Strobe_,.clk=Clock);
	assign IPv4_CheckSum_Complete = ParcerEndCycle; // --DFF(.d=ParcerEndCycle,.clk=Clock);

//	----------------------
	assign Sample_Enable = RxParcerActive_o & Pascer_Sample_Enable & ((ParcerCnt_o >= IP_HEADER_OFFSET) & (ParcerCnt_o < IP_HEADER_SIZE+IP_HEADER_OFFSET));// --Pascer_Sample_Enable;
	assign Sum20_Reg_out = Sum20_Reg_o;
	assign Sum16_Reg_out = Sum16_Reg_o;
	assign RxParcerActive_out = RxParcerActive_o;

endmodule
