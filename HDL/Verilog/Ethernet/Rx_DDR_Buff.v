//-----------------------------------------------------------------------------
// Title       : DDR input data from Ethernet
//-----------------------------------------------------------------------------
// File        : Rx_DDR_Buff.v
// Company     : INP SB RAS
// Created     : 19/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Rx_DDR_Buff
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Rx_DDR_Buff (
//-----------------------------------------------------------------------------
// Libraries
//-----------------------------------------------------------------------------
//`include "Eth_parameters.v"
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Input Parameters
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire			inclock, 

	input		wire 			aclr,
	input		wire [6:0]	datain,
	output	reg [6:0]	dataout_h,
	output	reg [6:0]	dataout_l

);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------

	reg [6:0]	data_h;
	reg [6:0]	data_l;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_RxD_0 (
		.Q1		(data_h(0)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(0)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(0)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_RxD_1 (
		.Q1		(data_h(1)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(1)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(1)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_RxD_2 (
		.Q1		(data_h(2)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(2)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(2)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_RxD_3 (
		.Q1		(data_h(3)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(3)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(3)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_RxDv (
		.Q1		(data_h(4)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(4)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(4)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_Crs (
		.Q1		(data_h(5)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(5)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(5)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);
	IDDR #(
		.DDR_CLK_EDGE("OPPOSITE_EDGE"), // "OPPOSITE_EDGE", "SAME_EDGE" 
												  //    or "SAME_EDGE_PIPELINED" 
		.INIT_Q1(1'b0), // Initial value of Q1: 1'b0 or 1'b1
		.INIT_Q2(1'b0), // Initial value of Q2: 1'b0 or 1'b1
		.SRTYPE("SYNC") // Set/Reset type: "SYNC" or "ASYNC" 
	) Eth0_Phy_RxClk (
		.Q1		(data_h(6)), // 1-bit output for positive edge of clock 
		.Q2		(data_l(6)), // 1-bit output for negative edge of clock
		.C			(inclock),   // 1-bit clock input
		.CE		(1'b1), // 1-bit clock enable input
		.D			(datain(6)),   // 1-bit DDR data input
		.R			(aclr),   // 1-bit reset
		.S			(1'b1)    // 1-bit set
	);

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------

	always @(inclock)
	begin
		dataout_h <= {data_h[6],data_h[5],data_h[4],data_h[3],data_h[2],data_h[1],data_h[0]};
		dataout_l <= {data_l[6],data_l[5],data_l[4],data_l[3],data_l[2],data_l[1],data_l[0]};
	end

endmodule
