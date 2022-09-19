//-----------------------------------------------------------------------------
// Title       : Eth_Pkt_Parcer
//-----------------------------------------------------------------------------
// File        : Eth_Pkt_Parcer.v
// Company     : INP SB RAS
// Created     : 14/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Eth_Pkt_Parcer
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole | in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Eth_Pkt_Parcer (
//-----------------------------------------------------------------------------
// Libraries
//-----------------------------------------------------------------------------
//`include "Eth_parameters.v"
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire			Clock, //-- System Clock, really Bus_Clock

	input		wire [15:0]	Rx_Data,
	output	wire [10:0]	Rx_Addr_o,
	input		wire			Rx_Parcer_RQ,
	input		wire [10:0]	Rx_NUM_Data,

	output	reg [10:0]	Tx_Addr,
	output	reg [15:0]	Tx_Data,
	output	reg			Tx_Word_Strobe,

		
	input		wire			Reset,
	output	wire			Progress_Flag,
	output	wire			Tx_Start,
	output	wire			CycleEndErr,

	input		wire [8*6-1:0]	Module_MAC,		//-- MAC address
	input		wire [8*4-1:0]	Module_IP,		//-- IP address
	input		wire [8*2-1:0]	Module_Port,	//-- Port number

	output	wire [15:0]	Identification,
	output	wire			MAC_recognized,

	//-- Standard bus connections
	output	wire			AccessRequest,
	input		wire			AccessGranted,
	output	wire			DirectOut,
	output	wire [15:0]	AddrBusOut,
	output	reg [15:0]	DataBusOut,
	input		wire [15:0]	DataBus_In,
	input		wire			DataBusStrobe
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

	parameter PacketLenghts_at_signaling_layer = 2048; //-- maximum length value in bytes
	parameter RxByte_Cnt_Width = 11;//clog2(PacketLenghts_at_signaling_layer_ext);
	parameter Eth_WORD_WIDTH = 16;

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------

//--	wire 								 MAC_Decoder; Eth_MAC_Decoder_v1
	wire [10:0]							 MAC_Decoder_Rx_Addr;
	wire [10:0]							 MAC_Decoder_Tx_Addr;
	wire [Eth_WORD_WIDTH-1:0]		 MAC_Decoder_Tx_Data;
	wire 									 MAC_Decoder_Tx_Word_Strobe;
	wire 									 MAC_Decoder_Rx_Parcer_RQ;
	
	wire 									 MAC_Decoder_Rx_Error_MAC;
	wire 									 MAC_Decoder_Next_Parcer;
//	wire 									 MAC_Rx_Parcer_in_progress;

//--	wire 									 ARP_Decoder; Eth_ARP_Decoder_v1
	wire [10:0]							 ARP_Decoder_Rx_Addr;
	wire [10:0]							 ARP_Decoder_Tx_Addr;
	wire [Eth_WORD_WIDTH-1:0]		 ARP_Decoder_Tx_Data;
	wire 									 ARP_Decoder_Tx_Word_Strobe;
	
	wire 									 ARP_Decoder_Rx_IP_Error;
	wire 									 ARP_Decoder_Rx_NOT_RQ;
	wire 									 ARP_Decoder_Tx_Start;
	wire 									 ARP_Decoder_Rx_TRUE_RQ;
//	wire 									 ARP_Decoder_Rx_Parcer_in_progress;
	
	wire 									 ARP_Decoder_Test;

//--	wire 									 IPv4_Decoder; Eth_IPv4_Decoder_v1
	wire [10:0]							 IPv4_Decoder_Rx_Addr;
	wire [10:0]							 IPv4_Decoder_Tx_Addr;
	wire [Eth_WORD_WIDTH-1:0]		 IPv4_Decoder_Tx_Data;
	wire 									 IPv4_Decoder_Tx_Word_Strobe;
	
	wire 									 IPv4_Decoder_Rx_Error_IP;
	wire 									 IPv4_Decoder_Rx_NOT_RQ;
	wire 									 IPv4_Decoder_Tx_Start;
	wire 									 IPv4_Decoder_Rx_TRUE_RQ;
//	wire 									 IPv4_Decoder_Rx_Parcer_in_progress;
	
//	wire 									 IPv4_Decoder_AccessRequest;
//	wire 									 IPv4_Decoder_DirectOut;
//	wire [15:0]							 IPv4_Decoder_AddrBusOut;
	
	wire 									 IPv4_Decoder_test;

//--	wire 									 IPv4_CheckSum_Ctrl : IPv4_checkSum;
	wire [10:0]							 IPv4_CheckSum_Ctrl_Rx_Addr;
	wire [10:0]							 IPv4_CheckSum_Ctrl_Tx_Addr;
	wire [Eth_WORD_WIDTH-1:0]		 IPv4_CheckSum_Ctrl_Tx_Data;
	wire 									 IPv4_CheckSum_Ctrl_Tx_Word_Strobe;
	wire 									 IPv4_CheckSum_Ctrl_Complete;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------
//	-- MAC address decoder
	Eth_MAC_Decoder_v2 MAC_Decoder
	(
		.Clock						(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data						(Rx_Data),
		.Rx_Addr						(MAC_Decoder_Rx_Addr),
		.Rx_Parcer_RQ				(Rx_Parcer_RQ),

		.Tx_Addr						(MAC_Decoder_Tx_Addr),
		.Tx_Data						(MAC_Decoder_Tx_Data),
		.Tx_Word_Strobe			(MAC_Decoder_Tx_Word_Strobe),

		.MAC_Addr0_i				({Module_MAC[7:0], Module_MAC[15:8]}),
		.MAC_Addr1_i				({Module_MAC[23:16], Module_MAC[31:24]}),
		.MAC_Addr2_i				({Module_MAC[39:32], Module_MAC[47:40]}),

		.Reset						(Reset),

		.Rx_Error_MAC				(MAC_Decoder_Rx_Error_MAC),
		.Next_Parcer				(MAC_Decoder_Next_Parcer)
//		.Rx_Parcer_in_progress	(MAC_Rx_Parcer_in_progress)
		);
	
//	-- ARP decoder
	Eth_ARP_Decoder_v2 ARP_Decoder
	(
		.Clock					(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data					(Rx_Data),
		.Rx_Addr					(ARP_Decoder_Rx_Addr),
		.Rx_Parcer_RQ			(MAC_Decoder_Next_Parcer),

		.Tx_Addr					(ARP_Decoder_Tx_Addr),
		.Tx_Data					(ARP_Decoder_Tx_Data),
		.Tx_Word_Strobe		(ARP_Decoder_Tx_Word_Strobe),

		.MAC_Addr0_i			({Module_MAC[7:0], Module_MAC[15:8]}),
		.MAC_Addr1_i			({Module_MAC[23:16], Module_MAC[31:24]}),
		.MAC_Addr2_i			({Module_MAC[39:32], Module_MAC[47:40]}),
		.IP_Addr0_i				({Module_IP[7:0], Module_IP[15:8]}),
		.IP_Addr1_i				({Module_IP[23:16], Module_IP[31:24]}),

		.Reset						(Reset),

		.Rx_IP_Error				(ARP_Decoder_Rx_IP_Error),
		.Rx_NOT_RQ					(ARP_Decoder_Rx_NOT_RQ),
		.Tx_Start					(ARP_Decoder_Tx_Start),
		.Rx_TRUE_RQ					(ARP_Decoder_Rx_TRUE_RQ),
//		.Rx_Parcer_in_progress	(ARP_Decoder_Rx_Parcer_in_progress),
		
		.Test							(ARP_Decoder_Test)
		);
//	-- IPv4 decoder
	Eth_IPv4_Decoder_v2 IPv4_Decoder
	(
		.Clock					(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data				(Rx_Data),
		.Rx_Addr				(IPv4_Decoder_Rx_Addr),
		.Rx_Parcer_RQ		(ARP_Decoder_Rx_NOT_RQ),
		.Rx_NUM_Data		(Rx_NUM_Data),

		.Tx_Addr				(IPv4_Decoder_Tx_Addr),
		.Tx_Data				(IPv4_Decoder_Tx_Data),
		.Tx_Word_Strobe	(IPv4_Decoder_Tx_Word_Strobe),

		.IP_Addr0_i			({Module_IP[7:0], Module_IP[15:8]}),
		.IP_Addr1_i			({Module_IP[23:16], Module_IP[31:24]}),
		.Port_i				({Module_Port[7:0], Module_Port[15:8]}),
		
		.Identification	(Identification),

		.Reset				(Reset),

		.Rx_Error_IP				(IPv4_Decoder_Rx_Error_IP),
		.Rx_NOT_RQ					(IPv4_Decoder_Rx_NOT_RQ),
		.Tx_Start					(IPv4_Decoder_Tx_Start),
		.Rx_TRUE_RQ					(IPv4_Decoder_Rx_TRUE_RQ),
//		.Rx_Parcer_in_progress	(IPv4_Decoder_Rx_Parcer_in_progress),
		
		.AccessRequest				(AccessRequest),
		.AccessGranted				(AccessGranted),
		.DirectOut					(DirectOut),
		.AddrBusOut					(AddrBusOut),
		.DataBus_In					(DataBus_In),// -- ???????????? ? ?????????? ???? ? ?????? ??????
		.DataBusOut					(DataBusOut),// -- ???????????? ? ?????????? ???? ? ?????? ??????
		.DataBusStrobe				(DataBusStrobe),// -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)

		.Test							(IPv4_Decoder_test)
		);
	
	IPv4_checkSum_v2 IPv4_CheckSum_Ctrl
	(
		.Clock				(Clock),// -- System Clock, really Bus_Clock

		.Rx_Data				(Rx_Data),
		.Rx_Addr				(IPv4_CheckSum_Ctrl_Rx_Addr),
		.Rx_Parcer_RQ		(IPv4_Decoder_Tx_Start),

		.Tx_Addr				(IPv4_CheckSum_Ctrl_Tx_Addr),
		.Tx_Data				(IPv4_CheckSum_Ctrl_Tx_Data),
		.Tx_Word_Strobe	(IPv4_CheckSum_Ctrl_Tx_Word_Strobe),

		.Reset				(Reset),

		.IPv4_CheckSum_Complete	(IPv4_CheckSum_Ctrl_Complete),

		.IP_ID				(Identification)
		);

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
// ------------------- data parcer 
//--	ParcerCycle_SRFF.(S,clk,R) = (Rx_Parcer_RQ,Clock,ParcerCycleEnd);   -- parcer work cycle 
	SRFF ParcerCycle_SRFF
	(
		.S		(Rx_Parcer_RQ),
		.CLK	(Clock),
		.R		(ParcerCycleEnd_o),
		.q		(Progress_Flag)
		);
//--	ParcerCycleEnd = Edge_Sensing_Sync(.d=(MAC_Decoder.Rx_Error_MAC | ARP_Decoder.Rx_IP_Error | ARP_Decoder.Rx_TRUE_RQ | IPv4_Decoder.Rx_Error_IP | IPv4_CheckSum_Ctrl.IPv4_CheckSum_Complete | IPv4_Decoder.Rx_NOT_RQ),.clk=Clock);
	Edge_Sensing ParcerCycleEnd
	(
		.clk	(Clock),
		.d		(MAC_Decoder_Rx_Error_MAC | ARP_Decoder_Rx_IP_Error | ARP_Decoder_Rx_TRUE_RQ | IPv4_Decoder_Rx_Error_IP | IPv4_CheckSum_Ctrl_Complete | IPv4_Decoder_Rx_NOT_RQ),
		.q		(ParcerCycleEnd_o)
		);
//--	ParcerCycleEnd = Edge_Sensing_Sync(.d=(MAC_Decoder.Rx_Error_MAC | ARP_Decoder.Rx_IP_Error | IPv4_Decoder.Rx_Error_IP | IPv4_Decoder.Rx_NOT_RQ),.clk=Clock);
	Edge_Sensing ParcerCycleERR
	(
		.clk	(Clock),
		.d		(MAC_Decoder_Rx_Error_MAC | ARP_Decoder_Rx_IP_Error | IPv4_Decoder_Rx_Error_IP | IPv4_Decoder_Rx_NOT_RQ),
		.q		(CycleEndErr)
		);

	Edge_Sensing Tx_Start_ES
	(
		.clk	(Clock),
		.d		(ARP_Decoder_Tx_Start | IPv4_CheckSum_Ctrl_Complete),
		.q		(Tx_Start)
		);

	assign Rx_Addr_o = MAC_Decoder_Rx_Addr | ARP_Decoder_Rx_Addr | IPv4_Decoder_Rx_Addr | IPv4_CheckSum_Ctrl_Rx_Addr;

	genvar i;
	generate
		for (i=0; i <= 10; i=i+1) 
		begin: Tx_Addr_i
			always @(Clock) 
			begin
				Tx_Addr[i] <= MAC_Decoder_Tx_Addr[i] | ARP_Decoder_Tx_Addr[i] | IPv4_Decoder_Tx_Addr[i] | IPv4_CheckSum_Ctrl_Tx_Addr[i];
			end
		end
	endgenerate
	generate
		for (i=0; i <= 15; i=i+1) 
		begin: Tx_Data_i
			always @(Clock) 
			begin
				Tx_Data[i] <= MAC_Decoder_Tx_Data[i] | ARP_Decoder_Tx_Data[i] | IPv4_Decoder_Tx_Data[i] | IPv4_CheckSum_Ctrl_Tx_Data[i];
			end
		end
	endgenerate
	always @(Clock) 
	begin
		Tx_Word_Strobe <= MAC_Decoder_Tx_Word_Strobe | ARP_Decoder_Tx_Word_Strobe | IPv4_Decoder_Tx_Word_Strobe | IPv4_CheckSum_Ctrl_Tx_Word_Strobe;
	end
	
endmodule
