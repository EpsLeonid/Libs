//-----------------------------------------------------------------------------
// Title       : Ethernet_TxRx
//-----------------------------------------------------------------------------
// File        : Ethernet_TxRx.v
// Company     : INP SB RAS
// Created     : 31/08/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Ethernet_TxRx
//-----------------------------------------------------------------------------
// Revision    : 1.0
// Revision    : 2.0 : Modify packets parser Eth_Up_Module -> Divided parsers in-out packets (Eth_ext_pkt and Eth_in_pkt)
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole or in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
//-----------------------------------------------------------------------------
// Libraries
//-----------------------------------------------------------------------------
//`include "Eth_parameters.v"
//-----------------------------------------------------------------------------
module Ethernet_TxRx #(
//-----------------------------------------------------------------------------
// Input Parameters
//-----------------------------------------------------------------------------
	parameter ETH_MODULE_FUNC_SET = 2,
	parameter NUM_MS_SIGNALS 		= 2
	)
	(
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire				System_Clock,
// -- PHY Ethernet I/O --------------------------------------------------------
	input		wire				Eth_Phy_RxClk,
	input		wire				Rx_Reset,
	input		wire				Carr,
	input		wire [3:0]		Rx_Data_nibble_input,
	input		wire				RxIntStart,
// -- Tx section --------------------------------------------------------------
	input		wire				Tx_Reset,
	input		wire				Eth_Phy_TxClk,
	output	wire				Eth_Phy_TxEn,
	output	wire [3:0]		Eth_Phy_TxD,
	output	wire				Eth_Phy_MdC,
	input		wire				Eth_Phy_MdIO,

	output	wire				Eth_RxTx_In_Progress,
	output	wire				Eth_Tx_End,
	output	wire				Transmit_of_Data_RQ,
// -- Standard bus connections ------------------------------------------------
	input		wire [NUM_MS_SIGNALS-1:0]		DataBus_In[15:0],
	output	wire [NUM_MS_SIGNALS-1:0]		DataBusOut[15:0],

	input		wire [NUM_MS_SIGNALS-1:0]		DataBusStrobe,
	input		wire [NUM_MS_SIGNALS-1:0]		Select,
	input		wire [NUM_MS_SIGNALS-1:0]		DirectIn,
	input		wire [NUM_MS_SIGNALS-1:0]		AddrBus_In[15:0],
	input		wire									Parcer_Reset=1'b0,
// -- Master Mode Signals -----------------------------------------------------
	output	wire [NUM_MS_SIGNALS-1:0]		AccessRequest,
	input		wire [NUM_MS_SIGNALS-1:0]		AccessGranted,
	output	wire [NUM_MS_SIGNALS-1:0]		DirectOut,
	output	wire [NUM_MS_SIGNALS-1:0]		AddrBusOut[15:0],
	output	wire [NUM_MS_SIGNALS-1:0]		RxIntStart_out,
// -- Test --------------------------------------------------------------------
	output	wire									Test_out
);

//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------
	wire				Byte_Output_Strobe;
	wire				Rx_Byte_Output_Strobe;
	wire [7:0]		Byte_Output;
	wire				Data_Frame_is_in_Progress;
	wire				Rx_Data_Frame_is_in_Progress;
	wire				Packet_Good_End;
	wire				Packet_bad_End;
	wire				Rx_Packet_Good_End;
	wire				Rx_Packet_bad_End;
	wire				Rx_Packet_End;
	wire				Eth_Tx_In_Progress;
	wire				Rx_Eth_Tx_In_Progress;
	wire				Tx_Data_RQ_in_progress_o;
	wire				Tx_Dummy_RQ_in_progress_o;
	wire				Eth_Tx_In_Progress_dff;
	wire				Tx_Eth_Tx_In_Progress;
	

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------
//----------------------------- Ethernet Reciver ------------------------------
	Fr_ether100_new Rx_Data_Layer_Coverter (
		.System_Clock						(System_Clock),
		.RxClk_Edge_at_System_Clock	(Eth_Phy_RxClk),
		.Reset								(Rx_Reset),
		.Carr									(Carr),
		.Rx_Data_nibble_input			(Rx_Data_nibble_input[3:0]),

		.Byte_Output_Strobe				(Byte_Output_Strobe),
		.Byte_Output						(Byte_Output[7:0]),
		.Data_Frame_is_in_Progress		(Data_Frame_is_in_Progress),
		.Packet_Good_End					(Rx_Packet_Good_End),
		.Packet_bad_End					(Rx_Packet_bad_End)
		);
//-----------------------------------------------------------------------------
//---------------------------- Ethernet Trasmitter ----------------------------
	Tx_Eth100_Sync Tx_Data_Layer_Coverter (
		.System_Clock						(System_Clock),
		.Reset								(Tx_Reset),
		.Transmit_of_Data_RQ				(Tx_FIFO_RQ_o),
		.Data_to_Transmit					(Out_FIFO_o),
//	-- Phy MII connection
		.MII_Tx_CLK							(Eth_Phy_TxClk),
		.MII_Tx_En							(Eth_Phy_TxEn),
		.MII_Tx_Data						(Eth_Phy_TxD[3:0]),
		
		.Byte_Readed_Strob				(Byte_Strobe_Tx),
		
//	-- Ethernet Data_Layer_Parser 
		.Eth_Tx_In_Progress				(Eth_Tx_In_Progress)
		);
//-----------------------------------------------------------------------------
//-------------------------- Ethernet Data_Layer_Parser -----------------------
// -- external ethernet packets
	generate
		if (ETH_MODULE_FUNC_SET == 1) begin: ExtEtherPkt
			Eth_ext_pkt Data_Layer_Parser (
				.Clock 						(System_Clock),
				.Byte_Strobe_Rx			(Rx_Byte_Output_Strobe),
				.Rx_Data						(Byte_Output[7:0]),
				.RxPacket_in_progress	(Rx_Data_Frame_is_in_Progress),
				.RxPacket_End				(Rx_Packet_End),
				.Packet_Good_End			(Rx_Packet_Good_End),
				.Packet_bad_End			(Rx_Packet_bad_End),
				.Out_FIFO_full				(Out_FIFO_f),
				.Tx_packet_AG				(Data_Tx_packet_RQ),
				.Tx_packet_RQ				(Data_Tx_packet_RQ),

		//		.Eth_Tx_In_Progress		(Tx_Eth_Tx_In_Progress),

		//		.Byte_Strobe_Tx			(Byte_Readed_Strob),

				.BUS_Clock					(System_Clock),
				.DataBus_In					(DataBus_In[0]),
				.DataBusStrobe				(DataBusStrobe[0]),
				.Reset						(Parcer_Reset),
				.DirectIn					(DirectIn[0]),
				.AddrBus_In					(AddrBus_In[0][12:0]),
				.Select_i					(Select_i[0]),

				.AccessGranted				(AccessGranted[0]),
		//	-- ?????????? ?????? ???????? ?????? ?? Ethernet 
				.RxIntStart					(RxIntStart),

				.Eth_RxTx_In_Progress	(Eth_RxTx_In_Progress),
				.DataBusOut					(DataBusOut[0]),
				.AccessRequest				(AccessRequest[0]),
				.DirectOut					(DirectOut[0]),
				.AddrBusOut					(AddrBusOut[0][15:0])
		//		.Transmit_of_Data_RQ		(Transmit_of_Data_RQ),
		//	-- test signals
		//		.RxIntStart_out			(RxIntStart_out)
			);
		end 
	endgenerate
	
	generate
		if (ETH_MODULE_FUNC_SET == 2) begin: EtherPkt
// -- external ethernet packets
			Eth_ext_pkt Data_Layer_Parser (
				.Clock 						(System_Clock),
				.Byte_Strobe_Rx			(Rx_Byte_Output_Strobe),
				.Rx_Data						(Byte_Output[7:0]),
				.RxPacket_in_progress	(Rx_Data_Frame_is_in_Progress),
				.RxPacket_End				(Rx_Packet_End),
				.Packet_Good_End			(Rx_Packet_Good_End),
				.Packet_bad_End			(Rx_Packet_bad_End),
				.Out_FIFO_full				(Out_FIFO_f),
				.Tx_packet_AG				(Tx_Data_packet_AG),
				.Tx_packet_RQ				(Data_Tx_packet_RQ),

		//		.Eth_Tx_In_Progress		(Tx_Eth_Tx_In_Progress),

		//		.Byte_Strobe_Tx			(Byte_Readed_Strob),

				.BUS_Clock					(System_Clock),
				.DataBus_In					(DataBus_In[0]),
				.DataBusStrobe				(DataBusStrobe[0]),
				.Reset						(Parcer_Reset),
				.DirectIn					(DirectIn[0]),
				.AddrBus_In					(AddrBus_In[0][12:0]),
				.Select_i					(Select_i[0]),

				.AccessGranted				(AccessGranted[0]),
		//	-- ?????????? ?????? ???????? ?????? ?? Ethernet 
				.RxIntStart					(RxIntStart),

				.Eth_RxTx_In_Progress	(Eth_RxTx_In_Progress),
				.DataBusOut					(DataBusOut[0]),
				.AccessRequest				(AccessRequest[0]),
				.DirectOut					(DirectOut[0]),
				.AddrBusOut					(AddrBusOut[0][15:0])
		//		.Transmit_of_Data_RQ		(Transmit_of_Data_RQ),
		//	-- test signals
		//		.RxIntStart_out			(RxIntStart_out)
			);

// -- internal ethernet packets
			Eth_int_pkt Dummy_Layer_Parser (
				.Clock 					(System_Clock),
				.Int_Start				(RxIntStart),
				.Out_FIFO_full			(Out_FIFO_f),
				.Tx_packet_AG			(Tx_Dummy_packet_AG),
				.Tx_packet_RQ			(Dummy_Tx_packet_RQ),
			
				.BUS_Clock				(System_Clock),
				.DataBus_In				(DataBus_In[1]),
				.DataBusStrobe			(DataBusStrobe[1]),
			
				.Reset					(Parcer_Reset),
				.DirectIn				(DirectIn[1]),
				.AddrBus_In				(AddrBus_In[1][12:0]),
				.Select					(Select[1]),
			 
				.AccessGranted			(AccessGranted[1]),

				.DataBusOut				(DataBusOut[1]),
				.AccessRequest			(AccessRequest[1]),
				.DirectOut				(DirectOut[1]),
				.AddrBusOut				(AddrBusOut[1][15:0]),
				.Eth_Tx_End				(Eth_Tx_End)
			);
		end 
	endgenerate

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
	generate
		if (ETH_MODULE_FUNC_SET == 2) begin: Int_ExtEtherPkt
//	Tx_Dummy_packet_RQ.(S,clk,R)						= (Dummy_Layer_Parser.Tx_packet_RQ, System_Clock, Tx_Dummy_packet_AG);
			SRFF Tx_Dummy_packet_RQ_SR
			(
				.S		(Dummy_Layer_Parser_Tx_packet_RQ),
				.CLK	(System_Clock),
				.R		(Tx_Dummy_packet_AG),
				.q		(Tx_Dummy_packet_RQ_o)
			);
//	Tx_Data_packet_RQ.(S,clk,R)						= (Data_Layer_Parser.Tx_packet_RQ, System_Clock, Tx_Data_packet_AG);
			SRFF Tx_Data_packet_RQ_SR
			(
				.S		(Data_Layer_Parser_Tx_packet_RQ),
				.CLK	(System_Clock),
				.R		(Tx_Data_packet_AG),
				.q		(Tx_Data_packet_RQ_o)
			);
			case ({Tx_Dummy_packet_RQ_o, Tx_Data_packet_RQ_o, Tx_Dummy_RQ_in_progress_o, Tx_Data_RQ_in_progress_o})
				 4'b1000: begin: Tx_Dummy_packet_RQ
								assign Tx_Dummy_RQ_in_progress_s = 1'b1; 
								assign Tx_Dummy_packet_AG = 1'b1; 
							end
				 4'b1100: begin: Tx_Dummy_Data_packet_RQ
								assign Tx_Dummy_RQ_in_progress_s = 1'b1; 
								assign Tx_Dummy_packet_AG = 1'b1; 
							end
				 4'b0100: begin: Tx_Data_packet_RQ
								assign Tx_Data_RQ_in_progress_s  = 1'b1; 
								assign Tx_Data_packet_AG  = 1'b1; 
							end
				 default: begin: Def
								assign Tx_Data_RQ_in_progress_s  = 1'b0;
								assign Tx_Dummy_RQ_in_progress_s = 1'b0; 
								assign Tx_Data_packet_AG			= 1'b0;
								assign Tx_Dummy_packet_AG			= 1'b0;
							end
			endcase
//	Tx_Dummy_RQ_in_progress.(clk,R) = (System_Clock,Dummy_Layer_Parser.Tx_End_Pkt_for_FIFO);
			SRFF Tx_Dummy_RQ_in_progress
			(
				.S		(Tx_Dummy_RQ_in_progress_s),
				.CLK	(System_Clock),
				.R		(Tx_Dummy_RQ_in_progress_r),
				.q		(Tx_Dummy_RQ_in_progress_o)
			);
//	Tx_Data_RQ_in_progress.(clk,R)  = (System_Clock,Data_Layer_Parser.Tx_End_Pkt_for_FIFO);
			SRFF Tx_Data_RQ_in_progress
			(
				.S		(Tx_Data_RQ_in_progress_s),
				.CLK	(System_Clock),
				.R		(Tx_Data_RQ_in_progress_r),
				.q		(Tx_Data_RQ_in_progress_o)
			);
		end
	endgenerate
	
	generate
		if (ETH_MODULE_FUNC_SET == 1) begin: Out_FIFO_1
			Eth_In_FIFO4kb Out_FIFO
			(
				.din		({Data_Tx_End_Pkt_for_FIFO, 1'b0, Data_Tx_Data_for_FIFO}), //--DFF(.d=TxRQ_Reset_ES.q,.clk=Clock);
				.clk		(System_Clock),
				.wr_en	(Data_Tx_Strobe_for_FIFO), //--DFF(.d=TxRQ_SRFF.q,.clk=Clock);
				.rd_en	(Byte_Strobe_Tx),
				.dout		(Out_FIFO_o),
				.full		(Out_FIFO_f),
				.empty	(Out_FIFO_e)
				);
		end
	endgenerate
	
	generate
		if (ETH_MODULE_FUNC_SET == 2) begin: Out_FIFO_2
			if ((Tx_Dummy_RQ_in_progress_o == 1'b0) & (Tx_Data_RQ_in_progress_o == 1'b1)) begin: Tx_Data_RQ_in_progress
				Eth_In_FIFO4kb Out_FIFO
				(
					.din		({Data_Tx_End_Pkt_for_FIFO, 1'b0, Data_Tx_Data_for_FIFO}), //--DFF(.d=TxRQ_Reset_ES.q,.clk=Clock);
					.clk		(System_Clock),
					.wr_en	(Data_Tx_Strobe_for_FIFO), //--DFF(.d=TxRQ_SRFF.q,.clk=Clock);
					.rd_en	(Byte_Strobe_Tx),
					.dout		(Out_FIFO_o),
					.full		(Out_FIFO_f),
					.empty	(Out_FIFO_e)
					);
			end else begin: Tx_Dummy_RQ_in_progress
				Eth_In_FIFO4kb Out_FIFO
				(
					.din		({Dummy_Tx_End_Pkt_for_FIFO, 1'b0, Dummy_Tx_Data_for_FIFO}), //--DFF(.d=TxRQ_Reset_ES.q,.clk=Clock);
					.clk		(System_Clock),
					.wr_en	(Dummy_Tx_Strobe_for_FIFO), //--DFF(.d=TxRQ_SRFF.q,.clk=Clock);
					.rd_en	(Byte_Strobe_Tx),
					.dout		(Out_FIFO_o),
					.full		(Out_FIFO_f),
					.empty	(Out_FIFO_e)
					);
			end
		end
	endgenerate
	
	SRFF Tx_FIFO_RQ
	(
		.S		(!Out_FIFO_e & !Eth_Tx_In_Progress & !GuardTime_o),
		.CLK	(System_Clock),
		.R		(Out_FIFO_o[9] & Byte_Strobe_Tx),
		.q		(Tx_FIFO_RQ_o)
	);
	
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
	always @(posedge Clock)
	begin
		Eth_Tx_In_Progress_dff <= Eth_Tx_In_Progress;
	end
	
	SRFF GuardTime
	(
		.S		((!Eth_Tx_In_Progress) & Eth_Tx_In_Progress_dff),
		.CLK	(Clock),
		.R		(GuardTime_Cnt_Rst),
		.q		(GuardTime_o)
		);
	
	assign Transmit_of_Data_RQ = Tx_FIFO_RQ;
	assign Test_out = 1'b0;
	
	always @(posedge Clock)
	begin
		Rx_Byte_Output_Strobe			<= Byte_Output_Strobe;
		Rx_Packet_Good_End				<= Packet_Good_End;
		Rx_Packet_bad_End					<= Packet_bad_End;
		Rx_Packet_End						<= (Packet_Good_End | Packet_bad_End);
		Rx_Data_Frame_is_in_Progress	<= Data_Frame_is_in_Progress;
		Tx_Eth_Tx_In_Progress			<= Eth_Tx_In_Progress;
	end

endmodule
