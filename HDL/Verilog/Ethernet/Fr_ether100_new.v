//-----------------------------------------------------------------------------
// Title       : Fr_ether100_new
//-----------------------------------------------------------------------------
// File        : Ethernet_TxRx.v
// Company     : INP SB RAS
// Created     : 31/08/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Fr_ether100_new
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole or in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Fr_ether100_new (
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire			System_Clock, // FPGA Clock
// -- DDR sampled and alighned Inputs
	input		wire			RxClk_Edge_at_System_Clock,	 // --resynchronized in external DDR-buffer
	input		wire [3:0]	Rx_Data_nibble_input,			 // -- Incoming Data.			--resynchronized in external DDR-buffer
	input		wire			Carr,									 // -- Carrier ( receive...)	--resynchronized in external DDR-buffer
	input		wire			Rx_Dv,								 // --resynchronized in external DDR-buffer
	input		wire			Reset,						 // -- Reset.		already synchronouse
// -- Minimal set of outputs
	output	wire			Data_Frame_is_in_Progress,
	output	reg			Byte_Output_Strobe,
	output	wire [7:0]	Byte_Output,
	output	reg			Packet_Good_End,
	output	reg			Packet_bad_End,
// -- Extended set of outputs
	output	wire			Packet_is_too_long,				 // --to Warning counter
	output	wire [11:0]	Row_output,							 // --Virtual pin
	output	wire			Gap_is_too_small,					 // --to Warning counter
	output	wire [4:0]	Gap_output,							 // --Virtual pin
	output	wire [4:0]	Preamble_progress_Cnt_out,		 // --Virtual pin
	output	reg			End_of_preamble,					 // --Virtual pin
	output	wire			Preamble_failed,					 // --to Warning counter
	output	wire			Byte_Ready_Strobe,				 // -- note the difference to 'Byte_Output_Strobe'
// -- CRC adjust outputs
	output	wire [31:0]	CRC_check_out,						 // --Virtual pin
	output	wire			CRC_Good,							 // --to Warning counter
	output	wire [31:0]	Bypass_Shifter_Output			 // --Virtual pin
	);
//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------
	wire 				Rx_Clock_Edge;
	wire 				Rx_Data_Valid;
	wire [11:0]		Row_Cnt_o;
	wire 				Rx_Data_Valid_Back_Edge_o;
	wire [4:0]		Gap_Cnt_o;
	wire 				Interframe_Gap_in_Progress_r;
	wire 				Interframe_Gap_in_Progress_o;
	
	wire 				Preamble_progress_Cnt_cen;
	wire 				Preamble_progress_Cnt_sclr;
	wire [4:0]		Preamble_progress_Cnt_o;
	wire 				Preamble_check_enable;
	wire 				Preamble_yet_good_s;
	wire 				Preamble_yet_good_o;
	wire 				End_of_good_Preamble;
	
	wire 				Frame_of_Data_is_in_Progress_o;
	
	wire 				Nibble_Cnt_sclr;
	wire [11:0]		Nibble_Cnt_o;
	wire 				Byte_Ready_Strobe_i;
	
	wire [3:0]		LSN_Register_o;
	wire 				Byte_output_Register_en;
	wire [7:0]		Byte_output_Register_o;
	
	reg [31:0]		Bypass_Shifter;
	wire 				CRC32_new_reset;
	wire [31:0]		CRC32_new_o;
	wire 				CRC_Good_i;
	reg [2:0]		CRC_Good_Recorder;
	wire 				CRC_of_packet_Looks_Good;
	
	reg 				Nibble_Cnt_o_good;

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------
// ------MII signalling proc
	assign Rx_Clock_Edge = RxClk_Edge_at_System_Clock;
	assign Rx_Data_Valid = Carr; // and Rx_DV;

// ----------------------------------------------------------
// ---------------------Simple timing check

	V_Counter  //-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	#(
		.WIDTH	(12)
	) Row_Cnt
	(
		.clock 	(System_Clock),
		.cnt_en	(Rx_Clock_Edge),
		.clk_en	(1'b1),
		.sclr		(!Rx_Data_Valid),
		.q			(Row_Cnt_o)
	);

	assign Packet_is_too_long = ((Row_Cnt_o[11:0] == (1530*2)) & (Rx_Clock_Edge)) ? 1'b1 : 1'b0; //--including preamble and CRC, non-jumbo
	assign Row_output = Row_Cnt_o;

//--	Rx_Data_Valid_Back_Edge.(clk, d)		=	(System_Clock, !Rx_Data_Valid);
	Edge_Sensing Rx_Data_Valid_Back_Edge 
	(
		.clk	(System_Clock),
		.d		(!Rx_Data_Valid),
		.q		(Rx_Data_Valid_Back_Edge_o)
	);
//--	Gap_Cnt.(clock, cnt_en, clk_en, sclr)	=	(System_Clock, Interframe_Gap_in_Progress.q, Rx_Clock_Edge , Rx_Data_Valid);
	V_Counter  //-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	#(
		.WIDTH (5)
	) Gap_Cnt
	(
		.clock 	(System_Clock),
		.cnt_en	(Interframe_Gap_in_Progress_o),
		.clk_en	(Rx_Clock_Edge),
		.sclr		(Rx_Data_Valid),
		.q			(Gap_Cnt_o)
	);
	assign Gap_output = Gap_Cnt_o;

//--	Interframe_Gap_in_Progress.(S, R, clk, ena) =	(	Rx_Data_Valid_Back_Edge.q,
//--														(Gap_Cnt.q[] > 27) OR Rx_Data_Valid,
//--														System_Clock, VCC
//--													);
	SRFF Interframe_Gap_in_Progress
	(
		.S		(Rx_Data_Valid_Back_Edge_o),
		.CLK	(System_Clock),
		.R		((Gap_Cnt_o > 27) | (Rx_Data_Valid )),
		.ena	(1'b1),
		.q		(Interframe_Gap_in_Progress_o)
	);
	assign Gap_is_too_small = Interframe_Gap_in_Progress_o & Rx_Data_Valid;
 //------------------------End of Simple timing check

 //-- Is preamble good?
//--	Preamble_progress_Cnt.(clock, cnt_en, sclr)	=	(System_Clock, 
//--													Rx_Clock_Edge and Preamble_yet_good.q , 
//--													(!Rx_Data_Valid) or (!Preamble_yet_good.q) or Frame_of_Data_is_in_Progress.q
//--													);
	V_Counter  //-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	#(
		.WIDTH (5)
	) Preamble_progress_Cnt
	(
		.clock 	(System_Clock),
		.cnt_en	(Rx_Clock_Edge & Preamble_yet_good_o),
		.clk_en	(1'b1),
		.sclr		((!Rx_Data_Valid) | (!Preamble_yet_good_o) | Frame_of_Data_is_in_Progress_o),
		.q			(Preamble_progress_Cnt_o)
		);
//--	if (Row_Cnt.q[11..0] <= 20)   -- CRS assertion is far away from Data Valid, And Rx_DV is not guaranteed for data immediately
//--	then Preamble_check_enable = Rx_Clock_Edge and Rx_Data_Valid;
//--	else Preamble_check_enable = gnd;
//--	end if;
	assign Preamble_check_enable = (Row_Cnt_o[11:0] <= 20) ? (Rx_Clock_Edge & Rx_Data_Valid) : 1'b0; 

//--	Preamble_yet_good.(S, R, clk, ena) = (	Rx_Data_nibble_input[3..0] == b"0101", 
//--											Rx_Data_nibble_input[3..0] != b"0101", 
//--											System_Clock, Preamble_check_enable
//--										);
	SRFF Preamble_yet_good
	(
		.S		(Rx_Data_nibble_input[3:0] == 4'b0101),
		.CLK	(System_Clock),
		.R		(!Preamble_yet_good_s),
		.ena	(Preamble_check_enable),
		.q		(Preamble_yet_good_o)
	);

//--	IF  ( 	(Row_Cnt.q[11..0] <= 22) 				//--	not so far from begin
//--		and	(Preamble_progress_Cnt.q[4..0] >= 9)	//--	not too short good simbols of preamble (15 is max possible)
//--		and	(Rx_Data_nibble_input[3..0] == b"1101")	//--	SFD simbol valid
//--		and (Rx_Clock_Edge)
//--		)
//--	then	End_of_good_Preamble	=	VCC;
//--	else	End_of_good_Preamble	=	gnd;
//--	end IF;

	assign Preamble_check_enable = ((Row_Cnt_o <= 22) & (Preamble_progress_Cnt_o >= 9) & (Rx_Data_nibble_input[3:0] == 4'b1101) & (Rx_Clock_Edge)) 
												? 1'b1 : 1'b0; 

	always @(posedge System_Clock)
	begin
		End_of_preamble <= End_of_good_Preamble;
	end
	assign Preamble_progress_Cnt_out = Preamble_progress_Cnt_o;

//--	IF  ( 	(Row_Cnt.q[11..0] == 23) 
//--		and	(!Frame_of_Data_is_in_Progress.q)
//--		and (Rx_Clock_Edge)
//--		)
//--	then	Preamble_failed			=	VCC;
//--	else	Preamble_failed			=	gnd;
//--	end IF;
	assign Preamble_failed = ((Row_Cnt_o <= 23) & (!Frame_of_Data_is_in_Progress_o) & (Rx_Clock_Edge)) 
												? 1'b1 : 1'b0; 

 //--- Will we start frame data receive???

//--	Frame_of_Data_is_in_Progress.(S, R, clk, ena) =	(	End_of_good_Preamble,
//--														Rx_Data_Valid_Back_Edge.q,
//--														System_Clock, VCC);
	SRFF Frame_of_Data_is_in_Progress
	(
		.S		(End_of_good_Preamble),
		.CLK	(System_Clock),
		.R		(Rx_Data_Valid_Back_Edge_o),
		.q		(Frame_of_Data_is_in_Progress_o)
	);

	assign Data_Frame_is_in_Progress = Frame_of_Data_is_in_Progress_o;

 //------------------- End of preamble check-----------------------------

 //-- Error processing to be here!!!!!!!!!!!!!!!!!!!!!!!!!!!!
 //----------------------------------------------------------------------
//--	Nibble_Cnt.(clock, cnt_en, sclr)	=	(System_Clock, Rx_Clock_Edge, 
//--						((!Rx_Data_Valid) or(!Frame_of_Data_is_in_Progress.q) or End_of_good_Preamble));
	V_Counter //-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	#(
		.WIDTH (12)
	)Nibble_Cnt
	(
		clock 	(System_Clock),
		cnt_en	(Rx_Clock_Edge),
		clk_en	(1'b1),
		sclr		((!Rx_Data_Valid) | (!Frame_of_Data_is_in_Progress_o) | End_of_good_Preamble),
		q			(Nibble_Cnt_o)
	);

	assign Byte_Ready_Strobe = (Rx_Clock_Edge) & Nibble_Cnt_o[0] & Frame_of_Data_is_in_Progress_o;
//--	LSN_Register.(clock, enable, data[3..0], load)	=	(System_Clock, Rx_Clock_Edge, Rx_Data_nibble_input[3..0], VCC);
	ShiftReg 
		#(
			.WIDTH (4)
		) LSN_Register
		(
			.clock	(System_Clock),
			.data		(Rx_Data_nibble_input),
			.enable	(Rx_Clock_Edge),
			.q			(LSN_Register_o)
		);
//--	Byte_output_Register.(clock, enable, load)		=	(System_Clock, 
//--														Rx_Clock_Edge and Nibble_Cnt.q[0], VCC);
//--	Byte_output_Register.(data[3..0], data[7..4])	=	(LSN_Register.q[3..0], Rx_Data_nibble_input[3..0]);
	ShiftReg 
		#(
			.WIDTH (8)
		) Byte_output_Register
		(
			.clock		(System_Clock),
//			.data[3:0]	(),
			.data			({Rx_Data_nibble_input,LSN_Register_o}),
			.enable		(Rx_Clock_Edge & Nibble_Cnt_o[0]),
			.q				(Byte_output_Register_o)
		);

	assign Byte_Output = Byte_output_Register_o;
	always @(posedge System_Clock)
	begin
		Byte_Output_Strobe <= Byte_Ready_Strobe;
	end 

 //----------------------------------------------------------
//--	CRC32_new.clock			=	System_Clock;
//--	CRC32_new.in[3..0]		=	Bypass_Shifter[31..28].q; //-- to match delay of CRC calc

//--	CRC32_new.reset			=	(Nibble_Cnt.q[]==7) and Rx_Clock_Edge; --End_of_good_Preamble;
//--	CRC32_new.write			=	Rx_Clock_Edge;
//--	CRC32_new.enable		=	Rx_Clock_Edge;

//--	CRC_check_out[31..0]	=	CRC32_new.out[31..0];
//--	CRC_Good				=	CRC32_new.out[31..0]==Bypass_Shifter[].q;
	assign CRC32_new_reset = ((Nibble_Cnt_o == 7) & Rx_Clock_Edge) ? 1'b1 : 1'b0; 
	crc32eth4 CRC32_new(
		.clock		(System_Clock), //MII_Tx_CLK;
		.enable		(Rx_Clock_Edge),//vcc; %progress.q or start_n;%
		.Data_i		(Bypass_Shifter[31:28]), // to match delay of CRC calc
//		.reset		(CRC32_new_reset),
		.reset		((Nibble_Cnt_o == 7) & Rx_Clock_Edge),
		.write_i		(Rx_Clock_Edge),
		.data_o		(CRC32_new_o)
		);
	assign CRC_Good = (CRC32_new_o == Bypass_Shifter) ? 1'b1 : 1'b0; 
	
//--	Bypass_Shifter[].(clk, ena)	=	(System_Clock, Rx_Clock_Edge);
	genvar i, j;
	generate
		for (j=0; j <= 7; j=j+1) 
		begin: Bypass_Shifter_j
			for (i=0; i <= 3; i=i+1) 
			begin: Bypass_Shifter_i
				always @(posedge System_Clock)
				begin
					if (Rx_Clock_Edge)
					begin
						if (j == 0)
						begin
							Bypass_Shifter[4*j+i] <= Rx_Data_nibble_input[3-i];
						end
						else begin
							Bypass_Shifter[4*j+i] <= Bypass_Shifter[4*(j-1)+i];
						end
					end
				end
			end
		end
	endgenerate
	assign Bypass_Shifter_Output = Bypass_Shifter;

 //------------------------------------------------------------------------------------------
 //-- Since time interval from end of data and Rx_DV deassertion is not stable -see datasheet of lxt972-
 //-- we need allow to check CRC_Good with some margin in time!!!
//--	CRC_Good_Recorder.(clock, enable, shiftin)	=	(System_Clock, Rx_Clock_Edge, CRC_Good);
	always @(posedge System_Clock)
	begin
		if (Rx_Clock_Edge) begin
			CRC_Good_Recorder[0] <= CRC_Good ;
			CRC_Good_Recorder[1] <= CRC_Good_Recorder[0];
			CRC_Good_Recorder[2] <= CRC_Good_Recorder[1];
		end
	end
	assign CRC_of_packet_Looks_Good = (CRC_Good_Recorder(0) != 1'b0 & CRC_Good_Recorder(1) != 1'b0 & CRC_Good_Recorder(2) != 1'b0) ? 1'b1 : 1'b0; //-- CRC was good near Rx_DV deassertion Rx_Data_Valid_Back_Edge.q
//--	Packet_Good_End = DFF(.clk=System_Clock, .d= (Rx_Data_Valid_Back_Edge.q and CRC_of_packet_Looks_Good and DFF(.clk=System_Clock, .d=(Nibble_Cnt.q[] > (45*2)))));
//--	Packet_bad_End = DFF(.clk=System_Clock, .d= (Rx_Data_Valid_Back_Edge.q and (!CRC_of_packet_Looks_Good or !DFF(.clk=System_Clock, .d=(Nibble_Cnt.q[] > (45*2))))));
	always @(posedge System_Clock)
	begin
		if (Nibble_Cnt_o > (45*2)) begin
			Nibble_Cnt_o_good <= 1'b1;
		end
		else begin
			Nibble_Cnt_o_good <= 1'b0;
		end
		Packet_Good_End <= Rx_Data_Valid_Back_Edge_o & CRC_of_packet_Looks_Good & Nibble_Cnt_o_good;
		Packet_bad_End  <= Rx_Data_Valid_Back_Edge_o & !CRC_of_packet_Looks_Good & !Nibble_Cnt_o_good;
	end
	
endmodule
