//-----------------------------------------------------------------------------
// Title       : Tx_Eth100_Sync
//-----------------------------------------------------------------------------
// File        : Tx_Eth100_Sync.v
// Company     : INP SB RAS
// Created     : 07/09/2022
// Created by  : Leonid Epshteyn
//-----------------------------------------------------------------------------
// Description : Tx_Eth100_Sync
//-----------------------------------------------------------------------------
// Revision    : 1.0
//-----------------------------------------------------------------------------
// Copyright (c) 2022 INP SB RAS
// This work may not be copied, modified, re-published, uploaded, executed, or
// distributed in any way, in any medium, whether in whole or in part, without
// prior written permission from INP SB RAS.
//-----------------------------------------------------------------------------
module Tx_Eth100_Sync (
//-----------------------------------------------------------------------------
// Input-Output Ports
//-----------------------------------------------------------------------------
	input		wire				System_Clock,
	input		wire				Reset,							 //-- Reset at System_Clock
	input		wire				Transmit_of_Data_RQ,			 //-- While it's active, we transmitting data.
	input		wire [7:0]		Data_to_Transmit,				 //-- External incomming data Byte, buffered to MII_Eth_Tx_CLK
	output	wire				Byte_Readed_Strob,			 //-- System_Clock referenced aknolege (1T clock)
	output	wire				Eth_Tx_In_Progress,			 //-- Transmittion is in progress(flag), System_Clock referenced
	output	reg				TxClk_Edge_at_System_Clock, //-- Implicit timing of external packet-shaping logic

// -- Interface to Phy device IC
	input		wire				MII_Tx_CLK,	 //-- Phy IC clock
	output	wire				MII_Tx_En,	 //-- Transmittion's in progress(flag).		(to i82555)
	output	wire [3:0]		MII_Tx_Data, //-- Data Nibble.					(to i82555)
// -- No collision detect in mind
		
// --Old signals
	output	wire				EtxRdy ,	 //-- Ready to work out next byte(flag) - (not really data sampled!!!)
	output	wire				crc_out,	 //-- CRC32 Transmition (flag).
		
// -- Debug used
	output	wire				Frame_Started_Strobe,	 //
	output	wire [3:0]		TX_PHASE_VP,	 //
	output	wire				Data_is_available,	 //
	output	wire				Load_Data_to_Shift_Reg,	 //
	output	wire [31:0]		CRC_check_out	 //
);
//-----------------------------------------------------------------------------
// Parameters
//-----------------------------------------------------------------------------
	parameter PREAMBLE		= 8'b01010101;
	parameter JK_BYTE			= 8'b11010101;
//-----------------------------------------------------------------------------
// Signal declarations
//-----------------------------------------------------------------------------
	reg 				MII_Tx_CLK_o;
	wire 				TxClk_Edg_at_System_Clock_o;
	reg 				TxClk_Edge_at_System_Clock_o;
	wire 				Data_is_available_o;
	wire [7:0]		Input_Data_Buffer_o;
	wire 				progress_r;
	wire 				progress_o;
	wire 				nibble_cnt_d;
	wire 				nibble_cnt_o;
	wire 				EtxInPro_o;
	wire 				Start_Frame_Strob_o;
	wire 				Next_byte_Rq_o;

	reg 				crc32_reset = 1'b0;
	reg 				crc32_write = 1'b0;
	wire [31:0]		crc32_o;

	wire 				crc_ena_f_o;
	reg 				crc_ena_f_s = 1'b0;
	reg 				crc_ena_f_r = 1'b0;
	
	reg 				tx_phase_en = 1'b0;
	wire 				tx_phase_sclr;
	wire [3:0]		tx_phase_o;

	//type sm is (s_idle, s_preamble, s_jk, s_data, s_crc, s_err); -- possible states
//	wire ctrl : sm; //-- state control signal
	parameter S_IDLE		= 6'b000001;
	parameter S_PREAMBLE	= 6'b000010;
	parameter S_JK			= 6'b000100;
	parameter S_DATA		= 6'b001000;
	parameter S_CRC		= 6'b010000;
	parameter S_ERR		= 6'b100000;

	(* FSM_ENCODING="ONE-HOT", SAFE_IMPLEMENTATION="NO" *) reg [5:0] sm = S_IDLE;

	reg 				sm_S_DATA;
	wire 				sm_S_IDLE;
	reg 				sm_S_CRC;
//	wire 				sm_clk;
//	wire 				sm_ena;
	
	reg 				Load_Data_to_Shift = 1'b0;
	
	reg [7:0]		shift_reg_data;
	wire [3:0]		shift_reg_o;

	reg 				end_n = 1'b0;
	reg 				idle_n;
	
	reg [3:0]		ExtDat_d;
	wire [3:0]		EtxDat_o;
	reg [4:0]		Fast_Out_Buff;

//-----------------------------------------------------------------------------
// Sub Module Section
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Process Section
//-----------------------------------------------------------------------------

	always @(posedge System_Clock)
	begin
		MII_Tx_CLK_o <= MII_Tx_CLK;
	end

//--	TxClk_Edg_at_System_Clock.(clk,d)	=	(System_Clock, (DFF(.clk=System_Clock, .d=MII_Tx_CLK)));
//--	TxClk_Edg_at_System_Clock : entity work.Edge_Sensing_Sync;
	Edge_Sensing TxClk_Edg_at_System_Clock
	(
		.clk		(System_Clock),
		.d			(MII_Tx_CLK_o), //--DFF
		.q			(TxClk_Edg_at_System_Clock_o)
	);

//--	TxClk_Edge_at_System_Clock	=	(DFF(.clk=System_Clock, .d=TxClk_Edg_at_System_Clock.q));
	always @(posedge System_Clock)
	begin
		TxClk_Edge_at_System_Clock_o <= TxClk_Edg_at_System_Clock_o;
		TxClk_Edge_at_System_Clock <= TxClk_Edg_at_System_Clock_o;
	end 
	
//--	Data_is_available	=	DFFE(.clk=System_Clock, .ena=TxClk_Edge_at_System_Clock, .d=Transmit_of_Data_RQ); //-- resync version of Start_Frame_RQ, EtxGo 
	DFFE Data_is_available_DFFE
	(
		.clk		(System_Clock),
		.en		(TxClk_Edge_at_System_Clock_o),
		.d			(Transmit_of_Data_RQ),
		.q			(Data_is_available_o)
		);

	assign Data_is_available = Data_is_available_o;

	//--progress.(S, clk, R, ena)	=	(Transmit_of_Data_RQ, System_Clock, (end_n OR Reset), TxClk_Edge_at_System_Clock);
	SRFF progress
	(
		.S			(Transmit_of_Data_RQ),
		.CLK		(System_Clock),
		.R			(end_n | Reset),
		.ena		(TxClk_Edge_at_System_Clock_o),
		.q			(progress_o)
		);

//--	EtxInPro.(d, clk,    ena)	=	(progress.q,     System_Clock,                   TxClk_Edge_at_System_Clock);
	DFFE EtxInPro
	(
		.clk		(System_Clock),
		.en		(TxClk_Edge_at_System_Clock_o),
		.d			(progress_o),
		.q			(EtxInPro_o)
		);

//--	Start_Frame_Strob.(d, clk,    ena)	=	(Transmit_of_Data_RQ,     System_Clock,       TxClk_Edge_at_System_Clock);
	DFFE Start_Frame_Strob
	(
		.clk		(System_Clock),
		.en		(TxClk_Edge_at_System_Clock_o),
		.d			(Transmit_of_Data_RQ),
		.q			(Start_Frame_Strob_o)
		);

	assign Eth_Tx_In_Progress		= EtxInPro_o;
	assign Frame_Started_Strobe	= Start_Frame_Strob_o;

//--	Input_Data_Buffer.(clock, data[], load, enable)=(System_Clock, Data_to_Transmit[], VCC, Next_byte_Rq.q);
	ShiftReg 
	#(
		.WIDTH	(8)
	) Input_Data_Buffer
	(
		.clock	(System_Clock),
		.data		(Data_to_Transmit),
		.enable	(Next_byte_Rq_o),
		.q			(Input_Data_Buffer_o)
		);

//--# Nibble counter (VCC - last nibble of the byte). ---------------------------
//--	nibble_cnt.(d, clk, ena)	=	((!nibble_cnt.q AND !Start_Frame_Strob.q) or idle_n, System_Clock, TxClk_Edge_at_System_Clock);
	DFFE nibble_cnt
	(
		.clk		(System_Clock),
		.en		(TxClk_Edge_at_System_Clock_o),
		.d			((!nibble_cnt_o) & (!Start_Frame_Strob_o) | idle_n),
		.q			(nibble_cnt_o)
		);

//--	Next_byte_Rq.(clk, d)		=	(System_Clock, Load_Data_to_Shift_Reg);//--Edge_Sensing_Sync
//--	Next_byte_Rq : entity work.Edge_Sensing_Sync;
	Edge_Sensing Next_byte_Rq
	(
		.clk		(System_Clock),
		.d			(Load_Data_to_Shift), //--DFFE
		.q			(Next_byte_Rq_o)
		);

	assign Byte_Readed_Strob = Next_byte_Rq_o;	//-- now referenced to real data load, not dummy as EtxRdy is
	
	assign EtxRdy = !nibble_cnt_o & sm_S_DATA; //-- now ignored

//--# Shift register. -----------------------------------------------------------	
//-- No Reset needed.
//--	shift_reg.(clock, enable, load)	=	(System_Clock, TxClk_Edge_at_System_Clock, nibble_cnt.q);
	uni_shift_gt 
	#(
		.N_IN		(8), 
		.N_OUT	(4)
	) shift_reg
	(
		.clock	(System_Clock),
		.data_i	(shift_reg_data),
		.load		(nibble_cnt_o),
		.enable	(TxClk_Edge_at_System_Clock_o),
		.data_o	(shift_reg_o)
		);
//	genvar i,j
//	generate
//		for (i=0; i <= 3; i=i+1) 
//		begin: sh_regs_i
//			for (j=0; j <= 1; j=j+1) 
//			begin: sh_regs_j
//				always @(posedge System_Clock)
//				begin
//					if (TxClk_Edge_at_System_Clock_o)
//					begin
//						if (nibble_cnt_o)
//						begin
//							sh_regs[i][j] = shift_reg_data[j * 4 + i];
//						end
//					end
//				end
//			end
//		end
//	endgenerate;
	
//--# Crc counter. --------------------------------------------------------------
	crc32eth4 crc32ether
	(
		.clock			(System_Clock), //--MII_Tx_CLK;
		.enable			(TxClk_Edge_at_System_Clock_o),//--vcc; %progress.q or start_n;%
		.Data_i			({shift_reg_o[0],shift_reg_o[1],shift_reg_o[2],shift_reg_o[3]}),
		.reset			(crc32_reset),
		.write_i			(crc32_write),
		.data_o			(CRC_check_out)
		);
//	assign CRC_check_out[31:0] = crc32_o;
	assign crc_out = sm_S_CRC;

	//-- Reset & write are in the state machine.

//--# The main state-machine logic. ---------------------------------------------
//--	tx_phase.clock 	= System_Clock;//--MII_Tx_CLK;
//--	tx_phase.sclr = Reset # end_n;
//--	tx_phase		: lpm_counter 	//-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
//--						WITH (LPM_WIDTH=4);

	V_Counter  //-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	#(
		.WIDTH	(4)
	) tx_phase
	(
		.clock 	(System_Clock),
		.cnt_en	(tx_phase_en),
		.clk_en	(1'b1),
		.sclr		(Reset | end_n),
		.q			(tx_phase_o)
		);

	//-- 'enable' is in the state machine.
	assign TX_PHASE_VP = tx_phase_o;

//--	idle_n	= !(sm.q0 # sm.q1 # sm.q2 # sm.q3 # sm.q4);
//	assign idle_n = sm_S_IDLE; //-- To do!
	
	always@(posedge System_Clock)
		if (nibble_cnt_o & TxClk_Edge_at_System_Clock_o) begin
			(* FULL_CASE, PARALLEL_CASE *) case (sm)
				S_IDLE : begin
					if (Data_is_available) begin
						idle_n				<= 1'b1;
						shift_reg_data		<= PREAMBLE;
						tx_phase_en			<= TxClk_Edge_at_System_Clock_o;//--vcc;
						sm						<= S_PREAMBLE;
					end
				end
				S_PREAMBLE : begin
					tx_phase_en <= nibble_cnt_o & TxClk_Edge_at_System_Clock_o;
					if (tx_phase_o < 8) begin
						shift_reg_data		<= PREAMBLE;
					end
					else if (tx_phase_o == 8) begin
						shift_reg_data		<= JK_BYTE;
						sm 					<= S_JK;
					end
					else begin
						sm 					<= S_ERR;
					end
				end
				S_JK : begin
					tx_phase_en				<= nibble_cnt_o & TxClk_Edge_at_System_Clock_o;
					crc32_reset				<= nibble_cnt_o;
					shift_reg_data			<= Input_Data_Buffer_o;//--EtxDi();
					Load_Data_to_Shift	<= nibble_cnt_o;//-- # start_n;
					sm							<= S_DATA;
				end
				S_DATA : begin
					if (!Data_is_available) begin
						sm_S_DATA			<= 1'b1;
						crc_ena_f_s			<= nibble_cnt_o;
						crc32_write			<= 1'b1;
						tx_phase_en			<= nibble_cnt_o & TxClk_Edge_at_System_Clock_o;
						sm						<= S_CRC;
					end
					else begin
						crc32_write				<= 1'b1;
						shift_reg_data			<= Input_Data_Buffer_o;//--EtxDi();
						Load_Data_to_Shift	<= nibble_cnt_o;//-- # start_n;
					end
				end
				S_CRC : begin
					if (tx_phase_o <14 & tx_phase_o > 9)
						tx_phase_en			<= nibble_cnt_o & TxClk_Edge_at_System_Clock_o;
					else if (tx_phase_o == 14) begin
						sm_S_CRC				<= 1'b1;
						crc_ena_f_r			<= nibble_cnt_o;
						end_n					<= nibble_cnt_o & TxClk_Edge_at_System_Clock_o;
						sm						<= S_IDLE;
					end
					else begin
						sm						<= S_ERR;
					end
				end
				default: begin  // Fault Recovery
					if (!Data_is_available)
						sm						<= S_IDLE;
				end
			endcase
		end
	
	assign Load_Data_to_Shift_Reg = Load_Data_to_Shift;

//--	crc_ena_f.(clk, ena) 	=	(System_Clock, TxClk_Edge_at_System_Clock); //--SRFFE
	SRFF crc_ena_f
	(
		.S		(crc_ena_f_s),
		.CLK	(System_Clock),
		.R		(crc_ena_f_r),
		.ENA	(TxClk_Edge_at_System_Clock_o),
		.q		(crc_ena_f_o)
		);

	genvar i;
	generate
		for (i=0; i <= 3; i=i+1) 
		begin: ExtDat_i
			always @(posedge System_Clock)
			begin
				if (TxClk_Edge_at_System_Clock)
				begin
					ExtDat_d[i] <= ((CRC_check_out[31-i] & crc_ena_f_o ) | (shift_reg_o[i] & !crc_ena_f_o));
				end
			end
			DFFE EtxDat
			(
				.clk	(System_Clock),
				.en	(TxClk_Edge_at_System_Clock_o),
				.d		(ExtDat_d[i]),
				.q		(EtxDat_o[i])
				);
		end
	endgenerate
	
	//-- Fast_buff length should be adjusted to ensure 12ns setup time and 0ns hold time to MII_Tx_CLK
	//-- of Phy device, depending of TX_CLK phase sampling method
	always @(posedge System_Clock) //--	Fast_Out, 3..0 - data, 4 - Tx_En
	begin
		Fast_Out_Buff[3:0] <= EtxDat_o;
		Fast_Out_Buff[4]	 <= EtxInPro_o;
	end
//-----------------------------------------------------
	assign MII_Tx_Data[3:0]	= Fast_Out_Buff[3:0];
	assign MII_Tx_En			= Fast_Out_Buff[4]; 

endmodule
