----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 24/08/2022  
-- Design Name: 
-- Module  Name: Fr_ether100_new - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: v.1.0
-- Description: "Data reception from ethernet PHY or LANC"
------------------------------------------------------------------------------
-- Version 2.0, 
-- Copyright Arkady Selivanov, November 19, 1998

-- This module can be used to recieve data block from :
--
-- a. Ethernet Interface Adapter (tested with LXT905, LevelOne inc.).
-- b. Ethernet LAN Controller    (tested with  82557, Intel).
--
--#  _Carr_ input is under control of pin TEN (transmit enable) in case b.
--#  _Carr_ input is under control of pin CRS (carrier sense)   in case a.
--
-- Module search in incoming data EthDat Start Frame Delimeter (SFD).
--
-- After SFD reception serial to parallel convertion starts (LSB - goes first).
--
-- Every recieved data byte is stored in hold register with _RxRdy_ pulse.
--
-- Additionally, at the end of  RxProgr one of this outputs  generates 
-- 100ns(SysClk period) pulse: _PktEnd_ , _PktBad_
-- Reception stops after _Carr_ input low level is detected. 
-- Recieve progress indicator __RxProgr__ is active between SFD and Carr=0
------------------------------------------------------------------------------
-- WARNING !!! CRC32 is on output in Reverse Inverted Nibble order.

-- Dependencies: 
--
-- Revision: 
-- Revision 0.01 - File Created
-- Additional Comments: 
--
----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
-- Uncomment the following library declaration if using
-- arithmetic functions with Signed or Unsigned values
use IEEE.NUMERIC_STD.ALL;
-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
use IEEE.STD_LOGIC_ARITH.ALL;
use IEEE.STD_LOGIC_UNSIGNED.ALL;
use ieee.math_real.all;

library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.parameters.all;

entity Fr_ether100_new is
	Port (
		System_Clock					: in std_logic;	--FPGA Clock
	--DDR sampled and alighned Inputs
		RxClk_Edge_at_System_Clock	: in std_logic;		--resynchronized in external DDR-buffer
		Rx_Data_nibble_input			: in std_logic_vector (3 downto 0);		-- Incoming Data.			--resynchronized in external DDR-buffer
		Carr 								: in std_logic;		-- Carrier ( receive...)	--resynchronized in external DDR-buffer
		Rx_Dv								: in std_logic;									--resynchronized in external DDR-buffer
		Reset 							: in std_logic := '0';		-- Reset.		already synchronouse

--Minimal set of outputs
		Data_Frame_is_in_Progress	: out std_logic;
		Byte_Output_Strobe			: out std_logic;
		Byte_Output						: out std_logic_vector (7 downto 0);
		Packet_Good_End 				: out std_logic;
		Packet_bad_End					: out std_logic;
		
		
--Extended set of outputs
		Packet_is_too_long			: out std_logic;	--to Warning counter
		Row_output						: out std_logic_vector (11 downto 0);--Virtual pin
		Gap_is_too_small				: out std_logic;	--to Warning counter
		Gap_output						: out std_logic_vector (4 downto 0);--Virtual pin
		Preamble_progress_Cnt_out	: out std_logic_vector (4 downto 0);--Virtual pin
		End_of_preamble				: out std_logic;--Virtual pin
		Preamble_failed				: out std_logic;			--to Warning counter
		Byte_Ready_Strobe				: out std_logic;	-- note the difference to 'Byte_Output_Strobe'
--CRC adjust outputs
		CRC_check_out					: out std_logic_vector (31 downto 0);--Virtual pin
		CRC_Good							: out std_logic;	--to Warning counter
		Bypass_Shifter_Output		: out std_logic_vector (31 downto 0)--Virtual pin
		);

end Fr_ether100_new;

architecture Behavioral of Fr_ether100_new is

	signal Rx_Clock_Edge						: std_logic;
	signal Rx_Data_Valid						: std_logic;
	signal Row_Cnt_o							: std_logic_vector (11 downto 0);
	signal Rx_Data_Valid_Back_Edge_o		: std_logic;
	signal Gap_Cnt_o							: std_logic_vector (4 downto 0);
	signal Interframe_Gap_in_Progress_r	: std_logic;
	signal Interframe_Gap_in_Progress_o	: std_logic;
	
	signal Preamble_progress_Cnt_cen		: std_logic;
	signal Preamble_progress_Cnt_sclr	: std_logic;
	signal Preamble_progress_Cnt_o		: std_logic_vector (4 downto 0);
	signal Preamble_check_enable			: std_logic;
	signal Preamble_yet_good_s				: std_logic;
	signal Preamble_yet_good_o				: std_logic;
	signal End_of_good_Preamble			: std_logic;
	
	signal Frame_of_Data_is_in_Progress_o	: std_logic;
	
	signal Nibble_Cnt_sclr					: std_logic;
	signal Nibble_Cnt_o						: std_logic_vector (11 downto 0);
	signal Byte_Ready_Strobe_i				: std_logic;
	
	signal LSN_Register_o					: std_logic_vector (3 downto 0);
	signal Byte_output_Register_en		: std_logic;
	signal Byte_output_Register_o			: std_logic_vector (7 downto 0);
	
	signal Bypass_Shifter					: std_logic_vector (31 downto 0);
	signal CRC32_new_reset					: std_logic;
	signal CRC32_new_o						: std_logic_vector (31 downto 0);
	signal CRC_Good_i 						: std_logic;
	signal CRC_Good_Recorder				: std_logic_vector (2 downto 0);
	signal CRC_of_packet_Looks_Good		: std_logic;
	
	signal Nibble_Cnt_o_good				: std_logic;

begin

	------MII signalling proc
	Rx_Clock_Edge	<=	RxClk_Edge_at_System_Clock;
	Rx_Data_Valid	<=	Carr; --and Rx_DV;

	----------------------------------------------------------

	---------------------Simple timing check
--	Row_Cnt.(clock, cnt_en, sclr)	=	(System_Clock, Rx_Clock_Edge, !Rx_Data_Valid);
	Row_Cnt : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => 12
		)
	port map (
		clock 	=> System_Clock,
		cnt_en	=> Rx_Clock_Edge,
		clk_en	=> '1',
		sclr		=> not Rx_Data_Valid,
		q			=> Row_Cnt_o
		);
	Packet_is_too_long <= '1' when ((Row_Cnt_o(11 downto 0) = (1530*2)) and (Rx_Clock_Edge = '1')) --including preamble and CRC, non-jumbo
						  else '0'; 

	Row_output <= Row_Cnt_o;

--	Rx_Data_Valid_Back_Edge.(clk, d)		=	(System_Clock, !Rx_Data_Valid);
	Rx_Data_Valid_Back_Edge : entity work.Edge_Sensing
	Port map(
		clk	=> System_Clock,
		d		=> not Rx_Data_Valid, 
		q		=> Rx_Data_Valid_Back_Edge_o
		);
--	Gap_Cnt.(clock, cnt_en, clk_en, sclr)	=	(System_Clock, Interframe_Gap_in_Progress.q, Rx_Clock_Edge , Rx_Data_Valid);
	Gap_Cnt : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => 5
		)
	port map (
		clock 	=> System_Clock,
		cnt_en	=> Interframe_Gap_in_Progress_o,
		clk_en	=> Rx_Clock_Edge,
		sclr		=> Rx_Data_Valid,
		q			=> Gap_Cnt_o
		);
	Gap_output <= Gap_Cnt_o;

--	Interframe_Gap_in_Progress.(S, R, clk, ena) =	(	Rx_Data_Valid_Back_Edge.q,
--														(Gap_Cnt.q[] > 27) OR Rx_Data_Valid,
--														System_Clock, VCC
--													);
	Interframe_Gap_in_Progress_r <= '1' when (Gap_Cnt_o > 27) OR Rx_Data_Valid = '1'
										else '0';
	Interframe_Gap_in_Progress : entity work.SRFF 
	port map (
		S		=> Rx_Data_Valid_Back_Edge_o,
		CLK	=> System_Clock,
		R		=> Interframe_Gap_in_Progress_r,
		ena	=> '1',
		q		=> Interframe_Gap_in_Progress_o
		);
		Gap_is_too_small <= Interframe_Gap_in_Progress_o and Rx_Data_Valid;
	------------------------End of Simple timing check

	-- Is preamble good?
--	Preamble_progress_Cnt.(clock, cnt_en, sclr)	=	(System_Clock, 
--													Rx_Clock_Edge and Preamble_yet_good.q , 
--													(!Rx_Data_Valid) or (!Preamble_yet_good.q) or Frame_of_Data_is_in_Progress.q
--													);
	Preamble_progress_Cnt_cen <= Rx_Clock_Edge and Preamble_yet_good_o;
	Preamble_progress_Cnt_sclr <= (not Rx_Data_Valid) or (not Preamble_yet_good_o) or Frame_of_Data_is_in_Progress_o;
	Preamble_progress_Cnt : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => 5
		)
	port map (
		clock 	=> System_Clock,
		cnt_en	=> Preamble_progress_Cnt_cen,
		clk_en	=> '1',
		sclr		=> Preamble_progress_Cnt_sclr,
		q			=> Preamble_progress_Cnt_o
		);
--	if (Row_Cnt.q[11..0] <= 20)   -- CRS assertion is far away from Data Valid, And Rx_DV is not guaranteed for data immediately
--	then Preamble_check_enable = Rx_Clock_Edge and Rx_Data_Valid;
--	else Preamble_check_enable = gnd;
--	end if;
	Preamble_check_enable <= (Rx_Clock_Edge and Rx_Data_Valid) when Row_Cnt_o(11 downto 0) <= 20
								else '0';
	
--	Preamble_yet_good.(S, R, clk, ena) = (	Rx_Data_nibble_input[3..0] == b"0101", 
--											Rx_Data_nibble_input[3..0] != b"0101", 
--											System_Clock, Preamble_check_enable
--										);
	Preamble_yet_good_s <= '1' when Rx_Data_nibble_input(3 downto 0) = b"0101"
							else '0';
	Preamble_yet_good : entity work.SRFF 
	port map (
		S		=> Preamble_yet_good_s,
		CLK	=> System_Clock,
		R		=> not Preamble_yet_good_s,
		ena	=> Preamble_check_enable,
		q		=> Preamble_yet_good_o
		);

--	IF  ( 	(Row_Cnt.q[11..0] <= 22) 				--	not so far from begin
--		and	(Preamble_progress_Cnt.q[4..0] >= 9)	--	not too short good simbols of preamble (15 is max possible)
--		and	(Rx_Data_nibble_input[3..0] == b"1101")	--	SFD simbol valid
--		and (Rx_Clock_Edge)
--		)
--	then	End_of_good_Preamble	=	VCC;
--	else	End_of_good_Preamble	=	gnd;
--	end IF;
	End_of_good_Preamble <= '1' when ((Row_Cnt_o <= 22) and (Preamble_progress_Cnt_o >= 9) 
											and (Rx_Data_nibble_input(3 downto 0) = b"1101") and (Rx_Clock_Edge = '1'))
							 else '0';
	process(System_Clock)
	begin
		End_of_preamble <= End_of_good_Preamble;
	end process;
	Preamble_progress_Cnt_out <= Preamble_progress_Cnt_o;

--	IF  ( 	(Row_Cnt.q[11..0] == 23) 
--		and	(!Frame_of_Data_is_in_Progress.q)
--		and (Rx_Clock_Edge)
--		)
--	then	Preamble_failed			=	VCC;
--	else	Preamble_failed			=	gnd;
--	end IF;
	Preamble_failed <= '1' when ((Row_Cnt_o <= 23) and (not Frame_of_Data_is_in_Progress_o) = '1'
									and	Rx_Clock_Edge = '1')
					  else '0';

	--- Will we start frame data receive???

--	Frame_of_Data_is_in_Progress.(S, R, clk, ena) =	(	End_of_good_Preamble,
--														Rx_Data_Valid_Back_Edge.q,
--														System_Clock, VCC);
	Frame_of_Data_is_in_Progress : entity work.SRFF 
	port map (
		S		=> End_of_good_Preamble,
		CLK	=> System_Clock,
		R		=> Rx_Data_Valid_Back_Edge_o,
		q		=> Frame_of_Data_is_in_Progress_o
		);

	Data_Frame_is_in_Progress <= Frame_of_Data_is_in_Progress_o;

	------------------- End of preamble check-----------------------------

	-- Error processing to be here!!!!!!!!!!!!!!!!!!!!!!!!!!!!
	----------------------------------------------------------------------
--	Nibble_Cnt.(clock, cnt_en, sclr)	=	(System_Clock, Rx_Clock_Edge, 
--						((!Rx_Data_Valid) or(!Frame_of_Data_is_in_Progress.q) or End_of_good_Preamble));
	Nibble_Cnt_sclr <= ((not Rx_Data_Valid) or (not Frame_of_Data_is_in_Progress_o) or End_of_good_Preamble);
	Nibble_Cnt : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => 12
		)
	port map (
		clock 	=> System_Clock,
		cnt_en	=> Rx_Clock_Edge,
		clk_en	=> '1',
		sclr		=> Nibble_Cnt_sclr,
		q			=> Nibble_Cnt_o
		);

	Byte_Ready_Strobe_i <= (Rx_Clock_Edge) and Nibble_Cnt_o(0) and Frame_of_Data_is_in_Progress_o ;
	Byte_Ready_Strobe <= Byte_Ready_Strobe_i;
--	LSN_Register.(clock, enable, data[3..0], load)	=	(System_Clock, Rx_Clock_Edge, Rx_Data_nibble_input[3..0], VCC);
	LSN_Register: entity work.ShiftReg
		generic map (WIDTH => 4) 
		port map(
			clock	=> System_Clock,
			data	=> Rx_Data_nibble_input,
			enable=> Rx_Clock_Edge,
			q		=> LSN_Register_o
		);
--	Byte_output_Register.(clock, enable, load)		=	(System_Clock, 
--														Rx_Clock_Edge and Nibble_Cnt.q[0], VCC);
--	Byte_output_Register.(data[3..0], data[7..4])	=	(LSN_Register.q[3..0], Rx_Data_nibble_input[3..0]);
	Byte_output_Register_en <= Rx_Clock_Edge and Nibble_Cnt_o(0);
	Byte_output_Register: entity work.ShiftReg
		generic map (WIDTH => 8) 
		port map(
			clock					=> System_Clock,
			data(3 downto 0)	=> LSN_Register_o,
			data(7 downto 4)	=> Rx_Data_nibble_input,
			enable				=> Byte_output_Register_en,
			q						=> Byte_output_Register_o
		);

	Byte_Output <= Byte_output_Register_o;
	process (System_Clock)
	begin
		Byte_Output_Strobe <= Byte_Ready_Strobe_i;
	end process;

	----------------------------------------------------------
--	CRC32_new.clock			=	System_Clock;
--	CRC32_new.in[3..0]		=	Bypass_Shifter[31..28].q;	-- to match delay of CRC calc
--
--	CRC32_new.reset			=	(Nibble_Cnt.q[]==7) and Rx_Clock_Edge; --End_of_good_Preamble;
--	CRC32_new.write			=	Rx_Clock_Edge;
--	CRC32_new.enable		=	Rx_Clock_Edge;
--
--	CRC_check_out[31..0]	=	CRC32_new.out[31..0];
--	CRC_Good				=	CRC32_new.out[31..0]==Bypass_Shifter[].q;
	CRC32_new_reset <= '1' when (Nibble_Cnt_o = 7) and Rx_Clock_Edge = '1'
					  else '0';
	CRC32_new : entity work.crc32eth4
		Port map(
		clock			=> System_Clock, --MII_Tx_CLK;
		enable		=> Rx_Clock_Edge,--vcc; %progress.q or start_n;%
		Data_i		=> Bypass_Shifter(31 downto 28), -- to match delay of CRC calc
		reset			=> CRC32_new_reset,
		write_i		=> Rx_Clock_Edge,
		data_o		=> CRC32_new_o
		);
	CRC_Good_i <= '1' when (CRC32_new_o = Bypass_Shifter)
			 else '0';
	CRC_Good <= CRC_Good_i;
	
--	Bypass_Shifter[].(clk, ena)	=	(System_Clock, Rx_Clock_Edge);
	Bypass_Shifter_j: for j in 0 to 7 generate
		Bypass_Shifter_i: for i in 0 to 3 generate
			process(System_Clock)
			begin
				if Rx_Clock_Edge = '1' then
					if j = 0 then
						Bypass_Shifter(4*j+i) <= Rx_Data_nibble_input(3-i);
					else 
						Bypass_Shifter(4*j+i) <= Bypass_Shifter(4*(j-1)+i);
					end if;
				end if;
			end process;
		end generate;
	end generate;
	Bypass_Shifter_Output <= Bypass_Shifter;

	------------------------------------------------------------------------------------------
	-- Since time interval from end of data and Rx_DV deassertion is not stable -see datasheet of lxt972-
	-- we need allow to check CRC_Good with some margin in time!!!
--	CRC_Good_Recorder.(clock, enable, shiftin)	=	(System_Clock, Rx_Clock_Edge, CRC_Good);
	process (System_Clock)
	begin
		if (System_Clock'event and System_Clock = '1') then
			if Rx_Clock_Edge = '1' then
				CRC_Good_Recorder(0) <= CRC_Good_i ;
				CRC_Good_Recorder(1) <= CRC_Good_Recorder(0);
				CRC_Good_Recorder(2) <= CRC_Good_Recorder(1);
			end if;
		end if;
	end process;
	CRC_of_packet_Looks_Good <= '1' when (CRC_Good_Recorder(0) /= '0' and CRC_Good_Recorder(1) /= '0' and CRC_Good_Recorder(2) /= '0')
								  else '0'; -- CRC was good near Rx_DV deassertion Rx_Data_Valid_Back_Edge.q
--	Packet_Good_End = DFF(.clk=System_Clock, .d= (Rx_Data_Valid_Back_Edge.q and CRC_of_packet_Looks_Good and DFF(.clk=System_Clock, .d=(Nibble_Cnt.q[] > (45*2)))));
--	Packet_bad_End = DFF(.clk=System_Clock, .d= (Rx_Data_Valid_Back_Edge.q and (!CRC_of_packet_Looks_Good or !DFF(.clk=System_Clock, .d=(Nibble_Cnt.q[] > (45*2))))));
	process (System_Clock)
	begin
		if (Nibble_Cnt_o > (45*2)) 
			then Nibble_Cnt_o_good <= '1';
			else Nibble_Cnt_o_good <= '0';
		end if;
		Packet_Good_End <= Rx_Data_Valid_Back_Edge_o and CRC_of_packet_Looks_Good and Nibble_Cnt_o_good;
		Packet_bad_End  <= Rx_Data_Valid_Back_Edge_o and not CRC_of_packet_Looks_Good and not Nibble_Cnt_o_good;
	end process;
	
end Behavioral;
