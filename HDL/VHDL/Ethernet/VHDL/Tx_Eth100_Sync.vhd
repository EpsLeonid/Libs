----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 27/06/2022  
-- Design Name: 
-- Module Name: Tx_Eth100_Sync - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: v.1.0
-- Description: 
--
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

entity Tx_Eth100_Sync is
	Port (
		System_Clock 					: in std_logic;	-- FPGA Logic Clock.
		
		Reset 							: in std_logic;	-- Reset at System_Clock
		Transmit_of_Data_RQ			: in std_logic;	-- While it's active, we transmitting data.
		Data_to_Transmit				: in std_logic_vector (7 downto 0);	-- External incomming data Byte, buffered to MII_Eth_Tx_CLK
		Byte_Readed_Strob				: out std_logic;	--System_Clock referenced aknolege (1T clock)
		Eth_Tx_In_Progress			: out std_logic;	-- Transmittion is in progress(flag), System_Clock referenced
		TxClk_Edge_at_System_Clock	: out std_logic;	--Implicit timing of external packet-shaping logic

		-- Interface to Phy device IC
		MII_Tx_CLK						: in std_logic; 			-- Phy IC clock
		MII_Tx_En						: out std_logic;	-- Transmittion's in progress(flag).		(to i82555)
		MII_Tx_Data						: out std_logic_vector (3 downto 0);	-- Data Nibble.					(to i82555)
	-- No collision detect in mind
		
		--Old signals
		EtxRdy 							: out std_logic;	-- Ready to work out next byte(flag) - (not really data sampled!!!)
		crc_out							: out std_logic;	-- CRC32 Transmition (flag).
		
		-- Debug used
		Frame_Started_Strobe 		: out std_logic;
		TX_PHASE_VP						: out std_logic_vector (3 downto 0);
		Data_is_available				: out std_logic;
		Load_Data_to_Shift_Reg		: out std_logic := '0';
		CRC_check_out					: out std_logic_vector (31 downto 0)
		);
		constant PREAMBLE				: std_logic_vector := b"01010101";
		constant JK_BYTE				: std_logic_vector := b"11010101";

end Tx_Eth100_Sync;

architecture Behavioral of Tx_Eth100_Sync is

	signal MII_Tx_CLK_o			: std_logic;
	signal TxClk_Edg_at_System_Clock_o	: std_logic;
	signal TxClk_Edge_at_System_Clock_o	: std_logic;
	signal Data_is_available_o	: std_logic;
	signal Input_Data_Buffer_o	: std_logic_vector (7 downto 0);
	signal progress_r 			: std_logic;
	signal progress_o 			: std_logic;
	signal nibble_cnt_d			: std_logic;
	signal nibble_cnt_o			: std_logic;
	signal EtxInPro_o 			: std_logic;
	signal Start_Frame_Strob_o	: std_logic;
	signal Next_byte_Rq_o 		: std_logic;

	signal crc32_reset			: std_logic := '0';
	signal crc32_write			: std_logic := '0';
	signal crc32_o					: std_logic_vector (31 downto 0);

	signal crc_ena_f_o			: std_logic;
	signal crc_ena_f_s			: std_logic := '0';
	signal crc_ena_f_r			: std_logic := '0';
	
	signal tx_phase_en			: std_logic := '0';
	signal tx_phase_sclr			: std_logic;
	signal tx_phase_o				: std_logic_vector (3 downto 0);

	type sm is (s_idle, s_preamble, s_jk, s_data, s_crc, s_err); -- possible states
	signal ctrl : sm; -- state control signal
	signal sm_S_DATA				: std_logic;
	signal sm_S_IDLE				: std_logic;
	signal sm_S_CRC				: std_logic;
	signal sm_clk					: std_logic;
	signal sm_ena					: std_logic;
	
	signal Load_Data_to_Shift	: std_logic;
	
	signal shift_reg_data		: std_logic_vector (7 downto 0);
	signal shift_reg_o			: std_logic_vector (3 downto 0);

	signal end_n					: std_logic := '0';
	signal idle_n					: std_logic := '0';
	
	signal ExtDat_d				: std_logic_vector (3 downto 0);
	signal EtxDat_o				: std_logic_vector (3 downto 0);
	signal Fast_Out_Buff			: std_logic_vector (4 downto 0);

begin

	process(System_Clock)
	begin
		MII_Tx_CLK_o <= MII_Tx_CLK;
	end process;

--	TxClk_Edg_at_System_Clock.(clk,d)	=	(System_Clock, (DFF(.clk=System_Clock, .d=MII_Tx_CLK)));
--	TxClk_Edg_at_System_Clock : entity work.Edge_Sensing_Sync;
	TxClk_Edg_at_System_Clock : entity work.Edge_Sensing
	Port map(
		clk	=> System_Clock,
		d		=> MII_Tx_CLK_o, --DFF
		q		=> TxClk_Edg_at_System_Clock_o
		);

--	TxClk_Edge_at_System_Clock	=	(DFF(.clk=System_Clock, .d=TxClk_Edg_at_System_Clock.q));
	process(System_Clock)
	begin
		TxClk_Edge_at_System_Clock_o <= TxClk_Edg_at_System_Clock_o;
		TxClk_Edge_at_System_Clock <= TxClk_Edg_at_System_Clock_o;
	end process;

--	Data_is_available	=	DFFE(.clk=System_Clock, .ena=TxClk_Edge_at_System_Clock, .d=Transmit_of_Data_RQ); -- resync version of Start_Frame_RQ, EtxGo 
	Data_is_available_DFFE: entity work.DFFE
	Port map(
		clk	=> System_Clock,
		en		=> TxClk_Edge_at_System_Clock_o,
		d		=> Transmit_of_Data_RQ,
		q		=> Data_is_available_o
		);

	Data_is_available <= Data_is_available_o;

	--progress.(S, clk, R, ena)	=	(Transmit_of_Data_RQ, System_Clock, (end_n OR Reset), TxClk_Edge_at_System_Clock);
	progress_r <= (end_n OR Reset);
	progress : entity work.SRFF 
	port map (
		S		=> Transmit_of_Data_RQ,
		CLK	=> System_Clock,
		R		=> progress_r,
		ena	=> TxClk_Edge_at_System_Clock_o,
		q		=> progress_o
		);

--	EtxInPro.(d, clk,    ena)	=	(progress.q,     System_Clock,                   TxClk_Edge_at_System_Clock);
	EtxInPro: entity work.DFFE
	Port map(
		clk	=> System_Clock,
		en		=> TxClk_Edge_at_System_Clock_o,
		d		=> progress_o,
		q		=> EtxInPro_o
		);

--	Start_Frame_Strob.(d, clk,    ena)	=	(Transmit_of_Data_RQ,     System_Clock,       TxClk_Edge_at_System_Clock);
	Start_Frame_Strob: entity work.DFFE
	Port map(
		clk	=> System_Clock,
		en		=> TxClk_Edge_at_System_Clock_o,
		d		=> Transmit_of_Data_RQ,
		q		=> Start_Frame_Strob_o
		);

	Eth_Tx_In_Progress	<= EtxInPro_o;
	Frame_Started_Strobe	<= Start_Frame_Strob_o;

--	Input_Data_Buffer.(clock, data[], load, enable)=(System_Clock, Data_to_Transmit[], VCC, Next_byte_Rq.q);
	Input_Data_Buffer: entity work.ShiftReg
		generic map (WIDTH => 8) 
		port map(
			clock	=> System_Clock,
			data	=> Data_to_Transmit,
			enable=> Next_byte_Rq_o,
			q		=> Input_Data_Buffer_o
		);

--# Nibble counter (VCC - last nibble of the byte). ---------------------------
--	nibble_cnt.(d, clk, ena)	=	((!nibble_cnt.q AND !Start_Frame_Strob.q) or idle_n, System_Clock, TxClk_Edge_at_System_Clock);
	nibble_cnt_d <= ((not(nibble_cnt_o) and not(Start_Frame_Strob_o)) or idle_n);
	nibble_cnt: entity work.DFFE
	Port map(
		clk	=> System_Clock,
		en		=> TxClk_Edge_at_System_Clock_o,
		d		=> nibble_cnt_d,
		q		=> nibble_cnt_o
		);

--	Next_byte_Rq.(clk, d)		=	(System_Clock, Load_Data_to_Shift_Reg);--Edge_Sensing_Sync
--	Next_byte_Rq : entity work.Edge_Sensing_Sync;
	Next_byte_Rq : entity work.Edge_Sensing
	Port map(
		clk	=> System_Clock,
		d		=> Load_Data_to_Shift, --DFFE
		q		=> Next_byte_Rq_o
		);

	Byte_Readed_Strob	<=	Next_byte_Rq_o;	-- now referenced to real data load, not dummy as EtxRdy is
	
	EtxRdy <= not(nibble_cnt_o) and sm_S_DATA; -- now ignored

--# Shift register. -----------------------------------------------------------	
-- No Reset needed.
--	shift_reg.(clock, enable, load)	=	(System_Clock, TxClk_Edge_at_System_Clock, nibble_cnt.q);
	shift_reg: entity work.uni_shift_gt
		generic map (N_IN => 8, N_OUT => 4)
		Port map(
			clock		=> System_Clock,
			data_i	=> shift_reg_data,
			load		=> nibble_cnt_o,
			enable	=> TxClk_Edge_at_System_Clock_o,
			data_o	=> shift_reg_o
		);
--	process (clock)
--	begin
--		if (clock'event and clock = '1') then
--			if TxClk_Edge_at_System_Clock_o = '1' then
--				if nibble_cnt_o = '1' then
--					shift_reg(7 downto 0) <= shift_reg_data;
--					shift_reg_o(3 downto 0) <= shift_reg(7 downto 0);
--			end if;
--		end if;
--	end process;
	
--# Crc counter. --------------------------------------------------------------
	crc32ether : entity work.crc32eth4
		Port map(
			clock				=> System_Clock, --MII_Tx_CLK;
			enable			=> TxClk_Edge_at_System_Clock_o,--vcc; %progress.q or start_n;%
			Data_i(3)		=> shift_reg_o(0),
			Data_i(2)		=> shift_reg_o(1),
			Data_i(1)		=> shift_reg_o(2),
			Data_i(0)		=> shift_reg_o(3),
			reset				=> crc32_reset,
			write_i			=> crc32_write,
			data_o			=> crc32_o
		);
	CRC_check_out(31 downto 0) <= crc32_o;

	crc_out <= sm_S_CRC;

	-- Reset & write are in the state machine.

--# The main state-machine logic. ---------------------------------------------
--	tx_phase.clock 	= System_Clock;--MII_Tx_CLK;
--	tx_phase.sclr = Reset # end_n;
--	tx_phase		: lpm_counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
--						WITH (LPM_WIDTH=4);

	tx_phase_sclr <= Reset or end_n;
	tx_phase : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => 4
		)
	port map (
		clock 	=> System_Clock,
		cnt_en	=> tx_phase_en,
		clk_en	=> '1',
		sclr		=> tx_phase_sclr,
		q			=> tx_phase_o
		);

	-- 'enable' is in the state machine.
	TX_PHASE_VP <= tx_phase_o;

--	idle_n	= !(sm.q0 # sm.q1 # sm.q2 # sm.q3 # sm.q4);
	idle_n <= sm_S_IDLE; -- To do!
	sm_clk <= System_Clock;
	sm_ena <= nibble_cnt_o and TxClk_Edge_at_System_Clock_o;
	
	StMach: process(System_Clock)
	begin
		CASE ctrl IS
			WHEN s_idle =>
				IF (Data_is_available_o = '1') THEN
					shift_reg_data <= PREAMBLE;
					ctrl <= s_preamble;
					tx_phase_en <= TxClk_Edge_at_System_Clock_o;--vcc;
				END IF;

			WHEN s_preamble =>
				tx_phase_en <= nibble_cnt_o and TxClk_Edge_at_System_Clock_o;--;
				IF(tx_phase_o <= 8) THEN
					shift_reg_data <= JK_BYTE;
					ctrl <= s_jk;
				ELSIF(tx_phase_o < 8) THEN
					shift_reg_data <= PREAMBLE;
				ELSE
					ctrl <= s_err;
				END IF;

			WHEN s_jk	=> 
				tx_phase_en					<= nibble_cnt_o and TxClk_Edge_at_System_Clock_o;--;
				ctrl							<= s_data;
				crc32_reset					<= nibble_cnt_o ;
				shift_reg_data				<= Input_Data_Buffer_o;--EtxDi();
				Load_Data_to_Shift		<= nibble_cnt_o;-- # start_n;

			WHEN s_data	=> 
				if(not (Data_is_available_o = '1')) THEN
					ctrl			<= s_crc;
					crc_ena_f_s <= nibble_cnt_o;
					crc32_write <= '1';
					tx_phase_en <= nibble_cnt_o and TxClk_Edge_at_System_Clock_o;--;
				ELSE
					crc32_write					<= '1';
					shift_reg_data				<= Input_Data_Buffer_o;--EtxDi();
					Load_Data_to_Shift		<=	nibble_cnt_o;-- # start_n;
				END IF;

			WHEN s_crc	=> 				
				IF(tx_phase_o = 14)THEN
					crc_ena_f_r	<= nibble_cnt_o;
					ctrl			<= s_idle;
					end_n			<= nibble_cnt_o and TxClk_Edge_at_System_Clock_o;--;
				ELSIF(tx_phase_o <14 and tx_phase_o > 9) THEN
					tx_phase_en <= nibble_cnt_o and TxClk_Edge_at_System_Clock_o;--;
				ELSE
					ctrl <= s_err;
				END IF;

			WHEN OTHERS	=>
				IF(not (Data_is_available_o = '1')) THEN
					ctrl <= s_idle;
				END IF;
		END CASE;
	end process;
	
	Load_Data_to_Shift_Reg <= Load_Data_to_Shift;

--	crc_ena_f.(clk, ena) 	=	(System_Clock, TxClk_Edge_at_System_Clock); --SRFFE
	crc_ena_f : entity work.SRFF 
		port map (
			S		=> crc_ena_f_s,
			CLK	=> System_Clock,
			ENA	=> TxClk_Edge_at_System_Clock_o,
			R		=> crc_ena_f_r,
			q		=> crc_ena_f_o
		);

	EtxDat_i : for i in 3 to 0 GENERATE
		ExtDat_d(i) <= ((crc32_o(31-i) and crc_ena_f_o ) OR (shift_reg_o(i) and not(crc_ena_f_o) ));
		EtxDat:entity work.DFFE
		Port map(
			clk	=> System_Clock,
			en		=> TxClk_Edge_at_System_Clock_o,
			d		=> ExtDat_d(i),
			q		=> EtxDat_o(i)
			);
	END GENERATE;
	
	-- Fast_buff length should be adjusted to ensure 12ns setup time and 0ns hold time to MII_Tx_CLK
	-- of Phy device, depending of TX_CLK phase sampling method
	process(System_Clock) --	Fast_Out, 3..0 - data, 4 - Tx_En
	begin
		Fast_Out_Buff(3 downto 0) <= EtxDat_o;
		Fast_Out_Buff(4)			  <= EtxInPro_o;
	end process;
-----------------------------------------------------
	MII_Tx_Data(3 downto 0)	<=	Fast_Out_Buff(3 downto 0);
	MII_Tx_En					<=	Fast_Out_Buff(4); 

end Behavioral;