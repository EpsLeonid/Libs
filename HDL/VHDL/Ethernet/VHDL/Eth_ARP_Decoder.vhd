----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 05/08/2022  
-- Design Name: 
-- Module Name: Eth_ARP_Decoder - Behavioral 
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

entity Eth_ARP_Decoder is
	Generic (
	WORD_WIDTH			: integer := 16 
	);
	Port (
		Clock				: in std_logic; -- System Clock, really Bus_Clock
		Rx_Data			: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		Rx_Addr			: out std_logic_vector (10 downto 0);  -- 
		Rx_Parcer_RQ	: in std_logic; 
		
		Tx_Addr			: out std_logic_vector (10 downto 0);  -- 
		Tx_Data			: out std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		Tx_Word_Strobe	: out std_logic;  -- 

		MAC_Addr0_i		: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		MAC_Addr1_i		: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		MAC_Addr2_i		: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		IP_Addr0_i		: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		IP_Addr1_i		: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 

		Reset				: in std_logic; 

		Rx_IP_Error					: out std_logic;
		Rx_NOT_RQ					: out std_logic;
		Tx_Start						: out std_logic;
		Rx_TRUE_RQ					: out std_logic;
		Rx_Parcer_in_progress	: out std_logic; -- 
		
		Test							: out std_logic
		);
		
	Constant PARCER_CYLCLE_WIDTH	: integer := 2;--4
	Constant PARCER_CYLCLE_CNT_WIDTH : integer := integer(Ceil(log2(real(PARCER_CYLCLE_WIDTH))))+1;

	Constant Eth_type_Pos			: integer := 6;--6; --6

	Constant ARP_type					: std_logic_vector := X"0608";

	Constant ARP_Hardware_type		: std_logic_vector := X"0100";
	Constant ARP_Protocol_type		: std_logic_vector := X"0008";
	Constant ARP_HP_size				: std_logic_vector := X"0406"; -- ?????? MAC(6) ? IP(4) ??????? ? ??????
	Constant ARP_Opcode_Request	: std_logic_vector := X"0100"; -- ?????? ARP ??????
	Constant ARP_Opcode_Answer		: std_logic_vector := X"0200"; -- ?????? ARP ?????

end Eth_ARP_Decoder;

architecture Behavioral of Eth_ARP_Decoder is

	signal Prescaler_o				: std_logic_vector (PARCER_CYLCLE_CNT_WIDTH downto 0);
	signal Rx_Data_tmp				: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Pascer_Sample_Enable	: std_logic;
	signal Rx_Parcer_RQ_ES_o		: std_logic;
	signal RxParcerActive_o			: std_logic;
	signal ParcerCnt_o				: std_logic_vector (4 downto 0);
	signal ARP_Request				: std_logic;
	signal Tx_Start_Pulse_Flag_o	: std_logic;
	signal Tx_Strobe					: std_logic;
	signal ARP_type_True				: std_logic;
	signal ARP_type_Flag_o			: std_logic;
	signal ParcerEndCyle				: std_logic;
	signal IP0_True,IP1_True		: std_logic;
	signal IP0_Addr_Flag_o			: std_logic;
	signal IP1_Addr_Flag_o			: std_logic;

begin

--	Prescaler.(clock, sclr) = (Clock, Pascer_Sample_Enable OR Rx_Parcer_RQ_ES.q);
	Prescaler : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => PARCER_CYLCLE_CNT_WIDTH
		)
	port map (
		clock 	=> Clock,
		cnt_en	=> '1',
		clk_en	=> '1',
		sclr		=> Pascer_Sample_Enable OR Rx_Parcer_RQ_ES_o,
		q			=> Prescaler_o
		);
--	IF (Prescaler.q() =  PARCER_CYLCLE_WIDTH-1) THEN  Pascer_Sample_Enable = VCC;
--											 ELSE  Pascer_Sample_Enable = GND;
--	END IF;
	Pascer_Sample_Enable <= '1' when (Prescaler_o = PARCER_CYLCLE_WIDTH-1) 
							 else '0';
	
-------------------------------------------- Rx section --------------------------------------------
	Rx_Data_tmp <= Rx_Data;

--	Rx_Parcer_RQ_ES.(d,clk)  = (Rx_Parcer_RQ, Clock);
	Rx_Parcer_RQ_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> Rx_Parcer_RQ,
		q		=> Rx_Parcer_RQ_ES_o
		);
--	RxParcerActive.(S,clk,R) = (Rx_Parcer_RQ_ES.q, Clock, ParcerEndCyle OR Reset);
	RxParcerActive: entity work.SRFF 
		port map (
			S		=> Rx_Parcer_RQ_ES_o,
			CLK	=> Clock,
			R		=> ParcerEndCyle OR Reset,
			q		=> RxParcerActive_o
			);

--	ParcerCnt.(clock,sclr,cnt_en) = (Clock, !RxParcerActive.q, RxParcerActive.q AND Pascer_Sample_Enable);
	ParcerCnt : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => 5
		)
	port map (
		clock 	=> Clock,
		cnt_en	=> RxParcerActive_o AND Pascer_Sample_Enable,
		clk_en	=> '1',
		sclr		=> not RxParcerActive_o,
		q			=> ParcerCnt_o
		);

	process (RxParcerActive_o)
	begin
		IF(RxParcerActive_o = '1') THEN
			Tx_Addr(4 downto 0) <= ParcerCnt_o; Tx_Addr(10 downto 5) <= (others => '0');
			IF ((ParcerCnt_o >= Eth_type_Pos) AND (ParcerCnt_o <= Eth_type_Pos+7)) THEN Rx_Addr(4 downto 0) <= ParcerCnt_o(4 downto 0); Rx_Addr(9 downto 5) <= (others => '0'); END IF;
			IF ((ParcerCnt_o = Eth_type_Pos) AND (Rx_Data_tmp = ARP_type)) THEN  ARP_type_True <= '1'; END IF;
			IF ((ParcerCnt_o = Eth_type_Pos+1) AND (ARP_type_Flag_o = '0')) THEN ParcerEndCyle <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+4) THEN 
				IF(Rx_Data_tmp = ARP_Opcode_Request) THEN ARP_Request <= '1'; END IF;-- ?????? ARP ??????, ????????? ?????? ???????? ?????? ?? Tx ???????
			END IF;
			IF (ParcerCnt_o = Eth_type_Pos+8) 
				THEN IF(Rx_Data_tmp = IP_Addr0_i) THEN IP0_True <= '1'; END IF;-- ????????? LSB IP-?????, ????????? ?????? ???????? ?????? ?? Tx ???????
			END IF;
			IF (ParcerCnt_o = Eth_type_Pos+9) 
				THEN IF(Rx_Data_tmp = IP_Addr1_i) THEN IP1_True <= '1'; END IF;-- ????????? MSB IP-?????, ????????? ?????? ???????? ?????? ?? Tx ???????
			END IF;
			
			IF (ParcerCnt_o = Eth_type_Pos)    THEN  Tx_Data <= ARP_type;				Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+1)  THEN  Tx_Data <= ARP_Hardware_type;	Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+2)  THEN  Tx_Data <= ARP_Protocol_type;	Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+3)  THEN  Tx_Data <= ARP_HP_size;			Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+4)  THEN  Tx_Data <= ARP_Opcode_Answer;	Tx_Strobe <= '1'; END IF;
			
			IF (ParcerCnt_o = Eth_type_Pos+5)  THEN  Tx_Data <= MAC_Addr0_i; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+6)  THEN  Tx_Data <= MAC_Addr1_i; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+7)  THEN  Tx_Data <= MAC_Addr2_i; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+8)  THEN  Rx_Addr(10 downto 0) <= b"00000010011"; Tx_Data <= IP_Addr0_i; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+9)  THEN  Rx_Addr(10 downto 0) <= b"00000010100"; Tx_Data <= IP_Addr1_i; Tx_Strobe <= '1'; END IF;
			
			IF (ParcerCnt_o = Eth_type_Pos+10) THEN  Rx_Addr(10 downto 0) <= b"00000001011"; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+11) THEN  Rx_Addr(10 downto 0) <= b"00000001100"; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+12) THEN  Rx_Addr(10 downto 0) <= b"00000001101"; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+13) THEN  Rx_Addr(10 downto 0) <= b"00000001110"; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+14) THEN  Rx_Addr(10 downto 0) <= b"00000001111"; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			IF (ParcerCnt_o = Eth_type_Pos+15) THEN  ParcerEndCyle <= '1'; END IF;
			
		ELSE
			ARP_type_True		 <= '0';
			Tx_Strobe			 <= '0';
			ARP_Request			 <= '0';
			Rx_Addr				 <= (others => '0');
			Tx_Addr				 <= (others => '0');
			Tx_Data				 <= (others => '0'); 
			ParcerEndCyle		 <= '0';
			IP0_True				 <= '0';
			IP1_True				 <= '0';
		END IF;
	end process;
	
	-- ?????
--	ARP_type_Flag.(S,clk,R,Ena)    	  = (ARP_type_True, Clock, Rx_Parcer_RQ_ES.q OR Reset,VCC);
	ARP_type_Flag : entity work.SRFF 
	port map (
		S		=> ARP_type_True,
		CLK	=> Clock,
		R		=> Rx_Parcer_RQ_ES_o or Reset,
		q		=> ARP_type_Flag_o
		);
--	IP0_Addr_Flag.(S,clk,R,Ena)        = (IP0_True, Clock, Rx_Parcer_RQ_ES.q OR Reset,VCC);
	IP0_Addr_Flag : entity work.SRFF 
	port map (
		S		=> ARP_type_True,
		CLK	=> Clock,
		R		=> Rx_Parcer_RQ_ES_o or Reset,
		q		=> IP0_Addr_Flag_o
		);
--	IP1_Addr_Flag.(S,clk,R,Ena)        = (IP1_True, Clock, Rx_Parcer_RQ_ES.q OR Reset,VCC);
	IP1_Addr_Flag : entity work.SRFF 
	port map (
		S		=> ARP_type_True,
		CLK	=> Clock,
		R		=> Rx_Parcer_RQ_ES_o or Reset,
		q		=> IP1_Addr_Flag_o
		);
--	Tx_Start_Pulse_Flag.(S,clk,R,Ena)  = (ARP_Request, Clock, Rx_Parcer_RQ_ES.q OR Reset,VCC);
	Tx_Start_Pulse_Flag : entity work.SRFF 
	port map (
		S		=> ARP_type_True,
		CLK	=> Clock,
		R		=> Rx_Parcer_RQ_ES_o or Reset,
		q		=> Tx_Start_Pulse_Flag_o
		);

	-- ??????
	Rx_IP_Error	<= ARP_type_Flag_o AND not(IP0_Addr_Flag_o AND IP1_Addr_Flag_o) AND ParcerEndCyle;--ARP_Err_Flag.q;
	Rx_NOT_RQ	<= (not ARP_type_Flag_o) AND ParcerEndCyle;
	Tx_Start		<= Tx_Start_Pulse_Flag_o AND IP0_Addr_Flag_o AND IP1_Addr_Flag_o AND ParcerEndCyle;
	Rx_Parcer_in_progress	<= RxParcerActive_o;
	--Tx_Word_Strobe 		  = (RxParcerActive.q AND Tx_Strobe AND Pascer_Sample_Enable); -- ????? ???????????? ? Tx ??????
	Tx_Word_Strobe	<= (Tx_Strobe AND Pascer_Sample_Enable); -- ????? ???????????? ? Tx ??????
	Rx_TRUE_RQ		<= ARP_type_Flag_o AND ParcerEndCyle;

	Test				<= ARP_type_Flag_o;

end Behavioral;