----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 05/08/2022  
-- Design Name: 
-- Module Name: Eth_MAC_Decoder - Behavioral 
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

entity Eth_MAC_Decoder is
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

		Reset				: in std_logic; 

		Rx_Error_MAC				: out std_logic;  -- 
		Next_Parcer					: out std_logic;  -- 
		Rx_Parcer_in_progress	: out std_logic  -- 
		);
		
		Constant Eth_MAC_Pos					: integer := 0;--0; --6

		Constant PARCER_CYLCLE_WIDTH		: integer := 2;
		Constant PARCER_CYLCLE_CNT_WIDTH : integer := integer(Ceil(log2(real(PARCER_CYLCLE_WIDTH))))+1;

end Eth_MAC_Decoder;

architecture Behavioral of Eth_MAC_Decoder is

	signal Prescaler_o	: std_logic_vector (PARCER_CYLCLE_CNT_WIDTH downto 0);
	signal Rx_Data_tmp		: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Pascer_Sample_Enable	: std_logic;
	signal Rx_Parcer_RQ_ES_o		: std_logic;
	signal RxParcerActive_o			: std_logic;
	signal ParcerCnt_o				: std_logic_vector (3 downto 0);
	signal ParcerCycle_SRFF_o		: std_logic;
	signal ParcerEndCyle				: std_logic;
	signal Rx_MAC_Addr_True			: std_logic_vector (3 downto 0);
	signal Rx_MAC_Addr_True_Flag	: std_logic_vector (3 downto 0);
	signal Rx_MAC_Addr_Err_Flag_o	: std_logic;

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
--	IF (Prescaler.q[] ==  PARCER_CYLCLE_WIDTH-1) THEN  Pascer_Sample_Enable = VCC;
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
		WIDTH => 3
		)
	port map (
		clock 	=> Clock,
		cnt_en	=> RxParcerActive_o AND Pascer_Sample_Enable,
		clk_en	=> '1',
		sclr		=> not RxParcerActive_o,
		q			=> ParcerCnt_o
		);

--	IF(RxParcerActive.q == VCC) THEN
--		 Rx_Addr[2..0] = ParcerCnt.q[]; Rx_Addr[10..3] = GND;
--		 IF ((ParcerCnt.q[] == Eth_MAC_Pos+0) AND ((Rx_Data_tmp[] == MAC_Addr0_[]) OR (Rx_Data_tmp[] ==H"FFFF")) ) THEN Rx_MAC_Addr_True[0] = VCC; ELSE Rx_MAC_Addr_True[0] = GND; END IF;
--		 IF ((ParcerCnt.q[] == Eth_MAC_Pos+1) AND ((Rx_Data_tmp[] == MAC_Addr1_[]) OR (Rx_Data_tmp[] ==H"FFFF")) ) THEN Rx_MAC_Addr_True[1] = VCC; ELSE Rx_MAC_Addr_True[1] = GND; END IF; 
--		 IF ((ParcerCnt.q[] == Eth_MAC_Pos+2) AND ((Rx_Data_tmp[] == MAC_Addr2_[]) OR (Rx_Data_tmp[] ==H"FFFF")) ) THEN Rx_MAC_Addr_True[2] = VCC; ELSE Rx_MAC_Addr_True[2] = GND; END IF;
--		 
--		 --  ????????????? ? ????????? ????? MAC-????? ??????????? ???????
--		 IF (ParcerCnt.q[] == Eth_MAC_Pos)   THEN  Tx_Addr[10..0] = 3; Tx_Data[] = MAC_Addr0_[]; Tx_Strobe = VCC; END IF;
--		 IF (ParcerCnt.q[] == Eth_MAC_Pos+1) THEN  Tx_Addr[10..0] = 4; Tx_Data[] = MAC_Addr1_[]; Tx_Strobe = VCC; END IF; 
--		 IF (ParcerCnt.q[] == Eth_MAC_Pos+2) THEN  Tx_Addr[10..0] = 5; Tx_Data[] = MAC_Addr2_[]; Tx_Strobe = VCC; END IF;
--		 
--		 --  ????????????? ? ????????? ????? MAC-????? ?????????? ???????
--		 IF (ParcerCnt.q[] == Eth_MAC_Pos+3) THEN  Tx_Addr[10..0] = 0; Tx_Data[] = Rx_Data_tmp[];	   Tx_Strobe = VCC; END IF;
--		 IF (ParcerCnt.q[] == Eth_MAC_Pos+4) THEN  Tx_Addr[10..0] = 1; Tx_Data[] = Rx_Data_tmp[];    Tx_Strobe = VCC; END IF;
--		 IF (ParcerCnt.q[] == Eth_MAC_Pos+5) THEN  Tx_Addr[10..0] = 2; Tx_Data[] = Rx_Data_tmp[];    Tx_Strobe = VCC; END IF;
--		
--		IF(ParcerCnt.q[] == Eth_MAC_Pos+6)  THEN  ParcerEndCyle = VCC; ELSE ParcerEndCyle = GND; END IF;
--			 
--								ELSE  Tx_Addr[10..0] = GND; Tx_Data[] = GND; 
--												 Rx_MAC_Addr_True[0] = GND; Rx_MAC_Addr_True[1] = GND; Rx_MAC_Addr_True[2] = GND;
--												 Tx_Strobe = GND;
--	END IF;
--	Rx_MAC_Addr_True[3] = DFF(.d = Rx_MAC_Addr_True_Flag[0].q AND Rx_MAC_Addr_True_Flag[1].q AND Rx_MAC_Addr_True_Flag[2].q, .clk = Clock);

	process (RxParcerActive_o)
	begin
		IF(RxParcerActive_o = '1') THEN
			 Rx_Addr <= ParcerCnt_o; Rx_Addr(10 downto 3) <= '0';
			 IF ((ParcerCnt_o = Eth_MAC_Pos+0) AND ((Rx_Data_tmp = MAC_Addr0_i) OR (Rx_Data_tmp = X"FFFF")) ) THEN Rx_MAC_Addr_True(0) <= '1'; ELSE Rx_MAC_Addr_True(0) <= '0'; END IF;
			 IF ((ParcerCnt_o = Eth_MAC_Pos+1) AND ((Rx_Data_tmp = MAC_Addr1_i) OR (Rx_Data_tmp = X"FFFF")) ) THEN Rx_MAC_Addr_True(1) <= '1'; ELSE Rx_MAC_Addr_True(1) <= '0'; END IF; 
			 IF ((ParcerCnt_o = Eth_MAC_Pos+2) AND ((Rx_Data_tmp = MAC_Addr2_i) OR (Rx_Data_tmp = X"FFFF")) ) THEN Rx_MAC_Addr_True(2) <= '1'; ELSE Rx_MAC_Addr_True(2) <= '0'; END IF;
			 
			 --  ????????????? ? ????????? ????? MAC-????? ??????????? ???????
			 IF (ParcerCnt_o = Eth_MAC_Pos)   THEN  Tx_Addr <= 3; Tx_Data <= MAC_Addr0_i; Tx_Strobe <= '1'; END IF;
			 IF (ParcerCnt_o = Eth_MAC_Pos+1) THEN  Tx_Addr <= 4; Tx_Data <= MAC_Addr1_i; Tx_Strobe <= '1'; END IF; 
			 IF (ParcerCnt_o = Eth_MAC_Pos+2) THEN  Tx_Addr <= 5; Tx_Data <= MAC_Addr2_i; Tx_Strobe <= '1'; END IF;
			 
			 --  ????????????? ? ????????? ????? MAC-????? ?????????? ???????
			 IF (ParcerCnt_o = Eth_MAC_Pos+3) THEN  Tx_Addr <= 0; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			 IF (ParcerCnt_o = Eth_MAC_Pos+4) THEN  Tx_Addr <= 1; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			 IF (ParcerCnt_o = Eth_MAC_Pos+5) THEN  Tx_Addr <= 2; Tx_Data <= Rx_Data_tmp; Tx_Strobe <= '1'; END IF;
			
			 IF(ParcerCnt_o = Eth_MAC_Pos+6)	 THEN  ParcerEndCyle <= '1'; ELSE ParcerEndCyle <= '0'; END IF;
				 
		ELSE  Tx_Addr <= '0'; Tx_Data <= '0'; Tx_Strobe <= '0';
				Rx_MAC_Addr_True(0) <= '0'; Rx_MAC_Addr_True(1) <= '0'; Rx_MAC_Addr_True(2) <= '0';
		END IF;
		
	Rx_MAC_Addr_True(3) <= Rx_MAC_Addr_True_Flag_o(0) AND Rx_MAC_Addr_True_Flag_o(1) AND Rx_MAC_Addr_True_Flag_o(2);
	end process;

--	FOR i IN 0 TO 3 GENERATE 
--		Rx_MAC_Addr_True_Flag[i].(S,clk,R) = (Rx_MAC_Addr_True[i], Clock, Rx_Parcer_RQ_ES.q OR ParcerEndCyle OR Reset); 
--	END GENERATE;
	Rx_MAC_Addr_True_Flag_i : for i in 3 to 0 generate
		Rx_MAC_Addr_True_Flag : entity work.SRFF 
		port map (
			S		=> Rx_MAC_Addr_True(i),
			CLK	=> System_Clock,
			R		=> Rx_Parcer_RQ_ES_o OR ParcerEndCyle OR Reset,
			q		=> Rx_MAC_Addr_True_Flag_o(i)
			);
	end generate;

--	Rx_MAC_Addr_Err_Flag.(S,clk,R) = ((!Rx_MAC_Addr_True_Flag[3].q) AND ParcerEndCyle, Clock, Rx_Parcer_RQ_ES.q OR Reset); 
	Rx_MAC_Addr_Err_Flag: entity work.SRFF 
	port map (
		S		=> not Rx_MAC_Addr_True_Flag_o(3) AND ParcerEndCyle,
		CLK	=> Clock,
		R		=> Rx_Parcer_RQ_ES_o OR Reset,
		q		=> Rx_MAC_Addr_Err_Flag_o
		);

	Rx_Error_MAC 				<= Rx_MAC_Addr_Err_Flag_o;               -- ????????? MAC-????? ????????
	Next_Parcer 				<= Rx_MAC_Addr_True_Flag_o(3) AND ParcerEndCyle;  -- MAC-????? ???????, ????????? ? ?????????? ???????
	Tx_Word_Strobe 			<= (RxParcerActive_o AND Tx_Strobe AND Pascer_Sample_Enable); -- ????? ???????????? ? Tx ?????? ???-????? ??????????, ???-????? ????????
	Rx_Parcer_in_progress	<= RxParcerActive_o;                     -- ???? ??????? ????????????? MAC-???????

end Behavioral;