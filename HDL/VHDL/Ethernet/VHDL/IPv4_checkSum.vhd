----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 11/08/2022  
-- Design Name: 
-- Module Name: IPv4_checkSum - Behavioral 
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

entity IPv4_checkSum is
	Port (
		Clock				: in std_logic; -- System Clock, really Bus_Clock
		
		Rx_Data			: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		Rx_Addr			: out std_logic_vector (10 downto 0);  -- 
		Rx_Parcer_RQ	: in std_logic; 
		
		Tx_Addr			: out std_logic_vector (10 downto 0);  -- 
		Tx_Data			: out std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		Tx_Word_Strobe	: out std_logic;  -- 

		IPv4_CheckSum_Complete		: out std_logic;

		IP_ID				: in std_logic_vector (WORD_WIDTH-1 downto 0);


		-----------------
		Sample_Enable	: out std_logic;
		Sum20_Reg_out	: out std_logic_vector (19 downto 0);
		Sum16_Reg_out	: out std_logic_vector (15 downto 0);
		RxParcerActive_out	: out std_logic;

		Reset				: in std_logic; 

		test				: out std_logic  -- 
		);
		
	Constant PARCER_CYLCLE_WIDTH		: integer :=  2;--20;
	Constant PARCER_CYLCLE_CNT_WIDTH	: integer :=  Ceil(log2(PARCER_CYLCLE_WIDTH))+1;
	Constant WORD_WIDTH					: integer :=  16; 

	CONSTANT PacketLenghts_at_signaling_layer		: integer :=  4096;--2048;-- maximum length value in bytes
	CONSTANT RxByte_Cnt_Width							: integer :=  Ceil( LOG2(PacketLenghts_at_signaling_layer))-1;

	CONSTANT IP_Header					: integer := 7;  -- 20 ???? (10 ?????) ??? IP ?????????
	CONSTANT IP_HEADER_SIZE				: integer := 10;
	CONSTANT IP_CHECKSUM_ADDR			: integer := 12;

end IPv4_checkSum;

architecture Behavioral of IPv4_checkSum is

	signal Prescaler_o				: std_logic_vector (PARCER_CYLCLE_CNT_WIDTH downto 0);
	signal Pascer_Sample_Enable	: std_logic;
	
	signal Rx_Data_					: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Rx_Parcer_RQ_ES_o		: std_logic;
	signal RxParcerActive_o			: std_logic;
	signal ParcerEndCycle			: std_logic;
	signal ParcerCnt_o				: std_logic_vector (RxByte_Cnt_Width-1 downto 0);
	
	signal Sum20_Reg					: std_logic_vector (19 downto 0);
	signal Sum20_Reg_en				: std_logic;
	signal Sum20_Reg_o				: std_logic;
	signal Sum16_Reg					: std_logic_vector (15 downto 0);
	signal Sum16_Reg_o				: std_logic;

	signal Adder2x20_data_a			: std_logic_vector (19 downto 0); -- ????????? ???? ????????? IP
	signal Adder2x20_data_b			: std_logic_vector (19 downto 0);
	signal Adder2x20_tmp				: std_logic_vector (20 downto 0);
	signal Adder2x20_o				: std_logic_vector (19 downto 0);
	signal Adder2x20_o_cout		: std_logic;
	signal Adder2x16_data_a			: std_logic_vector (15 downto 0); -- ????????? ???? ????????
	signal Adder2x16_data_b			: std_logic_vector (15 downto 0);
	signal Adder2x16_data_tmp		: std_logic_vector (16 downto 0);
	signal Adder2x16_data_o			: std_logic_vector (15 downto 0);
	signal Adder2x16_data_cout		: std_logic;
	signal FinAdder2x16_data_a		: std_logic_vector (15 downto 0); -- ????????? ???? ???????? ???????? ? 16??????? ????? CheckSum
	signal FinAdder2x16_data_b		: std_logic_vector (15 downto 0); 
	signal FinAdder2x16_data_tmp	: std_logic_vector (16 downto 0); 
	signal FinAdder2x16_data_o		: std_logic_vector (15 downto 0); 
	signal FinAdder2x16_data_cout	: std_logic;

	signal HeaderSum_Complete 				: std_logic; 
	signal HeaderSum_with_carry_count	: std_logic;
	signal IP_CheckSum_Complete			: std_logic;
	signal Tx_Word_Strobe_					: std_logic;

begin

--	Prescaler.(clock, sclr) = (Clock, Pascer_Sample_Enable OR Rx_Parcer_RQ_ES_o);
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
--	IF (Prescaler_o() =  PARCER_CYLCLE_WIDTH-1) THEN  Pascer_Sample_Enable = '1';
--											 ELSE  Pascer_Sample_Enable = '0';
--	END IF;
	Pascer_Sample_Enable <= '1' when (Prescaler_o = PARCER_CYLCLE_WIDTH-1) 
							 else '0';
	
-------------------------------------------- Rx section --------------------------------------------
--	Rx_Parcer_RQ_ES.(d,clk)  = (Rx_Parcer_RQ, Clock);
	Rx_Parcer_RQ_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> Rx_Parcer_RQ,
		q		=> Rx_Parcer_RQ_ES_o
		);
--	RxParcerActive.(S,clk,R) = (Rx_Parcer_RQ_ES_o, Clock, ParcerEndCyle OR Reset);
	RxParcerActive: entity work.SRFF 
		port map (
			S		=> Rx_Parcer_RQ_ES_o,
			CLK	=> Clock,
			R		=> ParcerEndCyle OR Reset,
			q		=> RxParcerActive_o
			);

--	ParcerCnt.(clock,sclr,cnt_en) = (Clock, !RxParcerActive_o, RxParcerActive_o AND Pascer_Sample_Enable);
	ParcerCnt : entity work.V_Counter 	-- Transmition phase (byte counter)(For Preamble & SFD & CRC).
	generic map(
		WIDTH => RxByte_Cnt_Width
		)
	port map (
		clock 	=> Clock,
		cnt_en	=> RxParcerActive_o AND Pascer_Sample_Enable,
		sload		=> Rx_Parcer_RQ_ES_o,
		data		=> IP_HEADER_OFFSET,
		q			=> ParcerCnt_o
		);
		
	process (Clock)
	begin
		if ((RxParcerActive_o AND Pascer_Sample_Enable AND ((ParcerCnt_o >= IP_HEADER_OFFSET) AND (ParcerCnt_o < IP_HEADER_SIZE+IP_HEADER_OFFSET))) = '1') then
			Adder2x20_data_a(19 downto 16) <= (others =>'0');
			Adder2x20_data_a(15 downto 0)  <= Rx_Data_;
			Adder2x20_data_b <=Sum20_Reg_o;
			Adder2x20_tmp <= Adder2x20_data_a + Adder2x20_data_b;
			Adder2x20_o		<= Adder2x20_tmp(19 downto 0);
			Adder2x20_cout <= Adder2x20_tmp(20);
			Sum20_Reg_en	<= '1';
		end if;
		if (HeaderSum_Complete AND Pascer_Sample_Enable) = '1' then
			Adder2x16_data_a(15 downto 4) <= (others => '0');
			Adder2x16_data_a(3 downto 0)	<= Sum20_Reg_o(19 downto 16);
			Adder2x16_data_b					<= Sum20_Reg_o(15 downto 0);
			Adder2x16_tmp	<= Adder2x16_data_a + Adder2x16_data_b;
			Adder2x16_o		<= Adder2x16_tmp(15 downto 0);
			Adder2x16_cout <= Adder2x16_tmp(16);
		end if;
		if (HeaderSum_with_carry_count AND Pascer_Sample_Enable) = '1' then
			FinAdder2x16_data_a(15 downto 1)	<= (others => '0');
			FinAdder2x16_data_a(0)				<= Adder2x16_data_cout;
			FinAdder2x16_data_b					<= Adder2x16_data_o;
			FinAdder2x16_tmp	<= FinAdder2x16_data_a + FinAdder2x16_data_b;
			FinAdder2x16_o		<= FinAdder2x16_tmp(15 downto 0);
			FinAdder2x16_cout <= FinAdder2x16_tmp(16);
		end if;
	end process;
--	Sum20_Reg.(data[],clock,enable,load,sclr) = (Adder2x20_data.result[],Clock,DFF(.d=RxParcerActive.q AND Pascer_Sample_Enable AND ((ParcerCnt.q[]>=IP_HEADER_OFFSET) AND (ParcerCnt.q[]<IP_HEADER_SIZE+IP_HEADER_OFFSET)),.clk=Clock) OR Rx_Parcer_RQ_ES.q,VCC,Rx_Parcer_RQ_ES.q);
	Sum20_Reg: entity work.ShiftReg
		generic map (WIDTH => 20) 
		port map(
			clock	=> Clock,
			data	=> Adder2x20_data_o,
			enable=> Sum20_Reg_en OR Rx_Parcer_RQ_ES_o,
			sclr	=> Rx_Parcer_RQ_ES,
			q		=> Sum20_Reg_o
			);
--	Sum16_Reg.(data[],clock,enable,load,sclr) = (FinAdder2x16_data.result[],Clock,(IP_CheckSum_Complete AND Pascer_Sample_Enable) OR Rx_Parcer_RQ_ES.q,VCC,Rx_Parcer_RQ_ES.q);
	Sum16_Reg: entity work.ShiftReg
		generic map (WIDTH => 16) 
		port map(
			clock	=> Clock,
			data	=> FinAdder2x16_data_o,
			enable=> (IP_CheckSum_Complete AND Pascer_Sample_Enable) OR Rx_Parcer_RQ_ES_o,
			sclr	=> Rx_Parcer_RQ_ES,
			q		=> Sum16_Reg_o
			);

	process (RxParcerActive_o)
	begin
		IF(RxParcerActive_o = '1') THEN
			Rx_Addr(10 downto 0) <= ParcerCnt_o; 
			IF (ParcerCnt_o = IP_HEADER_OFFSET+2)		 THEN Rx_Data_ <= IP_ID; -- ??????????? ????? ID_IP ? ????????? IP
			ELSE IF (ParcerCnt_o = IP_HEADER_OFFSET+5) THEN Rx_Data_ <= (others => '0'); -- ??????? ????????? CheckSum ?? ??????????
																	 ELSE Rx_Data_ <= Rx_Data;
			END IF;
		END IF;

		IF (ParcerCnt_o = IP_HEADER_SIZE+IP_HEADER_OFFSET)	  THEN HeaderSum_Complete				<= '1'; END IF;
		IF (ParcerCnt_o = IP_HEADER_SIZE+IP_HEADER_OFFSET+1) THEN HeaderSum_with_carry_count	<= '1'; END IF;
		IF (ParcerCnt_o = IP_HEADER_SIZE+IP_HEADER_OFFSET+2) THEN IP_CheckSum_Complete			<= '1'; END IF;
		IF (ParcerCnt_o = IP_HEADER_SIZE+IP_HEADER_OFFSET+3) THEN Tx_Word_Strobe_					<= '1'; Tx_Addr <= IP_CHECKSUM_ADDR; Tx_Data <= not Sum16_Reg_o; END IF;
		IF (ParcerCnt_o = IP_HEADER_SIZE+IP_HEADER_OFFSET+4) THEN ParcerEndCycle 					<= Pascer_Sample_Enable; END IF;

		ELSE
		HeaderSum_Complete 			<= '0'; 
		HeaderSum_with_carry_count	<= '0';
		IP_CheckSum_Complete			<= '0';
		Rx_Addr							<= (others => '0');
		Tx_Addr							<= (others => '0');
		Tx_Data							<= (others => '0');
		ParcerEndCycle					<= '0';
		end if;
	end process;
	
	Tx_Word_Strobe <= Tx_Word_Strobe_ AND Pascer_Sample_Enable;--Edge_Sensing_Sync(.d=Tx_Word_Strobe_,.clk=Clock);
	IPv4_CheckSum_Complete <= ParcerEndCycle;--DFF(.d=ParcerEndCycle,.clk=Clock);

	----------------------
	Sample_Enable <= RxParcerActive.q AND Pascer_Sample_Enable AND ((ParcerCnt_o >= IP_HEADER_OFFSET) AND (ParcerCnt_o < IP_HEADER_SIZE+IP_HEADER_OFFSET));--Pascer_Sample_Enable;
	Sum20_Reg_out <= Sum20_Reg_o;
	Sum16_Reg_out <= Sum16_Reg_o;
	RxParcerActive_out <= RxParcerActive_o;

end Behavioral;