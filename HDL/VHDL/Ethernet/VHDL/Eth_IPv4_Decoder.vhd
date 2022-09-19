----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 09/08/2022  
-- Design Name: 
-- Module Name: Eth_IPv4_Decoder - Behavioral 
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

entity Eth_IPv4_Decoder is
	Port (
		Clock				: in std_logic; -- System Clock, really Bus_Clock
		
		Rx_Data			: in std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		Rx_Addr			: out std_logic_vector (10 downto 0);  -- 
		Rx_Parcer_RQ	: in std_logic; 
		Rx_NUM_Data		: in std_logic_vector (10 downto 0);  -- 
		
		Tx_Addr			: out std_logic_vector (10 downto 0);  -- 
		Tx_Data			: out std_logic_vector (WORD_WIDTH-1 downto 0);  -- 
		Tx_Word_Strobe	: out std_logic;  -- 

		Reset				: in std_logic; 

		Rx_Error_IP		: out std_logic;  -- 
		Tx_Start			: out std_logic;  -- 
		Rx_Parcer_in_progress	: out std_logic;  -- 
		Rx_TRUE_RQ		: out std_logic;  -- 
		Rx_NOT_RQ		: out std_logic;  -- 

		IP_Addr0_		: in std_logic_vector (WORD_WIDTH-1 downto 0);
		IP_Addr1_		: in std_logic_vector (WORD_WIDTH-1 downto 0);
		Port_i			: in std_logic_vector (WORD_WIDTH-1 downto 0);

		Identification	: out std_logic_vector (WORD_WIDTH-1 downto 0);

		--??????? ??? ?????? ? ????????? ?????????? ????? ???????  
		AccessRequest	: out std_logic;  -- 
		AccessGranted	: in std_logic; 
		DirectOut		: out std_logic;  -- 
		AddrBusOut		: out std_logic_vector (15 downto 0);
		DataBus_In		: in std_logic_vector (15 downto 0);  -- ???????????? ? ?????????? ???? ? ?????? ??????
		DataBusStrobe	: in std_logic;  -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)

		test				: out std_logic  -- 
		);
		
	Constant PARCER_CYLCLE_WIDTH		: integer :=  2;--20;
	Constant PARCER_CYLCLE_CNT_WIDTH : integer := integer(Ceil(log2(real(PARCER_CYLCLE_WIDTH))))+1;
	Constant WORD_WIDTH					: integer :=  16; 

	CONSTANT PacketLenghts_at_signaling_layer		: integer :=  4096;--2048;-- maximum length value in bytes
	CONSTANT RxByte_Cnt_Width							: integer :=  integer(Ceil(log2(real(PacketLenghts_at_signaling_layer))))-1;

	Constant IPv4_type					: integer := X"0008";

	CONSTANT HeaderFrame_Type			: integer := 6;  -- 2 ???? ??? ??????? (ARP ??? IPv4 ???  downto .)
	--CONSTANT IP_Header					: integer := 7;  -- 20 ???? (10 ?????) ??? IP ?????????
	--CONSTANT UDP_Header				: integer := 17; -- 8 ???? (4 ?????) ??? UDP ?????????
	CONSTANT RxCommandPath				: integer := 21; -- ????? ????? ??????????? ????????
	CONSTANT RxDataPath					: integer := 22; -- ????? ????? ??????????? ??? ????? ??? ?????? ? ??????????? ?? ???????
	--CONSTANT RxDataWordLenght		: integer := 500;--(PacketLenghts_at_signaling_layer div 2)-RxCommandPath-1;
	CONSTANT RxByteLenght				: integer := (PacketLenghts_at_signaling_layer/2)-1;

	-- ???????????? ???????
	CONSTANT CMD_PING						: integer := 1; -- ??????? ???
	CONSTANT CMD_READ_AD					: integer := 2; -- ??????? ?????? ?????,??????,?????,?????? downto .
	CONSTANT CMD_WRITE_AD				: integer := 3; -- ??????? ?????? ?????,??????,?????,?????? downto .
	CONSTANT CMD_READ_AA					: integer := 4; -- ??????? ?????? ?????,?????,?????,????? downto . ? ?????? ??????,??????,????? downto .
	CONSTANT CMD_READ_BLOCK_16			: integer := 5; -- ??????? ?????? ?????, 16 ???? ?????? ? ??????????????? ??????, ?????, 16 ???? ?????? downto .  
	CONSTANT CMD_WRITE_BLOCK_16		: integer := 6; -- ??????? ?????? ?????, 16 ???? ?????? ? ??????????????? ??????, ?????, 16 ???? ?????? downto . 

end Eth_IPv4_Decoder;

architecture Behavioral of Eth_IPv4_Decoder is

	signal Prescaler_o				: std_logic_vector (PARCER_CYLCLE_CNT_WIDTH downto 0);
	signal Pascer_Sample_Enable	: std_logic;
	
	signal Rx_Data_					: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Rx_Parcer_RQ_ES_o		: std_logic;
	signal RxParcerActive_o			: std_logic;
	signal ParcerCnt_o				: std_logic_vector (RxByte_Cnt_Width-1 downto 0);
	
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
	Rx_Data_ <= Rx_Data;

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
		cnt_en	=> RxParcerActive_o AND ParcerCnt_Inc_,
		sload		=> Rx_Parcer_RQ_ES_o OR ParcerEndCycle,
		data		=> HeaderFrame_Type,
		q			=> ParcerCnt_o
		);

	process (RxParcerActive_o)
	begin
		IF(RxParcerActive_o = '1') THEN
			Tx_Addr(10 downto 0) <= ParcerCnt_o; 
			IF ((ParcerCnt_o = HeaderFrame_Type)   AND (Rx_Data_ = IPv4_type))	THEN IPv4_type_True <= '1'; END IF;
			IF ((ParcerCnt_o = HeaderFrame_Type+1) AND (IPv4_type_Flag_o = '0')) THEN ParcerEndCycle <= '1'; END IF;  -- ???? ??? ?? IPv4 ????????, ?? ????? ?????? ????? ???????? ??????
			IF (ParcerCnt_o  = Rx_NUM_Data(10 downto 0)) THEN ParcerEndCycle <= '1'; END IF;  -- ????? ?????? ????? ???????? ??????
			
			-- ????????? ??????
			IF (ParcerCnt_o = 6)  THEN Tx_Data <= Rx_Data_; END IF; -- ??? ?????? IPv4
			-- IP header 
			IF (ParcerCnt_o = 7)  THEN Tx_Data <= Rx_Data_; END IF; -- Ver,IHL + ToS
			IF (ParcerCnt_o = 8)  THEN Tx_Data <= Rx_Data_; END IF; -- ?????? ?????? ? ?????? IP ?????????
			IF (ParcerCnt_o = 9)  THEN Tx_Data <= Identification_Cnt_o; END IF; -- ????? ??????
			IF (ParcerCnt_o = 10) THEN Tx_Data <= Rx_Data_; END IF; -- ???? ???????????? ?????? (00-????? ??????, XX-????? ????? ??????)
			IF (ParcerCnt_o = 11) THEN Tx_Data <= Rx_Data_; END IF; -- TTL + Protocol
			IF (ParcerCnt_o = 12) THEN Tx_Data <= Rx_Data_; END IF; -- Header Checksum IP

			CASE ParcerCnt_o IS
				WHEN 9			=> Rx_Addr(10 downto 0) <= 15; -- ????????????? ???? ??? ???????? ?????????? IP_address
				WHEN 17			=> Rx_Addr(10 downto 0) <= 16; -- ????????????? ???? ??? ???????? ?????????? IP_address
				WHEN 15			=> Rx_Addr(10 downto 0) <= 13; Tx_Data <= Rx_Data_;
				WHEN 16			=> Rx_Addr(10 downto 0) <= 14; Tx_Data <= Rx_Data_;
				WHEN 18			=> Rx_Addr(10 downto 0) <= 17; Tx_Data <= Rx_Data_;
				WHEN OTHERS		=> Rx_Addr(10 downto 0) <= ParcerCnt_o;
			END CASE;
			-- ?????????? ???? ? ???????? IP_address
			IF (ParcerCnt_o = 13) THEN Tx_Data <= IP_Addr0_;	END IF;
			IF (ParcerCnt_o = 14) THEN Tx_Data <= IP_Addr1_; END IF;
			-- ???????? ?????????? IP_address
			IF (ParcerCnt_o = 9) THEN  
				if (Rx_Data_ = IP_Addr0_) THEN IP_True(0) <= '1'; END IF;
			END IF;
			IF (ParcerCnt_o = 17) THEN  
				if (Rx_Data_ = IP_Addr1_) THEN IP_True(1) <= '1'; END IF;
			END IF;
			IF ( (ParcerCnt_o = 18) AND ((IP0_Addr_Flag_o AND IP1_Addr_Flag_o) = '0') )
				THEN ParcerEndCycle	 <= '1';
					  Wrong_IP_address <= '1';
			END IF;
			
			-- UDP header
			IF (ParcerCnt_o = 17) THEN Tx_Data <= Port_i;	END IF; -- ???? 27015 ?? ?????????
			IF (ParcerCnt_o = 19) THEN Tx_Data <= Rx_Data_;	END IF;
			IF (ParcerCnt_o = 20) THEN Tx_Data <= '0';		END IF;
			
			-- ??????????? ? ?????????? ??? ????????? ???????
			IF (ParcerCnt_o = RxCommandPath) THEN CMD_CS <= '1'; Tx_Data <= (Rx_Data_ OR X"0002"); 
														ELSE CMD_CS <= '0';                                   
			END IF;
			IF (ParcerCnt_o = RxCommandPath+1) THEN Tx_Start_Pulse <= '1'; -- ???? IP ? UDP ????????? ?????????? ?? ?????????
														  ELSE Tx_Start_Pulse <= '0'; -- ?? ????? ?????? ??????? ???????? ???????? ?????? ?? Tx ???????
			END IF;

			-- ??????????? ????? ? ??????
			-- ??????????? ?????? ??? ?????,?????? downto .
			IF ((ParcerCnt_o >= RxDataPath) AND (ParcerCnt_o <= RxByteLenght)) THEN -- ?????, ??????, ?????, ??????,  downto . 
				IF (ParcerCnt_o(0) = '0') THEN ADDR_CS <= '1'; Tx_Data <= Rx_Data_; END IF;
				IF (ParcerCnt_o(0) = '1') THEN DATA_CS <= '1'; Tx_Data(15 downto 8) <= DataBus_In(7 downto 0); Tx_Data(7 downto 0) <= DataBus_In(15 downto 8); END IF;
			ELSE ADDR_CS <= '0'; DATA_CS <= '0';
			END IF;
			Requiest_Enable <= Pascer_Sample_Enable AND ADDR_CS;
			TA_REG_Load		 <= Pascer_Sample_Enable AND ADDR_CS;


			IF (DATA_CS = '1') THEN Tx_Word_Strobe_ <= Data_Sent_OK;-- AND !DFF(.d=Data_Sent_OK,.clk=Clock);
									 ELSE Tx_Word_Strobe_ <= Pascer_Sample_Enable;
			END IF;
			ParcerCnt_Inc_ <= Tx_Word_Strobe_;

			AddrBusOut(15 downto 0)	<= Target_Address_Reg_o;

		ELSE
			IPv4_type_True				 <= '0';
			Rx_Addr						 <= (others => '0');
			Tx_Addr						 <= (others => '0');
			Tx_Data						 <= (others => '0'); 
			ParcerEndCycle				 <= '0';
			CMD_CS						 <= '0';
			ADDR_CS						 <= '0';
			DATA_CS						 <= '0';
			AddrBusOut(15 downto 0)	 <= (others => '0');
			BUS_Direct					 <= '0';
			Requiest_Enable			 <= '0';
			TA_REG_Load					 <= '0';
			Tx_Word_Strobe_			 <= '0';
			IP_True(0)					 <= '0';
			IP_True(1)					 <= '0';
			Wrong_IP_address			 <= '0';
		end if;
	end process;
	
--	Identification_Cnt.(clock,cnt_en) = (Clock,Edge_Sensing_Sync(.d=IPv4_type_True,.clk=Clock));
	Identification_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> IPv4_type_True,
		q		=> Identification_ES_o
		);
	Identification_Cnt: entity work.V_Counter
	generic map(
		width => WORD_WIDTH
		)
	port map (
		clock		=> Clock,--Quarts,--
		clk_en	=> '1',
		cnt_en	=> Identification_ES_o,
		q			=> Identification_Cnt_o
		);

	-- Master Access Control: ????????? ?????? ?? ?????????? ???? ???????
	-- ?????????? ?????? ?? ???? ???????
--	Access_Request.S  	=	Requiest_Enable; 
--	Access_Request.clk 	=  	Clock;
--	Access_Request.R  	=	Data_Sent_OK; 
	Access_Request : entity work.SRFF 
	port map (
		S		=> Requiest_Enable,
		CLK	=> Clock,
		R		=> Data_Sent_OK,
		ena	=> '1',
		q		=> Access_Request_o
		);

	AccessRequest	<=	Access_Request_o;
	DirectOut		<= BUS_Direct;

	Data_Sent_OK	<=	DataBusStrobe AND AccessGranted;
	
--	Target_Command_Reg.(clock, enable, load, sclr) = (Clock, (CMD_CS  AND Pascer_Sample_Enable) OR Rx_Parcer_RQ_ES.q OR Reset, VCC, Rx_Parcer_RQ_ES.q OR Reset);
--	Target_Command_Reg.data[15..8] = Rx_Data_[7..0]; Target_Command_Reg.data[7..0] = Rx_Data_[15..8];
	Target_Command_Reg: entity work.ShiftReg
		generic map (WIDTH => WORD_WIDTH) 
		port map(clock	=> Clock,
				data(15 downto 8)	=> Rx_Data(7 downto 0),
				data(7 downto 0)	=> Rx_Data(15 downto 8),
				enable=> (CMD_CS  AND Pascer_Sample_Enable) OR Rx_Parcer_RQ_ES_o OR Reset,
				sclr	=> Rx_Parcer_RQ_ES_o OR Reset,
				q		=> Target_Command_Reg_o
		);
--	Target_Address_Reg.(clock, enable, load, sclr) = (Clock, TA_REG_Load OR Rx_Parcer_RQ_ES.q, VCC, Rx_Parcer_RQ_ES.q OR Reset);
--	Target_Address_Reg.data[15..8] = Rx_Data_[7..0]; Target_Address_Reg.data[7..0] = Rx_Data_[15..8];
	Target_Address_Reg: entity work.ShiftReg
		generic map (WIDTH => WORD_WIDTH) 
		port map(clock	=> Clock,
				data(15 downto 8)	=> Rx_Data(7 downto 0),
				data(7 downto 0)	=> Rx_Data(15 downto 8),
				enable=> TA_REG_Load OR Rx_Parcer_RQ_ES_o,
				sclr	=> Rx_Parcer_RQ_ES_o OR Reset,
				q		=> Target_Address_Reg_o
		);

--	CASE (Target_Command_Reg_o AND X"00FF")IS  -- ??????????? ??????? ????, ?????????? ??? ???????
--		WHEN CMD_READ_AD	=> BUS_Direct <= '0'; 
--		WHEN CMD_WRITE_AD	=> BUS_Direct <= '1'; 
--		WHEN OTHERS			=> BUS_Direct <= '0'; 
--	END CASE;
	BUS_Direct <= '1' when (Target_Command_Reg_o AND X"00FF") = CMD_WRITE_AD
				else '0';
--	IF(Target_Command_Reg.q[15..8] == 2) THEN Tx_Start_Pulse_EN = GND; -- ??????????? ??????? ????, ?????????? ????????? ??? ???????????? ???????
--													 ELSE Tx_Start_Pulse_EN = VCC;
--	END IF;
	Tx_Start_Pulse_EN <= '0' when (Target_Command_Reg_o(15 downto 8) = 2)  -- ??????????? ??????? ????, ?????????? ????????? ??? ???????????? ???????
						 ELSE '1';

	-- ?????
--	Raw_type_Flag.(S,clk,R)       = (Raw_type_True, Clock, Rx_Parcer_RQ_ES.q OR Reset);
	Raw_type_Flag : entity work.SRFF 
	port map (
		S		=> Raw_type_True,
		CLK	=> Clock,
		R		=> Rx_Parcer_RQ_ES_o OR Reset,
		ena	=> '1',
		q		=> Raw_type_Flag_o
		);
--	Tx_Start_Pulse_Flag.(S,clk,R)  = (Tx_Start_Pulse_EN AND Tx_Start_Pulse, Clock, %Rx_Parcer_RQ_ES.q OR% Reset OR ParcerEndCyle);
	Tx_Start_Pulse_Flag : entity work.SRFF 
	port map (
		S		=> Tx_Start_Pulse_EN AND Tx_Start_Pulse,
		CLK	=> Clock,
		R		=> Reset OR ParcerEndCyle,
		ena	=> '1',
		q		=> Tx_Start_Pulse_Flag_o
		);
--	IP_address_check_Flag[0].(S,clk,R) = (IP_True[0], Clock, Rx_Parcer_RQ_ES.q OR Reset);
	IP0_Addr_Flag: entity work.SRFF 
	port map (
		S		=> IP0_True,
		CLK	=> Clock,
		R		=> Reset OR Rx_Parcer_RQ_ES_o,
		ena	=> '1',
		q		=> IP0_Addr_Flag_o
		);
--	IP_address_check_Flag[1].(S,clk,R) = (IP_True[1], Clock, Rx_Parcer_RQ_ES.q OR Reset);
	IP1_Addr_Flag : entity work.SRFF 
	port map (
		S		=> IP1_True,
		CLK	=> Clock,
		R		=> Reset OR Rx_Parcer_RQ_ES_o,
		ena	=> '1',
		q		=> IP1_Addr_Flag_o
		);

	-- ??????
	Wrong_IP_address_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> Wrong_IP_address,
		q		=> Wrong_IP_address_ES_o
		);

	Rx_Error_IP					<= Wrong_IP_address_ES_o;--ARP_Err_Flag_o;
	Rx_NOT_RQ					<= (not IPv4_type_Flag_o) AND ParcerEndCyle;
	Tx_Start						<= Tx_Start_Pulse_Flag_o AND ParcerEndCyle;
	Rx_Parcer_in_progress	<= RxParcerActive_o;
	--Tx_Word_Strobe 		  = (RxParcerActive_o AND Tx_Strobe AND Pascer_Sample_Enable); -- ????? ???????????? ? Tx ??????
	Tx_Word_Strobe				<= (RxParcerActive_o AND Tx_Word_Strobe_); -- ????? ???????????? ? Tx ??????
	Rx_TRUE_RQ					<= IPv4_type_Flag_o AND ParcerEndCyle;
	
	Identification				<= Identification_Cnt_o;
	
	Test							<= IPv4_type_Flag_o;

end Behavioral;