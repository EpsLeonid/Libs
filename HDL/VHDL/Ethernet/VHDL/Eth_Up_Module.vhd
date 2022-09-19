----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 30/06/2022  
-- Design Name: 
-- Module Name: Eth_Up_Module - Behavioral 
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

entity Eth_Up_Module is
	Port (
		Clock						: in std_logic; -- System Clock, really Bus_Clock
		-- PHY Ethernet I/O
		-- Rx section    --Preambula, SOF and CRC are cutted out
		Byte_Strobe_Rx			: in std_logic;
		Rx_Data					: in std_logic_vector (7 downto 0);
		RxPacket_in_progress	: in std_logic;
		RxPacket_End			: in std_logic;
		Packet_Good_End		: in std_logic; -- CRC result
		Packet_bad_End			: in std_logic; -- CRC result

		RxIntStart				: in std_logic;
		RxIntStart_out			: out std_logic;

		Packet_Decode_Error	: out std_logic; --next packet is detected while current packet is in processing

		-- Tx section
		Byte_Strobe_Tx			: in std_logic;
		Tx_Data					: out std_logic_vector (7 downto 0); 
		Transmit_of_Data_RQ	: out std_logic;
		Eth_Tx_In_Progress	: in std_logic := '0';	-- Transmittion is in progress(flag), Phy is busy
		Eth_Tx_End				: out std_logic; -- 

		Eth_RxTx_In_Progress	: out std_logic; --


		-- Standard bus connections
		BUS_Clock				: in std_logic;
		DataBus_In				: in std_logic_vector (15 downto 0);  -- 
		DataBusOut				: out std_logic_vector (15 downto 0); -- 

		DataBusStrobe			: in std_logic;  -- 
		Select_i					: in std_logic;  -- 
		DirectIn					: in std_logic;  -- 
		AddrBus_In				: in std_logic_vector (12 downto 0);  -- 
		Reset						: in std_logic := '0';
		-- Master Mode Signals 
		AccessRequest			: out std_logic;
		AccessGranted			: in std_logic; 
		DirectOut				: out std_logic;
		AddrBusOut				: out std_logic_vector (15 downto 0)
		);
		
		constant PacketLenghts_at_signaling_layer	: integer := 4096;--2048;-- maximum length value in bytes
		constant RxByte_Cnt_Width						: integer := integer(Ceil(log2(real(PacketLenghts_at_signaling_layer)))); --12; --

		constant WORD_WIDTH								: integer := 16; 

		constant ETH_HEADER_LENGTH						: integer := 14;
		constant IP_HEADER_LENGTH						: integer := 20;
		constant UDP_HEADER_LENGTH						: integer := 8;
		constant OPCODE_LENGTH							: integer := 2;

		constant KLUKVA_DATA_LENGTH					: integer := ((32+512+32)*2)*2; --2304;
		constant HEADER_LENGTH_BYTES					: integer := ETH_HEADER_LENGTH + IP_HEADER_LENGTH + UDP_HEADER_LENGTH + OPCODE_LENGTH;
		constant HEADER_LENGTH_WORDS					: integer := HEADER_LENGTH_BYTES / 2;
		constant MASS_RAM_BYTE_Tx_Num					: integer := HEADER_LENGTH_BYTES + KLUKVA_DATA_LENGTH;

end Eth_Up_Module;

architecture Behavioral of Eth_Up_Module is

	signal RxByte_Cnt_sclr					: std_logic;
	signal RxByte_Cnt_o						: std_logic_vector (RxByte_Cnt_Width downto 0);
	signal RxByte_Cnt_Reg_o					: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal RAM_Overflow						: std_logic;
	signal RAM_Overflow_SRFF_o				: std_logic;
	signal Cnt_Overflow_Edge_o				: std_logic;
	signal RxReadyToRecive_r				: std_logic;
	signal RxReadyToRecive_o				: std_logic;
	signal RxReadyToRecive					: std_logic;
	signal SetRxReadyToRecive				: std_logic;
	signal SetRxReadyToRecive_Parcer_d	: std_logic;
	signal SetRxReadyToRecive_Parcer_o	: std_logic;
	signal SetRxReadyToRecive_Sc_Bus		: std_logic;
	signal RxRAM_wea							: std_logic;
	signal RxRAM_web							: std_logic;
	signal RxRAM_q_a							: std_logic_vector (7 downto 0);
	signal RxRAM_q_b							: std_logic_vector (15 downto 0);
	signal RxRAM_CS							: std_logic;
	signal Rx_RAM_Address_Bus				: std_logic_vector (RxByte_Cnt_Width-2 downto 0);

	signal RxIntRAM_web						: std_logic;
	signal RxIntRAM_q_a						: std_logic_vector (15 downto 0);
	signal RxIntRAM_q_b						: std_logic_vector (15 downto 0);
	signal RxIntRAM_CS						: std_logic;
	signal RxIntStart_ES_d					: std_logic;
	signal RxIntStart_ES_o					: std_logic;
	signal RxIntStart_node					: std_logic;
	signal Buffer_RAM_Data					: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal ParcerCycle_IntStart_o			: std_logic;

	signal Status_REG_o						: std_logic_vector (WORD_WIDTH-1 downto 0); -- ?????? ???????????? (???? ????? ??? ????????)
	signal Status_REG_CS						: std_logic;
	signal Status_REG_ES_d					: std_logic;
	signal Status_REG_ES_o					: std_logic;

	signal RxLostPackcet_ES_o				: std_logic;
	signal RxLostPacket_node				: std_logic;
	signal RxLostPacket_Cnt_o				: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal RxLostPacket_Cnt_REG_o			: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal RxLostPacket_Cnt_REG_CS		: std_logic;
	signal RxLostPacket_Cnt_REG_ES_d		: std_logic;
	signal RxLostPacket_Cnt_REG_ES_o		: std_logic;

	signal TxByte_Cnt_cnt_en				: std_logic;
	signal TxByte_Cnt_o						: std_logic_vector (RxByte_Cnt_Width downto 0);
	signal TxRAM_q_a							: std_logic_vector (7 downto 0);
	signal TxRAM_q_b							: std_logic_vector (15 downto 0);
	signal Tx_RAM_Address_Bus				: std_logic_vector (RxByte_Cnt_Width-2 downto 0);
	signal Tx_RAM_Data_Bus					: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal TxRAM_CS							: std_logic;
	signal TxRQ_Reset							: std_logic;
	signal TxRQ_Reset_ES_o					: std_logic;
	signal TxRQ_SRFF_r						: std_logic;
	signal TxRQ_SRFF_o						: std_logic;
	signal InternalTxStart					: std_logic;
	signal AnswerTxStart						: std_logic;
	signal AnswerTxStart_tmp_d				: std_logic;
	signal AnswerTxStart_tmp_o				: std_logic;
	signal AnswerTxStart_SR_o				: std_logic;
	signal AnswerTxStart_ES_o				: std_logic;
	signal TxStart								: std_logic;
	signal TxStart_ES_o						: std_logic;

	signal PacketLenghts_to_be_transmitted_Reg_o		:std_logic_vector (WORD_WIDTH-1 downto 0); 
	signal PacketLenghts_to_be_transmitted_Reg_CS	: std_logic;
	signal PacketLenghts_DataBus							: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal PacketLenghts_to_be_transmitted_Reg_EN	: std_logic;

	signal TxRAM_wren							: std_logic;

	type Module_MAC is array (0 to 2) of std_logic_vector (WORD_WIDTH-1 downto 0);
	type Module_IP is array (0 to 1) of std_logic_vector (WORD_WIDTH-1 downto 0);

	signal Module_MAC_Reg_en				: std_logic_vector (2 downto 0);
	signal Module_MAC_Reg_o					: Module_MAC;
	signal Module_MAC_Reg_CS				: std_logic_vector (2 downto 0);
	signal Module_IP_Reg_en					: std_logic;
	signal Module_IP_Reg_o					: Module_IP;
	signal Module_IP_Reg_CS					: std_logic_vector (1 downto 0);
	signal Port_Reg_en						: std_logic;
	signal Port_Reg_o							: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Port_Reg_CS						: std_logic;

	signal RxWordRecive_Reg_en				: std_logic;
	signal RxWordRecive_Reg_o				: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Rx_Packet_Lenght_Reg_en		: std_logic;
	signal Rx_Packet_Lenght_Reg_o			: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Rx_Packet_Lenght_Reg_i			: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal ParcerCycle_SRFF_s				: std_logic;
	signal ParcerCycle_SRFF_o				: std_logic;
	signal ParcerCycleEnd_d					: std_logic;
	signal ParcerCycleEnd_o					: std_logic;

	type Source_MAC is array (0 to 2) of std_logic_vector (WORD_WIDTH-1 downto 0);

--	signal MAC_Decoder						: Eth_MAC_Decoder;
	signal MAC_Decoder_Rx_Addr			: std_logic_vector (10 downto 0);
	signal MAC_Decoder_Tx_Addr			: std_logic_vector (10 downto 0);
	signal MAC_Decoder_Tx_Data			: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal MAC_Decoder_Tx_Word_Strobe	: std_logic;
	signal MAC_Decoder_Rx_Parcer_RQ		: std_logic;
	
	signal MAC_Decoder_Rx_Error_MAC		: std_logic;
	signal MAC_Decoder_Next_Parcer		: std_logic;
	signal MAC_Decoder_Next_Parcer_ES_o	: std_logic;
	signal MAC_Rx_Parcer_in_progress		: std_logic;

	signal Source_MAC_Reg_en				: std_logic_vector (2 downto 0);
	signal Source_MAC_Reg_o					: Source_MAC;
	
--	signal ARP_Decoder						: Eth_ARP_Decoder;
	signal ARP_Decoder_Rx_Addr		: std_logic_vector (10 downto 0);
	signal ARP_Decoder_Tx_Addr		: std_logic_vector (10 downto 0);
	signal ARP_Decoder_Tx_Data		: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal ARP_Decoder_Tx_Word_Strobe	: std_logic;
	
	signal ARP_Decoder_Rx_IP_Error		: std_logic;
	signal ARP_Decoder_Rx_NOT_RQ			: std_logic;
	signal ARP_Decoder_Tx_Start			: std_logic;
	signal ARP_Decoder_Rx_TRUE_RQ			: std_logic;
	signal ARP_Decoder_Rx_Parcer_in_progress	: std_logic;
	
	signal ARP_Decoder_Test					: std_logic;

--	signal IPv4_Decoder						: Eth_IPv4_Decoder;
	signal IPv4_Decoder_Rx_Addr		: std_logic_vector (10 downto 0);
	signal IPv4_Decoder_Tx_Addr		: std_logic_vector (10 downto 0);
	signal IPv4_Decoder_Tx_Data		: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal IPv4_Decoder_Tx_Word_Strobe	: std_logic;
	
	signal IPv4_Decoder_Identification	: std_logic_vector (15 downto 0);
	signal IPv4_Decoder_Rx_Error_IP	: std_logic;
	signal IPv4_Decoder_Rx_NOT_RQ		: std_logic;
	signal IPv4_Decoder_Tx_Start		: std_logic;
	signal IPv4_Decoder_Rx_TRUE_RQ	: std_logic;
	signal IPv4_Decoder_Rx_Parcer_in_progress		: std_logic;
	
	signal IPv4_Decoder_AccessRequest	: std_logic;
	signal IPv4_Decoder_DirectOut			: std_logic;
	signal IPv4_Decoder_AddrBusOut		: std_logic_vector (15 downto 0);
	
	signal IPv4_Decoder_test				: std_logic;
	
--	signal Raw_Decoder						: Eth_Raw_Decoder;
	signal Raw_Decoder_Rx_Addr		: std_logic_vector (10 downto 0);
	signal Raw_Decoder_Tx_Addr		: std_logic_vector (10 downto 0);
	signal Raw_Decoder_Tx_Data		: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Raw_Decoder_Tx_Word_Strobe	: std_logic;

	signal Raw_Decoder_Rx_Error_IP		: std_logic;
	signal Raw_Decoder_Tx_Start			: std_logic;
	signal Raw_Decoder_Rx_Parcer_in_progress	: std_logic;
	signal Raw_Decoder_Rx_TRUE_RQ			: std_logic;
	signal Raw_Decoder_Rx_NOT_RQ			: std_logic;
	signal Raw_Decoder_Identification	: std_logic_vector (WORD_WIDTH-1 downto 0);
	
	signal Raw_Decoder_AccessRequest		: std_logic;
	signal Raw_Decoder_DirectOut			: std_logic;
	signal Raw_Decoder_AddrBusOut			: std_logic_vector (15 downto 0);
	signal Raw_Decoder_test					: std_logic;
	
--	signal CCCD_Decoder						: Eth_CCCD_Decoder;
	signal CCCD_Decoder_Rx_Addr			: std_logic_vector (10 downto 0);
	signal CCCD_Decoder_Tx_Addr			: std_logic_vector (10 downto 0);
	signal CCCD_Decoder_Tx_Data			: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal CCCD_Decoder_Tx_Word_Strobe	: std_logic;

	signal CCCD_Decoder_Rx_Error_IP		: std_logic;
	signal CCCD_Decoder_Tx_Start			: std_logic;
	signal CCCD_Decoder_Rx_Parcer_in_progress			: std_logic;
	signal CCCD_Decoder_Rx_TRUE_RQ		: std_logic;
	signal CCCD_Decoder_Rx_NOT_RQ			: std_logic;
	signal CCCD_Decoder_Identification	: std_logic_vector (WORD_WIDTH-1 downto 0);

	signal CCCD_Decoder_AccessRequest	: std_logic;
	signal CCCD_Decoder_DirectOut			: std_logic;
	signal CCCD_Decoder_AddrBusOut		: std_logic_vector (15 downto 0);
	signal CCCD_Decoder_test				: std_logic;

	signal DirectOut_tmp						: std_logic;
	signal SelDirPar							: std_logic_vector (2 downto 0);
	
	signal Test_Reg_en						: std_logic;
	signal Test_Reg_o							: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal Test_Reg_CS						: std_logic;

	signal DataBusOut_tmp					: std_logic_vector (15 downto 0);

--	signal IPv4_CheckSum_Ctrl				: IPv4_checkSum;
	signal IPv4_CheckSum_Ctrl_Rx_Addr	: std_logic_vector (10 downto 0);
	signal IPv4_CheckSum_Ctrl_Tx_Addr	: std_logic_vector (10 downto 0);
	signal IPv4_CheckSum_Ctrl_Tx_Data	: std_logic_vector (WORD_WIDTH-1 downto 0);
	signal IPv4_CheckSum_Ctrl_Tx_Word_Strobe: std_logic;
	signal IPv4_CheckSum_Ctrl_Complete		: std_logic;
	
	signal Rx_Packet_Cnt_o					: std_logic_vector (15 downto 0);
	signal Tx_Packet_Cnt_o					: std_logic_vector (15 downto 0);
	signal Rx_Packet_Cnt_Reset				: std_logic;
	signal Tx_Packet_Cnt_Reset				: std_logic;

	signal RxTxCycle_IntStart_o			: std_logic;

	signal In_FIFO_e							: std_logic;
	signal In_FIFO_wr_en						: std_logic;
	signal In_FIFO_rd_en						: std_logic;
	signal In_FIFO_o							: std_logic_vector (9 downto 0);
	signal Copy_Byte_Strobe					: std_logic;
	signal FIFO_to_RxRAM_Copy				: std_logic; -- ?????? ?????????? ????????????? ?????? ????????? ??????? ???????? ?????? ? ?????? ?????? ? ???????? ??????
	signal FIFO_to_RxRAM_Copy_ES_o		: std_logic;
	signal RAM_Ptr_Packet_End				: std_logic; -- ?????? ????????? ??????????? ?????? ?? FIFO ? ???????? ?????? ? ??????? ????????? ???????????? ??????

	signal Packet_Good_End_ES_o			: std_logic; -- ????????? ????????? ???????? ?????? (CRC ???????)
	signal Packet_bad_End_ES_o				: std_logic; -- ????????? ????????? ??????? ?????? (CRC ?????????)

	signal Out_FIFO_e							: std_logic;
	signal Out_FIFO_f							: std_logic;
	signal Out_FIFO_o							: std_logic_vector (9 downto 0);
	signal Tx_FIFO_RQ_s						: std_logic;
	signal Tx_FIFO_RQ_r						: std_logic;
	signal Tx_FIFO_RQ_o						: std_logic;
	-- ???????? ?????????? ????? ????????, ????????????? ??????
	signal GuardTime_Cnt_o					: std_logic_vector (7 downto 0);
	signal GuardTime_Cnt_Rst				: std_logic;
	signal GuardTime_s						: std_logic;
	signal GuardTime_o						: std_logic;
	signal Eth_Tx_In_Progress_del			: std_logic;
	
begin

-------------------------------------------- Input FIFO --------------------------------------------
--	RxIntStart_out = RxIntStart_ES.q;--RxIntStart;
	RxIntStart_out <= RxIntStart_ES_o;
	
	In_FIFO_wr_en <= Byte_Strobe_Rx OR RxPacket_End;
	In_FIFO_rd_en <= not(In_FIFO_e) AND Copy_Byte_Strobe AND RxReadyToRecive_o;
	In_FIFO: entity work.Eth_In_FIFO4kb
	Port map(
		din(7 downto 0)	=> Rx_Data(7 downto 0), 

		din(8)	=> Packet_bad_End,
		din(9)	=> Packet_Good_End,

		clk		=> Clock,
		wr_en		=> In_FIFO_wr_en,
		rd_en		=> In_FIFO_rd_en,
		
		empty		=> In_FIFO_e,
		dout		=> In_FIFO_o
		);

	FIFO_to_RxRAM_Copy	<=	not(In_FIFO_e) and Copy_Byte_Strobe AND RxReadyToRecive_o;
	Copy_Byte_Strobe		<=	'1';

--	Packet_Good_End_ES.(d,clk) = (In_FIFO.q(9],Clock);
	Packet_Good_End_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> In_FIFO_o(9),
		q		=> Packet_Good_End_ES_o
		);

--	Packet_bad_End_ES.(d,clk)  = (In_FIFO.q(8],Clock);
	Packet_bad_End_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> In_FIFO_o(8), 
		q		=> Packet_bad_End_ES_o
		);

	-------------------------------------------- Rx section --------------------------------------------
--	RxReadyToRecive.(S,clk,R) = (SetRxReadyToRecive, Clock, RAM_Ptr_Packet_End OR RxIntStart_ES.q);
	RxReadyToRecive_r <= RAM_Ptr_Packet_End OR RxIntStart_ES_o;
	RxReadyToRecive_SR : entity work.SRFF 
	port map (
		S		=> SetRxReadyToRecive,
		CLK	=> Clock,
		R		=> RxReadyToRecive_r,
		q		=> RxReadyToRecive_o
		);

	SetRxReadyToRecive <= SetRxReadyToRecive_Sc_Bus OR SetRxReadyToRecive_Parcer_o;
	Eth_RxTx_In_Progress <= not(RxReadyToRecive_o);

--	RxByte_Cnt.(clock,clk_en,cnt_en,sclr) = (Clock,VCC, FIFO_to_RxRAM_Copy , --Enable to count actual length
--													SetRxReadyToRecive OR Packet_bad_End_ES.q    --Not to clear while processing in progress to know actual length
--														); 
--	RxByte_Cnt.data() = MASS_RAM_BYTE_Tx_Num-1;  
--	RxByte_Cnt.sload  = RxIntStart_ES.q;
	RxByte_Cnt_sclr <= (SetRxReadyToRecive OR Packet_bad_End_ES_o);
	RxByte_Cnt : entity work.V_Counter 
		generic map(
			WIDTH => RxByte_Cnt_Width+1
			)
		port map (
			clock 	=> clock,--Quarts,--
			cnt_en	=>	FIFO_to_RxRAM_Copy,
			sload		=> RxIntStart_ES_o,
			data		=> conv_std_logic_vector((MASS_RAM_BYTE_Tx_Num - 1),13),
			sclr		=> RxByte_Cnt_sclr,
			q			=> RxByte_Cnt_o
			);

--	IF 	(RxByte_Cnt.q[] == PacketLenghts_at_signaling_layer-1) --Check to not overwrite RAM at ubnormal length packets
--		THEN RAM_Overflow	= VCC;
--		ELSE RAM_Overflow	= GND;
--	END IF;
	RAM_Overflow <= '0' when (RxByte_Cnt_o = (PacketLenghts_at_signaling_layer - 1))else --Check to not overwrite RAM at ubnormal length packets
						 '1';

--	RAM_Overflow_SRFF.(S,clk,R) = (Cnt_Overflow_Edge.q,Clock,SetRxReadyToRecive);
	RAM_Overflow_SRFF : entity work.SRFF 
	port map (
		S		=> Cnt_Overflow_Edge_o,
		CLK	=> Clock,
		R		=> SetRxReadyToRecive,
		q		=> RAM_Overflow_SRFF_o
		);
		
--	Cnt_Overflow_Edge.(clk,d)	=	(Clock, RAM_Overflow); -- 
	Cnt_Overflow_Edge: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> RAM_Overflow, 
		q		=> Cnt_Overflow_Edge_o
		);

	Packet_Decode_Error <= Cnt_Overflow_Edge_o; -- 

	RAM_Ptr_Packet_End <= Packet_Good_End_ES_o;
	process(Clock)
	begin
		RxReadyToRecive <= RxReadyToRecive_o;
	end process;

	--RxIntStart_ES.(d,clk)     = (RxIntStart AND DFF(.d=RxReadyToRecive.q,.clk=Clock), Clock);
	RxIntStart_ES_d <= RxIntStart and RxReadyToRecive;
	RxIntStart_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> RxIntStart_ES_d, 
		q		=> RxIntStart_ES_o
		);

--	RxIntStart_node = DFF(.d=RxIntStart_ES.q,.clk=Clock);
	process(Clock)
	begin
		RxIntStart_node <= RxIntStart_ES_o;
	end process;

--	IF((RxPacket_in_progress == VCC) AND (RxReadyToRecive.q == GND)) THEN RxLostPacket_node = VCC; -- 
--																 ELSE RxLostPacket_node = GND; -- 
--	END IF;
	RxLostPacket_node <= '1' when ((RxPacket_in_progress = '1') and (RxReadyToRecive = '0')) 
						 else '0'; -- 

--	RxRAM.data_a(7 downto 0) = In_FIFO_o(7 downto 0);  -- 
--	RxRAM.(address_a(RxByte_Cnt_Width-1 downto 0)   , clock_a, wren_a                       ) =
--		(RxByte_Cnt.q(RxByte_Cnt_Width-1 downto 0], Clock  , FIFO_to_RxRAM_Copy AND !RAM_Overflow_SRFF.q); 
--
--	RxRAM.(address_b(RxByte_Cnt_Width-2 downto 0)     , clock_b  , data_b(15 downto 8)   , data_b(7 downto 0)     , wren_b  ) =
--		(Rx_RAM_Address_Bus[], BUS_Clock, DataBus_In(7 downto 0], DataBus_In(15 downto 8], RxRAM_CS AND DataBusStrobe AND DirectIn AND Select); 
	RxRAM_wea <= FIFO_to_RxRAM_Copy and not(RAM_Overflow_SRFF_o);
	RxRAM_web <= RxRAM_CS AND DataBusStrobe AND DirectIn AND Select_i;
	RxRAM: entity work.EthBufferRAM2048
		Port map(
			dina					=> In_FIFO_o(7 downto 0),
			addra					=> RxByte_Cnt_o(RxByte_Cnt_Width-1 downto 0),
			clka					=> Clock,
			wea					=> RxRAM_wea,
			douta					=> RxRAM_q_a,
			
			addrb					=> Rx_RAM_Address_Bus,
			clkb					=> BUS_Clock,
			dinb(15 downto 8)	=> DataBus_In(7 downto 0),
			dinb(7 downto 0)	=> DataBus_In(15 downto 8),
			web					=> RxRAM_web,
			doutb					=> RxRAM_q_b
			);

--	RxLostPackcet_ES.(d,clk)                            = (RxLostPacket_node, Clock);
	RxLostPackcet_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> RxLostPacket_node, 
		q		=> RxLostPackcet_ES_o
		);

--	RxLostPacket_Cnt.(clock,clk_en,cnt_en,sclr)        = (Clock, VCC, RxLostPackcet_ES.q, GND%RxPacketError_Cnt_REG_ES.q%);
	RxLostPacket_Cnt: entity work.V_Counter
	generic map(
		width => 26
		)
	port map (
		clock		=> Clock,--Quarts,--
		clk_en	=> '1',
		cnt_en	=> RxLostPackcet_ES_o,
		sclr		=> '0',
		q			=> RxLostPacket_Cnt_o
		);

--	RxLostPacket_Cnt_REG.(data[],clock, load, enable)  = (RxLostPacket_Cnt.q[], BUS_Clock, VCC, VCC%RxPacketError_Cnt_REG_ES.q%);
	RxLostPacket_Cnt_REG: entity work.ShiftReg
		generic map (WIDTH => WORD_WIDTH) 
		port map(
			clock	=> BUS_Clock,
			data	=> RxLostPacket_Cnt_o,
			load	=> '1',
			enable=> '1',
			q		=> RxLostPacket_Cnt_REG_o
		);

--	RxLostPacket_Cnt_REG_ES.(d,clk)                    = (RxLostPacket_Cnt_REG_CS AND Select AND DataBusStrobe, BUS_Clock);
	RxLostPacket_Cnt_REG_ES_d <= RxLostPacket_Cnt_REG_CS AND Select_i AND DataBusStrobe;
	RxLostPacket_Cnt_REG_ES: entity work.Edge_Sensing
	Port map(
		clk	=> BUS_Clock,
		d		=> RxLostPacket_Cnt_REG_ES_d,
		q		=> RxLostPacket_Cnt_REG_ES_o
		);

	-------------------------------------------- Output FIFO --------------------------------------------
--	Out_FIFO.data(7 downto 0) 	=	TxRAM.q_a();
--
--	Out_FIFO.data(8) 		=	GND; 
--	Out_FIFO.data(9) 		=	DFF(.d=TxRQ_Reset_ES.q,.clk=Clock); -- ?????? ????????? ??????
--
--	Out_FIFO.clock			=	Clock;
--	Out_FIFO.wrreq  		=	DFF(.d=TxRQ_SRFF.q,.clk=Clock);
--	Out_FIFO.rdreq  		=	Byte_Strobe_Tx;    

	Out_FIFO: entity work.Eth_In_FIFO4kb
		Port map(
			din(7 downto 0)	=> TxRAM_q_a,
			din(8)				=> '0',
			din(9)				=> TxRQ_Reset_ES_o, --DFF(.d=TxRQ_Reset_ES.q,.clk=Clock);
			clk					=> Clock,
			wr_en					=> TxRQ_SRFF_o, --DFF(.d=TxRQ_SRFF.q,.clk=Clock);
			rd_en					=> Byte_Strobe_Tx,
			dout					=> Out_FIFO_o,
			full					=> Out_FIFO_f,
			empty					=> Out_FIFO_e
		);

--	Tx_FIFO_RQ				=	SRFF(.S=!Out_FIFO.empty AND !Eth_Tx_In_Progress AND !GuardTime,.clk=Clock,.R=Out_FIFO.q(9) AND Byte_Strobe_Tx);--DFF(.d=!Out_FIFO.empty,.clk=Clock);--SRFF(.S=!Out_FIFO.empty AND !Tx_FIFO_RQ,.clk=Clock,.R=Out_FIFO.q(9]); 
	Tx_FIFO_RQ_s <= not Out_FIFO_e AND not Eth_Tx_In_Progress AND not GuardTime_o;
	Tx_FIFO_RQ_r <= Out_FIFO_o(9) AND Byte_Strobe_Tx;
	Tx_FIFO_RQ: entity work.SRFF 
		port map (
			S		=> Tx_FIFO_RQ_s,
			CLK	=> Clock,
			R		=> Tx_FIFO_RQ_r,
			q		=> Tx_FIFO_RQ_o
			);

	Transmit_of_Data_RQ	<=	Tx_FIFO_RQ_o;
	Tx_Data					<=	Out_FIFO_o(7 downto 0);

	-- ???????? ?????????? ????? ????????
--	GuardTime_Cnt.(clock,cnt_en,sclr) = (Clock, GuardTime, GuardTime_Cnt_Rst);
	GuardTime_Cnt: entity work.V_Counter
	generic map(
		width => 8
		)
	port map (
		clock		=> Clock,--Quarts,--
		clk_en	=> '1',
		cnt_en	=> GuardTime_o,
		sclr		=> GuardTime_Cnt_Rst,
		q			=> GuardTime_Cnt_o
		);
	GuardTime_Cnt_Rst <= '1' when (GuardTime_Cnt_o > 99)
						 else '0';

--	GuardTime = SRFF(.S=!Eth_Tx_In_Progress AND DFF(.d=Eth_Tx_In_Progress,.clk=Clock),.clk=Clock,.R=GuardTime_Cnt_Rst);
	process (clock)
	begin
		if clock'event and clock = '1' then  
			Eth_Tx_In_Progress_del <= Eth_Tx_In_Progress;
		end if;
	end process;
	
	GuardTime_s <= not Eth_Tx_In_Progress and Eth_Tx_In_Progress_del;
	GuardTime: entity work.SRFF 
		port map (
			S		=> GuardTime_s,
			CLK	=> Clock,
			R		=> GuardTime_Cnt_Rst,
			q		=> GuardTime_o
			);

	------------------------------------------- Tx section ------------------------------------------------- 
	TxStart <= InternalTxStart OR AnswerTxStart;
--	TxRQ_SRFF.(S,clk,R) = (TxStart ,Clock,TxRQ_Reset_ES.q OR Reset); 
	TxRQ_SRFF_r <= TxRQ_Reset_ES_o OR Reset;
	TxRQ_SRFF: entity work.SRFF 
		port map (
			S		=> TxStart,
			CLK	=> Clock,
			R		=> TxRQ_SRFF_r,
			q		=> TxRQ_SRFF_o
			);
--	TxByte_Cnt.(clock,clk_en,cnt_en,sclr) = (Clock,VCC, TxRQ_SRFF.q AND !Out_FIFO.full, TxStart);
	TxByte_Cnt_cnt_en <= TxRQ_SRFF_o and not Out_FIFO_f;
	TxByte_Cnt: entity work.V_Counter
	generic map(
		width => RxByte_Cnt_Width+1
		)
	port map (
		clock		=> Clock,--Quarts,--
		clk_en	=> '1',
		cnt_en	=> TxByte_Cnt_cnt_en,
		sclr		=> TxStart,
		q			=> TxByte_Cnt_o
		);
	TxRQ_Reset <= '1' when ((TxByte_Cnt_o >= PacketLenghts_at_signaling_layer-1) OR (TxByte_Cnt_o >= PacketLenghts_to_be_transmitted_Reg_o(RxByte_Cnt_Width downto 0)))
				else '0';

--	TxRQ_Reset_ES.(d,clk) = (TxRQ_Reset,Clock);
	TxRQ_Reset_ES: entity work.Edge_Sensing
	Port map(
		clk	=> BUS_Clock,
		d		=> TxRQ_Reset,
		q		=> TxRQ_Reset_ES_o
		);

--	TxRAM.data_a(7 downto 0) = GND;  
--	TxRAM.(address_a(RxByte_Cnt_Width-1 downto 0)   , clock_a, wren_a) =
--		(TxByte_Cnt.q(RxByte_Cnt_Width-1 downto 0], Clock  , GND   ); 
--
--	TxRAM.(address_b(RxByte_Cnt_Width-2 downto 0)         , clock_b  , data_b()         , wren_b  ) =
--		(Tx_RAM_Address_Bus(RxByte_Cnt_Width-2 downto 0], BUS_Clock, Tx_RAM_Data_Bus[], TxRAM_wren);
	TxRAM: entity work.EthBufferRAM2048
		Port map(
			dina		=> (others => '0'),
			addra		=> TxByte_Cnt_o(RxByte_Cnt_Width-1 downto 0),
			clka		=> Clock,
			wea		=> '0',
			douta		=> TxRAM_q_a,
			
			dinb		=> Tx_RAM_Data_Bus,
			addrb		=> Tx_RAM_Address_Bus(RxByte_Cnt_Width-2 downto 0),
			clkb		=> BUS_Clock,
			web		=> TxRAM_wren,
			doutb		=> TxRAM_q_b
			);

	-------------------------------------------------------------------------------------------------------------------  


	-----------------------------???????? ?????? ??? ???????? ?????? ?? ???????? ??????--------------------------------  
--	RxIntRAM.(address_a()     	 , clock_a  , data_a(15 downto 8)   , data_a(7 downto 0)     , wren_a  ) =
--			(Rx_RAM_Address_Bus[], BUS_Clock  , DataBus_In(7 downto 0], DataBus_In(15 downto 8], GND); 
--	RxIntRAM.(address_b() 		 			  , clock_b  , data_b(15 downto 8)   , data_b(7 downto 0)     , wren_b  ) =
--			(AddrBus_In(RxByte_Cnt_Width-2 downto 0], BUS_Clock, DataBus_In(7 downto 0], DataBus_In(15 downto 8], RxIntRAM_CS AND DataBusStrobe AND DirectIn AND Select); 

	RxIntRAM_web <= (RxIntRAM_CS AND DataBusStrobe AND DirectIn AND Select_i);
	RxIntRAM: entity work.RAM2048_2p
		Port map(
			dina(15 downto 8)	=> DataBus_In(7 downto 0),
			dina(7 downto 0)	=> DataBus_In(15 downto 8),
			addra					=> Rx_RAM_Address_Bus,
			clka					=> BUS_Clock,
			wea					=> "0",
			douta					=> RxIntRAM_q_a,
			
			addrb					=> AddrBus_In(RxByte_Cnt_Width-2 downto 0),
			clkb					=> BUS_Clock,
			dinb(15 downto 8)	=> DataBus_In(7 downto 0),
			dinb(7 downto 0)	=> DataBus_In(15 downto 8),
			web					=> RxIntRAM_web,
			doutb					=> RxIntRAM_q_b
		);

--	ParcerCycle_IntStart.(S,clk,R) = (RxIntStart_ES.q,Clock, ParcerCycleEnd);
	ParcerCycle_IntStart: entity work.SRFF 
		port map (
			S		=> RxIntStart_ES_o,
			CLK	=> Clock,
			R		=> ParcerCycleEnd_o,
			q		=> ParcerCycle_IntStart_o
			);
	 Buffer_RAM_Data <= RxIntRAM_q_a when ParcerCycle_IntStart_o = '1'
						ELSE RxRAM_q_b;
	-------------------------------------------------------------------------------------------------------------------  
	 

	------------------- ?????? ???????, ?????????? ??????????? ? ??? 
--	ParcerCycle_SRFF.(S,clk,R) = ((RAM_Ptr_Packet_End AND RxReadyToRecive.q) OR RxIntStart_ES.q,Clock,ParcerCycleEnd);   -- ???? ?? ???????? ?????????? ?????? ?? ????? ???????? ?????
	ParcerCycle_SRFF_s <= (RAM_Ptr_Packet_End AND RxReadyToRecive_o) OR RxIntStart_ES_o;
	ParcerCycle_SRFF: entity work.SRFF 
		port map (
			S		=> ParcerCycle_SRFF_s,
			CLK	=> Clock,
			R		=> ParcerCycleEnd_o,
			q		=> ParcerCycle_SRFF_o
			);
--	ParcerCycleEnd = Edge_Sensing_Sync(.d=(MAC_Decoder.Rx_Error_MAC OR ARP_Decoder.Rx_TRUE_RQ OR IPv4_Decoder.Rx_Error_IP OR ARP_Decoder.Rx_IP_Error OR IPv4_CheckSum_Ctrl.IPv4_CheckSum_Complete OR CCCD_Decoder.Rx_TRUE_RQ OR Raw_Decoder.Rx_TRUE_RQ OR Raw_Decoder.Rx_NOT_RQ),.clk=Clock);
	ParcerCycleEnd_d <= (MAC_Decoder_Rx_Error_MAC OR ARP_Decoder_Rx_TRUE_RQ OR IPv4_Decoder_Rx_Error_IP OR ARP_Decoder_Rx_IP_Error OR IPv4_CheckSum_Ctrl_Complete OR CCCD_Decoder_Rx_TRUE_RQ OR Raw_Decoder_Rx_TRUE_RQ OR Raw_Decoder_Rx_NOT_RQ);
	ParcerCycleEnd: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> ParcerCycleEnd_d,
		q		=> ParcerCycleEnd_o
		);
--	SetRxReadyToRecive_Parcer = Edge_Sensing_Sync(.d=TxRQ_Reset_ES.q OR MAC_Decoder.Rx_Error_MAC OR ARP_Decoder.Rx_IP_Error OR IPv4_Decoder.Rx_Error_IP OR Raw_Decoder.Rx_NOT_RQ,.clk=Clock);
	SetRxReadyToRecive_Parcer_d <= TxRQ_Reset_ES_o OR MAC_Decoder_Rx_Error_MAC OR ARP_Decoder_Rx_IP_Error OR IPv4_Decoder_Rx_Error_IP OR Raw_Decoder_Rx_NOT_RQ;
	SetRxReadyToRecive_Parcer: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> SetRxReadyToRecive_Parcer_d,
		q		=> SetRxReadyToRecive_Parcer_o
		);

	-- ??????? ?? ?????? ???????? ?????? ?? Tx ??????
--	AnswerTxStart_ = Edge_Sensing_Sync(.d=ARP_Decoder.Tx_Start OR IPv4_CheckSum_Ctrl.IPv4_CheckSum_Complete OR CCCD_Decoder.Tx_Start OR Raw_Decoder.Tx_Start,.clk=Clock);
	AnswerTxStart_tmp_d <= ARP_Decoder_Tx_Start OR IPv4_CheckSum_Ctrl_Complete OR CCCD_Decoder_Tx_Start OR Raw_Decoder_Tx_Start;
	AnswerTxStart_tmp: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> AnswerTxStart_tmp_d,
		q		=> AnswerTxStart_tmp_o
		);
--	IF(TxRQ_SRFF_o = '1') THEN AnswerTxStart = Edge_Sensing_Sync(.d=!SRFF(.S=AnswerTxStart_,.clk=Clock,.R=TxRQ_Reset),.clk=Clock); -- ????? ?????????, ??????? ????? ??????????? ??????????
--					ELSE AnswerTxStart = AnswerTxStart_;
--	END IF;
	AnswerTxStart_SR: entity work.SRFF
		port map (
			S		=> AnswerTxStart_tmp_o,
			CLK	=> Clock,
			R		=> TxRQ_Reset,
			q		=> AnswerTxStart_SR_o
			);
	AnswerTxStart_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> not AnswerTxStart_SR_o,
		q		=> AnswerTxStart_ES_o
		);
	AnswerTxStart <= AnswerTxStart_ES_o when TxRQ_SRFF_o = '1'
					else AnswerTxStart_tmp_o;

	Eth_Tx_End <= TxRQ_Reset_ES_o AND RxTxCycle_IntStart_o;				-- ?????????????? ???????-????????? ??????????? ??????
--	RxTxCycle_IntStart.(S,clk,R) = (RxIntStart_ES.q,Clock, TxRQ_Reset_ES.q);	-- ?????? ??? ???????? ?????? ?? ???????? ??????
	RxTxCycle_IntStart: entity work.SRFF 
		port map (
			S		=> RxIntStart_ES_o,
			CLK	=> Clock,
			R		=> TxRQ_Reset_ES_o,
			q		=> RxTxCycle_IntStart_o
			);

	-- MAC ????? ???????????? ? ???????????? ?????? ?? ???????? ????????? ?? Ethernet 
--	MAC_Decoder.Clock = Clock;
--	MAC_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset) 	 = (Buffer_RAM_Data()		 , RAM_Ptr_Packet_End OR RxIntStart_ES.q, SetRxReadyToRecive);
--	MAC_Decoder.(MAC_Addr0_(15 downto 8], MAC_Addr0_(7 downto 0]) = (Module_MAC_Reg(0].q(7 downto 0], Module_MAC_Reg(0].q(15 downto 8]);
--	MAC_Decoder.(MAC_Addr1_(15 downto 8], MAC_Addr1_(7 downto 0]) = (Module_MAC_Reg(1].q(7 downto 0], Module_MAC_Reg(1].q(15 downto 8]);
--	MAC_Decoder.(MAC_Addr2_(15 downto 8], MAC_Addr2_(7 downto 0]) = (Module_MAC_Reg(2].q(7 downto 0], Module_MAC_Reg(2].q(15 downto 8]);

	MAC_Decoder_Rx_Parcer_RQ <= RAM_Ptr_Packet_End OR RxIntStart_ES_o;
	MAC_Decoder: entity work.Eth_MAC_Decoder
		Port map(
			Clock				=> Clock, -- System Clock, really Bus_Clock

			Rx_Data			=> Buffer_RAM_Data,
			Rx_Addr			=> MAC_Decoder_Rx_Addr,
			Rx_Parcer_RQ	=> MAC_Decoder_Rx_Parcer_RQ,

			Tx_Addr			=> MAC_Decoder_Tx_Addr,
			Tx_Data			=> MAC_Decoder_Tx_Data,
			Tx_Word_Strobe	=> MAC_Decoder_Tx_Word_Strobe,

			MAC_Addr0_i(15 downto 8)	=> Module_MAC_Reg_o(0)(7 downto 0),
			MAC_Addr0_i(7 downto 0)		=> Module_MAC_Reg_o(0)(15 downto 8),
			MAC_Addr1_i(15 downto 8)	=> Module_MAC_Reg_o(1)(7 downto 0),
			MAC_Addr1_i(7 downto 0)		=> Module_MAC_Reg_o(1)(15 downto 8),
			MAC_Addr2_i(15 downto 8)	=> Module_MAC_Reg_o(2)(7 downto 0),
			MAC_Addr2_i(7 downto 0)		=> Module_MAC_Reg_o(2)(15 downto 8),

			Reset							=> SetRxReadyToRecive,

			Rx_Error_MAC				=> MAC_Decoder_Rx_Error_MAC,
			Next_Parcer					=> MAC_Decoder_Next_Parcer,
			Rx_Parcer_in_progress	=> MAC_Rx_Parcer_in_progress
			);

	-- ????? MAC-?????? ?????????? ??????????? ???????
--	FOR i IN 0 TO 2 GENERATE  
--		Source_MAC_Reg(i].(data[],clock, load, enable) = (RxRAM.q_b[],Clock,VCC, (Rx_RAM_Address_Bus() == 3+i) AND MAC_Decoder_Tx_Word_Strobe);
--	END GENERATE;
	Source_MAC_Reg_i: for i in 0 to 2 generate
		Source_MAC_Reg_en(i) <= '1' when (Rx_RAM_Address_Bus = 3+i) AND (MAC_Decoder_Tx_Word_Strobe = '1')
								else '0';
		Source_MAC_Reg: entity work.ShiftReg
			generic map (WIDTH => WORD_WIDTH) 
			port map(
					clock	=> Clock,
					data	=> RxRAM_q_b,
					enable=> Source_MAC_Reg_en(i),
					q		=> Source_MAC_Reg_o(i)
			);
	end generate;
	-- ??????????? ????? ???????? ????(???????????? 4 ????? CRC) ? ????????????? ? 16?????? ?????
--	IF RxTxCycle_IntStart.q == VCC THEN
--		Rx_Packet_Lenght_Reg.(data(15 downto RxByte_Cnt_Width],data(RxByte_Cnt_Width-1 downto 0]) = (B"0000",RxByte_Cnt.q(RxByte_Cnt_Width downto 1]-(HEADER_LENGTH_WORDS-1));
--							  ELSE
--		Rx_Packet_Lenght_Reg.(data(15 downto RxByte_Cnt_Width],data(RxByte_Cnt_Width-1 downto 0]) = (B"0000",RxByte_Cnt.q(RxByte_Cnt_Width downto 1]-(HEADER_LENGTH_WORDS-1)-3);
--	END IF;
--	Rx_Packet_Lenght_Reg.(clock,enable,load) = (Clock,RAM_Ptr_Packet_End OR RxIntStart_node,VCC); 

	Rx_Packet_Lenght_Reg_i <= (B"0000" & (RxByte_Cnt_o(RxByte_Cnt_Width downto 1)-(HEADER_LENGTH_WORDS-1))) when RxTxCycle_IntStart_o = '1' 
								else (B"0000" & (RxByte_Cnt_o(RxByte_Cnt_Width downto 1)-(HEADER_LENGTH_WORDS-1)-3));
	Rx_Packet_Lenght_Reg_en <= RAM_Ptr_Packet_End OR RxIntStart_node;
	Rx_Packet_Lenght_Reg: entity work.ShiftReg
		generic map (WIDTH => WORD_WIDTH) 
		port map(
				clock	=> Clock,
				data	=> Rx_Packet_Lenght_Reg_i,
				enable=> Rx_Packet_Lenght_Reg_en,
				q		=> Rx_Packet_Lenght_Reg_o
		);

--	RxWordRecive_Reg.(data(15 downto RxByte_Cnt_Width],data(RxByte_Cnt_Width-1 downto 0]) = (B"0000",RxByte_Cnt.q(RxByte_Cnt_Width downto 1]);
--	RxWordRecive_Reg.(clock,enable,load) = (Clock,RAM_Ptr_Packet_End OR RxIntStart_node,VCC); 
	RxWordRecive_Reg_en <= RAM_Ptr_Packet_End OR RxIntStart_node;
	RxWordRecive_Reg: entity work.ShiftReg
		generic map (WIDTH => WORD_WIDTH) 
		port map(
			clock	=> Clock,
			data(RxByte_Cnt_Width-1 downto 0)				=> RxByte_Cnt_o(RxByte_Cnt_Width downto 1),
			data(WORD_WIDTH-1 downto RxByte_Cnt_Width)	=> (others => '0'),
			enable=> RxWordRecive_Reg_en,
			q		=> RxWordRecive_Reg_o
		);

--	ARP_Decoder.Clock = Clock;
--	ARP_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset) = (Buffer_RAM_Data[],  MAC_Decoder.Next_Parcer, SetRxReadyToRecive);
--	ARP_Decoder.(MAC_Addr0_(15 downto 8], MAC_Addr0_(7 downto 0]) = (Module_MAC_Reg(0].q(7 downto 0], Module_MAC_Reg(0].q(15 downto 8]);
--	ARP_Decoder.(MAC_Addr1_(15 downto 8], MAC_Addr1_(7 downto 0]) = (Module_MAC_Reg(1].q(7 downto 0], Module_MAC_Reg(1].q(15 downto 8]);
--	ARP_Decoder.(MAC_Addr2_(15 downto 8], MAC_Addr2_(7 downto 0]) = (Module_MAC_Reg(2].q(7 downto 0], Module_MAC_Reg(2].q(15 downto 8]);
--	ARP_Decoder.(IP_Addr0_(15 downto 8], IP_Addr0_(7 downto 0]) = (Module_IP_Reg(0].q(7 downto 0], Module_IP_Reg(0].q(15 downto 8]);
--	ARP_Decoder.(IP_Addr1_(15 downto 8], IP_Addr1_(7 downto 0]) = (Module_IP_Reg(1].q(7 downto 0], Module_IP_Reg(1].q(15 downto 8]);
	ARP_Decoder: entity work.Eth_ARP_Decoder
		Port map(
			Clock				=> Clock, -- System Clock, really Bus_Clock

			Rx_Data			=> Buffer_RAM_Data,
			Rx_Addr			=> ARP_Decoder_Rx_Addr,
			Rx_Parcer_RQ	=> MAC_Decoder_Next_Parcer,

			Tx_Addr			=> ARP_Decoder_Tx_Addr,
			Tx_Data			=> ARP_Decoder_Tx_Data,
			Tx_Word_Strobe	=> ARP_Decoder_Tx_Word_Strobe,

			MAC_Addr0_i(15 downto 8)=> Module_MAC_Reg_o(0)(7 downto 0),
			MAC_Addr0_i(7 downto 0)	=> Module_MAC_Reg_o(0)(15 downto 8),
			MAC_Addr1_i(15 downto 8)=> Module_MAC_Reg_o(1)(7 downto 0),
			MAC_Addr1_i(7 downto 0)	=> Module_MAC_Reg_o(1)(15 downto 8),
			MAC_Addr2_i(15 downto 8)=> Module_MAC_Reg_o(2)(7 downto 0),
			MAC_Addr2_i(7 downto 0)	=> Module_MAC_Reg_o(2)(15 downto 8),
			IP_Addr0_i(15 downto 8)	=> Module_IP_Reg_o(0)(7 downto 0),
			IP_Addr0_i(7 downto 0)	=> Module_IP_Reg_o(0)(15 downto 8),
			IP_Addr1_i(15 downto 8)	=> Module_IP_Reg_o(1)(7 downto 0),
			IP_Addr1_i(7 downto 0)	=> Module_IP_Reg_o(1)(15 downto 8),

			Reset							=> SetRxReadyToRecive,

			Rx_IP_Error					=> ARP_Decoder_Rx_IP_Error,
			Rx_NOT_RQ					=> ARP_Decoder_Rx_NOT_RQ,
			Tx_Start						=> ARP_Decoder_Tx_Start,
			Rx_TRUE_RQ					=> ARP_Decoder_Rx_TRUE_RQ,
			Rx_Parcer_in_progress	=> ARP_Decoder_Rx_Parcer_in_progress,
			Test							=> ARP_Decoder_Test
			);

--	IPv4_Decoder.Clock = Clock;
--	IPv4_Decoder.Rx_NUM_Data() = RxWordRecive_Reg.q(RxByte_Cnt_Width-2 downto 0]-2;
--	IPv4_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[],  ARP_Decoder.Rx_NOT_RQ, SetRxReadyToRecive);
--	IPv4_Decoder.(IP_Addr0_(15 downto 8], IP_Addr0_(7 downto 0]) = (Module_IP_Reg(0].q(7 downto 0], Module_IP_Reg(0].q(15 downto 8]);
--	IPv4_Decoder.(IP_Addr1_(15 downto 8], IP_Addr1_(7 downto 0]) = (Module_IP_Reg(1].q(7 downto 0], Module_IP_Reg(1].q(15 downto 8]);
--	IPv4_Decoder.(Port(15 downto 8], Port(7 downto 0]) 			= (Port_Reg.q(7 downto 0], Port_Reg.q(15 downto 8]);
--	IPv4_Decoder.DataBus_In() = DataBus_In();
--	IPv4_Decoder.(AccessGranted, DataBusStrobe) = (AccessGranted, DataBusStrobe);
	IPv4_Decoder: entity work.Eth_IPv4_Decoder
		Port map(
			Clock				=> Clock, -- System Clock, really Bus_Clock

			Rx_Data			=> Buffer_RAM_Data,
			Rx_Addr			=> IPv4_Decoder_Rx_Addr,
			Rx_Parcer_RQ	=> ARP_Decoder_Rx_NOT_RQ,
			Rx_NUM_Data		=> RxWordRecive_Reg_o(RxByte_Cnt_Width-2 downto 0)-2,

			Tx_Addr			=> IPv4_Decoder_Tx_Addr,
			Tx_Data			=> IPv4_Decoder_Tx_Data,
			Tx_Word_Strobe	=> IPv4_Decoder_Tx_Word_Strobe,

			IP_Addr0_i(15 downto 8)	=> Module_IP_Reg_o(0)(7 downto 0),
			IP_Addr0_i(7 downto 0)	=> Module_IP_Reg_o(0)(15 downto 8),
			IP_Addr1_i(15 downto 8)	=> Module_IP_Reg_o(1)(7 downto 0),
			IP_Addr1_i(7 downto 0)	=> Module_IP_Reg_o(1)(15 downto 8),
			Port_i(15 downto 8)		=> Port_Reg_o(7 downto 0),
			Port_i(7 downto 0)		=> Port_Reg_o(15 downto 8),
			
			Identification				=> IPv4_Decoder_Identification,

			Reset							=> SetRxReadyToRecive,

			Rx_Error_IP					=> IPv4_Decoder_Rx_Error_IP,
			Rx_NOT_RQ					=> IPv4_Decoder_Rx_NOT_RQ,
			Tx_Start						=> IPv4_Decoder_Tx_Start,
			Rx_TRUE_RQ					=> IPv4_Decoder_Rx_TRUE_RQ,
			Rx_Parcer_in_progress	=> IPv4_Decoder_Rx_Parcer_in_progress,
			
			AccessRequest				=> IPv4_Decoder_AccessRequest,
			AccessGranted				=> AccessGranted,
			DirectOut					=> IPv4_Decoder_DirectOut,
			AddrBusOut					=> IPv4_Decoder_AddrBusOut,
			DataBus_In					=> DataBus_In,  -- ???????????? ? ?????????? ???? ? ?????? ??????
			DataBusStrobe				=> DataBusStrobe,  -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)

			test							=> IPv4_Decoder_test
			);
			
--	IPv4_CheckSum_Ctrl.Clock = Clock;
--	IPv4_CheckSum_Ctrl.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[], IPv4_Decoder.Tx_Start, SetRxReadyToRecive);
--	IPv4_CheckSum_Ctrl.IP_ID() = IPv4_Decoder.Identification();
	IPv4_CheckSum_Ctrl: entity work.IPv4_CheckSum
		Port map(
			Clock				=> Clock, -- System Clock, really Bus_Clock

			Rx_Data			=> Buffer_RAM_Data,
			Rx_Addr			=> IPv4_CheckSum_Ctrl_Rx_Addr,
			Rx_Parcer_RQ	=> IPv4_Decoder_Tx_Start,

			Tx_Addr			=> IPv4_CheckSum_Ctrl_Tx_Addr,
			Tx_Data			=> IPv4_CheckSum_Ctrl_Tx_Data,
			Tx_Word_Strobe	=> IPv4_CheckSum_Ctrl_Tx_Word_Strobe,

			Reset				=> SetRxReadyToRecive,

			IPv4_CheckSum_Complete	=> IPv4_CheckSum_Ctrl_Complete,

			IP_ID							=> IPv4_Decoder_Identification

			-----------------
--			Sample_Enable			=> IPv4_CheckSum_Ctrl_Sample_Enable,
--			Sum20_Reg_out			=> IPv4_CheckSum_Ctrl_Sum20_Reg_out,
--			Sum16_Reg_out			=> IPv4_CheckSum_Ctrl_Sum16_Reg_out,
--			RxParcerActive_out	=> IPv4_CheckSum_Ctrl_RxParcerActive_out
			);

--	CCCD_Decoder.Clock = Clock;
--	CCCD_Decoder.Rx_NUM_Data() = RxWordRecive_Reg.q(RxByte_Cnt_Width-2 downto 0);
--	CCCD_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[],  IPv4_Decoder.Rx_NOT_RQ, SetRxReadyToRecive);
--	CCCD_Decoder.Rx_Packet_Lenght()  = Rx_Packet_Lenght_Reg.q[];--(RxWordRecive_Reg.q[]-(HEADER_LENGTH_WORDS-1));--42);
--	CCCD_Decoder.DataBus_In() = DataBus_In();
--	CCCD_Decoder.(AccessGranted, DataBusStrobe) = (AccessGranted, DataBusStrobe);
	CCCD_Decoder: entity work.Eth_CCCD_Decoder
		Port map(
			Clock							=> Clock, -- System Clock, really Bus_Clock

			Rx_Data						=> Buffer_RAM_Data,
			Rx_Addr						=> CCCD_Decoder_Rx_Addr,
			Rx_Parcer_RQ				=> IPv4_Decoder_Rx_NOT_RQ,
			Rx_NUM_Data					=> RxWordRecive_Reg_o(RxByte_Cnt_Width-2 downto 0)-2,

			Tx_Addr						=> CCCD_Decoder_Tx_Addr,
			Tx_Data						=> CCCD_Decoder_Tx_Data,
			Tx_Word_Strobe				=> CCCD_Decoder_Tx_Word_Strobe,

			Rx_Error						=> CCCD_Decoder_Rx_Error_IP,
			Tx_Start						=> CCCD_Decoder_Tx_Start,
			Rx_Parcer_in_progress	=> CCCD_Decoder_Rx_Parcer_in_progress,
			Rx_TRUE_RQ					=> CCCD_Decoder_Rx_TRUE_RQ,
			Rx_NOT_RQ					=> CCCD_Decoder_Rx_NOT_RQ,
			
			Identification				=> CCCD_Decoder_Identification,
			
			Rx_Packet_Lenght			=> Rx_Packet_Lenght_Reg_o,
			
			AccessRequest				=> CCCD_Decoder_AccessRequest,
			AccessGranted				=> AccessGranted,
			DirectOut					=> CCCD_Decoder_DirectOut,
			AddrBusOut					=> CCCD_Decoder_AddrBusOut,
			DataBus_In					=> DataBus_In,  -- ???????????? ? ?????????? ???? ? ?????? ??????
			DataBusStrobe				=> DataBusStrobe,  -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)
			
			Reset							=> SetRxReadyToRecive,
			
			test							=> CCCD_Decoder_test
			);
	
--	Raw_Decoder.Clock = Clock;
--	Raw_Decoder.Rx_NUM_Data() = RxWordRecive_Reg.q(RxByte_Cnt_Width-2 downto 0);
--	Raw_Decoder.(Rx_Data[], Rx_Parcer_RQ, Reset)    = (Buffer_RAM_Data[], CCCD_Decoder.Rx_NOT_RQ, SetRxReadyToRecive);
--	Raw_Decoder.Rx_Packet_Lenght()  = Rx_Packet_Lenght_Reg.q[];--(RxWordRecive_Reg.q[]-(HEADER_LENGTH_WORDS-1));--42);
--	Raw_Decoder.DIMA_Ident() = Test_Reg.q();
--	Raw_Decoder.DataBus_In() = DataBus_In();
--	Raw_Decoder.(AccessGranted, DataBusStrobe) = (AccessGranted, DataBusStrobe);
	Raw_Decoder: entity work.Eth_Raw_Decoder
		Port map(
			Clock							=> Clock, -- System Clock, really Bus_Clock

			Rx_Data						=> Buffer_RAM_Data,
			Rx_Addr						=> Raw_Decoder_Rx_Addr,
			Rx_Parcer_RQ				=> CCCD_Decoder_Rx_NOT_RQ,
			Rx_NUM_Data					=> RxWordRecive_Reg_o(RxByte_Cnt_Width-2 downto 0)-2,

			Tx_Addr						=> Raw_Decoder_Tx_Addr,
			Tx_Data						=> Raw_Decoder_Tx_Data,
			Tx_Word_Strobe				=> Raw_Decoder_Tx_Word_Strobe,

			Rx_Error						=> Raw_Decoder_Rx_Error_IP,
			Tx_Start						=> Raw_Decoder_Tx_Start,
			Rx_Parcer_in_progress	=> Raw_Decoder_Rx_Parcer_in_progress,
			Rx_TRUE_RQ					=> Raw_Decoder_Rx_TRUE_RQ,
			Rx_NOT_RQ					=> Raw_Decoder_Rx_NOT_RQ,
			
			Identification				=> Raw_Decoder_Identification,
			
			Rx_Packet_Lenght			=> Rx_Packet_Lenght_Reg_o,
			DIMA_Ident					=> Test_Reg_o,
			
			AccessRequest				=> Raw_Decoder_AccessRequest,
			AccessGranted				=> AccessGranted,
			DirectOut					=> Raw_Decoder_DirectOut,
			AddrBusOut					=> Raw_Decoder_AddrBusOut,
			DataBus_In					=> DataBus_In,  -- ???????????? ? ?????????? ???? ? ?????? ??????
			DataBusStrobe				=> DataBusStrobe,  -- ????? ??????/???????? ?????? ?????? (??????? ???????, ??????????? ?? ??????? ??????)
			
			Reset							=> SetRxReadyToRecive,
			
			test							=> Raw_Decoder_test
			);

	AccessRequest	<= IPv4_Decoder_AccessRequest	 OR Raw_Decoder_AccessRequest OR CCCD_Decoder_AccessRequest;
	DirectOut_tmp	<= IPv4_Decoder_DirectOut		 OR Raw_Decoder_DirectOut		OR CCCD_Decoder_DirectOut;
	DirectOut		<= DirectOut_tmp;
	AddrBusOut		<= IPv4_Decoder_AddrBusOut		 OR Raw_Decoder_AddrBusOut		OR CCCD_Decoder_AddrBusOut;

--	IF(ParcerCycle_SRFF.q == VCC) THEN 
--		FOR i IN 0 TO RxByte_Cnt_Width-2 GENERATE  
--			Tx_RAM_Address_Bus(i) = DFF(.d=MAC_Decoder.Tx_Addr(i) OR ARP_Decoder.Tx_Addr(i) OR IPv4_Decoder.Tx_Addr(i) OR Raw_Decoder.Tx_Addr(i) OR IPv4_CheckSum_Ctrl.Tx_Addr(i) OR CCCD_Decoder.Tx_Addr(i],.clk=Clock);
--		END GENERATE;
--		Rx_RAM_Address_Bus() = MAC_Decoder.Rx_Addr() OR ARP_Decoder.Rx_Addr() OR IPv4_Decoder.Rx_Addr() OR Raw_Decoder.Rx_Addr() OR IPv4_CheckSum_Ctrl.Rx_Addr() OR CCCD_Decoder.Rx_Addr();
--		FOR i IN 0 TO 15 GENERATE 
--			Tx_RAM_Data_Bus(i) = DFF(.d=MAC_Decoder.Tx_Data(i) OR ARP_Decoder.Tx_Data(i) OR IPv4_Decoder.Tx_Data(i) OR Raw_Decoder.Tx_Data(i) OR IPv4_CheckSum_Ctrl.Tx_Data(i) OR CCCD_Decoder.Tx_Data(i],.clk=Clock);
--		END GENERATE;
--		TxRAM_wren				= DFF(.d=MAC_Decoder.Tx_Word_Strobe OR ARP_Decoder.Tx_Word_Strobe OR IPv4_Decoder.Tx_Word_Strobe OR Raw_Decoder.Tx_Word_Strobe OR IPv4_CheckSum_Ctrl.Tx_Word_Strobe OR CCCD_Decoder.Tx_Word_Strobe,.clk=Clock);
--	ELSE 
--		Tx_RAM_Address_Bus(RxByte_Cnt_Width-2 downto 0) = AddrBus_In(RxByte_Cnt_Width-2 downto 0);
--		Tx_RAM_Data_Bus(15 downto 8)	= DataBus_In(7 downto 0); Tx_RAM_Data_Bus(7 downto 0) = DataBus_In(15 downto 8);
--		TxRAM_wren							= TxRAM_CS AND DataBusStrobe AND DirectIn AND Select;
--		Rx_RAM_Address_Bus()				= AddrBus_In(RxByte_Cnt_Width-2 downto 0);
--	END IF;
	process (clock, ParcerCycle_SRFF_o)
	begin
		IF(ParcerCycle_SRFF_o = '1') THEN 
			RxByte_Cnt_Width_i: for i in 0 to RxByte_Cnt_Width-2 loop
				Tx_RAM_Address_Bus(i) <= MAC_Decoder_Tx_Addr(i) OR ARP_Decoder_Tx_Addr(i) OR IPv4_Decoder_Tx_Addr(i) OR Raw_Decoder_Tx_Addr(i) OR IPv4_CheckSum_Ctrl_Tx_Addr(i) OR CCCD_Decoder_Tx_Addr(i);
			end loop;
			Rx_RAM_Address_Bus <= MAC_Decoder_Rx_Addr OR ARP_Decoder_Rx_Addr OR IPv4_Decoder_Rx_Addr OR Raw_Decoder_Rx_Addr OR IPv4_CheckSum_Ctrl_Rx_Addr OR CCCD_Decoder_Rx_Addr;
			for i in 0 to 15 loop 
				Tx_RAM_Data_Bus(i) <= MAC_Decoder_Tx_Data(i) OR ARP_Decoder_Tx_Data(i) OR IPv4_Decoder_Tx_Data(i) OR Raw_Decoder_Tx_Data(i) OR IPv4_CheckSum_Ctrl_Tx_Data(i) OR CCCD_Decoder_Tx_Data(i);
			end loop;
			TxRAM_wren <= MAC_Decoder_Tx_Word_Strobe OR ARP_Decoder_Tx_Word_Strobe OR IPv4_Decoder_Tx_Word_Strobe OR Raw_Decoder_Tx_Word_Strobe OR IPv4_CheckSum_Ctrl_Tx_Word_Strobe OR CCCD_Decoder_Tx_Word_Strobe;
			ELSE 
				Tx_RAM_Address_Bus(RxByte_Cnt_Width-2 downto 0) <= AddrBus_In(RxByte_Cnt_Width-2 downto 0);
				Tx_RAM_Data_Bus(15 downto 8) <= DataBus_In(7 downto 0); 
				Tx_RAM_Data_Bus(7 downto 0)  <= DataBus_In(15 downto 8);
				TxRAM_wren <= TxRAM_CS AND DataBusStrobe AND DirectIn AND Select_i;
				Rx_RAM_Address_Bus <= AddrBus_In(RxByte_Cnt_Width-2 downto 0);
		END IF;
	end process;
	--------------------------------------- BUS Section ---------------------------------------------------   

	--************************************************************************* 
	-- ?????? ???????? ???????? ? ???? ???????? ?????? ??? ? ?????? ?????????? ?????? ??????
	process (Select_i, DirectOut_tmp, ParcerCycle_SRFF_o)
	begin
		SelDirPar <= Select_i & DirectOut_tmp & ParcerCycle_SRFF_o;
		CASE (SelDirPar) IS          
			WHEN b"100"  => DataBusOut					 <= DataBusOut_tmp; -- ???? ?????? ?? ????????? ????
			WHEN b"110"  => DataBusOut					 <= DataBusOut_tmp; -- ???? ?????? ?? ????????? ????
			WHEN b"011"  => DataBusOut(15 downto 8) <= Buffer_RAM_Data(7 downto 0);  -- ???? ?????? ???????????? ??????? ?? Ethernet
								 DataBusOut(7 downto 0)  <= Buffer_RAM_Data(15 downto 8); 
			WHEN b"111"  => DataBusOut(15 downto 8) <= Buffer_RAM_Data(7 downto 0);  -- ???? ?????? ???????????? ??????? ?? Ethernet
								 DataBusOut(7 downto 0)  <= Buffer_RAM_Data(15 downto 8); 
			WHEN b"101"  => DataBusOut					 <= DataBusOut_tmp;
			WHEN OTHERS  => DataBusOut					 <= (others =>'0'); 
		END CASE;
	end process;

	TxRAM_CS <= '0';

	process (Clock)
	begin
		IF ((AddrBus_In >= 0) AND (AddrBus_In < 2048)) THEN 
			DataBusOut_tmp(7 downto 0) <= RxRAM_q_b(15 downto 8); DataBusOut_tmp(15 downto 8) <= RxRAM_q_b(7 downto 0);   --DataBusOut() = RxRAM.q_b(); 
			RxRAM_CS <= '1';    
		ELSE 
			RxRAM_CS <= '0'; 
		END IF;

		IF (AddrBus_In = 2048) THEN 
			DataBusOut_tmp <= Status_REG_o;
			Status_REG_CS <= '1';  
		ELSE 
			Status_REG_CS <= '0'; 
		END IF;

		IF (AddrBus_In = 2049) 
			THEN SetRxReadyToRecive_Sc_Bus <= DataBusStrobe; 
			ELSE SetRxReadyToRecive_Sc_Bus <= '0'; 
		END IF;

		IF (AddrBus_In = 2050) 
			THEN InternalTxStart <= DataBusStrobe;
			ELSE InternalTxStart <= '0';
		END IF;

		IF (AddrBus_In = 2051) THEN 
			DataBusOut_tmp <= RxByte_Cnt_Reg_o;--PacketLenghts_to_be_transmitted_Reg.q();
			PacketLenghts_to_be_transmitted_Reg_CS <= '1';
		ELSE 
			PacketLenghts_to_be_transmitted_Reg_CS <= '0';
		END IF;

		IF (AddrBus_In = 2052) THEN 
			DataBusOut_tmp <= RxLostPacket_Cnt_REG_o; -- ????? ?? ???????? ??????? ?? ????? ????????? ????????
			RxLostPacket_Cnt_REG_CS <= '1';
		ELSE 
			RxLostPacket_Cnt_REG_CS <= '0';

		END IF;
		  -- MAC-????? ? IP-????? ????????
		FOR i IN 0 TO 2 loop  -- MAC-?????
			IF (AddrBus_In = 2053+i) THEN 
				DataBusOut_tmp <= Module_MAC_Reg_o(i);
				Module_MAC_Reg_CS(i) <= '1';
			ELSE 
				Module_MAC_Reg_CS(i) <= '0';
			END IF;
		END loop;
		FOR i IN 0 TO 1 loop  --IP-????? ????????
			IF (AddrBus_In = 2056+i) THEN 
				DataBusOut_tmp <= Module_IP_Reg_o(i);
				Module_IP_Reg_CS(i) <= '1';
			ELSE 
				Module_IP_Reg_CS(i) <= '0';
			END IF;
		END loop;

		-- MAC-????? ?????????? ???????????? ??????? ??????????
		IF (AddrBus_In = 2058) THEN DataBusOut_tmp(7 downto 0) <= Source_MAC_Reg_o(0)(15 downto 8); DataBusOut_tmp(15 downto 8) <= Source_MAC_Reg_o(0)(7 downto 0); END IF;
		IF (AddrBus_In = 2059) THEN DataBusOut_tmp(7 downto 0) <= Source_MAC_Reg_o(1)(15 downto 8); DataBusOut_tmp(15 downto 8) <= Source_MAC_Reg_o(1)(7 downto 0); END IF;
		IF (AddrBus_In = 2060) THEN DataBusOut_tmp(7 downto 0) <= Source_MAC_Reg_o(2)(15 downto 8); DataBusOut_tmp(15 downto 8) <= Source_MAC_Reg_o(2)(7 downto 0); END IF;

		-- Port ????????
		IF (AddrBus_In = 2061) THEN 
			DataBusOut_tmp <= Port_Reg_o;--DataBusOut(7 downto 0) = Port_Reg.q(15 downto 8); DataBusOut(15 downto 8) = Port_Reg.q(7 downto 0);
			Port_Reg_CS <= '1';
		ELSE 
			Port_Reg_CS <= '0';
		END IF;

		IF (AddrBus_In = 2062) THEN 
			DataBusOut_tmp <= Test_Reg_o;
			Test_Reg_CS <= '1';
		ELSE 
			Test_Reg_CS <= '0';
		END IF;

		IF (AddrBus_In = 2063) THEN DataBusOut_tmp <= IPv4_Decoder_Identification; END IF;
		IF (AddrBus_In = 2064) THEN DataBusOut_tmp <= Raw_Decoder_Identification;  END IF;

		IF (AddrBus_In = 2065) THEN DataBusOut_tmp <= Rx_Packet_Cnt_o; END IF;
		IF (AddrBus_In = 2066) THEN DataBusOut_tmp <= Tx_Packet_Cnt_o; END IF;
		IF (AddrBus_In = 2067) THEN DataBusOut_tmp <= (others => '0'); 
											 Rx_Packet_Cnt_Reset <= DataBusStrobe AND DirectIn AND Select_i; 
									  ELSE Rx_Packet_Cnt_Reset <= '0';
		END IF;
		IF (AddrBus_In = 2068) THEN DataBusOut_tmp <= (others => '0'); 
											 Tx_Packet_Cnt_Reset <= DataBusStrobe AND DirectIn AND Select_i;
									  ELSE Tx_Packet_Cnt_Reset <= '0';
		END IF;
		-- ????? ???? ???????????? ?? ???????? ??????
		IF (AddrBus_In = 2069) THEN DataBusOut_tmp <= conv_std_logic_vector(MASS_RAM_BYTE_Tx_Num, 16); END IF;

		IF ( (AddrBus_In >= 4096) AND (AddrBus_In <= 6143) ) THEN 
			DataBusOut_tmp(7 downto 0) <= RxIntRAM_q_b(15 downto 8); DataBusOut_tmp(15 downto 8) <= RxIntRAM_q_b(7 downto 0); 
			RxIntRAM_CS <= '1';    
		ELSE 
			RxIntRAM_CS <= '0';     
		END IF;
	end process;
	--***************************************************************************

	-- MAC-????? ? IP-????? ????????
	Module_MAC_Reg: FOR i IN 0 TO 2 GENERATE 
--	Module_MAC_Reg(i).(data[],clock, load, enable) = (DataBus_In,Clock,'1', Module_MAC_Reg_CS(i) AND DataBusStrobe AND DirectIn AND Select_i);
		Module_MAC_Reg_en(i) <= Module_MAC_Reg_CS(i) AND DataBusStrobe AND DirectIn AND Select_i;
		Module_MAC_Reg_i: entity work.ShiftReg
			generic map (WIDTH => 16) 
			port map(
				clock	=> Clock,
				data	=> DataBus_In,
				enable=> Module_MAC_Reg_en(i),
				q		=> Module_MAC_Reg_o(i)
			);
	END GENERATE;
	
	Module_IP_Reg: FOR i IN 0 TO 1 GENERATE  
--	Module_IP_Reg(i).(data[],clock, load, enable) = (DataBus_In,Clock,'1', Module_IP_Reg_CS(i) AND DataBusStrobe AND DirectIn AND Select_i);
		Module_IP_Reg_en <= Module_IP_Reg_CS(i) AND DataBusStrobe AND DirectIn AND Select_i;
		Module_IP_Reg_i: entity work.ShiftReg
			generic map (WIDTH => 16) 
			port map(
				clock	=> Clock,
				data	=> DataBus_In,
				enable=> Module_IP_Reg_en,
				q		=> Module_IP_Reg_o(i)
			);
	END GENERATE;
--	Port_Reg.(data[],clock, load, enable) = (DataBus_In[],Clock,VCC, Port_Reg_CS AND DataBusStrobe AND DirectIn AND Select);
	Port_Reg_en <= Port_Reg_CS AND DataBusStrobe AND DirectIn AND Select_i;
	Port_Reg: entity work.ShiftReg
		generic map (WIDTH => 16) 
		port map(
			clock	=> Clock,
			data	=> DataBus_In,
			enable=> Port_Reg_en,
			q		=> Port_Reg_o
		);
--	Test_Reg.(data[],clock, load, enable) = (DataBus_In[],Clock,VCC, Test_Reg_CS AND DataBusStrobe AND DirectIn AND Select);
	Test_Reg_en <= Test_Reg_CS AND DataBusStrobe AND DirectIn AND Select_i;
	Test_Reg: entity work.ShiftReg
		generic map (WIDTH => 16) 
		port map(
			clock	=> Clock,
			data	=> DataBus_In,
			enable=> Test_Reg_en,
			q		=> Test_Reg_o
		);

--	Status_REG_ES.(d,clk) 						= (Status_REG_CS AND Select, Clock);
	Status_REG_ES_d <= Status_REG_CS AND Select_i;
	Status_REG_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> Status_REG_ES_d,
		q		=> Status_REG_ES_o
		);
--	Status_REG.(clock, load, enable)			= (BUS_Clock, VCC,  Status_REG_ES.q);
--	Status_REG.data(12 downto 0)				= RxByte_Cnt.q();
--	Status_REG.data(13)							= RxReadyToRecive.q;
--	Status_REG.data(14)							= '0';--IPv4_Decoder.Rx_NOT_RQ;--RxPacket_in_progress;
--	Status_REG.data(15)							= VCC;--Raw_Decoder.Tx_Start;--ARP_Decoder.Test;--ARP_Decoder.Rx_Error;%
	Status_REG: entity work.ShiftReg
		generic map (WIDTH => 16) 
		port map(
			clock		=> BUS_Clock,
			data(12 downto 0)	=> RxByte_Cnt_o,
			data(13)				=> RxReadyToRecive_o,
			data(14)				=> '0',--IPv4_Decoder.Rx_NOT_RQ;--RxPacket_in_progress;
			data(15)				=> '1',--Raw_Decoder.Tx_Start;--ARP_Decoder.Test;--ARP_Decoder.Rx_Error;%
			enable	=> Status_REG_ES_o,
			q			=> Status_REG_o
		);


	-- ?????????? ????? ???????????? ????
--	PacketLenghts_to_be_transmitted_Reg.(data[],clock, load, enable) = (PacketLenghts_DataBus[], BUS_Clock, VCC, PacketLenghts_to_be_transmitted_Reg_EN);
	PacketLenghts_to_be_transmitted_Reg: entity work.ShiftReg
		generic map (WIDTH => 16) 
		port map(
			clock	=> BUS_Clock,
			data	=> PacketLenghts_DataBus,
			enable=> PacketLenghts_to_be_transmitted_Reg_EN,
			q		=> PacketLenghts_to_be_transmitted_Reg_o
		);
	process (Select_i)
	begin
		IF(Select_i = '1') THEN 
			PacketLenghts_DataBus <= DataBus_In;  
			PacketLenghts_to_be_transmitted_Reg_EN <= (PacketLenghts_to_be_transmitted_Reg_CS AND DataBusStrobe AND DirectIn AND Select_i);
		ELSE 
			IF RxTxCycle_IntStart_o = '1' THEN
				PacketLenghts_DataBus(RxByte_Cnt_Width downto 0) <= (RxByte_Cnt_o - 1); -- ???????? 1 ???? ????? ?????? Packet_Good_End ??? Packet_bad_End
			ELSE
				PacketLenghts_DataBus(RxByte_Cnt_Width downto 0) <= (RxByte_Cnt_o - 7); -- ???????? 7 ???? 
			END IF;
			PacketLenghts_DataBus(15 downto RxByte_Cnt_Width+1) <= (others => '0');
			PacketLenghts_to_be_transmitted_Reg_EN <= AnswerTxStart;
		END IF; 
	end process;
--	RxByte_Cnt_Reg.data(RxByte_Cnt_Width downto 0)  = (RxByte_Cnt.q[]-1);
--	RxByte_Cnt_Reg.data(15 downto RxByte_Cnt_Width+1) = GND;
--	RxByte_Cnt_Reg.(clock, load, enable) = (Clock,'1',Edge_Sensing_Sync(.d=FIFO_to_RxRAM_Copy,.clk=Clock));
	RxByte_Cnt_Reg: entity work.ShiftReg
		generic map (WIDTH => 16) 
		port map(
			clock	=> BUS_Clock,
			data(RxByte_Cnt_Width downto 0) => CONV_STD_LOGIC_VECTOR((CONV_INTEGER(RxByte_Cnt_o) - 1),RxByte_Cnt_Width+1),
			data(15 downto RxByte_Cnt_Width+1) => (others =>'0'),
			enable=> FIFO_to_RxRAM_Copy_ES_o,
			q		=> RxByte_Cnt_Reg_o
		);
	FIFO_to_RxRAM_Copy_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> FIFO_to_RxRAM_Copy,
		q		=> FIFO_to_RxRAM_Copy_ES_o
		);

	------------ ???????? ?????????(????????? MAC-???????) ? ???????????? ???????
--	Rx_Packet_Cnt.(clock,cnt_en,sclr) = (BUS_Clock,Edge_Sensing_Sync(.d=MAC_Decoder.Next_Parcer,.clk=BUS_Clock),Rx_Packet_Cnt_Reset);  
	Rx_Packet_Cnt: entity work.V_Counter
	generic map(
		width => 16
		)
	port map (
		clock		=> BUS_Clock,--Quarts,--
		clk_en	=> '1',
		cnt_en	=> MAC_Decoder_Next_Parcer_ES_o,
		sclr		=> Rx_Packet_Cnt_Reset,
		q			=> Rx_Packet_Cnt_o
		);
	MAC_Decoder_Next_Parcer_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> MAC_Decoder_Next_Parcer,
		q		=> MAC_Decoder_Next_Parcer_ES_o
		);

--	Tx_Packet_Cnt.(clock,cnt_en,sclr) = (BUS_Clock,Edge_Sensing_Sync(.d=TxStart,.clk=BUS_Clock),Tx_Packet_Cnt_Reset);  
	Tx_Packet_Cnt: entity work.V_Counter
	generic map(
		width => 16
		)
	port map (
		clock		=> BUS_Clock,--Quarts,--
		clk_en	=> '1',
		cnt_en	=> TxStart_ES_o,
		sclr		=> Tx_Packet_Cnt_Reset,
		q			=> Tx_Packet_Cnt_o
		);
	TxStart_ES: entity work.Edge_Sensing
	Port map(
		clk	=> Clock,
		d		=> TxStart,
		q		=> TxStart_ES_o
		);

end Behavioral;