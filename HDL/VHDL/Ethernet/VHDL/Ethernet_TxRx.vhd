----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 27/06/2022  
-- Design Name: 
-- Module Name: Ethernet_TxRx - Behavioral 
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

entity Ethernet_TxRx is
	Port (
		System_Clock				: in std_logic; -- System Clock, really Bus_Clock
-- PHY Ethernet I/O
	-- Rx section--Preambula, SOF and CRC are cutted out
		Eth_Phy_RxClk				: in std_logic;
		Rx_Reset						: in std_logic := '0';
		Carr 							: in std_logic;	-- Crs and RxDv; 
		Rx_Data_nibble_input		: in std_logic_vector (3 downto 0);
		RxIntStart					: in std_logic;

	-- Tx section
		Tx_Reset						: in std_logic := '0';	
		Eth_Phy_TxClk				: in std_logic;	--Eth0_Phy_TxClk
		Eth_Phy_TxEn				: out std_logic;	--Eth0_Phy_TxEn
		Eth_Phy_TxD					: out std_logic_vector (3 downto 0);	--Eth0_Phy_TxD(3 downto 0)
		Eth_Phy_MdC					: out std_logic;	--Eth0_Phy_MdC
		Eth_Phy_MdIO				: in std_logic;	--Eth0_Phy_MdIO
		 
		Eth_RxTx_In_Progress		: out std_logic; 
		Eth_Tx_End					: out std_logic; 
		Transmit_of_Data_RQ		: out std_logic; 

	-- Standard bus connections
		DataBus_i					: in std_logic_vector (15 downto 0);
		DataBus_o					: out std_logic_vector (15 downto 0); 

		DataBusStrobe 			 	: in std_logic;
		Select_i					 	: in std_logic;
		DirectIn						: in std_logic; 
		AddrBus_In					: in std_logic_vector (12 downto 0);
		Parcer_Reset				: in std_logic := '0';
	-- Master Mode Signals 
		AccessRequest				: out std_logic;
		AccessGranted				: in std_logic; 
		DirectOut					: out std_logic;
		AddrBusOut					: out std_logic_vector (15 downto 0);

		RxIntStart_out				: out std_logic
	);
end Ethernet_TxRx;

architecture Behavioral of Ethernet_TxRx is

	signal Byte_Output_Strobe				: std_logic;
	signal Rx_Byte_Output_Strobe			: std_logic;
	signal Byte_Output						: std_logic_vector(7 downto 0);
	signal Data_Frame_is_in_Progress		: std_logic;
	signal Rx_Data_Frame_is_in_Progress	: std_logic;
	signal Packet_Good_End					: std_logic;
	signal Packet_bad_End					: std_logic;
	signal Rx_Packet_Good_End				: std_logic;
	signal Rx_Packet_bad_End				: std_logic;
	signal Rx_Packet_End						: std_logic;
	signal Eth_Tx_In_Progress				: std_logic;
	signal Rx_Eth_Tx_In_Progress			: std_logic;

begin

--**************************************** Ethernet  ****************************************************	
------------------------------------- Ethernet Reciver ---------------------------------

--Eth_Rx_Signaling_to_Data_Layer_Coverter.System_Clock 					= 	System_Clock;	
--Eth_Rx_Signaling_to_Data_Layer_Coverter.RxClk_Edge_at_System_Clock		= 	Eth_Phy_RxClk;--Eth0_RxClk_Edge_at_System_Clock_DDR;
--
--Eth_Rx_Signaling_to_Data_Layer_Coverter.Reset						    = 	Rx_Reset;--GND;
--Eth_Rx_Signaling_to_Data_Layer_Coverter.Carr 							=	Carr;--Eth0_Rx_Data_Reg.q(5) AND Eth0_Rx_Data_Reg.q(4);		--Crs and RxDv; -- ?? ??????? ????? ??????? ???? ?????? ??????
--Eth_Rx_Signaling_to_Data_Layer_Coverter.Rx_Data_nibble_input(3 downto 0) 		= 	Rx_Data_nibble_input(3 downto 0);--Eth0_Rx_Data_Reg.q(3 downto 0);  

	Eth_Rx_Signaling_to_Data_Layer_Coverter : entity work.Fr_ether100_new
	port map (
		System_Clock							=> System_Clock,
		RxClk_Edge_at_System_Clock			=> Eth_Phy_RxClk,
		Reset										=> Rx_Reset,
		Carr										=> Carr,
		Rx_Data_nibble_input(3 downto 0)	=>Rx_Data_nibble_input(3 downto 0),

		Byte_Output_Strobe					=> Byte_Output_Strobe,
		Byte_Output(7 downto 0)				=> Byte_Output(7 downto 0),
		Data_Frame_is_in_Progress			=> Data_Frame_is_in_Progress,
		Packet_Good_End						=> Rx_Packet_Good_End,
		Packet_bad_End							=> Rx_Packet_bad_End
	);

  ------------------------------------ Ethernet Trasmitter ------------------------------
--Eth_Tx_Data_to_Signaling_Layer_Coverter.System_Clock					=	System_Clock;
--Eth_Tx_Data_to_Signaling_Layer_Coverter.Reset							=	Tx_Reset;--GND;
--Eth_Tx_Data_to_Signaling_Layer_Coverter.Transmit_of_Data_RQ				=	Eth_Data_Layer_Parser_Builder.Transmit_of_Data_RQ;	-- ?????? ????????
--Eth_Tx_Data_to_Signaling_Layer_Coverter.Data_to_Transmit(7 downto 0)			=	Eth_Data_Layer_Parser_Builder.Tx_Data(7 downto 0);   		-- ?????? ??? ????????
--
--Eth_Tx_Data_to_Signaling_Layer_Coverter.MII_Tx_CLK						=	Eth_Phy_TxClk;
--Eth_Phy_TxEn															=	Eth_Tx_Data_to_Signaling_Layer_Coverter.MII_Tx_En;
--Eth_Phy_TxD(3 downto 0)														=	Eth_Tx_Data_to_Signaling_Layer_Coverter.MII_Tx_Data(3 downto 0);
--Eth_Phy_MdC                                								=	GND;
	Eth_Tx_Data_to_Signaling_Layer_Coverter : entity work.Tx_Eth100_Sync
	port map (
		System_Clock						=> System_Clock,
		Tx_Reset								=> Tx_Reset,
		Transmit_of_Data_RQ				=> Eth_Data_Layer_Parser_Builder_Transmit_of_Data_RQ,
		Data_to_Transmit(7 downto 0)	=> Eth_Data_Layer_Parser_Builder_Tx_Data(7 downto 0),
	-- Phy MII connection
		MII_Tx_CLK							=> Eth_Phy_TxClk,
		MII_Tx_En							=> Eth_Phy_TxEn,
		MII_Tx_Data(3 downto 0)			=> Eth_Phy_TxD(3 downto 0),
		
	-- Ethernet Data_Layer_Parser 
		Eth_Tx_In_Progress				=> Eth_Tx_In_Progress
	);

	Eth_Phy_MdC								<=	'0';

----------------------------------  Ethernet Data_Layer_Parser ------------------------------- 
--Eth_Data_Layer_Parser_Builder.Clock 					= System_Clock;
--Eth_Data_Layer_Parser_Builder.Byte_Strobe_Rx			= DFF(.clk=System_Clock, .d=(Eth_Rx_Signaling_to_Data_Layer_Coverter.Byte_Output_Strobe));
--Eth_Data_Layer_Parser_Builder.Rx_Data(7 downto 0)				= Eth_Rx_Signaling_to_Data_Layer_Coverter.Byte_Output(7 downto 0);
--Eth_Data_Layer_Parser_Builder.RxPacket_in_progress		= DFF(.clk=System_Clock, .d=(Eth_Rx_Signaling_to_Data_Layer_Coverter.Data_Frame_is_in_Progress));
--Eth_Data_Layer_Parser_Builder.RxPacket_End				= DFF(.clk=System_Clock, .d=(Eth_Rx_Signaling_to_Data_Layer_Coverter.Packet_Good_End OR Eth_Rx_Signaling_to_Data_Layer_Coverter.Packet_bad_End));
--Eth_Data_Layer_Parser_Builder.Packet_Good_End			= DFF(.clk=System_Clock, .d=(Eth_Rx_Signaling_to_Data_Layer_Coverter.Packet_Good_End));
--Eth_Data_Layer_Parser_Builder.Packet_bad_End			= DFF(.clk=System_Clock, .d=(Eth_Rx_Signaling_to_Data_Layer_Coverter.Packet_bad_End));
--Eth_Data_Layer_Parser_Builder.Eth_Tx_In_Progress		= DFF(.clk=System_Clock, .d=(Eth_Tx_Data_to_Signaling_Layer_Coverter.Eth_Tx_In_Progress));
--
--Eth_Data_Layer_Parser_Builder.Byte_Strobe_Tx      		= Eth_Tx_Data_to_Signaling_Layer_Coverter.Byte_Readed_Strob;  
--
--Eth_Data_Layer_Parser_Builder.(BUS_Clock   , DataBus_In(15 downto 0), DataBusStrobe) =
--						  (System_Clock,   DataBus_In   , DataBusStrobe);
--Eth_Data_Layer_Parser_Builder.Reset			    		= Parcer_Reset;--GND;
--Eth_Data_Layer_Parser_Builder.DirectIn					= DirectIn;
--Eth_Data_Layer_Parser_Builder.AddrBus_In(12 downto 0)			= AddrBus_In(12 downto 0);
--Eth_Data_Layer_Parser_Builder.Select_i			    	= Select_i;
--
--Eth_Data_Layer_Parser_Builder.AccessGranted	    		= AccessGranted;
---- ?????????? ?????? ???????? ?????? ?? Ethernet 
--Eth_Data_Layer_Parser_Builder.RxIntStart           		= RxIntStart;
--
--Eth_RxTx_In_Progress		= Eth_Data_Layer_Parser_Builder.Eth_RxTx_In_Progress;
--DataBusOut					= Eth_Data_Layer_Parser_Builder.DataBusOut;
--AccessRequest				= Eth_Data_Layer_Parser_Builder.AccessRequest;
--DirectOut					= Eth_Data_Layer_Parser_Builder.DirectOut;
--AddrBusOut(15 downto 0)	= Eth_Data_Layer_Parser_Builder.AddrBusOut(15 downto 0);
--Eth_Tx_End					= Eth_Data_Layer_Parser_Builder.Eth_Tx_End;
--Transmit_of_Data_RQ		= Eth_Data_Layer_Parser_Builder.Transmit_of_Data_RQ;

---- test signals
--RxIntStart_out 			= Eth_Data_Layer_Parser_Builder.RxIntStart_out;

	process(System_Clock)
	begin
		Rx_Byte_Output_Strobe	<= Byte_Output_Strobe;
		Rx_Packet_Good_End		<= Packet_Good_End;
		Rx_Packet_bad_End			<= Packet_bad_End;
		Rx_Packet_End				<= (Packet_Good_End OR Packet_bad_End);
		Rx_Data_Frame_is_in_Progress <= Data_Frame_is_in_Progress;
		Tx_Eth_Tx_In_Progress	<= Eth_Tx_In_Progress;
	end process;

	Eth_Data_Layer_Parser_Builder : entity work.Eth_Up_Module
	port map (
		Clock 						=> System_Clock,
		Byte_Strobe_Rx				=> Rx_Byte_Output_Strobe,
		Rx_Data(7 downto 0)		=> Byte_Output(7 downto 0),
		RxPacket_in_progress		=> Rx_Data_Frame_is_in_Progress,
		RxPacket_End				=> Rx_Packet_End,
		Packet_Good_End			=> Rx_Packet_Good_End,
		Packet_bad_End				=> Rx_Packet_bad_End,
		Eth_Tx_In_Progress		=> Tx_Eth_Tx_In_Progress,

		Byte_Strobe_Tx				=> Byte_Readed_Strob,

		BUS_Clock					=> System_Clock,
		DataBus_In(15 downto 0)	=> DataBus_In,
		DataBusStrobe				=> DataBusStrobe,
		Reset							=> Parcer_Reset,
		DirectIn						=> DirectIn,
		AddrBus_In(12 downto 0)	=> AddrBus_In(12 downto 0),
		Select_i						=> Select_i,

		AccessGranted				=> AccessGranted,
		-- ?????????? ?????? ???????? ?????? ?? Ethernet 
		RxIntStart					=> RxIntStart,

		Eth_RxTx_In_Progress		=> Eth_RxTx_In_Progress,
		DataBusOut					=> DataBusOut,
		AccessRequest				=> AccessRequest,
		DirectOut					=> DirectOut,
		AddrBusOut(15 downto 0)	=> AddrBusOut(15 downto 0),
		Eth_Tx_End					=> Eth_Tx_End,
		Transmit_of_Data_RQ		=> Transmit_of_Data_RQ,

		-- test signals
		RxIntStart_out 			=> RxIntStart_out

	);

end Behavioral;