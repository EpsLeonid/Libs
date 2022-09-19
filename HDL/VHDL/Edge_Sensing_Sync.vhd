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

library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.parameters.all;

entity Edge_Sensing_Sync is
	Port (
		clk		: in std_logic;         -- Clock
		clr		: in std_logic := '0';  -- Common FPGA Reset
		d			: in std_logic;         -- External signal to be synchronized
		ena		: in std_logic := '1';
		q			: out std_logic        -- Synchronized out
	);
end Edge_Sensing_Sync;

architecture Behavioral of Edge_Sensing_Sync is

begin

end Behavioral;