----------------------------------------------------------------------------------
-- Company: BINP
-- Engineer: Epshteyn Leonid
-- 
-- Create Date: 02/08/2022  
-- Design Name: 
-- Module Name: crc32eth4 - Behavioral 
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

entity crc32eth4 is
	Port (
		Data_i		: in std_logic_vector (3 downto 0); -- Incomming Data inputs.
		Clock			: in std_logic; -- Clock.
		reset			: in std_logic;
		enable		: in std_logic; -- Any activity enable.
		write_i		: in std_logic; -- Crc count enable (or simple shift per nibble)
		Data_o		: out std_logic_vector (31 downto 0) -- Current Crc checksum. 
		);
end crc32eth4;

architecture Behavioral of crc32eth4 is

	signal CRC		: std_logic_vector (31 downto 0);

begin

--	CRC[].(clk, ena) = (clock, enable or reset);

--	CRC(0).d = ((in(0]) and write_i xor CRC(28).q) or reset;
--	CRC(1).d = ((in(1) xor in(0) xor CRC(28).q) and write_i xor CRC(29).q) or reset;
--	CRC(2).d = ((in(2) xor in(1) xor in(0) xor CRC(28).q xor CRC(29).q) and write_i xor CRC(30).q) or reset;
--	CRC(3).d = ((in(3) xor in(2) xor in(1) xor CRC(29).q xor CRC(30).q) and write_i xor CRC(31).q) or reset;
--	CRC(4).d = ((in(3) xor in(2) xor in(0) xor CRC(28).q xor CRC(30).q xor CRC(31).q) and write_i xor CRC(0).q) or reset;
--	CRC(5).d = ((in(3) xor in(1) xor in(0) xor CRC(28).q xor CRC(29).q xor CRC(31).q) and write_i xor CRC(1).q) or reset;
--	CRC(6).d = ((in(2) xor in(1) xor CRC(29).q xor CRC(30).q) and write_i xor CRC(2).q) or reset;
--	CRC(7).d = ((in(3) xor in(2) xor in(0) xor CRC(28).q xor CRC(30).q xor CRC(31).q) and write_i xor CRC(3).q) or reset;
--	CRC(8).d = ((in(3) xor in(1) xor in(0) xor CRC(28).q xor CRC(29).q xor CRC(31).q) and write_i xor CRC(4).q) or reset;
--	CRC(9).d = ((in(2) xor in(1) xor CRC(29).q xor CRC(30]) and write_i xor CRC(5).q) or reset;
--	CRC(10).d = ((in(3) xor in(2) xor in(0) xor CRC(28).q xor CRC(30).q xor CRC(31).q) and write_i xor CRC(6).q) or reset;
--	CRC(11).d = ((in(3) xor in(1) xor in(0) xor CRC(28).q xor CRC(29).q xor CRC(31).q) and write_i xor CRC(7).q) or reset;
--	CRC(12).d = ((in(2) xor in(1) xor in(0) xor CRC(28).q xor CRC(29).q xor CRC(30).q) and write_i xor CRC(8).q) or reset;
--	CRC(13).d = ((in(3) xor in(2) xor in(1) xor CRC(29).q xor CRC(30).q xor CRC(31).q) and write_i xor CRC(9).q) or reset;
--	CRC(14).d = ((in(3) xor in(2) xor CRC(30).q xor CRC(31).q) and write_i xor CRC(10).q) or reset;
--	CRC(15).d = ((in(3) xor CRC(31).q) and write_i xor CRC(11).q) or reset;
--	CRC(16).d = ((in(0) xor CRC(28).q) and write_i xor CRC(12).q) or reset;
--	CRC(17).d = ((in(1) xor CRC(29).q) and write_i xor CRC(13).q) or reset;
--	CRC(18).d = ((in(2) xor CRC(30).q) and write_i xor CRC(14).q) or reset;
--	CRC(19).d = ((in(3) xor CRC(31).q) and write_i xor CRC(15).q) or reset;
--	CRC(20).d = (CRC(16).q) or reset;
--	CRC(21).d = (CRC(17).q) or reset;
--	CRC(22).d = ((in(0) xor CRC(28).q) and write_i xor CRC(18).q) or reset;
--	CRC(23).d = ((in(1) xor in(0) xor CRC(28).q xor CRC(29).q) and write_i xor CRC(19).q) or reset;
--	CRC(24).d = ((in(2) xor in(1) xor CRC(29).q xor CRC(30).q) and write_i xor CRC(20).q) or reset;
--	CRC(25).d = ((in(3) xor in(2) xor CRC(30).q xor CRC(31).q) and write_i xor CRC(21).q) or reset;
--	CRC(26).d = ((in(3) xor in(0) xor CRC(28).q xor CRC(31).q) and write_i xor CRC(22).q) or reset;
--	CRC(27).d = ((in(1) xor CRC(29).q) and write_i xor CRC(23).q) or reset;
--	CRC(28).d = ((in(2) xor CRC(30).q) and write_i xor CRC(24).q) or reset;
--	CRC(29).d = ((in(3) xor CRC(31).q) and write_i xor CRC(25).q) or reset;
--	CRC(30).d = (CRC(26).q) or reset;
--	CRC(31).d = (CRC(27).q) or reset;

process (Clock, reset)
begin
	if Clock'event and Clock='1' then 
		if reset = '1' then
			CRC <= (others => '1');
		elsif (enable = '1') then
			CRC(0) <= ((Data_i(0) and write_i) xor CRC(28));
			CRC(1) <= (((Data_i(1) xor Data_i(0) xor CRC(28)) and write_i) xor CRC(29));
			CRC(2) <= (((Data_i(2) xor Data_i(1) xor Data_i(0) xor CRC(28) xor CRC(29)) and write_i) xor CRC(30));
			CRC(3) <= (((Data_i(3) xor Data_i(2) xor Data_i(1) xor CRC(29) xor CRC(30)) and write_i) xor CRC(31));
			CRC(4) <= (((Data_i(3) xor Data_i(2) xor Data_i(0) xor CRC(28) xor CRC(30) xor CRC(31)) and write_i) xor CRC(0));
			CRC(5) <= (((Data_i(3) xor Data_i(1) xor Data_i(0) xor CRC(28) xor CRC(29) xor CRC(31)) and write_i) xor CRC(1));
			CRC(6) <= (((Data_i(2) xor Data_i(1) xor CRC(29) xor CRC(30)) and write_i) xor CRC(2));
			CRC(7) <= (((Data_i(3) xor Data_i(2) xor Data_i(0) xor CRC(28) xor CRC(30) xor CRC(31)) and write_i) xor CRC(3));
			CRC(8) <= (((Data_i(3) xor Data_i(1) xor Data_i(0) xor CRC(28) xor CRC(29) xor CRC(31)) and write_i) xor CRC(4));
			CRC(9) <= (((Data_i(2) xor Data_i(1) xor CRC(29) xor CRC(30)) and write_i) xor CRC(5));
			CRC(10) <= (((Data_i(3) xor Data_i(2) xor Data_i(0) xor CRC(28) xor CRC(30) xor CRC(31)) and write_i) xor CRC(6));
			CRC(11) <= (((Data_i(3) xor Data_i(1) xor Data_i(0) xor CRC(28) xor CRC(29) xor CRC(31)) and write_i) xor CRC(7));
			CRC(12) <= (((Data_i(2) xor Data_i(1) xor Data_i(0) xor CRC(28) xor CRC(29) xor CRC(30)) and write_i) xor CRC(8));
			CRC(13) <= (((Data_i(3) xor Data_i(2) xor Data_i(1) xor CRC(29) xor CRC(30) xor CRC(31)) and write_i) xor CRC(9));
			CRC(14) <= (((Data_i(3) xor Data_i(2) xor CRC(30) xor CRC(31)) and write_i) xor CRC(10));
			CRC(15) <= (((Data_i(3) xor CRC(31)) and write_i) xor CRC(11));
			CRC(16) <= (((Data_i(0) xor CRC(28)) and write_i) xor CRC(12));
			CRC(17) <= (((Data_i(1) xor CRC(29)) and write_i) xor CRC(13));
			CRC(18) <= (((Data_i(2) xor CRC(30)) and write_i) xor CRC(14));
			CRC(19) <= (((Data_i(3) xor CRC(31)) and write_i) xor CRC(15));
			CRC(20) <= (CRC(16));
			CRC(21) <= (CRC(17));
			CRC(22) <= (((Data_i(0) xor CRC(28)) and write_i) xor CRC(18));
			CRC(23) <= (((Data_i(1) xor Data_i(0) xor CRC(28) xor CRC(29)) and write_i) xor CRC(19));
			CRC(24) <= (((Data_i(2) xor Data_i(1) xor CRC(29) xor CRC(30)) and write_i) xor CRC(20));
			CRC(25) <= (((Data_i(3) xor Data_i(2) xor CRC(30) xor CRC(31)) and write_i) xor CRC(21));
			CRC(26) <= (((Data_i(3) xor Data_i(0) xor CRC(28) xor CRC(31)) and write_i) xor CRC(22));
			CRC(27) <= (((Data_i(1) xor CRC(29)) and write_i) xor CRC(23));
			CRC(28) <= (((Data_i(2) xor CRC(30)) and write_i) xor CRC(24));
			CRC(29) <= (((Data_i(3) xor CRC(31)) and write_i) xor CRC(25));
			CRC(30) <= (CRC(26));
			CRC(31) <= (CRC(27));
		end if;
	end if;
end process;

	Data_o(31 downto 0) <= not CRC(31 downto 0);

end Behavioral;