----------------------------------------------------------------------------------
-- Company: 
-- Engineer: 
-- 
-- Create Date:    15:44:10 08/23/2018 
-- Design Name: 
-- Module Name:    ShiftReg - Behavioral 
-- Project Name: 
-- Target Devices: 
-- Tool versions: 
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
--use IEEE.NUMERIC_STD.ALL;

-- Uncomment the following library declaration if instantiating
-- any Xilinx primitives in this code.
--library UNISIM;
--use UNISIM.VComponents.all;

entity ShiftReg is
	GENERIC (WIDTH		: integer := 16;
				DIRACTION: std_logic := '1'); 
	port(	clock	: in std_logic;
			enable: in std_logic := '1';
			data	: in std_logic_vector(WIDTH-1 downto 0);
			sset	: in std_logic := '0';
			svalue	: in std_logic_vector(WIDTH-1 downto 0);
			load	: in std_logic := '1';
			aclr	: in std_logic := '0';
			sclr	: in std_logic := '0';
			shiftout : out std_logic; 
			q		: out std_logic_vector(WIDTH-1 downto 0));
end ShiftReg;

architecture Behavioral of ShiftReg is

	signal tmp			: std_logic_vector(WIDTH-1 downto 0);
	signal tmp_shift	: std_logic_vector(WIDTH-2 downto 0);

begin

	process (clock, aclr)
	begin
		if aclr ='1' then 
			tmp <= (others => '0'); 
		elsif clock'event and clock='1' then  
			if sclr ='1' then 
				tmp <= (others => '0'); 
			elsif enable = '1' then 
				if sset = '1' then 
					tmp <= svalue; 
				elsif load= '1' then 
					tmp <= data; 
				elsif (DIRACTION = '1') then
					tmp_shift <= tmp(width-2 downto 0) & data;
					shiftout <= tmp_shift(0);
				else
					tmp_shift <= tmp(width-1 downto 1) & data;
					shiftout <= tmp_shift(WIDTH-1);
				end if;
			end if;
		end if;
	end process;
	q <= tmp;

end Behavioral;
