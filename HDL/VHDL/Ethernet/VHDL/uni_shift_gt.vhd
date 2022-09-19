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

library UNISIM;
use UNISIM.VComponents.all;

library work;
use work.parameters.all;

entity uni_shift_gt is
	Generic (N_IN	: integer := 8;
				N_OUT	: integer := 4
	); 
	Port (
	data_i		: in std_logic_vector (N_IN-1 downto 0);
	clock			: in std_logic; 
	enable		: in std_logic; 
	load			: in std_logic; 
	
	data_o		: OUT std_logic_vector (N_OUT-1 downto 0)
	);
	
end uni_shift_gt;

architecture Behavioral of uni_shift_gt is

	type sh_regs_array is array (0 to N_OUT-1) of std_logic_vector ((N_IN / N_OUT)-1 downto 0);
	signal sh_regs		: sh_regs_array;
	signal sh_regs_shiftout	: std_logic_vector (N_OUT-1 downto 0);
	
begin
	
	process (clock)
	begin
		if (clock'event and clock = '1') then
			for i IN N_OUT - 1 TO 0 loop
				for j IN (N_IN / N_OUT)-1 TO 0 loop
					sh_regs(i)(j) <= data_i(j * N_OUT + i);	
				end loop;
			end loop;
			sh_regs(1) <= sh_regs(0);
			sh_regs_shiftout <= sh_regs(1);
		end if;
	data_o <= sh_regs_shiftout;
	end process;
	
end Behavioral;
