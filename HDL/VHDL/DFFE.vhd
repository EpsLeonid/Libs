----------------------------------------------------------------------------------
library IEEE;
use IEEE.STD_LOGIC_1164.ALL;
library work;

entity DFFE is
	Port ( D		: in  STD_LOGIC;
			 CLK	: in  STD_LOGIC;
			 EN	: in  STD_LOGIC;
			 SCLR	: in  STD_LOGIC := '0';
			 Q 	: out STD_LOGIC);
end DFFE;

architecture Utility of DFFE is
	signal Trig : STD_LOGIC := '0';

begin
	process (CLK,D,SCLR)
		begin
			if CLK'event and CLK='1' then  
				if SCLR='1' then   
					Q <= '0';
				elsif EN ='1' then
					Q <= D;
				end if;
			end if;
	end process;
end Utility;

