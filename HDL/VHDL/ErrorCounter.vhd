----------------------------------------------------------------------------------
-- Company: 
-- Engineer: Epshteyn Leonid
-- 
-- Create Date:		27/06/2022 
-- Design Name:		
-- Module Name:		ErrorCounter 
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

entity ErrorCounter is
	generic (Width				: NATURAL := 8);
				
	port(		clock				: in std_logic;
				DataBusStrobe	: in STD_LOGIC := '1';
				DataBusOut		: out std_logic_vector(Width-1 downto 0);
				AddrBus_i		: in std_logic_vector(Width-1 downto 0);
				Select_i			: in std_logic := '1';
				ErrorInputs		: in std_logic := '0');
	constant Word_Width 		: integer := 16;
	constant AddrWidth 		: integer(floor(log2(real(Width))) + 1);
end ErrorCounter;

architecture Behavioral of ErrorCounter is

	signal Edge_of_Error_Input_o	: std_logic_vector(Width-1 downto 0);
	signal Reg_of_Error_o			: std_logic_vector(Width-1 downto 0);
	signal Counter_of_Error_o		: std_logic_vector(Width-1 downto 0);
	signal Clear_Reg					: std_logic;
	signal Clear_Counters			: std_logic;
	signal C_R							: std_logic;
	signal C_C							: std_logic;

begin

--VARIABLE
--    Edge_of_Error_Input[Width-1 downto 0]			: EdgeSensing;
--    Reg_of_Error[Width-1 downto 0]				: SRFF	;
--    Counter_of_Error[Width-1 downto 0]		 	: lpm_counter with (lpm_width=16);
--    Clear_Reg, Clear_Counters, C_R,C_C		: node;
--BEGIN

--FOR i IN 0 TO Width-1 GENERATE
--  Edge_of_Error_Input[i].(clk, d)	         = (Clock, ErrorInputs[i] );
--  Reg_of_Error[i].(clk, S, R )		         = (Clock, Edge_of_Error_Input[i].q, Clear_Reg);
--  Counter_of_Error[i].(clock, cnt_en, sclr ) = (Clock, Edge_of_Error_Input[i].q, Clear_Counters);
--END GENERATE;
	Error_Input: for i in 0 to Width-1 generate
		Edge_of_Error_Input: entity work.Edge_Sensing
			port map (
				CLK 	=> clock,
				D		=>	ErrorInputs(i),
				Q		=>	Edge_of_Error_Input_o(i));
		Reg_of_Error : entity work.SRFF 
			port map (
				S		=> Edge_of_Error_Input_o(i),
				CLK	=> clock,
				R		=> Clear_Reg,
				q		=> Reg_of_Error_o(i));
		Counter_of_Error : entity work.V_Counter 
			generic map(
				WIDTH => 16
				)
			port map (
				clock 	=> clock,--Quarts,--
				cnt_en	=>	Edge_of_Error_Input_o(i),
				sclr		=> Clear_Counters,
				q			=> Counter_of_Error_o(i));
	end generate;

	process(clock)
	begin
		if (AddrBus_i(AddrWidth-1 downto 0) = '0') THEN 
																	DataBusOut(Width-1 downto 0)				<= Reg_of_Error_o(Width-1 downto 0); 
																	DataBusOut(Word_Width-1 downto Width)	<= '0'; 
		end if;
	end process;

	AddrDataBus:FOR i IN 1 TO Width GENERATE
		process (clock)
		begin
			if (AddrBus_i(AddrWidth-1 downto 0) = i) then 
				 DataBusOut <= Counter_of_Error_o(i-1); 
			end if;
		end process;
	end generate;

	process (clock)
	begin
		IF (AddrBus_i(AddrWidth-1 downto 0) = 0) then 
			C_R <= '1'; 
			C_C <= '0'; 
		END IF;
	end process;
	process (clock)
	begin
		if (AddrBus_i(AddrWidth-1 downto 0) = Width) then 
			C_R <= '0'; 
			C_C <= '1'; 
		end if;
	end process;
	process (clock)
	begin
		Clear_Reg		<= Select_i AND DataBusStrobe AND C_R;
		Clear_Counters	<=	Select_i AND DataBusStrobe AND C_C;
	end process;

end Behavioral;