/*
	DFFE примитив (триггер D-типа с разрешающим входом ENA)
	+----------------------------+
	| CLRN | CLK  | ENA | D | Q  |
	+----------------------------+	
	|  L   |  X   | X   | X | L  |
	|  H   |  X   | L   | X | Qo |
	|  H   |  _/^ | H   | L | L  |
	|  H   |  _/^ | H   | H | H  |
	|  H   |  L   | H   | X | Qo |
	|  H   |  H   | H   | X | Qo |
	+----------------------------+

	DFFE #(?) ?NAME?(.clrn(), .clk(), .ena(), .d(), .q());
*/

`timescale 1ns / 10ps

module DFFE
#(	parameter W = 1 )
(
	input bit		clrn,			// Асинхронный сброс 
	input bit		clk,
	input bit		[W-1 : 0]ena	// Разрешение на запись
	input bit		[W-1 : 0]d,
	output bit		[W-1 : 0]q
);

bit [W-1 : 0]Q;

genvar i;
generate 
	for (i = 0; i < W; i = i + 1) 
	begin : generate_DFFE
	
	always_ff @(posedge clk or negedge clrn)
		begin
		if(!clrn)
			Q[i] <= 1'b0;
		else if(ena[i])
			Q[i] <= d[i];
		end
		
    end
endgenerate

assign #1 q = Q;

endmodule:DFFE
