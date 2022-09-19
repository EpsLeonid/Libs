/*
	DFF примитив (триггер D-типа, сдвигающий)
	+----------------------+
	| CLRN | CLK  | D | Q  |
	+----------------------+	
	|  L   |  X   | X | L  |
	|  H   |  _/^ | L | L  |
	|  H   |  _/^ | H | H  |
	|  H   |  L   | X | Qo |
	|  H   |  H   | X | Qo |
	+----------------------+
	
	DFF #(?) ?NAME?(.clrn(), .clk(), .d(), .q())
*/

`timescale 1ns / 10ps

module DFF
#(	parameter W = 1 )
(
	input bit		clrn,		// Асинхронный сброс 
	input bit		clk,
	input bit		[W-1 : 0]d,
	output bit		[W-1 : 0]q
);

bit [W-1 : 0]Q;

genvar i;
generate 
	for (i = 0; i < W; i = i + 1) 
	begin : generate_DFF
	
	always_ff @(posedge clk or negedge clrn)
		begin
		if(!clrn)
			Q[i] <= 1'b0;
		else
			Q[i] <= d[i];
		end
		
    end
endgenerate

assign #1 q = Q;

endmodule:DFF
