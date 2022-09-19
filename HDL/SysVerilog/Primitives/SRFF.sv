/*
	SRFF примитив (триггер RS-типа)
	+--------------------------+
	| CLRN | CLK  | S | R | Q  |
	+--------------------------+	
	|  L   |  X   | X | X | L  |
	|  H   |  L   | X | X | Qo |
	|  H   |  H   | X | X | Qo |
	|  H   |  _/^ | L | L | Qo |
	|  H   |  _/^ | H | L | H  |
	|  H   |  _/^ | L | H | L  |
	|  H   |  _/^ | H | H | X  | 
	+--------------------------+
	
 	SRFF #(?) ?NAME?(.clrn(), .clk(), .s(), .r(), .q());
*/

`timescale 1ns / 10ps

module SRFF
#(	parameter W = 1 )
(
	input bit		clrn,		// Асинхронный сброс 
	input bit		clk,
	input bit		[W-1 : 0]s,	// Установка в 1
	input bit		[W-1 : 0]r, // Установка в 0
	output bit		[W-1 : 0]q
);

bit [W-1 : 0]Q;

genvar i;
generate 
	for (i = 0; i < W; i = i + 1) 
	begin : generate_SRFF
	
	always_ff @(posedge clk or negedge clrn)
		begin
		if(!clrn)
			Q[i] <= 1'b0;
		else if(s[i])
			Q[i] <= 1'b1;
		else if(r[i])
			Q[i] <= 1'b0;
		end
		
    end
endgenerate

assign #1 q = Q;

endmodule:SRFF
