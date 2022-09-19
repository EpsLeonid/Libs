/*
 	Reg #(.W()) ?NAME?(.clk(),.ena(),.clrn(),.d(),.q());
*/
`timescale 1ns / 10ps
module REG
#(	parameter W = 1 )
(
	input bit		clk,
	input bit		ena,
	input bit		clrn,
	input bit		[W-1 : 0]d,
	output bit		[W-1 : 0]q
);

bit [W-1 : 0]Q;

genvar i;
generate 
	for (i = 0; i < W; i = i + 1) 
	begin : generate_dff

		always_ff @(posedge clk or negedge clrn)
		begin
			if(!clrn)
				Q[i] <= 1'b0;
			else if(ena)
				Q[i] <= d[i];
		end
    end
endgenerate
assign #1 q = Q;
endmodule:REG
