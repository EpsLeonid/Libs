`timescale 1ns / 1ps

module testbench
(
);

bit clk, reset,ena;
	
initial 
begin
	clk		= 1'b0;
	reset	= 1'b0;
	
#30	reset 	= 1'b1;
#5	reset 	= 1'b0;
end

initial
begin
	ena		= 1'b0;
#20	ena		= 1'b1;
end


always 
  #5  clk = !clk; 


bit	[7:0]d;

initial 
begin
	d = 8'b0;
#10 d = 8'b11111111;
#50	d = 8'b0;
end


bit	[7:0]DFF_q;  
DFF #(8) DFF_inst(.clrn(!reset),.clk(clk),.d(d),.q(DFF_q));

bit	[7:0]DFFE_q;
DFFE #(8) DFFE_inst(.clrn(!reset), .clk(clk), .d(d), .q(DFFE_q));

bit	[7:0]SRFF_q;
bit [7:0]s;
bit [7:0]r;
initial 
begin
	s = 8'b0;
	r = 8'b0;
#10 s = 8'b11111111;
#45 s = 8'b0;
	r = 8'b11111111;
end

SRFFE #(8) SRFF_inst(.clrn(!reset), .clk(clk), .ena(ena), .s(s), .r(r), .q(SRFF_q));

endmodule:testbench