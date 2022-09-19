module pdo
(
	i, o, obar
);

input i;
output o;
output obar;

cycloneiii_pseudo_diff_out pdo_wys
(
	.i(i),
	.o(o),
	.obar(obar)    
);

endmodule