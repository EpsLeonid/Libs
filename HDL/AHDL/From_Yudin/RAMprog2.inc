FUNCTION  RAMprog
(
	data_a[15..0],
	address_a[7..0],
	wren_a,
	data_b[15..0],
	address_b[7..0],
	wren_b,
	inclock,
	inclocken,
	outclock,
	outclocken
)
WITH  ( RAMprog_Init_File )

RETURNS (
	q_a[15..0],
	q_b[15..0]
);
