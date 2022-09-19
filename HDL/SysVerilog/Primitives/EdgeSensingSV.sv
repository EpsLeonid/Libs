/*	
	Формирователь одного импульса, по фронту входного сигнала 
	функция принимает нарастающий фронт входного сигнала (.d = _/^ ) 
	по фронту тактовой частоты (.clk = _/^ ), и формирует 1 импульс 
	длительностью  1 такт  тактовой частоты
*/

module EdgeSensingSV
(
	input   clk,    // входная частота
	input   reset,  // внешний сброс (по высокому уровню)
	input   d,	    // входной сигнал
	output  q_r,    // выходной импульс нарастающий фронт
	output	q_f		// выходной импульс спадающий фронт
);

bit [1:0]DFF_q;

DFF 		DFF1
(
	.clk	(clk),
	.clrn	(!reset),
	.d		(d),
	.q		(DFF_q[0])
);

DFF 		DFF2
(
	.clk	(clk),
	.clrn	(!reset),
	.d		(DFF_q[0]),
	.q		(DFF_q[1])
);


assign q_r =  DFF_q[0] & !DFF_q[1];
assign q_f = !DFF_q[0] &  DFF_q[1];

endmodule: EdgeSensingSV
