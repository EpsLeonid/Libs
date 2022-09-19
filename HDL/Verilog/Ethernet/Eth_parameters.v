// -- Map of Objects' addresses in Ethernet 

// --	constant ThisDeviceAddr			= X"0555555555CF";	-- MAC
// --	constant ThisDeviceIPAddr		= X"C0A80003";		-- IP: 192.168. 0. 3 <- for test on the table
// --	constant ThisDeviceIPAddr		= X"C0A8400A";		-- IP: 192.168. 64. 10
	`define PacketLenghts_at_signaling_layer_ext	= 2048;//-- maximum length value in bytes
	`define PacketLenghts_at_signaling_layer_int	= 4096;//-- maximum length value in bytes
	`define RxByte_Cnt_Width_ext						= integer(Ceil(log2(real(PacketLenghts_at_signaling_layer_ext))));
	`define RxByte_Cnt_Width_int						= integer(Ceil(log2(real(PacketLenghts_at_signaling_layer_int))));

	`define Eth_WORD_WIDTH								= 16; 

	`define ETH_HEADER_LENGTH							= 14;
	`define IP_HEADER_LENGTH								= 20;
	`define UDP_HEADER_LENGTH							= 8;
	`define OPCODE_LENGTH									= 2;

	`define KLUKVA_DATA_LENGTH							= ((32+512+32)*2)*2; //--2304;
	`define HEADER_LENGTH_BYTES							= ETH_HEADER_LENGTH + IP_HEADER_LENGTH + UDP_HEADER_LENGTH + OPCODE_LENGTH;
	`define HEADER_LENGTH_WORDS							= HEADER_LENGTH_BYTES / 2;
	`define MASS_RAM_BYTE_Tx_Num						= HEADER_LENGTH_BYTES + KLUKVA_DATA_LENGTH;
	
