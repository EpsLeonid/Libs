
-- Memory/Registers Map of "Standard" CMD-3 Projects
-- Internal Resourse Map
%
	  HHHHH		HHHHHHH		HHHHHHH		HHHHHHH
	 H     H	H      H	H      H	H      H
	H      H	H      H	H      H	H      H
	H      H	H      H	H      H	HHHHHHH
	HHHHHHHH	H      H	H      H	H   H
	H      H	H      H	H      H	H     H
	H      H	HHHHHHH		HHHHHHH		H      H
%
-- Skeleton standard addresses
Constant Ph_Sw_0_Offset			= H"1200" ;
Constant Ph_Sw_1_Offset			= H"6200" ;
Constant Ph_Sw_Size				= 16;

Constant Link_0_IFace_Offset 		= H"0C00" ; -- Commad List 
Constant Link_1_IFace_Offset 		= H"6C00" ; -- Commad List 
Constant Link_IFace_Size  			= 1024 ;

Constant Test_MEM_Offset   		= H"2000" ;
Constant Test_MEM_Size     		= 256 ;
Constant Test_REG_Offset   		= H"2200" ;
Constant Test_REG_Size     		= 256 ;

Constant POSS_Offset			= H"1600" ;
Constant POSS_Size				= 128;

Constant System_Counters_Offset	= H"1400" ;
Constant System_Counters_Size		= 16;

Constant Event_Ctrl_Offset 		= H"0600" ; -- Control registers
Constant Event_Ctrl_Size  		= 32 ;
Constant Event_Ctrl_Present		=	"YES";	-- LVDS Bus sharing GENERATE

Constant Silicon_ID_Offset 		= H"1800" ; -- 
Constant Silicon_ID_Size  		= 256 ;

-- Skeleton  board-depended addresses
Constant DC_FORs_Block_Offset  	= H"0A00" ; -- Registers and scalers of FOR
Constant DC_FORs_Block_Size    	= 64 ;
Constant FORs_Module_Present	=	"NO";	-- CAMAC Bus sharing GENERATE

Constant CAMAC_RW_Multiplexed	=	"NO";

-- Board specific addresses

Constant Event_Meneger_Offset	= H"0700" ;
Constant Event_Meneger_Size		= 32 ;

Constant 	Random_Generator_Offset	= H"2400" ;
Constant 	Random_Generator_Size  	= 16 ;

Constant 	Blk_Ctrl_Offset			= H"2500" ;
Constant 	Blk_Ctrl_Size			= 32 ;

Constant 	Utility_Gen_Offset		= H"2700" ;
Constant 	Utility_Gen_Size		= 16 ;

Constant Bunch_Crossing_Offset		= H"2600";
Constant Bunch_Crossing_Size		= 32;

Constant DownLink0_IFace_Offset		= H"3000"; -- DownLinks in MCHS
Constant DownLink1_IFace_Offset		= H"3200";
Constant DownLink2_IFace_Offset		= H"3400";
Constant DownLink3_IFace_Offset		= H"3600";
Constant DownLink4_IFace_Offset		= H"3800";
Constant DownLink5_IFace_Offset		= H"3A00";
Constant DownLink6_IFace_Offset		= H"3C00"; -- DownLinks in MCHS
Constant DownLink7_IFace_Offset		= H"3E00";
Constant DownLink8_IFace_Offset		= H"4000";
Constant DownLink9_IFace_Offset		= H"4200";
Constant DownLink10_IFace_Offset	= H"4400";
Constant DownLink11_IFace_Offset	= H"4600";
Constant DownLink12_IFace_Offset	= H"4800"; -- DownLinks in MCHS
Constant DownLink13_IFace_Offset	= H"4A00";
Constant DownLink14_IFace_Offset	= H"4C00";
Constant DownLink15_IFace_Offset	= H"4E00";
Constant DownLink_IFace_Size		= 256;

Constant ADIS_0_IFace_Offset		= H"5000";
Constant ADIS_IFace_Size			= 512;


Constant ADC_IFace_Offset   	= H"0200" ;
Constant ADC_IFace_Size     	= 256 ;

Constant MUX_IFace_Offset   	= H"0400" ;
Constant MUX_IFace_Size     	= 64 ;

Constant Threshold_Ctrl_Offset 	= H"0800" ; -- Memory of Thresholds
Constant Threshold_Ctrl_Size   	= 32 ;

Constant Mu_FORs_Offset  		= H"8000" ;
Constant Mu_FORs_Size    		= 64 ;

Constant Cell_Offset			= H"8100" ;
Constant Cell_Size    			= 64 ;

Constant Scalers_Offset   		= H"8200" ;
Constant Scalers_Size     		= 256 ;

Constant Air_IFace_Offset   		= H"8700" ;
Constant Air_IFace_Size     		= 256 ;

Constant LED_IFace_Offset 		= H"8800" ;
Constant LED_IFace_Size			= 256 ;

Constant OvV_IFace_Offset 		= H"8900" ;
Constant OvV_IFace_Size			= 256 ;

Constant Loop1_Ctrl_IFace_Offset	= H"8A00" ;
Constant Loop1_Ctrl_IFace_Size		= 256 ;

Constant Loop2_Ctrl_IFace_Offset	= H"8B00" ;
Constant Loop2_Ctrl_IFace_Size		= 256 ;

Constant Link_Scope_Offset			= H"1000" ;
Constant Link_Scope_Size  			= 16 ;
