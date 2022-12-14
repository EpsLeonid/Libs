-- WARNING: Do NOT edit the input and output ports in this file in a text
-- editor if you plan to continue editing the block that represents it in
-- the Block Editor! File corruption is VERY likely to occur.

-- Copyright (C) 1991-2008 Altera Corporation
-- Your use of Altera Corporation's design tools, logic functions 
-- and other software and tools, and its AMPP partner logic 
-- functions, and any output files from any of the foregoing 
-- (including device programming or simulation files), and any 
-- associated documentation or information are expressly subject 
-- to the terms and conditions of the Altera Program License 
-- Subscription Agreement, Altera MegaCore Function License 
-- Agreement, or other applicable license agreement, including, 
-- without limitation, that your use is for the sole purpose of 
-- programming logic devices manufactured by Altera and sold by 
-- Altera or its authorized distributors.  Please refer to the 
-- applicable agreement for further details.


-- Generated by Quartus II Version 8.1 (Build Build 163 10/28/2008)
-- Created on Fri Sep 02 15:24:29 2022

FUNCTION Ethernet_TxRx (System_Clock, Eth_Phy_RxClk, Rx_Reset, Carr, Rx_Data_nibble_input[3..0], RxIntStart, Tx_Reset, Eth_Phy_TxClk, Eth_Phy_MdIO, DataBus_In[(NUM_MS_SIGNALS) - (1)..0][15..0], DataBusStrobe[(NUM_MS_SIGNALS) - (1)..0], Select[(NUM_MS_SIGNALS) - (1)..0], DirectIn[(NUM_MS_SIGNALS) - (1)..0], AddrBus_In[(NUM_MS_SIGNALS) - (1)..0][12..0], Parcer_Reset, AccessGranted[(NUM_MS_SIGNALS) - (1)..0])
	WITH (ETH_MODULE_FUNC_SET, NUM_MS_SIGNALS)
	RETURNS (Eth_Phy_TxEn, Eth_Phy_TxD[3..0], Eth_Phy_MdC, Eth_RxTx_In_Progress, Eth_Tx_End, Transmit_of_Data_RQ, DataBusOut[(NUM_MS_SIGNALS) - (1)..0][15..0], AccessRequest[(NUM_MS_SIGNALS) - (1)..0], DirectOut[(NUM_MS_SIGNALS) - (1)..0], AddrBusOut[(NUM_MS_SIGNALS) - (1)..0][15..0], Test_out);
