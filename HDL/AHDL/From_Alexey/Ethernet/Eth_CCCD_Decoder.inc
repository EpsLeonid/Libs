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
-- Created on Wed Jun 03 11:56:28 2020

FUNCTION Eth_CCCD_Decoder (clock, Rx_Data[(16) - (1)..0], Rx_Parcer_RQ, Rx_NUM_Data[10..0], Reset, Rx_Packet_Lenght[(16) - (1)..0], AccessGranted, DataBus_In[15..0], DataBusStrobe)
	RETURNS (Rx_Addr[10..0], Tx_Addr[10..0], Tx_Data[(16) - (1)..0], Tx_Word_Strobe, Rx_Error, Tx_Start, Rx_Parcer_in_progress, Rx_TRUE_RQ, Rx_NOT_RQ, Identification[(16) - (1)..0], AccessRequest, DirectOut, AddrBusOut[15..0], test);
