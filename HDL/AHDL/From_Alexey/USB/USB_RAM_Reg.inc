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
-- Created on Thu Sep 30 16:05:07 2010

FUNCTION USB_RAM_Reg (FT_TXEn, FT_RXFn, FT_DATA_In[7..0], Bus_Clock, Reset, DataBus_In[15..0], DataBusStrobe, AccessGranted, Select, Direct_In, AddrBus_In[8..0])
	RETURNS (FT_RDn, FT_WR, FT_DATA_Out[7..0], FT_ZZ, USB_Active, Header_recognized, Trailer_recognized, Packet_Proc, Length_is_wrong, RAM_Filling, RAM_WE, Header_recognized_VP, Trailer_recognized_VP, Sample_Enable_VP, Error_at_packet_is_detected, DataBusOut[15..0], Error, AccessRequest, DirectOut, AddrBusOut[15..0]);