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
-- Created on Wed May 04 09:56:52 2011

FUNCTION Random_Gen (clock, DataBusStrobe, Direct, AddrBusExt[7..0], DataBusIn[(16) - (1)..0], Select, Reset, Start_Seed_Load)
	WITH (PolinomPower, PolinomValue, Seed, Threshold)
	RETURNS (DataBusOut[(16) - (1)..0], Random_Period_Output, Det_Period_Output, CRC_Parallel_Out[(32) - (1)..0], CRC_Serial_Out, WORKING, Test1, Test2, Test3, Test4, Test5, Test6);
