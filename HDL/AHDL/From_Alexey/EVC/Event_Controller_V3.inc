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
-- Created on Thu Sep 12 18:36:15 2019

FUNCTION Event_Controller_V3 (Bus_Clock, Reset_Ext, DataBusIn[(16) - (1)..0], DataBusStrobe, AddrBusIn[7..0], Select, DirectIn, Cavity_Clock, Time_Set_Command[7..0], Internal_Start_request, Comb_FOR, Forward_Panel_In0, Forward_Panel_In1, Busy_on_Link_Active, Busy_on_Digitizing, Busy_on_Reloading, Slow_Interface_is_done, Event_Terminate_Request)
	RETURNS (DataBusOut[(16) - (1)..0], Event_is_in_progress, Next_Event_Ena, Slow_Interface_readout_eligible, Synchro_0, Synchro_1, Start_CLBR_Links, Start_ORDINAR_Links, Error, Cavity_Refrensed_1us_period_Pulse, Cavity_Refrensed_1ms_period_Pulse, Phase_Alignig_Output);
