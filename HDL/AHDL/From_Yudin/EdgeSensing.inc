--TITLE "EdgeSensing";
--This file contains function for Positive Edge Sensing
-- The function produces positive pulse of 2clock duration for positive
-- ( LOW -> HIGH ) input signal transition

-- Unificated NAMES of inputs and outputs (with LevelSensing, DelayLine)

FUNCTION  EdgeSensing
(
    d,          -- Input signal to synchronize
    clk,        -- Clock
    Reset       -- Reset
)  
RETURNS
(  
    q           --: output;  -- Synchronized out
);

