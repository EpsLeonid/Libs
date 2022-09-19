FUNCTION Phase_Switch 
(
clock, 
Phase0, 
Reset
)
	WITH 
(
FmaxThresh, 
FminThresh 
)
	RETURNS 
(
Phase25, 
LinkClock_Enabled
);
