-- Ver3.6 (14.09.2009)

FUNCTION Arbiter 
(
    BARq[(DeviceMaxNumber-1)..0], 
    AddressValid, clock, Reset
)
WITH (DeviceMaxNumber, TimeOut)
	
RETURNS 
(
    BAGd[(DeviceMaxNumber-1)..0], 
    AddressSettled, 
    DataStrobe, 
    Error
--    ,Test1, Test2
);
