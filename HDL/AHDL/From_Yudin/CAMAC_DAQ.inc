-- Edited MANUALLY !!

FUNCTION  CAMAC_DAQ
            (W[16..1], clock, B, N, A[4..1], F[5..1], S1, S2,                   AccessGranted, DataBusStrobe,     DataBusIn[15..0] , Confirm, LamSet, LamReset, LinkClockCaptured, LinkActive, LinkError, Busy_on_Digitizing, Busy_on_Copy, DeadTime, StatusR_d[15..8])
    WITH    (RefClock)
    RETURNS (R[16..1], oQ, oX, oL,  Aout[4..1], CAMAC_Active, Rd_En, Wr_En, RdWr, AccessReq, AddrBusOut[15..0], DataBusOut[15..0], StatusRW_q[15..0], LAM, Start, Reset, Error, test[4..1]);
