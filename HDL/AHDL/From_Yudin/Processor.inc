FUNCTION  PROCESSOR 
(
    Clock,
    Sync,
    ProgInit,
    Int[4..1],
    BaseAddr[7..0],
    DataIn[15..0],
    AccessGranted,
    DataStrobe,
    Reset,
    ProgRAM_addr[ 7..0],
    ProgRAM_data[15..0],
    ProgRAM_we
)

RETURNS (
    AddrOut[15..0],
    DataOut[15..0],
    AccessReq,
    RdWr,
    Working,
    Error,
    ProgRAM_q[15..0],

    test[12..1]
);
