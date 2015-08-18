
gtIDInfo   GROUP,TYPE
ID            LONG
Info          ANY
            END

qtIDInfo   QUEUE(gtIDInfo)
           END

ctIDInfo   CLASS,TYPE,MODULE('ctIDInfo.clw'),LINK('ctIDInfo.clw')
Q               &qtIDInfo
CONSTRUCT       PROCEDURE
DESTRUCT        PROCEDURE
Del             PROCEDURE
Free            PROCEDURE              
FindByID        PROCEDURE(LONG xID ),LONG,PROC !Returns ErrorCode()
GetByPtr        PROCEDURE(LONG xPtr),LONG,PROC !Returns ErrorCode()
AddReplace      PROCEDURE(LONG xID, ? xInfo)
Add             PROCEDURE(LONG xID, ? xInfo)
Records         PROCEDURE(),LONG
            END

! vim: set filetype=clarion:
