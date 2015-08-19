         INCLUDE('ctIDInfo.inc'),ONCE

ctTechData  CLASS(ctIDInfo),TYPE,MODULE('ctTechData.clw'),LINK('ctTechData.clw')
Showing        GROUP
FromRow           LONG
ToRow             LONG
QChanges          LONG !CHANGES(SELF.Q) as of last ShowData
               END
ClassName      CSTRING(10)
ShowFEQ        UNSIGNED
DESTRUCT       PROCEDURE() 
Init           PROCEDURE(STRING xClassName, UNSIGNED xShowFEQ)
ShowData       PROCEDURE(LONG xFrom, LONG xTo)
HideData       PROCEDURE
Log            PROCEDURE(LONG xID, ? xInfo)
DebugOne       PROCEDURE()
            END

! vim: set filetype=clarion:
