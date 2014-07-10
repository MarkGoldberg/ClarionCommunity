

   INCLUDE('ctINI.inc'),ONCE

ctIPData  CLASS,TYPE,MODULE('ctIPData.clw'),LINK('ctIPData.clw')!,_IPDataLinkMode_,DLL(_IPDataDLLMode_)
INI            &ctINI
IP_DLL          CSTRING(255) 
IP_Machine      CSTRING(255)
IP_PORT         CSTRING(  6) !0..65535

HeartBeatThread SIGNED

CONSTRUCT       PROCEDURE
DESTRUCT        PROCEDURE
Init            PROCEDURE(STRING iniBaseName, STRING iniFolder)  
ReadINI         PROCEDURE(STRING inDefaultMachine, STRING inDefaultPort, STRING inDefaultDLL)
SetOwner        PROCEDURE(*CSTRING outIPDrvOwner)

StartHeartBeat  PROCEDURE()
Open            PROCEDURE(*FILE inFile)
        END

