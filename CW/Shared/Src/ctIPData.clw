
!Region Modular 
   MEMBER
   MAP
     HeartBeat(STRING inTicks)
   END
   INCLUDE('ctIPData.inc'),ONCE
   INCLUDE('Errors.clw'),ONCE


MOD:IPDRV::Owner    CSTRING(255)
IPBogus                      FILE,CREATE,NAME('IPBogus.tps'),DRIVER('IPDRV'),OWNER(MOD:IPDRV::OWNER)
PK                             KEY(ID)
Record                         RECORD
ID                               LONG
                               END
                             END
                             
!Consider creating a KeyValuePair file                             
  
!EndRegion Modular 

!Region ctIPData 
!-----------------------------
ctIPData.Construct     PROCEDURE
  CODE
  SELF.INI &= NEW ctINI  
  
!-----------------------------
ctIPData.Init          PROCEDURE(STRING iniBaseName, STRING iniFolder)  
  CODE
  SELF.INI.SetBase(iniBaseName)
  SELF.INI.SetFolder(iniFolder)
 
  
!-----------------------------
ctIPData.Destruct      PROCEDURE
  CODE
  CLOSE(IPBogus)

!-----------------------------
ctIPData.ReadINI  PROCEDURE(STRING inDefaultMachine, STRING inDefaultPort, STRING inDefaultDLL)
!2239 Default PORT
!2240 Default SSL PORT
  CODE
! IP_Machine = '192.168.0.14'  !VMDEV -- for names, Kellys says I need NETBIOS or "Enable file and printer sharing"
  SELF.IP_Machine = SELF.INI.Fetch('Server','Machine',inDefaultMachine) 
  SELF.IP_PORT    = SELF.INI.Fetch('Server','Port'   ,inDefaultPort)  !2340 is the default SSL Port
  SELF.IP_DLL     = SELF.INI.Fetch('Server','DLL'    ,inDefaultDLL)
  
   
!-----------------------------
ctIPData.SetOwner PROCEDURE(*CSTRING outIPDrvOwner)
  CODE  
  outIPDrvOwner = SELF.IP_DLL & '@' & SELF.IP_Machine & ':' & SELF.IP_PORT
  MOD:IPDRV::Owner  = outIPDrvOwner

  

!-----------------------------
ctIPData.StartHeartBeat PROCEDURE()
Ticks STRING(10)
  CODE
  SELF.Open(IPBogus)
  Ticks = SELF.INI.Fetch('Settings','HeartBeatInterval', 10 ) * TIME:Minute
  SELF.HeartBeatThread = START(HeartBeat,, Ticks & '')

!-----------------------------
ctIPData.Open PROCEDURE(*FILE inFile)
  !see B4_SF for CheckOpen ...
OpenMode BYTE  
  CODE
!  IF glo:RO.Get()  THEN OpenMode = FileAccessMode:ReadOnly  + FileAccessMode:AnyAccess  !us:readonly, others:any access !only read, NEVER SET!!!
!                   ELSE 
                         OpenMode = FileAccessMode:Default
!  END
  LOOP
     OPEN(inFile, OpenMode)
     CASE ERRORCODE()
       OF NoError        !Return if no error
     OROF IsOpenErr      !or if already open
                         BREAK
     
       OF NoFileErr      !If file was not found
                         CREATE(inFile)
                         IF ERRORCODE()
                            MESSAGE('Failed to Open or Create['& NAME(inFile) &']|Error['& ERROR() &']-['& ERRORFILE() &']')
                            BREAK
                         END
     ELSE
                         MESSAGE('Failed to Open|File['& NAME(inFile) &']|Error['& ERROR() &']-['& ERRORFILE() &']')
     END

   END  
  
!-----------------------------
HeartBeat PROCEDURE(STRING inTicks)
Window WINDOW('HeartBeat for IPDriver'),AT(,,100,25),GRAY
    END
  CODE
  OPEN(Window)
  Window{PROP:Hide} = TRUE
  Window{PROP:Timer} = inTicks
  ASSERT(0,'<4,2,7>'&'HeartBeat start, Ticks['& Window{PROP:Timer} &']')
  ACCEPT
    ! ASSERT(0,'<4,2,7>'&', EVENT()['& EVENT() &']')
    CASE EVENT()
      OF EVENT:TIMER; ASSERT(0,'<4,2,7>'&'HeartBeat for IPDriver')
                      SEND(IPBogus,'IPEXEC HeartBeat') !<-- might be interesting to send some info like the Name of this machine.
    END
  END
  
  
  
  
!EndRegion ctIPData 



