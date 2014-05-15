   MEMBER
!Region Module Level   
!Updates (to copy to the .inc)
!Sept-18-06 MG: changed OMIT/COMPILES for HOWMANY() to be C61 vs. C60
!               .DumpControls sort by FEQ
!               .ShowControl re-order columns to lead to show columns with more consistent widths first, making it easier to read.
!               CREATE:ComboButton in .DescribeType

   INCLUDE('debuger.inc'),ONCE
   INCLUDE('equates.clw'),ONCE
   INCLUDE('FileAccessModes.EQU'),ONCE
   
Verbose EQUATE(1) !0..N   

                          COMPILE('***',_width32_)
ANSI_NAME EQUATE('A')
                          !  ***
                          OMIT('***',_width32_)
ANSI_NAME EQUATE('')
                          !  ***

   MAP
     MODULE('Winapi')
       !see http://msdn.microsoft.com/library/default.asp?url=/library/en-us/debug/base/debugging_functions.asp
       !see http://msdn.microsoft.com/en-us/library/windows/desktop/ms679297(v=vs.85).aspx
       debugbreak(),PASCAL !,RAW,NAME('debugbreak')
       OutputDebugSTRING(*CSTRING),PASCAL,RAW,NAME('OutputDebugString' & ANSI_NAME)
     END

     MODULE('C%V%RUN%X%')
       debugerNameMessage (*CSTRING, UNSIGNED EventNum ),RAW,NAME('WslDebug$NameMessage')   !Note: use   Event() + EVENT:FIRST  else will get WM_*
          COMPILE('** C55+ **',_C55_)
       debugerGetFieldName(SIGNED FEQ            ),*CSTRING,RAW,NAME('Cla$FIELDNAME')
          !END-COMPILE('** C55+ **',_C55_)
          OMIT('** C55+ **',_C55_)
       debugerGetFieldName(SIGNED FEQ            ),LONG,RAW,NAME('Cla$FIELDNAME')
          !END-OMIT('** C55+ **',_C55_)
     END

     MyAssertHook2   (UNSIGNED LineNumber, STRING filename, STRING message)  !Not technically part of the class
     ODS(STRING msg),NAME('debuger_ods') !<-- was getting duplicate symbols 
   END



 PRAGMA('define(profile=>off)')
 COMPILE('***',profile)
! @===========================================================================================
! Don`t try and profile this, you get infinite recursion (when used by EnterProc/LeaveProc)
! Set profile=>off on this module in the project
! Note: in C6, profile=>off    possibly renamed to   proc_trace=>off
!
  This comment will prevent you from compiling unless profile=>off on this modu1e
! @===========================================================================================
 !end-COMPILE('***',profile)



                                      !OMIT('_ifdef_',EVENT:APP=08000h)
                                      COMPILE('_ifdef_',EVENT:APP=0)
EVENT:APP         EQUATE(08000h)
EVENT:APP_LAST    EQUATE(0BFFFh)
                                !END_COMPILE('_ifdef_',EVENT:APP=0)



DebugersQ         QUEUE      !added to handle multiple instances cleaner.
Debuger            &Debuger
                  END
!module_Debuger   &Debuger

MOD:ClearedInInit BYTE(FALSE)
Suffix            EQUATE('<13,10>')
!EndRegion Module Level
!Region Procedures (not methods)
!-----------------------------------------------------------------------------------------!
ODS                          PROCEDURE(STRING msg)
!Helper function, to be used internally to the class when the class itself for debugging the class
sz &CSTRING
  CODE
  sz &= NEW CSTRING( SIZE(msg) + SIZE(Suffix) + 1 )
  sz  =                   msg  &      Suffix
  OutputDebugSTRING(sz)
  DISPOSE(sz)
!-----------------------------------------------------------------------------------------!
MyAssertHook2                PROCEDURE(UNSIGNED LineNumber, STRING filename, STRING argMSG)  !Not technically part of the class
  !Note: ASSERT will ONLY call this procedure if you have compiled with Debugging On or  asserts=>on
  !FYI: I found doc. re: asserts on in the C60help.hlp under ASSERT()

  !Useage:   Assert(0,eqDBG&'Message to display')
  !Purpose:  Will add MODULE/LineNumber to output
  !          also makes it easy to call the debuger, from modules with an empty member, hence no debuger instance in scope

  !Updated: Size(eqDBG) replaced with LEN(eqDBG) so this code can be used in C5
  !         Aug 8, 2005 - added .MatchAssertMsg logic, moved GPF logic to a ROUTINE
  !
  !NOTE: module_Debuger is set in Set_Module_Debuger() called via .INIT
  !      there CAN be problems when there are multiple debuger classes around.
  !      especially since there is no stack used for the Module_Debugger
  !      problems: a) using the "wrong" debuger
  !                b) the last debuger to call .Set_module_debuger has been de-instantiated, but others AssertHook2 is still in effect

  !Replaced Module_Debuger with DebugersQ.Debuger


DEBUGMSG   BYTE,AUTO
Matched    LONG,AUTO
  CODE
  !ODS('`MyAssertHook2 Debuger Absent?['& CHOOSE( DebugersQ.Debuger &= NULL)  &'] Records(DebugersQ)['& RECORDS(DebugersQ) &'] POINTER(DebugersQ)['& POINTER(DebugersQ) &'] argMSG['& sub(argMSG,1,1024) &']')
  !GET(DebugersQ, RECORDS(DebugersQ))
  !ODS('`MyAssertHook2 Debuger Absent?['& CHOOSE( DebugersQ.Debuger &= NULL)  &'] Records(DebugersQ)['& RECORDS(DebugersQ) &'] POINTER(DebugersQ)['& POINTER(DebugersQ) &'] argMSG['& sub(argMSG,1,1024) &']')
  !ODS('`MyAssertHook2')

  IF LEN(argMSG) >= LEN(eqDBG) AND argMSG[ 1 : LEN(eqDBG) ] = eqDBG
     DEBUGMSG =  TRUE
     IF LEN(argMSG) = LEN(eqDBG) !added Nov 7, 2003 (after getting index out of range errors)
        argMSG = ''
     ELSE
        argMSG =  argMSG[ LEN(eqDBG) + 1 : SIZE(argMSG) ]
     END
  ELSE
     DEBUGMSG =  FALSE
  END

  !ODS('`DEBUGMSG['& DEBUGMSG &']')

  IF NOT (DebugersQ.Debuger &= NULL)
    IF  DEBUGMSG
       ! DebugersQ.Debuger.DebugOut('in['& CLIP(filename) &' @'& LineNumber &'] Thread['& THREAD() &']  '& argMSG )
       DebugersQ.Debuger.ODS('in['& CLIP(filename) &' @'& LineNumber &'] Thread['& THREAD() &']  '& argMSG )
    ELSE
       Matched =  DebugersQ.Debuger.MatchAssertMsg(LineNumber, FileName, argMSG)
       DebugersQ.Debuger.DebugOut('Debuger, Matched['& Matched &']=['& DebugersQ.Debuger.DescribeAction(Matched) &'] MatchedTo['& DebugersQ.Debuger.AssertMessagesQ.szMSG &']')
       IF DebugersQ.Debuger.AssertHookAction(Matched, LineNumber, FileName, argMSG) = 0
          !DebugersQ.Debuger.DebugOut('Past AssertHookAction Debuger, Matched['& Matched &']')
          CASE Matched
            OF AssertMsgAction::IGNORE
               !do nothing

            OF AssertMsgAction::AUTOGPF
               DebugersQ.Debuger.DebugOut('Assertion failed in['& CLIP(filename) &' @'& LineNumber &']  '& argMSG )
               DO MyAssertHook2::GPFNOW

            OF AssertMsgAction::NOMATCH
          OROF AssertMsgAction::PROMPT
                DebugersQ.Debuger.DebugOut('Assertion failed in['& CLIP(filename) &' @'& LineNumber &']  '& argMSG )
                DebugersQ.Debuger.ShowMessageWindow('Assertion failed in ['& CLIP(filename) &' @'& LineNumber &']'& |
                                                     CHOOSE(LEN(CLIP(argMSG))>0,'||'& CLIP(argMSG),''), 'ASSERT')

          END !case Matched
       END
    END

  ELSE
     IF DEBUGMSG
        ODS('`in['& CLIP(filename) &' @'& LineNumber &']  '& argMSG ) !+ Oct/12/06
     !ODS('`About to Call MESSAGE, no debuger present')
     ELSE  !~Aug/29/08 MG : behavior changed, ise DEBUGMSG, the avoid the offer to GPF
        CASE MESSAGE('Assertion failed in ['& CLIP(filename) &'] @['& LineNumber &']'& |
                      CHOOSE(LEN(CLIP(argMSG))>0,'||'& CLIP(argMSG),''),               |
                      'ASSERT', ICON:Exclamation, '&Continue|&Halt',1)

          OF 1; !do nothing
          OF 2; DO MyAssertHook2::GPFNOW
        END !case
     END
  END

!----------------------------------------------------
MyAssertHook2::GPFNOW ROUTINE
!data
!lZero       long
!  code
  !ODS('`MyAssertHook2::GPFNOW')

  SYSTEM{prop:asserthook}  = 0  !Stop recursive calls into assert handler
  SYSTEM{prop:asserthook2} = 0  !Stop recursive calls into assert handler
  debugbreak()

  !lZero = 0
  !lZero = lZero / lZero         !Causes a divide by zero GPF -- no longers works (tested in C6.9034)
  !ODS('`MyAssertHook2::GPFNOW POST GPF lZero['& lZero &']')
  EXIT
!EndRegion Procedures (not methods)
 
  
!Region DebugerAutoInit
!-----------------------------------------------------------------------------------------!
DebugerAutoInit.CONSTRUCT PROCEDURE
   CODE
   !ODS('`DebugerAutoInit.construct'& Suffix)
   SELF.mg_init('AutoInit')

!EndRegion DebugerAutoInit
!Region Debuger Methods
Debuger.CONSTRUCT            PROCEDURE
   CODE
   !ODS('`Debuger.construct [start]'& Suffix)
   !ODS('`Debuger.construct SELF['& ADDRESS(SELF) &'] Records(DebugersQ)['& RECORDS(DebugersQ) &'] Pointer['& POINTER(DebugersQ) &']'& Suffix)
   !SELF.Set_Module_Debuger()
   RETURN
!-----------------------------------------------------------------------------------------!
Debuger.mg_init              PROCEDURE(STRING ProgramName)
   CODE
   SELF.Init(ProgramName,DEBUGER::ENABLED,0,DEBUGER::CLEAR_CMDLINE,DEBUGER::ASCII_NEVER) !DEBUGER::ASCII_PROMPT
   SELF.AddAssertMsg(0,'^PrintEvent',AssertMsgAction::PrintEvent, Match:Regular + Match:NoCase )
!-----------------------------------------------------------------------------------------!

Debuger.init                 PROCEDURE(STRING argPgmname,BYTE argOSmode,SHORT argDuplicates,BYTE argClear, BYTE argAsciiByDefault)
   CODE
   SELF.DebugFilter = '`'
   SELF.AppendCRLF  = TRUE           !mg Feb/5/04 -- may wish to turn off "Options->Force Carrigage Returns" in debugView
   SELF.pgmname     = argPgmname
   SELF.osmode      = argOSmode      !this parameter if true, turns on debug regaredless of project setting
   SELF.duplicates  = argDuplicates  ! default number of same debug messages in a row before a warning message is issued

   CASE argOSMode
     OF DEBUGER::DISABLED       ; SELF.DebugActive = FALSE
     OF DEBUGER::ENABLED        ; SELF.DebugActive = TRUE
     OF DEBUGER::ENABLE_CMDLINE ; SELF.DebugActive = CHOOSE( COMMAND('/Debuger') <> '' )
   ELSE                         ; SELF.DebugActive = TRUE
   END

!  SELF.DebugOut('Debuger  MOD:ClearedInInit['&MOD:ClearedInInit&'] argClear['&argClear&'] command(''/clear'')['&command('/clear')&'] SELF.debugactive['&SELF.debugactive&']')
   IF ~MOD:ClearedInInit
      CASE argClear
        OF DEBUGER::CLEAR_ALWAYS ;                           SELF.ClearLog(); MOD:ClearedInInit=TRUE
        OF DEBUGER::CLEAR_CMDLINE; IF COMMAND('/Clear') THEN SELF.ClearLog(); MOD:ClearedInInit=TRUE END
      END !case
   END

   IF SELF.DebugActive
      IF Verbose > 0
                             SELF.delayOut('Program['& CLIP(COMMAND(0)) &'] ')
         IF SELF.osmode THEN SELF.debugout('DebugerOn[Always]')           ! force debug on regardless of debug in project
                        ELSE SELF.debugout('DebugerOn[Command line]')     ! force debug on a production app
         END
      END
      IF Verbose > 3
         SELF.debugout('Debuger Class last modified['& CLIP(FORMAT(DATE(12,11,2009),@D18)) &']')
         SELF.debugout('Debuger Class Updates: http://www.monolithcc.com/clarion/debuger.zip')
         SELF.DebugOut ('-{90}')
         SELF.debugout('')
      END
   END

   SELF.Set_dumpQue_DefaultFileName('ExportQ.csv')
   SELF.Set_dumpQue_AsciiByDefault(argAsciiByDefault)

   DO Init::SetEventOffset

   SELF.Set_Module_Debuger() !moved .Construct
   SELF.Set_AssertHook2()

   SELF.UserEventNameQ &= NEW qtUserEventName; CLEAR(SELF.UserEventNameQ)
   SELF.AddUserEvent('EVENT:DoResize',EVENT:User-1)

   SELF.AssertMessagesQ &= NEW qtAssertMessages; CLEAR(SELF.AssertMessagesQ)
   IF Verbose > 3 THEN ODS(SELF.DebugFilter &'Debuger.init [end]') END
!-------------------------------------
Init::SetEventOffset  ROUTINE
  !Purpose: set SELF.EventOffset which varies with the version of CW
  !         this value is used by GetEventDescr to determine when to call WslDebug$NameMessage
DATA
EventNum  LONG
Pass      BYTE
Lo        LONG
Hi        LONG
  CODE
                           COMPILE('**++** _C60_Plus_',_C60_)
  SELF.EventOffset = 0A000h
                                 !  **++** _C60_Plus_
                           OMIT   ('**--** _PRE_C6_',_C60_)
  SELF.EventOffset = 01400h
                                 !  **--** _PRE_C6_

  IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED' THEN EXIT END

  SELF.DebugOut('SELF.EventOffset is not correct, trying to find a correct value')

  SELF.EventOffset = CHOOSE(SELF.EventOffset = 01400h, 0A000h, 01400h)
  if UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED' THEN EXIT END

  SELF.EventOffset = GETINI('Debuger','EventOffset', -1)
  CASE SELF.EventOffset
    OF -2; SELF.DebugOut('Stored value for EventOffset indicates no valid offset to be found, not searching')
    OF -1; SELF.DebugOut('SELF.EventOffset is not correct, searching for correct value')
  ELSE   ; IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED'
              SELF.DebugOut('Using stored value for SELF.EventOffset')
              EXIT
           END
  END

  !The loops are split out to search more likely ranges first
  !for efficiency it makes more sense to check offsets incrementing by 100, searching for a result that starts with 'EVENT'
  LOOP Pass = 1 TO 4
    EXECUTE Pass
      BEGIN; Lo = 0A000h; Hi = 0AFFFh END
      BEGIN; Lo = 01000h; Hi = 01FFFh END
      BEGIN; Lo = 00000h; Hi = 00FFFh END
      BEGIN; Lo = 0B000h; Hi = 0FFFFh END
    END
    LOOP EventNum = Lo TO Hi
       SELF.EventOffset = EventNum
       IF UPPER(SELF.GetEventDescr(EVENT:ACCEPTED)) = 'EVENT:ACCEPTED'
          PUTINI('Debuger','EventOffset',SELF.EventOffset)
          EXIT
       END
    END
  END

  SELF.DebugOut('Could not find a working offset for .EventOffset')
  SELF.EventOffset = -2
  PUTINI('Debuger','EventOffset',SELF.EventOffset)
!-----------------------------------------------------------------------------------------!
Debuger.destruct             PROCEDURE()
   CODE
   IF Verbose > 5 THEN ODS('`Debuger.Destruct v') END
   SELF.kill()
   IF Verbose > 5 THEN ODS('`Debuger.Destruct ^') END
!-----------------------------------------------------------------------------------------!
Debuger.kill                 PROCEDURE()
   CODE
   IF Verbose > 5 THEN ODS('`Debuger.kill (start) pgmname['& SELF.pgmname &'] SELF['& ADDRESS(SELF) &'] Records(DebugersQ)['& RECORDS(DebugersQ) &']') END

   IF SELF.debugactive
      SELF.debugout('Program['& CLIP(COMMAND(0)) &'] Ended ['& CLIP(FORMAT(TODAY(),@D18)) &' '& CLIP(FORMAT(CLOCK(),@T8)) &']')
      SELF.debugactive = FALSE
   END

   IF ~(SELF.UserEventNameQ &= NULL)
      FREE   (SELF.UserEventNameQ)
      DISPOSE(SELF.UserEventNameQ)
   END

   IF ~(SELF.AssertMessagesQ &= NULL)
      SELF.FreeAssertMsg()
      DISPOSE(SELF.AssertMessagesQ)
   END

   SELF.Clear_Module_Debuger()

   IF Verbose > 5 THEN ODS('`Debuger.kill (end) SELF['& ADDRESS(SELF) &']') END
!-----------------------------------------------------------------------------------------!
Debuger.AddUserEvent         PROCEDURE(STRING argEventName,LONG argEventEquate)
 CODE
 IF ~SELF.UserEventNameQ &= NULL
    IF SELF.GetUserEvent(argEventEquate)
       SELF.UserEventNameQ.EventName    = argEventName
       PUT(SELF.UserEventNameQ)
    ELSE
       SELF.UserEventNameQ.EventEquate  = argEventEquate
        SELF.UserEventNameQ.EventName    = argEventName
       ADD(SELF.UserEventNameQ)
    END
  END
!-----------------------------------------------------------------------------------------!
Debuger.GetUserEvent         PROCEDURE(LONG argEventEquate)!string
  CODE
  IF ~SELF.UserEventNameQ &= NULL
    SELF.UserEventNameQ.EventEquate  = argEventEquate
    GET(SELF.UserEventNameQ, SELF.UserEventNameQ.EventEquate)
    RETURN CHOOSE( ERRORCODE()=0, SELF.UserEventNameQ.EventName, '')
  ELSE
    RETURN ''
  END
!-----------------------------------------------------------------------------------------!
Debuger.GetEventDescr_WM     PROCEDURE(LONG argEvent)!,string   !No SELF.OffsetWork, or UserEvents
  !todo: received events with missing values:
  !  0x007Fh - WM_GETICON
  !  0x0215h
  !  0x02A2h
  !  0x0400h
lcl:Retval   LIKE(qtUserEventName.EventName)
qWM_WINDOWPOSCHANGING EQUATE(70) !added Feb/4/05
  CODE
  CASE argEvent
    OF qWM_WINDOWPOSCHANGING; lcl:RetVal = 'WM_WINDOWPOSCHANGING' !had problems with GPFs I assume it has todo with
    OF 0219H                ; lcl:RetVal = 'WM_DEVICECHANGE'
  ELSE debugerNameMessage(lcl:RetVal, argEvent)
  END
  RETURN lcl:RetVal
!-----------------------------------------------------------------------------------------!
Debuger.GetEventDescr        PROCEDURE(LONG argEvent)!,string  !prototype set to default to -1
   !NameMessage     (*cstring, unsigned EventNum ),name('WslDebug$NameMessage'),raw   !Note: use   Event() + EVENT:FIRST  else will get WM_*
lcl:Retval   LIKE(qtUserEventName.EventName)
lcl:EventNum UNSIGNED
  CODE
  IF argEvent = -1
     argEvent = EVENT()
  END
  lcl:RetVal = SELF.GetUserEvent( argEvent )
  IF ~lcl:RetVal
    CASE argEvent
      OF Event:User                      ; lcl:RetVal = 'EVENT:User'
      OF Event:User + 1 TO Event:Last    ; lcl:RetVal = 'EVENT:User + '& argEvent - Event:User
      OF Event:APP                       ; lcl:RetVal = 'EVENT:App'
      OF Event:APP  + 1 TO Event:APP_LAST; lcl:RetVal = 'EVENT:App + ' & argEvent - Event:APP

    ELSE                                 ; IF SELF.EventOffset = -2   !indicates could not find a valid offset
                                              lcl:RetVal = 'EVENT['& argEvent &']'
                                           ELSE
                                              lcl:EventNum = argEvent + SELF.EventOffset  ! 1400h (pre c6) or A000h (c6) !EVENT:FIRST equate(01400h)/(0A000h)
                                              debugerNameMessage(lcl:RetVal, lcl:EventNum)
                                           END
    END
  END
  RETURN lcl:RetVal ! CLIP(lcl:RetVal)
!-----------------------------------------------------------------------------------------!
Debuger.GetFEQDescr          PROCEDURE(SIGNED argFEQ)!,string  !prototype set to default to -1
  !  GetFieldName    (signed FEQ            ),*cstring,raw,name('Cla$FIELDNAME')
Retval       CSTRING(60) !<--- some arbitrary length
lcl:FEQ      SIGNED
szRef       &CSTRING
  CODE
  lcl:FEQ = CHOOSE(argFEQ = -MAX:FEQ, FIELD(), argFEQ)
                                             COMPILE('** C55+ **',_C55_)
  RetVal  = debugerGetFieldName(lcl:FEQ)
                                            !END-COMPILE('** C55+ **',_C55_)
                                             OMIT('** C55+ **',_C55_)
  szRef  &= debugerGetFieldName(lcl:FEQ)
  RetVal  = szRef
                                            !END-OMIT('** C55+ **',_C55_)
  RETURN RetVal 
!-----------------------------------------------------------------------------------------!
Debuger.PrintEvent           PROCEDURE(<STRING argHeader>,<BYTE argForceDebug>)
   !originally designed to mimic 'WslDebug$PrintEvent')
   !'WslDebug$PrintEvent' has output something like: "EVENT:Selected            ?BUTTON1       (1)<13,10>"
  CODE

  SELF.debugout(  FORMAT(SELF.GetEventDescr() & CHOOSE(0{prop:AcceptAll}<>1,'',  ' - AcceptAll')   ,@s30              ) & |       
      CHOOSE(                 FIELD()=0  ,'',      ' Field('& SELF.GetFEQDescr()           & ' ='& FIELD()       &')' ) & |
      CHOOSE(                 FOCUS()=0  ,'',      ' Focus('& SELF.GetFEQDescr(FOCUS())    & ' ='& FOCUS()       &')' ) & |
      CHOOSE(              SELECTED()=0  ,'',   ' Selected('& SELF.GetFEQDescr(SELECTED()) & ' ='& SELECTED()    &')' ) & |
      CHOOSE(  FOCUS(){prop:SelStart}=0  ,'',   ' SelStart('& FOCUS(){prop:SelStart}                             &')' ) & |
      CHOOSE(  FOCUS(){prop:SelEnd  }=0  ,'',     ' SelEnd('& FOCUS(){prop:SelEnd}                               &')' ) & |
      CHOOSE(               KEYCODE()=0  ,'',    ' KeyCode('&                                      KEYCODE()     &')' ) & |
      CHOOSE(             ERRORCODE()=0  ,'',      ' Error('& ERRORCODE()                  &': ' & CLIP(ERROR()) &')' ) & |
                                                  ' Thread('& THREAD()                                           &')'   & |
      CHOOSE(       CONTENTS(FOCUS())='' ,'',   ' Contents('& CONTENTS(FOCUS())                                  &')' ) & |
      CHOOSE(FOCUS(){prop:ScreenText}='' ,'', ' ScreenText('& FOCUS(){prop:ScreenText}                           &')' ) & |
                '', argHeader,,argForceDebug)

!v-- Previous Version --v
!  SELF.debugout(       FORMAT(SELF.GetEventDescr(),@s30)                              & | !~Mar/20/08 @s20 -> @s30
!                   ' Field('& SELF.GetFEQDescr()           & ' ='& FIELD()       &')' & |
!                   ' Focus('& SELF.GetFEQDescr(FOCUS())    & ' ='& FOCUS()       &')' & |
!                ' Selected('& SELF.GetFEQDescr(SELECTED()) & ' ='& SELECTED()    &')' & |
!                ' SelStart('& FOCUS(){prop:SelStart}                             &')' & |
!                  ' SelEnd('& FOCUS(){prop:SelEnd}                               &')' & |
!                 ' KeyCode('&                                      KEYCODE()     &')' & |
!                   ' Error('& ERRORCODE()                  &': ' & CLIP(ERROR()) &')' & |
!                  ' Thread('& THREAD()                                           &')' & |
!                ' Contents('& CONTENTS(FOCUS())                                  &')' & |
!              ' ScreenText('& FOCUS(){prop:ScreenText}                           &')' & |
!               ' AcceptAll('& 0{prop:AcceptAll}                                  &')' & | !+Aug/31/06
!                '', argHeader,,argForceDebug)


!simpler version:!  SELF.debugout(SELF.GetEventDescr() & '<32>{10}' & SELF.GetFEQDescr() & '<32>{10}('& FIELD() &')', argHeader,,argForceDebug)
  RETURN
!-----------------------------------------------------------------------------------------!
Debuger.delayout             PROCEDURE(STRING argBody,<STRING argHeader>,BYTE argForceDebug)
PARAM:DEBUGOUT:HEADER EQUATE(3) !omittable parameter number, remember to add 1 for SELF
  CODE
  IF SELF.debugActive OR argForceDebug
     IF NOT SELF.DelayActive
        !MG: consider forcing output, when header changes (and is non-null)
        IF NOT OMITTED(PARAM:DEBUGOUT:HEADER)
             SELF.DelayHeader = argHeader
        ELSE SELF.DelayHeader = ''
        END
        SELF.DelayBody   = argBody
        SELF.DelayActive = TRUE
     ELSE
        IF NOT OMITTED(PARAM:DEBUGOUT:HEADER) AND argHeader AND argHeader <> SELF.DelayHeader
           !Force output, and start a new delay
           SELF.debugout('',SELF.DelayHeader,FALSE,TRUE)
           SELF.DelayHeader = argHeader
           SELF.DelayBody   = argBody
        ELSE
           SELF.DelayBody   = CLIP(SELF.DelayBody) & argBody
        END
     END
  END
!-----------------------------------------------------------------------------------------!
Debuger.Message              PROCEDURE(STRING argBody,<STRING argHeader>,BYTE argShowMessage,BYTE argForceDebug)   !string body,<string> header,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE
PARAM:DEBUGOUT:HEADER EQUATE(3) !omittable parameter number, remember to add 1 for SELF
  CODE
  IF OMITTED(PARAM:DEBUGOUT:HEADER)
     SELF.debugout( argBody,          , argShowMessage, argForceDebug)
  ELSE
     SELF.debugout( argBody, argHeader, argShowMessage, argForceDebug)
  END
!-----------------------------------------------------------------------------------------!
Debuger.ODS                  PROCEDURE(STRING xMessage) !+Mar/13/08
  CODE
  ODS(SELF.debugfilter & xMessage)
!-----------------------------------------------------------------------------------------!
Debuger.debugout_splitby     PROCEDURE(STRING argBody,STRING argSplitBy, <STRING argHeader>,BYTE argShowMessage,BYTE argForceDebug)
  !doesn't split within the .delaybody
CurrChar  LONG(1)
NextChar  LONG
nMax      LONG,AUTO
  CODE
  !IF SELF.DelayActive  THEN NextChar = INSTRING( SELF.delaybody, argSplitBy, 1, 1)
  !ELSIF ~NextChar      THEN NextChar = INSTRING( argbody       , argSplitBy, 1, 1)
  !END

  LOOP
    NextChar = INSTRING(  argSplitBy, argbody, 1, CurrChar)
    IF ~NextChar THEN BREAK END
    SELF.DebugOut( argbody[ CurrChar : NextChar - 1],argHeader, argShowMessage, argForceDebug)
    CurrChar = NextChar + SIZE(argSplitBy)
  END

  nMax = LEN(CLIP(argBody))
  IF CurrChar <= nMax
     SELF.DebugOut( argbody[ CurrChar : nMax ],argHeader, argShowMessage, argForceDebug)
  END
!-----------------------------------------------------------------------------------------!
Debuger.debugout             PROCEDURE(STRING argBody,<STRING argHeader>,BYTE argShowMessage,BYTE argForceDebug)   !string body,<string> header,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE
PARAM:DEBUGOUT:HEADER EQUATE(3) !omittable parameter number, remember to add 1 for SELF
!lcl:Altmsg LIKE(SELF.Thismsg) !side-effect argument into ShowMessageWindow
   CODE
   IF SELF.debugActive OR argForceDebug
      IF argShowMessage
         SELF.ShowMessageWindow(argBody,argHeader)
      END

      !consider swapping logic around to give running counts of duplicates in SELF.thismsg

      SELF.thismsg = SELF.debugfilter                                                       & |
                     SELF.pgmname                                                  & ' - '  & |
                     CHOOSE(OMITTED(PARAM:DEBUGOUT:HEADER)=TRUE,'',CLIP(argHeader) & ' - ') & |
                     CHOOSE(SELF.DelayActive=TRUE, CLIP(SELF.delaybody), '') & argBody      & |
                     CHOOSE(SELF.AppendCRLF =FALSE,'','<13,10>')

      OutputDebugString (SELF.thismsg)  ! send the debug message to the viewer

      IF SELF.duplicates <> 0  !MG Policy: only track lastmsg,NumberSame if going to do something with them....
         IF  SELF.lastmsg <> SELF.thismsg
             SELF.lastmsg  = SELF.thismsg
             SELF.NumberSame  = 0
         ELSE
             SELF.NumberSame += 1
             !if SELF.duplicates <> 0 and SELF.NumberSame % SELF.duplicates = 0
             IF SELF.NumberSame % SELF.duplicates = 0
                SELF.ShowMessageWindow('A series of ' & SELF.NumberSame & ' duplicate debug messages have been issued' & |
 |                    '||'                                     & |
 |                    'Body         ['&argBody           &']|' & |
 |                    'DebugFilter  ['& SELF.debugFilter &']|' & |
 |                    'Program Name ['& SELF.pgmname     &']|' & |
 |                    'header       ['& CLIP(argHeader)  &'] Omitted?['& OMITTED(PARAM:DEBUGOUT:HEADER) &']|' &|
 |                    'CRLF Status  ['& SELF.AppendCRLF  &']|' & |
 |                    'Delay Status ['& SELF.DelayActive &']|' & |
                    'Message      ['& CLIP(SELF.thismsg) &']',argHeader)
             END
         END
      END
      SELF.DelayActive = FALSE
   END
!-----------------------------------------------------------------------------------------!
Debuger.ShowMessageWindow    PROCEDURE(string argBody, string argHeader)
                                                     COMPILE('**32bit**',_width32_)
                                                        COMPILE('*debug*',_debug_)
DEBUGER::BUTTONLIST EQUATE('&Continue|&Halt|&Debug')
                                                        !END-COMPILE('*debug*',_debug_)
                                                        OMIT('*debug*',_debug_)
DEBUGER::BUTTONLIST EQUATE('&Continue|&Halt')
                                                        !END-OMIT('*debug*',_debug_)
                                                     !END-COMPILE('**32bit**',_width32_)
                                                     OMIT('**32bit**',_width32_)
DEBUGER::BUTTONLIST EQUATE('&Continue|&Halt')
                                                     !END-OMIT('**32bit**',_width32_)
   CODE
   IF SELF.ShowMessageWindow_Beep
      BEEP(BEEP:SystemExclamation)
   END

   CASE MESSAGE(argBody,SELF.pgmname & CHOOSE(LEN(CLIP(argHeader))=0,'', ' - ' & argHeader), ICON:Exclamation, DEBUGER::BUTTONLIST)
     OF 1; !do nothing                               ! Name: &OK  (Default)
     OF 2; SELF.debugout('Program Halted -');HALT()  ! Name: &Abort
     OF 3; SELF.debugbreak()                         ! Name: Debug
   END !CASE
!-----------------------------------------------------------------------------------------!
Debuger.DebugBreak           PROCEDURE   ! only for 32 bit...doesnt work in 16 bit mode
   CODE
      COMPILE('***',_width32_)
   IF SELF.Debugactive
      debugbreak()    ! asm ROUTINE
   END
      !END-COMPILE('***',_width32_)
!-----------------------------------------------------------------------------------------!
Debuger.Clear_Module_Debuger PROCEDURE
QPtr  LONG
   CODE
   !ODS('`Debuger.Clear_Module_Debuger Start Records(DebugersQ)['& RECORDS(DebugersQ) &'] Curr is NULL?['& CHOOSE( DebugersQ.Debuger &= NULL)  &']  SELF['& ADDRESS(SELF) &'] Pointer['& POINTER(DebugersQ) &']')
!   LOOP QPtr = RECORDS(DebugersQ) TO 1 BY -1
!     GET(DebugersQ,QPtr)
!     IF DebugersQ.Debuger &= SELF
!        DELETE(DebugersQ)
!        GET(DebugersQ,QPtr - 1)
!        IF ERRORCODE()
!           DebugersQ.Debuger &= NULL
!        END
!        BREAK
!     END
!   END
   !ODS('`Debuger.Clear_Module_Debuger END   Records(DebugersQ)['& RECORDS(DebugersQ) &'] Curr is NULL?['& CHOOSE( DebugersQ.Debuger &= NULL)  &']  SELF['& ADDRESS(SELF) &'] Pointer['& POINTER(DebugersQ) &']')


   LOOP QPtr = RECORDS(DebugersQ) TO 1 BY -1
     GET(DebugersQ,QPtr)
     IF DebugersQ.Debuger &= SELF
        DELETE(DebugersQ)
        BREAK
     END
   END

   IF RECORDS(DebugersQ)
      GET(DebugersQ, RECORDS(DebugersQ))
   ELSE
      DebugersQ.Debuger &= NULL
   END
!-----------------------------------------------------------------------------------------!
Debuger.Set_Module_Debuger   PROCEDURE
QPtr  LONG,AUTO
   CODE
   !module_Debuger          &= SELF  !Note: can be confusing if there are multiple instances of debugers
                                    !as an ASSERT(0,eqDBG&'yada') can show the wrong prefix,
                                    !this is becauase the module debuger is set to the most recently instantiated debuger

   !consider adding protections against adding more than once
   LOOP QPtr = RECORDS(DebugersQ) TO 1 BY -1
     GET(DebugersQ,QPtr)
     IF DebugersQ.Debuger &= SELF
        !ODS('`Debuger.Set_Module_Debuger -- returning, SELF already present Records(DebugersQ)['& RECORDS(DebugersQ) &']  SELF['& ADDRESS(SELF) &']  Pointer['& POINTER(DebugersQ) &']')
        RETURN
     END
   END

   DebugersQ.Debuger &= SELF
   ADD(DebugersQ)
   !ODS('`Debuger.Set_Module_Debuger  pgmname['& SELF.pgmname &'] Records(DebugersQ)['& RECORDS(DebugersQ) &']  SELF['& ADDRESS(SELF) &']  Pointer['& POINTER(DebugersQ) &']')
!-----------------------------------------------------------------------------------------!
Debuger.Set_AssertHook2      PROCEDURE
   CODE
   !SELF.DebugOut('Debuger.Set_AssertHook2 MyAssertHook2['& ADDRESS(MyAssertHook2) &']')
   SYSTEM{prop:asserthook2} = ADDRESS(MyAssertHook2)
   !SELF.DebugOut('Debuger.Set_AssertHook2 MyAssertHook2['& ADDRESS(MyAssertHook2) &']')
!-----------------------------------------------------------------------------------------!
Debuger.FreeAssertMsg        PROCEDURE()
QPtr  LONG
   CODE
   IF ~(SELF.AssertMessagesQ &= NULL)
      LOOP QPtr = RECORDS(SELF.AssertMessagesQ) TO 1 BY -1
        GET    (SELF.AssertMessagesQ,QPtr)
        DISPOSE(SELF.AssertMessagesQ.szMSG)
        DELETE (SELF.AssertMessagesQ)
      END
   END
!-----------------------------------------------------------------------------------------!
Debuger.DelAssertMsg         PROCEDURE(STRING   argMessage)
   CODE
   SELF.AssertMessagesQ.szMSG = CLIP(argMessage) !clip prob. unneeded
   GET(SELF.AssertMessagesQ, SELF.AssertMessagesQ.szMSG)
   IF ~ERRORCODE()
      DISPOSE(SELF.AssertMessagesQ.szMSG)
      DELETE (SELF.AssertMessagesQ)
   END

!-----------------------------------------------------------------------------------------!
Debuger.AddAssertMsg         PROCEDURE(LONG argPriority, STRING   argMessage, LONG argAction, LONG argMatchMode)                         !added Aug,7 2005!
!see LRM for MATCH
!MATCH:* equates are found in Equates.clw
!NOTE: consider adding MATCH:NOCASE to your argMatchMode
!NOTE: I believe the following are identical
! dbg.AddAssertMsg(0,'You are calling CLOSE(*) instead of FileManager.Close()'    ,AssertMsgAction::IGNORE,Match:Wild    + MATCH:NoCase)
! dbg.AddAssertMsg(0,'You are calling CLOSE(.*) instead of FileManager\.Close()'  ,AssertMsgAction::IGNORE,Match:Regular + MATCH:NoCase)
   CODE
   SELF.AssertMessagesQ.Priority  = argPriority
   SELF.AssertMessagesQ.szMSG    &= NEW CSTRING(LEN(CLIP(argMessage)) + 1)
   SELF.AssertMessagesQ.szMSG     =                 CLIP(argMessage)
!   SELF.DebugOut('Debuger.AddAssertMsg SELF.AssertMessagesQ.szMSG['& SELF.AssertMessagesQ.szMSG &'] CLIP(argMessage)['& CLIP(argMessage) &'] SELF.AssertMessagesQ.MatchMode['& SELF.AssertMessagesQ.MatchMode &'] Action['& SELF.AssertMessagesQ.Action &']')
!   SELF.AssertMessagesQ.szMSG     =                      argMessage
!   SELF.DebugOut('Debuger.AddAssertMsg SELF.AssertMessagesQ.szMSG['& SELF.AssertMessagesQ.szMSG &'] CLIP(argMessage)['& CLIP(argMessage) &'] SELF.AssertMessagesQ.MatchMode['& SELF.AssertMessagesQ.MatchMode &'] Action['& SELF.AssertMessagesQ.Action &']')

   SELF.AssertMessagesQ.Action    = argAction
   SELF.AssertMessagesQ.MatchMode = argMatchMode
   ADD(SELF.AssertMessagesQ,-SELF.AssertMessagesQ.Priority) !higher numbers go first



!-----------------------------------------------------------------------------------------!
Debuger.MatchAssertMsg       PROCEDURE(UNSIGNED LineNumber, STRING filename, STRING argMSG)         !added Aug,7 2005!
N  LONG
   CODE
   IF ~(SELF.AssertMessagesQ &= NULL)
      SELF.DebugOut('Debuger.MatchAssertMsg, RECORDS(SELF.AssertMessagesQ)['& RECORDS(SELF.AssertMessagesQ) &'] argMSG['& argMSG &']')
      LOOP N = 1 TO RECORDS(SELF.AssertMessagesQ)
         GET(SELF.AssertMessagesQ,N)
         SELF.DebugOut('Debuger.MatchAssertMsg N['& N &'] SELF.AssertMessagesQ.szMSG['& SELF.AssertMessagesQ.szMSG &'] SELF.AssertMessagesQ.MatchMode['& SELF.AssertMessagesQ.MatchMode &'] Action['& SELF.AssertMessagesQ.Action &']')
         IF MATCH(argMSG, SELF.AssertMessagesQ.szMSG , SELF.AssertMessagesQ.MatchMode)
            RETURN SELF.AssertMessagesQ.Action
         END
      END
   END
   SELF.DebugOut('Debuger.MatchAssertMsg  NO MATCH')
   CLEAR(SELF.AssertMessagesQ)
   RETURN AssertMsgAction::NOMATCH



!-----------------------------------------------------------------------------------------!
Debuger.AssertHookAction     PROCEDURE(LONG Action, UNSIGNED LineNumber, STRING filename, STRING argMSG) !,LONG,VIRTUAL !added Aug,8 2005!
   CODE
   IF Action=AssertMsgAction::PrintEvent
      SELF.PrintEvent('in['& CLIP(filename) &' @'& LineNumber &']  '& argMSG )
      RETURN 1
   END
   RETURN 0


!-----------------------------------------------------------------------------------------!
Debuger.DescribeAction       PROCEDURE(LONG Action) !string
   CODE
   CASE Action
     OF AssertMsgAction::NOMATCH   ; RETURN 'NoMatch'
     OF AssertMsgAction::IGNORE    ; RETURN 'Ignore'
     OF AssertMsgAction::PROMPT    ; RETURN 'Prompt'
     OF AssertMsgAction::AUTOGPF   ; RETURN 'AutoGPF'
     OF AssertMsgAction::PrintEvent; RETURN 'PrintEvent'
   ELSE                            ; RETURN 'UNKNOWN!'
   END


!================================================================
! The DumpQueue Source Was Copied from
!    "Debugging Queues with Excel" by Alan Telford
!    http://www.clarionmag.com/cmag/v5/v5n02debugq.html
! Some changes have been made
!================================================================

!-----------------------------------------------------------------------------------------!
Debuger.DumpQue             PROCEDURE  (STRING argheader, QUEUE argQ, <STRING argFilename>, <STRING argFormat>, <LONG arglimit>, <BYTE argforce>) !,String ,Proc ! Declare Procedure
OMITTED::FileName EQUATE(4)
  !Updated: by Mark Goldberg Dec/22/04
  !           added  argfileName = 'NOFILE' logic to suppress ascii export
  !           added SaveQState/RestoreQState logic
  !           added Format entries of 'X' to suppress a given field
  !           rewrote DumpQue::LoadPictureFormatQ_Rtn to use INSTRING
  !           added '/Clip=on'
  !         by Mark Goldberg Aug/31/06
  !           fixed a leak when DumpQue:ProcedureReturn was called early, but after DumpQ::SaveQState had been called
  !         by Mark Goldberg Oct/27/09 -- fixed argForce when SELF.debugactive false (passed argForce on to .DebugOut
  !Todo: replace ASCII driver with DOS driver or undocumented RTL commands
  !      consider adding conditional equates around the ASCII file work
  !      create an alternate form of format, where fields are explicitly requested vs. excluded

  !WARNING: be aware of numeric pictures that add commas -- when working with the ASCII file



eMaxLineLength  EQUATE(4096)
N_ExportQFile   CSTRING(FILE:MaxFilePath),STATIC
 ! -> c6 command:  PRAGMA('project(#pragma link(C%V%ASC%X%%L%.LIB))'
 !   If you use DRIVER('ASCII','/TAB=-100') and add a tab ('<9>') as separator between the queue fields and give the filename the extension .xls you can open it in Excel without any effort.
ExportQFile     FILE,DRIVER('ASCII','/CLIP = on'),CREATE,NAME(N_ExportQFile)
RECORD            RECORD
LINE                STRING(eMaxLineLength)
                  END
                END

FieldCnt        LONG
Line            CSTRING(eMaxLineLength+1)
LinePrefix      CSTRING( 20)  !'Rec['& r:Ndx &'] '  where max r:Ndx = RECORDS(ArgQ)
RetVal          CSTRING(500)

FormatQ         QUEUE         ! Queue which stores picture formats used when exporting data
Pic               CSTRING(31) !changed to cString May/10/05 so can avoid CLIP()ing
                END
UseFile         BYTE !added Dec/22/04 MG
UseSelf         BYTE !added Dec/23/04 MG Flag indicates if SELF.debugout will be called
Hold:QBuffer    &STRING
Hold:QPtr       LONG

FnameOmitted    BYTE

DQ_Assert       BYTE(TRUE) !used in Assert, True means off

  CODE
  IF SUB(argFormat,1,1)='D' THEN DQ_Assert=FALSE; argFormat=SUB(argFormat,2,LEN(argFormat)) END !added MG May/10/05

  UseSelf = CHOOSE( SELF.debugactive OR argforce)          !fixed MG Dec/24/04 - was backwards
  ODS(SELF.debugfilter & 'Debuger.DumpQue UseSelf['& UseSelf &']  SELF.debugactive['& SELF.debugactive &'] argforce['& argforce &']')

  FnameOmitted = CHOOSE( OMITTED(OMITTED::FileName) OR argFileName='')
  IF FnameOmitted
        UseFile = CHOOSE( SELF.dumpQue_AsciiByDefault         <> DEBUGER::ASCII_NEVER    AND |
                          UPPER(SELF.dumpQue_DefaultFileName) <> DEBUGER::DUMPQUE_NOFILE     |
                        )
  ELSE  UseFile = CHOOSE( UPPER(argFileName)                  <> DEBUGER::DUMPQUE_NOFILE )
  END
  !ASSERT(0,eqDBG&'UseFile['& UseFile &'] FnameOmitted['& FnameOmitted &'] argFileName['& argFileName &'] Default['& SELF.dumpQue_DefaultFileName &']')
!! -- how detect an &= NULL, when we're not working with a reference ?
!!  if not Address(argQ)
!!     Assert(0,eqDBG&'Debuger DumpQue - an invalid Queue was given - returning')
!!     UseSelf = FALSE
!!     RetVal  = 'Invalid Queue Given'
!!  end

  IF ~UseSelf AND ~UseFile
     ODS(SELF.debugfilter & 'Debuger.DumpQue Early Return')
     RetVal = 'no output requested'
     DO DumpQue::ProcedureReturn
  END

  ODS('Debuger.DumpQue argFormat['& argFormat &']')
                                                ! ; ASSERT(0,eqDBG&'DumpQue')
  DO DumpQue::SaveQState                        ! ; ASSERT(0,eqDBG&'DumpQue')
  DO DumpQue::CreateFile_Rtn                    ! ; ASSERT(0,eqDBG&'DumpQue')
  DO DumpQue::CountFieldsInQueue_Rtn            ! ; ASSERT(0,eqDBG&'DumpQue')
  DO DumpQue::LoadPictureFormatQ_Rtn            ! ; ASSERT(0,eqDBG&'DumpQue')
  DO DumpQue::WriteQueueHeader_Rtn              ! ; ASSERT(0,eqDBG&'DumpQue')
  DO DumpQue::WriteQueueRecords_Rtn             ! ; ASSERT(0,eqDBG&'DumpQue')
 !DO DumpQue::RestoreQState                     ! ; ASSERT(0,eqDBG&'DumpQue') !moved Aug/31/06, to ProcedureReturn

  IF UseFile
     CLOSE(ExportQFile)
  END
  RetVal = ''
  DO DumpQue::ProcedureReturn

!------------------------------------------------------------------
DumpQue::ProcedureReturn ROUTINE
  IF UseSelf
     SELF.DebugOut('DumpQ - ' & LinePrefix & argheader & ' - RetVal['& retval &']',,,argforce)
     SELF.DebugOut('DumpQ - ' & LinePrefix & '={42}',,,argforce)
  END
  IF NOT (Hold:QBuffer &= NULL)  !+ Aug/31/06
     DO DumpQue::RestoreQState   !+ Aug/31/06
  END                            !+ Aug/31/06
  RETURN retval

!------------------------------------------------------------------
DumpQue::SaveQState     ROUTINE
  Hold:QBuffer &= NEW STRING( SIZE(argQ))
  ! ODS('Dubugger.DumpQue::SaveQState ADDRESS(Hold:QBuffer)['& ADDRESS(Hold:QBuffer) &']')
  Hold:QBuffer  = argQ
  Hold:QPtr     = POINTER(argQ)

!------------------------------------------------------------------
DumpQue::RestoreQState  ROUTINE
  GET(argQ,Hold:QPtr)
  argQ = Hold:QBuffer
  ! ODS('Dubugger.DumpQue::RestoreQState ADDRESS(Hold:QBuffer)['& ADDRESS(Hold:QBuffer) &']')
  DISPOSE(Hold:QBuffer)
  
  
!------------------------------------------------------------------
DumpQue::CreateFile_Rtn ROUTINE
  IF ~RECORDS(argQ)
    RetVal = 'Queue is empty. No export file produced.'
    DO DumpQue::ProcedureReturn
  END
  IF UseFile
     IF FnameOmitted OR UPPER(CLIP(argFileName)) = DEBUGER::DUMPQUE_PROMPT
       N_ExportQFile = SELF.dumpQue_DefaultFileName

       IF SELF.dumpQue_AsciiByDefault = DEBUGER::ASCII_PROMPT
       ! prompt for a filename if not already provided
          IF ~FILEDIALOG('Export Queue to file ...', N_ExportQFile, 'CSV files (*.csv)|*.csv', FILE:SAVE + FILE:KEEPDIR + FILE:LONGNAME) !10011b)
            RetVal = 'No export file selected from FileDialog'
           DO DumpQue::ProcedureReturn
          END
       END
     ELSE
       N_ExportQFile = argFileName
     END
     CREATE(ExportQFile)
     IF ~ERRORCODE()
       OPEN  (ExportQFile, FileAccessMode:Default)
     END
     IF ERRORCODE()
        RetVal = 'No export file produced.|Error: '&CLIP(ERRORCODE()) & ' ' & CLIP(ERROR())
        DO DumpQue::ProcedureReturn
     END
     IF UseSelf
        SELF.debugout('DumpQ File created - ' & N_ExportQfile,argheader,,argforce)
     END
  END

!------------------------------------------------------------------
DumpQue::CountFieldsInQueue_Rtn ROUTINE
!| Count the number of fields in the queue, and store in FIELDCNT
!| Todo - make this a generic exposed method
  DATA
r:Any ANY
  CODE
  FieldCnt = 0
  LOOP
    r:Any &= WHAT(argQ, FieldCnt+1)
    IF r:Any &= NULL THEN BREAK END
    FieldCnt += 1
  END
  !ASSERT(DQ_Assert,eqDBG&'FieldCnt['& FieldCnt &']')

!------------------------------------------------------------------
DumpQue::LoadPictureFormatQ_Rtn ROUTINE
!| If an optional argFORMAT string exists, then parse this string (using | as delimiter)
!| and store it in FORMATQ to be used when writing out to file.
  DATA
r:Pos     LONG
r:NextPos LONG
r:Ndx     LONG
r:Len     LONG
  CODE
  !Note: in C6 could use IsGroup() logic to help avoid some problems... (should really be done when building the FormatQ) -- could use the Header instead (prepend with a space...)
  IF argFormat <> ''
    r:Pos = 1
    r:Len = LEN(argformat)
    LOOP r:Ndx = 1 TO FieldCnt
      IF r:Pos > r:Len THEN BREAK END
      r:NextPos = INSTRING('|',argFormat,1,r:Pos)
      IF ~r:NextPos
          r:NextPos = r:Len + 1
          FormatQ.Pic = ''       !changed June/3/05
      ELSE
          FormatQ.Pic = argFormat[ r:Pos : r:NextPos - 1]
      END
      ADD(FormatQ)
      r:Pos = r:NextPos + 1
    END
  END

!  !---- self debugging ----!
!  SELF.DebugOut('DumpQue::LoadPictureFormatQ_Rtn FormatQ  FieldCnt['& FieldCnt &'] Format['& argFormat &']')
!  LOOP r:Ndx = 1 TO RECORDS(FormatQ)
!     GET(FormatQ, r:NDX)
!     SELF.DebugOut('FormatQ ['& r:NDX &'] Pic['& CLIP(FormatQ.Pic) &'] is null['& CHOOSE( FormatQ.Pic = '') &']')
!  end
!  !---- self debugging ----! -end

!------------------------------------------------------------------
DumpQue::WriteQueueHeader_Rtn ROUTINE
!| Write a header line to export file, with the NAMES of each column
  DATA
r:Ndx      LONG
r:Comma    CSTRING(2) !no auto
r:Quote    CSTRING('"')
                  COMPILE('**++** _C61_Plus_',_C61_)
r:DimCount LONG
                 !END-COMPILE('**++** _C61_Plus_',_C61_)
  CODE
  IF UseSelf
     SELF.DebugOut('DumpQ - ' & LinePrefix & '={42}',,,argforce)
     SELF.DebugOut('DumpQ - ' & LinePrefix & argHeader,,,argforce )
     r:Quote = ''
  END

  Line = ''           ! ; ASSERT(0,eqDBG&'DumpQue')
  LOOP r:Ndx = 1 TO FieldCnt
    GET(FormatQ, r:Ndx)
    IF ~ERRORCODE() AND FormatQ.Pic <> '' AND FormatQ.Pic[1]='X' THEN CYCLE END !added MG Dec/24/04
    !Line = Line & CHOOSE(~Line,'', ',') & '"' & CLIP(WHO(argQ, r:Ndx)) & '"'
                                                                             OMIT('**++** _C61_Plus_',_C61_)
    Line = Line & r:Comma & r:Quote & CLIP(WHO(argQ, r:Ndx)) & r:Quote
    r:Comma = ','
                                                                        !END-OMIT('**++** _C61_Plus_',_C61_)
                                                                          COMPILE('**++** _C61_Plus_',_C61_)
    IF HOWMANY(argQ,r:Ndx) = 1
       Line = Line & r:Comma & r:Quote & CLIP(WHO(argQ, r:Ndx)) & r:Quote
       r:Comma = ','
    ELSE
       LOOP r:DimCount = 1 to HOWMANY(argQ,r:Ndx)
         Line = Line & r:Comma & r:Quote & CLIP(WHO(argQ, r:Ndx)) & '['& r:DimCount &']' & r:Quote
         r:Comma = ','
       END
    END
                                                                        !END-COMPILE('**++** _C61_Plus_',_C61_)
    !ODS(SELF.DebugFilter & 'DumpQ Field['& r:NDX &'] WHO['& CLIP(WHO(argQ, r:NDX)) &']  Type['& SELF.GetVarType( WHAT(argQ, r:Ndx) ) &']=['& SELF.DescribeDataType(  SELF.GetVarType( WHAT(argQ, r:Ndx) ) )&']<13,10>')

                       ! ; ASSERT(0,eqDBG&'DumpQue r:Ndx['& r:Ndx &']')
  END
  DO DumpQue::WriteLine! ; ASSERT(0,eqDBG&'DumpQue')


!------------------------------------------------------------------
DumpQue::WriteLine            ROUTINE
  IF UseSelf
     SELF.DebugOut('DumpQ - ' & LinePrefix & Line,,,argforce )
  END
  IF UseFile
     ExportQFile.Line = Line
     ADD(ExportQFile)
  END

!------------------------------------------------------------------
DumpQue::WriteQueueRecords_Rtn ROUTINE
!| Write contents of Queue to file, one record at a time,
!| enclosing strings in "" (quotes), and using the format picture if supplied
  DATA
r:Ndx        LONG
r:FieldNdx   LONG
r:Any        ANY
!r:Delim     CSTRING(2)
r:NextValue  CSTRING(SIZE(LINE))
r:tmp        LONG
r:RecPicture CSTRING(10)
                               COMPILE('**++** _C61_Plus_',_C61_)
r:DimCount   LONG
                          !END-COMPILE('**++** _C61_Plus_',_C61_)
  CODE
  r:RecPicture = '@N_' & INT(LOG10(RECORDS(argQ))) + 1
  LOOP r:Ndx = 1 TO RECORDS(argQ)
                     !      ; ASSERT(0,eqDBG&'DumpQue r:Ndx['& r:Ndx &']')
    GET(argQ, r:Ndx)
    IF r:ndx > arglimit AND arglimit > 0
       IF UseSelf
          SELF.debugout('Max of ' & arglimit & ' Que records dumped',argheader,,argforce)
       END
       BREAK
    END
    LinePrefix = 'Rec['& FORMAT(r:Ndx,r:RecPicture) &'] '
    Line       = ''
    !r:Delim    = ''

                    !; ASSERT(0,eqDBG&'DumpQue r:Ndx['& r:Ndx &'] r:FieldNdx['& r:FieldNdx &'] of FieldCnt['& FieldCnt &'] ')
                    !; ASSERT(0,eqDBG&'Howmany['& HOWMANY(argQ,r:FieldNdx) &']')
    LOOP r:FieldNdx = 1 TO FieldCnt
      GET(FormatQ, r:FieldNdx)
                                                                             OMIT('**++** _C61_Plus_',_C61_)
      r:Any &= WHAT(argQ, r:FieldNdx)
                                                                        !END-OMIT('**++** _C61_Plus_',_C61_)
                                                                         COMPILE('**++** _C61_Plus_',_C61_)
      LOOP r:DimCount = 1 TO HOWMANY(argQ,r:FieldNdx)  !<--- added July,18,2005
         r:Any &= WHAT(argQ, r:FieldNdx,r:DimCount )
         !ASSERT(0,eqDBG&'r:FieldNdx['& r:FieldNdx &'] r:Any['& r:Any &']')
         !ASSERT(0,eqDBG&'r:FieldNdx['& r:FieldNdx &'] r:Any['& CLIP(r:Any) &']')
                                                                    !END-COMPILE('**++** _C61_Plus_',_C61_)


         !Note: in C6 could use IsGroup() logic to help avoid some problems... (should really be done when building the FormatQ)
         IF ~ERRORCODE() AND FormatQ.Pic <> ''
           ! Use picture format is one is supplied (eg date/time)
           CASE FormatQ.Pic[1]
             OF 'X'; CYCLE                                        !added MG Dec/22/04
             OF ' '; r:NextValue = FormatQ.Pic                    !added MG May/10/05 new policy
           ELSE    ; r:NextValue = FORMAT(r:Any, FormatQ.Pic)
           END
           !Line = Line & CHOOSE(~Line,'', ',') & FORMAT(r:Any, FormatQ.Pic)
         ELSIF ISSTRING(r:Any)
           ! enclose String data types in quotes ""
           r:NextValue = '"' & CLIP(r:Any) & '"'
           !Line = Line & CHOOSE(~Line,'', ',') & '"' & CLIP(r:Any) & '"'
         ELSE
           ! Non-string data can be exported as is
           r:NextValue = r:Any
           !Line = Line & CHOOSE(~Line,'', ',') & r:Any
         END
         !Line = Line & r:Delim & r:NextValue
         !r:Delim = ','
         Line = Line & r:NextValue & ','  !leaves a comma after the last field..., and is inneficient, should change to a LHS string slice ....
                                                                        COMPILE('**++** _C61_Plus_',_C61_)
      END !loop .. HOWMANY
                                                                  !END-COMPILE('**++** _C61_Plus_',_C61_)
    END !loop r:FieldNdx
                                         !; ASSERT(0,eqDBG&'DumpQue')
    r:tmp = LEN(CLIP(Line))              !; ASSERT(0,eqDBG&'DumpQue')
    IF r:tmp
       Line[ r:tmp ] = '<32>' !space
    END
                                         !; ASSERT(0,eqDBG&'DumpQue')
    DO DumpQue::WriteLine                !; ASSERT(0,eqDBG&'DumpQue')
  END
  LinePrefix = ''

!-----------------------------------------------------------------------------------------!
Debuger.Set_dumpQue_DefaultFileName PROCEDURE(STRING argNewFname) !added Jan/12/05 MG
  CODE
  SELF.dumpQue_DefaultFileName = argNewFname

!-----------------------------------------------------------------------------------------!
Debuger.Set_dumpQue_AsciiByDefault  PROCEDURE(BYTE    argNewValue) !enumerated: DEBUGER::ASCII_*
RetVal  LIKE(Debuger.dumpQue_AsciiByDefault) !BYTE
  CODE
  RetVal = SELF.dumpQue_AsciiByDefault
  CASE argNewValue
    OF DEBUGER::ASCII_NEVER
  OROF DEBUGER::ASCII_PROMPT
  OROF DEBUGER::ASCII_NOPROMT

       SELF.dumpQue_AsciiByDefault = argNewValue
  ELSE
       MESSAGE('Invalid new value for SELF.dumpQue_AsciiByDefault['& argNewValue &']','Programmer Error')
  END
  RETURN RetVal


!=================== Written by Skip Williams (received by MG: Jan/7/05) =========================!

!-----------------------------------------------------------------------------------------!
Debuger.ClearLog            PROCEDURE()   !Requires Debugview 4.3 or greater
!From:  http://www.sysinternals.com/ntw2k/freeware/debugview.shtml
!       Clear-output string: When DebugView sees the special debug output string "DBGVIEWCLEAR" it clears the output.

!Note: If this doesn't appear to work, then you are either:
!          a) using an older version of debugview
!       OR b) you are filtering the message

   CODE
   !ODS('`Debuger.ClearLog disabled')
   SELF.DebugOut('DBGVIEWCLEAR')
   SELF.DebugOut('Log Cleared') !+Sept/19/09



!-----------------------------------------------------------------------------------------!
Debuger.CheckError           PROCEDURE(STRING argHeader, BYTE argShowMessage, BYTE argForceDebug)   ! checks ERRORCODE()
svErrorCode LONG
svError     CSTRING(200)
   CODE
   svErrorCode = ERRORCODE()
   IF svErrorCode AND (SELF.debugactive OR argForceDebug)  !or argForceDebug !added MG: Aug 10,2005
      svError     = ERROR()
      SELF.debugout('ERROR: (' & svErrorCode & ') ' & svError,argHeader,argShowMessage,argForceDebug)
   END
   RETURN svErrorCode
!-----------------------------------------------------------------------------------------!
Debuger.DumpControls        PROCEDURE(<*WINDOW xWin>, SIGNED xLowFEQ=-MAX:FEQ, SIGNED xHiFEQ=MAX:FEQ)
  !Update: Sept/18/06 :: MG, Sort FEQs
  !        Mar /24/11 :: Refactored into routines
PARAM:xWin  EQUATE(2)
CurrFEQ     LONG !no ,AUTO
HoldTarget  &WINDOW

FEQs        QUEUE
FEQ           LONG
            END
  CODE
  DO DumpControls:PushTarget
  DO DumpControls:BuildFEQList
  DO DumpControls:GenerateOutputFromFEQList
  DO DumpControls:CleanUpFEQList
  DO DumpControls:PopTarget

!-----------------------------------
DumpControls:PushTarget     ROUTINE
  IF OMITTED(PARAM:xWin)           !C6 preferred notation:  !  IF OMITTED(xWin)
    xWin  &= SYSTEM{PROP:Target}
    HoldTarget &= NULL !+Sept/21/09
  ELSE
    HoldTarget &= SYSTEM{PROP:Target}
    SETTARGET( xWin )  !added so could make use of .GetFEQDescr() which assumes the current window
  END

  !IF xHiFEQ = -1 THEN CLEAR(xHiFEQ,1) END !changed default value to the CLEAR(x,1) value

!-----------------------------------
DumpControls:BuildFEQList    ROUTINE
  LOOP
    CurrFEQ = xWin{PROP:NextField, CurrFEQ}
    IF ~CurrFEQ
      BREAK
    END
    IF INRANGE( CurrFEQ, xLowFEQ, xHiFEQ)
       FEQs.FEQ = CurrFEQ
       ADD(FEQs)
       !ODS('Debuger.DumpControls FEQ['& CurrFEQ &']')
    END
  END
  SORT(FEQs, FEQs.FEQ)

!-----------------------------------
DumpControls:GenerateOutputFromFEQList       ROUTINE
  SELF.DebugOut('DumpControls ===[start]={42}')
  SELF.DebugOut('DumpControls for Window['& xWin{prop:Text} &'] FEQ Range['& xLowFEQ &' to '& xHiFEQ &']')

  LOOP CurrFEQ = 1 TO RECORDS(FEQs)
    GET(FEQs, CurrFEQ)
    SELF.ShowControl(xWin, FEQs.FEQ, SELF.GetFEQDescr(FEQs.FEQ) )
  END

  SELF.DebugOut('DumpControls ===[end]={42}')


!-----------------------------------
DumpControls:CleanUpFEQList  ROUTINE
  FREE(FEQs)

!-----------------------------------
DumpControls:PopTarget       ROUTINE
  IF NOT HoldTarget &= NULL
    SETTARGET( HoldTarget )
  END
!-----------------------------------------------------------------------------------------!
Debuger.ShowControl         PROCEDURE(*WINDOW xWin, SIGNED xFEQ, <STRING xDescr>)
  !Update: Sept/18/06 :: MG, rearranged, so columns will be more consistent (easier to read the output)
PARAM:xDESCR EQUATE(4)
  CODE
  SELF.Message('   FEQ[' & xFEQ &']'            &       | ! I was going to call SELF.GetFEQDescr( xFEQ ) but that assumes the current window
               '   Hide['& xWin$xFEQ{Prop:Hide} & ']' & |
               '   AT('  & xWin$xFEQ{Prop:AT,1} & ','   |
                         & xWin$xFEQ{Prop:AT,2} & ','   |
                         & xWin$xFEQ{Prop:AT,3} & ','   |
                         & xWin$xFEQ{Prop:AT,4} & ')' & |
               '   Type['& xWin$xFEQ{Prop:Type} & ']=['& SELF.DescribeType(xWin$xFEQ{Prop:Type}) &']' & | ! might be a number, see CREATE:* equates
               '   Text['& xWin$xFEQ{Prop:Text} & ']' & |
               CHOOSE( OMITTED(PARAM:xDESCR),'','  xDescr[' & xDescr &']')  & |
               ' Parent['& xWin$xFEQ{Prop:Parent} &']' &|
                 ' Use ['& xWin$xFEQ{PROP:Use}    &']' &|  ! < -- added Mar/4/2010
          ' UseAddress ['& xWin$xFEQ{PROP:UseAddress}  &']' &|  ! < -- added Mar/4/2010
          |!   ' Follows['& xWin$xFEQ{PROP:Follows }    &']' &|  <-- WRITE-ONLY property....
                 ' Skip['& xWin$xFEQ{PROP:Skip}        &']' &| 
                  ' Trn['& xWin$xFEQ{PROP:Trn}   &']' &|                   
               '')
!-----------------------------------------------------------------------------------------!
Debuger.DescribeType        PROCEDURE(LONG xType)
RetVal CSTRING(25)
CREATE:_ComboButton   EQUATE(CREATE:combo + 0100H)  !see NG:  Softvelocity.clarion.documentation  Thread:"What is {prop:type}=271"  Sept-18-2006
  CODE
  CASE xType
    OF CREATE:sstring      ; RetVal = 'create:SSTRING'
    OF CREATE:string       ; RetVal = 'create:STRING'
    OF CREATE:image        ; RetVal = 'create:IMAGE'
    OF CREATE:region       ; RetVal = 'create:REG' & 'ION' !hopefully R E G I O N won't trigger the code folding bugs
    OF CREATE:line         ; RetVal = 'create:LINE'
    OF CREATE:box          ; RetVal = 'create:BOX'
    OF CREATE:ellipse      ; RetVal = 'create:ELLIPSE'
    OF CREATE:entry        ; RetVal = 'create:ENTRY'
    OF CREATE:button       ; RetVal = 'create:BUTTON'
    OF CREATE:prompt       ; RetVal = 'create:PROMPT'
    OF CREATE:option       ; RetVal = 'create:OPTION'
    OF CREATE:check        ; RetVal = 'create:CHECK'
    OF CREATE:group        ; RetVal = 'create:GROUP'
    OF CREATE:list         ; RetVal = 'create:LIST'
    OF CREATE:combo        ; RetVal = 'create:COMBO'
    OF CREATE:spin         ; RetVal = 'create:SPIN'
    OF CREATE:text         ; RetVal = 'create:TEXT'
    OF CREATE:custom       ; RetVal = 'create:CUSTOM'
    OF CREATE:menu         ; RetVal = 'create:MENU'
    OF CREATE:item         ; RetVal = 'create:ITEM'
    OF CREATE:radio        ; RetVal = 'create:RADIO'
    OF CREATE:menubar      ; RetVal = 'create:MENUBAR'
    OF CREATE:application  ; RetVal = 'create:APPLICATION'
    OF CREATE:window       ; RetVal = 'create:WINDOW'
    OF CREATE:report       ; RetVal = 'create:REPORT'
    OF CREATE:header       ; RetVal = 'create:HEADER'
    OF CREATE:footer       ; RetVal = 'create:FOOTER'
    OF CREATE:break        ; RetVal = 'create:BREAK'
    OF CREATE:form         ; RetVal = 'create:FORM'
    OF CREATE:detail       ; RetVal = 'create:DETAIL'
    OF CREATE:ole          ; RetVal = 'create:OLE'
    OF CREATE:droplist     ; RetVal = 'create:DROPLIST'
    OF CREATE:dropcombo    ; RetVal = 'create:DROPCOMBO'
    OF CREATE:progress     ; RetVal = 'create:PROGRESS'
    OF CREATE:sheet        ; RetVal = 'create:SHEET'
    OF CREATE:tab          ; RetVal = 'create:TAB'
    OF CREATE:panel        ; RetVal = 'create:PANEL'
                                                   COMPILE('**++** _C60_Only_',_C60_)
    OF CREATE:rtf          ; RetVal = 'create:RTF'
                                              !END-COMPILE('**++** _C60_Only_',_C60_)
    OF CREATE:sublist      ; RetVal = 'create:SUBLIST'     ! found via  ?Combo_or_List_WithDrop{prop:ListFEQ)
    OF CREATE:toolbar      ; RetVal = 'create:TOOLBAR'
    OF CREATE:_ComboButton ; RetVal = 'create:COMBOBUTTON' ! UN-OFFICIAL the control found via: ?Combo{prop:ButtonFEQ}
  ELSE                     ; RetVal = 'Uknown['& xType &']'
  END
  RETURN RetVal
!-----------------------------------------------------------------------------------------!
Debuger.GetVarType          PROCEDURE(*? xaVar) !,LONG
!--- see softvelocity.public.clarion6 "Variable Data Type" Sept,12,2006 (code posted by dedpahom) -----!
!--- the following links are in Clarion and RUSSIAN -- can we get a translation please!
!http://www.clarionlife.net/content/view/159/29/
!http://www.clarionlife.net/content/view/153/29/
!translated pages seem to say that this will GPF in C50, but is ok in C55
UFO                             &TUFO_CallInterface
  CODE 
  ! COMPILE('***',_C70_) 
  !   MESSAGE('debuger.GetVarType has not been implemented')
  !   RETURN 0
  ! !END-COMPILE('***',_C70_)
    
  ! OMIT('***',_C70_)
  UFO &= ADDRESS(xaVar)
  RETURN UFO._Type( ADDRESS(xaVar) ) 
  !RETURN UFO._Type() !(42) !<-- my guess is that this arg. is ignored. -- confirmed
  !END-OMIT('***',_C70_)
!-----------------------------------------------------------------------------------------!
Debuger.DescribeDataType    PROCEDURE(LONG xType) !,STRING  !<-- assumes a DATATYPE:*  (see equates.clw)
RetVal CSTRING(25) !DataType:PDECIMAL has a length of 17
  CODE
  CASE xType                                               ! ITEMIZE(1),PRE(DataType)
    OF DataType:BYTE     ; RetVal = 'DataType:BYTE'         ! EQUATE
    OF DataType:SHORT    ; RetVal = 'DataType:SHORT'        ! EQUATE
    OF DataType:USHORT   ; RetVal = 'DataType:USHORT'       ! EQUATE
    OF DataType:DATE     ; RetVal = 'DataType:DATE'         ! EQUATE
    OF DataType:TIME     ; RetVal = 'DataType:TIME'         ! EQUATE
    OF DataType:LONG     ; RetVal = 'DataType:LONG'         ! EQUATE
    OF DataType:ULONG    ; RetVal = 'DataType:ULONG'        ! EQUATE
    OF DataType:SREAL    ; RetVal = 'DataType:SREAL'        ! EQUATE
    OF DataType:REAL     ; RetVal = 'DataType:REAL'         ! EQUATE
    OF DataType:DECIMAL  ; RetVal = 'DataType:DECIMAL'      ! EQUATE
    OF DataType:PDECIMAL ; RetVal = 'DataType:PDECIMAL'     ! EQUATE
    OF DataType:BFLOAT4  ; RetVal = 'DataType:BFLOAT4'      ! EQUATE(13)
    OF DataType:BFLOAT8  ; RetVal = 'DataType:BFLOAT8'      ! EQUATE
    OF DataType:STRING   ; RetVal = 'DataType:STRING'       ! EQUATE(18)
    OF DataType:CSTRING  ; RetVal = 'DataType:CSTRING'      ! EQUATE
    OF DataType:PSTRING  ; RetVal = 'DataType:PSTRING'      ! EQUATE
    OF DataType:MEMO     ; RetVal = 'DataType:MEMO'         ! EQUATE
    OF DataType:BLOB     ; RetVal = 'DataType:BLOB'         ! EQUATE(27)
  ELSE                   ; RetVal = 'Unknown['& xType &']'
                           !31 seems to be returned for references (and ANY)  (c6.9056)
  END
  RETURN RetVal
!-----------------------------------------------------------------------------------------!
Debuger.GroupDiff           PROCEDURE(*GROUP xaA, *GROUP xaB, STRING xDescr, STRING xDescrA, STRING xDescrB)
  !Purpose show each component of the groups that do not match.
  !Both groups are assume to be the same structure
  !Currently Tests WHAT() only, not WHO() for differences.
  !Todo -- add field formatting support

_WhatA     ANY
_WhatB     ANY
_Where     LONG(1)
!CurrDim   LONG   !<--- C61 + support HOWMANY
WhatDiffCount  LONG(0)
  CODE

  IF xaA = xaB
     SELF.DebugOut(xDescr & ' both groups are the same')
  ELSE
     LOOP
       _WhatA &= WHAT(xaA, _Where)
       _WhatB &= WHAT(xaB, _Where)

       IF _WhatA &= NULL OR _WhatB &= NULL
          IF NOT (_WhatA &= NULL AND _WhatB &= NULL)
             SELF.DebugOut(xDescr & 'Mismatched Group structures, found at Field['& _Where &']')
          END
          BREAK
       END
       IF _WhatA <> _WhatB
          IF WHO(xaA, _Where) <> WHO(xaB, _Where)
             SELF.DebugOut(xDescr & ' Difference '& xDescrA &'['& WHO(xaA, _Where) &']=['& _WhatA &'] ' & |
                                                    xDescrB &'['& WHO(xaB, _Where) &']=['& _WhatB &'] '   |
                           )
          ELSE
             SELF.DebugOut(xDescr & ' Difference  ['& WHO(xaA, _Where) &']  '& xDescrA &'=['& _WhatA &'] ' & |
                                                                               xDescrB &'=['& _WhatB &'] '   |
                           )
          END
          WhatDiffCount += 1
       END

       _Where += 1
     END !loop

     SELF.DebugOut(xDescr & ' ['& WhatDiffCount &'] Difference(s) found')
  END
!-----------------------------------------------------------------------------------------!
Debuger.DumpGroup           PROCEDURE(*GROUP xaGroup, STRING xLinePrefix)
_What      ANY
_Where     LONG(1)
!CurrDim   LONG   !<--- C61 + support HOWMANY
  !Todo -- add field formatting support
  CODE
  LOOP
    _What  &= WHAT(xaGroup,_Where)
    IF _What &= NULL
       BREAK
    END

    SELF.DebugOut(xLinePrefix &' '& WHO(xaGroup,_Where) &' = ['& _What &']')
    _Where += 1
  END
!-----------------------------------------------------------------------------------------!
Debuger.DiffBYTEs           PROCEDURE(*GROUP      xA , *GROUP      xB , STRING xDescr, STRING xDescrA, STRING xDescrB) !created Aug/13/07 by MG
CurrBYTE LONG
MaxA     LONG
MaxB     LONG
MaxAB    LONG
MinAB    LONG
!BYTEsA   BYTE,DIM(SIZE(XA)+1)
!BYTEsB   BYTE,DIM(SIZE(XB)+1)
sA       &STRING !STRING(SIZE(xA)),OVER(xA)
sB       &STRING !STRING(SIZE(xB)),OVER(xB)

  CODE
  sA &= NEW STRING(SIZE(xA))
  sA  = xA
  sB &= NEW STRING(SIZE(xB))
  sB  = xB

  !BYTEsA[] = xA
  !BYTEsB[] = xB
  MaxA = SIZE(xA)
  MaxB = SIZE(xB)
  IF MaxA > MaxB
       MaxAB = MaxA; MinAB = MaxB
  ELSE MaxAB = MaxB; MinAB = MaxA
  END
  SELF.DebugOut('DiffBYTEs ['& xDescr &'] A['& xDescrA &']size['& MaxA &']   B['& xDescrB &']size['& MaxB &']  vvv[start]')
  LOOP CurrBYTE = 1 TO MinAB
    IF VAL(sA[CurrBYTE]) <> VAL(sB[CurrBYTE])
       SELF.DebugOut('[' & FORMAT(CurrBYTE,@N05)&'] A['& VAL(sA[CurrBYTE]) &'] B['& VAL(sB[CurrBYTE]) &']')
    END
  END
  IF MaxA > MaxB
     LOOP CurrBYTE = MinAB + 1 TO MaxA
       SELF.DebugOut('[' & FORMAT(CurrBYTE,@N05)&'] A['& VAL(sA[CurrBYTE]) &'] (A Larger)')
     END
  ELSIF MaxA < MaxB
     LOOP CurrBYTE = MinAB + 1 TO MaxB
       SELF.DebugOut('[' & FORMAT(CurrBYTE,@N05)&'] B['& VAL(sB[CurrBYTE]) &'] (B Larger)')
     END
  END
  SELF.DebugOut('DiffBYTEs ['& xDescr &'] A['& xDescrA &'] B['& xDescrB &'] ^^^[end]')
!-----------------------------------------------------------------------------------------!
Debuger.StringAsVal         PROCEDURE(STRING xString,LONG EndAt=-1)
MaxPerLine LONG(16)
CurrByte   LONG,AUTO
  CODE
  IF EndAt = -1 OR EndAt > SIZE(xString)
     EndAt = SIZE(xString)
  END
  LOOP CurrByte = 1 TO EndAt
     SELF.DelayOut('['& FORMAT(VAL(xString[CurrByte]),@N_3) &'] ')
     IF CurrByte % MaxPerLine = 0
        SELF.Debugout('')
     END
  END
!EndRegion Debuger Methods


!Region Rem'd
!!-----------------------------------------------------------------------------------------!
!Debuger.Hexout  Procedure(String argToHexString,String argHeadr)
!!-----------------------------------------------------------------------------------------!
!i short
!
!   CODE
!   if SELF.debugactive
!      i = len(argToHexString) * 2 ! make receiving string twice as large as passed string
!      do hexit
!   end
!
!!-----------------------------------------------------------------------------------------!
!hexit ROUTINE  ! in a ROUTINE here so a dynamic string size can be used
!!-----------------------------------------------------------------------------------------!
!   data
!value  long
!result STRING(i)  !<--- Interesting Construct (not valid in C55eeF) (valid in 9015 and presumably all of C6) (see c:\cla\_mg\test\StrAlloc\StrAlloc.clw for exploration of this language contruct)
!hx     cSTRING(10)
!
!   CODE
!   loop i# = 1 to len(argToHexString)
!      value = val(argToHexString[i#])
!      LtoA(Value,hx,16)    ! convert to base 16 to a string
!      result = CLIP(result) & upper(hx)
!   end
!
!   SELF.debugout(result,argHeadr)
!EndRegion Rem'd
