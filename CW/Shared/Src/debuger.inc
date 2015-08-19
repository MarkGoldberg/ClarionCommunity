!ABCIncludeFile(DEBUGER)

       COMPILE('****ASCII',_C60_)
    PRAGMA ('link (C%V%ASC%X%%L%.LIB)')
  !END-COMPILE('****ASCII',_C60_)
       OMIT('****ASCII',_C60_)
  INCLUDE('_ver_.inc'),ONCE  !sets _VER_ based on the CW version
LinkClass_Ascii            CLASS,TYPE,LINK('C' & _VER_ & 'ASCX.LIB'); END  !Purpose is to link in the ASCII driver
  !END-OMIT('****ASCII',_C60_)


!-Rely On Once due to IDE Navigation Bugs-!     OMIT('*** ifdef Debuger ***',ifDef_Debuger)
!-Rely On Once due to IDE Navigation Bugs-!ifDef_Debuger  EQUATE(1)
     INCLUDE('TUFO.INT'),ONCE

                                    SECTION('eqDBG')
eqDBG                      EQUATE('<4,2,7>') ! CTRL+D, CTRL+B, CTRL+G   !added 5/22/03 by Mark Goldberg (to be used by Assert(x,y) as in Assert(0,eqDBG&'my message')
                                    SECTION('eqDBG -end')

MAXMSGLEN                  EQUATE(1000) !MG: I doubt that this is an OS limitation
MAX:FEQ                    EQUATE(2147483647) !equivalent of CLEAR(Long,1) = 2^31 - 1

DEBUGER::CLEAR_CMDLINE     EQUATE(0)
DEBUGER::CLEAR_ALWAYS      EQUATE(1)
DEBUGER::CLEAR_NEVER       EQUATE(2)

DEBUGER::DISABLED          EQUATE(0)
DEBUGER::ENABLED           EQUATE(1)
DEBUGER::ENABLE_CMDLINE    EQUATE(2)

DEBUGER::DUMPQUE_NOFILE    EQUATE('NOFILE')
DEBUGER::DUMPQUE_PROMPT    EQUATE('PROMPT') !Note: if an empty string is passed, and self.dumpQue_AsciiByDefault then will prompt too

DEBUGER::ASCII_NEVER       EQUATE(0)
DEBUGER::ASCII_PROMPT      EQUATE(1)
DEBUGER::ASCII_NOPROMT     EQUATE(2)


qtUserEventName            QUEUE,TYPE
EventEquate                  LONG
EventName                    CSTRING(40) !~Mar/19/08 MG
                           END


!see EQUATES.CLW !Match Flag Values (but since the file has no IFDEF logic, we can't rely on it)
! Match:Simple             EQUATE(0)
! Match:Wild               EQUATE(1)
! Match:Regular            EQUATE(2)
! Match:Soundex            EQUATE(3)
! Match:NoCase             EQUATE(10H)   ! May be added to Simple,Wild and Regular

AssertMsgAction            ITEMIZE,PRE(AssertMsgAction:)
NOMATCH    EQUATE(0)  !will have the form AssertMsgAction::NoMatch
IGNORE     EQUATE(1)
PROMPT     EQUATE(2)
AUTOGPF    EQUATE(3)
!AssetMsgAction::EVALUATE  EQUATE(4)
PrintEvent EQUATE(5)
User       EQUATE(100)
                           END



qtAssertMessages           QUEUE,TYPE
Priority                     LONG
szMSG                        &CSTRING
Action                       LONG
MatchMode                    LONG !should hold an AssetMsgAction::* value
                           END

!==========================================================================================================
Debuger                   CLASS,TYPE,MODULE('Debuger.CLW'),LINK('Debuger.CLW') !,_DebugerLinkMode_),DLL(_DebugerDllMode_)
!Region Properties
debugfilter                 STRING(1) !('`')  !Prepended to OutputDebugString calls, to facilitate FILTERING in debugviewer.exe
debugactive                 BYTE
pgmname                     CSTRING(25)
osmode                      BYTE
Duplicates                  LONG
NumberSame                  LONG
Lastmsg                     CSTRING(MAXMSGLEN)
Thismsg                     CSTRING(MAXMSGLEN)
ShowMessageWindow_Beep      BYTE(FALSE) 
DelayActive                 BYTE(FALSE)
DelayBody                   CSTRING(MAXMSGLEN)
DelayHeader                 CSTRING(MAXMSGLEN)
AppendCRLF                  BYTE     
EventOffset                 LONG     
dumpQue_DefaultFileName     CSTRING(FILE:MaxFilePath) 
dumpQue_AsciiByDefault      BYTE   !enumerated: DEBUGER::ASCII_*
UserEventNameQ              &qtUserEventName
AssertMessagesQ             &qtAssertMessages
!-----------------------
!EndRegion Properties
!Region Methods

mg_init                     PROCEDURE(STRING ProgramName)

!!!<summary>Initialized the class
!!!  <see cref="AppendCRLF"/>
!!!</summary>
!!!<param name='ProgramName'>Program Name</param>
!!!<param name='DebugAways'><para>Enumerated, see DEBUBUER:: equates, defaults to DEBUGER::ENABLE_CMDLINE</para>
!!!                                     <para>if true then debuger on regardless of project debug setting</para>
!!!                                    <para>'/Debuger' on the command line will also turn on the debuger functions for that launch</para></param>
!!!<param name='DuplicateCount'>Default=50 number of duplicate debug messages in a row before warning message.
!!!                                        if zero, then do not track duplicates</param>
!!!<param name='argClear'>Enumerated, see DEBUGER::CLEAR_*  Defaults to NEVER
!!!                       will send message to DebugView to clear itself (requires ver 4.3 or higher to work)</param>
!!!<param name='argAsciiByDefault'>Enumerated, see DEBUGER:ASCII_, Defaults to PROMPT 
!!!                                used to control default behavior of dumpQueue when argFilename is omitted</param>
Init                        PROCEDURE(STRING ProgramName,BYTE DebugAlways=DEBUGER::ENABLE_CMDLINE ,SHORT DuplicateCount=50, BYTE argClear=DEBUGER::CLEAR_NEVER, BYTE argAsciiByDefault=DEBUGER::ASCII_PROMPT)

CONSTRUCT                   PROCEDURE()
DESTRUCT                    PROCEDURE()
Kill                        PROCEDURE()
ClearLog                    PROCEDURE()  ! clear the debugview screen

Message                     PROCEDURE(STRING body,<STRING header>,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE) !simply calls .DebugOut, added for compatibility to Russ's version of the Debuger

!                           Parm 1 = body of message
!                           Parm 2 = message header
!                           Parm 3 = if not FALSE then a message() will be issued along with the debugoutstriung
!                           Parm 4 = if not FALSE then the debugout will be issued regardless of debug setting.
Debugout                    PROCEDURE(STRING body,<STRING header>,BYTE ShowMessage=FALSE,BYTE ForceDebug=FALSE)

DelayOut                    PROCEDURE(STRING body,<STRING header>,BYTE argForceDebug=FALSE)
Debugout_splitby            PROCEDURE(STRING argBody,STRING argSplitBy, <STRING argHeader>,BYTE argShowMessage=FALSE,BYTE argForceDebug=FALSE) !+Dec/7/08 (MG)


DebugBreak                  PROCEDURE()  ! 32bit api call (does nothing in 16bit)

ShowMessageWindow           PROCEDURE(STRING argBody, STRING argHeader)
!                           note: self.ShowMessageWindow_Beep FALSE->no beep ELSE beep

GetEventDescr_WM            PROCEDURE(LONG   argEvent   ),STRING
GetEventDescr               PROCEDURE(LONG   argEvent=-1),STRING     !Default value implies EVENT()
GetFEQDescr                 PROCEDURE(SIGNED argFEQ=-MAX:FEQ),STRING !Default value implies FIELD()  
PrintEvent                  PROCEDURE(<STRING argHeader>,<BYTE argForceDebug>)

Set_AssertHook2             PROCEDURE
Clear_Module_Debuger        PROCEDURE
Set_Module_Debuger          PROCEDURE

!-----------------------------------------------------------------------------------------!
!!!<Summary>
!!!  Send the Contents of a Queue to OutputDebugString or to a File
!!!</Summary>
!!!<param name='argHeader'>message header</param>
!!!<param name='argQ'>Queue to be dumped</param>
!!!<param name='argFileName'>3 - filename to be written too (if omitted, a prompt for filename will be issued</param>
!!!<param name='argFormat'>Picture format, can be used to filter columns form the queue</param>
!!!<param name='argLimit'>Maximum number of queue records to dump or 0 or Omit for ALL</param>
!!!<param name='argForce'>if not omitted and = true then the DumpQ will be issued regardless of debug setting.</param>
DumpQue                     PROCEDURE  (STRING argheader, QUEUE argQ, <STRING argFilename>, <STRING argFormat>, <LONG arglimit>, <BYTE argforce>),STRING,PROC


!!!<Summary>
!!!   Show message only when there is an error (prepend ErrorCode & Error to message)
!!!</Summary>
!!!<param name='argHeader'>message header</param>
!!!<param name='argShowMessage'>if not omitted and = True then a message() will be issued along with the debugoutstriung</param>
!!!<param name='argForceDebug'>if not omitted and = True then the debugout will be issued regardless of debug setting.</param>
!!! <returns>orig ErrorCode()</returns>      
CheckError                  PROCEDURE(STRING argHeader,BYTE argShowMessage=FALSE,BYTE argForceDebug=FALSE),LONG,PROC


Set_dumpQue_DefaultFileName PROCEDURE(STRING  argNewFname) !added Jan/12/05 MG
Set_dumpQue_AsciiByDefault  PROCEDURE(BYTE    argNewValue),BYTE,PROC!return previous value !enumerated: DEBUGER::ASCII_*

AddUserEvent                PROCEDURE(STRING argEventName,Long argEventEquate)
GetUserEvent                PROCEDURE(LONG argEventEquate),STRING

AddAssertMsg                PROCEDURE(Long     argPriority, STRING   argMessage, Long argAction, LONG argMatchMode=1)        !added Aug,7 2005!  !1 = MATCH:WILD in equates.clw
DelAssertMsg                PROCEDURE(STRING   argMessage)
FreeAssertMsg               PROCEDURE()
MatchAssertMsg              PROCEDURE(UNSIGNED LineNumber, STRING filename, STRING argMSG),LONG,VIRTUAL              !added Aug,7 2005!
AssertHookAction            PROCEDURE(LONG Action, UNSIGNED LineNumber, STRING filename, STRING argMSG),LONG,VIRTUAL !added Aug,8 2005!
DescribeAction              PROCEDURE(LONG Action),STRING

DumpControls                PROCEDURE(<*WINDOW xWin> , SIGNED xLowFEQ=-MAX:FEQ, SIGNED xHiFEQ=MAX:FEQ) !for dumping control info - calls ShowControl for each
ShowControl                 PROCEDURE( *WINDOW xWin  , SIGNED xFEQ            ,<STRING xDescr>       ) !Dumps details about one control
DescribeType                PROCEDURE(LONG xType),STRING  !<-- assumes a CREATE:*

GetVarType                  PROCEDURE(*? xaVar),LONG   !--- see softvelocity.public.clarion6 "Variable Data Type" Sept,12,2006 (code posted by dedpahom) -----!
DescribeDataType            PROCEDURE(LONG xType),STRING  !<-- assumes a DATATYPE:*  (see equates.clw)

GroupDiff                   PROCEDURE(*GROUP xaA,  *GROUP xaB, STRING xDescr, STRING xDescrA, STRING xDescrB) !created Oct/4/06 by MG
DiffBytes                   PROCEDURE(*GROUP  xA , *GROUP xB , STRING xDescr, STRING xDescrA, STRING xDescrB) !created Aug/13/07 by MG
DumpGroup                   PROCEDURE(*GROUP xaGroup, STRING xLinePrefix)                                           !created Oct/4/06 by MG
ODS                         PROCEDURE(STRING xMessage) !+Mar/13/08
StringAsVal                 PROCEDURE(STRING xString, LONG EndAt=-1)
!HexOut                     PROCEDURE(STRING,<STRING argHeader>)  ! Output Hexidecimal representation of the string
!EndRegion Methods
                          END !Class Debuger
!==========================================================================================================
DebugerAutoInit           CLASS(Debuger),TYPE,MODULE('Debuger.CLW') !,DLL(_DebugerDllMode_)
CONSTRUCT                   PROCEDURE !self.MG_Init('AutoInit')
                          END
!==========================================================================================================

 !end-OMIT('*** ifdef Debuger ***',ifDef_Debuger)




!  omit('***** documentation *****')
!  @===============================================================================
!  Credit: This class was origionally written by Skip Williams
!          see http://www.clarionmag.com/cmag/v5/v5n01debuger.html
!
!  You`ll want DebugView to download it visit (URL thanks to Russ @ ETC IV)
!     www.sysinternals.com/ntw2k/freeware/debugview.shtml
!
!  Modifications made by Mark Goldberg (clarion@monolithcc.com) are identified below as MG
!
!  Modifications:
!  MG Feb/4/03        a fair bit of coding sytle reformatting
!                     added ifdef, MAXMSGLEN
!                     added default values to prototyes, which allow OMITTED() logic to be removed
!                     added Self.BeepOnMessage
!                     in DebugOut, changed policy to only track LastMsg & NumberSame when Duplicates <> 0
!                     created ShowMessageWindow from a routine in Debugout
!                     added destruct (it just calls .KILL)
!
!  MG Feb/21/03       added PrintEvent, GetFEQDesc, GetEventDescr
!
!==============================================================================================
!  TODO:
!==============================================================================================
!    Add equates to clarify calls to DebugOut, ex:  Debug::ShowMessage (change to prompt user)
!    Derive a class from this, with helper functions,
!       Cla$FIELDNAME, Cla$NAMEMESSAGE
!       PrintDebugEvent
!       Track library state
!    Support duplicates based on HEADER, not just HEADER+BODY
!    Add running totals to HEADER, and to BODY -- Grand Total, and Contiguous Total
!       consider a queue of ASTRING
!    Create a PROFILE.CLW that uses this class
!    Evaluate the use of ThisMSG and LastMSG
!       use &cstring, and allocate space as needed
!    Property to show Thread() in message
!    Add support for lazy-write
!       basically same as printdebug, without a <13,10> at the endl.
!       so the message is cached until a non-lazy call is made to debugout
!    make the call OPTIONAL to self.Set_AssertHook2() in .INIT
!==============================================================================================
!
!  MG May/22/03        Added Set_AssertHook2, Module_Debuger work
!                        This allows an Assert(0,eqDBG & 'my real message') to generate a call
!                        to .DebugOut with the FileName/LineNumber of the Assert
!
!
!  MG Dec/23/04        Refined DumpQue work
!  MG Jan/06/05        Added self.EventOffset as I learned that it moved in C6 1400h to A000h
!                      Added Date/Time stamps to .INIT/.KILL in the output
!
!  MG Integrated        and enhanced some of Skip Williams work:
!                      added Skip's.ClearLog
!                      added 4th arg to .INIT to auto call .ClearLog (added equates too: DEBUGER::CLEAR_CMDLINE ,DEBUGER::CLEAR_ALWAYS, DEBUGER::CLEAR_NEVER
!                            _CMNDLINE =>  /CLEAR will to auto clear
!                      added module static to supress multiple cmdline clears when multiple instances of the class are used
!                      added Equates for .INIT 2nd argument,  DEBUGER::DISABLED,::ENABLED,::ENABLE_CMNDLINE
!
!  MG Jan/12/05        Added .dumpQue_DefaultFileName and .dumpQue_AsciiByDefault
!                      Added equates: DEBUGER::DUMPQUE_NOFILE and DEBUGER::DUMPQUE_PROMPT
!                      .INIT takes another argument to set .dumpQue_AsciiByDefault -- default is TRUE
!                      Added conditional link/dll attributes to both classes (and magic ABCIncludeFile comment to match)
!!!!            -----------------------------------------------------------------------------
!!!!
!!!!               A file will be written if:
!!!!               a) a filename is passed in (unless special values)
!!!!                   special value DEBUGER::DUMPQUE_NOFILE   -- no file use at all
!!!!                   special value DEBUGER::DUMPQUE_PROMPT -- will always prompt user for file
!!!!
!!!!               b) no filename passed in, and self.dumpQue_AsciiByDefault is <> DEBUGER::ASCII_NEVER
!!!!                    then use self.dumpQue_DefaultFileName
!!!!                    self.dumpQue_DefaultFileName will correctly handle DEBUGER::DUMPQUE_NOFILE (rare case)
!!!!                    if dumpQue_AsciiByDefault = DEBUGER::ASCII_PROMPT
!!!!                       then the default filename will be used in the FileDialog
!!!!                       else the default filename will be used automatically (no prompting)
!!!!
!!!!            -----------------------------------------------------------------------------


!  MG Jan/14/05        added UserEvent Naming, based on Dave Harm's Profiler
!                            UserEventNameQ         &qtUserEventName
!                            AddUserEvent           PROCEDURE(String argEventName,Long argEventEquate)
!                            GetUserEvent           PROCEDURE(Long argEventEquate),string
!                      added GetEventDescr_WM
!
! MG early   05        support EVENT:APP in addition to Event:User
! MG May     05        refined .DumpQue work
! MG June 16/05        added the _VER_ logic for linking in the Ascii Driver
! MG July    05        added HOWMANY() and WHAT(Stru, N, DIM) support when compiled with C6+
! MG July    05        Refined automatic messages when .init/kill this class
!                          show program name, use @D18 to localized date formmatting,
!                          show URL for this class updates, added some white space
! MG Aug 08/05         Added AssertMsg work (to help avoid an FM3 annoyance)
!                      Changed module_debug to a queue
!                      Added .construct / destruct
!
!
! MG Aug 10/05         .CheckError:   added ,PROC
!                              prefix message with 'ERROR:'
!                              pay attention to forceDebug argument
! MG        Aug 17/05  Fixed spelling AssetMsgAction:: => AssertMsgAction::
! MG        Jan 10/06  Added ODS(String msg) as an internal helper function
! MG & Russ Aug/16/06  Added .DumpControls and ShowControl=
! MG        Aug/22/06  Reformatted the .INC to CAPITALIZE keywords
!                      Altered .Dumpcontrols, ShowControls
!                      Added .Message which calls .DebugOut, done for Compatibility to Russ's Version of the Debuger
!                      Added .DescribeType
! MG        Aug/31/06  Fixed a leak in .DumpQue when DumpQue::ProcedureReturn is called early
!
! MG        Mar/09/07  Henceforth permission to use this source is contigent on the following:
!                      All source modifications must be made in such a way such that
!                      support for a version of Clarion (eg C55)
!                      or an environment (eg 32 bit, if 64 bit is ever supported)
!                      once supported in a released version can NOT be removed.
!
! MG       Sept/21/09  Fixed a bug in .DumpControls, which could GPF
!  @=======================================================================================
!  omit('***** documentation *****')

! vim: set filetype=clarion:
