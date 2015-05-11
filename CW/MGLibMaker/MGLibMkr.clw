     PROGRAM

!Updated 2014-Jan-14 By Mark Goldberg 
!  - Added Ability to Generate Classes (no parameters )
!  - Refactored some of the code.
!  - A Lot of code is still badly written
!                                  

!Region Ancient Documentation
! =========================================================================================
! Original Source code copied from shipping example of Libmaker that came with C5EEB3
! Updated: MG == by Mark Goldberg of Monolith Custom Computing, Inc.
!
! NOTE: MGLibMkr was built using C5EE, as a result the file drivers will not be recognized
!         by other versions of CW (ex: C4),
!         Work around:
!           a) Delete the 2 lines in Library, object and resource files that have the % symbols showing
!           b) Under Database driver libraries, add the ASCII and DOS drivers
!
!
! Updates:
!   MG 10/6/98
!       Changed user interface:
!          replaced buttons with toolbar
!          using MGResizeClass to resize the listbox
!          using 32-bit filedialogs
!          added EXE to file formats on the AddFile FileDialog
!          Fixed a minor bug in EnableControls
!          added VCR controls to listbox
!          Made the 1st column resizable (so can see lengthy prototypes)
!          Save & Restore 1st column width
!          Save & Restore Window Position & Size
!       Added:
!          Make CW Map feature
!          add command line support for file name to load
!          if do not give a file at the command line, then will auto-accept the fileAdd
!
!       Doesn`t Work:
!          Added DropID('~FILE') but it doesn`t seem to accept dragging from Explorer or WinFile !!
!            (failed under C5EEB3 & C4a)
!
!   MG 10/18/98 (1:20 - 4:15 )
!        Moved DropID('~FILE') from List control to Window, now it works!
!        Added several features of Arnor Baldvinssons, see lines marked with !AB
!        Changed Search system of Arnor`s to use the ExportQ and Colorize and set Icon to indicate Found Lines
!          wrote:
!           SetThisColor     (Long argNFG, Long argNBG,*FormatColorSet argFCS)
!           SetGloColors
!        Added Myself to Arnor`s help about window
!           (see Help about window documentation for additional changes)
!        Moved much of the "technical" file definitions into FileDefs.INC
!        Added::  ?List1{PROP:Format} = ?List1{PROP:Format} !<-- Added to force re-draw of Q (needed in C5EEB4 and likely elsewhere)
!        Changed the Search Window dramatically, renamed the search names too
!
!
!   MG 10/20/98 (7:15 - 10:00 )
!        Change PropList:Width,1 when Window{prop:Width} changes, so the function column resizes vs. the ordinal column
!          Removed save/restore column width
!        Added an icon for search (copied from a sample program posted by Andy Stapleton) -- is this OK?
!        Refined setting Icon for TreeLevel = +/- 1
!        Refined Contains search method, so that a single letter search will hit when found in the first character of the symbol
!        Changed Sort Buttons to a Radio Option, using a blank icon to accomplish a latched look
!        Added hot-letters to several buttons in the toolbar (see their Tip Help for indication)
!        Added support to launch a notepad with the just created CW MAP
!
!        Doesn`t Work:
!           Locator button on VCR control
!           CW MAP: lcl.symbol = lcl:symbol[ 1 : lcl.instringLoc - 1 ]  !BUG: for some DLL`s, I found one compiled with CBuilder 1.0, that this would be bad for
!
!   MG 11/16/98
!   MG 01/19/99 Added ",DLL(1),RAW" to map export
!   MG 12/07/01 Recompiled using C55eeF
!
!   MG  1/22/03 Quick and dirty addtion of the Ron Schofields .API file format --- UNFINISHED
!
! Future Features:
!   b) Keep a list of recent .DLL`s that have been opened
!   c) Add an option to auto-create a CW map with the same base name as the .LIB being saved
!   d) When delete a function, then mark it with colorization and an icon
!       i) add support for show/hide deleted functions
!      ii) pay attention to deleted functions when writing a .LIB
!     iii) add support for undeleting functions
!      iv) consider allowing deletion based on the results of the current search
!   e) Support for multiple files in a drag & drop situation
!   f) Support for multiple files in a AddFile
!   g) Enhance command line features
!   h) Support Recent file lists
!   i) have reasonable defaults for filenames when GenerateMap & WriteLib

! TODO (notes: June 20 2005)
!   replace color work with STYLES
!   remove lcl/rou GROUPS, use LCL: and ROU:
! =========================================================================================
!EndRegion Ancient Documentation

     MAP
        ReadExecutable
        DumpPEExportTable  (ULONG RawAddr, ULONG VirtAddr)
        DumpNEExports
        WriteLib
        ReadLib
        InfoWindow
        GenerateMap         (byte argRonsFormat)
        SelectQBE  
        SetThisColor       (Long argNFG, Long argNBG,*FormatColorSet argFCS)
        SetGloColors
		  ExportQ_to_FilterQ
		  GenerateClasses
        LongestSymbol      (),LONG   !in ExportQ Level 2
        CleanSymbol        (STRING xSymbol),STRING
        AppendAscii        (STRING xLine)
     END

qINI_File      Equate('MGLibMkr.ini')
qOrdColWidth   Equate(41) 

     include(   'KeyCodes.clw'),ONCE
     include(    'Equates.clw'),ONCE
     include(   'FileDefs.inc'),ONCE
     include('ResizeClass.inc'),ONCE
MGResizeClass ResizeClassType


FormatColorSet    Group,Type
NFG                 Long
NBG                 Long
SFG                 Long
SBG                 Long
                  end
Glo     Group
Colors      Group
Defaults      Like(FormatColorSet),dim(2)
Found         Like(FormatColorSet),dim(2)
            end
SortOrder   String('Original')
DisplayCount uLong
FoundCount   uLong !MG Added 12/7/01
RonsFormat   byte
        END !glo





ExportQ   QUEUE,PRE(EXQ)
symbol      STRING(128)
ColorNFG    LONG    !Normal Foreground color for FName
ColorNBG    LONG    !Normal Background color for FName
ColorSFG    LONG    !Selected Foreground color for FName
ColorSBG    LONG    !Selected Background color for FName
icon        LONG          !AB (was short)
treelevel   SHORT
ordinal         USHORT
!Modified by MJS to allow for longer file names
module      STRING(FILE:MaxFilePath)
orgorder    LONG          !AB
SearchFlag  Byte          !MG (enum field, see SearchFlag::* below)
          END

ASCIIfile  FILE,DRIVER('ASCII'),PRE(ASCII),CREATE,NAME(FileName)
            Record
Line          String(2 * Size(ExportQ.symbol) + 30)
            END
          End
          
!Region - Tmp Region          
FilteredQ QUEUE(ExportQ),PRE(FLTQ)
          END


SearchFlag::Default       Equate(0)
SearchFlag::Found         Equate(1)

LOC:FoundRec  LONG        !AB   (mg, shows found count on the window -- todo)


window WINDOW('MGLibMkr'),AT(,,288,207),CENTER,GRAY,IMM,SYSTEM,ICON('LIBRARY.ICO'), |
         FONT('Segoe UI'),ALRT(DeleteKey),DROPID('~FILE'),RESIZE
      TOOLBAR,AT(0,0,288,41),USE(?TOOLBAR1)
         BUTTON('&O'),AT(2,2,14,14),USE(?AddFile),ICON(ICON:Open),TIP('Open / Ad' & |
               'd File... (ALT+0)'),FLAT,LEFT
         BUTTON('&S'),AT(19,2,14,14),USE(?SaveAs),DISABLE,ICON(ICON:Save), |
               TIP('Save .LIB As... (ALT+S)'),FLAT,LEFT
         BUTTON,AT(36,2,14,14),USE(?Clear),DISABLE,ICON('clear.ico'),TIP('Empty ' & |
               'the listbox'),FLAT,LEFT
         BUTTON,AT(53,2,14,14),USE(?Exit),STD(STD:Close),ICON('Exit.ico'), |
               TIP('Exit the program'),FLAT,LEFT
         BUTTON,AT(87,2,14,14),USE(?Info),ICON(ICON:Help),TIP('Info'),FLAT,LEFT
         BUTTON('Generate CW &Map'),AT(135,4,81,10),USE(?MakeMap),DISABLE, |
               TIP('A head start on making a CW Map ...'),FLAT,LEFT
         CHECK('Rons Format'),AT(222,5),USE(glo.RonsFormat)
         BUTTON('Generate Classes'),AT(135,16,81,10),USE(?GenerateClasses),DISABLE, |
               TIP('A head start on making a CW Classes...'),FLAT,LEFT
         OPTION('Sort Order'),AT(4,16,123,23),USE(glo.SortOrder),BOXED
            RADIO('Original'),AT(8,24,37,13),USE(?glo:SortOrder:Radio1), |
                  ICON('Blank.ico')
            RADIO('Function'),AT(47,24,37,13),USE(?glo:SortOrder:Radio2), |
                  ICON('Blank.ico')
            RADIO('Ordinal'),AT(86,24,37,13),USE(?glo:SortOrder:Radio3), |
                  ICON('Blank.ico')
         END
         PROMPT('Found Count'),AT(127,29,48,8),USE(?GLO:FoundCount:Prompt),TRN, |
               FONT('Arial',8,COLOR:Blue,FONT:regular,CHARSET:ANSI),RIGHT
         STRING(@n6),AT(173,29,,8),USE(glo.FoundCount),TRN,RIGHT(1),FONT(,, |
               COLOR:Blue,,CHARSET:ANSI)
         STRING('Total Count'),AT(210,29,38,8),USE(?GLO:DisplayCount:Prompt),TRN,RIGHT, |
               FONT('Arial',8,,FONT:regular,CHARSET:ANSI)
         STRING(@n6),AT(246,29,,8),USE(glo.DisplayCount),TRN,RIGHT(1)
         BUTTON('&F'),AT(70,4,14,12),USE(?FindButton),DISABLE,FONT('MS Sans Seri' & |
               'f',8,,FONT:regular),ICON('Cs_srch.ico'),TIP('Search for string (' & |
               'ALT+F)'),FLAT,LEFT
      END
      SHEET,AT(4,4,283,160),USE(?SHEET)
         TAB('All'),USE(?TAB:All)
            LIST,AT(8,20,272,138),USE(?List1),DISABLE,VSCROLL,FONT('Arial',8,, |
                  FONT:regular),COLUMN,VCR,FROM(ExportQ),FORMAT('143L(2)|M*IT~Mo' & |
                  'dule and function~30R(3)~Ordinal~@N_5B@')
         END
         TAB('Filtered Only'),USE(?TAB:Filtered)
            LIST,AT(8,24,272,138),USE(?Filtered),DISABLE,VSCROLL,FONT('Arial',8, |
                  ,FONT:regular),COLUMN,VCR,FROM(FilteredQ),FORMAT('143L(2)|M*IT' & |
                  '~Module and function~30R(3)~Ordinal~@N_5B@')
         END
      END
   END

! LIST,AT(172,23,164,151),USE(?SearchList),HIDE,VSCROLL,FONT('MS Sans Serif',8,,FONT:regular),FORMAT('45L(1)|M~Module~@S20@87L(1)|M~Symbol~@S40@20R(1)|~Ordinal~L@N_5@'),FROM(FQ)

   CODE
   DO PreAccept
   DO AcceptLoop
   DO PostAccept

!------------------------------------------------------
PostAccept           ROUTINE
   MGResizeClass.Close_Class
   if ~0{prop:Iconize}
      PutIni('Position','X',0{prop:XPOS } ,qINI_FILE)
      PutIni('Position','Y',0{prop:YPOS } ,qINI_FILE)
      PutIni('Position','W',0{prop:Width} ,qINI_FILE)
      PutIni('Position','H',0{prop:Height},qINI_FILE)
   !  PutIni('Position','Col1W',?List1{proplist:Width,1},qINI_FILE)
   end
   PutIni('Settings','RonsFormat',glo.RonsFormat,qINI_FILE)
!------------------------------------------------------
PreAccept            ROUTINE
   SetGloColors()
   OPEN(window)
   System{prop:Icon} = window{prop:icon} !Have any non-minimize-able additional windows share the same Icon
   ExportQ.orgorder = 0       !AB

   ! ?list1{prop:vcr}=TRUE   !Suppress the ? button in the vcr
   window {PROP:minheight}  = 76
   window {PROP:minwidth }  = window{PROP:width}
   MGResizeClass.Init_Class(window)
   MGResizeClass.Add_ResizeQ(?List1          ,'L/T R/B')
   MGResizeClass.Add_ResizeQ(?Filtered       ,'L/T R/B')
   MGResizeClass.Add_ResizeQ(?SHEET          ,'L/T R/B')
   MGResizeClass.Add_ResizeQ(?TAB:All        ,'L/T R/B')
   MGResizeClass.Add_ResizeQ(?TAB:Filtered   ,'L/T R/B')

	
   !---- apparently there is a problem with my resize, when I pass an FEQ from the toolbar
   !---- this has the effect of breaking the resize, although the INITIAL resize works, which leads me to believe that
   !---- the problem is related to my use of 0{prop:  } code
!      MGResizeClass.Add_ResizeQ(?GLO:DisplayCount:Prompt,'R/T R/T')
!      MGResizeClass.Add_ResizeQ(?GLO:DisplayCount       ,'R/T R/T')
   !---- apparently there is a problem with my resize, when I pass an FEQ from the toolbar -- end

  !MGResizeClass.Add_ResizeQ(?SearchList,'L/T R/B') !Problem!

   0{prop:XPOS}   = GetIni('Position','X',0{prop:XPOS } ,qINI_FILE)
   0{prop:YPOS}   = GetIni('Position','Y',0{prop:YPOS } ,qINI_FILE)
   0{prop:Width}  = GetIni('Position','W',0{prop:Width} ,qINI_FILE)
   0{prop:Height} = GetIni('Position','H',0{prop:Height},qINI_FILE)
   glo.RonsFormat = GetIni('Settings','RonsFormat',FALSE,qINI_FILE)

   ?List1{PROP:LineHeight}  = 8               !AB

   !?List1{proplist:Width,1} = GetIni('Position','Col1W',?List1{proplist:Width,1},qINI_FILE)
   MGResizeClass.Perform_Resize()
   ?List1{proplist:Width,1} = ?List1{prop:Width} - qOrdColWidth

   ?List1{PROP:iconlist, 1} = '~Opened.ico'
   ?List1{PROP:iconlist, 2} = '~Closed.ico'
   ?List1{PROP:iconlist, 3} = '~Found.ico'

   ?Filtered{proplist:Width,1} = ?Filtered{prop:Width} - qOrdColWidth

   ?Filtered{PROP:iconlist, 1} = '~Opened.ico'
   ?Filtered{PROP:iconlist, 2} = '~Closed.ico'
   ?Filtered{PROP:iconlist, 3} = '~Found.ico'

!   ReadFileName  = GetIni('RecentFiles','Read' ,ReadFileName ,qINI_File)
!   WriteFileName = GetIni('RecentFiles','Write',WriteFileName,qINI_File)
!   AsciiFileName = GetIni('RecentFiles','Ascii',AsciiFileName,qINI_File)

   FileName = Command()
   if len(clip(FileName))
        DO FileAdded        
   else Post(Event:Accepted,?AddFile)
   end
   DISPLAY()
!------------------------------------------------------
!Region Accept Loop
AcceptLoop           ROUTINE   
   ACCEPT	 
     CASE ACCEPTED()
       OF ?glo:SortOrder   ; DO Accepted:SortOrder
       OF ?FindButton      ; DO Accepted:FindButton
       OF ?MakeMap         ; DO Accepted:MakeMap
       OF ?GenerateClasses ; IF RECORDS(ExportQ)>0 THEN GenerateClasses() END                 
       OF ?Info            ; InfoWindow()
       OF ?Clear           ; DO Accepted:Clear
       OF ?AddFile         ; DO Accepted:AddFile
       OF ?SaveAs          ; DO Accepted:SaveAs
     END 

     CASE EVENT()
       OF EVENT:expanded
     OROF EVENT:contracted ; DO OnExpandContract
       OF EVENT:AlertKey   ; DO OnAlertKey
       OF Event:Drop       ; DO OnDrop
       OF EVENT:Locate     ; DO OnLocate				
       OF EVENT:TabChanging; ExportQ_to_FilterQ() !could be a bit more subtle, to only catch when switching TO the FilterQ			
     END      
     
     IF MGResizeClass.Perform_Resize()
        ?List1   {proplist:Width,1} = ?List1   {prop:Width} - qOrdColWidth
		  ?Filtered{proplist:Width,1} = ?Filtered{prop:Width} - qOrdColWidth
     END
     
     DO EnableDisable
     DO Set_DisplayCount
   END 

!------------------------------------------------------
Set_DisplayCount     ROUTINE
  glo.DisplayCount = Records(ExportQ)
  !todo: refine this value as desired,
  !  show a count of found records when a search is in effect
  !  show only level 2 records
!------------------------------------------------------
EnableDisable       ROUTINE
  ?List1             {PROP:Disable} = CHOOSE( RECORDS(ExportQ) = 0 )
  ?Filtered          {PROP:Disable} = ?List1{PROP:Disable}
  ?SaveAs            {PROP:Disable} = ?List1{PROP:Disable}
  ?Clear             {PROP:Disable} = ?List1{PROP:Disable}
  ?MakeMap           {PROP:Disable} = ?List1{PROP:Disable}
  ?GenerateClasses   {PROP:Disable} = ?List1{PROP:Disable}
  ?glo:SortOrder     {PROP:Disable} = ?List1{PROP:Disable}
  ?FindButton        {PROP:Disable} = ?List1{PROP:Disable}
  
  

!Region Accepted ROUTINEs
Accepted:SortOrder ROUTINE       
   SetCursor(CURSOR:Wait)
   Execute Choice(?glo:SortOrder)
      BEGIN; SORT(ExportQ,ExportQ.orgorder)                                 ; SORT(FilteredQ,FLTQ:orgorder)                           ; END
      BEGIN; SORT(ExportQ,ExportQ.Module,ExportQ.TreeLevel,ExportQ.symbol)  ; SORT(FilteredQ,FLTQ:Module,FLTQ:TreeLevel,FLTQ:symbol)  ; END
      BEGIN; SORT(ExportQ,ExportQ.Module,ExportQ.TreeLevel,ExportQ.Ordinal) ; SORT(FilteredQ,FLTQ:Module,FLTQ:TreeLevel,FLTQ:Ordinal) ; END
   END
   ?List1   {PROP:Format} = ?List1   {PROP:Format} !<-- Added to force re-draw of Q (needed in C5EEB4 and likely elsewhere)
   ?Filtered{PROP:Format} = ?Filtered{PROP:Format} !<-- Added to force re-draw of Q (needed in C5EEB4 and likely elsewhere)
   SetCursor()
   
Accepted:FindButton ROUTINE
   SelectQBE()
   ?List1   {PROP:Format} = ?List1   {PROP:Format} !<-- Added to force re-draw of Q (needed in C5EEB4 and likely elsewhere)
   ?Filtered{PROP:Format} = ?Filtered{PROP:Format} !<-- Added to force re-draw of Q (needed in C5EEB4 and likely elsewhere)

Accepted:MakeMap  ROUTINE
    IF RECORDS(ExportQ)>0
     Clear(FileName) !FileName = AsciiFileName
     IF FileDialog('Save Clarion Map definition as ...', FileName, |
        'CW Source and Includes (*.clw,*.inc)|*.clw;*.inc|Source Only|*.clw|Includes Only|*.inc|All|*.*',FILE:Save+FILE:LongName)
        GenerateMap(glo.RonsFormat)
        !AsciiFileName = FileName
        if Message('Would you like to view the map with Notepad?','Note',ICON:Question,Button:Yes+Button:No,Button:Yes)=Button:Yes
           Run('notepad ' & FileName)
        END
     END
   END
Accepted:Clear ROUTINE       
   FREE(ExportQ); FREE(FilteredQ)   
   Window{PROP:Text} = 'LibMaker'
   DISPLAY

Accepted:AddFile  ROUTINE
   IF FileDialog('Import symbols from file ...', FileName, |
                 'DLLs and LIBs|*.dll;*.lib|DLL files (*.dll)|*.dll|LIB files (*.lib)|*.lib|Executables (*.exe)|All files (*.*)|*.*', |
                 FILE:LongName)
     DISPLAY
     do FileAdded
   END   

Accepted:SaveAs   ROUTINE       
   IF RECORDS(ExportQ)>0
     Clear(FileName) !FileName = WriteFileName
     IF FileDialog('Save OMF library definition as ...', FileName, 'Library files (*.lib)|*.lib',FILE:Save+FILE:LongName)
        Window{PROP:Text} = 'LibMaker - ' & CLIP(FileName) !AB
        SetCursor(CURSOR:Wait)                             !AB
        SORT(ExportQ  ,ExportQ.orgorder)                         !AB
        SORT(FilteredQ,FLTQ:orgorder)                         
        WriteLib
        SetCursor()                                        !AB
        !WriteFileName = FileName
     END
   END
!EndRegion
!Region OnEvent ROUTINEs

OnExpandContract     ROUTINE     
    !TODO: Bug FilterdQ
    i# = ?List1{PROPLIST:MouseDownRow}
    GET(ExportQ, i#)
    ExportQ.treelevel = -ExportQ.treelevel
    IF ExportQ.icon = 1
       ExportQ.icon = 2
    ELSE
       ExportQ.icon = 1
    END
    PUT(ExportQ)
    DISPLAY(?list1)

OnAlertKey           ROUTINE       
   !TODO: Bug FilterdQ
    IF KeyCode()=DeleteKey
       GET(ExportQ, CHOICE(?List1))
       DELETE(ExportQ)
       IF (ExportQ.treelevel<2)
         GET(ExportQ, CHOICE(?List1))
         LOOP WHILE (ExportQ.treelevel=2)
           DELETE(ExportQ)
           GET(ExportQ, CHOICE(?List1))
           IF (ERRORCODE())
             BREAK
           END
         END
       END
       
       DISPLAY(?list1)
    END !IF KeyCode()=DeleteKey

OnDrop               ROUTINE       
  !Message('We got a drop event!|Drop() [' & DropID() & ']','debug')
  !TODO: Support for MULTIPLE Files at once
  !TODO: Set the TITLE as is done with FileAdd
  FileName = DropID()
  do FileAdded
  

OnLocate             ROUTINE       
  BEEP()  !debugging beep (never happens--bug!)
  !note, ,VCR(?FindButton) doesn't work for me either
  Post(Event:Accepted,?FindButton)
!EndRegion OnEvent ROUTINEs
!EndRegion Accept Loop

!------------------------------------------------------
FileAdded            ROUTINE
  SetCursor(CURSOR:Wait)
  IF INSTRING('.LIB', UPPER(FileName), 1, 1)
    ReadLib()
  ELSE
    ReadExecutable()
  END
  Window{PROP:Text} = 'LibMaker - ' & CLIP(FileName)  !AB  !MG Note: This will only show that LAST file added, when multiple are added
  !PutIni('RecentFiles','Read',FileName,qINI_File)
  SetCursor()
!========================================================================================
!========================================================================================
!========================================================================================
!========================================================================================
!========================================================================================
ReadExecutable       PROCEDURE !gets export table from 16 or 32-bit file or LIB file
sectheaders ULONG   ! File offset to section headers
sections    USHORT  ! File offset to section headers
VAexport    ULONG   ! Virtual address of export table, according to data directory
                    ! This is used as an alternative way to find table if .edata not found
   CODE
   OPEN(EXEfile, 0)
   GET(EXEfile, 1, SIZE(EXE:DOSheader))
   IF EXE:dos_magic = 'MZ' THEN
     newoffset = EXE:dos_lfanew
     GET(EXEfile, newoffset+1, SIZE(EXE:PEheader))
     IF EXE:pe_signature = 04550H THEN
       sectheaders = EXE:pe_optsize+newoffset+SIZE(EXE:PEheader)
       sections = EXE:pe_nsect
       ! Read the "Optional header"
       GET(EXEfile, newoffset+SIZE(EXE:PEheader)+1, SIZE(EXE:Optheader))
       IF EXE:opt_DataDirNum THEN
          ! First data directory describes where to find export table
          GET(EXEfile, newoffset+SIZE(EXE:PEheader)+SIZE(EXE:OptHeader)+1,SIZE(EXE:DataDir))
          VAexport = EXE:data_VirtualAddr
       END

       LOOP i# = 1 TO sections
         GET(EXEfile,sectheaders+1,SIZE(EXE:sectheader))
         sectheaders += SIZE(EXE:sectheader)
         IF EXE:sh_SectName = '.edata' THEN
            DumpPEExportTable(EXE:sh_VirtAddr, EXE:sh_VirtAddr - EXE:sh_rawptr)
         ELSIF EXE:sh_VirtAddr <= VAexport AND |
               EXE:sh_VirtAddr+EXE:sh_RawSize > VAexport
            DumpPEExportTable(VAexport, EXE:sh_VirtAddr - EXE:sh_rawptr)
         END
       END
     ELSE
       GET(EXEfile, newoffset+1, SIZE(EXE:NEheader))
       DumpNEExports
     END
   END
   CLOSE(EXEfile)

!========================================================================================
DumpPEExportTable    PROCEDURE(VirtualAddress, ImageBase) !gets export table from a PE format file (32-bit)

NumNames  ULONG
Names     ULONG
Ordinals  ULONG
Base      ULONG

j         USHORT



   CODE
   GET(EXEfile, VirtualAddress-ImageBase+1, SIZE(EXE:ExpDirectory))
   NumNames      = EXE:exp_NumNames
   Names         = EXE:exp_AddrNames
   Ordinals      = EXE:exp_AddrOrds
   Base          = EXE:exp_Base
   GET(EXEfile, EXE:exp_Name-ImageBase+1, SIZE(EXE:cstringval))
   !Added code to pares the first character of the module name as when a .Net unmanaged dll
   !A \ seems to be generated as first character
   if EXE:cstringval[1] = '\' THEN
	 EXE:cstringval = EXE:cstringval[2:SIZE(EXE:cstringval)]
   END
   ExportQ.Module    = EXE:cstringval
   ExportQ.Symbol    = EXE:cstringval
   ExportQ.treelevel = 1
   ExportQ.icon      = 1
   ExportQ.ordinal   = 0
   ExportQ.orgorder += 1  !AB
   !glo.Colors.Defaults[1]

   ExportQ.SearchFlag= SearchFlag::Default
   ExportQ.ColorNFG  = glo.Colors.Defaults[1].NFG  !LONG    !Normal   Foreground color
   ExportQ.ColorNBG  = glo.Colors.Defaults[1].NBG  !LONG    !Normal   Background color
   ExportQ.ColorSFG  = glo.Colors.Defaults[1].SFG  !LONG    !Selected Foreground color
   ExportQ.ColorSBG  = glo.Colors.Defaults[1].SBG  !LONG    !Selected Background color
   ADD(ExportQ)

   ExportQ.treelevel = 2
   ExportQ.icon      = 0
   ExportQ.ColorNFG  = glo.Colors.Defaults[2].NFG  !LONG    !Normal   Foreground color
   ExportQ.ColorNBG  = glo.Colors.Defaults[2].NBG  !LONG    !Normal   Background color
   ExportQ.ColorSFG  = glo.Colors.Defaults[2].SFG  !LONG    !Selected Foreground color
   ExportQ.ColorSBG  = glo.Colors.Defaults[2].SBG  !LONG    !Selected Background color
   LOOP j = 0 TO NumNames-1
      GET(EXEfile, Names    + j*4 - ImageBase+1, SIZE(EXE:ulongval))
      GET(EXEfile, EXE:ulongval   - ImageBase+1, SIZE(EXE:cstringval))
      ExportQ.symbol = EXE:cstringval
      GET(EXEfile, Ordinals + j*2 - ImageBase+1, SIZE(EXE:ushortval))
      ExportQ.ordinal = EXE:ushortval+Base
      ExportQ.orgorder +=1   !AB
      ADD(ExportQ)
   END



!========================================================================================
SetGloColors         PROCEDURE  
!  -----------------------------------------------
!  Create : 10/18/98 by Monolith Custom Computing, Inc
!  Purpose: To set the color schemes for conditional coloring
!
!  Note: N  -- Normal
!        S  -- Selected
!        FB -- ForeGround
!        BG -- BackGround
!  -----------------------------------------------
CurrentColor    Like(FormatColorSet)
  CODE
  SetThisColor(Color:None    ,Color:None    ,CurrentColor) ; glo.Colors.Defaults[1] = CurrentColor
  SetThisColor(Color:None    ,Color:None    ,CurrentColor) ; glo.Colors.Defaults[2] = CurrentColor
  SetThisColor(Color:Green   ,Color:None    ,CurrentColor) ; glo.Colors.Found[1]    = CurrentColor
  SetThisColor(Color:Blue    ,Color:None    ,CurrentColor) ; glo.Colors.Found[2]    = CurrentColor

  !SetThisColor(Color:Blue    ,Color:None    ,glo.Colors.Found[2])  !doesn't complie (no matching prototype)

!========================================================================================
SetThisColor         PROCEDURE(Long argNFG, Long argNBG, *FormatColorSet argFCS)
  code
  argFCS.NFG = argNFG
  argFCS.NBG = argNBG
  argFCS.SFG = argNBG !Reverse of Normal
  argFCS.SBG = argNFG !Reverse of Normal


!!========================================================================================
!AddExportQ    Procedure
!  -----------------------------------------------
!  Create : 10/18/98 by Monolith Custom Computing, Inc
!  Purpose: To add the current ExportQ Buffer to the ExportQ
!           While setting some Additional fields for 'behind the scenes' work
!  -----------------------------------------------
!  code
!  ExportQ.orgorder +=1   !AB
!  ADD(ExportQ)


!========================================================================================
DumpNEExports   PROCEDURE ! DumpNEexports gets export table from a NE format file (16-bit)
j  ULONG
r  ULONG
   CODE
! First get the module name - stored as first entry in resident name table
   j =             EXE:ne_nrestab+1
   r = newoffset + EXE:ne_restab+1
   GET(EXEfile, r, SIZE(EXE:pstringval))
   ExportQ.Module     = EXE:pstringval
   ExportQ.symbol     = EXE:pstringval
   ExportQ.ordinal    = 0
   ExportQ.treelevel  = 1
   ExportQ.icon       = 1
   ExportQ.orgorder  +=1   !AB
   ExportQ.SearchFlag = SearchFlag::Default
   ExportQ.ColorNFG   = glo.Colors.Defaults[1].NFG  !LONG    !Normal   Foreground color
   ExportQ.ColorNBG   = glo.Colors.Defaults[1].NBG  !LONG    !Normal   Background color
   ExportQ.ColorSFG   = glo.Colors.Defaults[1].SFG  !LONG    !Selected Foreground color
   ExportQ.ColorSBG   = glo.Colors.Defaults[1].SBG  !LONG    !Selected Background color

   ADD(ExportQ)
   r += LEN(EXE:pstringval)+1    !move past module name
   r += 2                        !move past ord#

! Now pull apart the resident name table. First entry is the module name, read above
   ExportQ.treelevel = 2
   ExportQ.icon      = 0
   ExportQ.ColorNFG  = glo.Colors.Defaults[2].NFG  !LONG    !Normal   Foreground color
   ExportQ.ColorNBG  = glo.Colors.Defaults[2].NBG  !LONG    !Normal   Background color
   ExportQ.ColorSFG  = glo.Colors.Defaults[2].SFG  !LONG    !Selected Foreground color
   ExportQ.ColorSBG  = glo.Colors.Defaults[2].SBG  !LONG    !Selected Background color
   LOOP
     GET(EXEfile, r, SIZE(EXE:pstringval))
     IF LEN(EXE:pstringval)=0 THEN
       BREAK
     END
     ExportQ.symbol = EXE:pstringval
     r += LEN(EXE:pstringval)+1
     GET(EXEfile, r, SIZE(EXE:ushortval))
     r += 2
     ExportQ.ordinal = EXE:ushortval
     ExportQ.orgorder +=1   !AB
     ADD(ExportQ)
   END

! Now pull apart the non-resident name table. First entry is the description, and is skipped
   GET(EXEfile, j, SIZE(EXE:pstringval)) ;    j += LEN(EXE:pstringval)+1
   GET(EXEfile, j, SIZE(EXE:ushortval))  ;    j += 2
   LOOP
     GET(EXEfile, j, SIZE(EXE:pstringval))
     IF LEN(EXE:pstringval)=0 THEN
       BREAK
     END
     ExportQ.symbol = EXE:pstringval
     j += LEN(EXE:pstringval)+1
     GET(EXEfile, j, SIZE(EXE:ushortval))
     j += 2
     ExportQ.ordinal = EXE:ushortval
     ExportQ.orgorder +=1   !AB
     ADD(ExportQ)
   END

!========================================================================================
WriteLib        PROCEDURE !writes out all info in the export Q to a LIB file
i  ULONG !USHORT !changed 7/24/02, based on note in comp.lang.clarion
   CODE
   CREATE(LIBfile)
   OPEN(LIBfile)
   LOOP i = 1 TO RECORDS(ExportQ)
      GET(ExportQ, i)
      IF ExportQ.treelevel=2 THEN
        ! Record size is length of the strings, plus two length bytes, a two byte
        ! ordinal, plus the header length (excluding the first three bytes)
        LIB:typ     = 88H
        LIB:kind    = 0A000H
        LIB:bla     = 1
        LIB:ordflag = 1
        LIB:len = LEN(CLIP(ExportQ.module))+LEN(CLIP(ExportQ.symbol))+2+2+SIZE(LIB:header)-3 
                                                  ADD(LIBfile, SIZE(LIB:header))
        LIB:pstringval = CLIP(ExportQ.symbol) ;   ADD(LIBfile, LEN(LIB:pstringval)+1)
        LIB:pstringval = CLIP(ExportQ.module) ;   ADD(LIBfile, LEN(LIB:pstringval)+1)
        LIB:ushortval  =      ExportQ.ordinal ;   ADD(LIBfile, SIZE(LIB:ushortval))
      END
   END
   CLOSE(LIBfile)

!========================================================================================
ReadLib              PROCEDURE !reads back in a LIB file output by WriteLib above or by IMPLIB etc

!Region  '****** softvelocity.products.c55ee.bugs posting ******')
!@   > I used LibMaker to list the contents of Win32.lib .  I know that the
!@   > Registry APIs are in the lib but LibMaker does not list them.  Is there
!@   > a bug in LibMaker?
!@
!@   Yes, there is a stupid bug: the ReadLib function uses variables of USHORT
!@   type as pointers in the LIB file. As result, files with size over 64KB
!@   can't be processed correctly. I think, LibMaker's sources are provided
!@   among examples. Therefore you can fix that bug yourself: just change types
!@   of ii and jj variables local for ReadLib from USHORT to LONG and remake
!@   the program.
!@
!@   Alexey Solovjev
!
!  MG Note: Need to review c55\examples\src\libmaker\libmaker.clw
!           pay attention to line just below "if modulename <> lastmodule"
!           Also notice the NEW ExportQ.Libno field
!EndRegion 

i          LONG   !changed 12/20/00 due to Alexey Solovjev Posting USHORT
j          LONG   !changed 12/20/00 due to Alexey Solovjev Posting USHORT
lastmodule STRING(20)
modulename STRING(20)
symbolname STRING(128)
ordinal    USHORT

   CODE
   OPEN(LIBfile, 0)
   i = 1
   ExportQ.SearchFlag= SearchFlag::Default
   LOOP
      GET(LIBfile, i, SIZE(LIB:header))     ! Read next OMF record
      IF ERRORCODE() OR LIB:typ = 0 OR LIB:len = 0 THEN
         BREAK                              ! All done
      END
      j = i + SIZE(LIB:header)              ! Read export info from here
      i = i + LIB:len + 3                   ! Read next OMF record from here
      IF LIB:typ = 88H AND LIB:kind = 0A000H AND LIB:bla = 1 AND LIB:ordflag = 1 THEN
          GET(LIBfile, j, SIZE(LIB:pstringval))
          symbolname = LIB:pstringval
          j += LEN(LIB:Pstringval)+1
          GET(LIBfile, j, SIZE(LIB:pstringval))
          modulename = LIB:pstringval
          j += LEN(LIB:Pstringval)+1
          GET(LIBfile, j, SIZE(LIB:ushortval))
          ordinal = LIB:ushortval
          IF modulename <> lastmodule      ! A LIB can describe multiple DLLs
             lastmodule = modulename
             ExportQ.treelevel = 1
             ExportQ.icon = 1
             ExportQ.symbol = modulename
             ExportQ.module = modulename
             ExportQ.ordinal = 0
             ExportQ.orgorder +=1   !AB
             ExportQ.ColorNFG  = glo.Colors.Defaults[1].NFG  !LONG    !Normal   Foreground color
             ExportQ.ColorNBG  = glo.Colors.Defaults[1].NBG  !LONG    !Normal   Background color
             ExportQ.ColorSFG  = glo.Colors.Defaults[1].SFG  !LONG    !Selected Foreground color
             ExportQ.ColorSBG  = glo.Colors.Defaults[1].SBG  !LONG    !Selected Background color
             ADD(ExportQ)			 
             ExportQ.ColorNFG  = glo.Colors.Defaults[2].NFG  !LONG    !Normal   Foreground color
             ExportQ.ColorNBG  = glo.Colors.Defaults[2].NBG  !LONG    !Normal   Background color
             ExportQ.ColorSFG  = glo.Colors.Defaults[2].SFG  !LONG    !Selected Foreground color
             ExportQ.ColorSBG  = glo.Colors.Defaults[2].SBG  !LONG    !Selected Background color
          END
          ExportQ.treelevel = 2
          ExportQ.icon = 0
          ExportQ.symbol = symbolname
          ExportQ.module = modulename
          ExportQ.ordinal = ordinal
          ExportQ.orgorder +=1   !AB
          !Color was set above
          ADD(ExportQ)		  
      END
   END
   CLOSE(LIBfile)

!========================================================================================
InfoWindow           PROCEDURE
   !AB, significant change
   !MG, changed color:Silver to color:BtnShadow
   !    added My Name & E-mail
   !    button1:
   !       changed exit Icon to be consistent with rest of the program,
   !       added ,DEFAULT
   !       added ,Std(Std:Close)


!Myconst     group
!!BuildDate      long(Nov302001 + 4) !Syntax error: Expression must be constant
!BuildDate       long(73392) !12/4/01
!Version         String('2')
!            end
!       STRING('Build:'),AT(145,93),USE(?String14),FONT(,,COLOR:BTNSHADOW,)
!       STRING(@D17),AT(163,93,41,10),USE(MyConst.BuildDate),FONT(,,COLOR:BTNSHADOW,)
!       STRING('Ver'),AT(102,93),USE(?VerStr),FONT(,,COLOR:BTNSHADOW,)
!       STRING(@s3),AT(116,93,,10),USE(MyConst.Version),FONT(,,COLOR:BTNSHADOW,)


infowin WINDOW('About LibMaker'),AT(,,234,124),FONT('MS Sans Serif',8,,FONT:regular),PALETTE(256),SYSTEM, |
         GRAY
       PANEL,AT(6,6,74,86),USE(?Panel1),BEVEL(5)
       IMAGE('AB256.BMP'),AT(9,9),USE(?Image1)
       GROUP,AT(85,6,139,86),USE(?Group),COLOR(COLOR:Black)
         STRING('LibMaker'),AT(87,9,135,16),USE(?String1),TRN,CENTER,FONT('Arial',14,,FONT:bold)
         BOX,AT(85,6,139,86),USE(?Box1),COLOR(COLOR:Black),FILL(COLOR:BTNSHADOW),LINEWIDTH(2)
         STRING('Modified:'),AT(97,30),USE(?String3),TRN,RIGHT
         STRING('Arnór Baldvinsson'),AT(135,30),USE(?String4),TRN
         STRING('Denmark'),AT(135,41),USE(?String6),TRN
         STRING('E-mail:'),AT(99,57),USE(?String7),TRN,RIGHT
         STRING('arnorbld@post3.tele.dk'),AT(135,57),USE(?String8),TRN
         STRING('http://www.icetips.com'),AT(135,70,76,10),USE(?String10),TRN
       END
       STRING('Additional Modifications:'),AT(4,102),USE(?String9)
       STRING('Mark Goldberg'),AT(81,102),USE(?String11),FONT(,,,FONT:bold)
       BUTTON('&Close'),AT(184,107,45,14),USE(?Button1),LEFT,ICON('Exit.ico'),STD(STD:Close),DEFAULT
     END

   CODE
   OPEN(infowin)
   ACCEPT 
   END


!========================================================================================
SelectQBE            PROCEDURE  !whole procedure written by AB
   !Updates:
   !  10/18/98 MG
   !      INI work arguments
   !      Heavy cosmetic screen changes
   !      Call to SetColors_ExportQ

   !Future Features
   !  Search Options:
   !     to Add    to   the search set (i.e. OR  support)
   !     to Remove from the search set (i.e. AND support)
   !     to Clear & Add to the search (i.e. start over)

lcl     group
SearchString    STRING(40)
SearchOption    BYTE
ExportQ_Rec     uLong
SearchFlag      like(ExportQ.SearchFlag)
UnknownFlagCnt  uLong
        end

Swindow WINDOW('Define search'),AT(,,229,38),FONT('MS Sans Serif',8,,FONT:regular),SYSTEM,GRAY
       STRING('Search for'),AT(9,3),USE(?String1)
       ENTRY(@s20),AT(45,3,180,10),USE(lcl.SearchString)
       OPTION('Search Method'),AT(3,14,121,21),USE(lcl.SearchOption),BOXED
         RADIO('Contains'),AT(15,22),USE(?Option1:Radio1),TIP('Will find symbols that have the search string in them ANYWHERE')
         RADIO('Starts with'),AT(71,22),USE(?Option1:Radio2),TIP('Will only find symbols that start with the search string')
       END
       BUTTON('&Search Now'),AT(128,21,48,14),USE(?SearchButton),DEFAULT
       BUTTON('&Cancel'),AT(180,21,45,14),USE(?CloseButton),LEFT,ICON('Exit.ico'), |
           STD(STD:Close)
     END
  !Todo: Consider changing "symbol" to "Function/Module"
  CODE
  lcl.SearchString = GetIni('Search','For'   ,'',qINI_File)
  lcl.SearchOption = GetIni('Search','Method',2 ,qINI_File)
  OPEN(Swindow)
  DISPLAY
  accept
    case ACCEPTED()
      of ?SearchButton; DO SearchQueue
                        Break !close window
    end  !case Accepted
  END !Accept Loop
  PutIni('Search','For'   ,lcl.SearchString,qINI_File)
  PutIni('Search','Method',lcl.SearchOption,qINI_File)

!-------------------------------------------------------------
SearchQueue        ROUTINE  !of SelectQBE
data
rou group
!SearchString    Like(lcl.SearchString),auto
SearchString    cstring(Size(lcl.SearchString))  !Use Cstring, so don't have to clip inside of "contains" loop
LenSS           uShort!,auto !Length of SearchStrign
    end
  code
  rou.SearchString = Upper(clip(lcl.SearchString))

  SetCursor(Cursor:Wait)
  lcl.UnknownFlagCnt = 0
  glo.FoundCount     = 0 !MG Added 12/7/01

  if lcl.SearchOption = 1  !Contains
    loop lcl.ExportQ_Rec = 1 TO Records(ExportQ)
      get(ExportQ,lcl.ExportQ_Rec)
     !lcl.SearchFlag = Choose( Instring(CLIP(UPPER(lcl.SearchString)),CLIP(UPPER(ExportQ.Symbol)),1,1) > 1 |
     !lcl.SearchFlag = Choose( Instring(clip(rou.SearchString)       ,Upper(Clip(ExportQ.Symbol)),1,1) > 0 |
      lcl.SearchFlag = Choose( Instring(rou.SearchString             ,Upper(Clip(ExportQ.Symbol)),1,1) > 0 |
                               ,SearchFlag::Found   |
                               ,SearchFlag::Default |
                             )
      do Update_ExportQ
    end !loop

  else  !SearchOption must be "Starts With"
    rou.LenSS = LEN(CLIP(lcl.SearchString))
    loop lcl.ExportQ_Rec = 1 TO Records(ExportQ)
      get(ExportQ,lcl.ExportQ_Rec)
     !lcl.SearchFlag = Choose( upper(ExportQ.Symbol[1 : LEN(CLIP(lcl.SearchString))]) = CLIP(UPPER(lcl.SearchString)) |
      lcl.SearchFlag = Choose( upper(ExportQ.Symbol[1 : rou.LenSS]) = rou.SearchString |
                               ,SearchFlag::Found   |
                               ,SearchFlag::Default |
                             )
      do Update_ExportQ
    end !loop
  end !if lcl.SearchOption = 1

  SetCursor()
  if lcl.UnknownFlagCnt  !can be incremented in Update_ExportQ ROUTINE
     Message('Unknown SearchFlag happened [' & lcl.UnknownFlagCnt & '] times','Programming Error',Icon:Exclamation)
  end
	
  ExportQ_to_FilterQ()
	
!-------------------------------------------------------------
Update_ExportQ  ROUTINE  !of SelectQBE, called by SearchQueue
  !todo set ExportQ.Icon to match Default & Found
  !  note: will need to set prop:iconList,2 & 3 upstream

  if lcl.SearchFlag = SearchFlag::Found then glo.FoundCount += 1 end !MG Added 12/7/01

  case lcl.SearchFlag
    of ExportQ.SearchFlag
             !do nothing we're already set

    of SearchFlag::Default
             ExportQ.ColorNFG = glo.Colors.Defaults[ExportQ.treeLevel].NFG
             ExportQ.ColorNBG = glo.Colors.Defaults[ExportQ.treeLevel].NBG
             ExportQ.ColorSFG = glo.Colors.Defaults[ExportQ.treeLevel].SFG
             ExportQ.ColorSBG = glo.Colors.Defaults[ExportQ.treeLevel].SBG
             ExportQ.SearchFlag   = lcl.SearchFlag
             case ExportQ.TreeLevel
               of  1; ExportQ.Icon = 1
               of -1; ExportQ.Icon = 2
             else   ; ExportQ.Icon = 0
             end
             Put(ExportQ)

    of SearchFlag::Found
             ExportQ.ColorNFG = glo.Colors.Found[ExportQ.treeLevel].NFG
             ExportQ.ColorNBG = glo.Colors.Found[ExportQ.treeLevel].NBG
             ExportQ.ColorSFG = glo.Colors.Found[ExportQ.treeLevel].SFG
             ExportQ.ColorSBG = glo.Colors.Found[ExportQ.treeLevel].SBG
             ExportQ.SearchFlag   = lcl.SearchFlag
             ExportQ.Icon         = 3 !found
             Put(ExportQ)

  else       lcl.UnknownFlagCnt += 1
  end !case lcl.SearchFlag

!========================================================================================
ExportQ_to_FilterQ   PROCEDURE()
ExportPtr LONG,AUTO
  CODE
  FREE(FilteredQ)
  LOOP ExportPtr = 1 TO RECORDS(ExportQ)
     GET(ExportQ, ExportPtr)
     IF ExportQ.Icon = 3 !Found
        FilteredQ = ExportQ
        ADD(FilteredQ)
     END  
  END

!EndRegion - Tmp Region

!========================================================================================
GenerateMap      PROCEDURE(BYTE argRonsFormat) !writes out all info in the export Q to an ASCII file, to aid in building CW Map statements
  !Created 10/6/98 by Monolith Custom Computing, Inc.  
  !todo, review whole ExportQ, and indent to maximum for a prettier file


  !Consider support for Ron Schofield's .API file format
  ! <header>
  ! <prototypes>
  !
  ! a single header is:
  ! API Name|Library|DLL|Equate File|Structure File
  !
  ! multiple prototypes, one per line:   one is:
  ! Procedure|Prototype|Proc Attributes|Proc Name



lcl             group
exq_rec            USHORT
symbol             like(ExportQ.symbol)!,auto
instringLoc        ushort
CurrModule         like(ExportQ.Module) !no auto
Assumed_Attributes string(',pascal,raw,dll(1)')
MaxSymLen   LONG
                end

   CODE
   lcl.MaxSymLen = LongestSymbol() !in ExportQ Level 2   

   CREATE(Asciifile)
   OPEN  (Asciifile)

   LOOP lcl.exq_rec = 1 TO RECORDS(ExportQ)
      GET(ExportQ, lcl.exq_rec)
      case ExportQ.Treelevel
        of 1; DO   TreeLevel1
        of 2; DO   TreeLevel2
      end 
   END

   if argRonsFormat
        Ascii:Line = ''
   else Ascii:Line = '<32>{5}end !module(''' & clip(lcl.CurrModule) & ''')'     !Should be what we saved the .LIB as
   end
   
   Add  (ASCIIFile)
   CLOSE(Asciifile)

TreeLevel1 ROUTINE        
    if lcl.exq_rec >1
       if argRonsFormat
            Ascii:Line = ''
       else Ascii:Line = '<32>{5}end !module(''' & clip(lcl.CurrModule) & ''')'     !Should be what we saved the .LIB as
       end
       Add(ASCIIFile)
       Ascii:Line = ''
       Add(ASCIIFile)
    end
    !---------- replace the .DLL or .EXE module extension with .LIB
    lcl.CurrModule      = ExportQ.module
    lcl.instringLoc     = instring('.DLL',upper(lcl.CurrModule),1,1)
    if ~lcl.instringLoc
        lcl.instringLoc = instring('.EXE',upper(lcl.CurrModule),1,1)
    end
    if lcl.instringLoc
       lcl.CurrModule[lcl.instringLoc + 1 : lcl.instringLoc + 3] = 'lib'
    end
    !---------- replace the .DLL or .EXE module extension with .LIB -end
    if argRonsFormat
         ! API Name|Library|DLL|Equate File|Structure File
         Ascii:Line = 'Todo Header Line: API Name|Library|DLL|Equate File|Structure File'  !<--- todo
    else Ascii:Line = '<32>{5}module(''' & clip(lcl.CurrModule) & ''')'             !Should be what we saved the .LIB as
    end
    Add(ASCIIFile)

TreeLevel2 ROUTINE
    lcl.symbol = CleanSymbol(ExportQ.symbol)
    if argRonsFormat
         ! Procedure|Prototype|Proc Attributes|Proc Name
         Ascii:Line =             clip(lcl:symbol) &'|<32>{40}|'& clip(lcl.Assumed_Attributes) & '|'   & clip(ExportQ.symbol)
    else Ascii:Line = '<32>{8}' & clip(lcl.symbol) & all('<32>',lcl.MaxSymLen - len(clip(lcl.Symbol))) & '(<9>{4}),pascal,raw,dll(1),name(''' & clip(ExportQ.symbol) & ''')'
    end
    Add(ASCIIFile)
   
!========================================================================================
CleanSymbol   PROCEDURE(STRING xSymbol)!,STRING
AtIsAt   LONG,AUTO
   CODE   
   LOOP
      CASE xSymbol[1]
        OF '_'  
      OROF '?'  
      OROF '@'
              xSymbol = xSymbol[ 2 : SIZE(xSymbol) ]
      ELSE    
              BREAK
      END         
   END
   
   AtIsAt = INSTRING('@', xSymbol,1)
   IF  AtIsAt <> 0
       xSymbol = xSymbol[ 1 : AtIsAt - 1 ]  !BUG: for some DLL`s, I found one compiled with CBuilder 1.0, that this would be bad for
   end   
   RETURN xSymbol
   
!========================================================================================
LongestSymbol PROCEDURE() !in ExportQ Level 2
QRec          LONG,AUTO
RetMaxSymLen  LONG
CurrSymLen    LONG,AUTO
symbol        LIKE(ExportQ.symbol)!,auto
instringLoc   LONG,AUTO
   CODE   
   RetMaxSymLen = 0
   LOOP QRec = 1 TO RECORDS(ExportQ)
      GET(ExportQ, QRec)
      case ExportQ.treelevel
        of 2; CurrSymLen = len(clip( CleanSymbol( ExportQ.symbol)))
              if RetMaxSymLen < CurrSymLen
                 RetMaxSymLen = CurrSymLen
              end
              !MG: should cache this strip'd symbol in a queue, and process that intead vs. duplicating the effort below
      end
   end
   RETURN RetMaxSymLen
   
!========================================================================================

GenerateClasses   PROCEDURE
QRec           LONG,AUTO
GenDir         CSTRING (FILE:MaxFilePath+1)
CLW_Name       CSTRING (FILE:MaxFilePath+1)
INC_Name       CSTRING (FILE:MaxFilePath+1)
ModuleBaseName CSTRING(SIZE(ExportQ.module) + 1)
ClassName      LIKE(ModuleBaseName)
MaxSymLen      LONG,AUTO !Used For Alignment
   CODE  
   
   DO PromptForFolder !may return
   SETCURSOR(Cursor:Wait)

   MaxSymLen = LongestSymbol() + 1 !in ExportQ Level 2   
   IF MaxSymLen < 10
      MaxSymLen = 10
   END

   DO Generate:AllINC
   DO Generate:AllCLW
   
   SETCURSOR()

PromptForFolder ROUTINE !may return
   GenDir = 'C:\Tmp\GenDir' !<-- TODO Fix
   IF NOT FileDialog('Where should I generate the classes?', GenDir, 'All Directories|*.*',FILE:KeepDir + FILE:NoError + FILE:LongName + FILE:Directory)
      RETURN
   END
   IF GenDir[LEN(GenDir)]<>'\'
      GenDir =   GenDir  & '\'
   END


SetNames            ROUTINE
   ModuleBaseName = SUB(ExportQ.module, 1, INSTRING('.',ExportQ.module,1,1) - 1)
   CLW_Name       = 'ct' & ModuleBaseName & '.clw'
   INC_Name       = 'ct' & ModuleBaseName & '.inc'
   ClassName      = 'ct' & ModuleBaseName 

Ascii_Start             ROUTINE   
   CREATE(ASCIIfile)
   IF ERRORCODE()
      MESSAGE('Failed to Create File['& FileName &']')
      !Now what?
   END
   OPEN(ASCIIFile)

Ascii_Done              ROUTINE
      FLUSH(ASCIIFile)
      CLOSE(ASCIIfile)

!Region Generate:INC
Generate:AllINC    ROUTINE
   GET(ExportQ, 1)
   LOOP WHILE NOT ERRORCODE()
      DO SetNames
      FileName = GenDir & INC_Name
      DO Ascii_Start      
      DO Generate:OneINC_Fill   !<---
      DO Ascii_Done      
      GET( ExportQ,  POINTER(ExportQ) +1)
   END

   
Generate:OneINC_Methods   ROUTINE
   GET( ExportQ,  POINTER(ExportQ) +1 )
   LOOP WHILE NOT ERRORCODE()
      CASE ExportQ.treelevel
        OF 1; BREAK
        OF 2; AppendAscii( CLIP(ExportQ.symbol) & ALL(' ',MaxSymLen - LEN(CLIP(ExportQ.symbol))) &'PROCEDURE(   )')
      END
      GET( ExportQ,  POINTER(ExportQ) +1)
   END   

   
   
   
Generate:OneINC_Fill ROUTINE   
   AppendAscii('!ABCIncludeFile(Yada)')
   AppendAscii('')
   AppendAscii('! This File Was Generated by LibMaker on '& FORMAT(TODAY(),@D1) &' at '& FORMAT(CLOCK(),@T1))
   AppendAscii('! Arguments and return values are unknown')
   AppendAscii('')
   AppendAscii('INCLUDE(<39>API.EQU<39>),ONCE')
   AppendAscii('')
   AppendAscii(ClassName &'  CLASS,TYPE,MODULE(<39>' & CLW_Name & '<39>),LINK(<39>' & CLW_Name & '<39>) !,_YadaLinkMode),DLL(_YadaDllMode)')
   AppendAscii('CONSTRUCT'& ALL(' ',MaxSymLen - 9) &'PROCEDURE()')
   AppendAscii('DESRTRUCT'& ALL(' ',MaxSymLen - 9) &'PROCEDURE()')
   DO Generate:OneINC_Methods
   AppendAscii( ALL(' ', LEN(ClassName) + 2) & 'END')
!EndRegion Generate:INC
   
Generate:AllCLW    ROUTINE
   GET(ExportQ, 1)
   LOOP WHILE NOT ERRORCODE()
      DO SetNames
      FileName = GenDir & CLW_Name
      DO Ascii_Start      
      DO Generate:OneCLW_Fill   !<---
      DO Ascii_Done      
      GET( ExportQ,  POINTER(ExportQ) +1)
   END

Generate:OneCLW_Fill ROUTINE   
   DATA
OrigPtr LONG,AUTO   
   CODE
   AppendAscii('    MEMBER()')
   AppendAscii('    MAP')
                           OrigPtr = POINTER(ExportQ)
     DO Generate:OneCLW_MAP
                           GET(ExportQ, OrigPtr)
   AppendAscii('    END')
   AppendAscii('')
   AppendAscii('    INCLUDE(<39>'& INC_Name  &'<39>),ONCE')
   AppendAscii('')
   
   ExportQ.symbol = 'CONSTRUCT'; DO Generate:OneCLW_Methods:One
   ExportQ.symbol = 'DESTRUCT' ; DO Generate:OneCLW_Methods:One
   
   DO Generate:OneCLW_Methods

Generate:OneCLW_MAP     ROUTINE
   DATA
szCleanedSymbol   CSTRING(SIZE(ExportQ.symbol) + 1)
   CODE
   GET( ExportQ,  POINTER(ExportQ) +1 )
   LOOP WHILE NOT ERRORCODE()
      CASE ExportQ.treelevel
        OF 1; BREAK
        OF 2; szCleanedSymbol = CLIP(CleanSymbol(ExportQ.symbol))
              AppendAscii( '<32>{8}'  |
                          & szCleanedSymbol |
                          & ALL('<32>',MaxSymLen - LEN(szCleanedSymbol)) |
                          & '(<9>{4}),pascal,raw,dll(1),name(<39>' & CLIP(ExportQ.symbol) & '<39>)')
      END
      GET( ExportQ,  POINTER(ExportQ) +1)
   END   
   

Generate:OneCLW_Methods ROUTINE
   GET( ExportQ,  POINTER(ExportQ) +1 )
   LOOP WHILE NOT ERRORCODE()
      CASE ExportQ.treelevel
        OF 1; BREAK
        OF 2; DO Generate:OneCLW_Methods:One
      END
      GET( ExportQ,  POINTER(ExportQ) +1)
   END   

Generate:OneCLW_Methods:One   ROUTINE
   DATA
szCleanedSymbol   CSTRING(SIZE(ExportQ.symbol) + 1)
   CODE
   !AppendAscii( CLIP(ExportQ.symbol) & ALL(' ',MaxSymLen - LEN(CLIP(ExportQ.symbol))) &'PROCEDURE(   )')
   szCleanedSymbol = CLIP(CleanSymbol(ExportQ.symbol))
   AppendAscii('')
   AppendAscii(ClassName &'.'& szCleanedSymbol    & ALL('<32>',MaxSymLen - LEN(szCleanedSymbol))   & 'PROCEDURE(  )')
   AppendAscii('   CODE')
   
               

  


AppendAscii    PROCEDURE(STRING xLine)  
   CODE
   ASCII:Line = xLine
   ADD(ASCIIFile)
   

   
