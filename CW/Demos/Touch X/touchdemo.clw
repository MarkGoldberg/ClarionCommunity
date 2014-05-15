    PROGRAM               


    INCLUDE( 'Responders.inc'),ONCE
   !INCLUDE(      'Touch.inc'),ONCE !<-- Implied by Responders

    INCLUDE( 'ctTechData.inc'),ONCE
    INCLUDE('TouchHelper.inc'),ONCE 
oTouchHelper TouchHelper    !This class has no properties, just methods

    INCLUDE(    'debuger.inc'),ONCE
DBG        Debuger


FEQ         EQUATE(SIGNED)
TIME:Tick   EQUATE(1)
TIME:Second EQUATE(100 * TIME:Tick)

EXTENT      GROUP,TYPE
cx             LONG
cy             LONG
            END

  MAP
     TouchTest()
     MODULE('')
       OutputDebugString (CONST *CSTRING),PASCAL,NAME('OutputDebugStringA')
     END
     ODS (STRING Msg)  !Clarionized OutputDebugString
  END

  CODE
  DBG.mg_init('TouchDemo')  
  DBG.ClearLog()


  SYSTEM{PROP:Icon} = '~Touch.ico'      
  TouchTest()  ; ODS('^TouchDemo')
  RETURN


! =============================================================================
! =============================================================================
! =============================================================================
TouchTest  PROCEDURE()

TouchTest   BYTE(0)
ZoomTest    BYTE(0)
TechData    BYTE(0) !Should we display some cool Coordinates and stats?

TechData_Starts  ctTechData !Technical Data - with a queue, we will use it to store the -- start points
TechData_Drops   ctTechData !Technical Data - with a queue, we will use it to store the -- points when the finger leaves the screen
TechData_Paths   ctTechData !Technical Data - with a queue, we will use it to store the -- coordinates, real time
!Vestigial UI
!            CHECK('Touch'),AT(1,1,9,6),USE(TouchTest),SKIP,TRN,HIDE,FONT('Microsoft Sans Serif',10),ICON('touch.png')
!            CHECK('Zoom'),AT(1,9,9,6),USE(ZoomTest),SKIP,TRN,HIDE,ICON('Fit-To-Size.png')

W   WINDOW('Touch Input Test'),AT(,,524,258),CENTER,IMM,AUTO,SYSTEM,MAX,FONT('MS Shell Dlg',12),COLOR(COLOR:White),WALLPAPER('wp.jpg'),Tiled
        TOOLBAR,AT(0,0,524,47),USE(?TB),FONT(,,COLOR:White),COLOR(COLOR:Black),WALLPAPER('toolbar.jpg'),Tiled
            BUTTON,AT(11,24,23,20),USE(?Close),SKIP,STD(STD:Close),FONT('Microsoft Sans Serif',8,COLOR:Gray),ICON('close.png'),FLAT,TRN
            BUTTON,AT(164,0,56,48),USE(?Touch_BTN),SKIP,FONT('Microsoft Sans Serif',8),COLOR(COLOR:Black),ICON('wtouch.png'),FLAT,TRN
            BUTTON,AT(222,0,56,48),USE(?Drag_BTN),SKIP,FONT('Microsoft Sans Serif',8),ICON('drag.png'),FLAT,TRN
            BUTTON('Tech Data'),AT(280,0,56,48),USE(?Tech_Data),SKIP,FONT('Microsoft Sans Serif',8),COLOR(COLOR:Black),ICON('off.png'),FLAT,TRN
            TEXT,AT(341,0,63,48),USE(?Starts),SKIP,TRN,FLAT,FONT('Microsoft Sans Serif',7,0CEFFFFH),READONLY
            TEXT,AT(406,0,52,48),USE(?Paths),SKIP,TRN,FLAT,FONT('Microsoft Sans Serif',7,0FFFFE2H),READONLY
            TEXT,AT(460,0,63,48),USE(?Drops),SKIP,TRN,FLAT,FONT('Microsoft Sans Serif',7,0FFFEFEH),COLOR(COLOR:Black),READONLY
        END
        IMAGE('touch_mode.png'),AT(7,2,157,48),USE(?Selected_Mode),HIDE,TILED
        IMAGE('morfo.png'),AT(199,49,87,72),USE(?Img),HIDE
    END

               

gtPointInfo     GROUP,TYPE !<-- specific to this application's UI
TouchPointID      UNSIGNED 
RepresentFEQ      FEQ
TimeToAlter       LONG 
TimeToDestroy     LONG
                END

TouchResponder  CLASS(ResponderTouchType)
PointInfo         GROUP(gtPointInfo),DIM(10) !10 is a bad idea, it was set due to 10 fingers, but you can drag, release, drag release quickly and have many more than 10 
                  END
StartTest         PROCEDURE()
EndTest           PROCEDURE()
Tick              PROCEDURE(LONG)
FindPt            PROCEDURE(UNSIGNED),UNSIGNED
OnEvent           PROCEDURE(*TouchData xTouchData),BYTE,PROC,DERIVED
                END

ZoomResponder  CLASS(ResponderGestureType)
X                UNSIGNED  
Y                UNSIGNED
W                UNSIGNED
H                UNSIGNED
Org              LIKE(TPOINT)
NaturalSz        LIKE(EXTENT)
Sz               LIKE(EXTENT)

StartTest        PROCEDURE()
EndTest          PROCEDURE()

OnEvent          PROCEDURE(*InputData xInputData),BYTE,PROC,DERIVED
               END

            
  CODE
  DO PreAcceptLoop
  DO    AcceptLoop
                  ODS('  TouchTest Closing Window')
  CLOSE (W)      ;ODS('^ TouchTest')
  RETURN

PreAcceptLoop      ROUTINE

  OPEN (W)
  W{PROP:Pixels}      = TRUE
  W{PROP:Buffer}      = 1
  W{PROP:LazyDisplay} = 1 
  !Once the wallpaper and images prob is solved, in the meanwhile it should stay static.
  W{PROP:Maximize}    = TRUE 

  TechData_Starts.INIT('Starts',?Starts)
  TechData_Drops .INIT('Drops' ,?Drops )
  TechData_Paths .INIT('Paths' ,?Paths )
 
  TouchResponder.Init( W {PROP:TouchInterface} ) 
  ZoomResponder .Init( W {PROP:TouchInterface} )

!Region AcceptLoop
AcceptLoop                     ROUTINE
  ACCEPT             
    DBG.PrintEvent()
                  
    CASE EVENT()
      OF EVENT:Timer    ; IF TouchTest
                             TouchResponder.Tick (CLOCK())
                          END    

      OF EVENT:Accepted ; CASE ACCEPTED()
                            OF ?Touch_BTN  ; DO Accepted:Touch_BTN     
                            OF ?Drag_BTN   ; DO Accepted:Drag_BTN
                            OF ?Tech_Data  ; DO Accepted:Tech_Data
                          END
    END       
  END

Accepted:Touch_BTN             ROUTINE
  IF TouchTest = TRUE
     TouchTest = FALSE
     DO StopTouchTest
  ELSE
     TouchTest = TRUE
     DO StopZoomTest
     DO StartTouchTest
  END            
  SETCURSOR()

Accepted:Drag_BTN              ROUTINE
  IF ZoomTest = TRUE
     ZoomTest = FALSE
     DO StopZoomTest
  ELSE
     ZoomTest = TRUE
     DO StopTouchTest
     DO StartZoomTest
  END
                               
Accepted:Tech_Data             ROUTINE
  IF  TechData = TRUE
      TechData = FALSE
      ?Tech_Data{PROP:Icon} = '~off.png'
      HIDE(?Starts)
      HIDE(?Paths)
      HIDE(?Drops)
  ELSE
      TechData = TRUE
      ?Tech_Data{PROP:Icon} = '~on.png'
      UNHIDE(?Starts)
      UNHIDE(?Paths)
      UNHIDE(?Drops)
  END

! ----------------------------------------------------------------------------
StartTouchTest                 ROUTINE
  DO StartTouchTest:ProgramSpecific
  TouchResponder.StartTest() 
  TouchResponder.EnableEvents() !SELF.Notifier.TouchResponder (TouchResponder.IPointerResponder) !<-- TOUCH

StartTouchTest:ProgramSpecific ROUTINE
  ?Selected_Mode{PROP:Text} = '~touch_mode.png'
  UNHIDE(?Selected_Mode)
  W{PROP:Text} = 'Touch Mode Selected'

  TechData_Drops .HideData()
  TechData_Starts.HideData()
  TechData_Paths .HideData()
  

! ----------------------------------------------------------------------------
StartZoomTest                  ROUTINE    
  DO StartZoomTest:ProgramSpecific
  ZoomResponder.StartTest()                             
 !ZoomResponder.Notifier.AllowZoomGesture (?Img, TRUE) 
!v-- debugging attempt, didn't help
!  ZoomResponder.Notifier.AllowZoomGesture (?Touch_BTN , FALSE) 
!  ZoomResponder.Notifier.AllowZoomGesture (?Drag_BTN  , FALSE) 
!  ZoomResponder.Notifier.AllowZoomGesture (?Tech_Data , FALSE) 
!^-- debugging attempt, didn't help
  ZoomResponder.EnableEvents()  !Notifier.InputResponder (ZoomResponder.IGestureResponder) !<-- TOUCH

StartZoomTest:ProgramSpecific  ROUTINE
         ?Selected_Mode{PROP:Text} = '~drag_mode.png'
  UNHIDE(?Selected_Mode)
  W{PROP:Text} = 'Zoom Mode Selected'
  SETCURSOR('~morfo.cur')

  TechData_Drops .HideData()
  TechData_Starts.HideData()
  TechData_Paths .HideData()


! ----------------------------------------------------------------------------
StopTouchTest                  ROUTINE   
  DO StopTouchTest:ProgramSpecific
  TouchResponder.DisableEvents() ! Notifier.TouchResponder()             !<-- TOUCH
  TouchResponder.EndTest()

StopTouchTest:ProgramSpecific  ROUTINE
  HIDE(?Selected_Mode)
  W{PROP:Text} = 'SoftVelocity Touch Demo'
  TouchTest = FALSE

  TechData_Paths .Free()
  TechData_Starts.Free()
  TechData_Drops .Free()
  TechData_Paths .ShowData(1,1)
  TechData_Starts.ShowData(1,1)
  TechData_Drops .ShowData(1,1)

! ----------------------------------------------------------------------------
StopZoomTest                   ROUTINE
  DO StopZoomTest:ProgramSpecific
  ZoomResponder.DisableEvents() ! Notifier.InputResponder()             !<-- TOUCH
  ZoomResponder.EndTest()

StopZoomTest:ProgramSpecific   ROUTINE
  HIDE(?Selected_Mode)
  W{PROP:Text} = 'SoftVelocity Touch Demo'
  SETCURSOR()
  ZoomTest = FALSE

 
!EndRegion AcceptLoop   
           


! =============================================================================
!
! Multi-touch test
!    
! =============================================================================
!Region TouchResponder Methods

TouchResponder.StartTest  PROCEDURE()
  CODE       
  CLEAR(SELF.PointInfo)
  W {PROP:Timer} = 20


! ----------------------------------------------------------------------------
TouchResponder.EndTest    PROCEDURE()
i        UNSIGNED,AUTO
  CODE
  W {PROP:Timer} = 0

  LOOP i = 1 TO MAXIMUM(SELF.PointInfo, 1)
    IF         SELF.PointInfo[i].RepresentFEQ <> 0
      DESTROY (SELF.PointInfo[i].RepresentFEQ)
    END
    CLEAR(SELF.PointInfo[i])
  END


! ----------------------------------------------------------------------------
TouchResponder.Tick       PROCEDURE (LONG tm)    
i        UNSIGNED,AUTO
FEQExist BOOL(FALSE)
PtInfo   &gtPointInfo
  CODE
  LOOP i = 1 TO MAXIMUM(SELF.PointInfo, 1)
    PtInfo  &= SELF.PointInfo[i]     

    IF PtInfo.RepresentFEQ THEN FEQExist = TRUE END
   
    IF    PtInfo.TimeToAlter  <> 0   |
      AND PtInfo.TimeToAlter  <  tm  |
      AND PtInfo.RepresentFEQ <> 0   |
    THEN 
       PtInfo.RepresentFEQ {PROP:Text} = '~dying_dot.png'
    END
        
    IF PtInfo.TimeToDestroy <> 0 AND PtInfo.TimeToDestroy < tm      
      IF PtInfo.RepresentFEQ <> 0  
         DESTROY (PtInfo.RepresentFEQ)
      END   
      CLEAR(PtInfo) ! orig source left .TouchPointID  unchanged
    END
  END

  IF NOT FEQExist
     0{PROP:Timer} = 0
  END


! ----------------------------------------------------------------------------
TouchResponder.FindPt     PROCEDURE (UNSIGNED id)
i       UNSIGNED,AUTO
  CODE
  i = MAXIMUM(SELF.PointInfo, 1)

  LOOP
    IF SELF.PointInfo[i].TouchPointID = id AND SELF.PointInfo[i].RepresentFEQ <> 0 
       RETURN i
    END
    i -= 1
  UNTIL i = 0

  RETURN 0


! ----------------------------------------------------------------------------
TouchResponder.OnEvent PROCEDURE(*TouchData xTouchData) !BYTE,PROC,VIRTUAL  !Called by Parent
pt      &TouchPoint,AUTO
i       UNSIGNED,AUTO
ret     BYTE(FALSE)      
xy      LIKE(TPOINT),AUTO
    CODE
    pt &= xTouchData.ptAction

?   ODS('TouchResponder ' &  oTouchHelper.TouchPoint_ToString(pt))
                     
    i = SELF.FindPt (pt.ID)
    IF BAND (pt.PTFlags, PTFLAG_NEW)
       IF i = 0
          DO Touched:New
       END
    ELSE
       IF i <> 0
         DO Touched:Existing  !Note: i is side-effected in
       END
    END

    RETURN RET
!----------------------- ------------------
Touched:Existing    ROUTINE
    DATA
PIC    CSTRING(32),AUTO
    CODE
    !IF       BAND(pt.PTFlags, PTFLAG_UP)    
    !END    
    IF        BAND(pt.PTFlags, PTFLAG_INCONTACT)            THEN  PIC = '~p_dot.png'       !;clr = COLOR:Blue           
    ELSIF     BAND(pt.PTFlags, PTFLAG_CANCELED + PTFLAG_UP) THEN  PIC = '~alive_dot.png'   ; DO Touched:Canceled_Up  !;clr = COLOR:Red
    ELSIF NOT BAND(pt.PTFlags, PTFLAG_DOWN)                 THEN  PIC = '~dying_dot.png'   !;clr = COLOR:Yellow 
    ELSE                                                          PIC = '~default_dot.png' !;clr = COLOR:Blue                    
    END

    IF        BAND(pt.PTFlags, PTFLAG_UPDATE)               THEN  DO Touched:Update
    END      
    SELF.PointInfo[i].RepresentFEQ {PROP:Text} = PIC            
    ret = TRUE

!-----------------------
Touched:Canceled_Up ROUTINE   !aka Drops         
    SELF.PointInfo[i].TimeToAlter   = CLOCK() + 1.3 * TIME:Second
    SELF.PointInfo[i].TimeToDestroy = CLOCK() + 1.7 * TIME:Second
     
    IF TechData
       TechData_Drops.LOG(SELF.PointInfo[i].RepresentFEQ, 'Point ' & SELF.PointInfo[i].RepresentFEQ & ' Dropped at ' & SELF.PointInfo[i].RepresentFEQ{PROP:Xpos} & ',' & SELF.PointInfo[i].RepresentFEQ{PROP:Ypos}) 
       !Note: Orig Code, was add only, no updates
    END


Touched:Update      ROUTINE
    xy.x = pt.ptlocal.x
    xy.y = pt.ptlocal.y
    SETPOSITION (SELF.PointInfo[i].RepresentFEQ, xy.x - 40, xy.y - 40, 80, 80)

    IF TechData
       TechData_Paths.LOG( SELF.PointInfo[i].RepresentFEQ, 'Point ' & SELF.PointInfo[i].RepresentFEQ & ' at ' & xy.x - 40 & ',' & xy.y - 40 )
    END
  
!----------------------- ------------------
Touched:New         ROUTINE             
  DATA
PtInfo   &gtPointInfo
  CODE                                 
  i = MAXIMUM(SELF.PointInfo, 1)

  LOOP UNTIL SELF.PointInfo[i].RepresentFEQ = 0
        i -= 1
  UNTIL i = 0

  IF i <> 0
      PtInfo  &= SELF.PointInfo[i]       

      PtInfo.RepresentFEQ = CREATE (0, CREATE:Image)
      PtInfo.RepresentFEQ{PROP:Text}     = '~default_dot.png'
      PtInfo.RepresentFEQ{PROP:Centered} = TRUE !If we do not do this, then the pictures will be scaled and will produce artifacts

      xy.x = pt.ptlocal.x
      xy.y = pt.ptlocal.y
      SETPOSITION (PtInfo.RepresentFEQ, xy.x - 40, xy.y - 40, 80, 80)
     
      UNHIDE (PtInfo.RepresentFEQ)
                              
      PtInfo.TouchPointID  = pt.ID
      PtInfo.TimeToDestroy = 0
      PtInfo.TimeToAlter   = 0

      IF TechData
         TechData_Starts.LOG( PtInfo, 'Started point ' & PtInfo.RepresentFEQ & ' at ' & xy.x - 40 & ',' & xy.y - 40  )            
         !Note: Orig Code, was add only, no updates 
      END
      
      ret = TRUE

      IF 0{PROP:Timer} = 0
         0{PROP:Timer} = 20
      END
  END


!EndRegion TouchResponder Methods
! =============================================================================
!
! Zoom test
!    
! =============================================================================

!Region ZoomResponder Methods
ZoomResponder.StartTest                PROCEDURE()
  CODE
  DO StartTest:ProgramSpecific       
  SELF.Notifier.AllowZoomGesture (?Img, TRUE) 

StartTest:ProgramSpecific ROUTINE
  DATA
WindowW      UNSIGNED,AUTO
WindowH      UNSIGNED,AUTO
RandomImage  LONG,AUTO
  CODE
  !--- Let's spice up this test
    RandomImage = RANDOM(1,5)
    CASE RandomImage
      OF 1; ?Img{PROP:Text} = '~morfo.png'
      OF 2; ?Img{PROP:Text} = '~sv_logo.png'
      OF 3; ?Img{PROP:Text} = '~clarion.png'
      OF 4; ?Img{PROP:Text} = '~H5.png'
      OF 5; ?Img{PROP:Text} = '~CC_LOG.JPG' 
    END 
        
  ! --- Get natural image size
  ?Img {PROP:NoWidth}  = TRUE
  ?Img {PROP:NoHeight} = TRUE

  GETPOSITION (?Img,,, SELF.W, SELF.H)

  SELF.NaturalSz.cx = SELF.W
  SELF.NaturalSz.cy = SELF.H

  ! --- Place it in the center of WINDOW originally
  GETPOSITION (0,,, WindowW, WindowH)
  WindowH -= ?TB {PROP:Height}

  SELF.X = (WindowW - SELF.W) / 2
  SELF.Y = (WindowH - SELF.H) / 2


  SETPOSITION (?Img, SELF.X, SELF.Y)
       
  
  ! --- Unhide and activate
  UNHIDE (?Img)

  ?Img {PROP:Active} = TRUE   ; ODS('setting IMG PROP:Active = TRUE')     !<--- TOUCH
  !LRM["PROP:Active"] = Setting PROP:Active to TRUE for an IMAGE control (runtime only) makes it active:
  !                     it can be a parent/child of other controls and 
  !                     is painted separately without caching into the metafile of the owner window's background (i.e.,IMAGE controls with scroll bars).
  

! ----------------------------------------------------------------------------
ZoomResponder.EndTest                  PROCEDURE()
  CODE
  SELF.Notifier.AllowZoomGesture (?Img, FALSE)
  HIDE (?Img)

! ----------------------------------------------------------------------------

ZoomResponder.OnEvent PROCEDURE(*InputData xInputData)!,BYTE,PROC,DERIVED  !Called by Parent
WW      UNSIGNED,AUTO
WH      UNSIGNED,AUTO
sz      LIKE(EXTENT),AUTO
RetVal  BYTE(TRUE)
   CODE           
   ODS('ZoomResponder.OnEvent - '&   oTouchHelper.InputData_ToString(xInputData) )                             

   CASE xInputData.IA
     OF GESTURE_PAN     ; DO Gesture:Pan

     OF GESTURE_ZOOMIN
   OROF GESTURE_ZOOMOUT ; DO Gesture:Zoom

   ELSE                 ; !ODS('ZOOM Gesture['& gesture.IA &']')
                          RetVal = FALSE
   END

   RETURN RetVal

Gesture:Pan         ROUTINE
  IF        BAND (xInputData.Param, INFLAG_BEGIN) THEN DO Gesture:Pan:Begin
  ELSIF NOT BAND (xInputData.Param, INFLAG_END)   THEN DO Gesture:Pan:NotEnd
  END

Gesture:Pan:Begin   ROUTINE
                                        !ODS('Gesture Pan - Begin')
    GETPOSITION (?Img, SELF.X, SELF.Y, SELF.W, SELF.H)
    SELF.Org.x = SELF.X
    SELF.Org.y = SELF.Y

Gesture:Pan:NotEnd  ROUTINE
  DATA
dx      SIGNED,AUTO
dy      SIGNED,AUTO
  CODE
                                        !ODS('Gesture Pan - NotEnd')
    dx = xInputData.ptaction.pt.x - xInputData.pt2.pt.x
    dy = xInputData.ptaction.pt.y - xInputData.pt2.pt.y

    IF dx <> 0 OR dy <> 0
      GETPOSITION (?Img,,, SELF.W, SELF.H)
      GETPOSITION (0   ,,, WW    , WH)
      WH -= ?TB {PROP:Height}
        
      SELF.X = SELF.Org.X + dx
      
      IF SELF.X + SELF.W > WW
         SELF.X =          WW - SELF.W
      END
      IF SELF.X < 0
         SELF.X = 0
      END

      SELF.Y = SELF.Org.Y + dy
      
      IF SELF.Y + SELF.H > WH
         SELF.Y =          WH - SELF.H 
      END
      IF SELF.Y < 0
         SELF.Y = 0
      END
        
                                                        !HIDE(?Img)   !Attempt to solve Artifacts, results in hard to drag image
      SETPOSITION (?Img, SELF.X, SELF.Y)
                                                        !UNHIDE(?Img) !Attempt to solve Artifacts, results in hard to drag image
        
      ?Paths{PROP:Text} =  'Current Coords ' & SELF.X & ',' & SELF.Y
    END

Gesture:Zoom        ROUTINE
      IF        BAND (xInputData.Param, INFLAG_BEGIN) THEN DO Gesture:Zoom:Begin
      ELSIF NOT BAND (xInputData.Param, INFLAG_END)   THEN DO Gesture:Zoom:NotEnd
      END

Gesture:Zoom:Begin  ROUTINE
                                       !ODS('Gesture Zoom - Begin')
      GETPOSITION (?Img, SELF.X, SELF.Y, SELF.W, SELF.H)
      
      SELF.Sz.cx = SELF.W
      SELF.Sz.cy = SELF.H
      SELF.Org.x = SELF.X + SELF.W / 2
      SELF.Org.y = SELF.Y + SELF.H / 2

Gesture:Zoom:NotEnd ROUTINE
      !Consider Caching the initial .Ratio, and subtracting it from all remaining
      ! to correct for the initial jump
      IF xInputData.IA = GESTURE_ZOOMIN
        sz.cx = INT (SELF.Sz.cx * xInputData.Ratio)
        sz.cy = INT (SELF.Sz.cy * xInputData.Ratio)
      ELSE
        sz.cx = INT (SELF.Sz.cx / xInputData.Ratio)
        sz.cy = INT (SELF.Sz.cy / xInputData.Ratio)
      END

      IF sz.cx <> SELF.W OR sz.cy <> SELF.H
        GETPOSITION (0,,, WW, WH)
        WH -= ?TB {PROP:Height}

        IF sz.cx < SELF.NaturalSz.cx OR sz.cy < SELF.NaturalSz.cy
           sz.cx = SELF.NaturalSz.cx
           sz.cy = SELF.NaturalSz.cy
        END

        IF sz.cx > WW
           sz.cx = WW
           sz.cy = WW * SELF.sz.cy / SELF.sz.cx
        END
        IF sz.cy > WH
           sz.cy = WH
           sz.cx = WH * SELF.sz.cx / SELF.sz.cy
        END

        SELF.W = sz.cx
        SELF.H = sz.cy
        SELF.X = SELF.Org.x - sz.cx / 2
        SELF.Y = SELF.Org.y - sz.cy / 2

        IF SELF.X < 0
           SELF.X = 0
        END
        IF SELF.Y < 0
           SELF.Y = 0
        END

        SETPOSITION (?Img, SELF.X, SELF.Y, SELF.W, SELF.H)            
                                      !ODS('Gesture Zoom - NotEnd')
      END

!EndRegion ZoomResponder Methods
! =============================================================================
!
! Tracing
!    
! =============================================================================
!Region Debugging


! On PEN input (testing on a Surface2Pro)
! Param indicates which button is being used
! Param=0 - Tip    (pressed or not)
! Param=1 - Magnet (pressed or not) 
! Param=2 - Eraser Not Pressed
! Param=3
! Param=4
! Param=5
! Param=6 - Eraser Pressed

!When in contact, then Buttons=16, Pressure > 0

! =============================================================================
ODS  PROCEDURE(STRING Msg)  !Clarionized OutputDebugString, the ` is to aid Filtering in DbgView
szMsg CSTRING(SIZE(Msg) + 4)  ! 4 = 1 (for '`') + 2 (for '<13,10>) + 1 (for <0> terminator)
  CODE  
  szMsg = '`' & MSG & '<13,10>' !and an implied <0>
  OutputDebugString(szMsg)

!EndRegion Debugging

