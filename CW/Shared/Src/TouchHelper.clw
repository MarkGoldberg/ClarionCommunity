  MEMBER()

  INCLUDE('WinEqu.clw'),ONCE
  MAP
    MODULE('Undocumented_RTL')
       FEQ_ToString(SIGNED FEQ),*CSTRING,RAW,NAME('Cla$FIELDNAME')       
    END      
    MODULE('InWin32LIB')
      ExpandEnvironmentStrings(CONST *CSTRING xIn, <*CSTRING xOut>, DWORD nOutSize),DWORD,PROC,PASCAL,RAW,NAME('ExpandEnvironmentStringsA')
    END
 END

  INCLUDE('TouchHelper.inc'),ONCE
  !See TouchDef.INC for EQUATES


eqDBG EQUATE('<4,2,7>')

TouchHelper.Construct               PROCEDURE()     
!Consider reading HKLM\Software\[Wow6432Node\]\Microsoft\CurrentVerion\App Paths\TabTip.exe
!My machine shows a (Default) node of type expand_sz with value 	%CommonProgramFiles%\microsoft shared\ink\TabTip.exe
sz     LIKE(TouchHelper.TouchKeyboard)
newLen DWORD
  CODE                   
  sz = GETREG( REG_LOCAL_MACHINE, 'Software\Microsoft\Windows\CurrentVersion\App Paths\TabTip.exe') !<valuename>,<*? valuetype>)
  IF LEN(sz) = 0       
     SELF.TouchKeyboard = ''
  ELSE
     newLen = ExpandEnvironmentStrings(sz, SELF.TouchKeyboard, SIZE(SELF.TouchKeyboard))
  END

  !ASSERT(0,eqDBG&'sz['& sz &']')
  !ASSERT(0,eqDBG&'SELF.TouchKeyboard['& SELF.TouchKeyboard &'] newLen['& newLen &']')


TouchHelper.ShowTouchKeyboard          PROCEDURE() 
  !UNTESTED, from http://stackoverflow.com/questions/15646684/windows-8-desktop-app-open-tabtip-exe-to-secondary-keyboard-for-numeric-textbo
  !in "HKEY_CURRENT_USER\Software\Microsoft\TabletTip\1.7" (windows 8) 
  !     REG_DWORD KeyboardLayoutPreference       (value of [0=regular layout, 1=split keyboard with the numberpad in the middle)
  !     REG_DWORD LastUsedModalityWasHandwriting also has to be 0 or if 1, when tabtip is started again it will open with the pen handwriting area
  CODE          
  IF LEN(SELF.TouchKeyboard) !Pre CLIP()'d CString
     RUN(SELF.TouchKeyboard)
  END



TouchHelper.PointerTypes_ToString   PROCEDURE(SIGNED xPointerType)!,STRING
Answer STRING(7) !<-- length of longest answer, will return fixed width
   CODE
   CASE xPointerType
     OF PT_POINTER ; Answer = 'Generic'
     OF PT_TOUCH   ; Answer = 'Touch'
     OF PT_PEN     ; Answer = 'Pen'
     OF PT_MOUSE   ; Answer = 'Mouse'
   ELSE            ; Answer = '?['& xPointerType &']'
   END
   RETURN Answer

TouchHelper.TouchAction_ToString   PROCEDURE(SIGNED xTouchAction)!,STRING !Guessing at DataType
Answer STRING(9) !<-- length of longest answer, will return fixed width
   CODE
   CASE xTouchAction
     OF TOUCH_UNDEFINED ; Answer = 'UNDEFINED'
     OF TOUCH_ENTER     ; Answer = 'ENTER'
     OF TOUCH_LEAVE     ; Answer = 'LEAVE'
     OF TOUCH_ACTIVATE  ; Answer = 'ACTIVATE'
     OF TOUCH_DOWN      ; Answer = 'DOWN'
     OF TOUCH_UP        ; Answer = 'UP'
     OF TOUCH_UPDATE    ; Answer = 'UPDATE'
     OF TOUCH_WHEEL     ; Answer = 'WHEEL'
     ELSE               ; Answer = '?['& xTouchAction &']'
   END
   RETURN Answer


   
TouchHelper.InputAction_ToString PROCEDURE(SIGNED xInputAction)!,STRING
Answer STRING(19) !<-- length of longest answer, will return fixed width
   CODE
   CASE xInputAction
     OF INPUT_UNDEFINED      ; Answer = 'INPUT_UNDEFINED'
     OF INPUT_ENTER          ; Answer = 'INPUT_ENTER'
     OF INPUT_LEAVE          ; Answer = 'INPUT_LEAVE'
     ! v=== Drag-n-drop
     OF INPUT_STARTDRAGGING  ; Answer = 'INPUT_STARTDRAGGING'
     OF INPUT_DRAGGING       ; Answer = 'INPUT_DRAGGING'
     OF INPUT_ENDDRAGGING    ; Answer = 'INPUT_ENDDRAGGING'
     ! v=== Mouse input
     OF MOUSE_DOWN           ; Answer = 'MOUSE_DOWN'
     OF MOUSE_UP             ; Answer = 'MOUSE_UP'
     OF MOUSE_DBLCLICK       ; Answer = 'MOUSE_DBLCLICK'
     OF MOUSE_HOVER          ; Answer = 'MOUSE_HOVER'
     OF MOUSE_WHEEL          ; Answer = 'MOUSE_WHEEL'
     ! v=== Touch gestures
     OF GESTURE_PAN          ; Answer = 'GESTURE_PAN'
     OF GESTURE_TAP          ; Answer = 'GESTURE_TAP'
     OF GESTURE_DBLTAP       ; Answer = 'GESTURE_DBLTAP'
     OF GESTURE_PRESSANDTAP  ; Answer = 'GESTURE_PRESSANDTAP'
     OF GESTURE_ZOOMIN       ; Answer = 'GESTURE_ZOOMIN'
     OF GESTURE_ZOOMOUT      ; Answer = 'GESTURE_ZOOMOUT'
     OF GESTURE_ROTATE       ; Answer = 'GESTURE_ROTATE'
   ELSE                      ; Answer = '?['& xInputAction &']'
   END
   RETURN Answer


TouchHelper.InputFlags_ToString  PROCEDURE(LONG xFlag) !,STRING
  CODE
  RETURN CHOOSE( BAND(xFLAG, INFLAG_LBUTTON    )=0,'', 'LBUTTON   ') & |   'First button'
         CHOOSE( BAND(xFLAG, INFLAG_RBUTTON    )=0,'', 'RBUTTON   ') & |   'Second button'
         CHOOSE( BAND(xFLAG, INFLAG_MBUTTON    )=0,'', 'MBUTTON   ') & |   'Third button'
         CHOOSE( BAND(xFLAG, INFLAG_XBUTTON1   )=0,'', 'XBUTTON1  ') & |   'Fourth button'
         CHOOSE( BAND(xFLAG, INFLAG_XBUTTON2   )=0,'', 'XBUTTON2  ') & |   'Fifth button'
         CHOOSE( BAND(xFLAG, INFLAG_CONTROL    )=0,'', 'CONTROL   ') & |   'Ctrl button is down during event'
         CHOOSE( BAND(xFLAG, INFLAG_SHIFT      )=0,'', 'SHIFT     ') & |   'Shift button is down during event'
         CHOOSE( BAND(xFLAG, INFLAG_BEGIN      )=0,'', 'BEGIN     ') & |   'Gesture begins'
         CHOOSE( BAND(xFLAG, INFLAG_INERTIA    )=0,'', 'INERTIA   ') & |   'Pan gesture has inertia information'
         CHOOSE( BAND(xFLAG, INFLAG_END        )=0,'', 'END       ') & |   'Gesture ends'
         CHOOSE( BAND(xFLAG, INFLAG_CAPTURED   )=0,'', 'CAPTURED  ') & |   'Capture active during event'
         CHOOSE( BAND(xFLAG, INFLAG_VERTICAL   )=0,'', 'VERTICAL  ') & |   'Panning or wheel rotation has vertical component'
         CHOOSE( BAND(xFLAG, INFLAG_HORIZONTAL )=0,'', 'HORIZONTAL') & |   'Panning or wheel rotation has horizontal component'
         CHOOSE( BAND(xFLAG, INFLAG_TWOFINGERS )=0,'', 'TWOFINGERS')      !'Panning is using two fingers'

TouchHelper.PointFlags_ToString  PROCEDURE(LONG xFlags)!,STRING
  CODE
  IF                   xFlags =PTFLAG_NONE THEN RETURN ' NONE'  END
  RETURN CHOOSE( BAND (xFlags, PTFLAG_NEW      )=0,'', ' NEW'      )& |
         CHOOSE( BAND (xFlags, PTFLAG_INRANGE  )=0,'', ' INRANGE'  )& |
         CHOOSE( BAND (xFlags, PTFLAG_INCONTACT)=0,'', ' INCONTACT')& |
         CHOOSE( BAND (xFlags, PTFLAG_PRIMARY  )=0,'', ' PRIMARY'  )& |
         CHOOSE( BAND (xFlags, PTFLAG_DOWN     )=0,'', ' DOWN'     )& |
         CHOOSE( BAND (xFlags, PTFLAG_UPDATE   )=0,'', ' UPDATE'   )& |
         CHOOSE( BAND (xFlags, PTFLAG_UP       )=0,'', ' UP'       )& |
         CHOOSE( BAND (xFlags, PTFLAG_CANCELED )=0,'', ' CANCELED' )& |
         CHOOSE( BAND (xFlags, PTFLAG_WHEEL    )=0,'', ' WHEEL'    )& |
         CHOOSE( BAND (xFlags, PTFLAG_HWHEEL   )=0,'', ' HWHEEL'   )

TouchHelper.TouchPoint_ToString PROCEDURE(CONST *TouchPoint xTouchPoint) !,STRING    
  !TouchResponder gets these
  CODE
  RETURN     'ID=' &                             xTouchPoint.ID           & ';' & |
         ' Point=' &         SELF.Point_ToString(xTouchPoint.pt)          & ';' & |
    ' LocalPoint=' &         SELF.Point_ToString(xTouchPoint.ptLocal)     & ';' & |
        ' PTType=' &  SELF.PointerTypes_ToString(xTouchPoint.PTType)      & ';' & | 
   ' InputAction=' &  SELF .InputAction_ToString(xTouchPoint.ia)          & ';' & |
   ' TouchAction=' &  SELF .TouchAction_ToString(xTouchPoint.ia)          & ';' & | !<-- .ia as a TOUCHaction
        ' target=' &                             xTouchPoint.Target       & ';' & | 
         ' Flags=' &  SELF  .PointFlags_ToString(xTouchPoint.PTFlags)     & ';' & |
            SELF.ShowWhenNotZero('Buttons'     , xTouchPoint.Buttons     ) & | USHORT, must be bitmapped
            SELF.ShowWhenNotZero('Param'       , xTouchPoint.Param       ) & |
            SELF.ShowWhenNotZero('Distance'    , xTouchPoint.Distance    ) & |
            SELF.ShowWhenNotZero('Pressure'    , xTouchPoint.Pressure    ) & |
            SELF.ShowWhenNotZero('TiltX'       , xTouchPoint.TiltX       ) & | Probably should be considered a POINT
            SELF.ShowWhenNotZero('TiltY'       , xTouchPoint.TiltY       ) & |
            SELF.ShowWhenNotZero('Rotation'    , xTouchPoint.Rotation    ) & |
            SELF.ShowWhenNotZero('Downtime'    , xTouchPoint.Downtime    ) & |
            SELF.ShowWhenNotZero('LastMoveTime', xTouchPoint.LastMoveTime) & |
            SELF.ShowWhenNotZero('UpdateTime'  , xTouchPoint.UpdateTime  ) & |
  ''             
    
TouchHelper.InputData_ToString     PROCEDURE(CONST *InputData xInputData)!,STRING
  !GestureResponder gets these
  CODE    
  RETURN 'Ctl='& xInputData.ctl &'='&             FEQ_ToString             (           xInputData.ctl             )        & |
        ' ia=' &                             SELF.InputAction_ToString     (           xInputData.ia              )        & |
      CHOOSE(xInputData.ptAction &= NULL, '',SELF.Point_ToStringWhenNotZero('PtLocal', xInputData.ptAction.ptLocal) )      & |
      CHOOSE(xInputData.p1       &= NULL, '',SELF.Point_ToStringWhenNotZero('p1'     , xInputData.p1      .ptLocal) )      & |
      CHOOSE(xInputData.pt2      &= NULL, '',SELF.Point_ToStringWhenNotZero('pt2'    , xInputData.pt2     .ptLocal) )      & |
                                             SELF.ShowWhenNotZero          ('Buttons', xInputData.Buttons         )        & | USHORT, must be bitmapped
                      ' Param='&             SELF.InputFlags_ToString      (           xInputData.param           )        & |
                                             SELF.ShowWhenNotZero          ('Ratio'  , xInputData.Ratio           )        & | Shares bytes with Angle/Ratio/Delta/Distance 
                                             SELF.Point_ToStringWhenNotZero('Scroll' , xInputData.Scroll          )        & |  
                                             SELF.Point_ToStringWhenNotZero('Speed'  , xInputData.Speed           )        & |
   ''

TouchHelper.ShowWhenNotZero            PROCEDURE(STRING xName, ? xValue)!,STRING
  CODE
  IF xValue 
     RETURN ' '& xName &'='& xValue &';'
  END
  RETURN ''
                          
TouchHelper.Point_ToStringWhenNotZero  PROCEDURE(STRING xName, CONST *TPOINT    xPt)!,STRING                         
  CODE 
  RETURN CHOOSE( xPt.X OR xPt.Y, ' ' & xName & SELF.Point_ToString(xPt), '')
  

TouchHelper.Point_ToString         PROCEDURE(CONST *TPOINT    xPt)!,STRING
  CODE
  RETURN '('& FORMAT(xPt.X,@N-5) &','& FORMAT(xPt.Y,@N-5) &')'



