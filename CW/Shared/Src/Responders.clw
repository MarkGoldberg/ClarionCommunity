 MEMBER()
 MAP
 END     
 INCLUDE('Responders.inc'),ONCE


!Region BaseResponderType
!===============================================================================
ResponderBaseType.Init                        PROCEDURE(LONG xTouchInterfaceAddress)
  CODE
  SELF.Notifier &= 0 + xTouchInterfaceAddress

ResponderBaseType.EnableEvents                PROCEDURE()!,VIRTUAL
  CODE
  MESSAGE('Programmer Error, ResponderBaseType.EnableEvents is Meant to be Dervied')

ResponderBaseType.DisableEvents               PROCEDURE()!,VIRTUAL
  CODE
  MESSAGE('Programmer Error, ResponderBaseType.DisableEvents is Meant to be Dervied')

!EndRegion BaseResponderType



!Region ResponderGestureType
!===============================================================================
ResponderGestureType.EnableEvents             PROCEDURE()!,DERIVED
  CODE
  SELF.Notifier.InputResponder ( SELF.IGestureResponder )           

ResponderGestureType.DisableEvents           PROCEDURE()!,DERIVED
  CODE
  SELF.Notifier.InputResponder()

ResponderGestureType.IGestureResponder.Event  PROCEDURE (*InputData xInputData)!,BYTE
gesture    &InputData,AUTO
RetHandled BYTE(ProcessEvent:Continue)
  CODE                                                          
  gesture &= xInputData
  LOOP UNTIL gesture &= NULL
    RetHandled = SELF.OnEvent(gesture)       !<-- Does return a value
    IF RetHandled = ProcessEvent:Continue
       BREAK
    END
    gesture &= gesture.Next()  
                                !ASSERT(0,eqDBG&'  ResponderGestureType.IGestureResponder.Event Next['& CHOOSE( gesture &= NULL,'IsNull', 'Address('& ADDRESS(gesture) &')' ) &']')
  END
                                        
  RETURN RetHandled
 !RETURNING ProcessEvent:Stop will prevent further interpretation of this event
 ! for example: ProcessEvent:Stop will prevent a click on a control from posting an EVENT:Accepted

    
ResponderGestureType.OnEvent                  PROCEDURE(*InputData xInputData)!,BYTE,PROC,VIRTUAL
  CODE
  RETURN ProcessEvent:Continue

!EndRegion ResponderGestureType 
          


!Region ResponderTouchType
!===============================================================================
ResponderTouchType.EnableEvents               PROCEDURE()!,DERIVED
  CODE
  SELF.Notifier.TouchResponder( SELF.IPointerResponder)

ResponderTouchType.DisableEvents             PROCEDURE()!,DERIVED
  CODE
  SELF.Notifier.TouchResponder()

ResponderTouchType.IPointerResponder.Event    PROCEDURE (*TouchData xTouchData)!,BYTE
td          &TouchData,AUTO
RetHandled  BYTE(ProcessEvent:Continue)
  CODE
  td &= xTouchData

  IF td.ctl <> 0
     RETURN FALSE    ! Touch outside WINDOW's client area !<-- WHY FILTER THIS ? (and why not in the loop ?)
  END

  LOOP UNTIL td &= NULL
    RetHandled = SELF.OnEvent( td )  !<-- Does return a value
    IF RetHandled = ProcessEvent:Continue
       BREAK
    END
    td &= td.Next()
                       !ASSERT(0,eqDBG&'ResponderTouchType.IPointerResponder.Event Next['& CHOOSE( td &= NULL,'IsNull', 'Address('& ADDRESS(td) &')' ) &']')
  END

  RETURN RetHandled
 !RETURNING ProcessEvent:Stop will prevent further interpretation of this event

                  
ResponderTouchType.OnEvent                    PROCEDURE(*TouchData xTouchData)!,BYTE,PROC,VIRTUAL
  CODE      
  RETURN ProcessEvent:Continue

!EndRegion ResponderTouchType

