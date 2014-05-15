       INCLUDE('CWTouch.inc'),ONCE
               
ProcessEvent:Stop     EQUATE(TRUE)
ProcessEvent:Continue EQUATE(FALSE)

ResponderBaseType    CLASS,TYPE,MODULE('Responders.clw'),LINK('Responders.clw')
Notifier                 &ITouchNotifier
Init                     PROCEDURE(LONG xTouchInterfaceAddress)   !I should probably pass in a Window or an FEQ -- todo, understand what has a PROP:TouchInterface
EnableEvents             PROCEDURE(),VIRTUAL
DisableEvents            PROCEDURE(),VIRTUAL
                      END

ResponderGestureType  CLASS(ResponderBaseType),TYPE,IMPLEMENTS(IGestureResponder),MODULE('Responders.clw'),LINK('Responders.clw')                                                             
EnableEvents             PROCEDURE(),DERIVED
DisableEvents            PROCEDURE(),DERIVED
OnEvent                  PROCEDURE(*InputData xInputData),BYTE,PROC,VIRTUAL
                      END

ResponderTouchType    CLASS(ResponderBaseType),TYPE,IMPLEMENTS(IPointerResponder),MODULE('Responders.clw'),LINK('Responders.clw')
EnableEvents             PROCEDURE(),DERIVED
DisableEvents            PROCEDURE(),DERIVED
OnEvent                  PROCEDURE(*TouchData xTouchData),BYTE,PROC,VIRTUAL
                      END   
                  

                                        

