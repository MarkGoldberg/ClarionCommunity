  MEMBER()
  MAP
  END
  INCLUDE('WindowManagerTouch.inc'),ONCE     

        
MOD:TouchHelper    TouchHelper

WindowManagerTouch.Open                        PROCEDURE(*Window pWindow,<*Window pOwner>)
  CODE
  PARENT.Open(pWindow,pOwner)
  REGISTER(EVENT:Selected, ADDRESS(SELF.OnSelected), ADDRESS(SELF))
  SELF.IsAutoShowingTouchKeyboard = TRUE

WindowManagerTouch.OnSelected                  PROCEDURE()                                
ShowKeyboard BOOL(FALSE)
  CODE
  IF SELF.IsAutoShowingTouchKeyboard 
     CASE Field(){PROP:Type}
       OF CREATE:combo
     OROF CREATE:dropcombo
     OROF CREATE:entry 
     OROF CREATE:spin 
     OROF CREATE:rtf
     OROF CREATE:text    
                           ShowKeyboard = TRUE
     ELSE
     END
         
     IF ShowKeyboard                                     
        MOD:TouchHelper.ShowTouchKeyboard() 
     END
  END              
  RETURN LEVEL:Benign
               



