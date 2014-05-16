!ABCIncludeFile
       
 INCLUDE(   'ABWindow.inc'),ONCE
 INCLUDE('TouchHelper.inc'),ONCE
   

WindowManagerTouch CLASS(WindowManager),TYPE,MODULE('WindowManagerTouch.CLW'),LINK('WindowManagerTouch.clw',_ABCLinkMode_),DLL(_ABCDllMode_)
IsAutoShowingTouchKeyboard  BOOL                          
Open                        PROCEDURE(*Window pWindow,<*Window pOwner>),DERIVED
OnSelected                  PROCEDURE(),LONG               
                   END
