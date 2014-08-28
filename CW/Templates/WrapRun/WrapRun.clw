  PROGRAM
!#----------------
! This DLL was created in order to support RUN()'ing a command from within a template  
! Work is similar to ODS.DLL shown in http://archive.clarionmag.com/cmag/v8/v8n09templatedebugger.html
!#----------------
!#GROUP(%WrapRun,%Command)
!#RUNDLL('WRAPRUN.DLL','WRAPRUN',%Command),WIN32
!#----------------
  MAP
  	WrapRun(*CSTRING Command)
  END
  CODE
  
WrapRun PROCEDURE(*CSTRING Command)	  
	CODE
	RUN(Command)
	