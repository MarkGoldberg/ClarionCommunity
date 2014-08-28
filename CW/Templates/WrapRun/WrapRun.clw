  PROGRAM
!#----------------
! This DLL was created in order to support RUN()'ing a command from within a template  
! Work is similar to ODS.DLL shown in http://archive.clarionmag.com/cmag/v8/v8n09templatedebugger.html
!#----------------
!#GROUP(%RunReturnRightAway,%Command)
!#RUNDLL('WRAPRUN.DLL','RUN_RETURNRIGHTAWAY',%Command),WIN32
!#----------------
!#GROUP(%RunWaitUntilCompleted,%Command)
!#RUNDLL('WRAPRUN.DLL','RUN_WAITUNTILCOMPLETED',%Command),WIN32
!#----------------

RUN:Default            EQUATE(0)
RUN:ReturmImm          EQUATE(0)
RUN:WaitUntilCompleted EQUATE(1)

  MAP
  	Run_ReturnRightAway   (*CSTRING Command)
  	Run_WaitUntilCompleted(*CSTRING Command)
  END
  CODE

Run_ReturnRightAway   PROCEDURE(*CSTRING xCommand)
	CODE
	RUN(xCommand, RUN:ReturmImm)
  
Run_WaitUntilCompleted PROCEDURE(*CSTRING xCommand)	  
	CODE
	RUN(xCommand, RUN:WaitUntilCompleted)
	