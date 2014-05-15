!-Rely On Once due to IDE Navigation Bugs-!       OMIT('*** ifdef ctVLB ***',ifDef_ctVLB)
!-Rely On Once due to IDE Navigation Bugs-!ifDef_ctVLB  EQUATE(1)

  !-----------------------------------------------
  !Created : Sept 29, 2009 by Mark Goldberg
  !Based On: LRM Documentation for VLBProc
  !-----------------------------------------------

ctVLB    CLASS,TYPE,MODULE('ctVLB.clw'),LINK('ctVLB.clw')  !,_VlbLinkMode_),DLL(_VlbDLLMode_) !add PRAGMAs if working with EXE/DLL
Q              &QUEUE,PROTECTED
ochanges       LONG,PROTECTED

Init           PROCEDURE(WINDOW xWin, SIGNED xFEQ, QUEUE xaQ)
VLBproc        PROCEDURE(LONG xRow, SHORT xCol),STRING,PRIVATE   !Required first parameter is implicit in a CLASS method

HandleData     PROCEDURE(LONG xRow, SHORT xCol),STRING,VIRTUAL
CalcChanges    PROCEDURE(),LONG,VIRTUAL
GetRowCount    PROCEDURE(),LONG,VIRTUAL
GetColCount    PROCEDURE(),LONG,VIRTUAL
IsQChanged     PROCEDURE(),LONG,VIRTUAL
GetRow         PROCEDURE(LONG xRow),VIRTUAL
HandleCol      PROCEDURE(SHORT xCol),STRING,VIRTUAL

           END

!end       OMIT('*** ifdef ctVLB ***',ifDef_ctVLB)


