  MEMBER
  MAP
  END
  INCLUDE('ctVLB.inc'),ONCE


ctVLB.CalcChanges PROCEDURE()!,VIRTUAL,LONG
  CODE
  RETURN CHANGES(SELF.Q)

ctVLB.GetRowCount   PROCEDURE()!,LONG,VIRTUAL
  CODE
  RETURN RECORDS(SELF.Q)

ctVLB.GetColCount   PROCEDURE()!,LONG,VIRTUAL
RetVal   LONG(1) !no Auto
CurrCol  ANY
  CODE
  !Assume at least ONE column
  LOOP
     CurrCol &= WHAT(SELF.Q, RetVal) 
     IF CurrCol &= NULL
        BREAK
     END
     RetVal += 1
  END
  RETURN RetVal


ctVLB.IsQChanged     PROCEDURE()!,LONG,VIRTUAL
tmpChanges LIKE(ctVLB.oChanges)
  CODE
  tmpChanges   =   SELF.CalcCHANGES()
  IF SELF.oChanges <> tmpChanges
     SELF.oChanges =  tmpChanges
     RETURN TRUE
  END
  RETURN FALSE



ctVLB.Init  PROCEDURE(WINDOW xWin, SIGNED xFEQ, QUEUE xaQ)
  !Consider saving the WINDOW and FEQ
  CODE
  SELF.Q &= xaQ
  SELF.ochanges = SELF.CalcCHANGES()  !does this work correctly with a generic Q ?

  xWin $ xFEQ{PROP:VLBval}  = ADDRESS(SELF)           !Must assign this first
  xWin $ xFEQ{PROP:VLBproc} = ADDRESS(SELF.VLBproc)    ! then this





ctVLB.VLBproc PROCEDURE(LONG xRow, SHORT xCol)  !Required first parameter is implied

ROW:GetRowCount  EQUATE(-1)
ROW:GetColCount  EQUATE(-2)
ROW:IsQChanged   EQUATE(-3)

  CODE
  CASE xRow
     OF ROW:GetRowCount; RETURN SELF.GetRowCount()
     OF ROW:GetColCount; RETURN SELF.GetColCount()
     OF ROW:IsQChanged ; RETURN SELF.IsQChanged()
  END
  RETURN SELF.HandleData(xRow, xCol)


ctVLB.HandleData PROCEDURE(LONG xRow, SHORT xCol)
  CODE
  SELF.GetRow(xRow)
  RETURN SELF.HandleCol(xCol)


ctVLB.GetRow PROCEDURE(LONG xRow)
  CODE
  GET(SELF.Q, xRow)


ctVLB.HandleCol PROCEDURE(SHORT xCol)
  CODE
  RETURN WHAT(SELF.Q, xCol)

