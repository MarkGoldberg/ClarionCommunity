  MEMBER
  MAP
  END
  INCLUDE('ctTechData.inc'),ONCE

eqDBG EQUATE('<4,2,7>') !Special meaning to AssertHook2 in Debuger class


!Region ctTechData
ctTechData.DESTRUCT       PROCEDURE() 
    CODE
    ASSERT(0,eqDBG&'ctTechData.DESTRUCT Class['& SELF.ClassName &']')
 
    
!================================================================
ctTechData.Init           PROCEDURE(STRING xClassName, UNSIGNED xShowFEQ)
    CODE
    SELF.ClassName = xClassName
    SELF.ShowFEQ   = xShowFEQ

!================================================================
ctTechData.Log            PROCEDURE(LONG xID, ? xInfo)
    CODE
    SELF.AddReplace(xID, xInfo) 
    SELF.DebugOne()  
    SELF.ShowData(0,-9) !Show Last 10 Records, in Descending order

!================================================================
ctTechData.DebugOne       PROCEDURE()
    CODE
    ASSERT(0,eqDBG& SELF.ClassName &'['& SELF.Q.ID &'] - ' & SELF.Q.Info)

!================================================================
ctTechData.ShowData     PROCEDURE(LONG xFrom, LONG xTo)
  CODE
                                !ASSERT(0,eqDBG&'v ctTechData.ShowData Class['& SELF.ClassName &']')
?                       ASSERT( SELF.ShowFEQ <> 0                     ,'ctTechData.Show ShowFEQ cannot be 0 here')
?                       ASSERT( SELF.ShowFEQ{PROP:Type} = CREATE:Text ,'ctTechData.Show ShowFEQ should be a TEXT control')

  DO ShowData:CorrectRanges
  DO ShowData:SaveShowingInfo  !May RETURN from this Method, if it decides there's nothing to do.
  DO ShowData:ShowNow
                                !ASSERT(0,eqDBG&'^ ctTechData.ShowData Class['& SELF.ClassName &']')

ShowData:CorrectRanges  ROUTINE
  DATA
RecordsInQ  LONG,AUTO
  CODE
  RecordsInQ  = SELF.RECORDS()
                                  !ASSERT(0,eqDBG&'v ShowData:CorrectRanges From['& xFrom &'], To['& xTo &'] Records['& SELF.RECORDS() &']')
  IF xFrom <= 0 
     xFrom = RecordsInQ + xFrom  
     IF xFrom <= 0
        xFrom  = 1
     END
  END
  IF xFrom > RecordsInQ
     xFrom = RecordsInQ
  END

  IF xTo   <= 0
     xTo   = RecordsInQ + xTo
     IF xTo <= 0
        xTo  = 1
     END
  END
  IF xTo   > RecordsInQ
     xTo   = RecordsInQ
  END
                                  !ASSERT(0,eqDBG&'^ ShowData:CorrectRanges From['& xFrom &'], To['& xTo &'] Records['& SELF.RECORDS() &']')

ShowData:SaveShowingInfo  ROUTINE
  DATA
CurrChanges LONG,AUTO
  CODE
  CurrChanges = CHANGES(SELF.Q)
  IF   SELF.Showing.FromRow  <> xFrom       |
    OR SELF.Showing.ToRow    <> xTo         |
    OR SELF.Showing.QChanges <> CurrChanges |
  THEN
      SELF.Showing.FromRow  = xFrom
      SELF.Showing.ToRow    = xTo
      SELF.Showing.QChanges = CurrChanges
  ELSE
     RETURN !Already showing
  END

ShowData:ShowNow  ROUTINE
  DATA
ChangeBy    LONG,AUTO
QRec        LONG,AUTO
  CODE
  ChangeBy = CHOOSE( xFrom < xTo, 1, -1)
  ASSERT(0,eqDBG&'ShowData:ShowNow  From['& xFrom &'], To['& xTo &'] By['& ChangeBy &'] Records['& SELF.RECORDS() &']')
  SELF.ShowFEQ{PROP:Text} = ''
  LOOP QRec = xFrom TO xTo BY ChangeBy
    SELF.GetByPtr(QRec)
    SELF.ShowFEQ{PROP:Text} = SELF.ShowFEQ{PROP:Text} & SELF.Q.Info & '<13,10>'
  END
  ASSERT(0,eqDBG&'^ ctTechData.ShowData Class['& SELF.ClassName &']')


!================================================================
ctTechData.HideData       PROCEDURE
  CODE
  SELF.ShowFEQ{PROP:Text} = ''
!EndRegion ctTechData

