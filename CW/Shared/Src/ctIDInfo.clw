  MEMBER()
  MAP
  END
  INCLUDE('ctIDInfo.inc'),ONCE
  INCLUDE('Errors.clw'),ONCE

eqDBG EQUATE('<4,2,7>') !Special meaning to AssertHook2 in Debuger class

ctIDInfo.Construct PROCEDURE()
    CODE
    SELF.Q &= NEW qtIDInfo

ctIDInfo.Destruct  PROCEDURE()
    CODE
                            !ASSERT(0,eqDBG&'v ctIDInfo.Destruct')
    IF NOT (SELF.Q &= NULL)
        SELF.Free()
        DISPOSE(SELF.Q)
    END
                            !ASSERT(0,eqDBG&'^ ctIDInfo.Destruct')

ctIDInfo.Del             PROCEDURE
   CODE
                            !ASSERT(0,eqDBG&'v ctIDInfo.Del')
?  ASSERT(~ SELF.Q &= NULL, 'Queue cannot be null here[ctIDInfo.Del]')   
                            !ASSERT(0,eqDBG&'  ctIDInfo.Del ADDRESS(SELF.Q)['& ADDRESS(SELF.Q)      &']')
                            !ASSERT(0,eqDBG&'  ctIDInfo.Del ADDRESS(Info)  ['& ADDRESS(SELF.Q.Info) &']')
   IF ~(SELF.Q.Info &= NULL)
                            !    ;ASSERT(0,eqDBG&'  ctIDInfo.Del setting Info to NULL') ! was GPFing right after this line
        SELF.Q.Info &= NULL !    ;ASSERT(0,eqDBG&'  ctIDInfo.Del') 
   ELSE                     !    ;ASSERT(0,eqDBG&'  ctIDInfo.Del Was Null') 
   END
   DELETE(SELF.Q)           !;ASSERT(0,eqDBG&'^ ctIDInfo.Del')

ctIDInfo.Free            PROCEDURE              
   CODE
                            !ASSERT(0,eqDBG&'v ctIDInfo.Free')
?  ASSERT(~ SELF.Q &= NULL, 'Queue cannot be null here[ctIDInfo.Free]')   
   LOOP WHILE SELF.GetByPtr(1) = NoError         
        SELF.Del()
        ASSERT(0,eqDBG&'ctIDInfo.Free RECORDS['& SELF.RECORDS() &']')
   END
                            !ASSERT(0,eqDBG&'^ ctIDInfo.Free')

ctIDInfo.FindByID            PROCEDURE(SIGNED xID)!,LONG !Returns ErrorCode()
    CODE
    SELF.Q.ID = xID
    GET(SELF.Q, SELF.Q.ID)
    RETURN ErrorCode()

ctIDInfo.GetByPtr        PROCEDURE(LONG xPtr)!,LONG,PROC !Returns ErrorCode()
   CODE
?  ASSERT(~ SELF.Q &= NULL, 'Queue cannot be null here[ctIDInfo.GetByPtr]')   
   GET(SELF.Q, xPtr)
   RETURN ErrorCode()



ctIDInfo.AddReplace     PROCEDURE(SIGNED xID, ? xInfo)
   CODE
   IF SELF.FindByID(xID) = NoError  !IF Found THEN DEL END
      SELF.Q.Info  = xInfo
      PUT(SELF.Q)
   ELSE
      SELF.Add(xID, xInfo)
   END       

ctIDInfo.Add            PROCEDURE(SIGNED xID, ? xInfo)
   CODE
?  ASSERT(~ SELF.Q &= NULL, 'Queue cannot be null here[ctIDInfo.Add]')   
   CLEAR(SELF.Q) !<-- important as .Info is an ANY  
!  SELF.Q.Info &= NULL !<-- is this needed ?

   SELF.Q.ID    = xID
   SELF.Q.Info  = xInfo
   ADD(SELF.Q)

    
ctIDInfo.Records        PROCEDURE()!,LONG
    CODE
?  ASSERT(~ SELF.Q &= NULL, 'Queue cannot be null here[ctIDInfo.Records]')   
    RETURN RECORDS(SELF.Q)
