     omit('*** ifdef ResizeClassType ***',ifDef_ResizeClassType)
ifDef_ResizeClassType Equate(1)


Eq:ULX_Left    Equate(80H)  !Upper Left  X Left
Eq:ULX_Percent Equate(40H)  !               %
Eq:ULY_Top     Equate(20H)  !Upper Left  Y Top
!Eq:ULY_Percent Equate(10H)  !              %

Eq:LRX_Left    Equate(08H)  !Lower Right X Left
Eq:LRX_Percent Equate(04H)  !              %
Eq:LRY_Top     Equate(02H)  !Lower Right Y Top
!Eq:LRY_Percent Equate(01H)  !              %



ResizeQ_Type    Queue,TYPE
FEQ              short    !Field Equate
OrigX            long !ushort
OrigY            long !ushort
OrigW            long !ushort
OrigH            long !ushort
!!!!!!todo KeepSameWH        byte    !True -> ignore low order nibble of ResizeRules (Default True)
ResizeRules      byte     !Bit Map:
                          !      7: Upper Left : X  1=>Left 0=>Right
                          !      6:                 1=>Pcnt 0=>Abs
                          !      5: Upper Left : Y  1=>Top  0=>Bot
                          !      4: Reserved                           1=>Pcnt 0=>Abs
                          !
                          !      3: Lower Right: X  1=>Left 0=>Right
                          !      2:                 1=>Pcnt 0=>Abs
                          !      1: Lower Right: Y  1=>Top  0=>Bot
                          !      0: Reserved                           1=>Pcnt 0=>Abs
               end !end ResizeQ_type


ResizeClassType Class,Type,MODULE('ResizeClass.CLW'),LINK('ResizeClass.CLW',_mgLinkMode_) !,Dll(_mgDllMode_)
ResizeQ           &ResizeQ_Type !
ResizeWindow      &Window  !added 9/15/99
SkipDisplay        byte    !added 9/15/99
DisablePerform     byte    !added 6/20/02
!-----------
OrigWinW           long !ushort  !window Width
OrigWinH           long !ushort  !window Height
LastWinW           long !ushort  !window Width
LastWinH           long !ushort  !window Height

sOrient            string(7) !see doc in original Add_ResizeQ   !added June 15,2005
sPercent           string(3) !see doc in original Add_ResizeQ   !added June 15,2005
!-----------
Construct         Procedure() !added 6/20/02
FreeQueue         Procedure()
Init_Class        Procedure(Window argWindow)
Close_Class       Procedure()
Add_ResizeQ       Procedure(long   ArgFEQ,string ArgOrient,<string ArgPercent>) !original
Add_ResizeQ       Procedure(long   ArgFEQ) !added June 15,2005, will use self.ArgOrient, self.ArgPercent
Set_Orient        Procedure(string ArgOrient)  !added June 15,2005
Set_Percent       Procedure(string ArgPercent) !added June 15,2005
Perform_Resize    Procedure(),byte,proc  !Returns False if resize was bypassed
ResetOrigPosition Procedure(<long argFEQ>) !to get rid of splitter construct of Closing and Re-initializing
                end !end class

   !end-omit('*** ifdef ResizeClassType ***',ifDef_ResizeClassType)

!Last Revised:  June 15,2005 -- Aded sOrient,sPercent with the goal of Add_ResizeQ(long argFEQ)

! vim: set filetype=clarion:
