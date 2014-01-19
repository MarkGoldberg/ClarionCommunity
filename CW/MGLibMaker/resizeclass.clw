 Member
 Map
  Class_Min (any argA,any argB),any
  Class_Max (any argA,any argB),any
 end
 Include('ResizeClass.inc'),once

!!!   !NO_DEBUG  Equate(1) !TRUE)
!!!   !       omit('_NODEBUG_',NO_DEBUG)
!!!   _MGDEBUG_ Equate(1)
!!!           OMIT('_****_',_MGDEBUG_)  !<-- conditional omit not working???
!!!    bug
!!!    include('debuger.inc'),once
!!!   dbg  DebugerAutoInit
!!!                _****_

Class_Min Procedure(any argA,any argB)
  code
  if argA < argB then return argA else return argB end

Class_Max Procedure(any argA,any argB)
  code
  if argA > argB then return argA else return argB end


       Omit('******* Documentation *******')
!  =========================================================================
!  Created: By Mark Goldberg, of Monolith Custom Computing, Inc.
!  Updates: 3/27/98 Placed in ResizeClass.CLW
!           5/10/99 Added ResetOrigPosition   Procedure()
!           5/21/99 Revised ResetOrigPosition, to set Self.LastWin[W,H] and OrigWin[W,H]
!                   Revised Perform_Resize, to System{PROP:DeferMove} = 0
!           9/15/99 Added Self.ResizeWindow
!                     changed init, added field, now using SetTarget ...
!           6/20/02 Added DisablePerform and Construct
!           4/22/04 Added an optional argument to RestOrigPosition(<FEQ>)
!
!  Todo   : Rename Init_Class as a constructor  "Construct"
!              NO -- need to open(window) prior to the Init_Class
!           Rename Close_Class as a destructor  "Destruct"
!              NO -- as I want to add window positional saving
!           Add Code to save & restore window positioning
!           Add a method to reset Orig sizes (useful for splitters)
!
!  Class
!  =========================================================================
  !end-Omit('******* Documentation *******')

!=========================================================================
ResizeClassType.Construct           Procedure
  code
  Self.DisablePerform = TRUE
  Self.sPercent = ''
  Self.sOrient  = ''
          !PrintDebugString('ResizeClassType.Construct<13,10>')

!=========================================================================
ResizeClassType.FreeQueue           Procedure
  code
  free(self.ResizeQ)
!=========================================================================
ResizeClassType.Init_Class          Procedure(Window argWindow)
  code
  Self.ResizeWindow &= argWindow
!  Message('1: In ResizeClassType.Init_Class','debug')
  Self.ResizeQ &= New(ResizeQ_Type)
!  Message('1: In ResizeClassType.Init_Class','debug')
  Self.FreeQueue
  SetTarget(Self.ResizeWindow)
  self.LastWinW = 0{prop:Width }
  self.LastWinH = 0{prop:Height}
  self.OrigWinW = self.LastWinW
  self.OrigWinH = self.LastWinH
                                     !PrintDebugString('ResizeClassType.Init_Class, OrigW ['& Self.OrigWinW &'] OrigH ['& Self.OrigWinH &']<13,10>')
  SetTarget
  Self.SkipDisplay = FALSE
  Self.DisablePerform = FALSE
!=========================================================================
ResizeClassType.Close_Class         Procedure
  code
          !PrintDebugString('ResizeClassType.Close_Class<13,10>')
  if not (Self.ResizeQ &= NULL)
    Self.FreeQueue
    Dispose(Self.ResizeQ)
  !  !PrintDebugString('ResizeClassType.Close_Class, post dispose, ')
  !  if Self.ResizeQ &= NULL
  !       !PrintDebugString(' NOW NULL<13,10>')         !<--- this one occurred
  !  else !PrintDebugString(' STILL NON NULL<13,10>')
  !  end
  end
  Self.DisablePerform = TRUE
!=========================================================================
ResizeClassType.Perform_Resize      Procedure()

      omit('*** documenation ***')
! =========================================================================
! Returns    : True if a resize occured, else false.
! ArgResizeQ.ResizeRules is a bitmap (see notes where queue,type was declared)
!
! Notes: 9/15/99 StatusBar Bug, the display here can be an issue if Perform_resize is called prior to the accept loop
!                todo : establish the need for a perform_resize prior to the accept loop, or discard such calls
!                Added Self.SkipDisplay
!
!                Assemblies is resizing somewhat inconsistently on a maximize/restore
!                  much of the time it is just fine, other times the bottom half of the screen goes all to hell
!                  when this happens I`ve also noticed the [c] (formerly [i]) screen getting confused as well
!                  (noticied 9/15/99 c5eeb)
!
!       2/02/00  Added "or 0{prop:Iconize}" along with self.LastWin* checks to bypass any changes
!       4/28/04  Added debugging, moved SetTarget a litte further downstream, as experiencing thread switching
! =========================================================================
 !end-omit('*** documenation ***')


lcl:rec      ushort,auto
lcl:x         short,auto
lcl:y         short,auto
lcl:w         short,auto !May hold negative numbers temporarily
lcl:h         short,auto !May hold negative numbers temporarily
lcl:NewWinW  ushort,auto !0{prop:width }
lcl:NewWinH  ushort,auto !0{prop:height}
lcl:DiffWinW  short,auto !0{prop:width } - Self.OrigW    !May be negative
lcl:DiffWinH  short,auto !0{prop:height} - Self.OrigH    !May be negative
lcl:PcntWinW  real ,auto
lcl:PcntWinH  real ,auto


 code
!!       omit('_NODEBUG_',NO_DEBUG)
!!            Assert(0,eqDBG&'ResizeClassType.Perform_Resize thread['& thread() &'] Self.DisablePerform['& Self.DisablePerform &'] Self.SkipDisplay['& Self.SkipDisplay &']')  !cond-omit
!!             _NODEBUG_

 if Self.DisablePerform then return(FALSE) end
 if ~ Self.SkipDisplay  then display       end
! SetTarget(Self.ResizeWindow) !consider checking for  Self.ResizeWindow{prop:Hide}=TRUE
 if Self.ResizeWindow{prop:width} = self.LastWinW and Self.ResizeWindow{prop:height} = self.LastWinH or Self.ResizeWindow{Prop:Iconize}
!    SetTarget
    return(FALSE) !<====
 end

 SetTarget(Self.ResizeWindow) !consider checking for  Self.ResizeWindow{prop:Hide}=TRUE

 lcl:NewWinW  = Self.ResizeWindow{prop:width }
 lcl:NewWinH  = Self.ResizeWindow{prop:height}
 !PrintDebugString('ResizeClassType.Perform_Resize, #10<13,10>')
 !PrintDebugString('ResizeClassType.Perform_Resize, NewW ['& lcl:NewWinW &'] NewH ['& lcl:NewWinH &']<13,10>')


 lcl:DiffWinW = lcl:NewWinW - Self.OrigWinW;  lcl:PcntWinW = lcl:NewWinW / Self.OrigWinW
 lcl:DiffWinH = lcl:NewWinH - Self.OrigWinH;  lcl:PcntWinH = lcl:NewWinH / Self.OrigWinH

 System{PROP:DeferMove} = Records(Self.ResizeQ)

 loop lcl:rec = 1 to records(Self.ResizeQ)
     get(Self.ResizeQ,lcl:Rec)

     lcl:x = Self.ResizeQ.OrigX
     lcl:y = Self.ResizeQ.OrigY
     lcl:w = Self.ResizeQ.OrigW + Self.ResizeQ.OrigX
     lcl:h = Self.ResizeQ.OrigH + Self.ResizeQ.OrigY

     !-------------------------------------------------------- Upper Left X
     if   ~band(Self.ResizeQ.ResizeRules,Eq:ULX_Left   ) then lcl:x += lcl:DiffWinW   !ULX = Right
     elsif band(Self.ResizeQ.ResizeRules,Eq:ULX_Percent) then lcl:x *= lcl:PcntWinW   !ULX = %
     end
     !-------------------------------------------------------- Upper Left Y
     if   ~band(Self.ResizeQ.ResizeRules,Eq:ULY_Top    ) then lcl:y += lcl:DiffWinH   !ULY = Bottom
   ! elsif band(Self.ResizeQ.ResizeRules,Eq:ULX_Percent) then lcl:y *= lcl:PcntWinH   !ULY = %
     end                                       !^!
     !-------------------------------------------------------- Lower Right X
     if   ~band(Self.ResizeQ.ResizeRules,Eq:LRX_Left   ) then lcl:w += lcl:DiffWinW   !LRX = R
     elsif band(Self.ResizeQ.ResizeRules,Eq:LRX_Percent) then lcl:w *= lcl:PcntWinW   !LRX = %
     end
     lcl:w -= lcl:x !note lcl:x has already been calculated
     !-------------------------------------------------------- Lower Right Y
     if   ~band(Self.ResizeQ.ResizeRules,Eq:LRY_Top    ) then lcl:h += lcl:DiffWinH   !ULY = Bottom
   ! elsif band(Self.ResizeQ.ResizeRules,Eq:LRX_Percent) then lcl:y *= lcl:PcntWinH   !ULY = Top-%
     end                                       !^!
     lcl:h -= lcl:y !note lcl:y has already been calculated
     !--------------------------------------------------------
     SetPosition(Self.ResizeQ.FEq, lcl:x, lcl:y, class_max(0,lcl:w), class_max(0,lcl:h))  !max added 6/6/03
 end
 self.LastWinW = lcl:NewWinW
 self.LastWinH = lcl:NewWinH

 System{PROP:DeferMove} = 0
 SetTarget
 Return(TRUE)


!=========================================================================
ResizeClassType.Set_Orient        Procedure(string ArgOrient)  !added June 15,2005
  code
  self.sOrient = argOrient
!=========================================================================
ResizeClassType.Set_Percent       Procedure(string ArgPercent) !added June 15,2005
  code
  self.sPercent = argPercent

!=========================================================================
ResizeClassType.Add_ResizeQ       Procedure(long ArgFEq)
  code
  self.Add_ResizeQ(ArgFEQ, self.sOrient, self.sPercent) !it's OK if self.sPercent='', it is handled even when not omitted

!=========================================================================
ResizeClassType.Add_ResizeQ       Procedure(long ArgFEQ,string ArgOrient,<string ArgPercent>) !original
PARAM:Percent Equate(4)       !ArgOrient-- off by one due to implicit 1st argument = class
      omit('*** documenation ***')
! =========================================================================
! ArgFEq     : a field equate
!              note: if ArgFEq = 0 then ArgOrient & ArgPercent are ignored
!
! ArgOrient  : A string that must be formatted as follows: (see feq)
!                [1] and [5] = 'L' -> Left or 'R' -> Right               !default R
!                [2] and [6] = '/' !position ignored, not important
!                [3] and [7] = 'T' -> Top  or 'B' -> Bottom              !default B
!                [4]         = ' '  !position ignored, not important
!
! ArgPercent : An optional string that must be formatted as follows (see feq) {defaults to 'A/A'}
!                [1] and [3] = 'A' -> Absolute or '%' -> Percent  !default 'A'
!                [2]         = '/' !position ignored, not important
!
! =========================================================================
 !end-omit('*** documenation ***')

  code
  !! message('Feq [' & argFEq & ']  Orient [' & ArgOrient & ']','In Add_ResizeQ')

  !======================================= added 6/27/03 -- allows revisions
  Self.ResizeQ.FEq = ArgFEq
  Get(Self.ResizeQ,Self.ResizeQ.FEq)
  if not ErrorCode() then Delete(Self.ResizeQ) end
  !======================================= added 6/27/03 -- allows revisions -end

  clear(Self.ResizeQ)
  Self.ResizeQ.FEq = ArgFEq
  GetPosition(Self.ResizeQ.FEq, Self.ResizeQ.OrigX, Self.ResizeQ.OrigY, Self.ResizeQ.OrigW, Self.ResizeQ.OrigH)

  if ArgFEq  ! <> 0

    if Len(clip(ArgOrient)) <> 7
       Message('Invalid pArgOrient [' & ArgOrient & ']| Field will not be resized','Programming Error - Add_ResizeQ',Icon:Asterisk, Button:Ok)
       Return
    end

    if ArgOrient[1] = 'L' then Self.ResizeQ.ResizeRules  = Eq:ULX_Left else Self.ResizeQ.ResizeRules = 0 end
    if ArgOrient[3] = 'T' then Self.ResizeQ.ResizeRules += Eq:ULY_Top  end
    if ArgOrient[5] = 'L' then Self.ResizeQ.ResizeRules += Eq:LRX_Left end
    if ArgOrient[7] = 'T' then Self.ResizeQ.ResizeRules += Eq:LRY_Top  end

    if ~omitted(PARAM:Percent)
      case Len(clip(ArgPercent))
        of 3 ; if ArgPercent[1] = '%' then Self.ResizeQ.ResizeRules  += Eq:ULX_Percent end
               if ArgPercent[3] = '%' then Self.ResizeQ.ResizeRules  += Eq:LRX_Percent end
        of 0
        else
              Message('Invalid pArgPercent [' & ArgPercent & ']| Field will not be resized','Programming Error - Add_ResizeQ',Icon:Asterisk, Button:Ok)
              Return
      end !case
    end
 end !if ArgFEq

 !!!!!!! message('About to Add(q)  Feq [' & argFEq & ']  Orient [' & ArgOrient & ']','In Add_ResizeQ')
 Add(Self.ResizeQ,Self.ResizeQ.FEq)
 if ErrorCode()
    Case Message('Error [' & Error() & ']|while adding Resize Queue Record','Internal Error',Icon:Hand, Button:Abort+Button:Ignore, Button:Ignore, 0)
       of Button:Abort  ; Halt(0,'Internal Error')
    !  of Button:Ignore ;
    end
 end


!=========================================================================
ResizeClassType.ResetOrigPosition   Procedure(<long argFEQ>) !Purpose, to get rid of Self.Close,do InitializeResizeQ construct
Param::argFEQ Equate(2)  !1=self
lcl:rec       ushort,auto
  code
  if not omitted( Param::argFEQ )
      Self.ResizeQ.FEQ = argFEQ
      get(Self.ResizeQ,Self.ResizeQ.FEQ)
      if ~ErrorCode()
         GetPosition(Self.ResizeQ.FEQ, Self.ResizeQ.OrigX, Self.ResizeQ.OrigY, Self.ResizeQ.OrigW, Self.ResizeQ.OrigH)
         put(Self.ResizeQ)
      end
  else
      loop lcl:rec = 1 to records(Self.ResizeQ)
          get(Self.ResizeQ,lcl:Rec)
          GetPosition(Self.ResizeQ.FEq, Self.ResizeQ.OrigX, Self.ResizeQ.OrigY, Self.ResizeQ.OrigW, Self.ResizeQ.OrigH)
          put(Self.ResizeQ)
      end !loop
      SetTarget(Self.ResizeWindow)
      self.LastWinW = 0{prop:Width }
      self.LastWinH = 0{prop:Height}
      self.OrigWinW = self.LastWinW
      self.OrigWinH = self.LastWinH
      SetTarget
  end



