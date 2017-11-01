 omit('***VER IS SET**',_IFDEF_VER_)
_IFDEF_VER_ EQUATE(1)
             compile('**',_C73_)
             !also applies to _C80_
_Ver_  EQUATE('LA')
             !       '**'
             omit('**C73**',_C73_)
               compile('**',_C70_)
_Ver_  EQUATE('70')
               !       '**'
               omit('**C70**',_C70_)
                 compile('**',_C60_)
_Ver_  EQUATE('60')
!     PRAGMA('project(#message "_VER_[60]")')
                 !       '**'
                 omit('**C60**',_C60_)
                    compile('**',_C55_)
_Ver_  EQUATE('55')
                    !       '**'
                    omit('**C55**',_C55_)
                      compile('**',_C5_)
_Ver_  EQUATE('5')
                      !       '**'
                      omit('**C50**',_C5_)
                         compile('**',_CLW21_)
_Ver_  EQUATE('4')  !<-- todo - confirm
                         !       '**'
                         omit('**C4**',_CLW21_)
                            compile('**',_CLW20_)
_Ver_  EQUATE('2')
                            !       '**'
                            omit('**C20**',_CLW20_)
                               compile('**',_CLW15_)
_Ver_  EQUATE('15') !<-- todo - confirm
                               !       '**'
                               omit('**C15**',_CLW15_)
                               compile('**',_CW_)
_Ver_  EQUATE('')   !<-- todo - confirm
                               !       '**'
                               !end-omit('**C15**',_CLW15_)
                            !end-omit('**C20**',_CLW20_)
                         !end-omit('**C4**',_CLW21_)
                      !end-omit('**C50**',_C5_)
                    !end-omit('**C55**',_C55_)
                 !end-omit('**C60**',_C60_)
              !end-omit('**C70**',_C70_)
           !end-omit('**C73**',_C73_)
!end-omit('***VER IS SET**',_IFDEF_CWVER_)

