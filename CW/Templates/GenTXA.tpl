#TEMPLATE (GenTXA, 'Mark Goldberg Shareware Template'),FAMILY('ABC'),FAMILY('CW20'),FAMILY('CLARION')
#!------------------------------------------------------------------------------------------------------------
#! Template Written: Apr-06-2011 by Monolith Custom Computing, Inc.
#!                   Oct-31-2017 added prompt to be able suppress by Module
#! e-mail: Clarion@MonolithCC.com
#!------------------------------------------------------------------------------------------------------------
#Extension (GenerateTXA,'Generate .TXA for each Module'),Application,Description('Generate .TXA for each Module')
#DISPLAY('Generate .TXA for each Module on every build')
#DISPLAY ('Also Generate:')
#PROMPT ('&Dictionary', CHECK), %GenTXD, DEFAULT(0)
#PROMPT ('&Whole Application', CHECK), %GenAPP, DEFAULT(0)
#PROMPT ('&By Module', CHECK), %ByModule, DEFAULT(1)
#DISPLAY ('')
#DISPLAY ('Select any file, in the folder to Export to:')
#PROMPT('Folder',SAVEDIALOG('Pick any file in this folder','*|*.*')),%ExportFile
#DISPLAY ('')
#DISPLAY ('This Extension is *much* faster than I expected')
#DISPLAY ('')
#DISPLAY ('https://github.com/MarkGoldberg/ClarionCommunity')
#DISPLAY ('Created  : Apr/6/11 by Monolith Custom Computing, Inc.')
#DISPLAY ('Update   : Feb/8/2014')
#DISPLAY ('')
#!-------------------------------------------------------------------------
#AT(%BeforeGenerateApplication)
  #DECLARE(%ExportFolder)
  #SET(%ExportFolder, SLICE(%ExportFile, 1, LEN(CLIP( %ExportFile )) - LEN(CLIP(TAILNAME(%ExportFile)))) )  #!get folder only
  #IF (%ByModule = 1)
     #FOR(%Module)
       #IF (ITEMS( %ModuleProcedure ) = 1 )
           #SELECT( %ModuleProcedure, 1 )
           #SET (%ValueConstruct, %ExportFolder & %Application &'_'& %ModuleProcedure &'.TXA')
       #ELSE
           #SET (%ValueConstruct, %ExportFolder & %ModuleBase &'.TXA')
       #ENDIF
       #CREATE(%ValueConstruct)
       #EXPORT(%Module)
       #CLOSE(%ValueConstruct)
     #ENDFOR
  #ENDIF 
  #!
  #!--- EXPORT(%Program), is redundant, testing shows it appears at the last module
  #!
  #IF (%GenTXD = 1)
    #!consider paying attention to %DictionaryChanged
    #SET (%ValueConstruct, %ExportFolder & TAILNAME(%DictionaryFile) &'.TXD')
    #CREATE(%ValueConstruct)
    #EXPORT(%DictionaryFile)
    #CLOSE(%ValueConstruct)
  #ENDIF
  #!
  #IF (%GenAPP = 1)
    #SET (%ValueConstruct, %ExportFolder & %Application &'(WholeApp).TXA')
    #CREATE(%ValueConstruct)
    #EXPORT
    #! generated 0 byte file: #EXPORT(%Application)
    #CLOSE(%ValueConstruct)
  #ENDIF
#ENDAT

