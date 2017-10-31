#TEMPLATE (GenTXARun, 'Mark Goldberg Shareware Template '),FAMILY('ABC'),FAMILY('CW20'),FAMILY('CLARION')
#!------------------------------------------------------------------------------------------------------------
#! Template Written: Apr-06-2011 by Monolith Custom Computing, Inc.
#!                   Oct-31-2017 added prompt to be able suppress by Module
#! e-mail: Clarion@MonolithCC.com
#!------------------------------------------------------------------------------------------------------------
#Extension (GenerateTXA,'Generate .TXA for each Module - uses Run for Dictionary'),Application,Description('Generate .TXA for each Module')   
#DISPLAY('Generate .TXA for each Module on every build')
#DISPLAY ('Also Generate:')
#PROMPT ('&Include Procedure Name', CHECK), %GenAPP, DEFAULT(0)
#PROMPT ('&Dictionary', CHECK), %GenDCTX, DEFAULT(0)
#PROMPT ('&Whole Application', CHECK), %GenAPPSLG, DEFAULT(0)
#PROMPT ('&By Module', CHECK), %ByModule, DEFAULT(1)
#DISPLAY ('')
#DISPLAY ('This Extension is *much* faster than I expected')
#DISPLAY ('')
#DISPLAY ('Created: Apr/6/11 by Monolith Custom Computing, Inc.')
#DISPLAY ('Last Update: Apr/6/11')
#DISPLAY ('')    
#!
#AT(%BeforeGenerateApplication)
  #IF (%ByModule = 1)
    #FOR(%Module)
     #IF (%GenAPP = 1)
        #SELECT( %ModuleProcedure,1)
        #SET (%ValueConstruct, %Application &'_'& %ModuleProcedure &'.TXA')
        #CREATE(%ValueConstruct)
        #EXPORT(%Module)
        #CLOSE(%ValueConstruct)
     #ELSE
        #SET (%ValueConstruct, %ModuleBase &'.TXA')
        #CREATE(%ValueConstruct)
        #EXPORT(%Module)
        #CLOSE(%ValueConstruct)
     #ENDIF
    #ENDFOR
  #ENDIF
  #!
  #!--- EXPORT(%Program), is redundant, testing shows it appears at the last module
  #!
  #IF (%GenDCTX = 1)   
    #DECLARE (%DctCmdString)
    #DECLARE (%ClarionCL)              
    #DECLARE(%TemplateRoot)
    #DECLARE(%RunDLL)          
    #SET(%RunDLL,'WrapRun.dll')
    #SET(%TemplateRoot,%CWRoot &'\Accessory\template\win\')
    #SET(%ClarionCL,  %CWRoot &'\bin\ClarionCL.exe /dx ')
    #SET (%DctCmdString, 'XCOPY '&%DictionaryFile & ' ' & %DictionaryFile &'.tmp /Y /Z')  
    #rundll(%TemplateRoot & %RunDLL,'RUN_WAITUNTILCOMPLETED',%DctCmdString)
    #SET (%DctCmdString, %ClarionCL &''& %DictionaryFile  &'.tmp '&%DictionaryFile&'.txd') 
    #rundll(%TemplateRoot & %RunDLL,'RUN_RETURNRIGHTAWAY', %DctCmdString )
  #ENDIF
  #!
  #IF (%GenAPPSLG = 1)
    #SET (%ValueConstruct, %Application &'(WholeApp).TXA')
    #CREATE(%ValueConstruct)
    #EXPORT
    #! generated 0 byte file: #EXPORT(%Application)
    #CLOSE(%ValueConstruct)
  #ENDIF
#ENDAT
