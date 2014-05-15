
-- Add paths that are effective for all configurations
-- eg *.exe = exe
-- The redirection system has an order of precedence where a line has priority over later lines

[Debug]
-- Add paths that are only effective when Debug configuration is being built
-- eg *.exe = exe\debug

[Release]
-- Add paths that are only effective when Release configuration is being built
-- eg *.exe = exe\release

[Common]
-- Add paths that are effective for all configurations
-- eg *.exe = exe

------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------
-- To set %Shared%, 
--   IDE.Tools.Options.Clarion.ClarionForWindows.Versions
--   select the version of the compiler you wish to use (at the top of the screen)
--   select the Tab[Redirection File]
--   click on Add set  [Macro]=Shared [Value]= <YourDriveDir>\ClarionCommunity\CW\Shared
--   click on OK
------------------------------------------------------------------------------------------
------------------------------------------------------------------------------------------


*.inc = %Shared%\Src
*.clw = %Shared%\Src
*.equ = %Shared%\Src
*.int = %Shared%\Src

*.cur = %Shared%\Images
*.emf = %Shared%\Images
*.wmf = %Shared%\Images
*.jpg = %Shared%\Images
*.png = %Shared%\Images
*.ico = %Shared%\Images


{include %REDDIR%\%REDNAME% }
