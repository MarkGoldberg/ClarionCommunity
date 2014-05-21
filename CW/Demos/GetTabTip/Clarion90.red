
*.clw = ..\..\Shared\Src
*.inc = ..\..\Shared\Src
*.int = ..\..\Shared\Src


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

{include %REDDIR%\%REDNAME% }
