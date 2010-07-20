@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing release files for Console
@rem Application Runner Classes.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2010
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

call Tidy.bat

cd .\..

set OutFile=Release\dd-consoleapp.zip
set DocsDir=Docs
set DemosDir=Demos

if not exist Release mkdir Release
if exist %OutFile% del %OutFile%

zip %OutFile% -9 PJConsoleApp.pas
zip %OutFile% -9 PJPipe.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Wiki.URL

zip %OutFile% -r -9 %DemosDir%\*.*
zip %OutFile% -d %DemosDir%\*.svn\*


endlocal
