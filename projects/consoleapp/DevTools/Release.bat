@rem ---------------------------------------------------------------------------
@rem Console Application Runner Classes.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Any copyright in this file is dedicated to the Public Domain.
@rem http://creativecommons.org/publicdomain/zero/1.0/
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

cd .\..

set OutFile=Release\dd-consoleapp.zip
set DocsDir=Docs
set DemosDir=Demos

if not exist Release mkdir Release
if exist %OutFile% del %OutFile%

zip %OutFile% -9 PJConsoleApp.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\DocumentationWiki.URL

zip %OutFile% -r -9 %DemosDir%\*.* -x *.svn\*

endlocal
