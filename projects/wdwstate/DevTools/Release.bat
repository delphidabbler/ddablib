@rem ---------------------------------------------------------------------------
@rem Window State Components.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2009
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

set ReleaseDir=Release
set OutFile=Release\dd-wdwstate.zip
set DocsDir=Docs
set HelpDir=Help
set DemoDir=Demos

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -9 PJWdwState.pas
zip %OutFile% -9 PJWdwState.dcr

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Wiki.URL

zip %OutFile% -j -9 %HelpDir%\PJWdwState.hlp
zip %OutFile% -j -9 %HelpDir%\PJWdwState.als

zip %OutFile% -r -9 %DemoDir%\*.*
zip %OutFile% -d %DemoDir%\*.svn\*

endlocal
