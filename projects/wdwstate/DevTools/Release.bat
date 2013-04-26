@rem ---------------------------------------------------------------------------
@rem Window State Components.
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
zip %OutFile% -j -9 %DocsDir%\MPL-2.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Documentation.URL

zip %OutFile% -j -9 %HelpDir%\PJWdwState.hlp
zip %OutFile% -j -9 %HelpDir%\PJWdwState.als

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
