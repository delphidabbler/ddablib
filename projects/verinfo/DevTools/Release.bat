@rem ---------------------------------------------------------------------------
@rem Version Information Component.
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
set OutFile=%ReleaseDir%\dd-verinfo.zip
set DocsDir=Docs
set HelpDir=Help
set DemoDir=Demo

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -9 PJVersionInfo.pas
zip %OutFile% -9 PJVersionInfo.dcr

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Wiki.URL

zip %OutFile% -j -9 %HelpDir%\PJVersionInfo.hlp
zip %OutFile% -j -9 %HelpDir%\PJVersionInfo.als

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
