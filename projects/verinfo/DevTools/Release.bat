@rem ---------------------------------------------------------------------------
@rem Version Information Component.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2009
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

zip %OutFile% -j -9 %HelpDir%\PJVersionInfo.hlp
zip %OutFile% -j -9 %HelpDir%\PJVersionInfo.als

zip %OutFile% -r -9 %DemoDir%\*.*
zip %OutFile% -d %DemoDir%\*.svn\*

endlocal
