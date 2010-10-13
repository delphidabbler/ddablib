@rem ---------------------------------------------------------------------------
@rem Clipboard Viewer Component.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2010.
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------


@echo off

setlocal

set ReleaseDir=Release
set OutFile=%ReleaseDir%\dd-cbview.zip
set HelpDir=Help
set DocsDir=Docs

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -j -9 PJCBView.pas
zip %OutFile% -j -9 PJCBView.dcr

zip %OutFile% -j -9 %HelpDir%\PJCBView.hlp
zip %OutFile% -j -9 %HelpDir%\PJCBView.als

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\DemoCode.htm
zip %OutFile% -j -9 %DocsDir%\Wiki.URL

endlocal
