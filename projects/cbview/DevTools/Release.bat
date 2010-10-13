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

cd .\..

set OutFile=Releases\dd-cbview.zip
set DocsDir=Docs
set HelpDir=Help

if exist %OutFile% del %OutFile%

zip -j -9 %OutFile% PJCBView.pas
zip -j -9 %OutFile% PJCBView.dcr

zip -j -9 %OutFile% %HelpDir%\PJCBView.hlp
zip -j -9 %OutFile% %HelpDir%\PJCBView.als

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\DemoCode.htm

endlocal
