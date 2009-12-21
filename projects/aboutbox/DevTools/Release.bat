@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing source code of About Box
@rem Component.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2009
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------


@echo off

setlocal

cd .\..

set OutFile=Release\dd-aboutbox.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help

if exist Release rmdir /S /Q Release
mkdir Release


zip -j -9 %OutFile% PJAbout.pas
zip -j -9 %OutFile% PJAbout.dfm
zip -j -9 %OutFile% PJAbout.dcr

zip -j -9 %OutFile% %HelpDir%\PJAbout.hlp
zip -j -9 %OutFile% %HelpDir%\PJAbout.als

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\Wiki.URL

zip -9 %OutFile% %DemoDir%\AboutBoxDemo.dpr
zip -9 %OutFile% %DemoDir%\AboutBoxDemo.res
zip -9 %OutFile% %DemoDir%\FmDemo.pas
zip -9 %OutFile% %DemoDir%\FmDemo.dfm
zip -9 %OutFile% %DemoDir%\VerInfo.rc
zip -9 %OutFile% %DemoDir%\VerInfo.res

endlocal
