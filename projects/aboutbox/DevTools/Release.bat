@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing source code of About Box
@rem Component.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 16 Jun 2008 - First version.
@rem ---------------------------------------------------------------------------


@echo off

setlocal

cd .\..

set OutFile=Releases\dd-aboutbox.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help
set Dir32=32
set Dir16=16

if exist %OutFile% del %OutFile%

zip -j -9 %OutFile% PJAbout.pas
zip -j -9 %OutFile% PJAbout.dfm

zip -9 %OutFile% %Dir32%\PJAbout.dcr

zip -9 %OutFile% %Dir16%\PJAbout.dcr

zip -j -9 %OutFile% %HelpDir%\PJAbout.hlp
zip -j -9 %OutFile% %HelpDir%\PJAbout.als

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm

zip -9 %OutFile% %DemoDir%\AboutBoxDemo.dpr
zip -9 %OutFile% %DemoDir%\AboutBoxDemo.res
zip -9 %OutFile% %DemoDir%\AboutBoxDemo.cfg
zip -9 %OutFile% %DemoDir%\FmDemo.pas
zip -9 %OutFile% %DemoDir%\FmDemo.dfm
zip -9 %OutFile% %DemoDir%\VerInfo.rc
zip -9 %OutFile% %DemoDir%\VerInfo.res

endlocal
