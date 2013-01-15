@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing source code of About Box
@rem Component.
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
zip -j -9 %OutFile% %DocsDir%\MPL-2.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\Documentation.URL

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
