@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing source code of Drop Files
@rem Components.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2010
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------


@echo off

setlocal

cd .\..

set OutFile=Release\dd-dropfiles.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help

if exist Release rmdir /S /Q Release
mkdir Release


zip -j -9 %OutFile% PJDropFiles.pas
zip -j -9 %OutFile% PJDropFiles.dcr
zip -j -9 %OutFile% PJDropFilesDsgn.pas
zip -j -9 %OutFile% PJDropFilesDsgn.dfm

zip -j -9 %OutFile% %HelpDir%\PJDropFiles.hlp
zip -j -9 %OutFile% %HelpDir%\PJDropFiles.als

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\Wiki.URL

zip %OutFile% -r -9 %DemoDir%\*.*
zip %OutFile% -d %DemoDir%\*.svn\*

endlocal
