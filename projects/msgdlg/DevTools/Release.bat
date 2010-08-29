@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing Message Dialog Components release
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2010
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

cd .\..

set OutFile=Release\dd-messagedlg.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help

if exist Release rmdir /S /Q Release
mkdir Release

zip -j -9 %OutFile% PJMessageDialog.pas
zip -j -9 %OutFile% PJMessageDialog.dcr

zip -j -9 %OutFile% %HelpDir%\PJMessageDialog.als
zip -j -9 %OutFile% %HelpDir%\PJMessageDialog.hlp

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\Wiki.URL

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
