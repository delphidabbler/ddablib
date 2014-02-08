@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing Message Dialogue Unit release
@rem files.
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

set OutFile=Release\dd-messagedlg.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help

if exist Release rmdir /S /Q Release
mkdir Release

zip -j -9 %OutFile% PJMessageDialog.pas
zip -j -9 %OutFile% PJMessageDialog.dcr

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL-2.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\Documentation.url

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
