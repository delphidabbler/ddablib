@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing Shell Folders Unit release
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

set OutFile=Release\dd-shellfolders.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help

if exist Release rmdir /S /Q Release
mkdir Release

zip -j -9 %OutFile% PJShellFolders.pas
zip -j -9 %OutFile% PJShellFolders.dcr
zip -j -9 %OutFile% PJShellFoldersDsgn.pas

zip -j -9 %OutFile% %HelpDir%\PJShellFolders.als
zip -j -9 %OutFile% %HelpDir%\PJShellFolders.hlp

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\Wiki.URL

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
