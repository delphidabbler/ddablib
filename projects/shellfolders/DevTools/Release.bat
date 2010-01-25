@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing Shell Folders Unit release
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2010
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

zip %OutFile% -r -9 %DemoDir%\*.*
zip %OutFile% -d %DemoDir%\*.svn\*

endlocal
