@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing Shell Folders Unit release
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007
@rem
@rem v1.0 of 03 Jul 2007 - First version.
@rem ---------------------------------------------------------------------------

@echo off
cd ..

set OutFile=Releases\dd-shellfolders.zip
del %OutFile%

zip -j -9 %OutFile% PJShellFolders.pas PJShellFolders.dcr PJShellFoldersDsgn.pas
zip -j -9 %OutFile% Docs\ChangeLog.txt Docs\ReadMe.htm
zip -j -9 %OutFile% Help\PJShellFolders.als Help\PJShellFolders.hlp
zip -r -9 %OutFile% Demo\*.*

cd DevTools
