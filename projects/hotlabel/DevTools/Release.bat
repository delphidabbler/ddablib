@rem ---------------------------------------------------------------------------
@rem Hot Label Component.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2010
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------


@echo off

setlocal

set ReleaseDir=Release
set OutFile=%ReleaseDir%\dd-hotlabel.zip
set HelpDir=Help
set DocsDir=Docs
set DemoDir=Demo

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -j -9 PJHotLabel.pas
zip %OutFile% -j -9 PJHotLabel.dcr

zip %OutFile% -j -9 %HelpDir%\PJHotLabel.hlp
zip %OutFile% -j -9 %HelpDir%\PJHotLabel.als

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
