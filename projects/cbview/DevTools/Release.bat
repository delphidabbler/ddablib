@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing Clipboard Viewer Component
@rem release files.
@rem
@rem Any copyright in this file is dedicated to the Public Domain.
@rem http://creativecommons.org/publicdomain/zero/1.0/
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

set ReleaseDir=Release
set OutFile=%ReleaseDir%\dd-cbview.zip
set HelpDir=Help
set DocsDir=Docs
set DemoDir=Demo

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -j -9 PJCBView.pas
zip %OutFile% -j -9 PJCBView.dcr

zip %OutFile% -j -9 %HelpDir%\PJCBView.hlp
zip %OutFile% -j -9 %HelpDir%\PJCBView.als

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL-2.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Documentation.url

zip %OutFile% -r -9 %DemoDir%\*.* -x *.svn\*

endlocal
