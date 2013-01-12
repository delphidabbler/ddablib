@rem ---------------------------------------------------------------------------
@rem Fractions Unit
@rem
@rem Script used to create zip file containing release files.
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
set OutFile=%ReleaseDir%\dd-fractions.zip
set DocsDir=Docs
set TestDir=Test

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -j -9 DelphiDabbler.Lib.Fractions.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL-2.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Documentation.URL

zip %OutFile% -r -9 %TestDir%\*.* -x *.svn\*

endlocal
