@rem ---------------------------------------------------------------------------
@rem Script used to a create zip file containing the release files for the HKEY
@rem Property Editor.
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
set OutFile=%ReleaseDir%\dd-hkeype.zip
set DocsDir=Docs

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -j -9 PJHKEYPropEdit.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm

endlocal
