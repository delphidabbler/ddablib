@rem ---------------------------------------------------------------------------
@rem Window State Components.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007
@rem
@rem v1.0 of 19 Sep 2007 - First version.
@rem ---------------------------------------------------------------------------

@echo off

setlocal

cd ..

set OutFile=Release\dd-wdwstate.zip

del %OutFile%

zip %OutFile% -9 PJWdwState.pas
zip %OutFile% -9 PJWdwState.dcr

zip %OutFile% -j -9 ..\HKeyPE\PJHKEYPropEdit.pas

zip %OutFile% -j -9 Docs\ChangeLog.txt
zip %OutFile% -j -9 Docs\MPL.txt
zip %OutFile% -j -9 Docs\ReadMe.htm

zip %OutFile% -j -9 Help\PJWdwState.hlp
zip %OutFile% -j -9 Help\PJWdwState.als

zip %OutFile% -r -9 Demos\*.*

endlocal
