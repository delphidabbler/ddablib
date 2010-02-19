@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing a Environment Variables Unit
@rem release.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 17 Aug 2008 - First version.
@rem ---------------------------------------------------------------------------


@echo off

setlocal

cd .\..

set OutFile=Releases\dd-envvars.zip
set DocsDir=Docs
set HelpDir=Help

if exist %OutFile% del %OutFile%

zip -j -9 %OutFile% PJEnvVars.pas
zip -j -9 %OutFile% PJEnvVars.dcr

zip -j -9 %OutFile% %HelpDir%\PJEnvVars.hlp
zip -j -9 %OutFile% %HelpDir%\PJEnvVars.als

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\DemoCode.htm

endlocal
