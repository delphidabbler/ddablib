@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing a Environment Variables Unit
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

cd .\..

set OutFile=Release\dd-envvars.zip
set SrcDir=
set DocsDir=Docs
set HelpDir=Help

if exist Release rmdir /S /Q Release
mkdir Release

zip -j -9 %OutFile% PJEnvVars.pas
zip -j -9 %OutFile% PJEnvVars.dcr

zip -j -9 %OutFile% %HelpDir%\PJEnvVars.als
zip -j -9 %OutFile% %HelpDir%\PJEnvVars.hlp

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm
zip -j -9 %OutFile% %DocsDir%\DemoCode.htm
zip -j -9 %OutFile% %DocsDir%\Wiki.URL

endlocal
