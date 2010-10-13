@rem ---------------------------------------------------------------------------
@rem Clipboard Viewer Component.
@rem
@rem Script used to create component's help file.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2010.
@rem
@rem Requires the DELPHI7 environment variable to store the Delphi 7 install
@rem directory.
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------


@echo off

setlocal

set HelpDir=..\Help

rem Check that required files exist

set ErrorMsg=

rem Build help file
%DELPHI7%\Help\Tools\HCRTF.exe -x %HelpDir%\PJCBView.hlp
if errorlevel 1 set ErrorMsg=Compilation failed
if not "%ErrorMsg%"=="" goto error
goto success

:error
rem Display error message
echo *** ERROR: %ErrorMsg%
goto end

:success
echo Succeeded

:end
rem All done

endlocal