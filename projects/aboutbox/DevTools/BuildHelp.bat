@rem ---------------------------------------------------------------------------
@rem Script used to create help file for About Box Component.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2009
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------


@echo off

set HelpDir=..\Help

set ErrorMsg=

rem Build help file into exe folder
%DELPHI7%\Help\Tools\HCRTF.exe -x %HelpDir%\PJAbout.hlp
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
