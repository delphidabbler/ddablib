@rem ---------------------------------------------------------------------------
@rem Script used to delete temporary files and directories in either a single
@rem project or all projects in the DelphiDabbler Code Library.
@rem
@rem Takes a single parameter which is either the id of the project to be tidied
@rem or "all", without the quotes, to tidy the complete project directory.
@rem
@rem Any copyright in this file is dedicated to the Public Domain.
@rem http://creativecommons.org/publicdomain/zero/1.0/
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off
setlocal

echo Tidying
echo ~~~~~~~
echo.

if not "%1" == "" goto checkforall
set ErrorMsg=No command specified: must be project id or "all".
goto error

:checkforall
if not "%1" == "all" goto checkid
set RootDir=..\..\projects
set InfoMsg=projects directory
goto start

:checkid
set RootDir=..\..\projects\%1
set InfoMsg="%1" project
if exist %RootDir% goto start
set ErrorMsg="%1" is not a valid project id
goto error

:start
echo Deleting temporary files from %InfoMsg%
del /S %RootDir%\*.~*
del /S %RootDir%\*.bak
del /S %RootDir%\*.dcu
del /S %RootDir%\*.ddp
del /S %RootDir%\*.dsk
del /S %RootDir%\*.exe
del /S /AH %RootDir%\*.GID
del /S %RootDir%\*.identcache
del /S %RootDir%\*.ini
del /S %RootDir%\*.local
del /S %RootDir%\*.tmp
echo.

echo Deleting temporary sub-directories from %InfoMsg%
if exist %RootDir%\Release rmdir /S /Q %RootDir%\Release
for /F "usebackq" %%i in (`dir /S /B /A:D %RootDir%\__history*`) do rmdir /S /Q %%i
echo.

goto end

:error
echo ** ERROR: %ErrorMsg%

:end
echo Done.

endlocal
