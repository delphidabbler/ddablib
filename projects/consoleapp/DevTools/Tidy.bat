@rem ---------------------------------------------------------------------------
@rem Script used to delete temporary and backup source files in the Console
@rem Application Runner classes source tree.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2010
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off
setlocal

echo Tidying
echo ~~~~~~~
echo.

set RootDir=..

echo Deleting temporary files
del /S %RootDir%\*.~* 
del /S %RootDir%\*.ddp 
del /S %RootDir%\*.dcu 
del /S %RootDir%\*.exe 
del /S %RootDir%\*.dsk 
del /S %RootDir%\*.bak
echo.

echo Deleting temporary directories
if exist %RootDir%\Release rmdir /S /Q %RootDir%\Release
for /F "usebackq" %%i in (`dir /S /B /A:D %RootDir%\__history*`) do rmdir /S /Q %%i
echo.

echo Done.

endlocal