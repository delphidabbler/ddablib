@rem ---------------------------------------------------------------------------
@rem Script used to delete temp and backup source files in the Console
@rem Application Runner classes.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007
@rem
@rem v1.0 of 03 Oct 2007 - Original version.
@rem ---------------------------------------------------------------------------

@echo off

setlocal

set SrcDir=..
set DocsDir=..\Docs
set ExeDir=..\Demos\Exe

echo Deleting *.~*, *.dcu, *.dsk and *.ddp from "%SrcDir%" and subfolders
del /S %SrcDir%\*.~* 
del /S %SrcDir%\*.dsk
del /S %SrcDir%\*.dcu
del /S %SrcDir%\*.ddp
echo.

echo Deleting *.exe from Demos executable directory
del /S %ExeDir%\*.exe

echo Deleting *.~* from "%DocsDir%" and subfolders
del /S %DocsDir%\*.~*
echo.

echo Done

endlocal
