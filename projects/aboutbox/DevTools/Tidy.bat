@rem ---------------------------------------------------------------------------
@rem Script used to delete About Box Dialogs's temp, backup files and test .dcu
@rem and .exe files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008-2009
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off
setlocal

echo Tidying
echo ~~~~~~~
echo.

set SrcDir=..

echo Deleting *.~* from "%SrcDir%" and subfolders
del /S %SrcDir%\*.~* 
echo.

echo Deleting *.dpp from "%SrcDir%" and subfolders
del /S %SrcDir%\*.ddp 
echo.

echo Deleting *.dcu from "%SrcDir%" and subfolders
del /S %SrcDir%\*.dcu 
echo.

echo Deleting *.exe from "%SrcDir%" and subfolders
del /S %SrcDir%\*.exe 
echo.

echo Deleting *.dsk from "%SrcDir%" and subfolders
del /S %SrcDir%\*.dsk 
echo.

echo Done.

endlocal
