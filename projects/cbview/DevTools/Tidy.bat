@rem ---------------------------------------------------------------------------
@rem Script used to delete Clipboard Viewer Component's backup files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2008
@rem
@rem v1.0 of 16 Aug 2008 - First version.
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

echo Done.

endlocal
