@rem ---------------------------------------------------------------------------
@rem Script used to delete temp and backup source files
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007
@rem
@rem v1.0 of 03 Jul 2007 - First version.
@rem ---------------------------------------------------------------------------

@echo off
set BaseDir=..

echo Deleting *.~*
del %BaseDir%\*.~*
del %BaseDir%\Demo\*.~*
del %BaseDir%\Docs\*.~*
del %BaseDir%\Help\*.~*

echo Deleting *.ddp
del %BaseDir%\Demo\*.ddp

echo Done.
