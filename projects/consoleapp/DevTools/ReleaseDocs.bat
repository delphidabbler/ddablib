@rem ---------------------------------------------------------------------------
@rem Console Application Runner Classes.
@rem
@rem Script used to create zip file containing PDF documentation.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007
@rem
@rem v1.0 of 03 Oct 2007 - First version.
@rem ---------------------------------------------------------------------------

@echo off

setlocal

call Tidy.bat

cd ..

set OutFile=Release\dd-consoleapp-docs.zip

del %OutFile%

zip %OutFile% -j -9 Docs\UserGuide.pdf

endlocal
