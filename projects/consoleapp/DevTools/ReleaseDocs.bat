@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing PDF documentation for Console
@rem Application Runner Classes.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2010
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

call Tidy.bat

cd ..

set OutFile=Release\dd-consoleapp-docs.zip

del %OutFile%

zip %OutFile% -j -9 Docs\UserGuide.pdf

endlocal
