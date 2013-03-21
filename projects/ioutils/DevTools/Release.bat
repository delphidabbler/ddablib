@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing release files for I/O Utilities
@rem Classes.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2012
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

cd .\..

set OutFile=Release\dd-ioutils.zip
set DocsDir=Docs

if not exist Release mkdir Release
if exist %OutFile% del %OutFile%

zip %OutFile% -9 PJPipe.pas
zip %OutFile% -9 PJPipeFilters.pas
zip %OutFile% -9 PJFileHandle.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL-2.0.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Documentation.URL

endlocal
