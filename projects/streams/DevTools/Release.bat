@rem ---------------------------------------------------------------------------
@rem Stream Extension Classes.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2009-2011
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

set OutFile=Release\dd-streams.zip
set DocsDir=Docs

cd .\..

if exist Release rmdir /S /Q Release
mkdir Release

zip %OutFile% -j -9 PJIStreams.pas
zip %OutFile% -j -9 PJStreamWrapper.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\MPL-2.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\Documentation.URL

zip %OutFile% -r -9 .\Demos\*.* -x *.svn\* *Test.*
zip %OutFile% -r -9 .\Tests\*.* -x *Bin\* *.svn\*

endlocal
