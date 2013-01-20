@rem ---------------------------------------------------------------------------
@rem MD5 Message Digest Unit.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Any copyright in this file is dedicated to the Public Domain.
@rem http://creativecommons.org/publicdomain/zero/1.0/
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off

setlocal

set ReleaseDir=Release
set OutFile=%ReleaseDir%\dd-md5.zip
set DocsDir=Docs
set TestDir=Test

cd .\..

if exist %ReleaseDir% rmdir /S /Q %ReleaseDir%
mkdir %ReleaseDir%

zip %OutFile% -j -9 PJMD5.pas

zip %OutFile% -j -9 %DocsDir%\ChangeLog.txt
zip %OutFile% -j -9 %DocsDir%\License.txt
zip %OutFile% -j -9 %DocsDir%\GPL-2.0.txt
zip %OutFile% -j -9 %DocsDir%\LGPL-2.1.txt
zip %OutFile% -j -9 %DocsDir%\MD5-Notice.txt
zip %OutFile% -j -9 %DocsDir%\MPL-1.1.txt
zip %OutFile% -j -9 %DocsDir%\ReadMe.htm
zip %OutFile% -j -9 %DocsDir%\DocumentationWiki.URL

zip %OutFile% -r -9 %TestDir%\*.* -x *.svn\*

endlocal
