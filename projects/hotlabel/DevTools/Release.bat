@rem ---------------------------------------------------------------------------
@rem Script used to create zip file containing source code of Hot Label
@rem Component
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007
@rem
@rem v1.0 of 17 Mar 2007 - First version.
@rem ---------------------------------------------------------------------------


@echo off

cd .\..

set OutFile=Releases\dd-hotlabel.zip
set SrcDir=
set DocsDir=Docs
set DemoDir=Demo
set HelpDir=Help

del %OutFile%

zip -j -9 %OutFile% PJHotLabel.pas
zip -j -9 %OutFile% PJHotLabel.dcr
zip -j -9 %OutFile% PJHotLabel.rc
zip -j -9 %OutFile% PJHotLabel.res
zip -j -9 %OutFile% Hand.cur

zip -j -9 %OutFile% %HelpDir%\PJHotLabel.hlp
zip -j -9 %OutFile% %HelpDir%\PJHotLabel.als

zip -j -9 %OutFile% %DocsDir%\ChangeLog.txt
zip -j -9 %OutFile% %DocsDir%\MPL.txt
zip -j -9 %OutFile% %DocsDir%\ReadMe.htm

zip -9 %OutFile% %DemoDir%\HotLabelDemo.dpr
zip -9 %OutFile% %DemoDir%\HotLabelDemo.res
zip -9 %OutFile% %DemoDir%\FmHotLabelDemo.pas
zip -9 %OutFile% %DemoDir%\FmHotLabelDemo.dfm
zip -9 %OutFile% %DemoDir%\DelphiDabbler.ico

cd DevTools
