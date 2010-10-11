@rem ---------------------------------------------------------------------------
@rem Hot Label Component.
@rem
@rem Script used to create zip file containing release files.
@rem
@rem Copyright (C) Peter Johnson (www.delphidabbler.com), 2007-2010
@rem
@rem $Rev$
@rem $Date$
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
