@rem ---------------------------------------------------------------------------
@rem Console Application Runner Classes.
@rem
@rem Script used to create correct format .dproj files for compilation of demos
@rem with Delphi 2007.
@rem
@rem Any copyright in this file is dedicated to the Public Domain.
@rem http://creativecommons.org/publicdomain/zero/1.0/
@rem
@rem $Rev$
@rem $Date$
@rem ---------------------------------------------------------------------------

@echo off
echo Updating .dproj files for Delphi 2007
echo.
FOR /R %%f IN (*.dproj) DO RENAME "%%f" "%%~nf.dproj.bak"
FOR /R %%f IN (*.dproj.2007) DO RENAME "%%f" "%%~nf"
echo Done