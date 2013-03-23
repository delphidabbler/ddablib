@echo off
echo Updating .dproj files for Delphi 2007
echo.
FOR /R %%f IN (*.dproj) DO RENAME "%%f" "%%~nf.dproj.bak"
FOR /R %%f IN (*.dproj.2007) DO RENAME "%%f" "%%~nf"
echo Done