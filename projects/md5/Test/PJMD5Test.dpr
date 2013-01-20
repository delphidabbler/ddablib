{
 * Delphi DUnit Test Project for PJMD5.pas
 * ---------------------------------------
 *
 * This project provides a DUnit test framework for the PJMD5.pas unit.
 *
 * When compiled with Delphi XE3 or later, either the 32 bit or 64 bit Windows
 * compiler can be used - just selected the required target platform.
 *
 * -----------------------------------------------------------------------------
 * $Rev$
 * $Date$
 * -----------------------------------------------------------------------------
}

program PJMD5Test;

uses
  Forms,
  GUITestRunner,
  TestFramework,
  TestPJMD5 in 'TestPJMD5.pas',
  PJMD5 in '..\PJMD5.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.

