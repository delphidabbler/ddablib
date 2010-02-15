{
 * Delphi DUnit Test Project for PJMD5.pas
 * ---------------------------------------
 *
 * This project provides a DUnit test framework for the PJMD5.pas unit. It can
 * be compiled as either a GUI or Console application.
 *
 * Console Application
 * ~~~~~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must be defined to create a console application.
 * From the IDE define the symbol in the conditional defines entry in project
 * options. To compile from the command line pass the -D switch to the compiler:
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B PJMD5Test
 *
 * GUI Application
 * ~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must not be defined to compile a GUI
 * application. From the IDE make sure the symbol is removed from the
 * conditional defines entry in project options if necessary. The command line
 * compiler needs to have the path to the folder where DUnit's .dfm file can be
 * found (usually $(BDS)\lib). Use the following command line:
 *
 *   DCC32 -B -R<path-to-dunit-binaries> PJMD5Test
 *
 * for example:
 *
 *   DCC32 -B -R"C:\Program Files\Embarcadero\RAD Studio\7.0\lib" PJMD5Test
 *
 * -----------------------------------------------------------------------------
 * $Rev$
 * $Date$
 * -----------------------------------------------------------------------------
}

program PJMD5Test;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  {$IFDEF CONSOLE_TESTRUNNER}
  TextTestRunner,
  {$ELSE}
  Forms,
  GUITestRunner,
  XPMan,
  {$ENDIF}
  TestFramework,
  PJMD5 in 'PJMD5.pas',
  TestPJMD5 in 'TestPJMD5.pas';

{$R *.RES}

begin
  {$IFDEF CONSOLE_TESTRUNNER}
  with TextTestRunner.RunRegisteredTests do
    Free;
  {$ELSE}
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
  {$ENDIF}
end.

