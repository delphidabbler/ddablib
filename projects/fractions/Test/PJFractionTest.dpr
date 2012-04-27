{
 * Delphi DUnit Test Project for PJFractions.pas
 * ---------------------------------------------
 *
 * This project provides a DUnit test framework for the PJFractions.pas unit. It
 * can be compiled as either a GUI or Console application.
 *
 * Console Application
 * ~~~~~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must be defined to create a console
 * application. From the IDE define the symbol in the conditional defines entry
 * in project options. To compile from the command line pass the -D switch to
 * the compiler:
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B PJFractionTest
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
 *   DCC32 -B -R<path-to-dunit-binaries> PJFractionTest
 *
 * for example:
 *
 *   DCC32 -B -R"C:\Program Files\Embarcadero\RAD Studio\7.0\lib" PJFractionTest
 *
 * -----------------------------------------------------------------------------
 * $Rev$
 * $Date$
 * -----------------------------------------------------------------------------
}

program PJFractionTest;

{$IFDEF CONSOLE_TESTRUNNER}
{$APPTYPE CONSOLE}
{$ENDIF}

uses
  Forms,
  {$IFDEF CONSOLE_TESTRUNNER}
  TextTestRunner,
  {$ELSE}
  GUITestRunner,
  {$ENDIF}
  XPMan,
  TestFramework,
  TestPJFractions in 'TestPJFractions.pas',
  PJFractions in '..\PJFractions.pas';

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

