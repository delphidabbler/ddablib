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
 * To compile a console application from the command line you must ensure the
 * CONSOLE_TESTRUNNER symbol is defined, either by creating an environment
 * variable with the name or by passing the symbol to DCC32 using the -D
 * switch.
 *
 * You also need to pass the path to the DUnit binaries using DCC32's -R switch
 * so that the compiler can find DUnit.
 *
 * Here is an example command line (entered all on one line):
 *
 *   DCC32 -DCONSOLE_TESTRUNNER -B
 *     -R"C:\Program Files (x86)\Embarcadero\RAD Studio\7.0\lib"
 *     PJMD5Test
 *
 * Change the -R path to suit your compile and DUnit installation path.
 *
 * GUI Application
 * ~~~~~~~~~~~~~~~
 *
 * The CONSOLE_TESTRUNNER symbol must not be defined to compile a GUI
 * application. From the IDE make sure the symbol is removed from the
 * conditional defines entry in project options if necessary.
 *
 * The compiler must also be able find the DUnit binaries. Ensure that the path
 * to the binaries is included in the search path specified in project options.
 *
 * 64 bit Application
 * ~~~~~~~~~~~~~~~~~~
 *
 * PJMD5.pas is compatible with the Delphi 64 bit Windows compiler and this test
 * program can also be compiled using that compiler.
 *
 * In the IDE you can create a 64 bit Windows target (Delphi XE2 and later) and
 * compile the program using that. From the command line you will need to
 * substitues DCC64 for DCC32 above.
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
  Forms,
  {$IFDEF CONSOLE_TESTRUNNER}
  TextTestRunner,
  {$ELSE}
  GUITestRunner,
  {$ENDIF}
  TestFramework,
  TestPJMD5 in 'TestPJMD5.pas',
  PJMD5 in '..\PJMD5.pas';

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

