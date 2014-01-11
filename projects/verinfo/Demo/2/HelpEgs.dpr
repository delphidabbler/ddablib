{
 * HelpEgs.dpr
 *
 * Project file for Version Information Component HelpEgs demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program HelpEgs;

{$UNDEF Supports_RTLNameSpaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE Supports_RTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}

uses
  {$IFDEF Supports_RTLNameSpaces}
  Vcl.Forms,
  {$ELSE}
  Forms,
  {$ENDIF}
  FmMain in 'FmMain.pas' {MainForm},
  FmEg1 in 'FmEg1.pas' {EgForm1},
  FmEg2 in 'FmEg2.pas' {EgForm2},
  FmEg3 in 'FmEg3.pas' {EgForm3},
  FmEg4 in 'FmEg4.pas' {EgForm4};

{$R *.RES}
{$R MultiVer.res}

begin
  Application.Title := 'TPJVersionInfo Help Examples';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEgForm1, EgForm1);
  Application.CreateForm(TEgForm2, EgForm2);
  Application.CreateForm(TEgForm3, EgForm3);
  Application.CreateForm(TEgForm4, EgForm4);
  Application.Run;
end.
