{
 * VIDemo.dpr
 *
 * Project file for Version Information Component VIDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program VIDemo;

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
  FmVIDemo in 'FmVIDemo.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Version Info Component Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
