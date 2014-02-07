{
 * Project file for DelphiDabbler Message Dialogue Components demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program PJMessageDialogDemo;

{$UNDEF Supports_RTLNameSpaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE Supports_RTLNameSpaces}
  {$IFEND}
{$ENDIF}

uses
  {$IFNDEF Supports_RTLNameSpaces}
  Forms,
  {$ELSE}
  Vcl.Forms,
  {$ENDIF}
  FmPJMessageDialogDemo in 'FmPJMessageDialogDemo.pas' {DemoForm};

{$R Icons.res}
{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Message Dialog Demo';
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
