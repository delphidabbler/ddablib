{ 
 * PJMessageDialogDemo.dpr
 *
 * Project file for demo program that demonstrates use of Message Dialog
 * Components.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2003-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
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
