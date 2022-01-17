{
 * ShellFolderDemo.dpr
 *
 * Project file for Shell Folders Unit demo program.
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


program ShellFolderDemo;

// Minimum compiler for this project is Delphi 7.
{$UNDEF RTLNameSpaces}
{$IF CompilerVersion >= 24.0} // Delphi XE3 and later
  {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
{$IFEND}
{$IF CompilerVersion >= 23.0} // Delphi XE2 and later
  {$DEFINE RTLNameSpaces}
{$IFEND}

uses
  {$IFNDEF RTLNameSpaces}
  Forms,
  {$ELSE}
  Vcl.Forms,
  {$ENDIF}
  ShellFolderDemoForm in 'ShellFolderDemoForm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Shell Folders Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
