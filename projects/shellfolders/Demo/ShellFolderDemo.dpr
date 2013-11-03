{ 
 * ShellFolderDemo.dpr
 *
 * Project file for Shell Folders Unit demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


program ShellFolderDemo;

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
