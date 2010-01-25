{ 
 * ShellFolderDemo.dpr
 *
 * Project file for Shell Folders Unit demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2003-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}


program ShellFolderDemo;

uses
  Forms,
  ShellFolderDemoForm in 'ShellFolderDemoForm.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Shell Folders Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
