{
 * Demo7.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #7: Redirecting Standard I/O using Pipes.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Demo7;

uses
  Forms,
  FmDemo7 in 'FmDemo7.pas' {Form2},
  PJPipe in '..\..\PJPipe.pas',
  PJConsoleApp in '..\..\PJConsoleApp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

