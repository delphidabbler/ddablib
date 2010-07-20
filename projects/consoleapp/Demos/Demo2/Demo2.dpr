{
 * Demo2.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #2: ExecAndWait that lets a GUI remain interactive.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Demo2;

uses
  Forms,
  FmDemo2 in 'FmDemo2.pas' {Form1},
  PJConsoleApp in '..\..\PJConsoleApp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

