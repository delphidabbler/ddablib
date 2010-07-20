{
 * Demo8.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #8: Echoing Console Output to a GUI.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Demo8;

uses
  Forms,
  FmDemo8 in 'FmDemo8.pas' {Form3},
  PJPipe in '..\..\PJPipe.pas',
  PJConsoleApp in '..\..\PJConsoleApp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

