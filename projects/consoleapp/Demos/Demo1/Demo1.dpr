{
 * Demo1.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #1: Example of ExecAndWait.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Demo1;

uses
  Forms,
  FmDemo1 in 'FmDemo1.pas' {Form1},
  PJConsoleApp in '..\..\PJConsoleApp.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

