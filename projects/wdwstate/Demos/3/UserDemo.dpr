{
 * UserDemo.dpr
 *
 * Project file for the Window State Components UserDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2006-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program UserDemo;

uses
  Forms,
  FmUserDemo in 'FmUserDemo.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
