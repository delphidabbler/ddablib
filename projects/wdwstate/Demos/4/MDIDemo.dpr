{
 * MDIDemo.pas
 *
 * Project file for the Window State Components MDIChild demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program MDIDemo;

uses
  Forms,
  FmMDIMain in 'FmMDIMain.pas' {Form1},
  FmMDIChild in 'FmMDIChild.pas' {Form2};

{$R *.res}     

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.CreateForm(TForm2, Form2);
  Application.Run;
end.

