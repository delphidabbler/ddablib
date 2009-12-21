 {
 * Main form for About Box Component demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2005-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
 }


program AboutBoxDemo;

uses
  Forms,
  FmDemo in 'FmDemo.pas' {Form1};
      
{$R *.res}
{$R VerInfo.res}

begin                       
  Application.Initialize;
  Application.Title := 'About Box Demo';
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
