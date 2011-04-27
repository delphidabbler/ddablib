{
 * Demo11.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #11: Customising the console window.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2011.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Demo11;

uses
  Forms,
  FmDemo11 in 'FmDemo11.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
