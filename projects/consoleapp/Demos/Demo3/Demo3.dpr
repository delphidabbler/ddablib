{
 * Demo3.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #3: Indicating Progress.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Demo3;

uses
  Forms,
  FmDemo3 in 'FmDemo3.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

