{
 * Demo4.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #4: Timing Out.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo4;

uses
  Forms,
  FmDemo4 in 'FmDemo4.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

