{
 * Demo3.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #3: Indicating Progress.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
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

