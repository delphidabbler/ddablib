{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #1: ExecAndWait.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo1;

uses
  Forms,
  FmDemo1 in 'FmDemo1.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

