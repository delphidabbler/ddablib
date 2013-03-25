{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #7: Redirecting standard i/o using pipes.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo7;

uses
  Forms,
  FmDemo7 in 'FmDemo7.pas' {Form2};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

