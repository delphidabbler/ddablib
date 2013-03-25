{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #6: Redirecting standard i/o using files.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo6;

uses
  Forms,
  FmDemo6 in 'FmDemo6.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

