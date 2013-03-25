{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #5: Terminating an Application.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo5;

uses
  Forms,
  FmDemo5 in 'FmDemo5.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

