{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #11: Customising the appearance of the console.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
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
