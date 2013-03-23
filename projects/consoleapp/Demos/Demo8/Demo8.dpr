{
 * Demo8.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #8: Echoing console output to a GUI.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo8;

uses
  Forms,
  FmDemo8 in 'FmDemo8.pas' {Form3};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

