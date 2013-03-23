{
 * Demo11.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #12: Handling Unicode output from console applications.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo12;

uses
  Forms,
  FmDemo12 in 'FmDemo12.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
