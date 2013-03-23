{
 * Demo9.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #9: Subclassing TPJConsoleApp.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo9;

uses
  Forms,
  FmDemo9 in 'FmDemo9.pas' {Form1},
  UConsoleAppEx in 'UConsoleAppEx.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.

