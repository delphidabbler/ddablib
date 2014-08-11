{
 * Project file for the child console app process used in the DelphiDabbler
 * Console Application Runner Classes demo program #13: Customising a console
 * app's environment block.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo13Child;

{$APPTYPE CONSOLE}

uses
  Classes,
  UEnvVars in 'UEnvVars.pas';

{$R *.res}

var
  EnvVars: TStrings;
  I: Integer;
begin
  // We simply get all environment variables stored in the app's environment
  // block and write each one, with its value, to the console.
  EnvVars := TStringList.Create;
  try
    GetAllEnvVars(EnvVars);
    for I := 0 to Pred(EnvVars.Count) do
    begin
      WriteLn(EnvVars[I]);
    end;
    WriteLn;
    Write('Press enter to end');
    ReadLn;
  finally
    EnvVars.Free;
  end;
end.

