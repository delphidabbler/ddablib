{
 * Project file for DelphiDabbler Environment Variables Unit demo program #1,
 * FireMonkey 2 version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program FMXDemo1;

uses
  FMX.Forms,
  FmFMXDemo1 in 'FmFMXDemo1.pas' {FMXDemo1Form},
  PJEnvVars in '..\..\..\PJEnvVars.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFMXDemo1Form, FMXDemo1Form);
  Application.Run;
end.
