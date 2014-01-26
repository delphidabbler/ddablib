{
 * Project file for Slave application of the DelphiDabbler Environment Variables
 * Unit demo program #2, VCL version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program SlaveApp;

uses
  Forms,
  FmSlave in 'FmSlave.pas' {SlaveForm},
  PJEnvVars in '..\..\..\PJEnvVars.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TSlaveForm, SlaveForm);
  Application.Run;
end.
