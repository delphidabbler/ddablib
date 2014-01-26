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
