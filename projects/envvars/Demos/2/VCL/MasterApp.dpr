program MasterApp;

uses
  Forms,
  FmMaster in 'FmMaster.pas' {MasterForm},
  PJEnvVars in '..\..\..\PJEnvVars.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TMasterForm, MasterForm);
  Application.Run;
end.
