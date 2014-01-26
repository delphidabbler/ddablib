{
 * Project filefor Master application of the DelphiDabbler Environment Variables
 * Unit demo program #2, VCL version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

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
