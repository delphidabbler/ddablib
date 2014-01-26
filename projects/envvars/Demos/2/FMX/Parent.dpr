{
 * Project file for the Parent application of the DelphiDabbler Environment
 * Variables Unit demo program #2, FireMonkey 2 version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Parent;

uses
  FMX.Forms,
  FmParent in 'FmParent.pas' {ParentForm},
  PJEnvVars in '..\..\..\PJEnvVars.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TParentForm, ParentForm);
  Application.Run;
end.

