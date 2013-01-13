{
 * VIDemo.dpr
 *
 * Project file for Version Information Component VIDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program VIDemo;

uses
  Forms,
  FmVIDemo in 'FmVIDemo.pas' {MainForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'Version Info Component Demo';
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
