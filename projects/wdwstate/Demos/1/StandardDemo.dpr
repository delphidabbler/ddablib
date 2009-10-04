{
 * StandardDemo.dpr
 *
 * Project file for Version Information Component StandardDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2005-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program StandardDemo;

uses
  Forms,
  FmDemo in 'FmDemo.pas' {DemoForm},
  FmDemoDlg in 'FmDemoDlg.pas' {DemoDlg};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
