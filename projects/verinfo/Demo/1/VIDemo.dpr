{
 * VIDemo.dpr
 *
 * Project file for Version Information Component VIDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2002-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
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
