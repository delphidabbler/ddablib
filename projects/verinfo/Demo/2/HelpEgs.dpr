{
 * HelpEgs.dpr
 *
 * Project file for Version Information Component HelpEgs demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2002-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program HelpEgs;

uses
  Forms,
  FmMain in 'FmMain.pas' {MainForm},
  FmEg1 in 'FmEg1.pas' {EgForm1},
  FmEg2 in 'FmEg2.pas' {EgForm2},
  FmEg3 in 'FmEg3.pas' {EgForm3},
  FmEg4 in 'FmEg4.pas' {EgForm4};

{$R *.RES}
{$R MultiVer.res}

begin
  Application.Title := 'TPJVersionInfo Help Examples';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TEgForm1, EgForm1);
  Application.CreateForm(TEgForm2, EgForm2);
  Application.CreateForm(TEgForm3, EgForm3);
  Application.CreateForm(TEgForm4, EgForm4);
  Application.Run;
end.
