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

{ ##
  @FILE                     HelpEgs.dpr
  @COMMENTS                 Project file for application that implements the
                            examples provided in the TPJVersionInfo component
                            help file.
  @PROJECT_NAME             Version Information Component
  @PROJECT_DESC             Component that reads version information from files.
  @AUTHOR                   Peter Johnson, LLANARTH, Ceredigion, Wales, UK
  @EMAIL                    peter.johnson@openlink.org
  @WEBSITE                  http://www.delphidabbler.com/
  @COPYRIGHT                © 2002, Peter D Johnson.
  @LEGAL_NOTICE             This demo program is placed in the public domain. It
                            may be freely copied and circulated on a not for
                            profit basis providing that the code is unmodified
                            and this notice and information about the author and
                            his copyright remains attached to the source code.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 17/02/2002
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 10/12/2005
      @COMMENTS             Mofified application title
    )
  )
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
