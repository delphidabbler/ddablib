{
 * Project file for IStreamWrap Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2001-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program IStreamWrap;

uses
  Forms,
  FmIStreamWrap in 'FmIStreamWrap.pas' {IStreamWrapForm},
  FmIStreamWrapHelp in 'FmIStreamWrapHelp.pas' {IStreamWrapHelpForm},
  PJIStreams in '..\..\PJIStreams.pas',
  PJStreamWrapper in '..\..\PJStreamWrapper.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIStreamWrapForm, IStreamWrapForm);
  Application.Run;
end.
