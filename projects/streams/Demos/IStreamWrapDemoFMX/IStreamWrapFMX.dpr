{
 * Project file for IStreamWrapFMX Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program IStreamWrapFMX;

uses
  FMX.Forms,
  FmIStreamWrap in 'FmIStreamWrap.pas' {IStreamWrapForm},
  PJIStreams in '..\..\PJIStreams.pas',
  PJStreamWrapper in '..\..\PJStreamWrapper.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TIStreamWrapForm, IStreamWrapForm);
  Application.Run;
end.
