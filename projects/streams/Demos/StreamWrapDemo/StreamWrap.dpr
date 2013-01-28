{
 * Project file for StreamWrap Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program StreamWrap;

uses
  Forms,
  FmStreamWrap in 'FmStreamWrap.pas' {StreamWrapForm},
  PJStreamWrapper in '..\..\PJStreamWrapper.pas';

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TStreamWrapForm, StreamWrapForm);
  Application.Run;
end.
