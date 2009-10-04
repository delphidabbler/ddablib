{
 * Project file for StreamWrap Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2001-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program StreamWrap;

uses
  Forms,
  FmStreamWrap in 'FmStreamWrap.pas' {StreamWrapForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TStreamWrapForm, StreamWrapForm);
  Application.Run;
end.
