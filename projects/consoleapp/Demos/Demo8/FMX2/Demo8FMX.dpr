{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #8: Echoing console output to a GUI (FireMonkey 2 version).
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo8FMX;

uses
  FMX.Forms,
  FmDemo8FMX in 'FmDemo8FMX.pas' {Demo8FMXForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDemo8FMXForm, Demo8FMXForm);
  Application.Run;
end.

