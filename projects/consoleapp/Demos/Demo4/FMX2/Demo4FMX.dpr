{
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #4: Timing Out (FireMonkey 2 version).
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo4FMX;

uses
  FMX.Forms,
  FmDemo4FMX in 'FmDemo4FMX.pas' {Demo4FMXForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDemo4FMXForm, Demo4FMXForm);
  Application.Run;
end.

