{
 * Demo11.dpr
 *
 * Project file for DelphiDabbler Console Application Runner Classes demo
 * program #11: Customising the appearance of the console (FireMonkey 2
 * version).
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo11FMX;

uses
  FMX.Forms,
  FmDemo11FXM in 'FmDemo11FXM.pas' {Demo11FMXForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDemo11FMXForm, Demo11FMXForm);
  Application.Run;
end.
