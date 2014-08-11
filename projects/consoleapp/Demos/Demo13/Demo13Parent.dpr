{
 * Project file for the parent process used in the DelphiDabbler Console
 * Application Runner Classes demo program #13: Customising a console app's
 * environment block.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Demo13Parent;

uses
  Forms,
  FmDemo13Parent in 'FmDemo13Parent.pas' {ParentForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TParentForm, ParentForm);
  Application.Run;
end.

