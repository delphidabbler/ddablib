{
 * Main form for the Child application of the DelphiDabbler Environment
 * Variables Unit demo program #2, FireMonkey 2 version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmChild;

interface

uses
  System.Classes,
  FMX.Types,
  FMX.Controls,
  FMX.Layouts,
  FMX.Memo,
  FMX.Forms;

type
  TChildForm = class(TForm)
    edEnvVars: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

var
  ChildForm: TChildForm;

implementation

uses
  PJEnvVars;

{$R *.FMX}

procedure TChildForm.FormCreate(Sender: TObject);
begin
  TPJEnvironmentVars.GetAll(edEnvVars.Lines);
end;

end.

