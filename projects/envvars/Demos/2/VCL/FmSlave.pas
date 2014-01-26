{
 * Main form for Slave application of the DelphiDabbler Environment Variables
 * Unit demo program #2, VCL version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmSlave;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TSlaveForm = class(TForm)
    edEnvVars: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

var
  SlaveForm: TSlaveForm;

implementation

uses
  PJEnvVars;

{$R *.DFM}

procedure TSlaveForm.FormCreate(Sender: TObject);
begin
  TPJEnvironmentVars.GetAll(edEnvVars.Lines);
end;

end.

