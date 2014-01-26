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

