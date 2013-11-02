{ 
 * FrDemo.pas
 *
 * Implements frame that catches dropped files for demo program that
 * demonstrates use of Drop Files Components with frames.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


unit FrDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, PJDropFiles, ExtCtrls, StdCtrls;

type
  TFrame1 = class(TFrame)
    Label1: TLabel;
    Memo1: TMemo;
    PJCtrlDropFiles1: TPJCtrlDropFiles;
    Panel1: TPanel;
    Label2: TLabel;
    Edit1: TEdit;
    PJExtFileFilter1: TPJExtFileFilter;
    procedure PJCtrlDropFiles1DropFiles(Sender: TObject);
    procedure Edit1Change(Sender: TObject);
  end;

implementation

{$R *.dfm}

procedure TFrame1.PJCtrlDropFiles1DropFiles(Sender: TObject);
var
  I: Integer;
  FN: string;
  S: string;
begin
  Memo1.Clear;
  for I := 0 to Pred(PJCtrlDropFiles1.Count) do
  begin
    FN := ExtractFileName(PJCtrlDropFiles1.Files[I]);
    if PJCtrlDropFiles1.IsFolder[I] then
      S := '[' + FN + ']'
    else
      S := FN;
    Memo1.Lines.Add(S);
  end;
end;

procedure TFrame1.Edit1Change(Sender: TObject);
begin
  PJExtFileFilter1.Extensions := Edit1.Text;
end;

end.
