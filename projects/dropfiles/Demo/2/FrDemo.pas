{ 
 * FrDemo.pas
 *
 * Implements frame that catches dropped files for demo program that
 * demonstrates use of Drop Files Components with frames.
 *
 * $Rev$
 * $Date$
 *
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is FrDemo.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
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
