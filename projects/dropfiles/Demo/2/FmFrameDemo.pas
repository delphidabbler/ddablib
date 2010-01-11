{ ##
  @PROJECT_NAME             FrameDemo
  @PROJECT_DESC             Demonstrates use of Drop Files components and
                            frames.
  @FILE                     FmFrameDemo.pas
  @COMMENTS                 Main form containing frames.
  @LEGAL_NOTICE             This demo program is distributed under the Mozilla
                            Public License - see below for details.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 19/03/2006
      @COMMENTS             Original version.
    )
  )
}


{
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
 * The Original Code is FmFrameDemo.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2006 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit FmFrameDemo;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, FrDemo, ExtCtrls, StdCtrls, PJDropFiles;

type
  TFmMain = class(TForm)
    Panel1: TPanel;
    frmLeft: TFrame1;
    frmRight: TFrame1;
    Panel2: TPanel;
    Label1: TLabel;
    Memo1: TMemo;
    PJFormDropFiles1: TPJFormDropFiles;
    Panel3: TPanel;
    procedure PJFormDropFiles1DropFiles(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  end;

var
  FmMain: TFmMain;

implementation

{$R *.dfm}

procedure TFmMain.PJFormDropFiles1DropFiles(Sender: TObject);
var
  Ctrl: TControl;
  ParentName: string;
  CtrlName: string;
  I: Integer;
  Marker: string;
begin
  // Get either frame or form on which drop happens
  Ctrl := PJFormDropFiles1.DropControl;
  CtrlName := Ctrl.Name;
  while Assigned(Ctrl) and not (Ctrl is TFrame) do
    Ctrl := Ctrl.Parent;
  if Assigned(Ctrl) then
    ParentName := Ctrl.Name + '.'
  else
    ParentName := '';
  Panel3.Caption := 'DropControl: ' + ParentName + CtrlName;
  Memo1.Clear;
  for I := 0 to Pred(PJFormDropFiles1.Count) do
  begin
    if PJFormDropFiles1.IsFolder[I] then
      Marker := 'Folder: '
    else
      Marker := 'File:   ';
    Memo1.Lines.Add(Marker + PJFormDropFiles1.Files[I]);
  end;
end;

procedure TFmMain.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

end.
