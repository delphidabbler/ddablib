{
 * FmFrameDemo.pas
 *
 * Main form file for demo program that demonstrates use of Drop Files
 * Components with frames.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


unit FmFrameDemo;

interface

{$UNDEF DELPHIXE2ANDUP}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 23.0} // Delphi XE2
    {$DEFINE DELPHIXE2ANDUP}
  {$IFEND}
{$ENDIF}

uses
  {$IFNDEF DELPHIXE2ANDUP}
  Classes, StdCtrls, Controls, Forms, ExtCtrls,
  {$ELSE}
  System.Classes, Vcl.StdCtrls, Vcl.Controls, Vcl.Forms, Vcl.ExtCtrls,
  {$ENDIF}
  PJDropFiles, FrDemo;

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
