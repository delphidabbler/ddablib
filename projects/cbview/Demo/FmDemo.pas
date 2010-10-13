unit FmDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls, PJCBView;

type
  TDemoForm = class(TForm)
    PJCBViewer1: TPJCBViewer;
    Button1: TButton;
    Memo1: TMemo;
    procedure Button1Click(Sender: TObject);
    procedure PJCBViewer1ClipboardChanged(Sender: TObject);
  end;

var
  DemoForm: TDemoForm;

implementation

uses
  Clipbrd;

{$R *.dfm}

procedure TDemoForm.Button1Click(Sender: TObject);
begin
  // We assume this button can only be clicked when there is text on clipboard,
  // so it is safe to display the text
  Memo1.Text := ClipBoard.AsText;
end;

procedure TDemoForm.PJCBViewer1ClipboardChanged(Sender: TObject);
begin
  // Clipboard has changed - emit beep and decide whether to enable button
  // (only if text is on clipboard)
  MessageBeep(0);
  Button1.Enabled := ClipBoard.HasFormat(CF_TEXT);
end;

end.
