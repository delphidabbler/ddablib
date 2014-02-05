{
 * Main form file for DelphiDabbler Clipboard Viewer Component demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo;

{$UNDEF RTLNAMESPACES}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNAMESPACES}
  {$IFEND}
{$ENDIF}

interface

uses
  {$IFNDEF RTLNAMESPACES}
  Classes,
  Controls,
  StdCtrls,
  Forms,
  {$ELSE}
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms,
  {$ENDIF}
  PJCBView;

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
  {$IFNDEF RTLNAMESPACES}
  Windows,
  Clipbrd;
  {$ELSE}
  Winapi.Windows,
  Vcl.Clipbrd;
  {$ENDIF}

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
