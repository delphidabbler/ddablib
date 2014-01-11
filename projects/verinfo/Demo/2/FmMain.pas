{
 * FmMain.pas
 *
 * Main form for Version Information Component HelpEgs demo program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmMain;

{$UNDEF Supports_RTLNameSpaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE Supports_RTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}

interface

uses
  // Delphi
  {$IFNDEF Supports_RTLNameSpaces}
  Forms, Classes, Controls, StdCtrls;
  {$ELSE}
  Vcl.Forms, System.Classes, Vcl.Controls, Vcl.StdCtrls;
  {$ENDIF}

type
  TMainForm = class(TForm)
    btnEg1: TButton;
    btnEg2: TButton;
    btnEg3: TButton;
    btnEg4: TButton;
    lblDesc: TLabel;
    procedure btnEg1Click(Sender: TObject);
    procedure btnEg2Click(Sender: TObject);
    procedure btnEg3Click(Sender: TObject);
    procedure btnEg4Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

uses
  // Delphi
  {$IFDEF Supports_RTLNameSpaces}
  Winapi.Windows,
  {$ELSE}
  Windows,
  {$ENDIF}
  // Project
  FmEg1, FmEg2, FmEg3, FmEg4;

{$R *.DFM}

procedure TMainForm.btnEg1Click(Sender: TObject);
  // Display example 1 dialog box
begin
  EgForm1.ShowModal;
end;

procedure TMainForm.btnEg2Click(Sender: TObject);
  // Display example 2 dialog box
begin
  EgForm2.ShowModal;
end;

procedure TMainForm.btnEg3Click(Sender: TObject);
  // Display example 3 dialog box
begin
  EgForm3.ShowModal;
end;

procedure TMainForm.btnEg4Click(Sender: TObject);
  // Display example 4 dialog box
begin
  EgForm4.ShowModal;
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  Caption := Application.Title;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
begin
  WinHelp(Handle, 'PJVersionInfo.hlp', HELP_QUIT, 0);
end;

end.
