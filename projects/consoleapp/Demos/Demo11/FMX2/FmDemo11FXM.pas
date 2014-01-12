{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #11: Customising the appearance of the console (FireMonkey 2 version).
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo11FXM;

{$UNDEF REQUIRES_FMX_STDCTRLS}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 25.0} // Delphi XE4 and later
    {$DEFINE REQUIRES_FMX_STDCTRLS}
  {$IFEND}
{$ENDIF}

interface

uses
  System.Classes, 
  FMX.Types, 
  FMX.ListBox, 
  FMX.Controls, 
  {$IFDEF REQUIRES_FMX_STDCTRLS}
  FMX.StdCtrls,
  {$ENDIF}
  FMX.Colors, 
  FMX.Forms,
  FMX.Edit;

type
  TDemo11FMXForm = class(TForm)
    btnRun: TButton;
    gbConsoleColours: TGroupBox;
    lblForeground: TLabel;
    lblBackground: TLabel;
    lblTitle: TLabel;
    edTitle: TEdit;
    gbWindowSize: TGroupBox;
    lblWindowWidth: TLabel;
    lblWindowHeight: TLabel;
    cbDefWindowSize: TCheckBox;
    edWindowWidth: TEdit;
    edWindowHeight: TEdit;
    gbWindowPos: TGroupBox;
    lblWindowLeft: TLabel;
    lblWindowTop: TLabel;
    cbDefWindowPos: TCheckBox;
    edWindowLeft: TEdit;
    edWindowTop: TEdit;
    cbForeground: TColorComboBox;
    cbBackground: TColorComboBox;
    procedure btnRunClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure EdNumberFilter(Sender: TObject; var Key: Char);
    procedure cbDefWindowSizeChange(Sender: TObject);
    procedure cbDefWindowPosChange(Sender: TObject);
  private
    procedure WorkHandler(Sender: TObject);
  end;


var
  Demo11FMXForm: TDemo11FMXForm;


implementation


uses
  System.SysUtils, System.Types,
  PJConsoleApp;


{$R *.FMX}

{ TDemo11FMXForm }

procedure TDemo11FMXForm.btnRunClick(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  btnRun.Enabled := False;
  try
    App := TPJConsoleApp.Create;
    try
      App.OnWork := WorkHandler;
      App.Visible := True;
      App.ConsoleTitle := edTitle.Text;
      if not cbDefWindowSize.IsChecked then
        App.WindowSize := MakeSize(
          StrToInt(edWindowWidth.Text), StrToInt(edWindowHeight.Text)
        );
      if not cbDefWindowPos.IsChecked then
        App.WindowPosition := Point(
          StrToInt(edWindowLeft.Text), StrToInt(edWindowTop.Text)
        );
      App.ConsoleColors := MakeConsoleColors(
        cbForeground.Color, cbBackground.Color
      );
      if not App.Execute('Timed 2') then
        raise Exception.CreateFmt(
          'Can''t execute program: error %d - "%s"',
          [App.ErrorCode, App.ErrorMessage]
        );
    finally
      App.Free;
    end;
  finally
    btnRun.Enabled := True;
  end;
end;

procedure TDemo11FMXForm.cbDefWindowPosChange(Sender: TObject);
begin
  lblWindowLeft.Enabled := not cbDefWindowPos.IsChecked;
  lblWindowTop.Enabled := not cbDefWindowPos.IsChecked;
  edWindowLeft.Enabled := not cbDefWindowPos.IsChecked;
  edWindowTop.Enabled := not cbDefWindowPos.IsChecked;
end;

procedure TDemo11FMXForm.cbDefWindowSizeChange(Sender: TObject);
begin
  lblWindowWidth.Enabled := not cbDefWindowSize.IsChecked;
  lblWindowHeight.Enabled := not cbDefWindowSize.IsChecked;
  edWindowWidth.Enabled := not cbDefWindowSize.IsChecked;
  edWindowHeight.Enabled := not cbDefWindowSize.IsChecked;
end;

procedure TDemo11FMXForm.EdNumberFilter(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, ['0'..'9', #8]) then
    Key := #0;
end;

procedure TDemo11FMXForm.FormCreate(Sender: TObject);
begin
  edTitle.Text := Caption;
end;

procedure TDemo11FMXForm.WorkHandler(Sender: TObject);
begin
  // makes GUI app response while console app is running
  Application.ProcessMessages;
end;

end.

