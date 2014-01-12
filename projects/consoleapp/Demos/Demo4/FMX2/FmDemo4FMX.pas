{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #4: Timing Out (FireMonkey 2 version).
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo4FMX;

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
  FMX.Controls,
  FMX.Forms,
  FMX.Memo,
  FMX.Edit,
  FMX.Layouts,
  System.Classes,
  {$IFDEF REQUIRES_FMX_STDCTRLS}
  FMX.StdCtrls,
  {$ENDIF}
  FMX.Types;

type
  TDemo4FMXForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Label2: TLabel;
    Edit1: TEdit;
    CheckBox1: TCheckBox;
    procedure Button1Click(Sender: TObject);
  private
    procedure WorkHandler(Sender: TObject);
  end;

var
  Demo4FMXForm: TDemo4FMXForm;

implementation

uses
  System.SysUtils,
  PJConsoleApp;

{$R *.FMX}

procedure TDemo4FMXForm.Button1Click(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  App := TPJConsoleApp.Create;
  try
    App.MaxExecTime := StrToInt(Edit1.Text);
    App.KillTimedOutProcess := Checkbox1.IsChecked;
    App.TimeSlice := 200;
    App.Visible := True;
    App.OnWork := WorkHandler;
    if App.Execute('Timed 5') then
      Memo1.Lines.Add('Application completed normally')
    else
      Memo1.Lines.Add(App.ErrorMessage)
  finally
    App.Free;
  end;
end;

procedure TDemo4FMXForm.WorkHandler(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  App := Sender as TPJConsoleApp;
  Memo1.Lines.Add(
    Format(
      'Elapsed time: %dms, Time to live: %dms',
      [App.ElapsedTime, App.TimeToLive]
    )
  );
  Application.ProcessMessages;  // keeps GUI interactive
end;

end.

