{
 * FmDemo.pas
 *
 * Main form for Version Information Component StandardDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2005-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  PJWdwState, StdCtrls, ExtCtrls, Registry;

type
  TDemoForm = class(TForm)
    btnShowDlg: TButton;
    PJRegWdwState1: TPJRegWdwState;
    Label1: TLabel;
    Panel1: TPanel;
    Memo1: TMemo;
    Splitter1: TSplitter;
    Memo2: TMemo;
    procedure btnShowDlgClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure PJRegWdwState1ReadWdwState(Sender: TObject; var Left, Top,
      Width, Height, State: Integer);
    procedure PJRegWdwState1GetRegData(var RootKey: HKEY;
      var SubKey: String);
    procedure PJRegWdwState1GettingRegData(const Reg: TRegistry);
    procedure PJRegWdwState1PuttingRegData(const Reg: TRegistry);
  end;

var
  DemoForm: TDemoForm;

implementation

uses FmDemoDlg;

{$R *.DFM}

{
  The TPJRegWdwState component is set up with the following non-default property
  values:
    AutoSaveRestore = True
    OnReadWdwState = PJRegWdwState1ReadWdwState
    OnGetRegData = PJRegWdwState1GetRegData
    OnGettingRegData = PJRegWdwState1GettingRegData
    OnPuttingRegData = PJRegWdwState1PuttingRegData
}

procedure TDemoForm.btnShowDlgClick(Sender: TObject);
  {Show the dialog modally}
begin
  // We show dialog modally.
  with TDemoDlg.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TDemoForm.FormCreate(Sender: TObject);
begin
  // Just to get info displayed in memo to display properly
  Memo1.WordWrap := True;
  Memo2.WordWrap := True;
end;

procedure TDemoForm.PJRegWdwState1ReadWdwState(Sender: TObject; var Left,
  Top, Width, Height, State: Integer);
begin
  // Display size and position info
  Label1.Caption := Format(
    'Left=%d, Top=%d, Width=%d, Height=%d',
    [Left, Top, Width, Height]
  )
  // We can override any of the size, position or state values read from
  // registry if needed. Note tho that if woIgnoreState or woIgnoreSize Options
  // are set state and/or size will be ignored anyway. We don't do this in this
  // demo however.
end;

procedure TDemoForm.PJRegWdwState1GetRegData(var RootKey: HKEY;
  var SubKey: String);
begin
  // Use following registry key for window data
  // if we know key at design time we can set TPJRegWdwState.SubKey property
  // instead, but we sometimes don't know key until run time, so we can also
  // handle this event
  SubKey := 'Software\DelphiDabbler\Demos\WindowState\Main';
end;

procedure TDemoForm.PJRegWdwState1GettingRegData(const Reg: TRegistry);
begin
  // This event is triggered just after the TPJRegWdwState component has read
  // the window state info from the registry. The event makes the TRegistry
  // object used by the component to read data available to the application.
  // Here we read the width of the left hand memo control from the registry.
  // WARNING: You shouldn't write data here.
  if Reg.ValueExists('MemoWidth') then
    Memo1.Width := Reg.ReadInteger('MemoWidth');
end;

procedure TDemoForm.PJRegWdwState1PuttingRegData(const Reg: TRegistry);
begin
  // This event is triggered just after the TPJRegWdwState component has written
  // the window state info to the registry. The event makes the TRegistry object
  // used by the component to write data available to the application.
  // Here we write the width of the left hand memo control to the registry.
  // WARNING: You shouldn't write data with names Height, Left, State, Top or
  // Width here - these are used by the component.
  Reg.WriteInteger('MemoWidth', Memo1.Width);
end;

end.
