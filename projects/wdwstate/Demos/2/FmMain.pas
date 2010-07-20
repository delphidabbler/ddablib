{
 * FmMain.pas
 *
 * Main form for the Window State Components StandAloneDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2005-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmMain;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, PJWdwState, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    CheckBox1: TCheckBox;
    Memo1: TMemo;
    procedure FormCreate(Sender: TObject);
    procedure CheckBox1Click(Sender: TObject);
  private
    fWdwState: TPJWdwState;
    function IsInWorkArea: Boolean;
    procedure SetInWorkArea(Flag: Boolean);
  end;

var
  Form1: TForm1;

implementation

uses
  IniFiles;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  Memo1.WordWrap := True;
  
  // Create stand-alone window state component
  // DO NOT use TPJWdwState.Create() constructor for this
  // Can use TPJRegWdwState instead of TPJWdwState here
  fWdwState := TPJWdwState.CreateStandAlone(Self);

  // Set up component properties: all have default values

  // we'll specify ini file and section in it
  // we will also use the same ini file to store whether we want to keep
  // window in work space: see IsInWorkArea and SetInWorkArea methods below
  fWdwState.IniFileName := ExtractFilePath(ParamStr(0)) + 'Custom.ini';
  fWdwState.Section := 'Window';

  // we want component to work automatically
  fWdwState.AutoSaveRestore := True;

  // we may want component to keep window in work area: depends on ini file
  // setting
  if IsInWorkArea then
  begin
    // We keep in work area if recorded in ini file
    fWdwState.Options := [woFitWorkArea];
    CheckBox1.Checked := True;
  end;
end;

procedure TForm1.CheckBox1Click(Sender: TObject);
begin
  // Record value in ini file for future reference
  SetInWorkArea(CheckBox1.Checked);
end;

function TForm1.IsInWorkArea: Boolean;
  {This method only used to check whether component should be set to keep in
  work area on next run: uses same ini file as window state component but uses
  [Options] section.}
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(fWdwState.IniFileName);
  try
    Result := Ini.ReadBool('Options', 'WorkArea', False);
  finally
    Ini.Free;
  end;
end;

procedure TForm1.SetInWorkArea(Flag: Boolean);
  {This method only used to record whether component should be set to keep in
  work area on next run: uses same ini file as window state component but uses
  [Options] section.}
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(fWdwState.IniFileName);
  try
    Ini.WriteBool('Options', 'WorkArea', Flag);
  finally
    Ini.Free;
  end;
end;

end.
