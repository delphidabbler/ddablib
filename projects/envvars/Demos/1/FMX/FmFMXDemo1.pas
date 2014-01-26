{
 * Main form for DelphiDabbler Environment Variables Unit demo program #1,
 * FireMonkey 2 version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmFMXDemo1;

{$UNDEF Requires_FMX_StdCtrls}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 25.0} // Delphi XE4 and later
    {$DEFINE Requires_FMX_StdCtrls}
  {$IFEND}
{$ENDIF}

interface

uses
  System.Classes,
  FMX.Controls,
  FMX.Layouts,
  FMX.ListBox,
  FMX.Edit,
  FMX.Forms,
  FMX.Types,
  {$IFDEF Requires_FMX_StdCtrls}
  FMX.StdCtrls,
  {$ENDIF}

  PJEnvVars;

type
  TFMXDemo1Form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edName: TEdit;
    edValue: TEdit;
    btnGetValue: TButton;
    btnSetValue: TButton;
    btnExists: TButton;
    btnDelete: TButton;
    btnCount: TButton;
    btnBlockSize: TButton;
    btnExpand: TButton;
    lbEnvVars: TListBox;
    Panel1: TPanel;
    lbNewEnv: TListBox;
    btnCreateBlock: TButton;
    Label3: TLabel;
    chkIncludeCurrent: TCheckBox;
    Panel2: TPanel;
    btnGetAllNames1: TButton;
    btnGetAllNames2: TButton;
    btnEnumNames1: TButton;
    btnEnumNames2: TButton;
    btnGetAll1: TButton;
    btnGetAll2: TButton;
    btnEnumVars1: TButton;
    btnEnumVars2: TButton;
    procedure btnGetValueClick(Sender: TObject);
    procedure btnSetValueClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnExistsClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnBlockSizeClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnGetAllNames1Click(Sender: TObject);
    procedure btnGetAll1Click(Sender: TObject);
    procedure btnGetAllNames2Click(Sender: TObject);
    procedure btnGetAll2Click(Sender: TObject);
    procedure btnEnumNames1Click(Sender: TObject);
    procedure btnEnumVars1Click(Sender: TObject);
    procedure btnEnumNames2Click(Sender: TObject);
    procedure btnEnumVars2Click(Sender: TObject);
    procedure btnCreateBlockClick(Sender: TObject);
  private
    procedure EnvNameCallback(const Name: string; Data: Pointer);
    procedure EnvVarCallback(const EnvVar: TPJEnvironmentVar; Data: Pointer);
  end;

var
  FMXDemo1Form: TFMXDemo1Form;

implementation

uses
  System.SysUtils,
  System.Types,
  FMX.Dialogs;

{$R *.fmx}

// Routine adapted from one with same name Code Snippets database at
// http://snippets.delphidabbler.com/
procedure MultiSzToStrings(const MultiSz: PChar; const Strings: TStrings);
var
  P: PChar;
begin
  if not Assigned(MultiSz) then
    Exit;
  P := MultiSz;
  while P^ <> #0 do
  begin
    Strings.Add(P);
    Inc(P, StrLen(P) + 1);
  end;
end;

{ TForm4 }

procedure TFMXDemo1Form.btnBlockSizeClick(Sender: TObject);
begin
  ShowMessageFmt('Block size = %d', [TPJEnvironmentVars.BlockSize]);
end;

procedure TFMXDemo1Form.btnCountClick(Sender: TObject);
begin
  ShowMessageFmt('Count = %d', [TPJEnvironmentVars.Count]);
end;

procedure TFMXDemo1Form.btnCreateBlockClick(Sender: TObject);
var
  BlockSize: Integer;
  Block: Pointer;
begin
  BlockSize := TPJEnvironmentVars.CreateBlock(
    lbNewEnv.Items, chkIncludeCurrent.IsChecked, nil, 0
  );
  GetMem(Block, BlockSize * SizeOf(Char));
  try
    TPJEnvironmentVars.CreateBlock(
      lbNewEnv.Items, chkIncludeCurrent.IsChecked, Block, BlockSize
    );
    lbEnvVars.Clear;
    MultiSzToStrings(PChar(Block), lbEnvVars.Items);
  finally
    FreeMem(Block, BlockSize * SizeOf(Char));
  end;
end;

procedure TFMXDemo1Form.btnDeleteClick(Sender: TObject);
begin
  CheckOSError(TPJEnvironmentVars.Delete(edName.Text));
end;

procedure TFMXDemo1Form.btnEnumNames1Click(Sender: TObject);
begin
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumNames(EnvNameCallback, Pointer(lbEnvVars.Items));
end;

procedure TFMXDemo1Form.btnEnumNames2Click(Sender: TObject);
begin
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumNames(
    procedure (const Name: string; Data: Pointer)
    begin
      lbEnvVars.Items.Add(Format('<%s>', [Name]));
    end,
    nil
  );
end;

procedure TFMXDemo1Form.btnEnumVars1Click(Sender: TObject);
begin
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumVars(EnvVarCallback, Pointer(lbEnvVars.Items));
end;

procedure TFMXDemo1Form.btnEnumVars2Click(Sender: TObject);
begin
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumVars(
    procedure (const EnvVar: TPJEnvironmentVar; Data: Pointer)
    begin
      lbEnvVars.Items.Add(Format('<%s> = "%s"', [EnvVar.Name, EnvVar.Value]));
    end,
    nil
  );
end;

procedure TFMXDemo1Form.btnExistsClick(Sender: TObject);
begin
  if TPJEnvironmentVars.Exists(edName.Text) then
    ShowMessageFmt('"%s" exists', [edName.Text])
  else
    ShowMessageFmt('"%s" does not exist', [edName.Text]);
end;

procedure TFMXDemo1Form.btnExpandClick(Sender: TObject);
begin
  edValue.Text := TPJEnvironmentVars.Expand(edName.Text);
end;

procedure TFMXDemo1Form.btnGetAll1Click(Sender: TObject);
begin
  TPJEnvironmentVars.GetAll(lbEnvVars.Items);
end;

procedure TFMXDemo1Form.btnGetAll2Click(Sender: TObject);
var
  EnvVars: TPJEnvironmentVarArray;
  EnvVar: TPJEnvironmentVar;
begin
  EnvVars := TPJEnvironmentVars.GetAll;
  lbEnvVars.Clear;
  for EnvVar in EnvVars do
    lbEnvVars.Items.Add(Format('[%s] = "%s"', [EnvVar.Name, EnvVar.Value]));
end;

procedure TFMXDemo1Form.btnGetAllNames1Click(Sender: TObject);
begin
  TPJEnvironmentVars.GetAllNames(lbEnvVars.Items);
end;

procedure TFMXDemo1Form.btnGetAllNames2Click(Sender: TObject);
var
  Names: TStringDynArray;
  Name: string;
begin
  Names := TPJEnvironmentVars.GetAllNames;
  lbEnvVars.Clear;
  for Name in Names do
    lbEnvVars.Items.Add(Format('[%s]', [Name]));
end;

procedure TFMXDemo1Form.btnGetValueClick(Sender: TObject);
begin
  edValue.Text := TPJEnvironmentVars.GetValue(edName.Text);
end;

procedure TFMXDemo1Form.btnSetValueClick(Sender: TObject);
begin
  CheckOSError( TPJEnvironmentVars.SetValue(edName.Text, edValue.Text));
end;

procedure TFMXDemo1Form.EnvNameCallback(const Name: string; Data: Pointer);
begin
  TStrings(Data).Add(Format('[%s]', [Name]));
end;

procedure TFMXDemo1Form.EnvVarCallback(const EnvVar: TPJEnvironmentVar;
  Data: Pointer);
begin
  TStrings(Data).Add(Format('[%s] = "%s"', [EnvVar.Name, EnvVar.Value]));
end;

end.

