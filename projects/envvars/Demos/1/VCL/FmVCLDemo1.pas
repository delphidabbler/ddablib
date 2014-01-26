{
 * Main form for DelphiDabbler Environment Variables Unit demo program #1, VCL
 * version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmVCLDemo1;

{$UNDEF Supports_RTLNamespaces}
{$UNDEF Supports_Closures}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 ad later
    {$DEFINE Supports_RTLNamespaces}
  {$IFEND}
  {$IF CompilerVersion >= 20.0} // Delphi 2009 and later
    {$DEFINE Supports_Closures}
  {$IFEND}
{$ENDIF}

interface

uses
  {$IFNDEF Supports_RTLNamespaces}
  Classes,
  StdCtrls,
  Controls,
  ExtCtrls,
  Forms,
  {$ELSE}
  System.Classes,
  Vcl.StdCtrls,
  Vcl.Controls,
  Vcl.ExtCtrls,
  Vcl.Forms,
  {$ENDIF}
  PJEnvVars;

type
  TVCLDemo1Form = class(TForm)
    Label1: TLabel;
    Label2: TLabel;
    edName: TEdit;
    edValue: TEdit;
    lbEnvVars: TListBox;
    btnGetValue: TButton;
    btnSetValue: TButton;
    btnDelete: TButton;
    btnExpand: TButton;
    btnExists: TButton;
    btnCount: TButton;
    btnBlockSize: TButton;
    Panel1: TPanel;
    Panel2: TPanel;
    btnGetAllNames1: TButton;
    btnGetAll1: TButton;
    btnGetAllNames2: TButton;
    btnGetAll2: TButton;
    btnEnumNames1: TButton;
    btnEnumVars1: TButton;
    btnEnumNames2: TButton;
    btnEnumVars2: TButton;
    btnCreateBlock: TButton;
    Label3: TLabel;
    lbNewEnv: TListBox;
    chkIncludeCurrent: TCheckBox;
    procedure btnBlockSizeClick(Sender: TObject);
    procedure btnCountClick(Sender: TObject);
    procedure btnCreateBlockClick(Sender: TObject);
    procedure btnDeleteClick(Sender: TObject);
    procedure btnEnumNames1Click(Sender: TObject);
    procedure btnEnumNames2Click(Sender: TObject);
    procedure btnEnumVars1Click(Sender: TObject);
    procedure btnEnumVars2Click(Sender: TObject);
    procedure btnExistsClick(Sender: TObject);
    procedure btnExpandClick(Sender: TObject);
    procedure btnGetAll1Click(Sender: TObject);
    procedure btnGetAll2Click(Sender: TObject);
    procedure btnGetAllNames1Click(Sender: TObject);
    procedure btnGetAllNames2Click(Sender: TObject);
    procedure btnGetValueClick(Sender: TObject);
    procedure btnSetValueClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    procedure EnvNameCallback(const Name: string; Data: Pointer);
    procedure EnvVarCallback(const EnvVar: TPJEnvironmentVar; Data: Pointer);
  end;

var
  VCLDemo1Form: TVCLDemo1Form;

implementation

uses
  {$IFNDEF Supports_RTLNamespaces}
  SysUtils,
  Types,
  Dialogs;
  {$ELSE}
  System.SysUtils,
  System.Types,
  Vcl.Dialogs;
  {$ENDIF}

{$R *.dfm}

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

procedure OSErrorCheck(const ErrCode: Integer);
begin
  if ErrCode = 0 then
    Exit;
  raise EOSError.Create(SysErrorMessage(ErrCode));
end;

{ TVCLDemo1Form }

procedure TVCLDemo1Form.btnBlockSizeClick(Sender: TObject);
begin
  ShowMessageFmt('Block size = %d', [TPJEnvironmentVars.BlockSize]);
end;

procedure TVCLDemo1Form.btnCountClick(Sender: TObject);
begin
  ShowMessageFmt('Count = %d', [TPJEnvironmentVars.Count]);
end;

procedure TVCLDemo1Form.btnCreateBlockClick(Sender: TObject);
var
  BlockSize: Integer;
  Block: Pointer;
begin
  BlockSize := TPJEnvironmentVars.CreateBlock(
    lbNewEnv.Items, chkIncludeCurrent.Checked, nil, 0
  );
  GetMem(Block, BlockSize * SizeOf(Char));
  try
    TPJEnvironmentVars.CreateBlock(
      lbNewEnv.Items, chkIncludeCurrent.Checked, Block, BlockSize
    );
    lbEnvVars.Clear;
    MultiSzToStrings(PChar(Block), lbEnvVars.Items);
  finally
    FreeMem(Block, BlockSize * SizeOf(Char));
  end;
end;

procedure TVCLDemo1Form.btnDeleteClick(Sender: TObject);
begin
  OSErrorCheck(TPJEnvironmentVars.Delete(edName.Text));
end;

procedure TVCLDemo1Form.btnEnumNames1Click(Sender: TObject);
begin
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumNames(EnvNameCallback, Pointer(lbEnvVars.Items));
end;

procedure TVCLDemo1Form.btnEnumNames2Click(Sender: TObject);
begin
  {$IFDEF Supports_Closures}
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumNames(
    procedure (const Name: string; Data: Pointer)
    begin
      lbEnvVars.Items.Add(Format('<%s>', [Name]));
    end,
    nil
  );
  {$ENDIF}
end;

procedure TVCLDemo1Form.btnEnumVars1Click(Sender: TObject);
begin
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumVars(EnvVarCallback, Pointer(lbEnvVars.Items));
end;

procedure TVCLDemo1Form.btnEnumVars2Click(Sender: TObject);
begin
  {$IFDEF Supports_Closures}
  lbEnvVars.Clear;
  TPJEnvironmentVars.EnumVars(
    procedure (const EnvVar: TPJEnvironmentVar; Data: Pointer)
    begin
      lbEnvVars.Items.Add(Format('<%s> = "%s"', [EnvVar.Name, EnvVar.Value]));
    end,
    nil
  );
  {$ENDIF}
end;

procedure TVCLDemo1Form.btnExistsClick(Sender: TObject);
begin
  if TPJEnvironmentVars.Exists(edName.Text) then
    ShowMessageFmt('"%s" exists', [edName.Text])
  else
    ShowMessageFmt('"%s" does not exist', [edName.Text]);
end;

procedure TVCLDemo1Form.btnExpandClick(Sender: TObject);
begin
  edValue.Text := TPJEnvironmentVars.Expand(edName.Text);
end;

procedure TVCLDemo1Form.btnGetAll1Click(Sender: TObject);
begin
  TPJEnvironmentVars.GetAll(lbEnvVars.Items);
end;

procedure TVCLDemo1Form.btnGetAll2Click(Sender: TObject);
var
  EnvVars: TPJEnvironmentVarArray;
  EnvVar: TPJEnvironmentVar;
  I: Integer;
begin
  EnvVars := TPJEnvironmentVars.GetAll;
  lbEnvVars.Clear;
  for I := Low(EnvVars) to High(EnvVars) do
  begin
    EnvVar := EnvVars[I];
    lbEnvVars.Items.Add(
      Format('[%s] = "%s"', [EnvVars[I].Name, EnvVars[I].Value])
    );
  end;
end;

procedure TVCLDemo1Form.btnGetAllNames1Click(Sender: TObject);
begin
  TPJEnvironmentVars.GetAllNames(lbEnvVars.Items);
end;

procedure TVCLDemo1Form.btnGetAllNames2Click(Sender: TObject);
var
  Names: TStringDynArray;
  I: Integer;
begin
  Names := TPJEnvironmentVars.GetAllNames;
  lbEnvVars.Clear;
  for I := Low(Names) to High(Names) do
    lbEnvVars.Items.Add(Format('[%s]', [Names[I]]));
end;

procedure TVCLDemo1Form.btnGetValueClick(Sender: TObject);
begin
  edValue.Text := TPJEnvironmentVars.GetValue(edName.Text);
end;

procedure TVCLDemo1Form.btnSetValueClick(Sender: TObject);
begin
  OSErrorCheck(TPJEnvironmentVars.SetValue(edName.Text, edValue.Text));
end;

procedure TVCLDemo1Form.EnvNameCallback(const Name: string; Data: Pointer);
begin
  TStrings(Data).Add(Format('[%s]', [Name]));
end;

procedure TVCLDemo1Form.EnvVarCallback(const EnvVar: TPJEnvironmentVar;
  Data: Pointer);
begin
  TStrings(Data).Add(Format('[%s] = "%s"', [EnvVar.Name, EnvVar.Value]));
end;

procedure TVCLDemo1Form.FormCreate(Sender: TObject);
begin
  {$IFNDEF Supports_Closures}
  btnEnumNames2.Enabled := False;
  btnEnumVars2.Enabled := False;
  {$ENDIF}
end;

end.


