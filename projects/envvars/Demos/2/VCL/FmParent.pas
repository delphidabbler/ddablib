{
 * Main form for the Parent application of the DelphiDabbler Environment
 * Variables Unit demo program #2, VCL version.
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmParent;

{$UNDEF Supports_RTLNamespaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 ad later
    {$DEFINE Supports_RTLNamespaces}
  {$IFEND}
{$ENDIF}

interface

uses
  {$IFNDEF Supports_RTLNamespaces}
  Classes,
  Controls,
  StdCtrls,
  Forms;
  {$ELSE}
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms;
  {$ENDIF}

type
  TParentForm = class(TForm)
    btnExecChild: TButton;
    edNewEnvVars: TMemo;
    chkIncludeCurrentBlock: TCheckBox;
    lblPrompt: TLabel;
    procedure btnExecChildClick(Sender: TObject);
  end;

var
  ParentForm: TParentForm;

implementation

uses
  {$IFNDEF Supports_RTLNamespaces}
  Windows,
  Dialogs,
  {$ELSE}
  Winapi.Windows,
  Vcl.Dialogs,
  {$ENDIF}
  PJEnvVars;

{$R *.DFM}

procedure ExecProg(const ProgName: string; EnvBlock: Pointer);
  // Creates a new process for given program passing any given environment block
var
  SI: TStartupInfo;
  PI: TProcessInformation;
  CreateFlags: DWORD;       // process creation flags
  SafeProgName: string;     // program name: safe for CreateProcessW
begin
  // Make ProgName parameter safe for passing to CreateProcessW (see
  // http://bit.ly/1jQJADa for details)
  SafeProgName := ProgName;
  UniqueString(SafeProgName);
  // Set up startup info record: all default values
  FillChar(SI, SizeOf(SI), 0);
  SI.cb := SizeOf(SI);
  // Set up creation flags: special flag required for Unicode environments,
  // which is what we want when Unicode support is enabled.
  // NOTE: The environment block is created in Unicode when compiled with a
  // Unicode version of Delphi. However, the unicode version of CreateProcess
  // (CreateProcessW) assumes the environment block to be ANSI unless we specify
  //  the CREATE_UNICODE_ENVIRONMENT flag.
  {$IFDEF UNICODE}
  CreateFlags := CREATE_UNICODE_ENVIRONMENT;  // passing a unicode env
  {$ELSE}
  CreateFlags := 0;
  {$ENDIF}
  // Execute the program
  // NOTE: CreateProcess = CreateProcessW when the UNICODE symbol is defined
  // while CreateProcess = CreateProcessA when UNICODE is undefined.
  if not CreateProcess(
    nil, PChar(SafeProgName), nil, nil, True,
    CreateFlags, EnvBlock, nil, SI, PI
  ) then
    ShowMessageFmt('Can''t execute "%s"', [ProgName]);
end;

procedure TParentForm.btnExecChildClick(Sender: TObject);
var
  EnvBlock: Pointer;
  BlockSize: Integer;
begin
  // Create the environment block
  BlockSize := TPJEnvironmentVars.CreateBlock(
    edNewEnvVars.Lines, chkIncludeCurrentBlock.Checked, nil, 0
  );
  GetMem(EnvBlock, BlockSize * SizeOf(Char));
  try
    TPJEnvironmentVars.CreateBlock(
      edNewEnvVars.Lines, chkIncludeCurrentBlock.Checked, EnvBlock, BlockSize
    );
    // Execute a child process
    ExecProg('Child.exe', EnvBlock);
  finally
    FreeMem(EnvBlock);
  end;
end;

end.

