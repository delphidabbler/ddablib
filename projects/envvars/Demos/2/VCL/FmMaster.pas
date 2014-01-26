unit FmMaster;

interface

uses
  Classes,
  Controls,
  StdCtrls,
  Forms;

type
  TMasterForm = class(TForm)
    btnExecSlave: TButton;
    edNewEnvVars: TMemo;
    chkIncludeCurrentBlock: TCheckBox;
    lblPrompt: TLabel;
    procedure btnExecSlaveClick(Sender: TObject);
  end;

var
  MasterForm: TMasterForm;

implementation

uses
  Windows,
  Dialogs,
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

procedure TMasterForm.btnExecSlaveClick(Sender: TObject);
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
    // Execute the slave app
    ExecProg('SlaveApp.exe', EnvBlock);
  finally
    FreeMem(EnvBlock);
  end;
end;

end.

