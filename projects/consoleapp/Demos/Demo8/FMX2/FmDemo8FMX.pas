{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #8: Echoing console output to a GUI (FireMonkey 2 version).
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo8FMX;

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
  FMX.Layouts,
  FMX.Types,
  FMX.Controls,
  {$IFDEF REQUIRES_FMX_STDCTRLS}
  FMX.StdCtrls,
  {$ENDIF}
  FMX.Forms,
  FMX.Memo,
  PJPipe,
  PJConsoleApp,
  PJPipeFilters,
  PJFileHandle;

type
  TDemo8FMXForm = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    fErrFilter, fOutFilter: TPJAnsiSBCSPipeFilter;
    procedure ErrLineEndHandler(Sender: TObject; const Line: AnsiString);
    procedure OutLineEndHandler(Sender: TObject; const Line: AnsiString);
    procedure WorkHandler(Sender: TObject);
    procedure CompletionHandler(Sender: TObject);
  end;

var
  Demo8FMXForm: TDemo8FMXForm;

implementation

uses
  System.SysUtils;

{$R *.FMX}

{ TDemo8FMXForm }

procedure TDemo8FMXForm.Button1Click(Sender: TObject);
var
  App: TPJConsoleApp;
  InFile: TPJFileHandle;
  OutPipe, ErrPipe: TPJPipe;
const
  InFileName = '..\TestData\MobyDick-ANSI.txt';
begin
  fOutFilter := nil;
  fErrFilter := nil;
  OutPipe := nil;
  ErrPipe := nil;
  InFile := nil;
  try
    // Open input file
    InFile := TPJFileHandle.Create(InFileName, fmOpenRead or fmShareDenyNone);

    // Create output pipes: one each for stdout and stderr
    OutPipe := TPJPipe.Create;
    ErrPipe := TPJPipe.Create;

    // Create filter objects used to format text from output pipe into lines
    fOutFilter := TPJAnsiSBCSPipeFilter.Create(OutPipe);
    fOutFilter.OnLineEnd := OutLineEndHandler;
    fErrFilter := TPJAnsiSBCSPipeFilter.Create(ErrPipe);
    fErrFilter.OnLineEnd := ErrLineEndHandler;

    App := TPJConsoleApp.Create;
    try
      // redirect stdin to file and stdout/stderr to pipes
      App.StdIn := InFile.Handle;
      App.StdOut := OutPipe.WriteHandle;
      App.StdErr := ErrPipe.WriteHandle;
      App.OnWork := WorkHandler;
      App.OnComplete := CompletionHandler;
      App.TimeSlice := 1;
      if not App.Execute('Echoer') then
        raise Exception.CreateFmt(
          'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
        );
    finally
      App.Free;
    end;
  finally
    FreeAndNil(fErrFilter);
    FreeAndNil(fOutFilter);
    ErrPipe.Free;
    OutPipe.Free;
    InFile.Free;
  end;
end;

procedure TDemo8FMXForm.CompletionHandler(Sender: TObject);
begin
  fOutFilter.Flush;
  fErrFilter.Flush;
end;

procedure TDemo8FMXForm.ErrLineEndHandler(Sender: TObject; const Line: AnsiString);
begin
  Memo2.Lines.Add(string(Line));
end;

procedure TDemo8FMXForm.OutLineEndHandler(Sender: TObject; const Line: AnsiString);
begin
  Memo1.Lines.Add(string(Line));
end;

procedure TDemo8FMXForm.WorkHandler(Sender: TObject);
begin
  fOutFilter.ReadPipe;
  fErrFilter.ReadPipe;
  Application.ProcessMessages;       // Let the memo controls update
end;

end.

