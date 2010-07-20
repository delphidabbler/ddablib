{
 * FmDemo8.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #8: Echoing Console Output to a GUI.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmDemo8;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  PJPipe, PJConsoleApp;

type
  TTextToLines = class(TObject)
  private
    fRemainder: string;
    fLines: TStrings;
  public
    procedure AddText(const Text: string);
    procedure Flush;
    constructor Create(const Lines: TStrings);
  end;

  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    Label2: TLabel;
    Label3: TLabel;
    procedure Button1Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
  private
    fErrPipe, fOutPipe: TPJPipe;
    fOutLines, fErrLines: TTextToLines;
    procedure WorkHandler(Sender: TObject);
    procedure CompletionHandler(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TTextToLines }

procedure TTextToLines.AddText(const Text: string);
var
  EOLPos: Integer;
begin
  if Text = '' then
    Exit;
  fRemainder := fRemainder + Text;
  EOLPos := Pos(#13#10, fRemainder);
  while EOLPos > 0 do
  begin
    fLines.Add(Copy(fRemainder, 1, EOLPos - 1));
    fRemainder := Copy(fRemainder, EOLPos + 2, MaxInt);
    EOLPos := Pos(#13#10, fRemainder);
  end;
end;

constructor TTextToLines.Create(const Lines: TStrings);
begin
  inherited Create;
  fLines := Lines;
  fRemainder := '';
end;

procedure TTextToLines.Flush;
begin
  if fRemainder <> '' then
  begin
    fLines.Add(fRemainder);
    fRemainder := '';
  end;
end;

{ TForm1 }

function OpenInputFile(const FileName: string): THandle;
var
  Security: TSecurityAttributes;  // file's security attributes
begin
  // Set up security structure so file handle is inheritable (NT)
  Security.nLength := SizeOf(Security);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := True;
  // Open the file for reading
  Result := CreateFile(
    PChar(FileName),        // file name
    GENERIC_READ,           // readable file
    FILE_SHARE_READ,        // share read access
    @Security,              // security attributes (NT only)
    OPEN_EXISTING,          // file must exist
    FILE_ATTRIBUTE_NORMAL,  // just the normal attributes
    0                       // no template file (NT only)
  );
  if Result = INVALID_HANDLE_VALUE then
    raise Exception.CreateFmt('Can''t open file "%s"', [FileName]);
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  App: TPJConsoleApp;
begin
  fOutPipe := nil;
  fErrPipe := nil;
  try
    fOutPipe := TPJPipe.Create;
    fErrPipe := TPJPipe.Create;
    App := TPJConsoleApp.Create;
    try
      App.StdIn := OpenInputFile('..\..\PJConsoleApp.pas');
      App.StdOut := fOutPipe.WriteHandle;
      App.StdErr := fErrPipe.WriteHandle;
      App.OnWork := WorkHandler;
      App.OnComplete := CompletionHandler;
      App.TimeSlice := 1;
      if not App.Execute('Echoer') then
        raise Exception.CreateFmt(
          'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
        );
    finally
      FileClose(App.StdIn);
      App.Free;
    end;
  finally
    FreeAndNil(fErrPipe);
    FreeAndNil(fOutPipe);
  end;
end;

procedure TForm1.CompletionHandler(Sender: TObject);
begin
  fOutLines.Flush;
  fErrLines.Flush;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  fOutLines := TTextToLines.Create(Memo1.Lines);
  fErrLines := TTextToLines.Create(Memo2.Lines);
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FreeAndNil(fErrLines);
  FreeAndNil(fOutLines);
end;

procedure TForm1.WorkHandler(Sender: TObject);

  procedure ProcessPipe(const Pipe: TPJPipe; const LineHandler: TTextToLines);
  var
    Text: string;
    BytesToRead: Cardinal;
    BytesRead: Cardinal;
  begin
    BytesToRead := Pipe.AvailableDataSize;
    SetLength(Text, BytesToRead);
    Pipe.ReadData(PChar(Text)^, BytesToRead, BytesRead);
    if BytesRead > 0 then
    begin
      SetLength(Text, BytesRead);
      LineHandler.AddText(Text);
    end;
  end;

begin
  ProcessPipe(fErrPipe, fErrLines);  // Read from standard error
  ProcessPipe(fOutPipe, fOutLines);  // Read from standard output
  Application.ProcessMessages;       // Let the memo controls update
end;

end.

