{
 * FmDemo8.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #8: Echoing Console Output to a GUI.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2011.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmDemo8;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  PJPipe, PJConsoleApp, PJPipeFilters;

type

  TForm1 = class(TForm)
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
  Form1: TForm1;

implementation

{$R *.dfm}

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
  fOutFilter := nil;
  fErrFilter := nil;
  try
    fOutFilter := TPJAnsiSBCSPipeFilter.Create(TPJPipe.Create, True);
    fOutFilter.OnLineEnd := OutLineEndHandler;
    fErrFilter := TPJAnsiSBCSPipeFilter.Create(TPJPipe.Create, True);
    fErrFilter.OnLineEnd := ErrLineEndHandler;
    App := TPJConsoleApp.Create;
    try
      App.StdIn := OpenInputFile('..\TestData\MobyDick-ANSI.txt');
      App.StdOut := fOutFilter.Pipe.WriteHandle;
      App.StdErr := fErrFilter.Pipe.WriteHandle;
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
    FreeAndNil(fErrFilter);
    FreeAndNil(fOutFilter);
  end;
end;

procedure TForm1.CompletionHandler(Sender: TObject);
begin
  fOutFilter.Flush;
  fErrFilter.Flush;
end;

procedure TForm1.ErrLineEndHandler(Sender: TObject; const Line: AnsiString);
begin
  Memo2.Lines.Add(string(Line));
end;

procedure TForm1.OutLineEndHandler(Sender: TObject; const Line: AnsiString);
begin
  Memo1.Lines.Add(string(Line));
end;

procedure TForm1.WorkHandler(Sender: TObject);
begin
  fOutFilter.ReadPipe;
  fErrFilter.ReadPipe;
  Application.ProcessMessages;       // Let the memo controls update
end;

end.

