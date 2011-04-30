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

  PJPipe, PJConsoleApp, PJPipeFilters, PJFileHandle;

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

procedure TForm1.Button1Click(Sender: TObject);
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

