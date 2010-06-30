{
  DelphiDabbler Console Application Runner Classes

  Demo Program 7: Redirecting Standard I/O using Pipes.

  FmDemo7.dpr
    v1.0 of 03 Oct 2007  - Main form. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
}

unit FmDemo7;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs,

  PJPipe, PJConsoleApp, StdCtrls;

type
  TForm1 = class(TForm)
    Button1: TButton;
    Label1: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    fOutPipe: TPJPipe;
    fOutStream: TStream;
    procedure WorkHandler(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  App: TPJConsoleApp;
  Text: string;
  InPipe: TPJPipe;
begin
  fOutStream := nil;
  fOutPipe := nil;
  // Write memo 1 contents into read pipe
  Text := Memo1.Text;
  InPipe := TPJPipe.Create(Length(Text));
  try
    InPipe.WriteData(PChar(Text)^, Length(Text));
    InPipe.CloseWriteHandle;
    // Create out pipe and stream that receives out pipe's data
    fOutPipe := TPJPipe.Create;
    fOutStream := TMemoryStream.Create;
    // Execute the application
    App := TPJConsoleApp.Create;
    try
      App.TimeSlice := 2; // forces more than one OnWork event
      App.OnWork := WorkHandler;
      App.StdIn := InPipe.ReadHandle;
      App.StdOut := fOutPipe.WriteHandle;
      if not App.Execute('Echoer "--> "') then
        raise Exception.CreateFmt(
          'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
        );
    finally
      App.Free;
    end;
    // Load data from output stream into memo 2
    fOutStream.Position := 0;
    Memo2.Lines.LoadFromStream(fOutStream);
  finally
    FreeAndNil(InPipe);
    FreeAndNil(fOutPipe);
    FreeAndNil(fOutStream);
  end;
end;

procedure TForm1.WorkHandler(Sender: TObject);
begin
  fOutPipe.CopyToStream(fOutStream, 0);
end;

end.

