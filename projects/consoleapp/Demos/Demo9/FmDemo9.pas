{
  DelphiDabbler Console Application Runner Classes

  Demo Program 9: Subclassing TPJConsoleApp.

  FmDemo9.dpr
    v1.0 of 03 Oct 2007  - Main form. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
}

unit FmDemo9;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls;

type
  TForm1 = class(TForm)
    Label1: TLabel;
    Button1: TButton;
    Memo1: TMemo;
    Memo2: TMemo;
    procedure Button1Click(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  UConsoleAppEx;

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
var
  App: TConsoleAppEx;
  InStream: TStream;
  OutStream: TStream;
begin
  // Set up i/o streams
  InStream := nil;
  OutStream := nil;
  try
    InStream := TMemoryStream.Create;
    Memo1.Lines.SaveToStream(InStream);
    InStream.Position := 0;
    OutStream := TMemoryStream.Create;
    // Run console app
    App := TConsoleAppEx.Create;
    try
      App.TimeSlice := 2;
      if not App.Execute('Echoer "-->"', InStream, OutStream) then
        raise Exception.CreateFmt(
          'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
        );
    finally
      App.Free;
    end;
    // Read output stream
    OutStream.Position := 0;
    Memo2.Lines.LoadFromStream(OutStream);
  finally
    InStream.Free;
    OutStream.Free;
  end;
end;

end.

