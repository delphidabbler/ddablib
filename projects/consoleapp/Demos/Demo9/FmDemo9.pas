{
 * FmDemo9.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #9: Subclassing TPJConsoleApp.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
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

