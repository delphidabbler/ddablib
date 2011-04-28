{
 * FmDemo6.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #6: Redirecting Standard I/O using Files.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2011.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmDemo6;

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
  end;

var
  Form1: TForm1;

implementation

uses
  PJConsoleApp, PJFileHandle;

{$R *.dfm}

{ TForm1 }

procedure TForm1.Button1Click(Sender: TObject);
const
  cInFile = 'Demo6-in.txt';
  cOutFile = 'Demo6-out.txt';
var
  App: TPJConsoleApp;
  InFile, OutFile: TPJFileHandle;
begin
  // Save Memo1 to file
  // OK for Unicode Delphis since SaveToFile defaults to saving as ANSI text
  // which is what Echoer program needs in input file.
  Memo1.Lines.SaveToFile(cInFile);
  // Execute the application
  InFile := nil;
  OutFile := nil;
  App := TPJConsoleApp.Create;
  try
    InFile := TPJFileHandle.Create(cInFile, fmOpenRead or fmShareDenyNone);
    OutFile := TPJFileHandle.Create(cOutFile, fmCreate or fmShareExclusive);
    App.Visible := False;
    App.StdIn := InFile.Handle;
    App.StdOut := OutFile.Handle;
    if not App.Execute('Echoer ">>> "') then
      raise Exception.CreateFmt(
        'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
      );
  finally
    OutFile.Free;
    InFile.Free;
    App.Free;
  end;
  // Load Memo2 from file
  // OK for Unicode Delphis since Echoer will have written ANSI text to output
  // file and LoadFromFile defaults to reading ANSI text.
  Memo2.Lines.LoadFromFile(cOutFile);
end;

end.

