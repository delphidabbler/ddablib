{
 * FmDemo6.pas
 *
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #6: Redirecting Standard I/O using Files.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
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
  PJConsoleApp;

{$R *.dfm}

{ TForm1 }

function CreateOutputFile(const FileName: string): THandle;
var
  Security: TSecurityAttributes;  // file's security attributes
begin
  // Set up security structure so file handle is inheritable (NT)
  Security.nLength := SizeOf(Security);
  Security.lpSecurityDescriptor := nil;
  Security.bInheritHandle := True;
  // Create new empty file
  Result := CreateFile(
    PChar(FileName),        // file name
    GENERIC_WRITE,          // writeable file
    0,                      // no sharing
    @Security,              // security attributes (NT only)
    CREATE_ALWAYS,          // always create the file, even if exists
    FILE_ATTRIBUTE_NORMAL,  // just the normal attributes
    0                       // no template file (NT only)
  );
  if Result = INVALID_HANDLE_VALUE then
    raise Exception.CreateFmt('Can''t create file "%s"', [FileName]);
end;

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
const
  cInFile = 'Demo6-in.txt';
  cOutFile = 'Demo6-out.txt';
var
  App: TPJConsoleApp;
begin
  // Save Memo1 to file
  Memo1.Lines.SaveToFile(cInFile);
  // Execute the application
  App := TPJConsoleApp.Create;
  try
    App.Visible := False;
    App.StdIn := OpenInputFile(cInFile);
    App.StdOut := CreateOutputFile(cOutFile);
    if not App.Execute('Echoer ">>> "') then
      raise Exception.CreateFmt(
        'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
      );
  finally
    FileClose(App.StdIn);
    FileClose(App.StdOut);
    App.Free;
  end;
  // Load Memo2 from file
  Memo2.Lines.LoadFromFile(cOutFile);
end;

end.

