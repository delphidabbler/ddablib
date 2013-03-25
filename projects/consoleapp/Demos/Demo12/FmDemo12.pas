{
 * Main form for DelphiDabbler Console Application Runner Classes demo program
 * #12: Handling Unicode output from console applications.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo12;

interface

uses
  Windows, Messages, SysUtils, Variants, Classes, Graphics, Controls, Forms,
  Dialogs, StdCtrls,

  PJConsoleApp, PJPipe, PJFileHandle, PJPipeFilters;

{$IF not Declared(UnicodeString)}
type
  UnicodeString = WideString;
{$IFEND}

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    Button1: TButton;
    Label1: TLabel;
    procedure Button1Click(Sender: TObject);
  private
    fOutFilter: TPJUnicodeBMPPipeFilter;
    procedure LineEndHandler(Sender: TObject; const Line: UnicodeString);
    procedure WorkHandler(Sender: TObject);
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.Button1Click(Sender: TObject);
var
  App: TPJConsoleApp;
  InFile: TPJFileHandle;
begin
  InFile := nil;
  App := nil;
  fOutFilter := TPJUnicodeBMPPipeFilter.Create(TPJPipe.Create, True);
  try
    fOutFilter.OnLineEnd := LineEndHandler;
    InFile := TPJFileHandle.Create(
      '..\TestData\MobyDick-ANSI.txt', fmOpenRead or fmShareDenyNone
    );
    App := TPJConsoleApp.Create;
    App.StdIn := InFile.Handle;
    App.StdOut := fOutFilter.Pipe.WriteHandle;
    App.TimeSlice := 5;
    App.OnWork := WorkHandler;
    App.CommandLine := 'Echoer ">>> " -u';
    if not App.Execute then
      raise Exception.CreateFmt(
        'Error %X: %s', [App.ErrorCode, App.ErrorMessage]
      );
  finally
    App.Free;
    InFile.Free;
    FreeAndNil(fOutFilter);
  end;
end;

procedure TForm1.LineEndHandler(Sender: TObject; const Line: UnicodeString);
begin
  Memo1.Lines.Add(Line);
end;

procedure TForm1.WorkHandler(Sender: TObject);
begin
  fOutFilter.ReadPipe;
  Application.ProcessMessages;
end;

end.
