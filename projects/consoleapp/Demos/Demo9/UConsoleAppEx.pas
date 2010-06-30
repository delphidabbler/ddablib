{
  DelphiDabbler Console Application Runner Classes

  Demo Program 9: Subclassing TPJConsoleApp.

  UConsoleAppEx
    v1.0 of 03 Oct 2007  - Implements class that extends TPJConsoleApp. Original
                           version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
}

unit UConsoleAppEx;

interface

uses
  Classes,
  PJConsoleApp, PJPipe;

type

  TConsoleAppEx = class(TPJCustomConsoleApp)
  private
    fOutPipe: TPJPipe;
    fOutStream: TStream;
  protected
    procedure DoWork; override;
  public
    function Execute(const CmdLine: string;
      const InStream, OutStream: TStream): Boolean;
    property ErrorCode;
    property ErrorMessage;
    property TimeSlice;
  end;

implementation

uses
  SysUtils;

{ TConsoleAppEx }

procedure TConsoleAppEx.DoWork;
begin
  fOutPipe.CopyToStream(fOutStream, 0);
end;

function TConsoleAppEx.Execute(const CmdLine: string; const InStream,
  OutStream: TStream): Boolean;
var
  InPipe: TPJPipe;
begin
  fOutStream := OutStream;
  InPipe := nil;
  fOutPipe := nil;
  try
    // Set up input pipe and associated with console app stdin
    InPipe := TPJPipe.Create(InStream.Size);
    InPipe.CopyFromStream(InStream, 0);
    InPipe.CloseWriteHandle;
    StdIn := InPipe.ReadHandle;
    // Set up output pipe and associate with console app stdout
    fOutPipe := TPJPipe.Create;
    StdOut := fOutPipe.WriteHandle;
    // Run the application
    Result := inherited Execute(CmdLine);
  finally
    StdIn := 0;
    StdOut := 0;
    FreeAndNil(fOutPipe);
    FreeAndNil(InPipe);
  end;
end;

end.

