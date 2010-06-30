{
  DelphiDabbler Console Application Runner Classes

  Demo helper program: Echoer.exe.

  Echoer.dpr
    v1.0 of 03 Oct 2007  - Whole application. Original version.

  Written by Peter Johnson (www.delphidabbler.com).
  This demo program and its source code is placed in the public domain with no
  restrictions as to use.

  THE PROGRAM IS PROVIDED "AS-IS", WITHOUT ANY EXPRESS OR IMPLIED WARRANTY. IN
  NO EVENT SHALL THE AUTHOR BE HELD LIABLE FOR ANY DAMAGES ARISING FROM THE USE
  OF THIS LIBRARY.
}

program Echoer;

{
  Application that echoes text on standard input to standard output prefixed
  by text passed as 1st parameter on command line. If no parameter is provided
  text is prefixed by '>' character

  Program exit code is always 0.

  Usage:
    Echoer [prefix-text]
  Eg: to run the with prefix "hello world > " use
    Echoer "hello world > "
}

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils;


{ Emulates C std lib stderr value by returning appropriate Windows handle }
function StdErr: Integer;
begin
  Result := Windows.GetStdHandle(STD_ERROR_HANDLE);
end;

{ Writes a single character or string to a file }
procedure WriteStr(Handle: THandle; const S: string);
var
  Dummy: DWORD;
begin
  Windows.WriteFile(Handle, PChar(S)^, Length(S), Dummy, nil);
end;

{ Writes a single character or string to a file followed by a newline }
procedure WriteStrLn(Handle: THandle; const S: string);
begin
  WriteStr(Handle, S + #13#10);
end;

var
  Prefix: string;
  Line: string;
  Count: Integer;
  ProgName: string;
begin
  ProgName := ExtractFileName(ParamStr(0));
  WriteStrLn(StdErr, ProgName + ' - Starting');
  Prefix := ParamStr(1);
  if Prefix = '' then
    Prefix := '>';
  WriteStrLn(StdErr, ProgName + ' - Using prefix: "' + Prefix + '"');
  Count := 0;
  while not EOF do
  begin
    Inc(Count);
    ReadLn(Line);
    WriteLn(Prefix, Line);
  end;
  WriteStrLn(StdErr, ProgName + ' - ' + IntToStr(Count) + ' lines written');
  WriteStrLn(StdErr, ProgName + ' - Finished');
end.
