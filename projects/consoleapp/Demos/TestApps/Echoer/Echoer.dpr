{
 * Echoer.dpr
 *
 * Whole source code for Echoer.exe helper program for DelphiDabbler Console
 * Application Runner Classes Demos. Echoes text from standard input onto
 * standard output with a given prefix to each line.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

program Echoer;

{
  Echoes text on standard input to standard output prefixed by text passed as
  1st parameter on command line. If no parameter is provided text is prefixed by
  '>' character.

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
