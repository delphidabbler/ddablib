{
 * Whole source code for Echoer.exe helper program for DelphiDabbler Console
 * Application Runner Classes Demos. Echoes text from standard input onto
 * standard output with a given prefix to each line.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program Echoer;

{
  Echoes text on standard input to standard output prefixed by text passed as
  1st parameter on command line. If no parameter is provided text is prefixed by
  '>' character.

  Program exit code is always 0.

  Usage:
    Echoer [prefix-text] [-u]
  Eg 1: to run the with prefix "hello world > " and Ansi text output use
    Echoer "hello world > "
  Eg 2: to run with the prefix "hello world > " and Unicode text output use
    Echoer "hello world > " -u
  Eg 3: to run with the default ">" prefix and Ansi text output use
    Echoer
  Eg 4: to run with the default ">" prefix and Unicode text output use
    Echoer -u
  Note:
    When the -u switch is specified, any prefix text preceed it.
}

{$UNDEF COMPILERSUPPORTED}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$DEFINE COMPILERSUPPORTED}
  {$IFEND}
{$ENDIF}

{$IFNDEF COMPILERSUPPORTED}
  {$MESSAGE FATAL 'Minimum compiler version is Delphi 7'}
{$ENDIF}

{$APPTYPE CONSOLE}

uses
  Windows, SysUtils;

{$IF not Declared(UnicodeString)}
type
  UnicodeString = WideString;
{$IFEND}

{ Emulates C std lib stdout value by returning appropriate Windows handle }
function StdOut: Integer;
begin
  Result := Windows.GetStdHandle(STD_OUTPUT_HANDLE);
end;

{ Emulates C std lib stderr value by returning appropriate Windows handle }
function StdErr: Integer;
begin
  Result := Windows.GetStdHandle(STD_ERROR_HANDLE);
end;

{ Writes an Ansi string to an output "file" }
procedure WriteStr(Handle: THandle; const S: AnsiString); overload;
var
  Dummy: DWORD;
begin
  Windows.WriteFile(Handle, Pointer(S)^, Length(S), Dummy, nil);
end;

{ Writes a Unicode string to an output "file" }
procedure WriteStr(Handle: THandle; const S: UnicodeString); overload;
var
  Dummy: DWORD;
begin
  Windows.WriteFile(
    Handle, Pointer(S)^, Length(S) * SizeOf(WideChar), Dummy, nil
  );
end;

{ Writes an Ansi string followed by a newline to an output "file"}
procedure WriteStrLn(Handle: THandle; const S: AnsiString); overload;
begin
  WriteStr(Handle, S + #13#10);
end;

{ Writes a Unicode string followed by a newline to an output "file"}
procedure WriteStrLn(Handle: THandle; const S: UnicodeString); overload;
begin
  WriteStr(Handle, S + #13#10);
end;

procedure WriteLine(Handle: THandle; const S: AnsiString; AsUnicode: Boolean);
begin
  if AsUnicode then
    WriteStrLn(Handle, UnicodeString(S))
  else
    WriteStrLn(Handle, S);
end;

function StrToAnsiStr(const S: string): AnsiString;
{$IFDEF UNICODE}
var
  Bytes: TBytes;
{$ENDIF}
begin
  {$IFDEF UNICODE}
  Bytes := TEncoding.Default.GetBytes(S);
  SetLength(Result, Length(Bytes));
  if Length(Bytes) > 0 then
    Move(Pointer(Bytes)^, Pointer(Result)^, Length(Bytes));
  {$ELSE}
  Result := S;
  {$ENDIF}
end;

var
  Prefix: AnsiString;
  Line: AnsiString;
  Count: Integer;
  ProgName: AnsiString;
  WantUnicode: Boolean;

begin
  // Parse command line
  ProgName := StrToAnsiStr(ExtractFileName(ParamStr(0)));
  if ParamStr(1) <> '-u' then
    Prefix := StrToAnsiStr(ParamStr(1))
  else
    Prefix := '';
  if Prefix = '' then
    Prefix := '>';
  WantUnicode := ((ParamCount = 2) and (ParamStr(2) = '-u'))
    or ((ParamCount = 1) and (ParamStr(1) = '-u'));

  // Write intro text to stderr
  WriteLine(StdErr, ProgName + ' - Starting', WantUnicode);
  WriteLine(
    StdErr, ProgName + ' - Using prefix: "' + Prefix + '"', WantUnicode
  );

  // Read lines of Ansi text from stdin and copy to stdout with prefix
  Count := 0;
  while not EOF do
  begin
    Inc(Count);
    ReadLn(Line);
    WriteLine(StdOut, Prefix + Line, WantUnicode);
  end;

  // Write closing text to stderr
  WriteLine(
    StdErr,
    ProgName + ' - ' + StrToAnsiStr(IntToStr(Count)) + ' lines written',
    WantUnicode
  );
  WriteLine(StdErr, ProgName + ' - Finished', WantUnicode);
end.

