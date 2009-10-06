{
 * Main form for StreamWrap Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2001-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmStreamWrap;

interface

uses
  // Delphi
  StdCtrls, Controls, Classes, Forms,
  // delphiDabbler Stream Library
  PJStreamWrapper;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0} // >= Delphi 7
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN UNSAFE_TYPE OFF}
  {$IFEND}
{$ENDIF}

type
  {
  TStreamWrapForm:
    Form class.
  }
  TStreamWrapForm = class(TForm)
    memoOutStr: TMemo;
    lblOutStr: TLabel;
    btnWrite: TButton;
    btnRead: TButton;
    memoInStr: TMemo;
    lblInStr: TLabel;
    btnClose: TButton;
    procedure btnWriteClick(Sender: TObject);
    procedure btnReadClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  end;

  {
  TStrStream:
    Descendant of TPJStreamWrapper that can read and write strings from any
    wrapped stream.
  }
  TStrStream = class(TPJStreamWrapper)
  public
    procedure WriteString(AString: string); // writes string to stream
    function ReadString: string;            // reads string from stream
  end;

var
  StreamWrapForm: TStreamWrapForm;

implementation

uses
  // Delphi
  Dialogs, SysUtils;

{$R *.DFM}

procedure TStreamWrapForm.btnWriteClick(Sender: TObject);
  {Write the strings in the left hand memo to the stream}
var
  SS: TStrStream;   // the string i/o enabled stream wrapper
  I: Integer;       // loops thru all strings in memo
begin
  // Create string writing stream wrapper round stream to new file
  SS := TStrStream.Create(
    TFileStream.Create('test.dat', fmCreate),     // create file & stream on fly
    True);                       // causes file stream to be freed automatically
  try
    // Write out each string in left hand memo to file
    for I := 0 to memoOutStr.Lines.Count - 1 do
      SS.WriteString(memoOutStr.Lines[I]);
  finally
    SS.Free;                                   // also frees wrapped file stream
  end;
end;

procedure TStreamWrapForm.btnReadClick(Sender: TObject);
var
  SS: TStrStream; // the string i/o enabled stream wrapper
  Str: string;    // a string
begin
  // Check file exists
  if FileExists('test.dat') then
  begin
    // Clear memo used to display strings
    memoInStr.Clear;
    // Create a string reading stream wrapping file stream to input file
    SS := TStrStream.Create(
      TFileStream.Create('test.dat', fmOpenRead),  // file stream created on fly
      True);                                  // file stream freed automatically
    try
      // Read all strings from file, adding to memo in angle brackets
      while SS.Position < SS.Size do
      begin
        Str := SS.ReadString;
        memoInStr.Lines.Add('<'+Str+'>');
      end;
    finally
      SS.Free;                                 // also frees wrapped file stream
    end;
  end
  else
    MessageDlg('There is no file to read - try writing some strings!',
      mtError, [mbOK], 0);
end;

procedure TStreamWrapForm.btnCloseClick(Sender: TObject);
  {Closes the application}
begin
  Close;
end;

{ TStrStream }

function TStrStream.ReadString: string;
  {Reads a string from the underlying stream}
var
  Len: Integer; // length of string
begin
  // Get length of string (as 32 bit integer)
  ReadBuffer(Len, SizeOf(Integer));
  // Now get string
  SetLength(Result, Len);
  ReadBuffer(Pointer(Result)^, Len * SizeOf(Char));
end;

procedure TStrStream.WriteString(AString: string);
var
  Len: Integer; // length of string
begin
  // Write out length of string as 32 bit integer
  Len := Length(AString);
  WriteBuffer(Len, SizeOf(Integer));
  // Now write out the string's characters
  WriteBuffer(Pointer(AString)^, Len * SizeOf(Char));
end;

end.
