{
 * Main form for IStreamWrap Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmIStreamWrap;

{$UNDEF REQUIRES_TYPES_UNIT}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
    {$DEFINE REQUIRES_TYPES_UNIT}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN UNSAFE_TYPE OFF}
  {$IFEND}
{$ENDIF}

interface

uses
  // Delphi
  {$IFDEF REQUIRES_TYPES_UNIT}
  System.UITypes,
  System.Types,
  {$ENDIF}
  ComCtrls,
  Menus,
  Buttons,
  StdCtrls,
  Controls,
  Classes,
  Forms,
  ActiveX;

type
  {
  TIStreamWrapForm:
    Form class.
  }
  TIStreamWrapForm = class(TForm)
    lblOutStr: TLabel;
    lblInStr: TLabel;
    memoOutStr: TMemo;
    btnMSAppend: TButton;
    btnMSRead: TButton;
    memoInStr: TMemo;
    btnClose: TButton;
    btnMSClear: TButton;
    btnMSToFile: TButton;
    btnMSStats: TButton;
    btnFSStats: TBitBtn;
    mnuFSStats: TPopupMenu;
    miTPJIStreamWrapper: TMenuItem;
    miTPJHandleIStreamWrapper1: TMenuItem;
    miTPJHandleIStreamWrapper2: TMenuItem;
    miTPJFileIStream: TMenuItem;
    sbStatus: TStatusBar;
    btnHelp: TButton;
    procedure btnFSStatsClick(Sender: TObject);
    procedure miFSStatsClick(Sender: TObject);
    procedure btnMSStatsClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnMSAppendClick(Sender: TObject);
    procedure btnMSReadClick(Sender: TObject);
    procedure btnMSClearClick(Sender: TObject);
    procedure btnMSToFileClick(Sender: TObject);
    procedure btnHelpClick(Sender: TObject);
  private
    fIMemStm: IStream;  // wraps a memory stream
    procedure UpdateStatusBar(const PanelID: Integer; const Msg: string);
    procedure ShowMemStmSize;
  end;

var
  IStreamWrapForm: TIStreamWrapForm;

implementation

uses
  // Delphi
  Windows, SysUtils, ComObj, Dialogs,
  // DelphiDabbler Stream Library
  PJIStreams,
  // Project
  FmIStreamWrapHelp;

{$R *.DFM}

const
  cTestFileName = 'Test.txt'; // name of test file

{ Helper routines }

function WinFileTimeToDOSDateTime(const FileTime: TFileTime): Integer;
var
  LocalFileTime: TFileTime;
begin
  FileTimeToLocalFileTime(FileTime, LocalFileTime);
  if not FileTimeToDOSDateTime(
    LocalFileTime, LongRec(Result).Hi, LongRec(Result).Lo
  ) then
    Result := 0;
end;

function WinFileTimeToDateTime(const FileTime: TFileTime): TDateTime;
begin
  try
    Result := FileDateToDateTime(WinFileTimeToDOSDateTime(FileTime));
  except
    Result := 0.0;
  end;
end;

function FileTimeFmt(const Date: TFileTime): string;
begin
  if (Date.dwLowDateTime = 0) and (Date.dwHighDateTime = 0) then
    Result := 'N/A'
  else
    Result := DateTimeToStr(WinFileTimeToDateTime(Date));
end;

procedure ShowStats(Stm: IStream);
var
  Malloc: IMalloc;  // COM allocator
  Stats: TStatStg;  // stream stats structure
begin
  // Get memory allocator we need to free stream name's memory
  OleCheck(CoGetMalloc(1, Malloc));
  // Get stream's stats, including name
  OleCheck(Stm.Stat(Stats, STATFLAG_DEFAULT));
  // Display the results
  MessageDlg(
    Format(
      'Stream stats'#10#10 +
      '     Name: %s'#10#10 +
      '     Size: %d'#10#10 +
      '     Creation date: %s'#10 +
      '     Last Modification date: %s'#10 +
      '     Last Accessed date: %s'#10,
      [
        Stats.pwcsName,
        Stats.cbSize,
        FileTimeFmt(Stats.ctime),
        FileTimeFmt(Stats.mtime),
        FileTimeFmt(Stats.atime)
      ]
    ),
    mtInformation,
    [mbOK],
    0
  );
  // Free the stats structure's stream name memory
  Malloc.Free(Stats.pwcsName);
end;

{ TIStreamWrapForm }

procedure TIStreamWrapForm.btnCloseClick(Sender: TObject);
begin
  Close;
end;

procedure TIStreamWrapForm.btnFSStatsClick(Sender: TObject);
var
  PopupPos: TPoint;
begin
  PopupPos := ClientToScreen(
    Point(btnFSStats.Left, btnFSStats.Top + btnFSStats.Height)
  );
  mnuFSStats.Popup(PopupPos.X, PopupPos.Y);
end;

procedure TIStreamWrapForm.btnHelpClick(Sender: TObject);
begin
  with TIStreamWrapHelpForm.Create(Self) do
    try
      ShowModal;
    finally
      Free;
    end;
end;

procedure TIStreamWrapForm.btnMSAppendClick(Sender: TObject);
begin
  // Write out the contents of the memo to the current stream position
  // we assume this is the end of the stream
  OleCheck(
    fIMemStm.Write(
      Pointer(memoOutStr.Text), Length(memoOutStr.Text) * SizeOf(Char), nil
    )
  );
  // Display size of stream in status bar
  ShowMemStmSize;
end;

procedure TIStreamWrapForm.btnMSClearClick(Sender: TObject);
begin
  // Clear stream by setting size to zero
  OleCheck(fIMemStm.SetSize(0));
  // Display size of stream in status bar: should be 0!
  ShowMemStmSize;
end;

procedure TIStreamWrapForm.btnMSReadClick(Sender: TObject);
var
  StreamSize: LargeInt;
  StreamStart: LargeInt;
  Buf: PChar;
  BytesRead: LongInt;
begin
  // Find end of stream to get size (could also use IStream.Stat method)
  OleCheck(fIMemStm.Seek(0, STREAM_SEEK_END, StreamSize));
  // Create a buffer to store all of memory stream + terminal #0 byte
  GetMem(Buf, StreamSize + SizeOf(Char));
  try
    // Zero all memory in buffer: ensures we have terminal #0 byte
    FillChar(Buf^, StreamSize + SizeOf(Char), 0);
    // Wind to start of stream and read it in
    OleCheck(
      fIMemStm.Seek(0, STREAM_SEEK_SET, StreamStart)
    );
    OleCheck(fIMemStm.Read(Buf, StreamSize, @BytesRead));
    // Copy buffer to memo where we're displaying stream
    memoInStr.Lines.SetText(Buf);
    // Display number of bytes read in status bar
    UpdateStatusBar(1,
      'Bytes read from Memory Stream = ' + IntToStr(BytesRead)
    );
  finally
    // Free the buffer
    FreeMem(Buf, StreamSize);
  end;
end;

procedure TIStreamWrapForm.btnMSStatsClick(Sender: TObject);
begin
  // Display stats for memory stream
  ShowStats(fIMemStm);
end;

procedure TIStreamWrapForm.btnMSToFileClick(Sender: TObject);
var
  FileStm: TPJIStreamWrapper;     // stream to output file
  MSSize: Int64;                  // size of mem stream
  Dummy: Int64;                   // unused stream pos required by Seek method
  BytesRead, BytesWritten: Int64; // bytes read from mem stm / written to file
begin
  // Create stream to new test file
  FileStm := TPJIStreamWrapper.Create(
    TFileStream.Create(cTestFileName, fmCreate),
    True
  );
  // Find size of mem stream by winding to end and recording position
  // (position = sizew of stream)
  // (could also have used fIMemStm.Stat method to find size)
  OleCheck(fIMemStm.Seek(0, STREAM_SEEK_END, MSSize));
  // Wind to start of stream ready for copying
  OleCheck(fIMemStm.Seek(0, STREAM_SEEK_SET, Dummy));
  // Copy whole of mem stream to file stream
  OleCheck(fIMemStm.CopyTo(FileStm, MSSize, BytesRead, BytesWritten));
  // Display message confirming written
  MessageDlg(
    Format('Memory stream copied to %s'#10 +
      '    %d bytes read from memory stream'#10 +
      '    %d bytes written to file',
      [cTestFileName, BytesRead, BytesWritten]),
    mtInformation,
    [mbOK],
    0
  );
end;

procedure TIStreamWrapForm.FormCreate(Sender: TObject);
begin
  // Create an IStream wrapper to a memory stream: we use this for tranferring
  // data between left hand memo and right hand memo and/or file
  fIMemStm := TPJIStreamWrapper.Create(
    TMemoryStream.Create,
    True
  );
  ShowMemStmSize;
end;

procedure TIStreamWrapForm.miFSStatsClick(Sender: TObject);
var
  MI: TMenuItem;  // menu item selected
  Stm: IStream;   // stream opened to test file
  FileH: Integer; // file handle on test file (used to create handle stream)
begin
  // Check file exists and get out if it doesn't
  if not FileExists(cTestFileName) then
  begin
    MessageDlg(
      'File doesn''t exist.'#10 +
      'Use the "Copy Memory Stream to File" button to create the file.',
      mtError,
      [mbOK],
      0
    );
    Exit;
  end;
  // Intialise
  FileH := 0;
  MI := Sender as TMenuItem;
  // Decide how to open IStream on file:
  // different levels of stats details will be available depending on choice
  case MI.Tag of
    0:
      // Open a basic stream wrapper:
      //   name is constructed and no dates available
      Stm := TPJIStreamWrapper.Create(
        TFileStream.Create(cTestFileName, fmOpenRead),
        True
      );
    1:
    begin
      // Open a handle stream wrapper using file handle and THandleStream:
      //   1st way to open file using TPJHandleIStream
      //   name is constructed but file dates are available
      FileH := FileOpen(cTestFileName, fmOpenRead);
      Stm := TPJHandleIStreamWrapper.Create(
        THandleStream.Create(FileH),
        True
      );
    end;
    2:
      // Open a handle stream using a TFileStream:
      //   2nd way to open file using TPJHandleIStream
      //   name is constructed but file dates are available
      Stm := TPJHandleIStreamWrapper.Create(
        TFileStream.Create(cTestFileName, fmOpenRead),
        True
      );
    3:
      // Open a TPJFileIStream:
      //   name is name of file and dates are available
      Stm := TPJFileIStream.Create(cTestFileName, fmOpenRead);
  end;
  // Show the stats for the file: this will vary depending how opened
  ShowStats(Stm);
  // If we explicitly opened file (option 1) then we need to close it
  if FileH <> 0 then
    FileClose(FileH);
end;

procedure TIStreamWrapForm.ShowMemStmSize;
var
  Stats: TStatStg;  // stats for memory stream
begin
  // We get size by examining stream's stats: specifying STATFLAG_NONAME means
  // we don't get stream's name in structure and therefore don't need to free it
  // using IMalloc
  OleCheck(fIMemStm.Stat(Stats, STATFLAG_NONAME));
  // Show size on status bar
  UpdateStatusBar(0, 'Memory Stream Size = ' + IntToStr(Stats.cbSize));
end;

procedure TIStreamWrapForm.UpdateStatusBar(const PanelID: Integer;
  const Msg: string);
  {Displays given text in given status bar panel}
begin
  sbStatus.Panels[PanelID].Text := Msg;
end;

end.

