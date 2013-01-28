{
 * Help dialog form for IStreamWrap Stream Library Demo Program.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmIStreamWrapHelp;

interface

uses
  // Delphi
  StdCtrls, ComCtrls, Classes, Controls, Forms;

{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0} // >= Delphi 7
    {$WARN UNSAFE_CODE OFF}
    {$WARN UNSAFE_CAST OFF}
    {$WARN UNSAFE_TYPE OFF}
  {$IFEND}
{$ENDIF}

type
  {
  TIStreamWrapHelpForm:
    Help dialog form class.
  }
  TIStreamWrapHelpForm = class(TForm)
    reDisplay: TRichEdit;
    btnClose: TButton;
    procedure FormCreate(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  end;

implementation

uses
  // Delphi
  Windows, ActiveX, ComObj,
  // delphiDabbler Stream Library
  PJIStreams;

{$R *.DFM}

{$R Help.res}   // contains rich text for display in dlg box

procedure TIStreamWrapHelpForm.btnCloseClick(Sender: TObject);
  {Close application}
begin
  Close;
end;

procedure TIStreamWrapHelpForm.FormCreate(Sender: TObject);
  {Load rich edit control with rich text stored as resource
    YES, I know there's an easier way to do this(!):
      ResStm := TResourceStream.Create(HInstance, 100, RT_RCDATA);
      reDisplay.LoadFromStream(ResStm);
      ResStm.Free;
    The point of this code is to show how you can get an IStream interface on
    the program's resources. I then copy to a memory stream which also has an
    IStream interface, rewind the stream and then finally use the mem stream
    itself to load the rich edit control!}
var
  ResIStm: IStream;       // IStream interface to rtf resource stream
  ResStats: TStatStg;     // stats for resource stream
  MemIStm: IStream;       // IStream interface to memory stream
  MemStm: TMemoryStream;  // memory stream
  Dummy1, Dummy2: Int64;  // dummy vars required by IStream methods
begin
  // Create IStream object on resource
  ResIStm := TPJIStreamWrapper.Create(
    TResourceStream.CreateFromID(HInstance, 100, RT_RCDATA),
    True
  );
  // Get size of resource stream
  OleCheck(ResIStm.Stat(ResStats, STATFLAG_NONAME));
  // Create memory stream and IStream interface wrapper that leaves it open
  MemStm := TMemoryStream.Create;
  try
    MemIStm := TPJIStreamWrapper.Create(MemStm);
    // Copy resource stream to memory stream
    OleCheck(ResIStm.CopyTo(MemIStm, ResStats.cbSize, Dummy1, Dummy2));
    // Rewind memory stream
    MemIStm.Seek(0, STREAM_SEEK_SET, Dummy1);
    // Load rich edit control from underlying memory stream
    reDisplay.Lines.LoadFromStream(MemStm);
  finally
    // Manually dispose of memory stream
    MemStm.Free;
  end;
end;

end.
