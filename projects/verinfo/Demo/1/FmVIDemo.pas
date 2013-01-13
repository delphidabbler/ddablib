{
 * FmVIDemo.pas
 *
 * Main form for Version Information Component VIDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2002-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmVIDemo;

{$UNDEF Supports_RTLNameSpaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 15.0}   // >= Delphi 7
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 23.0}   // Delphi XE2
    {$DEFINE Supports_RTLNameSpaces}
  {$IFEND}
{$ENDIF}

interface

uses
  // Delphi
  {$IFDEF Supports_RTLNameSpaces}
  Vcl.ComCtrls, Vcl.StdCtrls, Vcl.Buttons, Vcl.Dialogs, Vcl.Controls,
  System.Classes, Vcl.Forms, Winapi.Windows, Winapi.Messages, Vcl.ExtCtrls,
  {$ELSE}
  ComCtrls, StdCtrls, Buttons, Dialogs, Controls, Classes, Forms, Windows,
  Messages, ExtCtrls,
  {$ENDIF}
  // DelphiDabbler component
  PJVersionInfo;

type
  TMainForm = class(TForm)
    dlgBrowse: TOpenDialog;
    gpFixed: TGroupBox;
    lvFixed: TListView;
    gpVar: TGroupBox;
    lblTrans: TLabel;
    lblStr: TLabel;
    cmbTrans: TComboBox;
    lvStr: TListView;
    bvlSpacer1: TBevel;
    viInfo: TPJVersionInfo;
    sbFileName: TSpeedButton;
    edFileName: TEdit;
    btnRefresh: TButton;
    btnClose: TButton;
    sbHints: TStatusBar;
    procedure cmbTransChange(Sender: TObject);
    procedure lvMouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure btnRefreshClick(Sender: TObject);
    procedure sbFileNameClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
  private
    procedure ClearDisplay;
      // Clear list views and combo box
    procedure Display;
      // Display version info for current file, if any
    procedure DisplayFFI;
      // Display fixed file info for current file
    procedure DisplayFFIItem(const Index: Integer; const FFI: TVSFixedFileInfo);
      // Display the fixed file item per the given index fixed file info
      // structure
    procedure DisplayTransInfo;
      // Display translation information in combo and selects first item if any
    procedure DisplayStringInfo(const TransIdx: Integer);
      // Display standard string information for given translation
    procedure WMDropFiles(var Msg: TWMDropFiles); message WM_DROPFILES;
      // Catch name of file dropped on window and enter in edit control
  end;

var
  MainForm: TMainForm;

implementation

uses
  // Delphi
  {$IFDEF Supports_RTLNameSpaces}
  System.SysUtils, System.Math, WinApi.ShellAPI;
  {$ELSE}
  SysUtils, Math, ShellAPI;
  {$ENDIF}

{$R *.DFM}

const
  // Index of fixed file info in lvFixed
  cIdxFileVersion = 0;
  cIdxProductVersion = 1;
  cIdxFileFlagsMask = 2;
  cIdxFileFlags = 3;
  cIdxFileOS = 4;
  cIdxFileType = 5;
  cIdxFileSubType = 6;
  cFirstFFIIndex = 0;
  cLastFFIIndex = 6;

// The following types and constant table definitions provide information
// required to display descriptions of fixed file information codes from version
// resources

type
  TTableEntry = record
    // maps codes to descriptions
    Code: DWORD;
    Desc: string;
  end;

const
  cFileType: array[0..6] of TTableEntry =
    // maps file types to descriptions
  (
    (Code: VFT_APP; Desc: 'Application'),
    (Code: VFT_DLL; Desc: 'DLL'),
    (Code: VFT_DRV; Desc: 'Device driver'),
    (Code: VFT_FONT; Desc: 'Font'),
    (Code: VFT_STATIC_LIB; Desc: 'Static link library'),
    (Code: VFT_VXD; Desc: 'Virtual device driver'),
    (Code: VFT_UNKNOWN; Desc: 'Unknown')
  );

  cFileOSBase: array[0..4] of TTableEntry =
    // maps base OS codes to descriptions
  (
    ( Code: VOS_NT; Desc: 'Windows NT' ),
    ( Code: VOS_DOS; Desc: 'MS-DOS' ),
    ( Code: VOS_OS232; Desc: 'OS2 32 bit' ),
    ( Code: VOS_OS216; Desc: 'OS2 16 bit' ),
    ( Code: VOS_UNKNOWN; Desc: 'Any' )
  );

  cFileOSTarget: array[0..4] of TTableEntry =
    // maps target OS codes to descriptions
  (
    ( Code: VOS__WINDOWS32; Desc: '32 bit Windows' ),
    ( Code: VOS__WINDOWS16; Desc: 'Windows 3.x' ),
    ( Code: VOS__PM32; Desc: 'Presentation Manager 32' ),
    ( Code: VOS__PM16; Desc: 'Presentation Manager 16' ),
    ( Code: VOS__BASE; Desc: 'Unknown' )
  );

// The following procedures extract the required descriptions of fixed file
// information codes from the tables above

function CodeToDesc(Code: DWORD;
  Table: array of TTableEntry): string;
  // Return description of given code using given table
var
  I: Integer;
begin
  Result := '';
  for I := Low(Table) to High(Table) do
    if Table[I].Code = Code then
    begin
      Result := Table[I].Desc;
      Break;
    end;
end;

function FileOSDesc(OS: DWORD): string;
  // Describe OS
var
  Target, Base: DWORD;
begin
  // get target and base OS
  Target := OS and $0000FFFF;
  Base := OS and $FFFF0000;
  // build description
  if Base = VOS_UNKNOWN then
    Result := CodeToDesc(Target, cFileOSTarget)
  else if Target = VOS__BASE then
    Result := CodeToDesc(Base, cFileOSBase)
  else
    Result := Format('%s on %s',
      [CodeToDesc(Target, cFileOSTarget),
      CodeToDesc(Base, cFileOSBase)]);
end;

function FileFlagsToStr(Flags: DWORD): string;
  // Return string of file flags from given bit set
const
  cFileFlags: array[0..5] of TTableEntry =
  (
    (Code: VS_FF_DEBUG;        Desc: 'Debug'),
    (Code: VS_FF_PRERELEASE;   Desc: 'Pre-release'),
    (Code: VS_FF_PATCHED;      Desc: 'Patched'),
    (Code: VS_FF_PRIVATEBUILD; Desc: 'Private build'),
    (Code: VS_FF_INFOINFERRED; Desc: 'Inferred'),
    (Code: VS_FF_SPECIALBUILD; Desc: 'Special build')  );
var
  I: Integer;
begin
  Result := '';
  for I := Low(cFileFlags) to High(cFileFlags) do
    if Flags and cFileFlags[I].Code = cFileFlags[I].Code then
      if Result = '' then
        Result := cFileFlags[I].Desc
      else
        Result := Result + ', ' + cFileFlags[I].Desc
end;

function VerFmt(const MS, LS: DWORD): string;
  // Format the version number from the given DWORDs containing the info
begin
  Result := Format('%d.%d.%d.%d',
    [HiWord(MS), LoWord(MS), HiWord(LS), LoWord(LS)])
end;

{ TMainForm }

procedure TMainForm.btnCloseClick(Sender: TObject);
  // Close the app
begin
  Close;
end;

procedure TMainForm.btnRefreshClick(Sender: TObject);
  // Display version info for file name entered in edit control
begin
  // Store name of required file in version info component: this reads ver info
  viInfo.FileName := edFileName.Text;
  // Record file's name in caption
  Caption := Format('%s - %s',
    [Application.Title, ExtractFileName(viInfo.FileName)]);
  // Display info, if any
  Display
end;

procedure TMainForm.ClearDisplay;
  // Clear list views and combo box

  // ---------------------------------------------------------------------------
  procedure ClearLV(LV: TListView);
  var
    Idx: Integer;
  begin
    for Idx := 0 to Pred(LV.Items.Count) do
      if LV.Items[Idx].SubItems.Count > 0 then
        LV.Items[Idx].SubItems[0] := '';
  end;
  // ---------------------------------------------------------------------------

begin
  ClearLV(lvFixed);
  ClearLV(lvStr);
  cmbTrans.Items.Clear;
  cmbTrans.ItemIndex := -1;
end;

procedure TMainForm.cmbTransChange(Sender: TObject);
  // When user selects a translation from combo box, display its string info
begin
  DisplayStringInfo(cmbTrans.ItemIndex);
end;

procedure TMainForm.Display;
  // Display version info for current file, if any
begin
  // Display version info: display gets cleared and message displayed if no info
  if viInfo.HaveInfo then
  begin
    DisplayFFI;
    DisplayTransInfo;
    DisplayStringInfo(cmbTrans.ItemIndex);
  end
  else
  begin
    ClearDisplay;
    MessageDlg(
      'No version resource information available for ' + viInfo.FileName,
      mtInformation, [mbOK], 0);
  end;
end;

procedure TMainForm.DisplayFFI;
  // Display fixed file info for current file
var
  Idx: Integer; // scans thru all fixed file items
begin
  // Display each fixed file item
  for Idx := cFirstFFIIndex to cLastFFIIndex do
  begin
    // Ensure there's a sub item in list view for this item
    if lvFixed.Items[Idx].SubItems.Count = 0 then
      lvFixed.Items[Idx].SubItems.Add('');
    // Display the item
    DisplayFFIItem(Idx, viInfo.FixedFileInfo);
  end;
end;

procedure TMainForm.DisplayFFIItem(const Index: Integer;
  const FFI: TVSFixedFileInfo);
  // Display the fixed file item per the given index fixed file info structure

  // ---------------------------------------------------------------------------
  procedure AddItem(const Index: Integer; const Value: string);
    {Set the first sub item of the given list item to the given value}
  begin
    lvFixed.Items[Index].SubItems[0] := Value;
  end;
  // ---------------------------------------------------------------------------

begin
  // Display the required item
  case Index of
    cIdxFileVersion:
      AddItem(cIdxFileVersion,
        VerFmt(FFI.dwFileVersionMS, FFI.dwFileVersionLS));
    cIdxProductVersion:
      AddItem(cIdxProductVersion,
        VerFmt(FFI.dwProductVersionMS, FFI.dwProductVersionLS));
    cIdxFileFlagsMask:
      AddItem(cIdxFileFlagsMask,
        FileFlagsToStr(FFI.dwFileFlagsMask));
    cIdxFileFlags:
      AddItem(cIdxFileFlags, FileFlagsToStr(FFI.dwFileFlags));
    cIdxFileOS:
      AddItem(cIdxFileOS, FileOSDesc(FFI.dwFileOS));
    cIdxFileType:
      AddItem(cIdxFileType, CodeToDesc(FFI.dwFileType, cFileType));
    cIdxFileSubType:
      case FFI.dwFileType of
        VFT_FONT, VFT_DRV, VFT_VXD:
          AddItem(cIdxFileSubType, Format('%0.8X', [FFI.dwFileSubType]));
        else AddItem(cIdxFileSubType, 'None');
      end;
  end;
end;

procedure TMainForm.DisplayStringInfo(const TransIdx: Integer);
  // Display standard string information for given translation
const
  // Details of names of all standard string information
  cStrInfoNames: array[0..11] of string = (
    'Comments',
    'CompanyName',
    'FileDescription',
    'FileVersion',
    'InternalName',
    'LegalCopyright',
    'LegalTrademarks',
    'OriginalFileName',
    'PrivateBuild',
    'ProductName',
    'ProductVersion',
    'SpecialBuild'
  );
var
  Idx: Integer; // index of string info name in table
begin
  // Select required translation in version info component
  viInfo.CurrentTranslation := TransIdx;
  // Display each piece of string info
  for Idx := Low(cStrInfoNames) to High(cStrInfoNames) do
  begin
    // Ensure we have a sub-item of list item in which to display info
    if lvStr.Items[Idx].SubItems.Count = 0 then
      lvStr.Items[Idx].SubItems.Add('');
    // Display string info or empty string if translation is not valid
    if viInfo.CurrentTranslation > -1 then
      // display string information
      lvStr.Items[Idx].SubItems[0] :=
        viInfo.StringFileInfo[cStrInfoNames[Idx]]
    else
      // clear string information
      lvStr.Items[Idx].SubItems[0] := '';
  end;
end;

procedure TMainForm.DisplayTransInfo;
  // Display translation information in combo and selects first item if any
var
  TransIdx: Integer;  // loops thru all translations
begin
  // Set up translations in combo box: indexes of items in combo = trans index
  cmbTrans.Clear;
  for TransIdx := 0 to Pred(viInfo.NumTranslations) do
  begin
    viInfo.CurrentTranslation := TransIdx;
    cmbTrans.Items.Add(Format('%s - %s', [viInfo.Language, viInfo.CharSet]));
  end;
  // Select first translation if there is one
  if viInfo.NumTranslations > 0 then
    cmbTrans.ItemIndex := 0
  else
    cmbTrans.ItemIndex := -1;
end;

procedure TMainForm.FormCreate(Sender: TObject);
  // App starting: initialise (tell Windows we accept file drag/drops)
begin
  DragAcceptFiles(Handle, True);
end;

procedure TMainForm.FormDestroy(Sender: TObject);
  // App closing: tidy up (no more file drag/drops)
begin
  DragAcceptFiles(Handle, False);
end;

procedure TMainForm.lvMouseMove(Sender: TObject; Shift: TShiftState;
  X, Y: Integer);
  // Display any info that is too wide for list view column in pop-up window
  // when mouse passes over the item
const
  {$J+}
  LastItem: TListItem = nil;  // list item under cursor last time event called
  {$J-}
var
  LV: TListView;    // list view triggering this event
  Item: TListItem;  // list item, if any, under mouse cursor
  StrW: Integer;    // width of string in right column for current list item

  // ---------------------------------------------------------------------------
  procedure SetHint(const HintStr: string);
    {Set list view's hint to given string, and switch on hinting if string non-
    empty}
  begin
    LV.Hint := HintStr;
    LV.ShowHint := HintStr <> '';
    sbHints.AutoHint := HintStr = ''; // only auto hint for interactive controls
  end;
  // ---------------------------------------------------------------------------

begin
  // Get reference to list view triggering this event
  LV := Sender as TListView;
  // Get list item under mouse cursor if any
  Item := LV.GetItemAt(X, Y);
  if Item <> LastItem then
  begin
    // Item has changed: ensure any active hint is cancelled so any new one will
    // be displayed
    LastItem := Item;
    Application.CancelHint;
  end;
  if Assigned(Item)
    and (X > LV.Columns[0].WidthType)
    and (Item.SubItems.Count > 0) then
  begin
    // Mouse cursor is over right column of a valid list item
    // .. we display narative as hint only if narrative is wider than column
    StrW := LV.StringWidth(Item.SubItems[0]);
    if StrW > LV.Columns[1].WidthType - 12 then
      SetHint(Item.SubItems[0])
    else
      SetHint('');
  end
  else
    // Mouse is not over an area containing narative text: no hint
    SetHint('');
end;

procedure TMainForm.sbFileNameClick(Sender: TObject);
  // Get a file name from uer and display in edit box
begin
  if dlgBrowse.Execute then
    edFileName.Text := dlgBrowse.FileName;
end;

procedure TMainForm.WMDropFiles(var Msg: TWMDropFiles);
  // Catch name of file dropped on window and enter in edit control
var
  NumDropped: Integer;  // no of files dropped
  NameLen: Integer;     // length of a file name
  FileName: string;     // name of a dropped file
begin
  inherited;
  // Find number of files dropped
  NumDropped := DragQueryFile(Msg.Drop, Cardinal(-1), nil, 0);
  try
    if NumDropped > 0 then
    begin
      // Find size required for filename buffer (without terminal #0)
      NameLen := DragQueryFile(Msg.Drop, 0, nil, 0);
      // Get name of dropped file: only interested in first if more than 1
      SetLength(FileName, NameLen);   // Delphi adds space for terminal #0
      DragQueryFile(Msg.Drop, 0, PChar(FileName), NameLen + 1);
      // Place name of file in edit control
      edFileName.Text := FileName;
    end;
  finally
    // Release handle assoc. with drag/drop
    DragFinish(Msg.Drop);
  end;
end;

end.
