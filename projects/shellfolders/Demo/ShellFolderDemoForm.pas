{
 * ShellFolderDemoForm.pas
 *
 * Main form and code for Shell Folders Unit demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2003-2013.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}


unit ShellFolderDemoForm;


interface

// Minimum compiler for this project is Delphi 7.
{$UNDEF RTLNameSpaces}
{$IF CompilerVersion >= 23.0} // Delphi XE2
  {$DEFINE RTLNameSpaces}
{$IFEND}

uses
  {$IFNDEF RTLNameSpaces}
  ImgList, Controls, StdCtrls, ComCtrls, Classes, ExtCtrls, Forms, ShlObj,
  Windows, Graphics,
  {$ELSE}
  Vcl.ImgList, Vcl.Controls, Vcl.StdCtrls, Vcl.ComCtrls, System.Classes,
  Vcl.ExtCtrls, Vcl.Forms, Winapi.ShlObj, Winapi.Windows, Vcl.Graphics,
  {$ENDIF}
  PJShellFolders;

type
  TForm1 = class(TForm)
    btnBrowse: TButton;
    BrowseDlg: TPJBrowseDialog;
    imgFolderLarge: TImage;
    ilLarge: TImageList;
    LV: TListView;
    rbOnSelChangeEx: TRadioButton;
    rbOnSelChange: TRadioButton;
    SpecialFolderInfo: TPJSpecialFolderInfo;
    chkBoldHeadText: TCheckBox;
    gpOptions: TGroupBox;
    chkShowHelp: TCheckBox;
    chkContextHelp: TCheckBox;
    chkStatusText: TCheckBox;
    chkDirsOnly: TCheckBox;
    chkNewDlgStyle: TCheckBox;
    lblIcon: TLabel;
    lblFolder: TLabel;
    imgFolderSmall: TImage;
    ilSmall: TImageList;
    chkHideMakeFolderBtn: TCheckBox;
    chkEditBox: TCheckBox;
    chkHint: TCheckBox;
    chkOnValidationFailed: TCheckBox;
    procedure btnBrowseClick(Sender: TObject);
    procedure BrowseDlgClose(Sender: TObject);
    procedure BrowseDlgInitialise(Sender: TObject);
    procedure BrowseDlgSelChange(Sender: TObject; FolderName,
      DisplayName: String; var StatusText: String; var OKEnabled: Boolean);
    procedure BrowseDlgSelChangeEx(Sender: TObject; PIDL: PItemIDList;
      var StatusText: String; var OKEnabled: Boolean);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure LVDblClick(Sender: TObject);
    procedure rbOnSelChangeExClick(Sender: TObject);
    procedure rbOnSelChangeClick(Sender: TObject);
    procedure LVSelectItem(Sender: TObject; Item: TListItem;
      Selected: Boolean);
    procedure DlgOptionsClick(Sender: TObject);
    procedure BrowseDlgValidationFailed(Sender: TObject; const EditText: string;
      var CanClose: Boolean);
    procedure chkOnValidationFailedClick(Sender: TObject);
  private
    fPJFont: TFont;
    procedure DisplaySpecialFolders;
    function IsSelectionSupported: Boolean;
    procedure SetOptionCheckState(const CheckBox: TCheckBox);
  end;

var
  Form1: TForm1;


implementation


uses
  {$IFNDEF RTLNameSpaces}
  SysUtils, Messages, ShellAPI, Dialogs;
  {$ELSE}
  System.SysUtils, Winapi.Messages, Winapi.ShellAPI, Vcl.Dialogs;
  {$ENDIF}


{$R *.DFM}


procedure TForm1.BrowseDlgClose(Sender: TObject);
  {Clear any displayed icon when browse dialog closes}
begin
  imgFolderLarge.Picture.Bitmap.Assign(nil);
  imgFolderSmall.Picture.Bitmap.Assign(nil);
  lblFolder.Caption := 'PATH:';
end;

procedure TForm1.BrowseDlgInitialise(Sender: TObject);
  {Customises browse for folder dialog box}
const
  cHeadlineTextID = $3742;
begin
  // Set bold font in the headline control??
  if chkBoldHeadText.Checked then
    SendMessage(
      GetDlgItem(BrowseDlg.Handle, cHeadlineTextID),
      WM_SETFONT,
      Integer(fPJFont.Handle),
      MakeLParam(1, 0)
    );
end;

procedure TForm1.BrowseDlgSelChange(Sender: TObject; FolderName,
  DisplayName: String; var StatusText: String; var OKEnabled: Boolean);
  {Event triggered when selection changes in browse dialog}
begin
  // Simply show display name in status text
  StatusText := DisplayName;
  // and always enable button
  OKEnabled := True;
end;

procedure TForm1.BrowseDlgSelChangeEx(Sender: TObject; PIDL: PItemIDList;
  var StatusText: String; var OKEnabled: Boolean);
  {Event triggered when selection changes in browse dialog}

  function IconIndex(PIDL: PItemIDList): Integer;
    {Return index of icon associated with given PIDL}
  var
    FI: TSHFileInfo;
  begin
    SHGetFileInfo(PChar(PIDL), 0, FI, SizeOf(FI), SHGFI_ICON + SHGFI_PIDL);
    Result := FI.iIcon;
  end;

var
  Path: string;
  IsVirtual: Boolean;
  DisplayName: string;
  IconIdx: Integer;
begin
  // Get index of icon for selected folder in system image list
  IconIdx := IconIndex(PIDL);
  // Get display name and path for selected folder and decide if virtual
  DisplayName := PIDLToFolderDisplayName(PIDL);
  Path := PIDLToFolderPath(PIDL);
  IsVirtual := Path = '';
  // Display folder info in status area of dlg (+ image list index)
  if not IsVirtual then
  begin
    if Path <> '' then
      if Length(Path) > 40 then
        StatusText := Format('File System: ... %s', [DisplayName])
      else
        StatusText := Format('File System: %s', [Path]);
  end
  else
    StatusText := Format('Virtual: %s', [DisplayName]);
  // Display large and small icons associated with selected folder on main form
  ilLarge.GetBitmap(IconIdx, imgFolderLarge.Picture.Bitmap);
  ilSmall.GetBitmap(IconIdx, imgFolderSmall.Picture.Bitmap);
  imgFolderLarge.Refresh;
  imgFolderSmall.Refresh;
  lblFolder.Caption := 'PATH: ' + Path;
end;

procedure TForm1.BrowseDlgValidationFailed(Sender: TObject;
  const EditText: string; var CanClose: Boolean);
  {Handles event triggered when an invalid path is entered in the browse for
  folders dialog box and an attempt is made to close the dialog box}
begin
  CanClose := MessageDlg(
    Format(
      'Folder "%s" does not exist.'#10#10'Close the dialog anyway?', [EditText]
    ),
    mtConfirmation,
    [mbYes, mbNo],
    0
  ) = mrYes;
end;

procedure TForm1.btnBrowseClick(Sender: TObject);
  {Display browse for folder dlg box for selected folder}         
var
  Item: TListItem;
  Msg: string;
begin
  Item := LV.Selected;
  Assert(Item <> nil);
  BrowseDlg.RootFolderID := StrToSpecialFolderID(Item.Caption);
  SpecialFolderInfo.FolderID := BrowseDlg.RootFolderID;
  BrowseDlg.Headline := 'Browsing for folders under: '
    + SpecialFolderInfo.DisplayName;
  BrowseDlg.FolderName := '';   // don't remember last time's folder
  if BrowseDlg.Execute then
  begin
    Msg := Format(
      'Folder chosen:'#13'    Folder Name: %s'#13'    Display Name: %s',
      [BrowseDlg.FolderName, BrowseDlg.DisplayName]
    );
    // display info about selected folder
    ShowMessage(Msg);
  end;
end;

procedure TForm1.chkOnValidationFailedClick(Sender: TObject);
  {Toggles whether browse dialog's OnValidationFailed event has a handler
  assigned}
begin
  if chkOnValidationFailed.Checked then
    BrowseDlg.OnValidationFailed := BrowseDlgValidationFailed
  else
    BrowseDlg.OnValidationFailed := nil;
end;

procedure TForm1.DisplaySpecialFolders;
  {Displays info about special folders}
var
  DisplayName, FolderPath: string;
  Enum: IPJSpecialFolderEnum;
  Item: TListItem;
begin
  // iterate special folders using enumerator
  Enum := TPJSpecialFolderEnum.Create;
  while not Enum.AtEnd do
  begin
    SpecialFolderInfo.FolderID := Enum.Next;
    if SpecialFolderInfo.IsSupported then
    begin
      // folder is supported: gather into
      if SpecialFolderInfo.IsVirtual then
        FolderPath := '<virtual folder>'
      else
        FolderPath := SpecialFolderInfo.Path;
      DisplayName := SpecialFolderInfo.DisplayName;
    end
    else
    begin
      // folder is not supported
      DisplayName := '<not supported>';
      FolderPath := '';
    end;
    // add collected info about folder to list view
    Item := LV.Items.Add;
    Item.Caption := SpecialFolderIdToStr(SpecialFolderInfo.FolderID);
    Item.SubItems.Add(DisplayName);
    Item.SubItems.Add(FolderPath);
  end;
end;

procedure TForm1.DlgOptionsClick(Sender: TObject);
  {OnClick event handler for dialog options check boxes. Updates dialog's
  Options property according to state of check box}
var
  CheckBox: TCheckBox;
begin
  CheckBox := Sender as TCheckBox;
  if CheckBox.Checked then
    BrowseDlg.Options := BrowseDlg.Options + [TPJBrowseDlgOption(CheckBox.Tag)]
  else
    BrowseDlg.Options := BrowseDlg.Options - [TPJBrowseDlgOption(CheckBox.Tag)];
end;

procedure TForm1.FormCreate(Sender: TObject);
  {Set up images, special dlg box font and display list of all supported special
  folders}

  function SysImgListHandle(WantLarge: Boolean): THandle;
    {Return handle to system image list containing large icons}
  var
    FI: TSHFileInfo;
    Flags: Word;
  begin
    Flags := SHGFI_SYSICONINDEX or SHGFI_ICON;
    if WantLarge then
      Flags := Flags or SHGFI_LARGEICON
    else
      Flags := Flags or SHGFI_SMALLICON;
    Result := SHGetFileInfo('C:\', 0, FI, SizeOf(FI), Flags);
  end;

begin
  // Make image lists containing large and small system images
  ilLarge.Handle := SysImgListHandle(True);
  ilSmall.Handle := SysImgListHandle(False);

  // Display the special folders
  DisplaySpecialFolders;

  // Create a font to use in browse for folder dlg headline
  fPJFont := TFont.Create;
  fPJFont.Assign(Self.Font);
  fPJFont.Style := [fsBold];

  // Set up dialog options check boxes
  chkShowHelp.Tag := Ord(boShowHelp);
  chkContextHelp.Tag := Ord(boContextHelp);
  chkStatusText.Tag := Ord(boStatusText);
  chkDirsOnly.Tag := Ord(boDirsOnly);
  chkNewDlgStyle.Tag := Ord(boNewDlgStyle);
  chkHideMakeFolderBtn.Tag := Ord(boHideMakeFolderBtn);
  chkEditBox.Tag := Ord(boEditBox);
  chkHint.Tag := Ord(boHint);
  SetOptionCheckState(chkShowHelp);
  SetOptionCheckState(chkContextHelp);
  SetOptionCheckState(chkStatusText);
  SetOptionCheckState(chkDirsOnly);
  SetOptionCheckState(chkNewDlgStyle);
  SetOptionCheckState(chkHideMakeFolderBtn);
  SetOptionCheckState(chkEditBox);
  SetOptionCheckState(chkHint);

  // Set up other controls
  chkOnValidationFailed.Checked := Assigned(BrowseDlg.OnValidationFailed);
end;

procedure TForm1.FormDestroy(Sender: TObject);
  {Tidy up - free special font}
begin
  fPJFont.Free;
end;

function TForm1.IsSelectionSupported: Boolean;
  {Return true if selected folder is supported on this system. Returns false if
  not supported or nothing selected}
var
  Item: TListItem;
begin
  Item := LV.Selected;
  if Assigned(Item) then
  begin
    SpecialFolderInfo.FolderID := StrToSpecialFolderID(Item.Caption);
    Result := SpecialFolderInfo.IsSupported;
  end
  else
    Result := False;
end;

procedure TForm1.LVDblClick(Sender: TObject);
  {Display browse dlg box for selected folder, if supported}
begin
  if IsSelectionSupported then
    btnBrowseClick(Self)
  else
    MessageBox(Handle, 'Shell folder not supported', 'Shell Folder Demo',
      MB_ICONERROR or MB_OK);
end;

procedure TForm1.LVSelectItem(Sender: TObject; Item: TListItem;
  Selected: Boolean);
  {Enable / disable browse button according to if selected folder is supported}
begin
  if Assigned(Item) and Selected then
    btnBrowse.Enabled := IsSelectionSupported;
end;

procedure TForm1.rbOnSelChangeExClick(Sender: TObject);
  {We use OnSelChangeEx event handler with browse dlg}
begin
  BrowseDlg.OnSelChangeEx := BrowseDlgSelChangeEx;
  BrowseDlg.OnSelChange := nil;
end;

procedure TForm1.rbOnSelChangeClick(Sender: TObject);
  {We use OnSelChange event handler with browse dlg}
begin
  BrowseDlg.OnSelChangeEx := nil;
  BrowseDlg.OnSelChange := BrowseDlgSelChange;
end;

procedure TForm1.SetOptionCheckState(const CheckBox: TCheckBox);
  {Sets check box state according to whether related dialog box option is
  included in options}
begin
  // We store related option in check box's Tag property
  CheckBox.Checked := TPJBrowseDlgOption(CheckBox.Tag) in BrowseDlg.Options;
end;

end.

