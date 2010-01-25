{ ##
  @PROJECT_NAME             ShellFolderDemo
  @PROJECT_DESC             Demonstrates shell folders unit.
  @FILE                     ShellFolderDemoForm.pas
  @COMMENTS                 Main form and code for demo program.
  @DEPENDENCIES             Requires TPJSpecialFolderInfo and TPJBrowseDialog.
  @LICENSE                  The demo is released under the Mozilla public
                            license (see below).
  @COPYRIGHT                Copyright (c) 2003-2007, Peter D Johnson.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 15/06/2003
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              1.1
      @DATE                 29/07/2003
      @COMMENTS             Updated - detail not known.
    )
    @REVISION(
      @VERSION              1.2
      @DATE                 22/08/2004
      @COMMENTS             Added code to demonstrate new style dialog.
    )
    @REVISION(
      @VERSION              1.3
      @DATE                 22/12/2005
      @COMMENTS             Fixed range check bug.
    )
    @REVISION(
      @VERSION              1.4
      @DATE                 03/07/2007
      @COMMENTS             + Refined to allow all browser dialog box options to
                              be configured from UI.
                            + Changed to display both small and large icon of
                              selected folder in browser dialog box. We now also
                              display path to selected folder, if any.
                            + Changed demo of browser dialog box customisation
                              making headline text bold instead of adding new
                              text control to dialog box.
    )
  )
}


{
 * ***** BEGIN LICENSE BLOCK *****
 *
 * Version: MPL 1.1
 *
 * The contents of this file are subject to the Mozilla Public License Version
 * 1.1 (the "License"); you may not use this file except in compliance with the
 * License. You may obtain a copy of the License at http://www.mozilla.org/MPL/
 *
 * Software distributed under the License is distributed on an "AS IS" basis,
 * WITHOUT WARRANTY OF ANY KIND, either express or implied. See the License for
 * the specific language governing rights and limitations under the License.
 *
 * The Original Code is ShellFolderDemoForm.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2003-2007 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit ShellFolderDemoForm;


interface


uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  ComCtrls, StdCtrls, ShlObj, ExtCtrls, ImgList,

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
  ShellAPI;


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
  // Make image list contain large system images
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
  SetOptionCheckState(chkShowHelp);
  SetOptionCheckState(chkContextHelp);
  SetOptionCheckState(chkStatusText);
  SetOptionCheckState(chkDirsOnly);
  SetOptionCheckState(chkNewDlgStyle);
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

