{
 * PJAbout.pas
 *
 * About Dialog Box component. Component that displays an about dialog box that
 * can get displayed information either from properties or from version
 * information.
 *
 * $Rev$
 * $Date$
 *
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
 * The Original Code is PJAbout.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1998-2009 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJAbout;


interface


{ Find if we have a Delphi 6 or higher compiler }
{$DEFINE DELPHI6ANDUP}
{$IFDEF VER80}  {Delphi 1}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER90}  {Delphi 2}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER93}  {C++ Builder 1}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER100} {Delphi 3}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER110} {C++ Builder 3}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER120} {Delphi 4}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER125} {C++ Builder 4}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER130} {Delphi 5}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}


uses
  { Delphi }
  Forms, ExtCtrls, StdCtrls, Buttons, Classes, Controls,
  {$IFDEF WIN32}
    Windows,
  {$ELSE}
    WinTypes, WinProcs, Dialogs,
  {$ENDIF}
  Graphics,
  { DelphiDabbler components }
  {$IFDEF WIN32}
    PJVersionInfo;
  {$ELSE}
    VerInfo;
  {$ENDIF}


{$R *.DFM}


type

  {
  TPJAboutBoxForm:
    Form class that defines the about box component's dialog box.
  }
  TPJAboutBoxForm = class(TForm)
    Bevel1: TBevel;
    Bevel2: TBevel;
    Bevel3: TBevel;
    ProgramLbl: TLabel;
    VersionLbl: TLabel;
    CopyrightLbl: TLabel;
    NotesLbl: TLabel;
    IconImg: TImage;
    DoneBtn: TBitBtn;
  end;

  {
  TPJAboutBtnPlacing:
    Determines horizontal placement of OK button in about dialog.
  }
  TPJAboutBtnPlacing = (abpLeft, abpCentre, abpRight);

  {
  TPJAboutBtnKinds:
    Determines caption of about dialog's OK button.
  }
  TPJAboutBtnKinds = (abkOK, abkDone, abkClose, abkCancel);

  {
  TPJAboutBtnGlyphs:
    Determines the glyph displayed on about dialog's OK button.
  }
  TPJAboutBtnGlyphs = (abgOK, abgCancel, abgIgnore, abgClose, abgNone);

  {
  TPJAboutPosition:
    Determines whether about dialog's position (centred or offset) is relative
    to screen, desktop or owning form.
  }
  TPJAboutPosition = (
    abpScreen,    {position relative to screen}
    abpDesktop,   {position relative to desktop workarea:
                  same as abpScreen on 16 bit windows}
    abpOwner      {position relative to owner control:
                  acts same as abpScreen if no owner or owner not win control}
  );

  {
  TPJAboutBoxDlg:
    The about box component class.
  }
  TPJAboutBoxDlg = class(TComponent)
  private
    {Property storage}
    fTitle: string;
    fProgramName: string;
    fVersion: string;
    fCopyright: string;
    fNotes: string;
    fButtonPlacing: TPJAboutBtnPlacing;
    fButtonKind: TPJAboutBtnKinds;
    fButtonGlyph: TPJAboutBtnGlyphs;
    fAutoDetectGlyphs: Boolean;
    fButtonHeight: Integer;
    fButtonWidth: Integer;
    fCentreDlg: Boolean;
    fDlgLeft: Integer;
    fDlgTop: Integer;
    fVersionInfo: TPJVersionInfo;
    fHelpContext: THelpContext;
    fPosition: TPJAboutPosition;
    {$IFDEF WIN32}
    fUseOwnerAsParent: Boolean;
    fUseOSStdFonts: Boolean;
    {$ENDIF}
    {Helper methods}
    procedure CentreInRect(Dlg: TPJAboutBoxForm; Rect: TRect);
      {Centres dialog box within a rectangle.
        @param Dlg [in] Reference to dialog box form.
        @param Rect [in] Rectangle within which dialog is centred.
      }
    procedure OffsetFromPoint(Dlg: TPJAboutBoxForm; TopLeft: TPoint);
      {Offsets dialog box by amount specified by DlgLeft and DlgTop properties.
        @param Dlg [in] Reference to dialog box form.
        @param Co-ordinates point from which dialog is to be offset.
      }
    procedure KeepOnScreen(Dlg: TPJAboutBoxForm);
      {Adjusts dialog box if necessary so that it appears wholly on screen.
        @param Dlg [in] Reference to dialog box form.
      }
    function DesktopWorkArea: TRect;
      {Gets desktop work area as rectangle.
        @return Win 32: the desktop excluding task bar and toolbars. Win 16:
          area of whole screen.
      }
    function ScreenArea: TRect;
      {Gets screen area as rectangle.
        @return Rectangle defining screen.
      }
    {$IFDEF WIN32}
    procedure SetParentToOwner(Dlg: TWinControl);
      {Sets parent of this dialog box to the window handle of the dialog's
      owner, if the owner is a window control.
        @param Dlg [in] Reference to dialog box object.
      }
    procedure SetDefaultFont(Font: TFont);
      {Set a font to underlying OSs default font.
        @param Font [in] Font to be set to default font.
      }
    {$ENDIF}
  protected
    procedure Notification(AComponent: TComponent;
      Operation: TOperation); override;
      {Check to see if any linked version info component is being deleted and
      clears reference to it if so.
        @param AComponent [in] Component being added or removed.
        @param Operations [in] Indicates whether component being added or
          removed.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Component constructor. Sets up dialog box.
        @param AOwner [in] Component that owns this component.
      }
    procedure Execute;
      {Displays the dialog box.
      }
  published
    property Title: string
      read fTitle write fTitle;
      {The text appearing in the dialog box title bar: default is "About"}
    property ProgramName: string
      read fProgramName write fProgramName;
      {The name of the program: appears on 1st line of about box - ignored if
      the VersionInfo property is assigned}
    property Version: string
      read fVersion write fVersion;
      {The program version: appears on 2nd line of about box - ignored if the
      VersionInfo property is assigned}
    property Copyright: string
      read fCopyright write fCopyright;
      {The program copyright message: appears on 3rd line of about box -
      ignored if the VersionInfo property is assigned}
    property Notes: string
      read fNotes write fNotes;
      {Notes about program: last entry in about box where text wraps up to 3
      lines - ignored if the VersionInfo property is assigned}
    property ButtonPlacing: TPJAboutBtnPlacing
      read fButtonPlacing write fButtonPlacing
      default abpCentre;
      {Placing of "Done" button: left, centre or right of bottom of box}
    property ButtonKind: TPJAboutBtnKinds
      read fButtonKind write fButtonKind
      default abkOK;
      {Kind of button: determines text that appears on button - 'OK', 'Cancel',
      'Done' or 'Close'}
    property ButtonGlyph: TPJAboutBtnGlyphs
      read fButtonGlyph write fButtonGlyph
      default abgOK;
      {Determines which glyph, if any, to display on dialog's OK button. The
      AutoDetectGlyphs property influences whether or not the glyph is actually
      is displayed}
    property AutoDetectGlyphs: Boolean
      read fAutoDetectGlyphs write fAutoDetectGlyphs
      default True;
      {Determines whether any image assigned to the ButtonGlyph property is
      displayed. If AutoDetectGlyphs is true then whether the image is displayed
      depends on several factors. On Windows 3.1 the glyph is displayed if
      Delphi 1's global MsgDlgGlyphs is true. On 32 bit Windows ButtonGlyph is
      ignored}
    property ButtonHeight: Integer
      read fButtonHeight write fButtonHeight
      {$IFDEF WIN32}
        default 25;
      {$ELSE}
        default 33;
      {$ENDIF}
      {The height of the button}
    property ButtonWidth: Integer
      read fButtonWidth write fButtonWidth
      {$IFDEF WIN32}
        default 75;
      {$ELSE}
        default 89;
      {$ENDIF}
      {The width of the button}
    property DlgLeft: Integer
      read fDlgLeft write fDlgLeft stored True
      default 0;
      {Offset of left side of about box. Offset may be relative to screen,
      desktop or owner form, depending on Position property. DlgLeft is ignored
      if CentreDlg is true}
    property DlgTop: Integer
      read fDlgTop write fDlgTop stored True
      default 0;
      {Offset of top of about box. Offset may be relative to screen, desktop or
      owner form, depending on Position property. DlgTop is ignored if CentreDlg
      is true}
    property CentreDlg: Boolean
      read fCentreDlg write fCentreDlg stored True
      default True;
      {When true the about box is centred relative to screen, desktop or owner
      form, depending on Position property}
    property VersionInfo: TPJVersionInfo
      read fVersionInfo write fVersionInfo;
      {Link to a TPJVersionInfo component. If this property is not nil then the
      ProductName, ProductVersion, LegalCopyright & Comments properties of that
      control are used instead of the ProgramName, Version, Copyright and Notes
      properties of this component}
    property HelpContext: THelpContext
      read fHelpContext write fHelpContext
      default 0;
      {Sets help context for dialog box: if this is non-zero pressing F1 when
      dialog box is displayed causes help topic with given context number in
      application's help file to be displayed}
    property Position: TPJAboutPosition
      read fPosition write fPosition
      default abpDesktop;
      {Determines whether CentreDlg, DlgTop and DlgLeft properties act relative
      to screen, desktop or owning form}
    {$IFDEF WIN32}
    property UseOwnerAsParent: Boolean
      read fUseOwnerAsParent write fUseOwnerAsParent
      default False;
      {When true sets window handle of dialog's owner, if any, as dialog's
      parent window. Property available on Win32 only. Provided for use when
      application's main form, rather than application window, is directly
      displayed in task bar. Set this to prevent main window from being able to
      be displayed in front of this dialog box}
    property UseOSStdFonts: Boolean
      read fUseOSStdFonts write fUseOSStdFonts
      default False;
      {When true causes dialog to use OSs standard fonts. This property is
      mainly of use to cause XP and Vista to use their differing default fonts
      in the dialog box. Property available on Win32 only}
    {$ENDIF}  
  end;


procedure Register;
  {Register this component.
  }


implementation


{ Component registration routine }

procedure Register;
  {Register this component.
  }
begin
  RegisterComponents('DelphiDabbler', [TPJAboutBoxDlg]);
end;


{ TPJAboutBoxDlg }

procedure TPJAboutBoxDlg.CentreInRect(Dlg: TPJAboutBoxForm; Rect: TRect);
  {Centres dialog box within a rectangle.
    @param Dlg [in] Reference to dialog box form.
    @param Rect [in] Rectangle within which dialog is centred.
  }
begin
  {Centre dialog in rectangle}
  Dlg.Left := (Rect.Right - Rect.Left - Dlg.Width) div 2 + Rect.Left;
  Dlg.Top := (Rect.Bottom - Rect.Top - Dlg.Height) div 2 + Rect.Top;
  {Ensure dialog is wholly on screen}
  KeepOnScreen(Dlg);
end;

constructor TPJAboutBoxDlg.Create(AOwner: TComponent);
  {Component constructor. Sets up dialog box.
    @param AOwner [in] Component that owns this component.
  }
begin
  inherited Create(AOwner);
  {Set default property values}
  fButtonPlacing := abpCentre;      {place button at centre of box}
  fButtonKind := abkOK;             {button kind is "OK"}
  fButtonGlyph := abgOK;            {use OK glyph}
  {$IFDEF WIN32}
    fButtonHeight := 25;            {default button height - 32 bit}
    fButtonWidth := 75;             {default button width - 32 bit}
  {$ELSE}
    fButtonHeight := 33;            {default button height - 16 bit}
    fButtonWidth := 89;             {default button width - 16 bit}
  {$ENDIF}
  fTitle := 'About';                {default caption}
  fCentreDlg := True;               {ensures dialog box is centred}
  fPosition := abpDesktop;          {ensures positioning relative to desktop}
  fAutoDetectGlyphs := True;        {default property value}
end;

function TPJAboutBoxDlg.DesktopWorkArea: TRect;
  {Gets desktop work area as rectangle.
    @return Win 32: the desktop excluding task bar and toolbars. Win 16: area of
      whole screen.
  }
begin
  {$IFDEF WIN32}
    {$IFDEF DELPHI6ANDUP}
      {Delphi 6 up: get desktop area from screen object}
      Result := Screen.WorkAreaRect;
    {$ELSE}
      {Delphi 2-5: get desktop area directly from Windows}
      SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
    {$ENDIF}
  {$ELSE}
    {Delphi 1: use whole screen}
    Result := ScreenArea;
  {$ENDIF}
end;

procedure TPJAboutBoxDlg.Execute;
  {Displays the dialog box.
  }
var
  Dlg: TPJAboutBoxForm;             {dialog box instance}
  UseButtonGlyphProperty: Boolean;  {flag true if to use ButtonGlyph property}
begin
  {Create dialog box instance}
  Dlg := TPJAboutBoxForm.Create(Owner);
  try

    {$IFDEF WIN32}
      {decide if to use dialog's owner as parent}
      if fUseOwnerAsParent then
        SetParentToOwner(Dlg);

      {decide on kind of font to use}
      if fUseOSStdFonts then
        {get standard font from windows}
        SetDefaultFont(Dlg.Font)
      else
        {use std font size for 32 bit Windows
        (in Win16 we keep form's font size}
        Dlg.Font.Size := 8;
    {$ENDIF}

    {Set window caption}
    Dlg.Caption := fTitle;

    if fVersionInfo <> nil then
    begin
      {Get displayed text from linked TPJVersionInfo component properties}
      Dlg.ProgramLbl.Caption := fVersionInfo.ProductName;
      Dlg.VersionLbl.Caption := fVersionInfo.ProductVersion;
      Dlg.CopyrightLbl.Caption := fVersionInfo.LegalCopyright;
      Dlg.NotesLbl.Caption := fVersionInfo.Comments;
    end
    else
    begin
      {No linked TPJVersionInfo: get displayed text from string properties. If
      Program name not specified, use application title}
      if ProgramName = '' then
        Dlg.ProgramLbl.Caption := Application.Title
      else
        Dlg.ProgramLbl.Caption := ProgramName;
      Dlg.VersionLbl.Caption := Version;
      Dlg.CopyrightLbl.Caption := Copyright;
      Dlg.NotesLbl.Caption := Notes;
    end;

    {Set icon to application icon}
    Dlg.IconImg.Picture.Graphic := Application.Icon;

    {Configure "done" button}
    {set button size}
    Dlg.DoneBtn.Height := fButtonHeight;
    Dlg.DoneBtn.Width := fButtonWidth;
    {place button horizontally}
    case ButtonPlacing of
      abpLeft:
        Dlg.DoneBtn.Left := Dlg.Bevel1.Left;
      abpRight:
        Dlg.DoneBtn.Left := Dlg.Bevel1.Width + Dlg.Bevel1.Left
          - Dlg.DoneBtn.Width;
      abpCentre:
        Dlg.DoneBtn.Left := (Dlg.ClientWidth - Dlg.DoneBtn.Width) div 2;
    end;
    {decide whether to use button glyph property, depending on value of
    AutoDetectGlyphs property and whether using 16 bit or 32 bit Delphi}
    if fAutoDetectGlyphs then
    begin
      {$IFDEF WIN32}
        UseButtonGlyphProperty := False;        {don't use glyphs in 32 bit}
      {$ELSE}
        UseButtonGlyphProperty := MsgDlgGlyphs; {use system variable in 16 bit}
      {$ENDIF}
    end
    else
      UseButtonGlyphProperty := True;
    if UseButtonGlyphProperty then
      {use ButtonGlyph property: load one bitmap resources present in all Delphi
      apps}
      case ButtonGlyph of
        abgOK:
          Dlg.DoneBtn.Glyph.Handle := LoadBitmap(HInstance, 'BBOK');
        abgCancel:
          Dlg.DoneBtn.Glyph.Handle := LoadBitmap(HInstance, 'BBCANCEL');
        abgIgnore:
          Dlg.DoneBtn.Glyph.Handle := LoadBitmap(HInstance, 'BBIGNORE');
        abgClose:
          Dlg.DoneBtn.Glyph.Handle := LoadBitmap(HInstance, 'BBCLOSE');
        abgNone:
          Dlg.DoneBtn.Glyph := nil;
      end
    else
      {ignore ButtonGlyphs property: don't use glyphs}
      Dlg.DoneBtn.Glyph := nil;
    {set button text per button kind property}
    case ButtonKind of
      abkOK: Dlg.DoneBtn.Caption := 'OK';
      abkDone: Dlg.DoneBtn.Caption := 'Done';
      abkClose: Dlg.DoneBtn.Caption := 'Close';
      abkCancel: Dlg.DoneBtn.Caption := 'Cancel';
    end;
    {adjust dialog box height according to button height}
    Dlg.ClientHeight := 166 + fButtonHeight;

    {Position dialog on screen}
    if fCentreDlg then
    begin
      {Centre dialog per Position property: ignore DlgLeft & DlgTop}
      case fPosition of
        abpScreen:
          {centre on screen}
          CentreInRect(Dlg, ScreenArea);
        abpDesktop:
          {centre on destop's work area}
          CentreInRect(Dlg, DesktopWorkArea);
        abpOwner:
        begin
          if (Owner is TWinControl) then
            {centre over owner control}
            CentreInRect(Dlg, (Owner as TWinControl).BoundsRect)
          else
            {no owner or owner not win control: centre on screen}
            CentreInRect(Dlg, ScreenArea);
        end;
      end;
    end
    else
    begin
      {position per DlgLeft and DlgTop and adjust to keep on screen}
      case fPosition of
        abpScreen:
          {offset relative to screen}
          OffsetFromPoint(Dlg, ScreenArea.TopLeft);
        abpDesktop:
          {offset relative to top left of desktop workarea}
          OffsetFromPoint(Dlg, DesktopWorkArea.TopLeft);
        abpOwner:
        begin
          if (Owner is TWinControl) then
            {offset relative to owner control}
            OffsetFromPoint(Dlg, (Owner as TWinControl).BoundsRect.TopLeft)
          else
            {no owner or not win control: offset relative to screen}
            OffsetFromPoint(Dlg, ScreenArea.TopLeft);
        end;
      end;
    end;

    {Set dialog's help context}
    Dlg.HelpContext := fHelpContext;

    {Show the dialog}
    Dlg.ShowModal;

  finally
    {Free the dialog box instance}
    Dlg.Free;
  end;
end;

procedure TPJAboutBoxDlg.KeepOnScreen(Dlg: TPJAboutBoxForm);
  {Adjusts dialog box if necessary so that it appears wholly on screen.
    @param Dlg [in] Reference to dialog box form.
  }
var
  DisplayBounds: TRect; {bounds of available display area}
begin
  {Calculate bounds of display area}
  DisplayBounds := DesktopWorkArea;
  {Adjust horizontally}
  if Dlg.Left < DisplayBounds.Left then
    Dlg.Left := DisplayBounds.Left
  else if Dlg.Left + Dlg.Width > DisplayBounds.Right then
    Dlg.Left := DisplayBounds.Right - Dlg.Width;
  {Adjust vertically}
  if Dlg.Top < DisplayBounds.Top then
    Dlg.Top := DisplayBounds.Top
  else if Dlg.Top + Dlg.Height > DisplayBounds.Bottom then
    Dlg.Top := DisplayBounds.Bottom - Dlg.Height;
end;

procedure TPJAboutBoxDlg.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Check to see if any linked version info component is being deleted and clears
  reference to it if so.
    @param AComponent [in] Component being added or removed.
    @param Operations [in] Indicates whether component being added or removed.
  }
begin
  if (Operation = opRemove) and (AComponent = fVersionInfo) then
    fVersionInfo := nil;
end;

procedure TPJAboutBoxDlg.OffsetFromPoint(Dlg: TPJAboutBoxForm;
  TopLeft: TPoint);
  {Offsets dialog box by amount specified by DlgLeft and DlgTop properties.
    @param Dlg [in] Reference to dialog box form.
    @param Co-ordinates point from which dialog is to be offset.
  }
begin
  {Calculate wanted offsets}
  Dlg.Left := TopLeft.X + DlgLeft;
  Dlg.Top := TopLeft.Y + DlgTop;
  {Ensure dlg is wholly on screen}
  KeepOnScreen(Dlg);
end;

function TPJAboutBoxDlg.ScreenArea: TRect;
  {Gets screen area as rectangle.
    @return Rectangle defining screen.
  }
begin
  Result := Rect(0, 0, Screen.Width, Screen.Height);
end;

{$IFDEF WIN32}
procedure TPJAboutBoxDlg.SetDefaultFont(Font: TFont);
  {Set a font to underlying OSs default font.
    @param Font [in] Font to be set to default font.
  }
var
  LogFont: TLogFont;  {logical font structure}
  FontHandle: HFONT;  {handle of required font}
begin
  {we treat icon desktop icon font as default if supported, or use default gui
  font otherwise}
  if SystemParametersInfo(
    SPI_GETICONTITLELOGFONT, SizeOf(LogFont), @LogFont, 0
  ) then
    FontHandle := CreateFontIndirect(LogFont)
  else
    FontHandle := GetStockObject(DEFAULT_GUI_FONT);
  Font.Handle := FontHandle;
end;

procedure TPJAboutBoxDlg.SetParentToOwner(Dlg: TWinControl);
  {Sets parent of this dialog box to the window handle of the dialog's owner, if
  the owner is a window control.
    @param Dlg [in] Reference to dialog box object.
  }
begin
  if Dlg.Owner is TWinControl then
    SetWindowLong(
      Dlg.Handle,
      GWL_HWNDPARENT,
      (Dlg.Owner as TWinControl).Handle
    );
end;
{$ENDIF}

end.
