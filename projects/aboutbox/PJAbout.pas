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
 * Portions created by the Initial Developer are Copyright (C) 1998-2011 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributors:
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJAbout;


interface


// Set conditionally defined symbols and switch of code warnings
{$DEFINE RESOURCESTRING}  // defined if resourcestring keyword supported
{$UNDEF SCREENWORKAREA}   // defined if Screen.WorkAreaRect property supported
{$UNDEF RTLNAMESPACES}    // defined if RTL units are qualified by namespace
{$IFDEF VER90}  // Delphi 2
  {$UNDEF RESOURCESTRING}
{$ENDIF}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 14.0} // Delphi 6
    {$DEFINE SCREENWORKAREA}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2
    {$DEFINE RTLNAMESPACES}
  {$IFEND}
{$ENDIF}


uses
  // Delphi
  {$IFNDEF RTLNAMESPACES}
  Forms, ExtCtrls, StdCtrls, Buttons, Classes, Controls, Windows, Graphics,
  {$ELSE}
  Vcl.Forms, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Buttons, System.Classes,
  Vcl.Controls, Winapi.Windows, Vcl.Graphics,
  {$ENDIF}
  // DelphiDabbler component
  PJVersionInfo;


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
    abpScreen,    // position relative to screen
    abpDesktop,   // position relative to desktop workarea
    abpOwner      // position relative to owner control: same as abpScreen if
                  // no owner or owner not win control
  );

  {
  TPJAboutBoxDlg:
    The about box component class.
  }
  TPJAboutBoxDlg = class(TComponent)
  private
    fTitle: string;                     // value of Title property
    fProgramName: string;               // value of ProgramName property
    fVersion: string;                   // value of Version property
    fCopyright: string;                 // value of Copyright property
    fNotes: string;                     // value of Notes property
    fButtonPlacing: TPJAboutBtnPlacing; // value of ButtonPlacing property
    fButtonKind: TPJAboutBtnKinds;      // value of ButtonKind property
    fButtonGlyph: TPJAboutBtnGlyphs;    // value of ButtonGlyph property
    fAutoDetectGlyphs: Boolean;         // value of AutoDetectGlyphs property
    fButtonHeight: Integer;             // value of ButtonHeight property
    fButtonWidth: Integer;              // value of ButtonWidth property
    fCentreDlg: Boolean;                // value of CentreDlg property
    fDlgLeft: Integer;                  // value of DlgLeft property
    fDlgTop: Integer;                   // value of DlgTop property
    fVersionInfo: TPJVersionInfo;       // value of VersionInfo property
    fHelpContext: THelpContext;         // value of HelpContext property
    fPosition: TPJAboutPosition;        // value of Position property
    fUseOwnerAsParent: Boolean;         // value of UseOwnerAsParent property
    fUseOSStdFonts: Boolean;            // value of UseOSStdFonts property
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
    procedure SetParentToOwner(Dlg: TWinControl);
      {Sets parent of this dialog box to the window handle of the dialog's
      owner, if the owner is a window control.
        @param Dlg [in] Reference to dialog box object.
      }
    procedure SetDefaultFont(Font: TFont);
      {Set a font to underlying OSs default font.
        @param Font [in] Font to be set to default font.
      }
    procedure ShowHandler(Sender: TObject);
      {Handler for about box form's OnShow event. Positions dialog box on
      screen.
        @param Sender [in] Reference to dialog box form.
      }
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
      default abgNone;
      {Determines which glyph, if any, to display on dialog's OK button.
      WARNING: If the deprecated AutoDetectGlyphs property is True then
      ButtonGlyph is ignored}
    property AutoDetectGlyphs: Boolean
      read fAutoDetectGlyphs write fAutoDetectGlyphs
      default False;
      {Determines whether any image assigned to the ButtonGlyph property is
      displayed. If AutoDetectGlyphs is true then ButtonGlyph is ignored.
      WARNING: This property is DEPRECATED and should always be set to False.
      Set ButtonGlyph to abgNone (default) to prevent display of gylphs on
      buttons}
    property ButtonHeight: Integer
      read fButtonHeight write fButtonHeight
      default 25;
      {The height of the button}
    property ButtonWidth: Integer
      read fButtonWidth write fButtonWidth
      default 75;
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
    property UseOwnerAsParent: Boolean
      read fUseOwnerAsParent write fUseOwnerAsParent
      default False;
      {When true sets window handle of dialog's owner, if any, as dialog's
      parent window. Provided for use when application's main form, rather than
      application window, is directly displayed in task bar. Set this to prevent
      main window from being able to be displayed in front of this dialog box}
    property UseOSStdFonts: Boolean
      read fUseOSStdFonts write fUseOSStdFonts
      default False;
      {When true causes dialog to use OSs standard fonts. This property is
      mainly of use to cause XP and Vista to use their differing default fonts
      in the dialog box}
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
  Dlg.Left := (Rect.Right - Rect.Left - Dlg.Width) div 2 + Rect.Left;
  Dlg.Top := (Rect.Bottom - Rect.Top - Dlg.Height) div 2 + Rect.Top;
  KeepOnScreen(Dlg);
end;

constructor TPJAboutBoxDlg.Create(AOwner: TComponent);
  {Component constructor. Sets up dialog box.
    @param AOwner [in] Component that owns this component.
  }
{$IFDEF RESOURCESTRING}
resourcestring
{$ELSE}
const
{$ENDIF}
  sDefaultTitle = 'About';  // default dialog box tile
begin
  inherited Create(AOwner);
  // Set default property values
  fButtonPlacing := abpCentre;
  fButtonKind := abkOK;
  fButtonGlyph := abgNone;
  fButtonHeight := 25;
  fButtonWidth := 75;
  fTitle := sDefaultTitle;
  fCentreDlg := True;
  fPosition := abpDesktop;
  fAutoDetectGlyphs := False;
end;

function TPJAboutBoxDlg.DesktopWorkArea: TRect;
  {Gets desktop work area as rectangle.
    @return Win 32: the desktop excluding task bar and toolbars. Win 16: area of
      whole screen.
  }
begin
  {$IFDEF SCREENWORKAREA}
  // Delphi 6 up: get desktop area from screen object
  Result := Screen.WorkAreaRect;
  {$ELSE}
  // Delphi 2-5: get desktop area directly from Windows
  SystemParametersInfo(SPI_GETWORKAREA, 0, @Result, 0);
  {$ENDIF}
end;

procedure TPJAboutBoxDlg.Execute;
  {Displays the dialog box.
  }
{$IFDEF RESOURCESTRING}
resourcestring
{$ELSE}
const
{$ENDIF}
  // Button captions
  sOKBtnCaption = 'OK';
  sDoneBtnCaption = 'Done';
  sCloseBtnCaption = 'Close';
  sCancelBtnCaption = 'Cancel';
var
  Dlg: TPJAboutBoxForm; // dialog box instance
begin
  // Create dialog box instance
  Dlg := TPJAboutBoxForm.Create(Owner);
  try
    Dlg.OnShow := ShowHandler;

    // decide if to use dialog's owner as parent
    if fUseOwnerAsParent then
      SetParentToOwner(Dlg);

    // decide on kind of font to use
    if fUseOSStdFonts then
      SetDefaultFont(Dlg.Font)
    else
      Dlg.Font.Size := 8;

    // Set window caption
    Dlg.Caption := fTitle;

    if fVersionInfo <> nil then
    begin
      // Get displayed text from linked TPJVersionInfo component properties
      Dlg.ProgramLbl.Caption := fVersionInfo.ProductName;
      Dlg.VersionLbl.Caption := fVersionInfo.ProductVersion;
      Dlg.CopyrightLbl.Caption := fVersionInfo.LegalCopyright;
      Dlg.NotesLbl.Caption := fVersionInfo.Comments;
    end
    else
    begin
      // No linked TPJVersionInfo: get displayed text from string properties. If
      // Program name not specified, use application title
      if ProgramName = '' then
        Dlg.ProgramLbl.Caption := Application.Title
      else
        Dlg.ProgramLbl.Caption := ProgramName;
      Dlg.VersionLbl.Caption := Version;
      Dlg.CopyrightLbl.Caption := Copyright;
      Dlg.NotesLbl.Caption := Notes;
    end;

    // Set icon to application icon
    Dlg.IconImg.Picture.Graphic := Application.Icon;

    // Configure "done" button
    Dlg.DoneBtn.Height := fButtonHeight;
    Dlg.DoneBtn.Width := fButtonWidth;
    case ButtonPlacing of
      abpLeft:
        Dlg.DoneBtn.Left := Dlg.Bevel1.Left;
      abpRight:
        Dlg.DoneBtn.Left := Dlg.Bevel1.Width + Dlg.Bevel1.Left
          - Dlg.DoneBtn.Width;
      abpCentre:
        Dlg.DoneBtn.Left := (Dlg.ClientWidth - Dlg.DoneBtn.Width) div 2;
    end;
    // decide whether to use button glyph property, depending on value of
    // AutoDetectGlyphs property
    if not fAutoDetectGlyphs then
      // use ButtonGlyph property: load one bitmap resources present in all
      // Delphi apps
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
      // ignore ButtonGlyphs property: don't use glyphs
      Dlg.DoneBtn.Glyph := nil;
    // set button text per button kind property
    case ButtonKind of
      abkOK: Dlg.DoneBtn.Caption := sOKBtnCaption;
      abkDone: Dlg.DoneBtn.Caption := sDoneBtnCaption;
      abkClose: Dlg.DoneBtn.Caption := sCloseBtnCaption;
      abkCancel: Dlg.DoneBtn.Caption := sCancelBtnCaption;
    end;
    // adjust dialog box height according to button height
    Dlg.ClientHeight := 166 + fButtonHeight;

    // Positioning of dialog on screen is done in OnShow handler to work round
    // problem in later Delphis that place main form on taskbar: setting
    // position here causes position info to be lost.

    // Set dialog's help context
    Dlg.HelpContext := fHelpContext;

    // Show the dialog
    Dlg.ShowModal;

  finally
    Dlg.Free;
  end;
end;

procedure TPJAboutBoxDlg.KeepOnScreen(Dlg: TPJAboutBoxForm);
  {Adjusts dialog box if necessary so that it appears wholly on screen.
    @param Dlg [in] Reference to dialog box form.
  }
var
  DisplayBounds: TRect; // bounds of available display area
begin
  DisplayBounds := DesktopWorkArea;
  // Adjust horizontally
  if Dlg.Left < DisplayBounds.Left then
    Dlg.Left := DisplayBounds.Left
  else if Dlg.Left + Dlg.Width > DisplayBounds.Right then
    Dlg.Left := DisplayBounds.Right - Dlg.Width;
  // Adjust vertically
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
  inherited;
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
  Dlg.Left := TopLeft.X + DlgLeft;
  Dlg.Top := TopLeft.Y + DlgTop;
  KeepOnScreen(Dlg);
end;

function TPJAboutBoxDlg.ScreenArea: TRect;
  {Gets screen area as rectangle.
    @return Rectangle defining screen.
  }
begin
  Result := Rect(0, 0, Screen.Width, Screen.Height);
end;

procedure TPJAboutBoxDlg.SetDefaultFont(Font: TFont);
  {Set a font to underlying OSs default font.
    @param Font [in] Font to be set to default font.
  }
var
  LogFont: TLogFont;  // logical font structure
  FontHandle: HFONT;  // handle of required font
begin
  // we treat icon desktop icon font as default if supported, or use default gui
  // font otherwise
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

procedure TPJAboutBoxDlg.ShowHandler(Sender: TObject);
  {Handler for about box form's OnShow event. Positions dialog box on screen.
    @param Sender [in] Reference to dialog box form.
  }
var
  Dlg: TPJAboutBoxForm; // reference to dialog box form
begin
  Dlg := Sender as TPJAboutBoxForm;
  if fCentreDlg then
  begin
    // Centre dialog per Position property: ignore DlgLeft & DlgTop.
    case fPosition of
      abpScreen:
        CentreInRect(Dlg, ScreenArea);
      abpDesktop:
        CentreInRect(Dlg, DesktopWorkArea);
      abpOwner:
      begin
        if (Owner is TWinControl) then
          CentreInRect(Dlg, (Owner as TWinControl).BoundsRect)
        else
          // no owner or owner not win control: centre on screen
          CentreInRect(Dlg, ScreenArea);
      end;
    end;
  end
  else
  begin
    // position per DlgLeft and DlgTop relative to position defined by Position
    // property and adjust to keep on screen.
    case fPosition of
      abpScreen:
        OffsetFromPoint(Dlg, ScreenArea.TopLeft);
      abpDesktop:
        OffsetFromPoint(Dlg, DesktopWorkArea.TopLeft);
      abpOwner:
      begin
        if (Owner is TWinControl) then
          OffsetFromPoint(Dlg, (Owner as TWinControl).BoundsRect.TopLeft)
        else
          // no owner or not win control: offset relative to screen
          OffsetFromPoint(Dlg, ScreenArea.TopLeft);
      end;
    end;
  end;
end;

end.

