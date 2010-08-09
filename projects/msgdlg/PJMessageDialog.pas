{ ##
  @PROJECT_NAME             Message Dialog Components
  @PROJECT_DESC             Components that provide customisable message dialog
                            boxes that wrap the Windows MessageBoxIndirect API
                            call and the Delphi VCL CreateMessageDialog
                            function.
  @FILE                     PJMessageDialog.pas
  @COMMENTS                 Component source code.
  @LEGAL_NOTICE             This component is distributed under the Mozilla
                            Public License - see below.
  @AUTHOR                   Peter D Johnson, LLANARTH, Ceredigion, Wales, UK.
  @OWNER                    DelphiDabbler
  @EMAIL                    peter.johnson@openlink.org
  @WEBSITE                  http://www.delphidabbler.com/
  @COPYRIGHT                © Peter D Johnson, 2001-2006.
  @HISTORY(
    @REVISION(
      @VERSION              1.0
      @DATE                 26/03/2001
      @COMMENTS             Original version.
    )
    @REVISION(
      @VERSION              2.0
      @DATE                 05/10/2003
      @COMMENTS             Complete re-write adding new components and
                            rewriting the v1 TPJMessageDialog component. Changes
                            are:
                            + Added new base classes for all components.
                            + Modified TPJMessageDialog to descend from common
                              base class with new TPJWinMsgDlg. In effect this
                              makes TPJMessageDialog a new component that
                              emulates the previous version.
                            + Setting IconKind to miUser and leaving
                              IconResource empty now displays MAINICON rather
                              than nothing.
                            + Fixed bug in TPJMessageDialog that failed to
                              display user icons under Win NT. We now reference
                              resources in Unicode under Win NT.
                            + MakeSound property of TPJMessageDialog now works
                              for all values of IconKind rather than just
                              miUser.
                            + Added new TPJWinMsgDlg component that also wraps
                              Windows MessageBoxIndirect call. This new
                              component is compatible with, and has properties
                              that are a subset of TPJVCLMsgDlg.
                            + Added new TPJVCLMsgDlg component that wraps the
                              Delphi VCL CreateMessageDialog function call that
                              is used to implement the MessageDlgXXXX Delphi
                              functions. This component provides a superset of
                              the properties of TPJWinMsgDlg and permits finer
                              control over the buttons that can appear. It also
                              allows the dialog to be positioned relative to
                              screen or owner form.
                            + Moved component registration to new
                              PJMessageDialogDsgn unit.
    )
    @REVISION(
      @VERSION              2.1
      @DATE                 28/12/2005
      @COMMENTS             + Moved component regsitration back to this unit
                              since design time unit no longer required
                              following removal of property editor.
                            + Fixed bug in TPJVCLMsgDlg that was causing
                              exception when displaying help from help button
                              in later Delphi versions. Did this by providing
                              OnClick handler for help button.
                            + Set default values for properties where this had
                              been omitted.
    )
    @REVISION(
      @VERSION              2.2
      @DATE                 27/03/2006
      @COMMENTS             + Added new OnShow and OnHide events to
                              TPJVCLMsgDlg. These provide access to dialog box's
                              form to permit further customisation.
                            + Added new protected FormHide and FormShow event
                              handlers for dialog box form on TPJVCLMsgDlg.
                            + Changed code that sets default button to work
                              with new TPJVCLMsgDlg.FormShow method.
                            + Added new TPJVCLMsgDlgFormEvent type for
                              TPJVCLMsgDlg's OnShow and OnHide events.
                            + Added new OnHelp event to TPJVCLMsgDlg and
                              TPJWinMsgDlg that is triggered when a help button
                              is clicked. If this event is assigned default
                              help handling is inhibited.
                            + Enabled TPJVCLMsgDlg to trigger help when F1
                              pressed.
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
 * The Original Code is PJMessageDialog.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 2001-2006 Peter
 * Johnson. All Rights Reserved.
 *
 * ***** END LICENSE BLOCK *****
}


unit PJMessageDialog;


interface


uses
  // Delphi
  Windows, Classes, Controls, StdCtrls, Dialogs, Forms;


type

  {
  TPJMsgDlgButtonGroup:
    The various groups of buttons that can be displayed in the various message
    boxes implemented by each component in this unit.
  }
  TPJMsgDlgButtonGroup = (
    bgAbortRetryIgnore, // Abort, Retry and Ignore buttons
    bgOK,               // A single OK button
    bgOKCancel,         // OK and a Cancel buttons
    bgRetryCancel,      // Retry and Cancel buttons
    bgYesNo,            // Yes and No buttons
    bgYesNoCancel,      // Yes, No and Cancel buttons
    bgUnknown           // An unsupported or unknown group of buttons
  );

  {
  TPJMsgDlgKind:
    The kinds of dialog boxes (icons, sounds, titles) that can be displayed by
    message dialog components that export the Kind property. Note that the
    appearance of the icons depends on the underlying OS. On Win XP
    mkApplication and mkWinLogo are the same.
  }
  TPJMsgDlgKind = (     // Icon         Default Title     Sound
    mkWarning,          // warning      "Warning"         MB_ICONEXCLAMATION
    mkInformation,      // information  "Information"     MB_ICONASTERISK
    mkQuery,            // query        "Confirm"         MB_ICONQUESTION
    mkError,            // error        "Error"           MB_ICONHAND
    mkUser,             // user-defined Application.Title MB_OK
    mkApplication,      // application  Application.Title MB_OK
    mkWinLogo           // Windows logo Application.Title MB_OK
  );

  {
  TPJMsgDlgBase:
    Abstract base class for all message box components in this unit. Provides
    a framework and common functionality and defines some abstract methods to be
    overridden by descendant classes to implement the actual dialog box using
    different underlying APIs.
  }
  TPJMsgDlgBase = class(TComponent)
  private // property fields
    fTitle: TCaption;
    fKind: TPJMsgDlgKind;
    fText: string;
    fMakeSound: Boolean;
    fIconResource: string;
    fButtonGroup: TPJMsgDlgButtonGroup;
    fHelpContext: THelpContext;
    fHelpFile: string;
    fOnHelp: TNotifyEvent;
  protected // property methods
    procedure SetButtonGroup(const Value: TPJMsgDlgButtonGroup); virtual;
  protected // properties
    property ButtonGroup: TPJMsgDlgButtonGroup
      read fButtonGroup write SetButtonGroup default bgOK;
      {Determines group of buttons displayed in dialog box}
    property HelpContext: THelpContext
      read fHelpContext write fHelpContext default 0;
      {ID of help topic accessed when Help button clicked or F1 pressed}
    property HelpFile: string
      read fHelpFile write fHelpFile;
      {Help file to be used to find help topic identified by HelpContext. If
      this property is '' then owner form help file is used if specified,
      otherwise application's help file is used}
    property IconResource: string
      read fIconResource write fIconResource;
      {Resource ID of icon displayed when Kind = mkUser}
    property Kind: TPJMsgDlgKind
      read fKind write fKind default mkInformation;
      {Kind of dialog box: determines the icon to be displayed, any sound played
      and the default title of the dialog}
    property MakeSound: Boolean
      read fMakeSound write fMakeSound default False;
      {Determines whether a sound is played when the dialog appears. Actual
      sound depends on value of Kind property}
    property Text: string
      read fText write fText;
      {Text displayed in dialog box body}
    property Title: TCaption
      read fTitle write fTitle;
      {Text displayed in window caption. If no text is supplied a default title
      is used that depends on the Kind property}
    property OnHelp: TNotifyEvent
      read fOnHelp write fOnHelp;
      {Event triggered when dialog box's help button is pressed. If this event
      is assigned the component's default help handling is inhibited}
  protected
    function GetDefaultTitle: string;
      {Returns default title for window based on kind of dialog}
    function GetWindowTitle: string;
      {Returns window tile: either value of Title property or default title if
      Title is ''}
    function GetHWND: THandle;
      {Returns the window handle of the form or control (if any) that owns this
      component}
    function GetIconResourceName: PChar;
      {Returns the name of the icon resource to be displayed. The icon depends
      on the Kind property and, when Kind=mkUser, the value of the IconResource
      property}
    function GetIconResNameFromStr(const Str: string): PChar; virtual; abstract;
      {Abstract function to be overridden to return a pointer to given string
      resource name}
    function GetIconResourceInstance: THandle;
      {Returns the handle of the module which includes the icon to be displayed
      as a resource. The handle depends on the Kind property}
    function GetHelpFileName: string;
      {Returns name of help file to be used: this is HelpFile property if set,
      otherwise it is the help file associated with the parent form}
    procedure Help;
      {Calls help topic specified by HelpContext property in required help file}
    function Show: Integer; virtual; abstract;
      {Abstract method to be overridden to configure and display dialog box and
      return code representing button pressed by user}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: sets default property values}
    function Execute: Integer;
      {Plays any required sound then displays the message box and returns a
      value relating to the button pressed to close the dialog box}
  end;

  {
  TPJWinMsgDlgCustom:
    Base class for the two components that create a message dialog using the
    Windows MessageBoxIndirect API. This class implements the framework provided
    by the parent class using the Windows API.
  }
  TPJWinMsgDlgCustom = class(TPJMsgDlgBase)
  protected
    function Show: Integer; override;
      {Configure and display dialog box and return code representing button
      pressed by user}
    function GetIconResNameFromStr(const Str: string): PChar; override;
      {Returns a pointer to given string resource name in the format expected by
      the MessageBoxIndirect API call: this is a PChar pointer to a wide char
      string under Windows NT and a PChar pointer to an ansi char string under
      Windows 9x}
  end;

  {
  TPJWinMsgDlg:
    Implements a dialog box that is created and displayed by the Windows
    MessageBoxIndirect API call. The exposed properties are a subset of those
    exposed by TPJVCLMsgDlg. This class simply publishes protected properties
    implemented in parent classes.
  }
  TPJWinMsgDlg = class(TPJWinMsgDlgCustom)
  published
    { Publishing inherited protected properties }
    property ButtonGroup;
    property HelpContext;
    property HelpFile;
    property IconResource;
    property Kind;
    property MakeSound;
    property Text;
    property Title;
    property OnHelp;
  end;

  {
  TPJMsgDlgIconKind:
    The kinds of icons that can be displayed in TPJMessageDialog dialog boxes.
    Also determines default dialog title and sound played. NOTE: provided for
    backwards compatibility.
  }
  TPJMsgDlgIconKind = ( // Icon         Default Title     Sound
    miWarning,          // warning      "Warning"         MB_ICONEXCLAMATION
    miInformation,      // information  "Information"     MB_ICONASTERISK
    miQuery,            // query        "Confirm"         MB_ICONQUESTION
    miError,            // error        "Error"           MB_ICONHAND
    miUser              // user-defined Application.Title MB_OK
  );

  {
  TPJMessageDialog:
    Implements a dialog box that is created and displayed by the Windows
    MessageBoxIndirect API call. Properties are exposed that are compatible with
    those of the equivalent component in release 1 of this unit, although the
    implementation is different.
  }
  TPJMessageDialog = class(TPJWinMsgDlgCustom)
  private // properties
    function GetIconKind: TPJMsgDlgIconKind;
    procedure SetIconKind(const Value: TPJMsgDlgIconKind);
  published
    { Publishing required inherited protected properties }
    property ButtonGroup;
    property HelpContext;
    property IconResource;
    property MakeSound;
    property Text;
    property Title;
    { New property }
    property IconKind: TPJMsgDlgIconKind
      read GetIconKind write SetIconKind default miWarning;
      {Kind of icon displayed in dialog box: also determines any sound played
      and the default title of the dialog}
  end;

  {
  TPJMsgDlgAlign:
    Permitted values for TPJVCLMsgDlg.Align property that specifies where dialog
    is displayed.
  }
  TPJMsgDlgAlign = (
    mdaScreenCentre,  // centred on screen
    mdaScreenOffset,  // offset on screen using OffsetLeft and OffsetTop
    mdaFormCentre,    // centred on owning form
    mdaFormOffset     // offset on owning form using OffsetLeft and OffsetTop
  );

  {
  TPJMsgDlgOption:
    Permitted values for inclusion in TPJMsgDlgOptions set.
  }
  TPJMsgDlgOption = (
    mdoInhibitCancel, // dialog can't cancel: no close & cancel btn or ESC key
    mdoAutoHelpBtn,   // help button displayed if HelpContext is non zero
    mdoShowCustomIcon // program icon is displayed when Kind=mtCustom
  );

  {
  TPJMsgDlgOptions:
    Set of options for TPJVCLMsgDlg component.
  }
  TPJMsgDlgOptions = set of TPJMsgDlgOption;

  {
  TPJVCLMsgDlgFormEvent:
    Type of event triggered when TPJVCLMsgDlg is shown. Provides access to
    dialog box TForm.
  }
  TPJVCLMsgDlgFormEvent = procedure(Sender: TObject; Dlg: TForm) of object;

  {
  TPJVCLMsgDlg:
    Implements a customisable dialog that is created using the VCL's
    CreateMessageDialog function. The properties of the component are a superset
    of those exposed by TPJWinMsgDlg. This class implements the framework
    provided by the parent class using the underlying VCL code.
  }
  TPJVCLMsgDlg = class(TPJMsgDlgBase)
  private // properties
    fButtons: TMsgDlgButtons;
    fOptions: TPJMsgDlgOptions;
    fAlign: TPJMsgDlgAlign;
    fOffsetLeft: Integer;
    fOffsetTop: Integer;
    fDefButton: TMsgDlgBtn;
    fOnHide: TPJVCLMsgDlgFormEvent;
    fOnShow: TPJVCLMsgDlgFormEvent;
    procedure SetButtons(const Value: TMsgDlgButtons);
  protected // properties
    procedure SetButtonGroup(const Value: TPJMsgDlgButtonGroup); override;
  private
    fOldAppHelpHandler: THelpEvent;
      {Records reference to any existing Application.OnHelp event handler to
      enable it to be restored when dialog box form is hidden: we replace
      Application.OnHelp with custom version for life of form diaolog}
  protected
    function Show: Integer; override;
      {Configure and display dialog box and return code representing button
      pressed by user}
    function GetIconResNameFromStr(const Str: string): PChar; override;
      {Returns a pointer to given string resource name}
    procedure PreventCloseOnCancel(Sender: TObject; var CanClose: Boolean);
      {OnCloseQuery event handler for dialog form used when we wish to prevent
      dialog from being closed on pressing cancel or clicking close button on
      dialog}
    procedure FocusDefaultButton(const Dlg: TForm);
      {Sets focus to dialog box's default button as specified by DefButton
      property, if valid}
    procedure FormShowHandler(Sender: TObject); virtual;
      {OnShow handler for dialog box form. Sets any required default button and
      triggers component's OnShow event, passing reference to dialog box form}
    procedure FormHideHandler(Sender: TObject); virtual;
      {OnHide event handler for dialog box form. Triggers component's OnHide
      event, passing reference to dialog box form}
    procedure FormKeyDownHandler(Sender: TObject; var Key: Word;
      Shift: TShiftState);
      {OnKeyDown event handler for dialog box form. Triggers help if key is F1 and
      dialog box contains a help button}
    procedure HelpClickHandler(Sender: TObject); virtual;
      {OnClick handler for Help button. Overrides VCL message box's own help
      handling to enable us to handle display of help}
    function FindHelpBtn(const Dlg: TForm): TButton;
      {Finds reference to dialog box form's help button}
    function AppHelpHandler(Command: Word; Data: Longint;
      var CallHelp: Boolean): Boolean;
      {Event handler for Application.OnHint that prevents default help system
      being used while dialog is displayed, to enable us to handle help
      ourselves}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: sets default property values}
    function CreateDialog: TForm;
      {Creates instance of dialog and returns it: caller is responsible for
      displaying and freeing dialog instance}
  published
    { Publishing inherited protected properties }
    property ButtonGroup; // property interacts with new Buttons property
    property HelpContext;
    property HelpFile;
    property IconResource;
    property Kind;
    property MakeSound;
    property Text;
    property Title;
    property OnHelp;
    { New properties }
    property Align: TPJMsgDlgAlign
      read fAlign write fAlign default mdaScreenCentre;
      {Determines alignment of dialog box in relation to owner form or screen}
    property Buttons: TMsgDlgButtons
      read fButtons write SetButtons default [mbOK];
      {Determines the buttons displayed in the dialog box - works in association
      with ButtonGroup property - i.e. making changes to one updates the other}
    property DefButton: TMsgDlgBtn
      read fDefButton write fDefButton default mbOK;
      {ID of default button: if the specified button is not in buttons list then
      this setting doesn't effect default button}
    property OffsetLeft: Integer
      read fOffsetLeft write fOffsetLeft default 0;
      {Horizontal offset of dialog box relative to screen or owner form,
      depending on Align property. Ignored if Align is mdaFormCentre or
      mdaScreenCentre}
    property OffsetTop: Integer
      read fOffsetTop write fOffsetTop default 0;
      {Vertical offset of dialog box relative to screen or owner form, depending
      on Align property. Ignored if Align is mdaFormCentre or mdaScreenCentre}
    property Options: TPJMsgDlgOptions
      read fOptions write fOptions default [mdoAutoHelpBtn, mdoShowCustomIcon];
      {Component options: for explanation see TPJMsgDlgOptions type definition
      above}
    property OnShow: TPJVCLMsgDlgFormEvent
      read fOnShow write fOnShow;
      {Event triggered when component's dialog box is shown. Provides access to
      the dialog box's form to enable dialog to be further customised. Form
      reference is valid only until OnHide event is triggered}
    property OnHide: TPJVCLMsgDlgFormEvent
      read fOnHide write fOnHide;
      {Event triggered when component's dialog box is hidden before being
      destroyed. Provides reference to dialog box's form. This form reference
      is invalid after this event completes}
  end;


procedure Register;
  {Registers components}


implementation


uses
  // Delphi
  SysUtils, Consts, ExtCtrls, Math;


procedure Register;
  {Registers components}
begin
  RegisterComponents(
    'DelphiDabbler',
    [TPJMessageDialog, TPJWinMsgDlg, TPJVCLMsgDlg]
  );
end;


{ TPJMsgDlgBase }

constructor TPJMsgDlgBase.Create(AOwner: TComponent);
  {Class constructor: sets default property values}
begin
  inherited;
  ButtonGroup := bgOK;
  Kind := mkInformation;
  MakeSound := False;
  HelpContext := 0;
  Title := '';
end;

function TPJMsgDlgBase.Execute: Integer;
  {Plays any required sound then displays the message box and returns a value
  relating to the button pressed to close the dialog box}
const
  // Table mapping dialog box kinds to API flags for sound to be played
  cSounds: array[TPJMsgDlgKind] of Integer = (
    MB_ICONEXCLAMATION, MB_ICONASTERISK, MB_ICONQUESTION, MB_ICONHAND,
    MB_OK, MB_OK, MB_OK);
begin
  if MakeSound then
    MessageBeep(cSounds[Kind]);
  Result := Show;
end;

function TPJMsgDlgBase.GetDefaultTitle: string;
  {Returns default title for window based on kind of dialog}
const
  // Table mapping dialog box kinds to default window titles
  cDefTitles: array[TPJMsgDlgKind] of string = (
    sMsgDlgWarning, sMsgDlgInformation, sMsgDlgConfirm, sMsgDlgError,
    '', '', '');
begin
  Result := cDefTitles[Kind];
  if Result = '' then
    Result := Application.Title;    // use application title if no default
end;

function TPJMsgDlgBase.GetHelpFileName: string;
  {Returns name of help file to be used: this is HelpFile property if set,
  otherwise it is the help file associated with the parent form}
begin
  if HelpFile <> '' then
    // we have help file specified: use it
    Result := HelpFile
  else if Assigned(Owner) and (Owner is TForm) then
    // no user-specified help file: use help file of owner form
    Result := (Owner as TForm).HelpFile;
end;

function TPJMsgDlgBase.GetHWND: THandle;
  {Returns the window handle of the form or control (if any) that owns this
  component}
begin
  if (Owner <> nil) and (Owner is TWinControl) then
    Result := (Owner as TWinControl).Handle
  else
    Result := 0;
end;

function TPJMsgDlgBase.GetIconResourceInstance: THandle;
  {Returns the handle of the module which includes the icon to be displayed as a
  resource. The handle depends on the Kind property}
begin
  if fKind = mkUser then
    // User icon must come from host program module
    Result := HInstance
  else
    // System icon required
    Result := 0;
end;

function TPJMsgDlgBase.GetIconResourceName: PChar;
  {Returns the name of the icon resource to be displayed. The icon depends on
  the Kind property and, when Kind=mkUser, the value of the IconResource
  property}
const
  // name of program icon in Delphi executable
  cMainIcon = 'MAINICON';
  // Table mapping IconKind to icon API flags
  cIcons: array[TPJMsgDlgKind] of PChar = (
    IDI_EXCLAMATION, IDI_ASTERISK, IDI_QUESTION, IDI_HAND,
    nil, IDI_APPLICATION, IDI_WINLOGO
  );
begin
  // Check to see if icon resource is predefined
  Result := cIcons[Kind];
  if Result = nil then
  begin
    // Icon resource not predefined: try to get from IconResource property
    // or use default value if property not set
    if IconResource = '' then
      Result := GetIconResNameFromStr(cMainIcon)
    else
      Result := GetIconResNameFromStr(IconResource);
  end;
end;

function TPJMsgDlgBase.GetWindowTitle: string;
  {Returns window tile: either value of Title property or default title if Title
  is ''}
begin
  if Title = '' then
    Result := GetDefaultTitle
  else
    Result := Title;
end;

procedure TPJMsgDlgBase.Help;
  {Displays help. If OnHelp event handler is assigned then this event is
  triggered. Otherwise the help topic specified by HelpContext property in
  required help file is displayed using WinHelp}
begin
  if Assigned(fOnHelp) then
    fOnHelp(Self)
  else
    Windows.WinHelp(GetHWND, PChar(GetHelpFileName), HELP_CONTEXT, HelpContext);
end;

procedure TPJMsgDlgBase.SetButtonGroup(const Value: TPJMsgDlgButtonGroup);
  {Virtual write access method for ButtonGroup property. Simply sets field
  value. May be overridden by descendant classes}
begin
  fButtonGroup := Value;
end;


{ TPJWinMsgDlgCustom }

procedure HelpCallback(var HelpInfo: THelpInfo); stdcall;
  {Callback procedure for Execute method procedure. Starts win help with help
  context passed in HelpInfo param}
var
  Cmp: TPJMsgDlgBase; // reference to component that called this callback
begin
  // Get reference to owner component
  // we've subverted dwContextId to store this reference
  Cmp := TPJMsgDlgBase(HelpInfo.dwContextId);
  // call Help method of owner object
  Cmp.Help;
end;

function TPJWinMsgDlgCustom.GetIconResNameFromStr(const Str: string): PChar;
  {Returns a pointer to given string resource name in the format expected by the
  MessageBoxIndirect API call: this is a PChar pointer to a wide char string
  under Windows NT and a PChar pointer to an ansi char string under Windows 9x}
begin
  if SysUtils.Win32Platform = VER_PLATFORM_WIN32_NT then
    Result := PChar(PWChar(WideString(Str)))
  else
    Result := PChar(Str);
end;

function TPJWinMsgDlgCustom.Show: Integer;
  {Configure and display dialog box and return code representing button pressed
  by user}
const
  // Table mapping TPJMsgDlgButtonGroup to API flags
  cButtonFlags: array[TPJMsgDlgButtonGroup] of Integer = (
    MB_ABORTRETRYIGNORE, MB_OK, MB_OKCANCEL, MB_RETRYCANCEL,
    MB_YESNO, MB_YESNOCANCEL, 0
  );
var
  MsgBoxParams: TMsgBoxParams;  // params passed to MessageBoxIndirect fn
begin
  // Set up TMsgBoxParams structure
  // zero the structure
  FillChar(MsgBoxParams, SizeOf(MsgBoxParams), 0);
  with MsgBoxParams do
  begin
    cbSize := SizeOf(TMsgBoxParams);
    hwndOwner := GetHWND;
    hInstance := GetIconResourceInstance;
    lpszIcon := GetIconResourceName;
    lpszText := PChar(Text);
    lpszCaption := PChar(GetWindowTitle);
    // set style flags for window: and note user defined icon
    dwStyle := cButtonFlags[fButtonGroup] or MB_USERICON;
    // if user supplied help context record it, set help button and callback
    if HelpContext <> 0 then
    begin
      // .. we *subvert* the dwContextHelpId field to store a reference to this
      //    object so we can reference it in help callback - this reference is
      //    then used to call the Help method
      dwStyle := dwStyle + MB_HELP;
      dwContextHelpId := DWORD(Self);
      lpfnMsgBoxCallback := @HelpCallback;
    end;
  end;
  // Display dlg and return result
  Result := Integer(MessageBoxIndirect(MsgBoxParams));
end;


{ TPJMessageDialog }

function TPJMessageDialog.GetIconKind: TPJMsgDlgIconKind;
  {Read access method for IconKind: we read the corresponding value from the
  protected Kind property and convert it into the required type}
begin
  // Note: TPJMsgDlgIconKind has equivalent values with same ordinal number as
  // TPJMsgDlgKind, but TPJMsgDlgKind has some additional values with no
  // equivalent in TPJMsgDlgIconKind. Because there is no direct access to the
  // Kind property in this class, the conversion should always be safe
  Assert(Ord(fKind) <= Ord(High(TPJMsgDlgIconKind)));
  Result := TPJMsgDlgIconKind(Ord(fKind));
end;

procedure TPJMessageDialog.SetIconKind(const Value: TPJMsgDlgIconKind);
  {Write access method for IconKind property: we write an equivalent value to
  the protected Kind property after converting to the required type}
begin
  // Note: TPJMsgDlgIconKind has equivalent values with same ordinal number as
  // TPJMsgDlgKind, but TPJMsgDlgKind has some additional values with no
  // equivalent in TPJMsgDlgIconKind. Because of this the new TPJMsgDlgIconKind
  // value must always be within the range of TPJMsgDlgKind and the conversion
  // is safe
  Kind := TPJMsgDlgKind(Ord(Value));
end;


{ TPJVCLMsgDlg }

function TPJVCLMsgDlg.AppHelpHandler(Command: Word; Data: Integer;
  var CallHelp: Boolean): Boolean;
  {Event handler for Application.OnHint that prevents default help system
  being used while dialog is displayed, to enable us to handle help
  ourselves}
begin
  CallHelp := False;
  Result := True;
end;

constructor TPJVCLMsgDlg.Create(AOwner: TComponent);
  {Class constructor: sets default property values}
begin
  inherited;
  fAlign := mdaScreenCentre;
  fDefButton := mbOK;
  fOffsetLeft := 0;
  fOffsetTop := 0;
  fOptions := [mdoAutoHelpBtn, mdoShowCustomIcon];
  // Note: fButtons := [mbOK] set when ButtonGroup assigned in inherited
end;

function TPJVCLMsgDlg.CreateDialog: TForm;
  {Creates instance of dialog and returns it: caller is responsible for
  displaying and freeing dialog instance}

  // ---------------------------------------------------------------------------
  function FindImage(const Dlg: TForm): TImage;
    {Finds reference to dialog box form's image control}
  var
    Idx: Integer; // loops thru all components on form
  begin
    Result := nil;
    for Idx := 0 to Pred(Dlg.ComponentCount) do
    begin
      if Dlg.Components[Idx] is TImage then
      begin
        Result := Dlg.Components[Idx] as TImage;
        Break;
      end;
    end;
  end;

  procedure InhibitCancelButtons(const Dlg: TForm);
    {Ensures no buttons on dialog respond to escape key press}
  var
    Idx: Integer; // loops thru all components on form
  begin
    // Switch off cancel property on all buttons
    for Idx := 0 to Pred(Dlg.ComponentCount) do
      if Dlg.Components[Idx] is TButton then
        (Dlg.Components[Idx] as TButton).Cancel := False;
  end;

  procedure AlignToScreen(const Dlg: TForm; UseOffset: Boolean;
    Left, Top: Integer);
    {Aligns given dialog box to screen. If UseOffset is true then dialog is set
    to given Left and Top position on screen. If UseOffset is false then dialog
    is centred on screen}
  begin
    if UseOffset then
    begin
      Dlg.Left := Max(Min(Left, Screen.Width - Dlg.Width), 0);
      Dlg.Top := Max(Min(Top, Screen.Height - Dlg.Height), 0);
    end
    else
    begin
      Dlg.Left := (Screen.Width - Dlg.Width) div 2;
      Dlg.Top := (Screen.Height - Dlg.Height) div 2;
    end;
  end;

  procedure AlignToForm(const Owner, Dlg: TForm; UseOffset: Boolean;
    Left, Top: Integer);
    {Aligns given dialog box over given owner form. If UseOffset is true then
    dialog is set to given Left and Top position relative to form. If UseOffset
    is false then dialog is centred over form}
  begin
    if UseOffset then
    begin
      Dlg.Left := Max(Min(Owner.Left + Left, Screen.Width - Dlg.Width), 0);
      Dlg.Top := Max(Min(Owner.Top + Top, Screen.Height - Dlg.Height), 0);
    end
    else
    begin
      Dlg.Left := Max(
        Min(
          Owner.Left + (Owner.Width - Dlg.Width) div 2,
          Screen.Width - Dlg.Width
        ),
        0
      );
      Dlg.Top := Max(
        Min(
          Owner.Top + (Owner.Height - Dlg.Height) div 2,
          Screen.Height - Dlg.Height
        ),
        0
      );
    end;
  end;
  // ---------------------------------------------------------------------------

const
  // Maps dialog Kind property to dialog types
  cDlgType: array[TPJMsgDlgKind] of TMsgDlgType = (
    mtWarning,        // dialog has warning icon
    mtInformation,    // dialog has information icon
    mtConfirmation,   // dialog has query icon
    mtError,          // dialog has error icon
    mtCustom,         // dialog has user defined icon
    mtCustom,         // dialog has Windows application icon
    mtCustom          // dialog has Windows logo
  );

var
  Img: TImage;            // reference to dialog's image that contains icon
  HelpBtn: TButton;       // reference to dialog's help button
  Btns: TMsgDlgButtons;   // set of buttons to be displayed

begin

  // Set up buttons
  Btns := fButtons;
  // remove cancel button if we're inhibiting cancelation of dialog
  if mdoInhibitCancel in Options then
    Exclude(Btns, mbCancel);
  // add or remove help button per help context if we're auto-detecting
  if mdoAutoHelpBtn in Options then
    if HelpContext = 0 then
      Exclude(Btns, mbHelp)
    else
      Include(Btns, mbHelp);
  // check we have at least one button capable of closing dlg: use mbOK if not
  if (Btns = []) or (Btns = [mbHelp]) then
    Include(Btns, mbOK);

  // Create dialog of required type
  if ((Kind = mkUser) and (mdoShowCustomIcon in Options))
    or (Kind in [mkApplication, mkWinLogo]) then
  begin
    // we need to display an icon not directly supported by VCL:
    // create dlg with kind that has icon (mtCustom doesn't) then fetch icon
    // from resources and assign to the dialog's image component to it
    Result := CreateMessageDialog(Text, mtInformation, Btns);
    Img := FindImage(Result);
    if Assigned(Img) then
      Img.Picture.Icon.Handle := LoadIcon(
        GetIconResourceInstance,  // gets handle to module with icon resource
        GetIconResourceName       // gets pointer to icon name
      )
  end
  else
    // we create with standard icon (or no icon if mtCustom and no icon needed)
    Result := CreateMessageDialog(Text, cDlgType[Kind], Btns);

  // Set caption of dialog if required
  Result.Caption := GetWindowTitle;

  // Set help file and context
  Result.HelpFile := GetHelpFileName;
  Result.HelpContext := HelpContext;

  // Set help button event handler if present
  if (mbHelp in Btns) then
  begin
    HelpBtn := FindHelpBtn(Result);
    if Assigned(HelpBtn) then
      HelpBtn.OnClick := HelpClickHandler;
  end;

  // Arrange dialog per align property and whether or not we have owner
  if Assigned(Owner) and (Owner is TForm)then
  begin
    // we have owner form: act per Align property
    case Align of
      mdaScreenCentre:
        AlignToScreen(Result, False, 0, 0);
      mdaScreenOffset:
        AlignToScreen(Result, True, fOffsetLeft, fOffsetTop);
      mdaFormCentre:
        AlignToForm(Owner as TForm, Result, False, 0, 0);
      mdaFormOffset:
        AlignToForm(Owner as TForm, Result, True, fOffsetLeft, fOffsetTop);
    end;
  end
  else
  begin
    // no owner form: can only align to screen
    case Align of
      mdaScreenCentre, mdaFormCentre:
        AlignToScreen(Result, False, 0, 0);
      mdaScreenOffset, mdaFormOffset:
        AlignToScreen(Result, True, fOffsetLeft, fOffsetTop);
    end;
  end;

  // Ensure form sees keypresses before controls
  Result.KeyPreview := True;

  // Set dialog form's show / hide / key down event handlers
  Result.OnShow := FormShowHandler;
  Result.OnHide := FormHideHandler;
  Result.OnKeyDown := FormKeyDownHandler;

  // Remove ability to cancel form if required
  if mdoInhibitCancel in Options then
  begin
    Result.OnCloseQuery := PreventCloseOnCancel;
    InhibitCancelButtons(Result);
    Result.BorderIcons := Result.BorderIcons - [biSystemMenu];
  end;

end;

function TPJVCLMsgDlg.FindHelpBtn(const Dlg: TForm): TButton;
  {Finds reference to dialog box form's help button}
var
  Idx: Integer; // loops thru all components on form
begin
  Result := nil;
  for Idx := 0 to Pred(Dlg.ComponentCount) do
  begin
    if (Dlg.Components[Idx] is TButton) and
       ((Dlg.Components[Idx] as TButton).Caption = sMsgDlgHelp) then
    begin
      Result := Dlg.Components[Idx] as TButton;
      Break;
    end;
  end;
end;

procedure TPJVCLMsgDlg.FocusDefaultButton(const Dlg: TForm);
  {Sets focus to dialog box's default button as specified by DefButton property,
  if valid}
var
  Idx: Integer; // loops thru all components on form
  Btn: TButton; // reference to button on form
const
  // Captions used for buttons of various kinds. Captions from Consts.pas
  cButtonCaptions: array[TMsgDlgBtn] of string = (
    sMsgDlgYes, sMsgDlgNo, sMsgDlgOK, sMsgDlgCancel, sMsgDlgAbort,
    sMsgDlgRetry, sMsgDlgIgnore, sMsgDlgAll, sMsgDlgNoToAll, sMsgDlgYesToAll,
    sMsgDlgHelp);
begin
  if not (DefButton in Buttons) then
    // No valid default button specified: do nothing (this means that default
    // button is decided by VCL's CreateMessageDialog function)
    Exit;
  // Loop thru all controls, searching for default button
  for Idx := 0 to Pred(Dlg.ComponentCount) do
  begin
    if Dlg.Components[Idx] is TButton then
    begin
      // We have button
      Btn := Dlg.Components[Idx] as TButton;
      if Btn.Caption = cButtonCaptions[DefButton] then
      begin
        // button is the default one: set Default property and focus it
        Btn.Default := True;
        Btn.SetFocus;
      end
      else
        // button is not default: clear its Default property
        Btn.Default := False;
    end;
  end;
end;

procedure TPJVCLMsgDlg.FormHideHandler(Sender: TObject);
  {OnHide event handler for dialog box form. Triggers component's OnHide event,
  passing reference to dialog box form}
begin
  // Trigger component's OnHide event
  if Assigned(fOnHide) then
    fOnHide(Self, Sender as TForm);
  // Restore any earlier application help handler
  Application.OnHelp := fOldAppHelpHandler;
  fOldAppHelpHandler := nil;
end;

procedure TPJVCLMsgDlg.FormKeyDownHandler(Sender: TObject; var Key: Word;
  Shift: TShiftState);
  {OnKeyDown event handler for dialog box form. Triggers help if key is F1 and
  dialog box contains a help button}
begin
  if (Key = VK_F1) and (Shift = []) then
  begin
    Key := 0;
    if FindHelpBtn(Sender as TForm) <> nil then
      Help;
  end;
end;

procedure TPJVCLMsgDlg.FormShowHandler(Sender: TObject);
  {OnShow handler for dialog box form. Sets any required default button and
  triggers component's OnShow event, passing reference to dialog box form}
begin
  // Disable application's default help processing: required for later Delphis
  // we use our own help event handler (restored when form hidden)
  fOldAppHelpHandler := Application.OnHelp;
  Application.OnHelp := AppHelpHandler;
  // Set required default button
  FocusDefaultButton(Sender as TForm);
  // Trigger component's OnShow event
  if Assigned(fOnShow) then
    fOnShow(Self, Sender as TForm);
end;

function TPJVCLMsgDlg.GetIconResNameFromStr(const Str: string): PChar;
  {Returns a pointer to given string resource name}
begin
  Result := PChar(Str);
end;

procedure TPJVCLMsgDlg.HelpClickHandler(Sender: TObject);
  {OnClick handler for Help button. Overrides VCL message box's own help
  handling to enable us to handle display of help}
begin
  // Call inherited Help method: we don't want default VCL message box help
  // handling since this causes exception in some Delphis
  Help;
end;

procedure TPJVCLMsgDlg.PreventCloseOnCancel(Sender: TObject;
  var CanClose: Boolean);
  {OnCloseQuery event handler for dialog form used when we wish to prevent
  dialog from being closed on pressing cancel or clicking close button on
  dialog}
begin
  case (Sender as TForm).ModalResult of
    mrCancel: CanClose := False;
    else CanClose := True;
  end;
end;

procedure TPJVCLMsgDlg.SetButtonGroup(const Value: TPJMsgDlgButtonGroup);
  {Write access method override for inherited ButtonGroup property. Records
  value (in inherited method) and updates Buttons property to store set of
  buttons in group}
begin
  inherited;
  case Value of
    bgAbortRetryIgnore: fButtons := [mbAbort, mbRetry, mbIgnore];
    bgOK: fButtons := [mbOK];
    bgOKCancel: fButtons := [mbOK, mbCancel];
    bgRetryCancel: fButtons := [mbRetry, mbCancel];
    bgYesNo: fButtons := [mbYes, mbNo];
    bgYesNoCancel: fButtons := [mbYes, mbNo, mbCancel];
    bgUnknown: fButtons := [];
  end;
end;

procedure TPJVCLMsgDlg.SetButtons(const Value: TMsgDlgButtons);
  {Write access method for Buttons property. Records value and updates
  ButtonGroup property to appropriate matching group (if any) or bgUnknown if
  Buttons set does not correspond to any predefined group}
begin
  fButtons := Value;
  if Value = [mbAbort, mbRetry, mbIgnore] then
    fButtonGroup := bgAbortRetryIgnore
  else if Value = [mbOK] then
    fButtonGroup := bgOK
  else if Value = [mbOK, mbCancel] then
    fButtonGroup := bgOKCancel
  else if Value = [mbRetry, mbCancel] then
    fButtonGroup := bgRetryCancel
  else if Value = [mbYes, mbNo] then
    fButtonGroup := bgYesNo
  else if Value = [mbYes, mbNo, mbCancel] then
    fButtonGroup := bgYesNoCancel
  else if Value = [mbYes, mbNo, mbCancel] then
    fButtonGroup := bgYesNoCancel
  else
    fButtonGroup := bgUnknown;
end;

function TPJVCLMsgDlg.Show: Integer;
  {Configure and display dialog box and return code representing button pressed
  by user}
begin
  with CreateDialog do
    try
      Result := ShowModal;
    finally
      Free;
    end;
end;

end.

