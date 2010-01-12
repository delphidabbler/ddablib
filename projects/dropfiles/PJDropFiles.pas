{ 
 * PJDropFiles.pas
 *
 * Components that enable files dragged and dropped from explorer to be
 * "caught" along with secondary components used to filter dropped files.
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
 * The Original Code is PJDropFiles.pas.
 *
 * The Initial Developer of the Original Code is Peter Johnson
 * (http://www.delphidabbler.com/).
 *
 * Portions created by the Initial Developer are Copyright (C) 1998-2010 Peter
 * Johnson. All Rights Reserved.
 *
 * Contributor(s):
 *   NONE
 *
 * ***** END LICENSE BLOCK *****
}


unit PJDropFiles;


interface


{ Find which compilers we have }
{$DEFINE DELPHI6ANDUP}
{$DEFINE DELPHI4ANDUP}
{$IFDEF VER80}  {Delphi 1}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER90}  {Delphi 2}
  {$UNDEF DELPHI4ANDUP}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER93}  {C++ Builder 1}
  {$UNDEF DELPHI4ANDUP}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER100} {Delphi 3}
  {$UNDEF DELPHI4ANDUP}
  {$UNDEF DELPHI6ANDUP}
{$ENDIF}
{$IFDEF VER110} {C++ Builder 3}
  {$UNDEF DELPHI4ANDUP}
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
  // Delphi
  Windows, Messages, Classes, Controls;


type


  {
  TPJFileFilter:
    Abstract base class for components that filter file names according to
    various criteria. These components are designed for use with the various
    drop files components to filter the dropped files. This class defines an
    abstract method that is called when files are dropped by the drop files
    components when a filter component is referenced.
  }
  TPJFileFilter = class(TComponent)
  public
    function Accept(const FilePath: string; const IsFolder: Boolean): Boolean;
      virtual; abstract;
      {Returns true if the given file (or folder - per IsFolder) passes through
      the filter and false if not}
  end;


  {
  TPJExtFileFilterStyle:
    Values of TPJExtFileFilter component's Style property.
  }
  TPJExtFileFilterStyle = (
    fsFilterFilesOnly,    // files are filtered by extension, folders pass thru
    fsFilterFoldersOnly,  // folders are filtered by extension, files pass thru
    fsAll                 // filter is applied to files and folders
  );


  {
  TPJExtFileFilter:
    Filter component used to filter out files that do not have extensions that
    match in a given list of extensions. The component is designed for use with
    drop files components.
  }
  TPJExtFileFilter = class(TPJFileFilter)
  private // properties
    fExtensions: TStringList;
    fStyle: TPJExtFileFilterStyle;
    function GetExtensions: string;
    procedure SetExtensions(const Value: string);
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: creates component and sets default property values}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    function Accept(const FilePath: string; const IsFolder: Boolean): Boolean;
      override;
      {Check the given file or folder (per IsFolder) and return true if the
      given file is of required type (per Style property) and has extension that
      matches one of those specified in Extensions property}
  published
    property Extensions: string
      read GetExtensions write SetExtensions;
      {List of valid extensions (';' separated): only files with an extension in
      the the list passes the filter unless Extensions = '', when all files
      pass through}
    property Style: TPJExtFileFilterStyle
      read fStyle write fStyle default fsFilterFilesOnly;
      {Style of filter applied by component. For explanation of values see
      comments on TPJExtFileFilterStyle}
  end;


  {
  TPJWildCardFileFilter:
    Filter component used to filter out files that do not have a file name
    matching a wild card.
  }
  TPJWildCardFileFilter = class(TPJFileFilter)
  private // properties
    fWildCard: string;
    procedure SetWildCard(const Value: string);
  private
    fWildCardChanged: Boolean;
      {Used to detect if wild card has changed since last call to Accept}
    fMatchingFiles: TStringList;
      {List of files in current folder that match wildcard}
    fCurrentFolder: string;
      {Current folder that list of matching files comes from}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor: creates component and owned object}
    destructor Destroy; override;
      {Class destructor: frees owned objects}
    function Accept(const FilePath: string; const IsFolder: Boolean): Boolean;
      override;
      {Check the given file or folder (per IsFolder) and return true if the
      given file matches wild card in current folder}
  published
    property WildCard: string read fWildCard write SetWildCard;
      {Files in a folder that match the given wildcard pass through the filter,
      while those that don't match the wild card do not. Wildcard is in usual
      DOS format - '?' and '*' are supported}
  end;


  {
  TPJDroppedFileFilter:
    Type of method used to handle OnFileFilter events. FileName is name of a
    file, IsFolder tells if FileName is a file or folder. Accept is set to true
    on calling the event handler: resetting to False means the file is filtered
    out and not included in list of dropped files.
  }
  TPJDroppedFileFilter = procedure(Sender: TObject; const FileName: string;
    const IsFolder: Boolean; var Accept: Boolean) of object;


  {
  TPJDropFilesOption:
    Enumeration containing all possible values for Options property.
  }
  TPJDropFilesOption = (
    dfoIncFolders,      // include folders in list of dropped files
    dfoIncFiles,        // include files in list of dropped filed
    dfoRecurseFolders   // recurse through all folder in list of dropped files
  );


  {
  TPJDropFilesOptions:
    Set containing possible values of Options property.
  }
  TPJDropFilesOptions = set of TPJDropFilesOption;


  {
  TPJAbstractDropFilesHelper:
    Helper class that implements interface to windows drag drop events and other
    common functionality for twp drop files classes. This can't be done in
    common base class for drop files classes since classes inherit from
    different components.
  }
  TPJAbstractDropFilesHelper = class(TObject)
  private
    fComp: TComponent;
      {Reference to the component the helper is working with}
  protected
    function GetHWnd: THandle; virtual;
      {Returns window handle that related control receives messages from. This
      is window handle of container returned by GetContainer method}
    function GetContainer: TWinControl; virtual; abstract;
      {Abstract method to return reference to the related windowed control that
      the component works with}
    procedure HandleFile(const FileName: string);
      {Processes the given file and updates file list according to
      RecurseFolders and ExcludeFolders properties. If ExcludeFolders is true
      then any folder names encoutnered are exlcuded from file list. If
      RecurseFolders is true then any folders and examined and the files in the
      folder are added to list (taking note of ExcludeFolders). Any subfolders
      are also searched recursively}
  public
    fForeGroundOnDrop: Boolean;
      {Value of related control's ForegroundOnDrop property: determines if
      window is brought to foreground when files dropped}
    fOnDropFiles: TNotifyEvent;
      {Value of related control's OnDropFiles event handler: triggered when
      files dropped and after they have been processed}
    fOnBeforeDrop: TNotifyEvent;
      {Value of related control's OnBeforeDrop property: triggered when files
      droped but before they are processed. Files property has no defined value
      when this event is triggered}
    fOnAfterDrop: procedure(hDrop: THandle) of object;
      {Event triggered after drop is complete but before drop handle is
      destroyed: this permits associated controls to perform additional
      processing with drop handle if required}
    fOnFileFilter: TPJDroppedFileFilter;
      {Event triggered when files are dropped for each dropped file (after
      checking required types per Options property. If Accept parameter of
      event handler is set to False the file is not included in the list of
      dropped files. This event handler is not called if a filter component
      filters out the file}
    fFilterComp: TPJFileFilter;
      {Reference to component used to filter the list of dropped files. This is
      used before the OnFileFilter event is triggered}
    fDropPoint: TPoint;
      {Point at which drop occured: can be changed by related components if
      necessary}
    fFileList: TStringList;
      {List of files dropped}
    fOptions: TPJDropFilesOptions;
      {Value related to control's Options property. The options apply to the
      handling of files and folders: determines if files and/or folders are
      listed and if folders are recursed}
    function GetDropControl: TControl;
      {Returns reference to control at position where cursor was released, or
      nil if no control under drop point}
    constructor Create(Comp: TComponent);
      {Class constructor: records reference to related component and helper
      object}
    destructor Destroy; override;
      {Class destructor: frees helper object}
    procedure AcceptFiles(Flag: Boolean);
      {Calls DragAcceptFiles with given value, if not designing and if related
      window handle exists}
    procedure FilesDropped(hDrop: THandle);
      {Uses drop handle to get list of dropped files and to set drop point.
      Triggers OnAfterDrop event before drop handle is destroyed}
    procedure DropFiles; virtual;
      {Called after dropped files are processed and drop handle has been freed.
      Triggers OnDropFiles event and brings drop target to front if required}
    procedure BeforeDrop; virtual;
      {Called before dropped files are processed. Triggers OnBeforeDrop event}
  end;


  {
  TPJDropFilesHelper:
    Helper class customised to TPJDropFiles.
  }
  TPJDropFilesHelper = class(TPJAbstractDropFilesHelper)
  protected
    function GetContainer: TWinControl; override;
      {Returns return reference to the component's related windowed control:
      this is the control itself in this class}
  end;


  {
  TPJFormDropFilesHelper:
    Helper class customised to TPJFormDropFiles.
  }
  TPJFormDropFilesHelper = class(TPJAbstractDropFilesHelper)
  protected
    function GetContainer: TWinControl; override;
      {Returns return reference to the component's related windowed control:
      this is the form on which owns the component}
  end;


  {
  TPJCtrlDropFilesHelper:
    Helper class customised to TPJCtrlDropFiles
  }
  TPJCtrlDropFilesHelper = class(TPJAbstractDropFilesHelper)
  protected
    function GetContainer: TWinControl; override;
      {Returns return reference to the control managed by the related component}
  end;


  {
  TPJDropFiles:
    Component that provides a container window which catches files dropped on it
    or any components parented by container.
  }
  TPJDropFiles = class(TCustomControl)
  private // properties
    fPassThrough: Boolean;
    function GetCount: Integer;
    function GetFile(Idx: Integer): string;
    function GetDropControl: TControl;
    function GetFileName: string;
    function GetOnDropFiles: TNotifyEvent;
    procedure SetOnDropFiles(const Value: TNotifyEvent);
    function GetDropPoint: TPoint;
    function GetForegroundOnDrop: Boolean;
    procedure SetForegroundOnDrop(const Value: Boolean);
    function GetOnBeforeDrop: TNotifyEvent;
    procedure SetOnBeforeDrop(const Value: TNotifyEvent);
    function GetOptions: TPJDropFilesOptions;
    procedure SetOptions(const Value: TPJDropFilesOptions);
    function GetIsFolder(Idx: Integer): Boolean;
    function GetOnFileFilter: TPJDroppedFileFilter;
    procedure SetOnFileFilter(const Value: TPJDroppedFileFilter);
    function GetFilter: TPJFileFilter;
    procedure SetFilter(const Value: TPJFileFilter);
  private // other
    fHelper: TPJDropFilesHelper;
      {Helper object that undertakes most of drag drop handling}
  protected
    procedure CreateWnd; override;
      {Window creation method - registers that window can accept dropped files}
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Sets Filter property to nil when referenced component is destroyed}
    procedure WMDropFiles(var Msg: TMessage); message WM_DROPFILES;
      {File drop message handler - used to record info about dropped files}
    procedure CMEnabledChanged(var Msg: TMessage); message CM_ENABLEDCHANGED;
      {Message triggered when Enabled property changes - used to toggle whether
      files can be accepted or not}
    procedure DoPassThrough(hDrop: THandle);
      {Event handler called by helper class that passes a customised drop files
      message on to owner control when PassThrough property is true}
  public
    constructor Create(AOwner: TComponent); override;
      {Control constructor - sets default values of properties and creates owned
      helper object}
    destructor Destroy; override;
      {Control destructor - frees owned helper object}
    procedure Paint; override;
      {Control paint handler - draws dashed outline to control only when
      designing}
    property Count: Integer read GetCount;
      {The number of files dropped}
    property Files[Idx: Integer]: string read GetFile;
      {The names of the dropped files}
    property FileName: string read GetFileName;
      {The name of the first file dropped, if any, otherwise empty string}
    property IsFolder[Idx: Integer]: Boolean read GetIsFolder;
      {Whether the dropped files in Files property are folders (true) or files
      (false)}
    property DropPoint: TPoint read GetDropPoint;
      {The mouse coordinates where the last files were dropped}
    property DropControl: TControl read GetDropControl;
      {The child control (if any) under the mouse when files are dropped -
      returns nil if no control under mouse}
  published
    { Inherited protected properties published }
    property Enabled stored True;
      {Enables/disables ability to receive dropped files}
    property Align;
    {$IFDEF DELPHI4ANDUP}
    property Anchors;
    property Constraints;
    {$ENDIF}
    property ParentShowHint;
    property ShowHint;
    property TabOrder;
    property TabStop;
    property Visible;
    { Inherited published properties given new defaults }
    property Width default 80;
    property Height default 60;
    { New properties }
    property Filter: TPJFileFilter
      read GetFilter write SetFilter;
      {Reference to a file filter component used to automatically filter dropped
      files. TPJFileFilter is an abstract class and concrete derived classes
      that provide different filtering options can be assigned to this property}
    property ForegroundOnDrop: Boolean
      read GetForegroundOnDrop write SetForegroundOnDrop;
      {When true the window containing the component is brought to front after
      files have been dropped}
    property Options: TPJDropFilesOptions
      read GetOptions write SetOptions;
      {Set of options that govern how dropped files are processed: whether
      simple files or folders are included in Files property and whether any
      folders are recursed}
    property PassThrough: Boolean
      read fPassThrough write fPassThrough;
      {Passes drop information on to main window as a custom message: can be
      intercepted by TPJFormDropFiles}
    { New events }
    property OnBeforeDrop: TNotifyEvent
      read GetOnBeforeDrop write SetOnBeforeDrop;
      {Event triggered just as drop occurs - before files have been processed
      (Files property is not valid at this point and shouldn't be accessed)}
    property OnDropFiles: TNotifyEvent
      read GetOnDropFiles write SetOnDropFiles;
      {Event triggered when files have been dropped and have been processed (
      Files property has been updated)}
    property OnFileFilter: TPJDroppedFileFilter
      read GetOnFileFilter write SetOnFileFilter;
      {Event triggered for each file dropped to enable user to filter out
      unwanted files or folders by setting event handler's Accept parameter to
      false}
  end;


  {
  TPJSubClassedDropFiles:
    Base class for drop files components handle drop files for an associated
    TWinControl by subclassing the associated control's window.
  }
  TPJSubClassedDropFiles = class(TComponent)
  private // properties
    fHelper: TPJAbstractDropFilesHelper;
    fEnabled: Boolean;
    function GetCount: Integer;
    function GetDropControl: TControl;
    function GetDropPoint: TPoint;
    function GetFile(Idx: Integer): string;
    function GetFileName: string;
    function GetFilter: TPJFileFilter;
    function GetForegroundOnDrop: Boolean;
    function GetIsFolder(Idx: Integer): Boolean;
    function GetOnBeforeDrop: TNotifyEvent;
    function GetOnDropFiles: TNotifyEvent;
    function GetOnFileFilter: TPJDroppedFileFilter;
    function GetOptions: TPJDropFilesOptions;
    procedure SetEnabled(const Value: Boolean);
    procedure SetFilter(const Value: TPJFileFilter);
    procedure SetForegroundOnDrop(const Value: Boolean);
    procedure SetOnBeforeDrop(const Value: TNotifyEvent);
    procedure SetOnDropFiles(const Value: TNotifyEvent);
    procedure SetOnFileFilter(const Value: TPJDroppedFileFilter);
    procedure SetOptions(const Value: TPJDropFilesOptions);
  protected
    function CreateAndInitHelper: TPJAbstractDropFilesHelper; virtual; abstract;
      {Creates and initializes helper object of appropriate type for component}
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Sets Filter property to nil when referenced component is destroyed}
    property Helper: TPJAbstractDropFilesHelper
      read fHelper write fHelper;
      {Helper class that undertakes most of drag drop handling}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates and intializes component}
    destructor Destroy; override;
      {Class destructor. Tears down component}
    { Run time properties }
    property Count: Integer read GetCount;
      {The number of files dropped}
    property DropControl : TControl read GetDropControl;
      {The child control (if any) under the mouse when files are dropped -
      returns nil if no control under mouse}
    property DropPoint: TPoint read GetDropPoint;
      {The mouse coordinates where the last files were dropped}
    property FileName: string read GetFileName;
      {The name of the first file dropped, if any, otherwise empty string}
    property Files[Idx: Integer]: string read GetFile;
      {The names of the dropped files}
    property IsFolder[Idx: Integer]: Boolean read GetIsFolder;
      {Whether the dropped files in Files property are folders (true) or files
      (false)}
  published
    { Properties }
    property Enabled: Boolean
      read fEnabled write SetEnabled default True;
      {Enables/disables ability to receive dropped files}
    property Filter: TPJFileFilter
      read GetFilter write SetFilter;
      {Reference to a file filter component used to automatically filter dropped
      files. TPJFileFilter is an abstract class and concrete derived classes
      that provide different filtering options can be assigned to this property}
    property ForegroundOnDrop: Boolean
      read GetForegroundOnDrop write SetForegroundOnDrop;
      {When true the window containing the component is brought to front after
      files have been dropped}
    property Options: TPJDropFilesOptions
      read GetOptions write SetOptions;
      {Set of options that govern how dropped files are processed: whether
      simple files or folders are included in Files property and whether any
      folders are recursed}
    { Events }
    property OnBeforeDrop: TNotifyEvent
      read GetOnBeforeDrop write SetOnBeforeDrop;
      {Event triggered just as drop occurs - before files have been processed
      (Files property is not valid at this point and shouldn't be accessed)}
    property OnDropFiles: TNotifyEvent
      read GetOnDropFiles write SetOnDropFiles;
      {Event triggered when files have been dropped and have been processed (
      Files property has been updated)}
    property OnFileFilter: TPJDroppedFileFilter
      read GetOnFileFilter write SetOnFileFilter;
      {Event triggered for each file dropped to enable user to filter out
      unwanted files or folders by setting event handler's Accept parameter to
      false}
  end;


  {
  TPJFormDropFiles:
    Component that subclasses owner form window to provide ability to catch
    files dropped anywhere on form window.
  }
  TPJFormDropFiles = class(TPJSubClassedDropFiles)
  private
    fOldWndProc, fNewWndProc: Pointer;
      {Reference to old window procedure and replacement used to trap file drop
      messages}
  protected
    procedure NewWndProc(var Msg: TMessage); virtual;
      {Window procedure used to sub-class owning form's window procedure to
      enable drop-file events to be handled}
    function FormHandle: THandle;
      {Returns window handle of owner form, or 0 if there is no owner}
    function CreateAndInitHelper: TPJAbstractDropFilesHelper; override;
      {Creates associated helper object}
  public
    constructor Create(AOwner: TComponent); override;
      {Component constructor. Creates component, ensuring that owner is a form
      and that no more that one of these components is present on the form}
    destructor Destroy; override;
      {Component destructor. Tears down component}
  end;


  {
  TPJCtrlDropFiles:
    Component that sub classes an associated controls window to provide ability
    to catch files dropped anywhere on associated control.
  }
  TPJCtrlDropFiles = class(TPJSubClassedDropFiles)
  private
    fManagedControl: TWinControl;
      {Storage for ManagedControl property}
    fPassThrough: Boolean;
      {Storage for PassThrough property}
    fOldWndProc, fNewWndProc: Pointer;
      {Reference to old window procedure and replacement used to trap file drop
      messages in handled control}
    procedure SetManagedControl(const Value: TWinControl);
      {Write access method for ManagedControl property}
    procedure RemoveManagedControl;
      {Removes currently managed control by switching off drop handling and
      restoring original window procedure}
    function ManagedControlHandle: THandle;
      {Returns handle of managed control or 0 if there is no managed control}
    function ClientToScreen(ClientPos: TPoint): TPoint;
      {Converts co-ordinates in terms of managed control's client area to screen
      co-ordinates}
  protected
    function CreateAndInitHelper: TPJAbstractDropFilesHelper; override;
      {Creates associated drop files helper object and sets up its OnAfterDrop
      event handler to enable messages to be passed through to the main form}
    procedure DoPassThrough(hDrop: THandle);
      {Event handler called by helper class that passes a customised drop files
      message on to owner control when PassThrough property is true}
    procedure Notification(AComponent: TComponent; Operation: TOperation);
      override;
      {Sets ManagedComponent property to nil when referenced component is
      destroyed}
    procedure NewWndProc(var Msg: TMessage); virtual;
      {Window procedure used to sub-class managed control's window procedure to
      enable drop-file events to be handled}
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Creates in initializes component}
    destructor Destroy; override;
      {Class destructor. Tears down component}
  published
    { Properties }
    property ManagedControl: TWinControl
      read fManagedControl write SetManagedControl;
      {Reference to associated control that we are managing drag and drop
      operations for}
    property PassThrough: Boolean
      read fPassThrough write fPassThrough default False;
      {Passes drop information on to main window as a custom message: can be
      intercepted by TPJFormDropFiles}
  end;


implementation


uses
  // Delphi
  SysUtils, Graphics, ShellAPI, Forms;


resourcestring
  // Error messages
  sOwnerNotForm = 'TPJFormDropFiles owner must be a form';
  sOnlyOneAllowed = 'Only one TPJFormDropFiles component is permitted on a '
    + 'form: %0:s is already present on %1:s';


var
  // Id of private, unique, PJ_DROPFILES message
  pvtPJDropFilesMsg: UINT;


{ TPJDropFiles }

procedure TPJDropFiles.CMEnabledChanged(var Msg : TMessage);
  {Message triggered when Enabled property changes - used to toggle whether
  files can be accepted or not}
begin
  inherited;
  fHelper.AcceptFiles(Enabled);
end;

constructor TPJDropFiles.Create(AOwner : TComponent);
  {Control constructor - sets default values of properties and creates owned
  helper object}
begin
  inherited Create(AOwner);
  // Make control accept child controls
  ControlStyle := ControlStyle + [csAcceptsControls];
  // Set default size
  Width := 80;
  Height := 60;
  // Ensure we're not a tab-stop
  TabStop := False;
  // Create helper object and set pass through handler
  fHelper := TPJDropFilesHelper.Create(Self);
  fHelper.fOnAfterDrop := DoPassThrough;
end;

procedure TPJDropFiles.CreateWnd;
  {Window creation method - registers that window can accept dropped files}
begin
  inherited CreateWnd;
  fHelper.AcceptFiles(Enabled);
end;

destructor TPJDropFiles.Destroy;
  {Control destructor - frees owned helper object}
begin
  fHelper.Free;
  inherited Destroy;
end;

procedure TPJDropFiles.DoPassThrough(hDrop: THandle);
  {Event handler called by helper class that passes a customised drop files
  message on to owner control when PassThrough property is true}
var
  OwnerCtrl: TWinControl; // control that owns this component
  ScreenPos: TPoint;      // screen position of drop point
  OwnerPos: TSmallPoint;  // position of drop point relative to owner control
begin
  if fPassThrough
    and Visible
    and Assigned(Owner)
    and (Owner is TWinControl) then
  begin
    // Find ultimate owning form
    OwnerCtrl := Owner as TWinControl;
    while Assigned(OwnerCtrl) and Assigned(OwnerCtrl.Owner)
      and (OwnerCtrl.Owner is TWinControl) do
      OwnerCtrl := OwnerCtrl.Owner as TWinControl;
    // Calculate drop position relative to owner control's window
    ScreenPos := ClientToScreen(fHelper.fDropPoint);
    OwnerPos := PointToSmallPoint(OwnerCtrl.ScreenToClient(ScreenPos));
    // Send the custom message to owner form containing new drop position
    SendMessage(OwnerCtrl.Handle, pvtPJDropFilesMsg, hDrop, Integer(OwnerPos));
  end;
end;

function TPJDropFiles.GetCount : integer;
  {Read access method for Count property - gets number of files dropped}
begin
  Result := fHelper.fFileList.Count;
end;

function TPJDropFiles.GetDropControl : TControl;
  {Read access method for DropControl property - returns child control which was
  under mouse when files were dropped, or nil if no child control was under
  mouse}
begin
  Result := fHelper.GetDropControl;
end;

function TPJDropFiles.GetDropPoint: TPoint;
  {Read access method for DropPoint property}
begin
  Result := fHelper.fDropPoint;
end;

function TPJDropFiles.GetFile(Idx: Integer): string;
  {Read access method for Files array property - returns name of file at the
  given array index}
begin
  Result := fHelper.fFileList[Idx];
end;

function TPJDropFiles.GetFileName: string;
  {Read access method for FileName property: returns name of first file dropped,
  if any, otherwise returns empty string}
begin
  if Count > 0 then
    Result := GetFile(0)
  else
    Result := '';
end;

function TPJDropFiles.GetFilter: TPJFileFilter;
  {Read access method for Filter property}
begin
  // This property can be accessed (from Notification method) before helper
  // object is created, so we test for that
  if Assigned(fHelper) then
    Result := fHelper.fFilterComp
  else
    Result := nil;
end;

function TPJDropFiles.GetForegroundOnDrop: Boolean;
  {Read access method for ForegroundOnDrop property}
begin
  Result := fHelper.fForegroundOnDrop;
end;

function TPJDropFiles.GetIsFolder(Idx: Integer): Boolean;
  {Read access method for IsFolder property}
begin
  Result := Boolean(fHelper.fFileList.Objects[Idx]);
end;

function TPJDropFiles.GetOnBeforeDrop: TNotifyEvent;
  {Read access method for OnBeforeDrop event property}
begin
  Result := fHelper.fOnBeforeDrop;
end;

function TPJDropFiles.GetOnDropFiles: TNotifyEvent;
  {Read access method for OnDropFiles event property}
begin
  Result := fHelper.fOnDropFiles;
end;

function TPJDropFiles.GetOnFileFilter: TPJDroppedFileFilter;
  {Read access method for OnFileFilter method}
begin
  Result := fHelper.fOnFileFilter;
end;

function TPJDropFiles.GetOptions: TPJDropFilesOptions;
  {Read access method for Options property}
begin
  Result := fHelper.fOptions;
end;

procedure TPJDropFiles.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Sets Filter property to nil when referenced component is destroyed}
begin
  inherited;
  if (AComponent = Filter) and (Operation = opRemove) then
    Filter := nil;
end;

procedure TPJDropFiles.Paint;
  {Control paint handler - draws dashed outline to control only when designing}
begin
  if csDesigning in ComponentState then
    with Canvas do
    begin
      Pen.Style := psDash;
      Brush.Style := bsClear;
      Rectangle(0, 0, Width, Height);
    end;
end;

procedure TPJDropFiles.SetFilter(const Value: TPJFileFilter);
  {Write access method for Filter property}
begin
  // Check helper object is assigned before attempting to set it: Notification
  // method may call this method before helper is created
  if Assigned(fHelper) then
    fHelper.fFilterComp := Value;
end;

procedure TPJDropFiles.SetForegroundOnDrop(const Value: Boolean);
  {Write access method for ForegroundOnDrop property}
begin
  fHelper.fForegroundOnDrop := Value;
end;

procedure TPJDropFiles.SetOnBeforeDrop(const Value: TNotifyEvent);
  {Write access method for OnBeforeDrop event property}
begin
  fHelper.fOnBeforeDrop := Value;
end;

procedure TPJDropFiles.SetOnDropFiles(const Value: TNotifyEvent);
  {Write access method for OnDropFiles event property}
begin
  fHelper.fOnDropFiles := Value;
end;

procedure TPJDropFiles.SetOnFileFilter(const Value: TPJDroppedFileFilter);
  {Write access methof for OnFileFilter event property}
begin
  fHelper.fOnFileFilter := Value;
end;

procedure TPJDropFiles.SetOptions(const Value: TPJDropFilesOptions);
  {Write access method for Options property}
begin
  fHelper.fOptions := Value;
end;

procedure TPJDropFiles.WMDropFiles(var Msg: TMessage);
  {File drop message handler - used to record info about dropped files}
begin
  // Trigger event that indicates drop is about to start
  fHelper.BeforeDrop;
  // Collect dropped files
  fHelper.FilesDropped(Msg.WParam);
  // Trigger drop files event
  fHelper.DropFiles;
  // We handled message
  Msg.Result := 0;
end;


{ TPJSubClassedDropFiles }

constructor TPJSubClassedDropFiles.Create(AOwner: TComponent);
  {Class constructor. Creates and intializes component}
begin
  inherited;
  fHelper := CreateAndInitHelper;
  fEnabled := True;
end;

destructor TPJSubClassedDropFiles.Destroy;
  {Class destructor. Tears down component}
begin
  fHelper.Free;
  inherited;
end;

function TPJSubClassedDropFiles.GetCount: Integer;
  {Read access method for Count property}
begin
  Result := Helper.fFileList.Count;
end;

function TPJSubClassedDropFiles.GetDropControl: TControl;
  {Read access method for DropControl property - returns child control of owner
  form which was under mouse when files were dropped, or nil if if no child
  control was under mouse}
begin
  Result := Helper.GetDropControl;
end;

function TPJSubClassedDropFiles.GetDropPoint: TPoint;
  {Read access method for DropPoint property}
begin
  Result := Helper.fDropPoint;
end;

function TPJSubClassedDropFiles.GetFile(Idx: Integer): string;
  {Read access method for Files property}
begin
  Result := Helper.fFileList[Idx];
end;

function TPJSubClassedDropFiles.GetFileName: string;
  {Read access method for FileName property: returns name of first file dropped,
  if any, otherwise returns empty string}
begin
  if Count > 0 then
    Result := GetFile(0)
  else
    Result := '';
end;

function TPJSubClassedDropFiles.GetFilter: TPJFileFilter;
  {Read access method for Filter property}
begin
  // This property can be accessed (from Notification method) before helper
  // object is created, so we test for that
  if Assigned(Helper) then
    Result := Helper.fFilterComp
  else
    Result := nil;
end;

function TPJSubClassedDropFiles.GetForegroundOnDrop: Boolean;
  {Read access method for ForegroundOnDrop property}
begin
  Result := Helper.fForegroundOnDrop;
end;

function TPJSubClassedDropFiles.GetIsFolder(Idx: Integer): Boolean;
  {Read access method for IsFolder property}
begin
  Result := Boolean(Helper.fFileList.Objects[Idx])
end;

function TPJSubClassedDropFiles.GetOnBeforeDrop: TNotifyEvent;
  {Read access method for OnBeforeDrop event property}
begin
  Result := Helper.fOnBeforeDrop;
end;

function TPJSubClassedDropFiles.GetOnDropFiles: TNotifyEvent;
  {Read access method for OnDropFiles event property}
begin
  Result := Helper.fOnDropFiles;
end;

function TPJSubClassedDropFiles.GetOnFileFilter: TPJDroppedFileFilter;
  {Read access method for OnFileFilter event property}
begin
  Result := Helper.fOnFileFilter;
end;

function TPJSubClassedDropFiles.GetOptions: TPJDropFilesOptions;
  {Read access method for Options property}
begin
  Result := Helper.fOptions;
end;

procedure TPJSubClassedDropFiles.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Sets Filter property to nil when referenced component is destroyed}
begin
  inherited;
  if (AComponent = Filter) and (Operation = opRemove) then
    Filter := nil;
end;

procedure TPJSubClassedDropFiles.SetEnabled(const Value: Boolean);
  {Write access method for Enabled property: sets value and whether accepts
  dragged files}
begin
  if fEnabled <> Value then
  begin
    fEnabled := Value;
    Helper.AcceptFiles(Value);
  end;
end;

procedure TPJSubClassedDropFiles.SetFilter(const Value: TPJFileFilter);
  {Write access method for Filter property}
begin
  // Check helper object is assigned before attempting to set it: Notification
  // method may call this method before helper is created
  if Assigned(Helper) then
    Helper.fFilterComp := Value;
end;

procedure TPJSubClassedDropFiles.SetForegroundOnDrop(const Value: Boolean);
  {Write access method for ForegroundOnDrop property}
begin
  Helper.fForegroundOnDrop := Value;
end;

procedure TPJSubClassedDropFiles.SetOnBeforeDrop(
  const Value: TNotifyEvent);
  {Write access method for OnBeforeDrop event property}
begin
  Helper.fOnBeforeDrop := Value;
end;

procedure TPJSubClassedDropFiles.SetOnDropFiles(const Value: TNotifyEvent);
  {Write access method for OnDropFiles event property}
begin
  Helper.fOnDropFiles := Value;
end;

procedure TPJSubClassedDropFiles.SetOnFileFilter(
  const Value: TPJDroppedFileFilter);
  {Write access method for OnFileFilter event property}
begin
  Helper.fOnFileFilter := Value;
end;

procedure TPJSubClassedDropFiles.SetOptions(
  const Value: TPJDropFilesOptions);
  {Write access method for Options property}
begin
  Helper.fOptions := Value;
end;


{ TPJFormDropFiles }

constructor TPJFormDropFiles.Create(AOwner: TComponent);
  {Component constructor. Creates component, ensuring that owner is a form and
  that no more that one of these components is present on the form}
var
  Idx: Integer; // scans thru components on owner form
begin
  // Check if owner is a form
  if (AOwner = nil) or not (AOwner is TForm) then
    raise Exception.Create(sOwnerNotForm);
  // Ensure that component is unique
  for Idx := 0 to Pred(AOwner.ComponentCount) do
    if AOwner.Components[Idx] is TPJFormDropFiles then
      raise Exception.CreateFmt(sOnlyOneAllowed,
        [AOwner.Components[Idx].Name, AOwner.Name]);
  inherited Create(AOwner);
  // Subclass the form (run time only)
  if not (csDesigning in ComponentState) then
  begin
    // install new window procedure and record previous one
    {$IFDEF DELPHI6ANDUP}
      fNewWndProc := Classes.MakeObjectInstance(NewWndProc);
    {$ELSE}
      fNewWndProc := Forms.MakeObjectInstance(NewWndProc);
    {$ENDIF}
    fOldWndProc := Pointer(SetWindowLong(
      FormHandle,
      GWL_WNDPROC,
      Integer(fNewWndProc)));
    // if enabled, notify that we can accept files
    Helper.AcceptFiles(Enabled);
  end
  else
  begin
    fNewWndProc := nil;
    fOldWndProc := nil;
  end;
end;

function TPJFormDropFiles.CreateAndInitHelper: TPJAbstractDropFilesHelper;
  {Creates associated helper object}
begin
  Result := TPJFormDropFilesHelper.Create(Self);
end;

destructor TPJFormDropFiles.Destroy;
  {Component destructor. Tears down component}
begin
  // Restore original window procedure (if sub-classed)
  if Assigned(fNewWndProc) then
  begin
    Helper.AcceptFiles(False);
    if FormHandle <> 0 then
      SetWindowLong(FormHandle, GWL_WNDPROC, Integer(fOldWndProc));
    {$IFDEF DELPHI6ANDUP}
      Classes.FreeObjectInstance(fNewWndProc);
    {$ELSE}
      Forms.FreeObjectInstance(fNewWndProc);
    {$ENDIF}
  end;
  inherited Destroy;
end;

function TPJFormDropFiles.FormHandle: THandle;
  {Returns window handle of owner form, or 0 if there is no owner}
begin
  if Assigned(Owner) and (Owner is TForm) then
    Result := (Owner as TForm).Handle
  else
    Result := 0;
end;

procedure TPJFormDropFiles.NewWndProc(var Msg: TMessage);
  {Window procedure used to sub-class owning form's window procedure to enable
  drop-file events to be handled}
begin
  // Handle messages
  if (Msg.Msg = WM_DROPFILES) or (Msg.Msg = pvtPJDropFilesMsg) then
  begin
    try
      // We only handle WM_DROPFILES and PJ_DROPFILES messages
      // check if control enabled
      if Enabled then
      begin
        // trigger OnBeforeDrop event
        Helper.BeforeDrop;
        // collect dropped files
        Helper.FilesDropped(Msg.WParam);
        if Msg.Msg = pvtPJDropFilesMsg then
          // msg passed thru: change drop point to be relative to this window
          Helper.fDropPoint := SmallPointToPoint(TSmallPoint(Msg.LParam));
        // trigger OnDropFiles event
        Helper.DropFiles;
      end;
      // we handled message
      if Msg.Msg = pvtPJDropFilesMsg then
        Msg.Result := 1
      else
        Msg.Result := 0;
    except
      // Get application to handle exceptions: they cause app to halt otherwise
      on E: Exception do
        Application.HandleException(E);
    end;
  end
  else
    // All other messages are handled by original window proc for form
    Msg.Result := CallWindowProc(fOldWndProc, FormHandle, Msg.Msg,
      Msg.WParam, Msg.LParam);
end;


{ TPJCtrlDropFiles }

function TPJCtrlDropFiles.ClientToScreen(ClientPos: TPoint): TPoint;
  {Converts co-ordinates in terms of managed control's client area to screen
  co-ordinates}
begin
  Result := ClientPos;
  Windows.ClientToScreen(ManagedControlHandle, Result);
end;

constructor TPJCtrlDropFiles.Create(AOwner: TComponent);
  {Class constructor. Creates in initializes component}
begin
  inherited;
  if not (csDesigning in ComponentState) then
  begin
    // install new window procedure and record previous one
    {$IFDEF DELPHI6ANDUP}
      fNewWndProc := Classes.MakeObjectInstance(NewWndProc);
    {$ELSE}
      fNewWndProc := Forms.MakeObjectInstance(NewWndProc);
    {$ENDIF}
  end;
end;

function TPJCtrlDropFiles.CreateAndInitHelper: TPJAbstractDropFilesHelper;
  {Creates associated drop files helper object and sets up its OnAfterDrop
  event handler to enable messages to be passed through to the main form}
begin
  Result := TPJCtrlDropFilesHelper.Create(Self);
  Result.fOnAfterDrop := DoPassThrough;
end;

destructor TPJCtrlDropFiles.Destroy;
  {Class destructor. Tears down component}
begin
  if Assigned(fNewWndProc) then
  begin
    {$IFDEF DELPHI6ANDUP}
      Classes.FreeObjectInstance(fNewWndProc);
    {$ELSE}
      Forms.FreeObjectInstance(fNewWndProc);
    {$ENDIF}
  end;
  inherited;
end;

procedure TPJCtrlDropFiles.DoPassThrough(hDrop: THandle);
  {Event handler called by helper class that passes a customised drop files
  message on to owner control when PassThrough property is true}
var
  OwnerCtrl: TWinControl; // control that owns this component
  ScreenPos: TPoint;      // screen position of drop point
  OwnerPos: TSmallPoint;  // position of drop point relative to owner control
begin
  // Only pass the event through if required by PassThrough property
  if fPassThrough
    and Assigned(fManagedControl)
    and (fManagedControl.Visible)
    and Assigned(Owner)
    and (Owner is TWinControl) then
  begin
    // Find ultimate owning form
    OwnerCtrl := Owner as TWinControl;
    while Assigned(OwnerCtrl) and Assigned(OwnerCtrl.Owner)
      and (OwnerCtrl.Owner is TWinControl) do
      OwnerCtrl := OwnerCtrl.Owner as TWinControl;
    // Calculate drop position relative to managed control's window
    ScreenPos := ClientToScreen(Helper.fDropPoint);
    OwnerPos := PointToSmallPoint(OwnerCtrl.ScreenToClient(ScreenPos));
    // Send the custom message to owner control containing new drop position
    SendMessage(OwnerCtrl.Handle, pvtPJDropFilesMsg, hDrop, Integer(OwnerPos));
  end;
end;

function TPJCtrlDropFiles.ManagedControlHandle: THandle;
  {Returns handle of managed control or 0 if there is no managed control}
begin
  if Assigned(fManagedControl) then
    Result := fManagedControl.Handle
  else
    Result := 0;
end;

procedure TPJCtrlDropFiles.NewWndProc(var Msg: TMessage);
  {Window procedure used to sub-class managed control's window procedure to
  enable drop-file events to be handled}
begin
  // Handle messages
  if Msg.Msg = WM_DROPFILES then
  begin
    try
      // We only handle WM_DROPFILES messages if control enabled
      if Enabled then
      begin
        Helper.BeforeDrop;
        Helper.FilesDropped(Msg.WParam);
        Helper.DropFiles;
      end;
      // we handled message
      Msg.Result := 0;
    except
      // Get application to handle exceptions: they cause app to halt otherwise
      on E: Exception do
        Application.HandleException(E);
    end;
  end
  else
    // All other messages are handled by original window proc for form
    Msg.Result := CallWindowProc(fOldWndProc, ManagedControlHandle, Msg.Msg,
      Msg.WParam, Msg.LParam);
end;

procedure TPJCtrlDropFiles.Notification(AComponent: TComponent;
  Operation: TOperation);
  {Sets ManagedComponent property to nil when referenced component is destroyed}
begin
  inherited;
  if (AComponent = fManagedControl) and (Operation = opRemove) then
    fManagedControl := nil;
    // We don't do RemoveManagedControl because by this time removed control
    // has no window handle
end;

procedure TPJCtrlDropFiles.RemoveManagedControl;
  {Removes currently managed control by switching off drop handling and
  restoring original window procedure}
begin
  if not (csDesigning in ComponentState) then
  begin
    Helper.AcceptFiles(False);
    if ManagedControlHandle <> 0 then
      SetWindowLong(ManagedControlHandle, GWL_WNDPROC, Integer(fOldWndProc));
  end;
  fManagedControl := nil;
end;

procedure TPJCtrlDropFiles.SetManagedControl(const Value: TWinControl);
  {Write access method for ManagedControl property}
begin
  // Removed any previous control before recording new one
  RemoveManagedControl;
  fManagedControl := Value;
  // Subclass the managed control and set up for drag drop (run time only)
  if not (csDesigning in ComponentState) then
  begin
    fOldWndProc := Pointer(SetWindowLong(
      ManagedControlHandle,
      GWL_WNDPROC,
      Integer(fNewWndProc))
    );
    Helper.AcceptFiles(Enabled);
  end;
end;


{ TPJAbstractDropFilesHelper }

procedure TPJAbstractDropFilesHelper.AcceptFiles(Flag: Boolean);
  {Calls DragAcceptFiles with given value, if not designing and if related
  window handle exists}
var
  Handle: THandle;  // window handle that receives drag drop messages
begin
  Handle := GetHWnd;
  if not (csDesigning in fComp.ComponentState) and (Handle <> 0) then
    DragAcceptFiles(Handle, Flag);
end;

procedure TPJAbstractDropFilesHelper.BeforeDrop;
  {Called before dropped files are processed. Triggers OnBeforeDrop event}
begin
  if Assigned(fOnBeforeDrop) then
    fOnBeforeDrop(fComp);
end;

constructor TPJAbstractDropFilesHelper.Create(Comp: TComponent);
  {Class constructor: records reference to related component and helper object}
begin
  inherited Create;
  fComp := Comp;
  fFileList := TStringList.Create;
  fOptions := [dfoIncFolders, dfoIncFiles];
end;

destructor TPJAbstractDropFilesHelper.Destroy;
  {Class destructor: frees helper object}
begin
  fFileList.Free;
  inherited Destroy;
end;

procedure TPJAbstractDropFilesHelper.DropFiles;
  {Called after dropped files are processed and drop handle has been freed.
  Triggers OnDropFiles event and brings drop target to front if required}
begin
  // Bring window to foreground if required
  if fForegroundOnDrop then
    SetForegroundWindow(GetHWnd);
  // Trigger OnDropFiles event
  if Assigned(fOnDropFiles) then
    fOnDropFiles(fComp);
end;

procedure TPJAbstractDropFilesHelper.FilesDropped(hDrop: THandle);
  {Uses drop handle to get list of dropped files and to set drop point. Triggers
  OnAfterDrop event before drop handle is destroyed}
var
  FileName: string; // name of a dropped file
  NameLen: Word;    // length of buffer required for name of dropped file
  NumDropped: Word; // number of files dropped
  Idx: Integer;     // loops through all dropped files
begin
  try
    // Clear file list
    fFileList.Clear;
    // Find number of files dropped
    NumDropped := DragQueryFile(hDrop, Cardinal(-1), nil, 0);
    for Idx := 0 to Pred(NumDropped) do
    begin
      // Find size required for filename buffer (without terminal #0)
      NameLen := DragQueryFile(hDrop, Idx, nil, 0);
      // Get name of dropped file
      SetLength(FileName, NameLen);   // Delphi adds space for terminal #0
      DragQueryFile(hDrop, Idx, PChar(FileName), NameLen + 1);
      // Process name of dropped file: add to list or optionally recurse if
      // folder
      HandleFile(FileName);
    end;
    // Find co-ordinates where files dropped
    DragQueryPoint(hDrop, fDropPoint);
    // Perform any additional special processing before drop handle destroyed
    if Assigned(fOnAfterDrop) then
      fOnAfterDrop(hDrop);
  finally
    // Release handle assoc. with drag/drop
    DragFinish(hDrop);
  end;
end;

function TPJAbstractDropFilesHelper.GetDropControl: TControl;
  {Returns reference to control at position where cursor was released, or nil if
  no control under drop point}

  // ---------------------------------------------------------------------------
  function FindCtrl(ParentCtrl: TWinControl; Pos: TPoint): TControl;
    {Find any child control of given parent control that contains given point
    (in screen co-ordinates) and return reference to it. Return nil if no such
    control}
  var
    Idx: Integer;         // loops thru child controls
    ChildCtrl: TControl;  // reference to a child control
  begin
    // Assumed not found
    Result := nil;
    // Change point to be relative to parent control
    Pos := ParentCtrl.ScreenToClient(Pos);
    // Scan child controls searching for match
    for Idx := 0 to Pred(ParentCtrl.ControlCount) do
    begin
      ChildCtrl := ParentCtrl.Controls[Idx];
      if PtInRect(ChildCtrl.BoundsRect, Pos) and ChildCtrl.Visible then
      begin
        Result := ChildCtrl;
        Break;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Container: TWinControl; // reference to control represented by component
  Pt: TPoint;             // drop point in screen co-ordinates
  ChildCtrl: TControl;    // reference to child control containing point
begin
  // Assume there's no control under drop point
  Result := nil;
  // Begin with control window associated with drop files control
  Container := GetContainer;
  if Assigned(Container) then
  begin
    // Check that drop point is in container: no control if not
    if PtInRect(Container.ClientRect, fDropPoint) then
    begin
      // Record drop point in screen co-ordinates
      Pt := Container.ClientToScreen(fDropPoint);
      // Find inner-most child control that is child of container
      ChildCtrl := Container;
      while (ChildCtrl <> nil) do
      begin
        Result := ChildCtrl;
        if Result is TWinControl then
          // we've found a windowed control containing point:
          // check if any of it's child controls also contain it
          ChildCtrl := FindCtrl(Result as TWinControl, Pt)
        else
          ChildCtrl := nil;
      end;
    end;
  end;
end;

function TPJAbstractDropFilesHelper.GetHWnd: THandle;
  {Returns window handle that related control receives messages from. This is
  window handle of container returned by GetContainer method}
var
  Ctrl: TWinControl;  // Control that related component works with
begin
  // Get related control
  Ctrl := GetContainer;
  // Now return any handle
  if Assigned(Ctrl) then
    Result := Ctrl.Handle
  else
    Result := 0;
end;

procedure TPJAbstractDropFilesHelper.HandleFile(const FileName: string);
  {Processes the given file and updates file list according to RecurseFolders
  and ExcludeFolders properties. If ExcludeFolders is true then any folder names
  encoutnered are exlcuded from file list. If RecurseFolders is true then any
  folders and examined and the files in the folder are added to list (taking
  note of ExcludeFolders). Any subfolders are also searched recursively}

  // ---------------------------------------------------------------------------
  function MakePath(FolderName: string): string;
    {Esnures the given folder name ends with a '\'}
  begin
    Result := FolderName;
    if (Length(Result) > 0) and (Result[Length(Result)] <> '\') then
      Result := Result + '\';
  end;

  function IsFolder(const FileName: string): Boolean;
    {Returns true if given file name is a folder and false if not}
  begin
    Result := SysUtils.FileGetAttr(FileName) and faDirectory = faDirectory;
  end;

  function PassesFilters(const FileName: string;
    const FileIsFolder: Boolean): Boolean;
    {Returns true if the given file or folder passes through any filters: either
    an assigned filter component or a filter event handler}
  begin
    if Assigned(fFilterComp) then
      // We have filter component: first check if we pass thru its filter
      Result := fFilterComp.Accept(FileName, FileIsFolder)
    else
      // No filter component: assume OK
      Result := True;
    // If we survived component filter, pass to any filter event handler
    if Result and Assigned(fOnFileFilter) then
      fOnFileFilter(Self, FileName, FileIsFolder, Result);
  end;

  procedure UpdateList(const FileName: string; const FileIsFolder: Boolean);
    {Updates file list with given file name. Name is added to list if it is
    required by Options property}
  begin
    if FileIsFolder then
    begin
      // this is folder: record only if folders included and passes filters
      if (dfoIncFolders in fOptions) and PassesFilters(FileName, True) then
        fFileList.AddObject(FileName, Pointer(True));
    end
    else
    begin
      // this is file: record only if folders included and passes filters
      if (dfoIncFiles in fOptions) and PassesFilters(FileName, False) then
        fFileList.AddObject(FileName, Pointer(False));
    end;
  end;
  // ---------------------------------------------------------------------------

var
  Path: string;     // folder name as a path
  SR: TSearchRec;   // record used by file search functions
  Res: Integer;     // result of file search functions (0 on success)
begin
  // Decide whether we need to examine files in a folder
  if IsFolder(FileName) then
  begin
    // We have a folder: add to list if OK to do so
    UpdateList(FileName, True);
    if (dfoRecurseFolders in fOptions) then
    begin
      // We have a folder and we want to recurse it
      // ensure folder name is a valid '\' terminated path
      Path := MakePath(FileName);
      // find all files in folder and recursively process each one found
      // (providing it is not a '.' or '..' special folder)
      Res := SysUtils.FindFirst(Path  + '*.*', faAnyFile, SR);
      try
        while Res = 0 do
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            HandleFile(Path + SR.Name);
          Res := SysUtils.FindNext(SR);
        end;
      finally
        SysUtils.FindClose(SR);
      end;
    end;
  end
  else
    // Not a folder - this is simple file: add to list if OK to do so
    UpdateList(FileName, False);
end;


{ TPJDropFilesHelper }

function TPJDropFilesHelper.GetContainer: TWinControl;
  {Returns return reference to the component's related windowed control: this is
  the control itself in this class}
begin
  Result := fComp as TWinControl;
end;


{ TPJFormDropFilesHelper }

function TPJFormDropFilesHelper.GetContainer: TWinControl;
  {Returns return reference to the component's related windowed control: this is
  the form on which owns the component}
var
  Owner: TComponent;  // owning control (a form)
begin
  // Get reference to component's owner
  Owner := (fComp as TPJFormDropFiles).Owner;
  // Return reference to owner or nil if  owner not win control
  if Owner is TWinControl then
    Result := Owner as TWinControl
  else
    Result := nil;
end;


{ TPJCtrlDropFilesHelper }

function TPJCtrlDropFilesHelper.GetContainer: TWinControl;
  {Returns return reference to the control managed by the related component}
begin
  Result := (fComp as TPJCtrlDropFiles).ManagedControl;
end;


{ TPJExtFileFilter }

function TPJExtFileFilter.Accept(const FilePath: string;
  const IsFolder: Boolean): Boolean;
  {Check the given file or folder (per IsFolder) and return true if the given
  file is of required type (per Style property) and has extension that matches
  one of those specified in Extensions property}

  // ---------------------------------------------------------------------------
  function IsFileExtValid(const FileName: string): Boolean;
    {Returns true if given file name's extension match one of the extensions in
    the list}
  var
    ExtIdx: Integer;  // loops thru all extensions in valid list
  begin
    Result := False;
    for ExtIdx := 0 to Pred(fExtensions.Count) do
    begin
      if AnsiCompareText(ExtractFileExt(FileName), fExtensions[ExtIdx]) = 0 then
      begin
        Result := True;
        Break;
      end;
    end;
  end;
  // ---------------------------------------------------------------------------

begin
  if Assigned(fExtensions) then
  begin
    // Apply filter as required
    case fStyle of
      fsFilterFilesOnly:
        // Apply filter to files, passing folders through unchecked
        Result := IsFolder or IsFileExtValid(FilePath);
      fsFilterFoldersOnly:
        // Apply filter to folders, passing files through unchecked
        Result := not IsFolder or IsFileExtValid(FilePath);
      fsAll:
        // Apply filter to files and folders
        Result := IsFileExtValid(FilePath);
      else
        // Catch all case: all valid styles trapped above
        Result := False;
    end;
  end
  else
    // no extensions recorded: always succeed
    Result := True;
end;

constructor TPJExtFileFilter.Create(AOwner: TComponent);
  {Class constructor: creates component and sets default property values}
begin
  inherited;
  fStyle := fsFilterFilesOnly;
end;

destructor TPJExtFileFilter.Destroy;
  {Class destructor: frees owned objects}
begin
  // Free any string list holding extensions
  fExtensions.Free;
  inherited;
end;

function TPJExtFileFilter.GetExtensions: string;
  {Read accessor for Extensions property: scans string list holding list of
  extensions and concatenates the extensions, separating with ';'}
var
  Idx: Integer; // loops thru string list
begin
  Result := '';
  if Assigned(fExtensions) then // fExtensions = nil => no extensions
  begin
    for Idx := 0 to Pred(fExtensions.Count) do
    begin
      if Idx = 0 then
        Result := Result + fExtensions[Idx]
      else
        Result := Result + ';' + fExtensions[Idx];
    end;
  end;
end;

procedure TPJExtFileFilter.SetExtensions(const Value: string);
  {Write accessor for Extensions property: parses given ';' separated list of
  extensions, ensures each extension begins with '.' and stores extensions in a
  string list}
var
  SepPos: Integer;    // position of ';' separator in string
  Remainder: string;  // remainder of value string still to be processed
  Ext: string;        // an extension
begin
  if Value <> '' then
  begin
    // Clear list of extension (or create if we have no list yet)
    if Assigned(fExtensions) then
      fExtensions.Clear
    else
      fExtensions := TStringList.Create;
    // Scan given string splitting out extensions
    Remainder := Value;
    repeat
      // Get next extension
      SepPos := AnsiPos(';', Remainder);
      if SepPos = 0 then
        // remainder of string holds one extension
        Ext := Remainder
      else
      begin
        // >1 extension in string: copy first one and delete from string
        Ext := Copy(Remainder, 1, SepPos - 1);
        Delete(Remainder, 1, SepPos);
      end;
      // Ensure extension starts with '.'
      if (Ext <> '') and (Ext[1] <> '.') then
        Ext := '.' + Ext;
      // Add extension if we have one
      if Ext <> '' then
        fExtensions.Add(Ext);
    until SepPos = 0;
  end
  else
  begin
    // Empty list => no extensions: free string list
    fExtensions.Free;
    fExtensions := nil;
  end;
end;


{ TPJWildCardFileFilter }

function TPJWildCardFileFilter.Accept(const FilePath: string;
  const IsFolder: Boolean): Boolean;
  {Check the given file or folder (per IsFolder) and return true if the given
  file matches wild card in current folder}
var
  Folder: string;   // folder containing given file name
  SR: TSearchRec;   // record used by file search functions
  Res: Integer;     // result of file search functions (0 on success)
begin
  if fWildCard <> '' then
  begin
    // We have wildcard: check for match
    // check if we've changed folder or wildcard since last file processed
    Folder := ExtractFilePath(FilePath);
    if (Folder <> fCurrentFolder) or fWildCardChanged then
    begin
      // folder or wildcard has changed:
      // get files matching wildcard in current folder
      fMatchingFiles.Clear;
      Res := SysUtils.FindFirst(Folder + fWildCard, faAnyFile, SR);
      try
        while Res = 0 do
        begin
          if (SR.Name <> '.') and (SR.Name <> '..') then
            // we have match: add file name to list
            fMatchingFiles.Add(SR.Name);
          Res := SysUtils.FindNext(SR);
        end;
      finally
        SysUtils.FindClose(SR);
      end;
      // record new current folder
      fCurrentFolder := Folder;
    end;
    // look up file name in list of matching files in current folder
    // we return true if file is in list
    Result := fMatchingFiles.IndexOf(ExtractFileName(FilePath)) > -1;
  end
  else
    // No wild card: return True (all match)
    Result := True;
  // Note that we've used the current wildcard
  fWildCardChanged := False;
end;

constructor TPJWildCardFileFilter.Create(AOwner: TComponent);
  {Class constructor: creates component and owned object}
begin
  inherited;
  // Create list to hold files that match widlcard in current folder
  fMatchingFiles := TStringList.Create;
end;

destructor TPJWildCardFileFilter.Destroy;
  {Class destructor: frees owned objects}
begin
  fMatchingFiles.Free;
  inherited;
end;


procedure TPJWildCardFileFilter.SetWildCard(const Value: string);
  {Write accessor for WildCard property: records that wild card has changed}
begin
  fWildCard := Value;
  fWildCardChanged := True;
end;

initialization


// Register custom message
pvtPJDropFilesMsg := RegisterWindowMessage('PJ_DROPFILES');


end.

