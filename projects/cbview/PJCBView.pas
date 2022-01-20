{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1999-2014, Peter Johnson (www.delphidabbler.com).
 *
 * Clipboard Viewer Component source code. Implements a component that monitors
 * the Windows clipboard and triggers an event whenever the content of the
 * clipboard changes.
 *
 * Thanks to Mason Wheeler for providing the clipboard listener code using the
 * more reliable AddClipboardFormatListener and RemoveClipboardFormatListener
 * API functions.
}


unit PJCBView;


{$DEFINE AllocateHWndIsInFormsUnit}
{$UNDEF RequiresRTLNameSpaces}
{$UNDEF SupportsRaiseLastOSError}
{$UNDEF SupportsStrict}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RequiresRTLNameSpaces}
  {$IFEND}
  {$IF CompilerVersion >= 18.0} // Delphi 2006 and later
    {$DEFINE SupportsStrict}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$UNDEF AllocateHWndIsInFormsUnit}
    {$DEFINE SupportsRaiseLastOSError}
  {$IFEND}
{$ENDIF}


interface


uses
  {$IFNDEF RequiresRTLNameSpaces}
  Windows,
  Messages,
  Classes;
  {$ELSE}
  Winapi.Windows,
  Winapi.Messages,
  System.Classes;
  {$ENDIF}


type
  ///  <summary>Component that monitors the Windows clipboard and triggers an
  ///  event whenever the clipboard contents change.</summary>
  ///  <remarks>On versions of Windows that support it the newer clipboard
  ///  format listener API is used to monitor the clipboard. On older versions
  ///  of Windows the older, and less reliable, clipboard viewer API is used
  ///  instead.</remarks>
  TPJCBViewer = class(TComponent)
  {$IFDEF SupportsStrict}strict{$ENDIF}
  private
    ///  <summary>Reference to any OnClipboardChanged event handler.</summary>
    fOnClipboardChanged: TNotifyEvent;
    ///  <summary>Value of TriggerOnCreation property.</summary>
    fTriggerOnCreation: Boolean;
    ///  <summary>Value of Enabled property.</summary>
    fEnabled: Boolean;
    ///  <summary>Handle of hidden clipboard viewer window.</summary>
    fHWnd: HWND;
    ///  <summary>Handle of next clipboard viewer handle in chain.</summary>
    ///  <remarks>Used only when old clipboard viewer API is in use, i.e. when
    ///  fUseNewAPI is False.</remarks>
    fHWndNextViewer: HWND;
    ///  <summary>Flag indicating if the new style clipboard format listener API
    ///  is available on the current OS.</summary>
    fUseNewAPI: Boolean;
    ///  <summary>Reference to AddClipboardFormatListener API function.
    ///  </summary>
    ///  <remarks>This reference is nil if the function is not supported by the
    ///  OS, i.e. if fUseNewAPI is False.</remarks>
    fAddClipboardFormatListener: function(hwnd: HWND): BOOL; stdcall;
    ///  <summary>Reference to RemoveClipboardFormatListener API function.
    ///  </summary>
    ///  <remarks>This reference is nil if the function is not supported by the
    ///  OS, i.e. if fUseNewAPI is False.</remarks>
    fRemoveClipboardFormatListener: function(hwnd: HWND): BOOL; stdcall;
    ///  <summary>Reference to SetClipboardViewer API function.</summary>
    ///  <remarks>This reference is nil if the newer clipboard format listener
    ///  API is available, i.e. if fUseNewAPI is True.</remarks>
    fSetClipboardViewer: function (hWndNewViewer: HWND): HWND; stdcall;
    ///  <summary>Reference to ChangeClipboardChain API function.</summary>
    ///  <remarks>This reference is nil if the newer clipboard format listener
    ///  API is available, i.e. if fUseNewAPI is True.</remarks>
    fChangeClipboardChain: function(hWndRemove, hWndNewNext: HWND): BOOL;
      stdcall;
  {$IFDEF SupportsStrict}strict{$ENDIF}
  protected
    ///  <summary>Triggers OnClipboardChanged event iff a handler is assigned
    ///  and the component is enabled.</summary>
    procedure ClipboardChanged; dynamic;
    ///  <summary>Fires OnClipboardChanged on component creation iff
    ///  TriggerOnCreation is True.</summary>
    procedure Loaded; override;
    ///  <summary>Window procedure for hidden clipboard viewer window.</summary>
    procedure WndMethod(var Msg: TMessage); virtual;
  public
    ///  <summary>Constructs new component instance and creates hidden clipboard
    ///  viewer window.</summary>
    constructor Create(AOwner: TComponent); override;
    ///  <summary>Destroys component instance and its hidden clipboard window.
    ///  </summary>
    destructor Destroy; override;
  published
    ///  <summary>Event triggered when clipboard contents change.</summary>
    ///  <remarks>Event will also be triggered on creation of the component iff
    ///  TriggerOnCreation property is True.</remarks>
    property OnClipboardChanged: TNotifyEvent
      read fOnClipboardChanged write fOnClipboardChanged;
    ///  <summary>Determines whether an OnClipboardChanged event is triggered
    ///  when the component is created.</summary>
    ///  <remarks>This property should only be set at design time. It has no
    ///  effect if set at run time.</remarks>
    property TriggerOnCreation: Boolean
      read fTriggerOnCreation write fTriggerOnCreation default True;
    ///  <summary>Enables and disables the component. When False
    ///  OnClipboardChanged events are never fired.</summary>
    property Enabled: Boolean read fEnabled write fEnabled default True;
  end;


///  Registers component with Delphi's component palette at design time.
procedure Register;


implementation


uses
  {$IFNDEF RequiresRTLNameSpaces}
  SysUtils, Forms;
  {$ELSE}
  System.SysUtils, Vcl.Forms;
  {$ENDIF}


resourcestring
  // Fatal error message displayed if no suitable clipboard monitoring API can
  // be found. *** This should never happen ***
  sAPINotSupported = '*** UNEXPECTED ERROR in Clipboard Viewer Component.'#10#10
    + 'No clipboard viewer API is not supported by this operating system.'#10#10
    + 'Please report this error at:'#10
    + '  https://code.google.com/p/ddab-lib/issues/list'#10
    + 'stating your operating system version.';

const
  // WM_CLIPBOARDUPDATE is not defined in the Messages unit of all supported
  // versions of Delphi, so we defined it here for safety.
  {$EXTERNALSYM WM_CLIPBOARDUPDATE}
  WM_CLIPBOARDUPDATE  = $031D;


procedure Register;
begin
  RegisterComponents('DelphiDabbler', [TPJCBViewer]);
end;

{ TPJCBViewer }

procedure TPJCBViewer.ClipboardChanged;
begin
  try
    // Fire event iff assigned and enabled
    if Assigned(fOnClipboardChanged) and fEnabled then
      fOnClipboardChanged(Self);
  except
    Application.HandleException(Self);
  end;
end;

constructor TPJCBViewer.Create(AOwner: TComponent);
const
  cUserKernelLib = 'user32.dll';
begin
  inherited;
  // Load required API functions: 1st try to load modern clipboard listener API
  // functions. If that fails try to load old-style clipboard viewer API
  // functions. This should never fail, but we raise an exception if the
  // impossible happens!
  fAddClipboardFormatListener := GetProcAddress(
    GetModuleHandle(cUserKernelLib), 'AddClipboardFormatListener'
  );
  fRemoveClipboardFormatListener := GetProcAddress(
    GetModuleHandle(cUserKernelLib), 'RemoveClipboardFormatListener'
  );
  fUseNewAPI := Assigned(fAddClipboardFormatListener)
    and Assigned(fRemoveClipboardFormatListener);
  if not fUseNewAPI then
  begin
    fSetClipboardViewer := GetProcAddress(
      GetModuleHandle(cUserKernelLib), 'SetClipboardViewer'
    );
    fChangeClipboardChain := GetProcAddress(
      GetModuleHandle(cUserKernelLib), 'ChangeClipboardChain'
    );
    if not Assigned(fSetClipboardViewer)
      or not Assigned(fChangeClipboardChain) then
      raise Exception.Create(sAPINotSupported);
  end;
  // Create hidden clipboard listener window
  {$IFDEF AllocateHWndIsInFormsUnit}
  fHWnd := Forms.AllocateHWnd(WndMethod);
  {$ELSE}
  {$IFDEF RequiresRTLNameSpaces}
  fHWnd := System.Classes.AllocateHWnd(WndMethod);
  {$ELSE}
  fHWnd := Classes.AllocateHWnd(WndMethod);
  {$ENDIF}
  {$ENDIF}
  if fUseNewAPI then
  begin
    // Register window as clipboard listener
    if not fAddClipboardFormatListener(fHWnd) then
      {$IFDEF SupportsRaiseLastOSError}
      RaiseLastOSError;
      {$ELSE}
      RaiseLastWin32Error;
      {$ENDIF}
  end
  else
  begin
    // Register window as clipboard viewer, storing handle of next window in
    // chain
    fHWndNextViewer := fSetClipboardViewer(fHWnd);
  end;
  // Default property values
  fTriggerOnCreation := True;
  fEnabled := True;
end;

destructor TPJCBViewer.Destroy;
begin
  // Remove clipboard listener or viewer
  if fUseNewAPI then
    fRemoveClipboardFormatListener(fHWnd)
  else
    fChangeClipboardChain(fHWnd, fHWndNextViewer);
  // Destroy listener window
  {$IFDEF AllocateHWndIsInFormsUnit}
  Forms.DeallocateHWnd(fHWnd);
  {$ELSE}
  {$IFDEF RequiresRTLNameSpaces}
  System.Classes.DeallocateHWnd(fHWnd);
  {$ELSE}
  Classes.DeallocateHWnd(fHWnd);
  {$ENDIF}
  {$ENDIF}
  inherited;
end;

procedure TPJCBViewer.Loaded;
begin
  inherited Loaded;
  // Trigger OnClipboardChanged event if required
  if fTriggerOnCreation then
    ClipboardChanged;
end;

procedure TPJCBViewer.WndMethod(var Msg: TMessage);
var
  MsgHandled: Boolean; // flag showing whether message was handled
begin
  MsgHandled := False;
  // Process necessary messages
  case Msg.Msg of
    WM_CLIPBOARDUPDATE: // handled only when newer listener API is being used
    begin
      if fUseNewAPI then
      begin
        MsgHandled := True;
        // Clipboard has changed: trigger event
        ClipboardChanged;
      end;
    end;
    WM_DRAWCLIPBOARD: // handled only when old viewer API is being used
    begin
      if not fUseNewAPI then
      begin
        MsgHandled := True;
        // Clipboard has changed: trigger event
        ClipboardChanged;
        // Pass on message to any next window in viewer chain
        if fHWndNextViewer <> 0 then
          SendMessage(fHWndNextViewer, Msg.Msg, Msg.WParam, Msg.LParam);
      end;
    end;
    WM_CHANGECBCHAIN: // handled only when old viewer API is being used
    begin
      if not fUseNewAPI then
      begin
        MsgHandled := True;
        // NOTE: although API documentation says we should return 0 if this
        // message is handled, example code on MSDN doesn't do this, so we
        // don't either.
        // Windows is detaching a clipboard viewer
        if HWND(Msg.WParam) = fHWndNextViewer then
          // window being detached is next one: record new "next" window
          fHWndNextViewer := HWND(Msg.LParam)
        else if fHWndNextViewer <> 0 then
          // window being detached is not next: pass message along
          SendMessage(fHWndNextViewer, Msg.Msg, Msg.WParam, Msg.LParam);
      end;
    end;
  end;
  if not MsgHandled then
    // We've not handled this message: do default processing
    Msg.Result := DefWindowProc(fHWnd, Msg.Msg, Msg.WParam, Msg.LParam);
end;

end.

