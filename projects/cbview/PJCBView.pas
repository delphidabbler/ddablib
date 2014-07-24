{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 1999-2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Clipboard Viewer Component source code. Implements a component that monitors
 * the Windows clipboard and triggers an event whenever the content of the
 * clipboard changes.
}


unit PJCBView;


{$DEFINE ALLOCATEHWNDINFORMS}
{$UNDEF RTLNAMESPACES}
{$UNDEF HASRAISELASTOSERROR}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE RTLNAMESPACES}
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$UNDEF ALLOCATEHWNDINFORMS}
    {$DEFINE HASRAISELASTOSERROR}
  {$IFEND}
{$ENDIF}


interface


uses
  // Delphi
  {$IFNDEF RTLNAMESPACES}
  Windows,
  Messages,
  Classes;
  {$ELSE}
  Winapi.Windows,
  Winapi.Messages,
  System.Classes;
  {$ENDIF}


type

  {
  TPJCBViewer:
    Component that monitors the Windows clipboard and triggers an event whenever
    the clipboard contents change.
  }
  TPJCBViewer = class(TComponent)
  private
    fOnClipboardChanged: TNotifyEvent;  // OnClipboardChanged event handler
    fTriggerOnCreation: Boolean;        // Value of TriggerOnCreation property
    fEnabled: Boolean;                  // Value of Enabled property
    fHWnd: HWND;                        // Handle of clipboard viewer window
    fHWndNextViewer: HWND;              // Next clipboard viewer handle in chain
    // New style clipboard API function pointers: nil if not supported by OS
    fAddClipboardFormatListener: function(hwnd: HWND): BOOL; stdcall;
    fRemoveClipboardFormatListener: function(hwnd: HWND): BOOL; stdcall;
    // Old style clipboard API function pointers: nil if new style available
    fSetClipboardViewer: function (hWndNewViewer: HWND): HWND; stdcall;
    fChangeClipboardChain: function(hWndRemove, hWndNewNext: HWND): BOOL;
      stdcall;
    // Flag indicating if new style API is available
    fUseNewAPI: Boolean;
  protected
    procedure ClipboardChanged; dynamic;
      {Triggers OnClipboardChanged event if any handler is assigned and
      component is enabled.
      }
    procedure Loaded; override;
      {Fires OnClipboardChanged event if required on component creation.
      }
    procedure WndMethod(var Msg: TMessage); virtual;
      {Window procedure for clipboard viewer window.
        @param Msg [in/out] Message to be handled. Result field my be altered.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Object constructor: Creates and registers clipboard viewer window and
      sets default property values.
        @param AOwner [in] Reference to owning component.
      }
    destructor Destroy; override;
      {Object destructor. Unregisters and destroys clipboard viewer window.
      }
  published
    property OnClipboardChanged: TNotifyEvent
      read fOnClipboardChanged write fOnClipboardChanged;
      {Event triggered when clipboard contents change}
    property TriggerOnCreation: Boolean
      read fTriggerOnCreation write fTriggerOnCreation default True;
      {When true causes OnClipboardChanged event to be triggered as soon as
      component is created, otherwise OnClipboardChanged event only triggers
      when clipboard actually changes. NOTE: this property can only be set at
      design time. It has no effect if set at run time}
    property Enabled: Boolean read fEnabled write fEnabled default True;
      {When true component triggers events when clipboard changes, when false
      these events are not triggered}
  end;


procedure Register;
  {Registers component with Delphi's component palette.
  }


implementation


uses
  // Delphi
  {$IFNDEF RTLNAMESPACES}
  SysUtils, Forms;
  {$ELSE}
  System.SysUtils, Vcl.Forms;
  {$ENDIF}


resourcestring
  sAPINotSupported = '*** UNEXPECTED ERROR in Clipboard Viewer Component.'#10#10
    + 'No clipboard viewer API is not supported by this operating system.'#10#10
    + 'Please report this error at:'#10
    + '  https://code.google.com/p/ddab-lib/issues/list'#10
    + 'stating your operating system version.';

procedure Register;
  {Registers component with Delphi's component palette.
  }
begin
  RegisterComponents('DelphiDabbler', [TPJCBViewer]);
end;

{ TPJCBViewer }

procedure TPJCBViewer.ClipboardChanged;
  {Triggers OnClipboardChanged event if any handler is assigned and component is
  enabled.
  }
begin
  try
    // Fire event if assigned and enabled
    if Assigned(fOnClipboardChanged) and fEnabled then
      fOnClipboardChanged(Self);
  except
    // Ensure any un-caught exception is handled by application
    Application.HandleException(Self);
  end;
end;

constructor TPJCBViewer.Create(AOwner: TComponent);
  {Object constructor: Creates and registers clipboard viewer window and sets
  default property values.
    @param AOwner [in] Reference to owning component.
  }
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
  {$IFDEF ALLOCATEHWNDINFORMS}
  fHWnd := Forms.AllocateHWnd(WndMethod);
  {$ELSE}
  {$IFDEF RTLNAMESPACES}
  fHWnd := System.Classes.AllocateHWnd(WndMethod);
  {$ELSE}
  fHWnd := Classes.AllocateHWnd(WndMethod);
  {$ENDIF}
  {$ENDIF}
  if fUseNewAPI then
  begin
    // Register window as clipboard listener
    if not fAddClipboardFormatListener(fHWnd) then
      {$IFDEF HASRAISELASTOSERROR}
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
  // Set default property values
  fTriggerOnCreation := True;
  fEnabled := True;
end;

destructor TPJCBViewer.Destroy;
  {Object destructor. Unregisters and destroys clipboard viewer window.
  }
begin
  // Removed clipboard listener or viewer
  if fUseNewAPI then
    fRemoveClipboardFormatListener(fHWnd)
  else
    fChangeClipboardChain(fHWnd, fHWndNextViewer);
  // Destroy listener window
  {$IFDEF ALLOCATEHWNDINFORMS}
  Forms.DeallocateHWnd(fHWnd);
  {$ELSE}
  {$IFDEF RTLNAMESPACES}
  System.Classes.DeallocateHWnd(fHWnd);
  {$ELSE}
  Classes.DeallocateHWnd(fHWnd);
  {$ENDIF}
  {$ENDIF}
  inherited;
end;

procedure TPJCBViewer.Loaded;
  {Fires OnClipboardChanged event if required on component creation.
  }
begin
  inherited Loaded;
  // Trigger OnClipboardChanged event if required
  if fTriggerOnCreation then
    ClipboardChanged;
end;

procedure TPJCBViewer.WndMethod(var Msg: TMessage);
  {Window procedure for clipboard viewer window.
    @param Msg [in/out] Message to be handled. Result field my be altered.
  }
var
  MsgHandled: Boolean; // flag showing whether message was handled
begin
  MsgHandled := False;
  // Process necessary messages
  // TODO: eliminate with statement
  with Msg do
  begin
    case Msg of
      WM_CLIPBOARDUPDATE:
      begin
        if fUseNewAPI then
        begin
          MsgHandled := True;
          // Clipboard has changed: trigger event
          ClipboardChanged;
        end;
      end;
      WM_DRAWCLIPBOARD:
      begin
        if not fUseNewAPI then
        begin
          MsgHandled := True;
          // Clipboard has changed: trigger event
          ClipboardChanged;
          // Pass on message to any next window in viewer chain
          if fHWndNextViewer <> 0 then
            SendMessage(fHWndNextViewer, Msg, WParam, LParam);
        end;
      end;
      WM_CHANGECBCHAIN:
      begin
        if not fUseNewAPI then
        begin
          MsgHandled := True;
          // NOTE: although API documentation says we should return 0 if this
          // message is handled, example code on MSDN doesn't do this, so we
          // don't either.
          // Windows is detaching a clipboard viewer
          if HWND(WParam) = fHWndNextViewer then
            // window being detached is next one: record new "next" window
            fHWndNextViewer := HWND(LParam)
          else if fHWndNextViewer <> 0 then
            // window being detached is not next: pass message along
            SendMessage(fHWndNextViewer, Msg, WParam, LParam);
        end;
      end;
    end;
    if not MsgHandled then
      // We're not handling this message: do default processing
      Result := DefWindowProc(fHWnd, Msg, WParam, LParam);
  end;
end;

end.

