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


interface


// Determine which unit to use for AllocateHWnd and DeallocateHWnd
// Delphi 2 to 5 use the Forms unit while Delphi 6 and later use Classes
{$DEFINE ALLOCATEHWNDINFORMS}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 14.0} // Delphi 6 and later
    {$UNDEF ALLOCATEHWNDINFORMS}
  {$IFEND}
{$ENDIF}


uses
  // Delphi
  Windows, Messages, Classes;


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
  Forms;


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
begin
  inherited;
  // Create hidden clipboard viewer window
  {$IFDEF ALLOCATEHWNDINFORMS}
    fHWnd := Forms.AllocateHWnd(WndMethod);
  {$ELSE}
    fHWnd := Classes.AllocateHWnd(WndMethod);
  {$ENDIF}
  // Register window as clipboard viewer, storing handle of next window in chain
  fHWndNextViewer := SetClipboardViewer(fHWnd);

  // Set default property values
  fTriggerOnCreation := True;
  fEnabled := True;
end;

destructor TPJCBViewer.Destroy;
  {Object destructor. Unregisters and destroys clipboard viewer window.
  }
begin
  // Remove clipboard viewer window from chain
  ChangeClipboardChain(fHWnd, fHWndNextViewer);
  // Destroy window
  {$IFDEF ALLOCATEHWNDINFORMS}
    Forms.DeallocateHWnd(fHWnd);
  {$ELSE}
    Classes.DeallocateHWnd(fHWnd);
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
begin
  // Process necessary messages
  with Msg do
  begin
    case Msg of
      WM_DRAWCLIPBOARD:
      begin
        // Clipboard has changed: trigger event
        ClipboardChanged;
        // Pass on message to any next window in viewer chain
        if fHWndNextViewer <> 0 then
          SendMessage(fHWndNextViewer, Msg, WParam, LParam);
      end;
      WM_CHANGECBCHAIN:
      begin
        // NOTE: although API documentation says we should return 0 if this
        // message is handled, example code on MSDN doesn't do this, so we don't
        // either.
        // Windows is detaching a clipboard viewer
        if HWND(WParam) = fHWndNextViewer then
          // window being detached is next one: record new "next" window
          fHWndNextViewer := HWND(LParam)
        else if fHWndNextViewer <> 0 then
          // window being detached is not next: pass message along
          SendMessage(fHWndNextViewer, Msg, WParam, LParam);
      end;
      else
        // We're not handling this message: do default processing
        Result := DefWindowProc(fHWnd, Msg, WParam, LParam);
    end;
  end;
end;

end.

