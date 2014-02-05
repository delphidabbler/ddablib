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
 * Source code of the TPJHotLabel component, which is a label that when clicked
 * opens a URL in the default web browser or email client.
 *
 * Documented at http://www.delphidabbler.com/url/hotlabel-docs
 *
 * ***** END LICENSE BLOCK *****
}


unit PJHotLabel;


interface


// Switch off warnings where this is supported
{$UNDEF Supports_RTLNamespaces}
{$DEFINE Requires_Forms_Unit}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE Supports_RTLNamespaces}
  {$IFEND}
  {$IF CompilerVersion >= 20.0} // Delphi 2009 and later
    {$UNDEF Requires_Forms_Unit}
  {$IFEND}
  {$IF CompilerVersion >= 15.0} // Delphi 7 and later
    {$WARN UNSAFE_CODE OFF}
  {$IFEND}
{$ENDIF}


uses
  // Delphi
  {$IFNDEF Supports_RTLNamespaces}
  SysUtils,
  Classes,
  Messages,
  {$IFDEF Requires_Forms_Unit}
  Forms,
  {$ENDIF}
  Graphics,
  Controls,
  StdCtrls;
  {$ELSE}
  System.SysUtils,
  System.Classes,
  Winapi.Messages,
  Vcl.Graphics,
  Vcl.Controls,
  Vcl.StdCtrls;
  {$ENDIF}


const
  crHand = crHandPoint;   // for backwards compatibility


type

  {
  EPJURLError:
    Exception triggered by TPJHotLabel when URL problems encountered.
  }
  EPJURLError = class(Exception);


  {
  TPJHLHintStyle:
    The various hint styles used by TPJHotLabel.
  }
  TPJHLHintStyle = (
    hsNormal,   // display normal hints per Hint property
    hsURL,      // display URL property in hint and ignore Hint property
    hsCustom    // display a custom hint by handling OnCustomHint event
                // (hsCustom is same hsNormal if OnCustomHint not handled)
  );


  {
  TPJHLCustomHintEvent:
    Type of event triggered just before hint is displayed when HintStyle
    property is set to hsCustom. Handler can change the default.
      @param Sender [in] Reference to component that triggered event.
      @param HintStr [in/out] Set to value of component's Hint property when
        called. Handler sets to desired hint string.
  }
  TPJHLCustomHintEvent = procedure(Sender: TObject; var HintStr: string)
    of object;


  {
  TPJHotLabel:
    Label component that opens a URL in the default web browser or email client
    when clicked.
  }
  TPJHotLabel = class(TLabel)
  private
    fHighlighted: Boolean;
      {Records whether label is highlighted}
    fValidateURL: Boolean;
      {Value of ValidateURL property}
    fURL: string;
      {Value of URL property}
    fCaptionIsURL: Boolean;
      {Value of CaptionIsURL property}
    fFont: TFont;
      {Value of Font property}
    fHighlightFont: TFont;
      {Value of HighlightFont property}
    fVisitedFont: TFont;
      {Value of VisitedFont property}
    fHighlightURL: Boolean;
      {Value of HighlightURL property}
    fVisited: Boolean;
      {Value of Visited property}
    fTrackVisits: Boolean;
      {Value of TrackVists property}
    fHintStyle: TPJHLHintStyle;
      {Value of HintStyle property}
    fOnCustomHint: TPJHLCustomHintEvent;
      {Reference to any OnCustomHint event handler}
    procedure CMMouseEnter(var Msg: TMessage); message CM_MOUSEENTER;
      {Handles mouse enter event. Highlights label text if required.
        @param Msg [in/out] Not used.
      }
    procedure CMMouseLeave(var Msg: TMessage); message CM_MOUSELEAVE;
      {Handles mouse leave event. Un-highlight label text if it was highlighted.
        @param Msg [in/out] Not used.
      }
    procedure CMHintShow(var Msg: TMessage); message CM_HINTSHOW;
      {Message handler that intercepts hints before they are displayed and sets
      hint as required by the HintStyle property. Triggers OnCustomHint event
      when HintStyle = hsCustom.
        @param Msg [in/out] Message parameters. Hint info structure pointed to
          by LParam parameter may be changed.
      }
    procedure SetValidateURL(const Value: Boolean);
      {Write access method for ValidateURL property.
        @param Value [in] New property value. If EPJURLError raised, URL is set
          to default URL.
        @except EPJURLError raised if URL does not have a supported protocol and
          Value is true.
      }
    procedure SetURL(const Value: string);
      {Write access method for URL property. Sets Caption property to same value
      if CaptionIsURL property is true.
        @param Value [in] New property value.
        @except EPJURLError raised if URL is not valid and ValidateURL is true.
      }
    procedure SetCaptionIsURL(const Value: Boolean);
      {Write access method for CaptionIsURL property. When set true we set
      Caption property to same value as URL property.
        @param Value [in] New property value.
      }
    procedure SetHighlightFont(const Value: TFont);
      {Write access method for HighlightFont property.
        @param Value [in] New property value.
      }
    procedure SetVisitedFont(const Value: TFont);
      {Write access method for VisitedFont property.
        @param Value [in] New property value.
      }
    procedure SetVisited(const Value: Boolean);
      {Write access method for Visited property.
        @param Value new property value.
      }
    procedure UpdateDisplayFont;
      {Updates font used to display label depending on its state.
      NOTE: Uses inherited Font property for this purpose.
      }
    procedure FontChange(Sender: TObject);
      {Handles changes to Font property by updating display font if label is in
      its un-highlighted normal state.
        @param Sender [in] Ignored.
      }
    procedure HighlightFontChange(Sender: TObject);
      {Handles changes to HighlightFont property by updating display font if
      label is in its highlighted state.
        @param Sender [in] Ignored.
      }
    procedure VisitedFontChange(Sender: TObject);
      {Handles changes to VisitedFont property by updating display font if label
      is in its un-highlighted visited state.
        @param Sender [in] Ignored.
      }
  protected
    procedure SetDefaultURL; virtual;
      {Sets the URL to the default value. Override this method in descendant
      classes to change this default.
      }
    procedure CheckURL(const URL: string); virtual;
      {Checks that a URL has one of the supported protocols. Override this
      method to add additional protocols in a descendant component.
        @param URL [in] URL to be checked.
        @except EPJURLError raised if URL does not have a supported protocol.
      }
    procedure Click; override;
      {Starts default browser or e-mail client using URL property when label
      clicked, providing URL is not empty and label is enabled.
        @except EPJURLError raised if Windows can't access URL.
      }
    procedure Loaded; override;
      {Ensures Caption matches URL property if CaptionIsURL property is true
      when component is loaded from resources.
      }
    procedure SetCaption(const Value: TCaption); virtual;
      {"Overridden" write access method for Caption property. Also sets URL
      property to same value when CaptionIsURL property is true.
        @param Value [in] New property value.
        @except EPJURLError raised if CaptionIsURL and ValidateURL are true and
          Value is not a valid URL.
      }
    function GetCaption: TCaption; virtual;
      {"Overridden" read access method for Caption property.
        @return Value of inherited Caption property.
      }
    function GetFont: TFont; virtual;
      {"Overridden" read access method for Font property.
        @return Value of Font property.
      }
    procedure SetFont(const Value: TFont); virtual;
      {"Overridden" write access method for Font property.
        @param Value [in] New property value.
      }
  public
    constructor Create(AOwner: TComponent); override;
      {Class constructor. Initialises object and sets default property values.
        @param AOwner [in] Reference to owning component or nil if no owner.
      }
    destructor Destroy; override;
      {Class destructor. Tears down object.
      }
  published
    // New properties
    property CaptionIsURL: Boolean
      read fCaptionIsURL write SetCaptionIsURL default True;
      {Determines if the Caption displays the URL per the URL property. When
      true the Caption displays the URL and setting either property updates the
      other. When false the Caption and URL are idependent of each other}
    property VisitedFont: TFont
      read fVisitedFont write SetVisitedFont;
      {Font used by label when in its visited state and it is not highlighted.
      }
    property Visited: Boolean
      read fVisited write SetVisited default False;
      {Indicates whether the label is in its "visited" state. Write the value
      to change the state. When TrackVists is True clicking the label will set
      this property True, providing the click action completes successfully}
    property TrackVisits: Boolean
      read fTrackVisits write fTrackVisits default False;
      {Specifies whether the label should automatically track visits. When True
      clicking the label sets the Visited property to True, providing the click
      action was completed successfully}
    property HighlightFont: TFont
      read fHighlightFont write SetHighlightFont;
      {Font used by label when in its highlighted state: i.e. when the mouse
      cursor is over the component and HighlightURL is True}
    property HighlightURL: Boolean
      read fHighlightURL write fHighlightURL default False;
      {Determines whether the label is highlighted using HighlightFont when the
      mouse cursor is over it}
    property HintStyle: TPJHLHintStyle
      read fHintStyle write fHintStyle default hsNormal;
      {Determines how the component's hint is displayed. See TPJHLHintStyle for
      details of possible values and their effects}
    property URL: string
      read fURL write SetURL;
      {URL to be accessed when the label is clicked. This value is validated if
      ValidateURL is true. Setting this property also sets Caption when
      CaptionIsURL is true}
    property ValidateURL: Boolean
      read fValidateURL write SetValidateURL default True;
      {Causes URL property to be validated when true. Invalid URLs causes an
      exception to be raised. Setting this property true when URL is invalid
      raises an exception and restores the default URL}
    property OnCustomHint: TPJHLCustomHintEvent
      read fOnCustomHint write fOnCustomHint;
      {Event triggered just before the component's hint is displayed when the
      HintStyle property is hsCustom. Handlers can modified the displayed hint}
    // Overridden inherited properties
    property Caption: TCaption
      read GetCaption write SetCaption;
      {Label's caption. When CaptionIsURL is true, the property has the same
      value and behaviour as the URL property}
    property Cursor default crHandPoint;
      {Cursor property is now a hand pointer by default}
    property Font: TFont
      read GetFont write SetFont;
      {Font used by label when not in its visited or highlighted state}
    property ParentFont default False;
      {ParentFont property is set to False by default since the default caption
      font style is different to the parent font}
  end;


procedure Register;
  {Registers component.
  }


implementation


uses
  // Delphi
  {$IFNDEF Supports_RTLNamespaces}
  Windows,
  ShellAPI;
  {$ELSE}
  Winapi.Windows,
  Winapi.ShellAPI;
  {$ENDIF}


const
  // URL used as default caption property by SetDefaultURL method
  cDefaultURL = 'http://example.com/';


// Message strings
resourcestring
  sCantAccessURL = 'Can''t access URL "%s"';
  sBadProtocol = 'Protocol not recognised for URL "%s"';


procedure Register;
  {Registers component.
  }
begin
  RegisterComponents('DelphiDabbler', [TPJHotLabel]);
end;


{ TPJHotLabel }

procedure TPJHotLabel.CheckURL(const URL: string);
  {Checks that a URL has one of the supported protocols. Override this method to
  add additional protocols in a descendant component.
    @param URL [in] URL to be checked.
    @except EPJURLError raised if URL does not have a supported protocol.
  }
const
  // List of recognised URLs
  cValidURLS: array[1..5] of string
    = ('http://', 'https://', 'mailto', 'file:', 'ftp://');
var
  I: Integer; // loops thru all recognised URLs
begin
  // Allow '': we don't ever try to jump to empty URL!
  if URL = '' then Exit;
  // Check caption against array of valid protocols: exit if we recognise one
  for I := Low(cValidURLS) to High(cValidURLS) do
    if CompareText(cValidURLS[I],Copy(URL, 1, Length(cValidURLS[I]))) = 0 then
      Exit;
  // If we get here, we haven't recognised the protocol so raise exception
  raise EPJURLError.CreateFmt(sBadProtocol, [URL]);
end;

procedure TPJHotLabel.Click;
  {Starts default browser or e-mail client using URL property when label
  clicked, providing URL is not empty and label is enabled.
    @except EPJURLError raised if Windows can't access URL.
  }
begin
  if Enabled and (fURL <> '')then
  begin
    if ShellExecute(0, nil, PChar(fURL), nil, nil, SW_SHOW) <= 32 then
      raise EPJURLError.CreateFmt(sCantAccessURL, [fURL]);
    if fTrackVisits and not fVisited then
      SetVisited(True);
  end;
  // Make sure inherited processing happens
  inherited Click;
end;

procedure TPJHotLabel.CMHintShow(var Msg: TMessage);
  {Message handler that intercepts hints before they are displayed and sets hint
  as required by the HintStyle property. Triggers OnCustomHint event when
  HintStyle = hsCustom.
    @param Msg [in/out] Message parameters. Hint info structure pointed to by
      LParam parameter may be changed.
  }
var
  HintStr: string;  // the hint to be displayed when custom hints being used
begin
  // Displayed hint depends on HintStyle property
  case fHintStyle of
    hsNormal:
      // Hint property displays as normal
      {Do nothing};
    hsURL:
      // Display URL in hint
      PHintInfo(Msg.LParam)^.HintStr := fURL;
    hsCustom:
    begin
      // Custom hint. User sets hint text in event handler. If event not handled
      // then hint is as specified by Hint property
      HintStr := Hint;
      if Assigned(fOnCustomHint) then
        fOnCustomHint(Self, HintStr);
      PHintInfo(Msg.LParam)^.HintStr := HintStr;
    end;
  end;
  inherited;
end;

procedure TPJHotLabel.CMMouseEnter(var Msg: TMessage);
  {Handles mouse enter event. Highlights label text if required.
    @param Msg [in/out] Not used.
  }
begin
  inherited;
  if HighlightURL and not (csDesigning in ComponentState)
    and not fHighlighted then
  begin
    fHighlighted := True;
    UpdateDisplayFont;
  end;
end;

procedure TPJHotLabel.CMMouseLeave(var Msg: TMessage);
  {Handles mouse leave event. Un-highlight label text if it was highlighted.
    @param Msg [in/out] Not used.
  }
begin
  inherited;
  if HighlightURL and not (csDesigning in ComponentState) and fHighlighted then
  begin
    fHighlighted := False;
    UpdateDisplayFont;
  end;
end;

constructor TPJHotLabel.Create(AOwner: TComponent);
  {Class constructor. Initialises object and sets default property values.
    @param AOwner [in] Reference to owning component or nil if no owner.
  }
begin
  inherited Create(AOwner);

  // Create fonts required by Font, HighlightFont and VisitedFont properties
  fFont := TFont.Create;
  fFont.OnChange := FontChange;
  fHighlightFont := TFont.Create;
  fHighlightFont.OnChange := HighlightFontChange;
  fVisitedFont := TFont.Create;
  fVisitedFont.OnChange := VisitedFontChange;

  // Set default values
  // NOTE: various defaults have been chosen to make default behaviour of
  // component emulate earlier versions
  Cursor := crHandPoint;
  Font.Color := clNavy; // sets ParentFont to False
  Font.Style := [fsUnderline];
  HighlightFont := Font;
  HighlightFont.Color := clRed;
  VisitedFont := Font;
  VisitedFont.Color := clBlue;
  SetValidateURL(True);
  fCaptionIsURL := True;
  SetDefaultURL;  // sets URL and Caption properties to default URL
  fHighlightURL := False;
  fHintStyle := hsNormal;
  inherited Caption := fURL;
  fHighlighted := False;
  fVisited := False;
  UpdateDisplayFont;
end;

destructor TPJHotLabel.Destroy;
  {Class destructor. Tears down object.
  }
begin
  fHighlightFont.Free;
  fFont.Free;
  inherited;
end;

procedure TPJHotLabel.FontChange(Sender: TObject);
begin
  if not (csDesigning in ComponentState) then
  begin
    if not fVisited and not fHighlighted then
      UpdateDisplayFont;
  end
  else
    inherited Font.Assign(fFont);
end;

function TPJHotLabel.GetCaption: TCaption;
  {"Overridden" read access method for Caption property.
    @return Value of inherited Caption property.
  }
begin
  Result := inherited Caption;
end;

function TPJHotLabel.GetFont: TFont;
  {"Overridden" read access method for Font property.
    @return Value of Font property.
  }
begin
  Result := fFont;
end;

procedure TPJHotLabel.HighlightFontChange(Sender: TObject);
begin
  if fHighlighted and not (csDesigning in ComponentState) then
    UpdateDisplayFont;
end;

procedure TPJHotLabel.Loaded;
  {Ensures Caption matches URL property if CaptionIsURL property is true when
  component is loaded from resources.
  }
begin
  inherited;
  if fCaptionIsURL then
    SetCaption(fURL);
end;

procedure TPJHotLabel.SetCaption(const Value: TCaption);
  {"Overridden" write access method for Caption property. Also sets URL property
  to same value when CaptionIsURL property is true.
    @param Value [in] New property value.
    @except EPJURLError raised if CaptionIsURL and ValidateURL are true and
      Value is not a valid URL.
  }
begin
  if fCaptionIsURL then
  begin
    // Caption is a URL
    if fValidateURL and not (csLoading in ComponentState) then
      CheckURL(Value);  // raises exception on bad URL
    fURL := Value;
  end;
  // Now update actual caption
  inherited Caption := Value;
end;

procedure TPJHotLabel.SetCaptionIsURL(const Value: Boolean);
  {Write access method for CaptionIsURL property. When set true we set Caption
  property to same value as URL property.
    @param Value [in] New property value.
  }
begin
  fCaptionIsURL := Value;
  if Value then
    SetCaption(fURL);
end;

procedure TPJHotLabel.SetDefaultURL;
  {Sets the URL to the default value. Override this method in descendant classes
  to change this default.
  }
begin
  try
    // Try to set URL to default
    SetURL(cDefaultURL)
  except on EPJURLError do
    // Catch exception thrown if default URL not valid
    // we also set URL to '': aren't we being cautious!
    SetURL('');
  end;
end;

procedure TPJHotLabel.SetFont(const Value: TFont);
  {"Overridden" write access method for Font property.
    @param Value [in] New property value.
  }
begin
  fFont.Assign(Value);
end;

procedure TPJHotLabel.SetHighlightFont(const Value: TFont);
  {Write access method for HighlightFont property.
    @param Value [in] New property value.
  }
begin
  fHighlightFont.Assign(Value);
end;

procedure TPJHotLabel.SetURL(const Value: string);
  {Write access method for URL property. Sets Caption property to same value if
  CaptionIsURL property is true.
    @param Value [in] New property value.
    @except EPJURLError raised if URL is not valid and ValidateURL is true.
  }
begin
  // We only validate URL and set Caption if component isn't loading
  if not (csLoading in ComponentState) then
  begin
    if fValidateURL then
      CheckURL(Value);
    if fCaptionIsURL then
      Caption := Value;
  end;
  fURL := Value;
end;

procedure TPJHotLabel.SetValidateURL(const Value: Boolean);
  {Write access method for ValidateURL property.
    @param Value [in] New property value. If EPJURLError raised, URL is set to
      default URL.
    @except EPJURLError raised if URL does not have a supported protocol and
      Value is true.
  }
begin
  // Record new value
  fValidateURL := Value;
  if fValidateURL then
    try
      CheckURL(fURL);
    except
      on E: EPJURLError do
      begin
        // Not a valid URL - replace it with default and re-raise
        // exception so user gets to know about error
        SetDefaultURL;
        raise;
      end;
    end;
end;

procedure TPJHotLabel.SetVisited(const Value: Boolean);
begin
  if fVisited = Value then
    Exit;
  fVisited := Value;
  if not (csDesigning in ComponentState) then
    UpdateDisplayFont;
end;

procedure TPJHotLabel.SetVisitedFont(const Value: TFont);
begin
  fVisitedFont.Assign(Value);
end;

procedure TPJHotLabel.UpdateDisplayFont;
var
  DisplayFont: TFont;
begin
  if fHighlighted then
    DisplayFont := fHighlightFont
  else if fVisited then
    DisplayFont := fVisitedFont
  else
    DisplayFont := fFont;
  inherited Font.Assign(DisplayFont);
end;

procedure TPJHotLabel.VisitedFontChange(Sender: TObject);
begin
  if fVisited and not fHighlighted and not (csDesigning in ComponentState) then
    UpdateDisplayFont;
end;

end.

