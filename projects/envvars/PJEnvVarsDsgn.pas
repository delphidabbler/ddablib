{
 * This Source Code Form is subject to the terms of the Mozilla Public License,
 * v. 2.0. If a copy of the MPL was not distributed with this file, You can
 * obtain one at http://mozilla.org/MPL/2.0/
 *
 * Copyright (C) 2014, Peter Johnson (www.delphidabbler.com).
 *
 * $Rev$
 * $Date$
 *
 * Component registration code for TPJEnvVars component from DelphiDabbler
 * Environment Variable Unit.
 *
 * NOTE: TPJEnvVars is deprecated. Add this unit to a design package only if
 * you want to install the component.
 *
 * ***** END LICENSE BLOCK *****
}


unit PJEnvVarsDsgn;


{$UNDEF Supports_RTLNamespaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 ad later
    {$DEFINE Supports_RTLNamespaces}
  {$IFEND}
{$ENDIF}


interface


// Registers the component with the Delphi IDE.
procedure Register;


implementation


uses
  {$IFNDEF Supports_RTLNamespaces}
  Classes,
  {$ELSE}
  System.Classes,
  {$ENDIF}
  PJEnvVars;

procedure Register;
begin
  RegisterComponents('DelphiDabbler', [TPJEnvVars]);
end;

end.
