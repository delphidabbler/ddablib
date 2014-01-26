{
 * Project file for Slave application of the DelphiDabbler Environment Variables
 * Unit demo program #2, VCL version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

program SlaveApp;

{$UNDEF Supports_MainFormOnTaskbar}
{$UNDEF Supports_RTLNamespaces}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 ad later
    {$DEFINE Supports_RTLNamespaces}
  {$IFEND}
  {$IF CompilerVersion >= 18.5} // Delphi 2007 and later
    {$DEFINE Supports_MainFormOnTaskbar}
  {$IFEND}
{$ENDIF}

uses
  {$IFNDEF Supports_RTLNamespaces}
  Forms,
  {$ELSE}
  Vcl.Forms,
  {$ENDIF}
  FmSlave in 'FmSlave.pas' {SlaveForm},
  PJEnvVars in '..\..\..\PJEnvVars.pas';

{$R *.RES}

begin
  Application.Initialize;
  {$IFDEF Supports_MainFormOnTaskbar}
  Application.MainFormOnTaskbar := True;
  {$ENDIF}
  Application.CreateForm(TSlaveForm, SlaveForm);
  Application.Run;
end.

