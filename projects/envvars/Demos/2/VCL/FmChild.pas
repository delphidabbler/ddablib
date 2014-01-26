{
 * Main form for the Child application of the DelphiDabbler Environment
 * Variables Unit demo program #2, VCL version.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmChild;

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

uses
  {$IFNDEF Supports_RTLNamespaces}
  Classes,
  Controls,
  StdCtrls,
  Forms;
  {$ELSE}
  System.Classes,
  Vcl.Controls,
  Vcl.StdCtrls,
  Vcl.Forms;
  {$ENDIF}

type
  TChildForm = class(TForm)
    edEnvVars: TMemo;
    procedure FormCreate(Sender: TObject);
  end;

var
  ChildForm: TChildForm;

implementation

uses
  PJEnvVars;

{$R *.DFM}

procedure TChildForm.FormCreate(Sender: TObject);
begin
  TPJEnvironmentVars.GetAll(edEnvVars.Lines);
end;

end.

