{ 
 * FrameDemo.dpr
 *
 * Project file for demo program that demonstrates use of Drop Files Components
 * with frames.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}


program FrameDemo;

{$UNDEF DELPHIXE2ANDUP}
{$IFDEF CONDITIONALEXPRESSIONS}
  {$IF CompilerVersion >= 24.0} // Delphi XE3 and later
    {$LEGACYIFEND ON}  // NOTE: this must come before all $IFEND directives
  {$IFEND}
  {$IF CompilerVersion >= 23.0} // Delphi XE2 and later
    {$DEFINE DELPHIXE2ANDUP}
  {$IFEND}
{$ENDIF}

uses
  {$IFNDEF DELPHIXE2ANDUP}
  Forms,
  {$ELSE}
  Vcl.Forms,
  {$ENDIF}
  FmFrameDemo in 'FmFrameDemo.pas' {FmMain},
  FrDemo in 'FrDemo.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DelphiDabbler Frame Drop Files Demo';
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
end.
