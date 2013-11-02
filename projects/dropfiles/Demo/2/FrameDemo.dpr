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

uses
  Forms,
  FmFrameDemo in 'FmFrameDemo.pas' {FmMain},
  FrDemo in 'FrDemo.pas' {Frame1: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.Title := 'DelphiDabbler Frame Drop Files Demo';
  Application.CreateForm(TFmMain, FmMain);
  Application.Run;
end.
