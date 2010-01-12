{ 
 * FrameDemo.dpr
 *
 * Project file for demo program that demonstrates use of Drop Files Components
 * with frames.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2006-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
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
