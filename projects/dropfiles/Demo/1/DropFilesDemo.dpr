{ 
 * DropFilesDemo.dpr
 *
 * Project file for demo program that demonstrates use of Drop Files Components
 * and filters.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2004-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}


program DropFilesDemo;

uses
  Forms,
  FmDropFilesDemo in 'FmDropFilesDemo.pas' {DropFilesDemoForm};

{$R *.RES}

begin
  Application.Initialize;
  Application.Title := 'DelphiDabbler Drop Files Components Demo';
  Application.CreateForm(TDropFilesDemoForm, DropFilesDemoForm);
  Application.Run;
end.
