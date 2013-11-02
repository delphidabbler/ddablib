{ 
 * DropFilesDemo.dpr
 *
 * Project file for demo program that demonstrates use of Drop Files Components
 * and filters.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
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
