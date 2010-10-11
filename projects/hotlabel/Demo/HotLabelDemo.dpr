{
 * HotLabelDemo.dpr
 *
 * Project file for demo program that demonstrates use of the Hot Label
 * Component.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2007-2010.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}


program HotLabelDemo;


uses
  Forms,
  FmHotLabelDemo in 'FmHotLabelDemo.pas' {DemoForm};


{$R *.RES}


begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.

