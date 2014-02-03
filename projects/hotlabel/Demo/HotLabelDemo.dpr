{
 * HotLabelDemo.dpr
 *
 * Project file for demo program that demonstrates use of the Hot Label
 * Component.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
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

