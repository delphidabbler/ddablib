program CBViewDemo;

uses
  Forms,
  FmDemo in 'FmDemo.pas' {DemoForm};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TDemoForm, DemoForm);
  Application.Run;
end.
