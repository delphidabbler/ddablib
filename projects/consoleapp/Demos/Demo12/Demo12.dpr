program Demo12;

uses
  Forms,
  FmDemo12 in 'FmDemo12.pas' {Form1};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
