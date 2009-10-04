program TestStreams;

uses
  Forms,
  TestFrameWork,
  GUITestRunner,
  TestPJIStreams in 'TestPJIStreams.pas',
  PJIStreams in '..\PJIStreams.pas',
  TestPJStreamWrapper in 'TestPJStreamWrapper.pas',
  PJStreamWrapper in '..\PJStreamWrapper.pas';

{$R *.RES}

begin
  Application.Initialize;
  GUITestRunner.RunRegisteredTests;
end.
