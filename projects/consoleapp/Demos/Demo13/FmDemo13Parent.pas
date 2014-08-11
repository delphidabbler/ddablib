{
 * Main form for the parent process used in the DelphiDabbler Console
 * Application Runner Classes demo program #13: Customising a console app's
 * environment block.
 *
 * $Rev$
 * $Date$
 *
 * Any copyright in this file is dedicated to the Public Domain.
 * http://creativecommons.org/publicdomain/zero/1.0/
}

unit FmDemo13Parent;


interface


uses
  StdCtrls, Controls, Classes, Forms;


type
  TParentForm = class(TForm)
    Label1: TLabel;
    Memo1: TMemo;
    CheckBox1: TCheckBox;
    Button1: TButton;
    procedure Button1Click(Sender: TObject);
  end;

var
  ParentForm: TParentForm;


implementation


uses
  PJConsoleApp, UEnvVars;

{$R *.dfm}


{ TParentForm }

procedure TParentForm.Button1Click(Sender: TObject);
var
  EnvBlockSize: Integer;
  EnvBlock: Pointer;
  App: TPJConsoleApp;
begin
  // We create an environment block that contains the environment variables
  // entered in Memo1. If CheckBox1 is checked we also include a copy of this
  // app's environment variables in the block. The pointer to the block is
  // stored in TPJConsoleApp's Environment property. This block must not be
  // freed until after the console app has terminated.

  // On versions of Delphi that have Unicode strings by default, the environment
  // block created by the CreateEnvBlock routine will be in Unicode encoded in
  // wide characters (2 byte) while on Delphis that have ANSI strings the
  // environment block will be created using ANSI characters (1 byte). Therefore
  // we use conditional compilation to set the UnicodeEnvironment property of
  // TPJConsoleApp to True on Unicode Delphis and False on AnsiString Delphis.
  
  EnvBlockSize := CreateEnvBlock(Memo1.Lines, CheckBox1.Checked, nil, 0);
  GetMem(EnvBlock, EnvBlockSize * SizeOf(Char));
  try
    CreateEnvBlock(Memo1.Lines, CheckBox1.Checked, EnvBlock, EnvBlockSize);
    App := TPJConsoleApp.Create;
    try
      App.CommandLine := 'Demo13Child.exe';
      App.Visible := True;
      App.Environment := EnvBlock;
      {$IFDEF UNICODE}
      App.UnicodeEnvironment := True;
      {$ELSE}
      App.UnicodeEnvironment := False;
      {$ENDIF}
      App.Execute;
    finally
      App.Free;
    end
  finally
    FreeMem(EnvBlock);
  end;
end;

end.

