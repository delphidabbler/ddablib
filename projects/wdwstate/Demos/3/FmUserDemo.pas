{
 * FmUserDemo.pas
 *
 * Main form for Version Information Component UserDemo demo program.
 *
 * $Rev$
 * $Date$
 *
 * This file is copyright (C) P D Johnson (www.delphidabbler.com), 2006-2009.
 * It may be used without restriction. This code distributed on an "AS IS"
 * basis, WITHOUT WARRANTY OF ANY KIND, either express or implied.
}

unit FmUserDemo;

interface

uses
  Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms,
  Dialogs, PJWdwState, StdCtrls, ExtCtrls;

type
  TForm1 = class(TForm)
    Memo1: TMemo;
    PJUserWdwState1: TPJUserWdwState;
    procedure PJUserWdwState1ReadData(Sender: TObject;
      var Data: TPJWdwStateData);
    procedure PJUserWdwState1SaveData(Sender: TObject;
      const Data: TPJWdwStateData);
  private
    function DataFile: string;
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

function TForm1.DataFile: string;
begin
  // Returns name of data file in same directly as application
  Result := ExtractFilePath(ParamStr(0)) + 'wdwstate.dat';
end;

procedure TForm1.PJUserWdwState1ReadData(Sender: TObject;
  var Data: TPJWdwStateData);
var
  FS: TFileStream;
begin
  // We first check there is a data file: exception in TFileStream.Create if not
  // Then we read binary data from it directly into Data
  if FileExists(DataFile) then
  begin
    FS := TFileStream.Create(DataFile, fmOpenRead or fmShareDenyNone);
    try
      FS.ReadBuffer(Data, SizeOf(Data));
    finally
      FS.Free;
    end;
  end;
end;

procedure TForm1.PJUserWdwState1SaveData(Sender: TObject;
  const Data: TPJWdwStateData);
var
  FS: TFileStream;
begin
  // We simply save Data param as binary data into a file stream
  FS := TFileStream.Create(DataFile, fmCreate);
  try
    FS.WriteBuffer(Data, SizeOf(Data));
  finally
    FS.Free;
  end;
end;

end.

