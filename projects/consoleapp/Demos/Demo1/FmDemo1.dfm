object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 1'
  ClientHeight = 145
  ClientWidth = 386
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 10
    Top = 10
    Width = 365
    Height = 82
    AutoSize = False
    Caption = 
      'First click ExecAndWait to run Timed.exe using the original stan' +
      'dalone function. Next click ExecAndWait2 to run the version base' +
      'd on TPJConsoleApp. In both cases try switching back to the main' +
      ' GUI while the console app is running - you shouldn'#39't be able to' +
      '.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 10
    Top = 108
    Width = 119
    Height = 31
    Caption = 'ExecAndWait'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 256
    Top = 108
    Width = 119
    Height = 31
    Caption = 'ExecAndWait2'
    TabOrder = 1
    OnClick = Button2Click
  end
end
