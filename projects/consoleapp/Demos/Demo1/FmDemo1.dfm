object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 1'
  ClientHeight = 118
  ClientWidth = 314
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 297
    Height = 71
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
    Left = 8
    Top = 88
    Width = 97
    Height = 25
    Caption = 'ExecAndWait'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 208
    Top = 88
    Width = 97
    Height = 25
    Caption = 'ExecAndWait2'
    TabOrder = 1
    OnClick = Button2Click
  end
end
