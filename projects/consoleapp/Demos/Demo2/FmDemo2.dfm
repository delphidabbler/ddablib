object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 2'
  ClientHeight = 134
  ClientWidth = 385
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
    Height = 60
    AutoSize = False
    Caption = 
      'Click the "Execute Timed.exe" button to run Timed.exe for 5 seco' +
      'nds. Try switching back to the main GUI while the console app is' +
      ' running - you should be able to do this.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 118
    Top = 89
    Width = 139
    Height = 30
    Caption = 'Execute Timed.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
end
