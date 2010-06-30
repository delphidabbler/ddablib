object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 2'
  ClientHeight = 109
  ClientWidth = 313
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
    Height = 49
    AutoSize = False
    Caption = 
      'Click the "Execute Timed.exe" button to run Timed.exe for 5 seco' +
      'nds. Try switching back to the main GUI while the console app is' +
      ' running - you should be able to do this.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 96
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Execute Timed.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
end
