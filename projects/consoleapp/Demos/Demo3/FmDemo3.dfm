object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 3'
  ClientHeight = 174
  ClientWidth = 385
  Color = clBtnFace
  Font.Charset = ANSI_CHARSET
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
      'nds. Switch back to this application and watch the progress bar.'
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
  object ProgressBar1: TProgressBar
    Left = 10
    Top = 138
    Width = 365
    Height = 21
    Max = 10
    TabOrder = 1
  end
end
