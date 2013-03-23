object Form1: TForm1
  Left = 257
  Top = 159
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 8'
  ClientHeight = 357
  ClientWidth = 582
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
    Width = 562
    Height = 60
    AutoSize = False
    Caption = 
      'Click the "Execute Echoer.exe" button to pass a file of text thr' +
      'ough Echoer.exe. The program'#39's standard output text is displayed' +
      ' in the left hand memo, while text the program writes to standar' +
      'd error is displayed in the right hand memo.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 10
    Top = 128
    Width = 57
    Height = 15
    Caption = 'Std Output'
  end
  object Label3: TLabel
    Left = 295
    Top = 128
    Width = 48
    Height = 15
    Caption = 'Std Error'
  end
  object Button1: TButton
    Left = 222
    Top = 89
    Width = 139
    Height = 30
    Caption = 'Execute Echoer.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 10
    Top = 148
    Width = 277
    Height = 198
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 295
    Top = 148
    Width = 277
    Height = 198
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
