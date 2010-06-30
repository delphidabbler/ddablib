object Form1: TForm1
  Left = 257
  Top = 159
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 8'
  ClientHeight = 290
  ClientWidth = 473
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 457
    Height = 49
    AutoSize = False
    Caption = 
      'Click the "Execute Echoer.exe" button to pass a file of text thr' +
      'ough Echoer.exe. The program'#39's standard output text is displayed' +
      ' in the left hand memo, while text the program writes to standar' +
      'd error is displayed in the right hand memo.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 104
    Width = 51
    Height = 13
    Caption = 'Std Output'
  end
  object Label3: TLabel
    Left = 240
    Top = 104
    Width = 41
    Height = 13
    Caption = 'Std Error'
  end
  object Button1: TButton
    Left = 180
    Top = 72
    Width = 113
    Height = 25
    Caption = 'Execute Echoer.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 120
    Width = 225
    Height = 161
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 240
    Top = 120
    Width = 225
    Height = 161
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
