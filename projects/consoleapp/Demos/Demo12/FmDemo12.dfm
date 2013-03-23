object Form1: TForm1
  Left = 672
  Top = 352
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 12'
  ClientHeight = 363
  ClientWidth = 518
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
    Left = 8
    Top = 8
    Width = 247
    Height = 15
    Caption = 'Echoer standard output (error output ignored)'
  end
  object Memo1: TMemo
    Left = 8
    Top = 27
    Width = 497
    Height = 281
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Button1: TButton
    Left = 182
    Top = 320
    Width = 153
    Height = 25
    Caption = 'Run "Echoer -u"'
    TabOrder = 1
    OnClick = Button1Click
  end
end
