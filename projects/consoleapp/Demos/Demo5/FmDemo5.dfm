object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 5'
  ClientHeight = 156
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
    Height = 81
    AutoSize = False
    Caption = 
      'Check the "Kill terminated app" check box if you want terminated' +
      ' applications to be forcibly terminated.'#13#10'Now click the "Execute' +
      ' Timed.exe" button to run Timed.exe for 5 seconds.'#13#10'Click the "T' +
      'erminate" button to terminate Timed.exe prematurely.'
    WordWrap = True
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 96
    Width = 121
    Height = 17
    Caption = 'Kill terminated app'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 8
    Top = 120
    Width = 105
    Height = 25
    Caption = 'Execute Timed.exe'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 200
    Top = 120
    Width = 105
    Height = 25
    Caption = 'Terminate'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
end
