object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 5'
  ClientHeight = 192
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
    Height = 100
    AutoSize = False
    Caption = 
      'Check the "Kill terminated app" check box if you want terminated' +
      ' applications to be forcibly terminated.'#13#10'Now click the "Execute' +
      ' Timed.exe" button to run Timed.exe for 5 seconds.'#13#10'Click the "T' +
      'erminate" button to terminate Timed.exe prematurely.'
    WordWrap = True
  end
  object CheckBox1: TCheckBox
    Left = 10
    Top = 118
    Width = 149
    Height = 21
    Caption = 'Kill terminated app'
    TabOrder = 0
  end
  object Button1: TButton
    Left = 10
    Top = 148
    Width = 129
    Height = 30
    Caption = 'Execute Timed.exe'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 246
    Top = 148
    Width = 129
    Height = 30
    Caption = 'Terminate'
    Enabled = False
    TabOrder = 2
    OnClick = Button2Click
  end
end
