object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 4'
  ClientHeight = 353
  ClientWidth = 385
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
    Width = 369
    Height = 105
    AutoSize = False
    Caption = 
      'Enter a maximum execution time (in milliseconds) in the edit box' +
      '.'#13#10'Check the "Kill timed out app" check box if you want any time' +
      'd out application to be forcibly terminated.'#13#10'Now click the "Exe' +
      'cute Timed.exe" button to run Timed.exe for 5 seconds. If time e' +
      'xpires before Timed.exe has completed a message will be displaye' +
      'd in the memo control.'#13#10'You will see a read out of the elapsed t' +
      'ime and remaining time to live for the application in the memo c' +
      'ontrol.'
    WordWrap = True
  end
  object Label2: TLabel
    Left = 8
    Top = 128
    Width = 154
    Height = 13
    Caption = 'Enter max execution time (in ms):'
  end
  object Button1: TButton
    Left = 137
    Top = 152
    Width = 113
    Height = 25
    Caption = 'Execute Timed.exe'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 184
    Width = 369
    Height = 153
    TabStop = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 168
    Top = 128
    Width = 57
    Height = 21
    TabOrder = 0
    Text = '3000'
  end
  object CheckBox1: TCheckBox
    Left = 248
    Top = 128
    Width = 129
    Height = 17
    Caption = 'Kill timed out app'
    TabOrder = 1
  end
end
