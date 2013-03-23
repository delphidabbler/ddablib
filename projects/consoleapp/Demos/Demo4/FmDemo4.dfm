object Form1: TForm1
  Left = 211
  Top = 114
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 4'
  ClientHeight = 434
  ClientWidth = 474
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
    Width = 454
    Height = 129
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
    Left = 10
    Top = 158
    Width = 182
    Height = 15
    Caption = 'Enter max execution time (in ms):'
  end
  object Button1: TButton
    Left = 169
    Top = 187
    Width = 139
    Height = 31
    Caption = 'Execute Timed.exe'
    TabOrder = 2
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 10
    Top = 226
    Width = 454
    Height = 189
    TabStop = False
    ScrollBars = ssVertical
    TabOrder = 3
  end
  object Edit1: TEdit
    Left = 207
    Top = 158
    Width = 70
    Height = 23
    TabOrder = 0
    Text = '3000'
  end
  object CheckBox1: TCheckBox
    Left = 305
    Top = 158
    Width = 159
    Height = 20
    Caption = 'Kill timed out app'
    TabOrder = 1
  end
end
