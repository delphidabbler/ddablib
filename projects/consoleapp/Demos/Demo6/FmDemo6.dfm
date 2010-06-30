object Form1: TForm1
  Left = 257
  Top = 159
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 6'
  ClientHeight = 280
  ClientWidth = 473
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
    Width = 457
    Height = 82
    AutoSize = False
    Caption = 
      'Enter some text in the left hand memo then click the "Execute Ec' +
      'hoer.exe" button.'#13#10'This does the following:'#13#10'1) Saves the text f' +
      'rom the left hand memo to a file named "Demo6-in.txt"'#13#10'2) Execut' +
      'es Echoer with parameter ">>> ", redirecting "Demo6-in.txt" to s' +
      'tandard input and standard output to "Demo6-out.txt".'#13#10'3) Loads ' +
      '"Demo6-out.txt" into the right hand memo control.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 180
    Top = 96
    Width = 113
    Height = 25
    Caption = 'Execute Echoer.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 8
    Top = 128
    Width = 225
    Height = 137
    Lines.Strings = (
      
        'Enter some text in the left hand memo then click the "Execute Ec' +
        'hoer.exe" button.'
      'This does the following:'
      
        '1) Saves the text from the left hand memo to a file named "Demo6' +
        '-in.txt"'
      
        '2) Executes Echoer with parameter ">>> ",redirecting "Demo6-in.t' +
        'xt" to standard input and standard output to "Demo6-out.txt".'
      '3) Loads "Demo6-out.txt" into the right hand memo control.')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 240
    Top = 128
    Width = 225
    Height = 137
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
