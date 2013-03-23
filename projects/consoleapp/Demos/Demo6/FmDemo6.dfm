object Form1: TForm1
  Left = 257
  Top = 159
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 6'
  ClientHeight = 345
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
    Height = 101
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
    Left = 222
    Top = 118
    Width = 139
    Height = 31
    Caption = 'Execute Echoer.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 10
    Top = 158
    Width = 277
    Height = 168
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
    Left = 295
    Top = 158
    Width = 277
    Height = 168
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
