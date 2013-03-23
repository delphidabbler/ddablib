object Form1: TForm1
  Left = 257
  Top = 159
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 9'
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
    Height = 119
    AutoSize = False
    Caption = 
      'Enter some text in the left hand memo then click the "Execute Ec' +
      'hoer.exe" button.'#13#10'This does the following:'#13#10'1) Writes left hand' +
      ' memo text to input pipe'#13#10'2) Executes Echoer with parameter "-->' +
      ' ", redirecting input pipe to standard input and standard output' +
      ' to output pipe.'#13#10'3) Copies ouput pipe data to a stream.'#13#10'4) Loa' +
      'ds output data stream into the right hand memo control.'
    WordWrap = True
  end
  object Button1: TButton
    Left = 222
    Top = 138
    Width = 139
    Height = 31
    Caption = 'Execute Echoer.exe'
    TabOrder = 0
    OnClick = Button1Click
  end
  object Memo1: TMemo
    Left = 10
    Top = 177
    Width = 277
    Height = 169
    Lines.Strings = (
      
        'Enter some text in the left hand memo then click the "Execute Ec' +
        'hoer.exe" button.'
      'This does the following:'
      '1) Write left hand memo text to input pipe'
      
        '2) Executes Echoer with parameter "--> ", redirecting input pipe' +
        ' to standard input and standard output to output pipe.'
      '3) Copies ouput pipe data to a stream.'
      '4) Loads output data stream into the right hand memo control.')
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object Memo2: TMemo
    Left = 295
    Top = 177
    Width = 277
    Height = 169
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 2
    WordWrap = False
  end
end
