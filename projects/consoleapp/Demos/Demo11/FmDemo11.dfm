object Form1: TForm1
  Left = 300
  Top = 122
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 11'
  ClientHeight = 301
  ClientWidth = 350
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object lblTitle: TLabel
    Left = 8
    Top = 8
    Width = 111
    Height = 13
    Caption = 'Console Windows &Title:'
  end
  object btnRun: TButton
    Left = 8
    Top = 264
    Width = 329
    Height = 25
    Caption = 'Run Console App'
    TabOrder = 3
    OnClick = btnRunClick
  end
  object gbConsoleColours: TGroupBox
    Left = 8
    Top = 56
    Width = 329
    Height = 113
    Caption = 'Console colours'
    TabOrder = 1
    object lblForeground: TLabel
      Left = 8
      Top = 20
      Width = 57
      Height = 13
      Caption = '&Foreground:'
      FocusControl = cbForeground
    end
    object lblBackground: TLabel
      Left = 176
      Top = 20
      Width = 61
      Height = 13
      Caption = '&Background:'
      FocusControl = cbBackground
    end
    object cbForeground: TColorBox
      Left = 8
      Top = 35
      Width = 145
      Height = 22
      Selected = clWhite
      Style = [cbStandardColors, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object cbBackground: TColorBox
      Left = 176
      Top = 35
      Width = 145
      Height = 22
      Style = [cbStandardColors, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 1
    end
    object btnDefaultColours: TButton
      Left = 8
      Top = 72
      Width = 129
      Height = 25
      Caption = 'Use Defaults'
      TabOrder = 2
      OnClick = btnDefaultColoursClick
    end
  end
  object edTitle: TEdit
    Left = 8
    Top = 24
    Width = 329
    Height = 21
    TabOrder = 0
  end
  object gbScrBufferSize: TGroupBox
    Left = 8
    Top = 176
    Width = 329
    Height = 81
    Caption = 'Console Screen Buffer Size'
    TabOrder = 2
    object lblScrBufX: TLabel
      Left = 8
      Top = 48
      Width = 10
      Height = 13
      Caption = '&X:'
      Enabled = False
      FocusControl = edScrBufX
    end
    object lblScrBufY: TLabel
      Left = 80
      Top = 48
      Width = 10
      Height = 13
      Caption = '&Y:'
      Enabled = False
      FocusControl = edScrBufY
    end
    object cbDefScrBufferSize: TCheckBox
      Left = 8
      Top = 16
      Width = 241
      Height = 17
      Caption = 'Use default'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbDefScrBufferSizeClick
    end
    object edScrBufX: TEdit
      Left = 24
      Top = 44
      Width = 49
      Height = 21
      Enabled = False
      MaxLength = 3
      TabOrder = 1
      Text = '80'
      OnKeyPress = EdNumberFilter
    end
    object edScrBufY: TEdit
      Left = 96
      Top = 44
      Width = 49
      Height = 21
      Enabled = False
      MaxLength = 3
      TabOrder = 2
      Text = '200'
      OnKeyPress = EdNumberFilter
    end
  end
end
