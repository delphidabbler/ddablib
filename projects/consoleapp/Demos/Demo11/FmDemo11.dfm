object Form1: TForm1
  Left = 595
  Top = 116
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 11'
  ClientHeight = 346
  ClientWidth = 348
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
    Top = 312
    Width = 329
    Height = 25
    Caption = '&Run Console App'
    TabOrder = 4
    OnClick = btnRunClick
  end
  object gbConsoleColours: TGroupBox
    Left = 8
    Top = 56
    Width = 329
    Height = 73
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
  end
  object edTitle: TEdit
    Left = 8
    Top = 24
    Width = 329
    Height = 21
    TabOrder = 0
  end
  object gbWindowSize: TGroupBox
    Left = 8
    Top = 136
    Width = 329
    Height = 81
    Caption = 'Window Size'
    TabOrder = 2
    object lblWindowWidth: TLabel
      Left = 8
      Top = 48
      Width = 31
      Height = 13
      Caption = '&Width:'
      Enabled = False
      FocusControl = edWindowWidth
    end
    object lblWindowHeight: TLabel
      Left = 104
      Top = 48
      Width = 34
      Height = 13
      Caption = '&Height:'
      Enabled = False
      FocusControl = edWindowHeight
    end
    object cbDefWindowSize: TCheckBox
      Left = 8
      Top = 16
      Width = 241
      Height = 17
      Caption = 'Use default'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbDefWindowSizeClick
    end
    object edWindowWidth: TEdit
      Left = 48
      Top = 44
      Width = 49
      Height = 21
      Enabled = False
      MaxLength = 3
      TabOrder = 1
      Text = '640'
      OnKeyPress = EdNumberFilter
    end
    object edWindowHeight: TEdit
      Left = 144
      Top = 44
      Width = 49
      Height = 21
      Enabled = False
      MaxLength = 3
      TabOrder = 2
      Text = '480'
      OnKeyPress = EdNumberFilter
    end
  end
  object gbWindowPos: TGroupBox
    Left = 8
    Top = 224
    Width = 329
    Height = 81
    Caption = 'Window Position'
    TabOrder = 3
    object lblWindowLeft: TLabel
      Left = 8
      Top = 48
      Width = 21
      Height = 13
      Caption = '&Left:'
      Enabled = False
      FocusControl = edWindowLeft
    end
    object lblWindowTop: TLabel
      Left = 104
      Top = 48
      Width = 22
      Height = 13
      Caption = 'T&op:'
      Enabled = False
      FocusControl = edWindowTop
    end
    object cbDefWindowPos: TCheckBox
      Left = 8
      Top = 16
      Width = 241
      Height = 17
      Caption = 'Use default'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbDefWindowPosClick
    end
    object edWindowLeft: TEdit
      Left = 48
      Top = 44
      Width = 49
      Height = 21
      Enabled = False
      MaxLength = 3
      TabOrder = 1
      Text = '0'
      OnKeyPress = EdNumberFilter
    end
    object edWindowTop: TEdit
      Left = 144
      Top = 44
      Width = 49
      Height = 21
      Enabled = False
      MaxLength = 3
      TabOrder = 2
      Text = '0'
      OnKeyPress = EdNumberFilter
    end
  end
end
