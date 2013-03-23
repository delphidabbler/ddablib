object Form1: TForm1
  Left = 595
  Top = 116
  BorderStyle = bsSingle
  Caption = 'TPJConsoleApp Demo 11'
  ClientHeight = 426
  ClientWidth = 428
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object lblTitle: TLabel
    Left = 10
    Top = 10
    Width = 130
    Height = 15
    Caption = 'Console Windows &Title:'
  end
  object btnRun: TButton
    Left = 10
    Top = 384
    Width = 405
    Height = 31
    Caption = '&Run Console App'
    TabOrder = 4
    OnClick = btnRunClick
  end
  object gbConsoleColours: TGroupBox
    Left = 10
    Top = 69
    Width = 405
    Height = 90
    Caption = 'Console colours'
    TabOrder = 1
    object lblForeground: TLabel
      Left = 10
      Top = 25
      Width = 67
      Height = 15
      Caption = '&Foreground:'
      FocusControl = cbForeground
    end
    object lblBackground: TLabel
      Left = 217
      Top = 25
      Width = 69
      Height = 15
      Caption = '&Background:'
      FocusControl = cbBackground
    end
    object cbForeground: TColorBox
      Left = 10
      Top = 43
      Width = 178
      Height = 22
      Selected = clWhite
      Style = [cbStandardColors, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 0
    end
    object cbBackground: TColorBox
      Left = 217
      Top = 43
      Width = 178
      Height = 22
      Style = [cbStandardColors, cbPrettyNames]
      ItemHeight = 16
      TabOrder = 1
    end
  end
  object edTitle: TEdit
    Left = 10
    Top = 30
    Width = 405
    Height = 23
    TabOrder = 0
  end
  object gbWindowSize: TGroupBox
    Left = 10
    Top = 167
    Width = 405
    Height = 100
    Caption = 'Window Size'
    TabOrder = 2
    object lblWindowWidth: TLabel
      Left = 10
      Top = 59
      Width = 34
      Height = 15
      Caption = '&Width:'
      Enabled = False
      FocusControl = edWindowWidth
    end
    object lblWindowHeight: TLabel
      Left = 128
      Top = 59
      Width = 39
      Height = 15
      Caption = '&Height:'
      Enabled = False
      FocusControl = edWindowHeight
    end
    object cbDefWindowSize: TCheckBox
      Left = 10
      Top = 20
      Width = 296
      Height = 21
      Caption = 'Use default'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbDefWindowSizeClick
    end
    object edWindowWidth: TEdit
      Left = 59
      Top = 54
      Width = 60
      Height = 23
      Enabled = False
      MaxLength = 3
      TabOrder = 1
      Text = '640'
      OnKeyPress = EdNumberFilter
    end
    object edWindowHeight: TEdit
      Left = 177
      Top = 54
      Width = 61
      Height = 23
      Enabled = False
      MaxLength = 3
      TabOrder = 2
      Text = '480'
      OnKeyPress = EdNumberFilter
    end
  end
  object gbWindowPos: TGroupBox
    Left = 10
    Top = 276
    Width = 405
    Height = 99
    Caption = 'Window Position'
    TabOrder = 3
    object lblWindowLeft: TLabel
      Left = 10
      Top = 59
      Width = 23
      Height = 15
      Caption = '&Left:'
      Enabled = False
      FocusControl = edWindowLeft
    end
    object lblWindowTop: TLabel
      Left = 128
      Top = 59
      Width = 23
      Height = 15
      Caption = 'T&op:'
      Enabled = False
      FocusControl = edWindowTop
    end
    object cbDefWindowPos: TCheckBox
      Left = 10
      Top = 20
      Width = 296
      Height = 21
      Caption = 'Use default'
      Checked = True
      State = cbChecked
      TabOrder = 0
      OnClick = cbDefWindowPosClick
    end
    object edWindowLeft: TEdit
      Left = 59
      Top = 54
      Width = 60
      Height = 23
      Enabled = False
      MaxLength = 3
      TabOrder = 1
      Text = '0'
      OnKeyPress = EdNumberFilter
    end
    object edWindowTop: TEdit
      Left = 177
      Top = 54
      Width = 61
      Height = 23
      Enabled = False
      MaxLength = 3
      TabOrder = 2
      Text = '0'
      OnKeyPress = EdNumberFilter
    end
  end
end
