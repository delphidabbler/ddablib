object DemoForm: TDemoForm
  Left = 275
  Top = 109
  BorderIcons = [biSystemMenu, biMinimize]
  BorderStyle = bsSingle
  Caption = 'Message Dialog Components Demo'
  ClientHeight = 471
  ClientWidth = 817
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  KeyPreview = True
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object tabCtrl: TTabControl
    Left = 0
    Top = 0
    Width = 817
    Height = 471
    Align = alClient
    TabOrder = 0
    Tabs.Strings = (
      'TPJWinMsgDlg'
      'TPJVCLMsgDlg')
    TabIndex = 0
    OnChange = tabCtrlChange
    object lblAlign: TLabel
      Left = 10
      Top = 39
      Width = 30
      Height = 15
      Caption = 'Align:'
      FocusControl = cbAlign
    end
    object lblOffsetLeft: TLabel
      Left = 98
      Top = 69
      Width = 55
      Height = 15
      Caption = 'OffsetLeft:'
      FocusControl = edOffsetLeft
    end
    object lblOffsetTop: TLabel
      Left = 226
      Top = 69
      Width = 55
      Height = 15
      Caption = 'OffsetTop:'
      FocusControl = edOffsetTop
    end
    object lblButtonGroup: TLabel
      Left = 10
      Top = 108
      Width = 72
      Height = 15
      Caption = 'ButtonGroup:'
      FocusControl = cbButtonGroup
    end
    object lblButtons: TLabel
      Left = 10
      Top = 148
      Width = 45
      Height = 15
      Caption = 'Buttons:'
      FocusControl = lbButtons
    end
    object lblDefButton: TLabel
      Left = 10
      Top = 276
      Width = 57
      Height = 15
      Caption = 'DefButton:'
      FocusControl = cbDefButton
    end
    object lblHelpContext: TLabel
      Left = 10
      Top = 315
      Width = 70
      Height = 15
      Caption = 'HelpContext:'
      FocusControl = edHelpContext
    end
    object lblHelpFile: TLabel
      Left = 10
      Top = 354
      Width = 49
      Height = 15
      Caption = 'HelpFile:'
      FocusControl = edHelpFile
    end
    object lblIconResource: TLabel
      Left = 414
      Top = 39
      Width = 80
      Height = 15
      Caption = 'IconResource:'
      FocusControl = cbIconResource
    end
    object lblKind: TLabel
      Left = 414
      Top = 78
      Width = 28
      Height = 15
      Caption = 'Kind:'
      FocusControl = cbKind
    end
    object lblMakeSound: TLabel
      Left = 414
      Top = 118
      Width = 68
      Height = 15
      Caption = 'MakeSound:'
      FocusControl = chkMakeSound
    end
    object lblOptions: TLabel
      Left = 414
      Top = 147
      Width = 46
      Height = 15
      Caption = 'Options:'
      FocusControl = lbOptions
    end
    object lblText: TLabel
      Left = 414
      Top = 216
      Width = 24
      Height = 15
      Caption = 'Text:'
      FocusControl = edText
    end
    object lblTitle: TLabel
      Left = 414
      Top = 295
      Width = 26
      Height = 15
      Caption = 'Title:'
      FocusControl = edText
    end
    object bvlVertical: TBevel
      Left = 394
      Top = 34
      Width = 2
      Height = 382
    end
    object cbAlign: TComboBox
      Left = 98
      Top = 39
      Width = 179
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      TabOrder = 0
    end
    object edOffsetLeft: TEdit
      Left = 167
      Top = 69
      Width = 51
      Height = 23
      TabOrder = 1
      OnKeyPress = edNumKeyPress
    end
    object edOffsetTop: TEdit
      Left = 295
      Top = 69
      Width = 51
      Height = 23
      TabOrder = 2
    end
    object btnExecute: TButton
      Left = 20
      Top = 423
      Width = 789
      Height = 41
      Caption = 'Store Properties && Execute'
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -20
      Font.Name = 'Arial'
      Font.Style = []
      ParentFont = False
      TabOrder = 17
      OnClick = btnExecuteClick
    end
    object cbButtonGroup: TComboBox
      Left = 98
      Top = 108
      Width = 179
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      TabOrder = 3
      OnChange = cbButtonGroupChange
    end
    object lbButtons: TCheckListBox
      Left = 98
      Top = 148
      Width = 179
      Height = 119
      OnClickCheck = lbButtonsClickCheck
      ItemHeight = 15
      TabOrder = 4
    end
    object cbDefButton: TComboBox
      Left = 98
      Top = 276
      Width = 179
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      TabOrder = 5
    end
    object btnHelpFile: TButton
      Left = 308
      Top = 354
      Width = 67
      Height = 26
      Caption = 'Browse...'
      TabOrder = 8
      OnClick = btnHelpFileClick
    end
    object cbIconResource: TComboBox
      Left = 502
      Top = 39
      Width = 179
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      TabOrder = 10
      Items.Strings = (
        '<empty string>'
        'GREENCIRCLE'
        '#678'
        'INVALID_NAME'
        'MAINICON')
    end
    object cbKind: TComboBox
      Left = 502
      Top = 78
      Width = 179
      Height = 23
      Style = csDropDownList
      ItemHeight = 15
      TabOrder = 11
    end
    object chkMakeSound: TCheckBox
      Left = 502
      Top = 118
      Width = 120
      Height = 20
      TabOrder = 12
    end
    object lbOptions: TCheckListBox
      Left = 502
      Top = 147
      Width = 179
      Height = 55
      OnClickCheck = lbOptionsClickCheck
      ItemHeight = 15
      TabOrder = 13
    end
    object edText: TMemo
      Left = 502
      Top = 216
      Width = 307
      Height = 60
      ScrollBars = ssBoth
      TabOrder = 14
      WordWrap = False
    end
    object edTitle: TEdit
      Left = 502
      Top = 295
      Width = 307
      Height = 23
      TabOrder = 15
    end
    object chkHelpEvent: TCheckBox
      Left = 98
      Top = 394
      Width = 199
      Height = 21
      Caption = 'Use OnHelp event handler'
      TabOrder = 9
    end
    object chkCustomise: TCheckBox
      Left = 502
      Top = 334
      Width = 307
      Height = 21
      Caption = 'Customise dialog using OnShow and OnHide'
      TabOrder = 16
    end
    object edHelpContext: TEdit
      Left = 98
      Top = 315
      Width = 71
      Height = 23
      TabOrder = 6
      OnKeyPress = edNumKeyPress
    end
    object edHelpFile: TEdit
      Left = 98
      Top = 354
      Width = 197
      Height = 23
      TabOrder = 7
    end
  end
  object dlgWinMsg: TPJWinMsgDlg
    IconResource = 'fred'
    Left = 408
    Top = 344
  end
  object dlgVCLMsg: TPJVCLMsgDlg
    Left = 440
    Top = 344
  end
  object dlgVCLDummy: TPJVCLMsgDlg
    Left = 472
    Top = 344
  end
  object dlgHelpFile: TOpenDialog
    Left = 504
    Top = 344
  end
end
