object DropFilesDemoForm: TDropFilesDemoForm
  Left = 191
  Top = 94
  BorderStyle = bsDialog
  Caption = 'DropFilesDemoForm'
  ClientHeight = 549
  ClientWidth = 778
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 197
    Top = 197
    Width = 145
    Height = 15
    Caption = 'Blue Panel extension filter:'
  end
  object Label2: TLabel
    Left = 384
    Top = 197
    Width = 131
    Height = 15
    Caption = 'Pink RTF wildcard filter: '
  end
  object Label3: TLabel
    Left = 20
    Top = 177
    Width = 106
    Height = 15
    Caption = 'Form custom filter -'
  end
  object Label4: TLabel
    Left = 20
    Top = 197
    Width = 137
    Height = 15
    Caption = 'Accepts file dates before:'
  end
  object dfPanel: TPJDropFiles
    Left = 0
    Top = 299
    Width = 778
    Height = 55
    Align = alBottom
    TabOrder = 0
    Filter = PanelExtFilter
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    PassThrough = False
    OnDropFiles = dfPanelDropFiles
    object Panel1: TPanel
      Left = 0
      Top = 0
      Width = 778
      Height = 55
      Align = alClient
      Caption = 'Panel1'
      Color = 16774636
      TabOrder = 0
      object Panel2: TPanel
        Left = 10
        Top = 9
        Width = 228
        Height = 40
        Caption = 'Panel2'
        ParentColor = True
        TabOrder = 0
      end
      object Panel3: TPanel
        Left = 542
        Top = 9
        Width = 227
        Height = 40
        Caption = 'Panel3'
        ParentColor = True
        TabOrder = 1
      end
    end
  end
  object RichEdit1: TRichEdit
    Left = 0
    Top = 354
    Width = 778
    Height = 195
    Align = alBottom
    Color = 16118015
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    ScrollBars = ssBoth
    TabOrder = 1
    WordWrap = False
  end
  object gbFormDropFiles: TGroupBox
    Left = 10
    Top = 10
    Width = 178
    Height = 149
    Caption = 'Drops on Form'
    TabOrder = 2
    object chkFormIncFiles: TCheckBox
      Left = 10
      Top = 30
      Width = 119
      Height = 20
      Caption = 'Include files'
      TabOrder = 0
      OnClick = FormOptionsClick
    end
    object chkFormIncFolders: TCheckBox
      Left = 10
      Top = 59
      Width = 119
      Height = 21
      Caption = 'Include folders'
      TabOrder = 1
      OnClick = FormOptionsClick
    end
    object chkFormRecurseFolders: TCheckBox
      Left = 10
      Top = 89
      Width = 119
      Height = 21
      Caption = 'Recurse Folders'
      TabOrder = 2
      OnClick = FormOptionsClick
    end
    object chkFormEnabled: TCheckBox
      Left = 10
      Top = 118
      Width = 119
      Height = 21
      Caption = 'Enabled'
      TabOrder = 3
      OnClick = chkFormEnabledClick
    end
  end
  object gbDropFiles: TGroupBox
    Left = 197
    Top = 10
    Width = 178
    Height = 178
    Caption = 'Drops on Blue Panel'
    TabOrder = 3
    object chkPanelIncFiles: TCheckBox
      Left = 10
      Top = 30
      Width = 119
      Height = 20
      Caption = 'Include files'
      TabOrder = 0
      OnClick = PanelOptionsClick
    end
    object chkPanelIncFolders: TCheckBox
      Left = 10
      Top = 59
      Width = 119
      Height = 21
      Caption = 'Include folders'
      TabOrder = 1
      OnClick = PanelOptionsClick
    end
    object chkPanelRecurseFolders: TCheckBox
      Left = 10
      Top = 89
      Width = 119
      Height = 21
      Caption = 'Recurse Folders'
      TabOrder = 2
      OnClick = PanelOptionsClick
    end
    object chkPanelPassThru: TCheckBox
      Left = 10
      Top = 118
      Width = 119
      Height = 21
      Caption = 'Pass Through'
      TabOrder = 3
      OnClick = chkPanelPassThruClick
    end
    object chkPanelEnabled: TCheckBox
      Left = 10
      Top = 148
      Width = 119
      Height = 21
      Caption = 'Enabled'
      TabOrder = 4
      OnClick = chkPanelEnabledClick
    end
  end
  object gbCtrlDropFiles: TGroupBox
    Left = 384
    Top = 10
    Width = 178
    Height = 178
    Caption = 'Drops on Pink RTF'
    TabOrder = 4
    object chkRTFIncFiles: TCheckBox
      Left = 10
      Top = 30
      Width = 119
      Height = 20
      Caption = 'Include files'
      TabOrder = 0
      OnClick = RTFOptionsClick
    end
    object chkRTFIncFolders: TCheckBox
      Left = 10
      Top = 59
      Width = 119
      Height = 21
      Caption = 'Include folders'
      TabOrder = 1
      OnClick = RTFOptionsClick
    end
    object chkRTFRecurseFolders: TCheckBox
      Left = 10
      Top = 89
      Width = 119
      Height = 21
      Caption = 'Recurse Folders'
      TabOrder = 2
      OnClick = RTFOptionsClick
    end
    object chkRTFPassThru: TCheckBox
      Left = 10
      Top = 118
      Width = 119
      Height = 21
      Caption = 'Pass Through'
      TabOrder = 3
      OnClick = chkRTFPassThruClick
    end
    object chkRTFEnabled: TCheckBox
      Left = 10
      Top = 148
      Width = 119
      Height = 21
      Caption = 'Enabled'
      TabOrder = 4
      OnClick = chkRTFEnabledClick
    end
  end
  object edExtensions: TEdit
    Left = 197
    Top = 217
    Width = 149
    Height = 23
    Color = 16774636
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clBlue
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 5
    Text = 'edExtensions'
    OnChange = edExtensionsChange
  end
  object edWildcard: TEdit
    Left = 384
    Top = 217
    Width = 149
    Height = 23
    Color = 16118015
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clRed
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 6
    Text = 'edWildcard'
    OnChange = edWildcardChange
  end
  object DateTimePicker1: TDateTimePicker
    Left = 20
    Top = 217
    Width = 168
    Height = 23
    Date = 38795.698863206020000000
    Time = 38795.698863206020000000
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentFont = False
    TabOrder = 7
  end
  object chkFormEnableFilter: TCheckBox
    Left = 20
    Top = 246
    Width = 119
    Height = 21
    Caption = 'Enable filter'
    TabOrder = 8
    OnClick = chkFormEnableFilterClick
  end
  object chkFormShowRejections: TCheckBox
    Left = 20
    Top = 268
    Width = 168
    Height = 21
    Caption = 'Show rejected files'
    TabOrder = 9
  end
  object chkPanelEnableFilter: TCheckBox
    Left = 197
    Top = 246
    Width = 119
    Height = 21
    Caption = 'Enable filter'
    TabOrder = 10
    OnClick = chkPanelEnableFilterClick
  end
  object chkRTFEnableFilter: TCheckBox
    Left = 384
    Top = 246
    Width = 119
    Height = 21
    Caption = 'Enable filter'
    TabOrder = 11
    OnClick = chkRTFEnableFilterClick
  end
  object dfShape: TPJDropFiles
    Left = 574
    Top = 197
    Width = 176
    Height = 80
    TabOrder = 12
    ForegroundOnDrop = False
    Options = [dfoIncFolders, dfoIncFiles]
    PassThrough = False
    OnDropFiles = dfShapeDropFiles
    object Shape1: TShape
      Left = 0
      Top = 0
      Width = 176
      Height = 80
      Align = alClient
      Brush.Color = 13828050
    end
  end
  object GroupBox1: TGroupBox
    Left = 571
    Top = 10
    Width = 179
    Height = 178
    Caption = 'Drops on Green Shape'
    TabOrder = 13
    object chkShapeIncFiles: TCheckBox
      Left = 10
      Top = 30
      Width = 119
      Height = 20
      Caption = 'Include files'
      TabOrder = 0
      OnClick = ShapeOptionsClick
    end
    object chkShapeIncFolders: TCheckBox
      Left = 10
      Top = 59
      Width = 119
      Height = 21
      Caption = 'Include folders'
      TabOrder = 1
      OnClick = ShapeOptionsClick
    end
    object chkShapeRecurseFolders: TCheckBox
      Left = 10
      Top = 89
      Width = 119
      Height = 21
      Caption = 'Recurse Folders'
      TabOrder = 2
      OnClick = ShapeOptionsClick
    end
    object chkShapePassThru: TCheckBox
      Left = 10
      Top = 118
      Width = 119
      Height = 21
      Caption = 'Pass Through'
      TabOrder = 3
      OnClick = chkShapePassThruClick
    end
    object chkShapeEnabled: TCheckBox
      Left = 10
      Top = 148
      Width = 119
      Height = 21
      Caption = 'Enabled'
      TabOrder = 4
      OnClick = chkShapeEnabledClick
    end
  end
  object dfForm: TPJFormDropFiles
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = dfFormDropFiles
    OnFileFilter = dfFormFileFilter
    Left = 256
    Top = 304
  end
  object dfRTF: TPJCtrlDropFiles
    Filter = RTFWildcardFilter
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = dfRTFDropFiles
    ManagedControl = RichEdit1
    Left = 8
    Top = 296
  end
  object PanelExtFilter: TPJExtFileFilter
    Extensions = '.pas;.dcu'
    Style = fsAll
    Left = 320
    Top = 304
  end
  object RTFWildcardFilter: TPJWildCardFileFilter
    WildCard = '*.*'
    Left = 288
    Top = 304
  end
end
