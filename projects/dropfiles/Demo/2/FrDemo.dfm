object Frame1: TFrame1
  Left = 0
  Top = 0
  Width = 320
  Height = 240
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  ParentColor = False
  ParentFont = False
  TabOrder = 0
  object Label1: TLabel
    Left = 0
    Top = 0
    Width = 320
    Height = 15
    Align = alTop
    Alignment = taCenter
    AutoSize = False
    Caption = 'Frame Drop Area'
    Color = clActiveCaption
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clCaptionText
    Font.Height = -15
    Font.Name = 'Arial'
    Font.Style = [fsBold]
    ParentColor = False
    ParentFont = False
  end
  object Memo1: TMemo
    Left = 0
    Top = 15
    Width = 320
    Height = 192
    Align = alClient
    Font.Charset = ANSI_CHARSET
    Font.Color = clWindowText
    Font.Height = -12
    Font.Name = 'Courier New'
    Font.Style = []
    ParentFont = False
    ReadOnly = True
    ScrollBars = ssBoth
    TabOrder = 0
    WordWrap = False
  end
  object Panel1: TPanel
    Left = 0
    Top = 207
    Width = 320
    Height = 33
    Align = alBottom
    BevelOuter = bvLowered
    TabOrder = 1
    object Label2: TLabel
      Left = 8
      Top = 10
      Width = 64
      Height = 15
      Caption = 'Extensions:'
    end
    object Edit1: TEdit
      Left = 83
      Top = 6
      Width = 137
      Height = 23
      TabOrder = 0
      OnChange = Edit1Change
    end
  end
  object PJCtrlDropFiles1: TPJCtrlDropFiles
    Filter = PJExtFileFilter1
    ForegroundOnDrop = False
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = PJCtrlDropFiles1DropFiles
    ManagedControl = Owner
    PassThrough = True
    Left = 40
    Top = 64
  end
  object PJExtFileFilter1: TPJExtFileFilter
    Left = 40
    Top = 104
  end
end
