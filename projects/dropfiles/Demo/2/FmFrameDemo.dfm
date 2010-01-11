object FmMain: TFmMain
  Left = 251
  Top = 114
  Width = 743
  Height = 571
  Caption = 'FmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'MS Sans Serif'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 13
  object Panel1: TPanel
    Left = 0
    Top = 211
    Width = 735
    Height = 301
    Align = alClient
    TabOrder = 0
    inline frmLeft: TFrame1
      Left = 1
      Top = 1
      Width = 360
      Height = 299
      Align = alLeft
      Color = clBtnFace
      ParentColor = False
      TabOrder = 0
      inherited Label1: TLabel
        Width = 360
      end
      inherited Memo1: TMemo
        Width = 360
        Height = 251
      end
      inherited Panel1: TPanel
        Top = 266
        Width = 360
      end
      inherited PJCtrlDropFiles1: TPJCtrlDropFiles
        ManagedControl = frmLeft
      end
    end
    inline frmRight: TFrame1
      Left = 361
      Top = 1
      Width = 373
      Height = 299
      Align = alClient
      Color = clBtnFace
      ParentColor = False
      TabOrder = 1
      inherited Label1: TLabel
        Width = 373
      end
      inherited Memo1: TMemo
        Width = 373
        Height = 251
      end
      inherited Panel1: TPanel
        Top = 266
        Width = 373
      end
      inherited PJCtrlDropFiles1: TPJCtrlDropFiles
        ManagedControl = frmRight
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 735
    Height = 211
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 733
      Height = 15
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Form Drop Area'
      Color = clActiveCaption
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clCaptionText
      Font.Height = -11
      Font.Name = 'MS Sans Serif'
      Font.Style = [fsBold]
      ParentColor = False
      ParentFont = False
    end
    object Memo1: TMemo
      Left = 1
      Top = 16
      Width = 733
      Height = 194
      Align = alClient
      Font.Charset = DEFAULT_CHARSET
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
  end
  object Panel3: TPanel
    Left = 0
    Top = 512
    Width = 735
    Height = 25
    Align = alBottom
    TabOrder = 2
  end
  object PJFormDropFiles1: TPJFormDropFiles
    ForegroundOnDrop = True
    Options = [dfoIncFolders, dfoIncFiles]
    OnDropFiles = PJFormDropFiles1DropFiles
    Left = 88
    Top = 32
  end
end
