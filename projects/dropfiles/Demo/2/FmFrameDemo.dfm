object FmMain: TFmMain
  Left = 251
  Top = 114
  Width = 743
  Height = 571
  Caption = 'FmMain'
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 15
  object Panel1: TPanel
    Left = 0
    Top = 260
    Width = 727
    Height = 242
    Align = alClient
    TabOrder = 0
    inline frmLeft: TFrame1
      Left = 1
      Top = 1
      Width = 443
      Height = 240
      Align = alLeft
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 0
      inherited Label1: TLabel
        Width = 443
        Height = 18
      end
      inherited Memo1: TMemo
        Top = 18
        Width = 443
        Height = 181
      end
      inherited Panel1: TPanel
        Top = 199
        Width = 443
        Height = 41
        inherited Label2: TLabel
          Left = 10
          Top = 12
          Width = 76
          Height = 17
        end
        inherited Edit1: TEdit
          Left = 102
          Top = 7
          Width = 169
          Height = 25
        end
      end
      inherited PJCtrlDropFiles1: TPJCtrlDropFiles
        ManagedControl = frmLeft
      end
    end
    inline frmRight: TFrame1
      Left = 444
      Top = 1
      Width = 282
      Height = 240
      Align = alClient
      Color = clBtnFace
      Font.Charset = DEFAULT_CHARSET
      Font.Color = clWindowText
      Font.Height = -15
      Font.Name = 'Arial'
      Font.Style = []
      ParentColor = False
      ParentFont = False
      TabOrder = 1
      inherited Label1: TLabel
        Width = 282
        Height = 18
      end
      inherited Memo1: TMemo
        Top = 18
        Width = 282
        Height = 181
      end
      inherited Panel1: TPanel
        Top = 199
        Width = 282
        Height = 41
        inherited Label2: TLabel
          Left = 10
          Top = 12
          Width = 76
          Height = 17
        end
        inherited Edit1: TEdit
          Left = 102
          Top = 7
          Width = 169
          Height = 25
        end
      end
      inherited PJCtrlDropFiles1: TPJCtrlDropFiles
        ManagedControl = frmRight
      end
    end
  end
  object Panel2: TPanel
    Left = 0
    Top = 0
    Width = 727
    Height = 260
    Align = alTop
    TabOrder = 1
    object Label1: TLabel
      Left = 1
      Top = 1
      Width = 725
      Height = 19
      Align = alTop
      Alignment = taCenter
      AutoSize = False
      Caption = 'Form Drop Area'
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
      Left = 1
      Top = 20
      Width = 725
      Height = 239
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
    Top = 502
    Width = 727
    Height = 31
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
