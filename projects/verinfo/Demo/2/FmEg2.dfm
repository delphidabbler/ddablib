object EgForm2: TEgForm2
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Example 2: String information properties'
  ClientHeight = 345
  ClientWidth = 449
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  Scaled = False
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 15
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 449
    Height = 297
    Align = alTop
    ReadOnly = True
    TabOrder = 0
  end
  object Button1: TButton
    Left = 11
    Top = 305
    Width = 92
    Height = 31
    Caption = 'Method 1'
    TabOrder = 1
    OnClick = Button1Click
  end
  object Button2: TButton
    Left = 346
    Top = 305
    Width = 92
    Height = 31
    Caption = 'Method 2'
    TabOrder = 3
    OnClick = Button2Click
  end
  object Button3: TButton
    Left = 155
    Top = 305
    Width = 139
    Height = 31
    Hint = 'Help file must be installed'
    Caption = 'View Example'
    TabOrder = 2
    OnClick = Button3Click
  end
  object PJVersionInfo1: TPJVersionInfo
    Left = 8
    Top = 8
  end
end
