object EgForm4: TEgForm4
  Left = 192
  Top = 108
  BorderStyle = bsDialog
  Caption = 'Example 4: FixedFileInfo property'
  ClientHeight = 409
  ClientWidth = 519
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
  object Memo1: TMemo
    Left = 0
    Top = 0
    Width = 519
    Height = 366
    Align = alTop
    ReadOnly = True
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object Button3: TButton
    Left = 190
    Top = 372
    Width = 139
    Height = 31
    Hint = 'Help file must be installed'
    Caption = 'View Example'
    TabOrder = 1
    OnClick = Button3Click
  end
  object PJVersionInfo1: TPJVersionInfo
    Left = 8
    Top = 8
  end
end
