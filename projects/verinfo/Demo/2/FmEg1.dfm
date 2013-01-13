object EgForm1: TEgForm1
  Left = 192
  Top = 107
  BorderStyle = bsDialog
  Caption = 'Example 1: Translation, char set & language'
  ClientHeight = 324
  ClientWidth = 599
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
  object ListBox1: TListBox
    Left = 0
    Top = 0
    Width = 599
    Height = 277
    Align = alTop
    ItemHeight = 15
    TabOrder = 0
  end
  object Button3: TButton
    Left = 230
    Top = 284
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
