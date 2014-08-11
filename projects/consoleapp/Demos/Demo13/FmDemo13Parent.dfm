object ParentForm: TParentForm
  Left = 278
  Top = 152
  BorderStyle = bsDialog
  Caption = 'TPJConsoleApp Demo 13 '
  ClientHeight = 355
  ClientWidth = 570
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Arial'
  Font.Style = []
  OldCreateOrder = False
  Position = poDesktopCenter
  Scaled = False
  PixelsPerInch = 96
  TextHeight = 15
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 483
    Height = 15
    Caption = 
      'Enter &environment variables to pass to new process in Name=Valu' +
      'e format, one per line:'
    FocusControl = Memo1
  end
  object Memo1: TMemo
    Left = 8
    Top = 32
    Width = 553
    Height = 241
    Lines.Strings = (
      'FOO=bar'
      'ANSWER=42')
    ScrollBars = ssVertical
    TabOrder = 0
  end
  object CheckBox1: TCheckBox
    Left = 8
    Top = 288
    Width = 553
    Height = 17
    Caption = '&Include this program'#39's environment variables in new block'
    TabOrder = 1
  end
  object Button1: TButton
    Left = 8
    Top = 320
    Width = 177
    Height = 25
    Caption = 'Execute Child Process'
    TabOrder = 2
    OnClick = Button1Click
  end
end
