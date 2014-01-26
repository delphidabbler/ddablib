object ChildForm: TChildForm
  Left = 200
  Top = 118
  Width = 729
  Height = 559
  Caption = 'VCL Demo 2: Child Process'
  Color = clBtnFace
  Constraints.MinHeight = 300
  Constraints.MinWidth = 400
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -12
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poDesktopCenter
  Scaled = False
  OnCreate = FormCreate
  PixelsPerInch = 96
  TextHeight = 14
  object edEnvVars: TMemo
    Left = 0
    Top = 0
    Width = 713
    Height = 521
    Align = alClient
    ScrollBars = ssVertical
    TabOrder = 0
  end
end
