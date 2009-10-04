object Form2: TForm2
  Left = 432
  Top = 185
  Width = 292
  Height = 235
  Caption = 'Form2'
  Color = clActiveBorder
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  FormStyle = fsMDIChild
  OldCreateOrder = False
  Position = poDefault
  Visible = True
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 8
    Top = 8
    Width = 69
    Height = 13
    Caption = 'CHILD FORM'
    Font.Charset = DEFAULT_CHARSET
    Font.Color = clWindowText
    Font.Height = -11
    Font.Name = 'Tahoma'
    Font.Style = [fsBold]
    ParentFont = False
  end
  object PJWdwState1: TPJWdwState
    AutoSaveRestore = True
    Options = [woFitWorkArea]
    Section = 'ChildForm'
    Left = 164
    Top = 96
  end
end
