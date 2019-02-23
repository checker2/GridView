object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TGridView ColumnClick Demo'
  ClientHeight = 300
  ClientWidth = 635
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object GridView1: TGridView
    Left = 0
    Top = 0
    Width = 635
    Height = 300
    Align = alClient
    Columns = <
      item
        Caption = 'Variable'
        DefWidth = 200
      end
      item
        Caption = 'Value'
        DefWidth = 400
      end>
    Header.Flat = False
    Header.FullSynchronizing = True
    Header.Synchronized = True
    ReadOnly = True
    ShowCellTips = False
    TabOrder = 0
    OnGetCellText = GridView1GetCellText
    OnGetSortDirection = GridView1GetSortDirection
    OnHeaderClick = GridView1HeaderClick
    ExplicitLeft = 232
    ExplicitTop = 116
    ExplicitWidth = 185
    ExplicitHeight = 105
  end
end
