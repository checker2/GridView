object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TGridView OwnerDraw Demo'
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
        Caption = 'ID'
      end
      item
        Caption = 'Summary'
        DefWidth = 230
      end
      item
        Caption = 'Assign To'
        DefWidth = 80
      end
      item
        Caption = 'Executed On'
        DefWidth = 120
      end
      item
        Caption = 'Status'
        DefWidth = 100
      end>
    ColumnsFullDrag = True
    DefaultHeaderMenu = True
    GridStyle = [gsHorzLine]
    HighlightEvenRows = True
    ParentShowHint = False
    Rows.Count = 10
    RowSelect = True
    ShowCellTips = True
    ShowHint = True
    TabOrder = 0
    OnGetCellColors = GridView1GetCellColors
    OnGetCellText = GridView1GetCellText
    OnMouseMove = GridView1MouseMove
  end
end
