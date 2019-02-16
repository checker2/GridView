object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TGridView Basic Demo'
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
        Caption = '#'
        FixedSize = True
        DefWidth = 32
      end
      item
        Caption = 'A'
      end
      item
        Caption = 'B'
      end
      item
        Caption = 'C'
      end
      item
        Caption = 'D'
      end
      item
        Caption = 'E'
      end
      item
        Caption = 'F'
      end
      item
        Caption = 'G'
      end
      item
        Caption = 'H'
      end
      item
        Caption = 'I'
      end
      item
        Caption = 'J'
      end
      item
        Caption = 'K'
      end
      item
        Caption = 'L'
      end
      item
        Caption = 'M'
      end
      item
        Caption = 'N'
      end
      item
        Caption = 'O'
      end
      item
        Caption = 'P'
      end
      item
        Caption = 'Q'
      end
      item
        Caption = 'R'
      end
      item
        Caption = 'S'
      end
      item
        Caption = 'T'
      end
      item
        Caption = 'U'
      end
      item
        Caption = 'V'
      end
      item
        Caption = 'W'
      end
      item
        Caption = 'X'
      end
      item
        Caption = 'Y'
      end
      item
        Caption = 'Z'
      end>
    ColumnsFullDrag = True
    DefaultEditMenu = True
    DefaultHeaderMenu = True
    DoubleBuffered = True
    Fixed.Count = 1
    Fixed.Flat = False
    Header.Flat = False
    Header.FullSynchronizing = True
    Header.Synchronized = True
    Rows.Count = 10
    RowSelect = True
    ShowCellTips = False
    TabOrder = 0
    OnGetCellText = GridView1GetCellText
    OnSetEditText = GridView1SetEditText
  end
end
