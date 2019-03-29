object Form1: TForm1
  Left = 0
  Top = 0
  Caption = 'TDBGridView MyBase Demo'
  ClientHeight = 499
  ClientWidth = 689
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  PixelsPerInch = 96
  TextHeight = 13
  object DBGridView1: TDBGridView
    Left = 0
    Top = 0
    Width = 689
    Height = 499
    Align = alClient
    ColumnClick = True
    ColumnsFullDrag = True
    DataSource = DataSource1
    GridHint = 'Double click on a grid to load data'
    Header.Flat = False
    MultiSelect = True
    ParentShowHint = False
    ShowCellTips = True
    ShowGridHint = True
    ShowHint = True
    TabOrder = 0
    OnDblClick = DBGridView1DblClick
  end
  object DataSource1: TDataSource
    DataSet = ClientDataSet1
    Left = 32
    Top = 48
  end
  object ClientDataSet1: TClientDataSet
    Aggregates = <>
    Params = <>
    Left = 32
    Top = 100
  end
  object OpenDialog1: TOpenDialog
    DefaultExt = 'xml'
    Filter = 
      'MyBase XML Table (*.xml)|*.xml|Client DataSet (*.cds)|*.cds|All ' +
      'Files (*.*)|*.*'
    Left = 32
    Top = 152
  end
end
