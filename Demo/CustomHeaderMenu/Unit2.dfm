object GridColumnsDlg: TGridColumnsDlg
  Left = 0
  Top = 0
  BorderStyle = bsDialog
  Caption = 'Choose Columns'
  ClientHeight = 418
  ClientWidth = 339
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = False
  Position = poScreenCenter
  OnShow = FormShow
  PixelsPerInch = 96
  TextHeight = 13
  object Label1: TLabel
    Left = 11
    Top = 11
    Width = 316
    Height = 33
    AutoSize = False
    Caption = 'Select the columns you want to display.'
    Transparent = True
    WordWrap = True
  end
  object Label2: TLabel
    Left = 11
    Top = 44
    Width = 44
    Height = 13
    Caption = 'Columns:'
  end
  object Bevel1: TBevel
    Left = 11
    Top = 372
    Width = 316
    Height = 4
    Shape = bsTopLine
    Visible = False
  end
  object ColumnsGrid: TGridView
    Left = 11
    Top = 63
    Width = 233
    Height = 290
    CheckBoxes = True
    Columns = <
      item
        CheckKind = gcCheckBox
      end>
    GridLines = False
    ShowCellTips = False
    ShowHeader = False
    TabOrder = 0
    OnCheckClick = ColumnsGridCheckClick
    OnGetCellText = ColumnsGridGetCellText
    OnGetCheckStateEx = ColumnsGridGetCheckStateEx
    OnResize = ColumnsGridResize
  end
  object ShowButton: TButton
    Left = 254
    Top = 63
    Width = 75
    Height = 23
    Caption = '&Show'
    TabOrder = 1
    OnClick = ShowButtonClick
  end
  object HideButton: TButton
    Left = 254
    Top = 91
    Width = 75
    Height = 23
    Caption = '&Hide'
    TabOrder = 2
    OnClick = ShowButtonClick
  end
  object OKButton: TButton
    Left = 174
    Top = 384
    Width = 75
    Height = 23
    Caption = 'OK'
    TabOrder = 3
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 254
    Top = 384
    Width = 75
    Height = 23
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 4
  end
end
