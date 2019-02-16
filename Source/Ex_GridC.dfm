object ColumnsEditorForm: TColumnsEditorForm
  Left = 202
  Top = 65
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Columns Editor'
  ClientHeight = 436
  ClientWidth = 542
  Color = clBtnFace
  Font.Charset = DEFAULT_CHARSET
  Font.Color = clWindowText
  Font.Height = -11
  Font.Name = 'Tahoma'
  Font.Style = []
  OldCreateOrder = True
  Position = poScreenCenter
  OnCreate = FormCreate
  OnDestroy = FormDestroy
  PixelsPerInch = 96
  TextHeight = 13
  object ColumnsGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 260
    Height = 384
    Caption = ' Columns '
    TabOrder = 0
    object ColumnsList: TListView
      Left = 12
      Top = 18
      Width = 235
      Height = 320
      Columns = <
        item
          Caption = 'Index'
          Width = 30
        end
        item
          Caption = 'Name'
          Width = 180
        end>
      ColumnClick = False
      HideSelection = False
      TabOrder = 0
      ViewStyle = vsReport
      OnChanging = ColumnsListChanging
      OnEnter = ColumnsListEnter
      OnKeyDown = ColumnsListKeyDown
    end
    object AddButton: TButton
      Left = 12
      Top = 346
      Width = 115
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object DeleteButton: TButton
      Left = 132
      Top = 346
      Width = 115
      Height = 25
      Caption = 'Delete'
      Enabled = False
      TabOrder = 2
      OnClick = DeleteButtonClick
    end
  end
  object PropertiesGroup: TGroupBox
    Left = 279
    Top = 8
    Width = 255
    Height = 384
    Caption = ' Column Properties '
    TabOrder = 1
    object IndexLabel: TLabel
      Left = 12
      Top = 24
      Width = 32
      Height = 13
      Caption = 'Index:'
      FocusControl = IndexEdit
    end
    object CaptionLabel: TLabel
      Left = 12
      Top = 50
      Width = 41
      Height = 13
      Caption = 'Caption:'
      FocusControl = CaptionEdit
    end
    object WidthLabel: TLabel
      Left = 12
      Top = 76
      Width = 32
      Height = 13
      Caption = 'Width:'
      FocusControl = WidthEdit
    end
    object MinWidthLabel: TLabel
      Left = 12
      Top = 102
      Width = 55
      Height = 13
      Caption = 'Min. Width:'
      FocusControl = MinWidthEdit
    end
    object MaxWidthLabel: TLabel
      Left = 12
      Top = 128
      Width = 59
      Height = 13
      Caption = 'Max. Width:'
      FocusControl = MaxWidthEdit
    end
    object AlignmentLabel: TLabel
      Left = 12
      Top = 154
      Width = 51
      Height = 13
      Caption = 'Alignment:'
      FocusControl = AlignmentCombo
    end
    object MaxLengthLabel: TLabel
      Left = 12
      Top = 180
      Width = 64
      Height = 13
      Caption = 'Max. Length:'
      FocusControl = MaxLengthEdit
    end
    object EditStyleLabel: TLabel
      Left = 12
      Top = 206
      Width = 49
      Height = 13
      Caption = 'Edit Style:'
      FocusControl = EditStyleCombo
    end
    object CheckKindLabel: TLabel
      Left = 12
      Top = 232
      Width = 56
      Height = 13
      Caption = 'Check Kind:'
      FocusControl = CheckKindCombo
    end
    object IndexEdit: TEdit
      Left = 98
      Top = 24
      Width = 142
      Height = 21
      MaxLength = 10
      TabOrder = 0
      OnChange = EnableApply
      OnKeyPress = IndexEditKeyPress
    end
    object CaptionEdit: TEdit
      Left = 98
      Top = 50
      Width = 142
      Height = 21
      TabOrder = 1
      OnChange = EnableApply
    end
    object WidthEdit: TEdit
      Left = 98
      Top = 76
      Width = 142
      Height = 21
      MaxLength = 10
      TabOrder = 2
      OnChange = EnableApply
      OnKeyPress = IndexEditKeyPress
    end
    object MinWidthEdit: TEdit
      Left = 98
      Top = 102
      Width = 142
      Height = 21
      MaxLength = 10
      TabOrder = 3
      OnChange = EnableApply
      OnKeyPress = IndexEditKeyPress
    end
    object MaxWidthEdit: TEdit
      Left = 98
      Top = 128
      Width = 142
      Height = 21
      MaxLength = 10
      TabOrder = 4
      OnChange = EnableApply
      OnKeyPress = IndexEditKeyPress
    end
    object AlignmentCombo: TComboBox
      Left = 98
      Top = 154
      Width = 142
      Height = 21
      Style = csDropDownList
      TabOrder = 5
      OnChange = EnableApply
      Items.Strings = (
        'Left'
        'Right'
        'Center')
    end
    object MaxLengthEdit: TEdit
      Left = 98
      Top = 180
      Width = 142
      Height = 21
      MaxLength = 10
      TabOrder = 6
      OnChange = EnableApply
      OnKeyPress = IndexEditKeyPress
    end
    object EditStyleCombo: TComboBox
      Left = 98
      Top = 206
      Width = 142
      Height = 21
      Style = csDropDownList
      TabOrder = 7
      OnChange = EnableApply
      Items.Strings = (
        'Simple'
        'Ellipsis'
        'PickList'
        'DataList'
        'UserDefine')
    end
    object FixedSizeCheck: TCheckBox
      Left = 98
      Top = 264
      Width = 142
      Height = 17
      Caption = 'Fixed Size'
      TabOrder = 9
      OnClick = EnableApply
    end
    object ReadOnlyCheck: TCheckBox
      Left = 98
      Top = 282
      Width = 142
      Height = 17
      Caption = 'ReadOnly'
      TabOrder = 10
      OnClick = EnableApply
    end
    object WantReturnsCheck: TCheckBox
      Left = 98
      Top = 300
      Width = 142
      Height = 17
      Caption = 'Want Returns'
      TabOrder = 11
      OnClick = EnableApply
    end
    object WordWrapCheck: TCheckBox
      Left = 98
      Top = 318
      Width = 142
      Height = 17
      Caption = 'Word Wrap'
      TabOrder = 12
      OnClick = EnableApply
    end
    object TabStopCheck: TCheckBox
      Left = 98
      Top = 336
      Width = 142
      Height = 17
      Caption = 'Tab Stop'
      TabOrder = 13
      OnClick = EnableApply
    end
    object VisibleCheck: TCheckBox
      Left = 98
      Top = 354
      Width = 142
      Height = 17
      Caption = 'Visible'
      TabOrder = 14
      OnClick = EnableApply
    end
    object CheckKindCombo: TComboBox
      Left = 98
      Top = 232
      Width = 142
      Height = 21
      Style = csDropDownList
      TabOrder = 8
      OnChange = EnableApply
      Items.Strings = (
        'None'
        'CheckBox'
        'RadioButton'
        'UserDefine')
    end
  end
  object OKButton: TButton
    Left = 299
    Top = 402
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 379
    Top = 402
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ApplyButton: TButton
    Left = 459
    Top = 402
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Apply'
    Enabled = False
    TabOrder = 4
    OnClick = ApplyButtonClick
  end
end
