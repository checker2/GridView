object HeaderEditorForm: THeaderEditorForm
  Left = 216
  Top = 87
  BorderIcons = [biSystemMenu]
  BorderStyle = bsDialog
  Caption = 'Header Editor'
  ClientHeight = 322
  ClientWidth = 522
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
  object SectionssGroup: TGroupBox
    Left = 8
    Top = 8
    Width = 260
    Height = 271
    Caption = ' Header '
    TabOrder = 0
    object SectionsTree: TTreeView
      Left = 12
      Top = 18
      Width = 235
      Height = 205
      HideSelection = False
      Indent = 19
      ReadOnly = True
      TabOrder = 0
      OnChange = SectionsTreeChange
      OnChanging = SectionsTreeChanging
      OnEnter = SectionsTreeEnter
      OnKeyDown = SectionsTreeKeyDown
    end
    object AddButton: TButton
      Left = 12
      Top = 234
      Width = 115
      Height = 25
      Caption = 'Add'
      TabOrder = 1
      OnClick = AddButtonClick
    end
    object DeleteButton: TButton
      Left = 132
      Top = 234
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
    Width = 235
    Height = 271
    Caption = ' Section Properties '
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
    object AlignmentLabel: TLabel
      Left = 12
      Top = 76
      Width = 51
      Height = 13
      Caption = 'Alignment:'
      FocusControl = AlignmentCombo
    end
    object IndexEdit: TEdit
      Left = 98
      Top = 24
      Width = 123
      Height = 21
      MaxLength = 10
      TabOrder = 0
      OnChange = EnableApply
      OnKeyPress = IndexEditKeyPress
    end
    object CaptionEdit: TEdit
      Left = 98
      Top = 50
      Width = 123
      Height = 21
      TabOrder = 1
      OnChange = EnableApply
    end
    object AlignmentCombo: TComboBox
      Left = 98
      Top = 76
      Width = 123
      Height = 21
      Style = csDropDownList
      TabOrder = 2
      OnChange = EnableApply
      Items.Strings = (
        'Left'
        'Right'
        'Center')
    end
    object WordWrapCheck: TCheckBox
      Left = 98
      Top = 108
      Width = 123
      Height = 17
      Caption = 'Word Wrap'
      TabOrder = 3
      OnClick = EnableApply
    end
  end
  object OKButton: TButton
    Left = 279
    Top = 288
    Width = 75
    Height = 25
    Caption = 'OK'
    Default = True
    TabOrder = 2
    OnClick = OKButtonClick
  end
  object CancelButton: TButton
    Left = 359
    Top = 288
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Cancel'
    ModalResult = 2
    TabOrder = 3
  end
  object ApplyButton: TButton
    Left = 439
    Top = 288
    Width = 75
    Height = 25
    Cancel = True
    Caption = 'Apply'
    Enabled = False
    TabOrder = 4
    OnClick = ApplyButtonClick
  end
end
