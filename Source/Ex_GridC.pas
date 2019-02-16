{
  Библиотека дополнительных компонентов

  Редактор столбцов компонента TGridView

  © Роман М. Мочалов, 1997-2019
  E-mail: checker@mail.ru
}

unit Ex_GridC;
                                        
interface

uses
  Windows, SysUtils, Classes, Controls, Forms, Graphics, StdCtrls, ComCtrls,
  Ex_Grid;

type

{ TColumnsEditorForm }

  TColumnsEditorForm = class(TForm)
    ColumnsGroup: TGroupBox;
    ColumnsList: TListView;
    AddButton: TButton;
    DeleteButton: TButton;
    PropertiesGroup: TGroupBox;
    IndexLabel: TLabel;
    IndexEdit: TEdit;
    CaptionLabel: TLabel;
    CaptionEdit: TEdit;
    WidthLabel: TLabel;
    WidthEdit: TEdit;
    MinWidthLabel: TLabel;
    MinWidthEdit: TEdit;
    MaxWidthLabel: TLabel;
    MaxWidthEdit: TEdit;
    AlignmentLabel: TLabel;
    AlignmentCombo: TComboBox;
    MaxLengthLabel: TLabel;
    MaxLengthEdit: TEdit;
    EditStyleLabel: TLabel;
    EditStyleCombo: TComboBox;
    CheckKindLabel: TLabel;
    CheckKindCombo: TComboBox;
    FixedSizeCheck: TCheckBox;
    ReadOnlyCheck: TCheckBox;
    WantReturnsCheck: TCheckBox;
    WordWrapCheck: TCheckBox;
    TabStopCheck: TCheckBox;
    VisibleCheck: TCheckBox;
    OKButton: TButton;
    CancelButton: TButton;
    ApplyButton: TButton;
    procedure EnableApply(Sender: TObject);
    procedure DisableApply(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure ColumnsListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
    procedure ColumnsListChanging(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
    procedure ColumnsListEnter(Sender: TObject);
    procedure ColumnsListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure OKButtonClick(Sender: TObject);
    procedure ApplyButtonClick(Sender: TObject);
    procedure AddButtonClick(Sender: TObject);
    procedure DeleteButtonClick(Sender: TObject);
    procedure IndexEditKeyPress(Sender: TObject; var Key: Char);
  private
    FGrid: TCustomGridView;
    FColumns: TGridColumns;
    FExceptCount: Integer;
    FChangeCount: Integer;
    procedure AddColumn;
    procedure DeleteAllColumns;
    procedure DeleteColumn;
    procedure CheckColumn;
    procedure GetColumn;
    procedure GetParams;
    procedure MoveColumnDown;
    procedure MoveColumnUp;
    procedure PutColumn;
    procedure PutParams;
    procedure RefreshView;
  public
    function Execute(Grid: TCustomGridView): Boolean;
  end;

function EditGridColumns(Grid: TCustomGridView): Boolean;

implementation

{$R *.DFM}

function EditGridColumns(Grid: TCustomGridView): Boolean;
begin
  with TColumnsEditorForm.Create(nil) do
  try
    Execute(Grid);
    Result := FChangeCount > 0;
  finally
    Free;
  end;
end;

type
  EColumnError = class(Exception);

{ TColumnsEditorForm }

procedure TColumnsEditorForm.AddColumn;
begin
  FColumns.Add;
  RefreshView;
  CaptionEdit.SetFocus;
  EnableApply(nil);
end;

procedure TColumnsEditorForm.DeleteAllColumns;
const
  Message = 'Delete all columns?';
  Flags = MB_YESNO or MB_ICONQUESTION;
begin
  if ColumnsList.Items.Count > 0 then
    if Application.MessageBox(Message, PChar(Caption), Flags) = IDYES then
    begin
      FColumns.Clear;
      RefreshView;
      EnableApply(nil);
    end;
end;

procedure TColumnsEditorForm.DeleteColumn;
var
  I: Integer;
begin
  with ColumnsList do
  begin
    I := Items.IndexOf(ItemFocused);
    if I <> -1 then
    begin
      FColumns[I].Free;
      RefreshView;
      ColumnsList.SetFocus;
      EnableApply(nil);
    end;
  end;
end;

procedure TColumnsEditorForm.CheckColumn;
var
  I: Integer;
begin
  if PropertiesGroup.Enabled then
  begin
    with IndexEdit do
    try
      I := StrToIntDef(Text, -1);
      if (I < 0) or (I > FColumns.Count - 1) then
        raise EColumnError.CreateFmt('Invalid index (%s)', [Text]);
    except
      SetFocus;
      raise;
    end;
    with WidthEdit do
    try
      StrToInt(Text);
    except
      SetFocus;
      raise;
    end;
    with MinWidthEdit do
    try
      StrToInt(Text);
    except
      SetFocus;
      raise;
    end;
    with MaxWidthEdit do
    try
      StrToInt(Text);
    except
      SetFocus;
      raise;
    end;
    with MaxLengthEdit do
    try
      StrToInt(Text);
    except
      SetFocus;
      raise;
    end;
  end;
end;

procedure TColumnsEditorForm.GetColumn;
var
  I: Integer;
begin
  with ColumnsList do
  begin
    I := Items.IndexOf(ItemFocused);
    if I <> -1 then
    begin
      IndexEdit.Text := IntToStr(I);
      CaptionEdit.Text := FColumns[I].Caption;
      WidthEdit.Text := IntToStr(FColumns[I].DefWidth);
      MinWidthEdit.Text := IntToStr(FColumns[I].MinWidth);
      MaxWidthEdit.Text := IntToStr(FColumns[I].MaxWidth);
      AlignmentCombo.ItemIndex := Ord(FColumns[I].Alignment);
      MaxLengthEdit.Text := IntToStr(FColumns[I].MaxLength);
      EditStyleCombo.ItemIndex := Ord(FColumns[I].EditStyle);
      CheckKindCombo.ItemIndex := Ord(FColumns[I].CheckKind);
      FixedSizeCheck.Checked := FColumns[I].FixedSize;
      ReadOnlyCheck.Checked := FColumns[I].ReadOnly;
      WantReturnsCheck.Checked := FColumns[I].WantReturns;
      WordWrapCheck.Checked := FColumns[I].WordWrap;
      TabStopCheck.Checked := not FColumns[I].TabStop;
      VisibleCheck.Checked := not FColumns[I].Visible;
    end;
  end;
end;

procedure TColumnsEditorForm.GetParams;
begin
  FColumns.Assign(FGrid.Columns);
  RefreshView
end;

procedure TColumnsEditorForm.MoveColumnDown;
var
  I: Integer;
begin
  with ColumnsList do
  begin
    I := Items.IndexOf(ItemFocused);
    if I < Items.Count - 1 then
    begin
      FColumns[I].Index := I + 1;
      RefreshView;
      ItemFocused.Selected := False;
      ItemFocused := Items[I + 1];
      ItemFocused.Selected := True;
      ItemFocused.MakeVisible(True);
      EnableApply(nil);
    end;
  end;
end;

procedure TColumnsEditorForm.MoveColumnUp;
var
  I: Integer;
begin
  with ColumnsList do
  begin
    I := Items.IndexOf(ItemFocused);
    if I > 0 then
    begin
      FColumns[I].Index := I - 1;
      RefreshView;
      ItemFocused.Selected := False;
      ItemFocused := Items[I - 1];
      ItemFocused.Selected := True;
      ItemFocused.MakeVisible(True);
      EnableApply(nil);
    end;
  end;
end;

procedure TColumnsEditorForm.PutColumn;
var
  I: Integer;
begin
  with ColumnsList do
  begin
    I := Items.IndexOf(ItemFocused);
    if I <> -1 then
      with FColumns[I] do
      begin
        Index := StrToInt(IndexEdit.Text);
        Caption := CaptionEdit.Text;
        DefWidth := StrToInt(WidthEdit.Text);
        MinWidth := StrToInt(MinWidthEdit.Text);
        MaxWidth := StrToInt(MaxWidthEdit.Text);
        Alignment := TAlignment(AlignmentCombo.ItemIndex);
        MaxLength := StrToInt(MaxLengthEdit.Text);
        FColumns[I].EditStyle := TGridEditStyle(EditStyleCombo.ItemIndex);
        CheckKind := TGridCheckKind(CheckKindCombo.ItemIndex);
        FixedSize := FixedSizeCheck.Checked;
        ReadOnly := ReadOnlyCheck.Checked;
        WantReturns := WantReturnsCheck.Checked;
        WordWrap := WordWrapCheck.Checked;
        TabStop := not TabStopCheck.Checked;
        Visible := not VisibleCheck.Checked;
        RefreshView;
      end;
  end;
end;

procedure TColumnsEditorForm.PutParams;
begin
  FGrid.Columns := FColumns;
end;

procedure TColumnsEditorForm.RefreshView;

  procedure BeginRefresh;
  begin
    ColumnsList.OnChange := nil;
    ColumnsList.OnChanging := nil;
    IndexEdit.OnChange := nil;
    CaptionEdit.OnChange := nil;
    WidthEdit.OnChange := nil;
    MinWidthEdit.OnChange := nil;
    MaxWidthEdit.OnChange := nil;
    AlignmentCombo.OnChange := nil;
    MaxLengthEdit.OnChange := nil;
    EditStyleCombo.OnChange := nil;
    CheckKindCombo.OnChange := nil;
    FixedSizeCheck.OnClick := nil;
    ReadOnlyCheck.OnClick := nil;
    WantReturnsCheck.OnClick := nil;
    WordWrapCheck.OnClick := nil;
    TabStopCheck.OnClick := nil;
    VisibleCheck.OnClick := nil;
  end;

  procedure EndRefresh;
  begin
    ColumnsList.OnChange := ColumnsListChange;
    ColumnsList.OnChanging := ColumnsListChanging;
    IndexEdit.OnChange := EnableApply;
    CaptionEdit.OnChange := EnableApply;
    WidthEdit.OnChange := EnableApply;
    MinWidthEdit.OnChange := EnableApply;
    MaxWidthEdit.OnChange := EnableApply;
    AlignmentCombo.OnChange := EnableApply;
    MaxLengthEdit.OnChange := EnableApply;
    EditStyleCombo.OnChange := EnableApply;
    CheckKindCombo.OnChange := EnableApply;
    FixedSizeCheck.OnClick := EnableApply;
    ReadOnlyCheck.OnClick := EnableApply;
    WantReturnsCheck.OnClick := EnableApply;
    WordWrapCheck.OnClick := EnableApply;
    TabStopCheck.OnClick := EnableApply;
    VisibleCheck.OnClick := EnableApply;
  end;

  procedure RefreshListView;
  var
    I, C: Integer;

    procedure SetCaption(Item: Integer; const Value: string);
    begin
      with ColumnsList.Items[Item] do
        if Caption <> Value then
        begin
          Caption := Value;
          Inc(C);
        end;
    end;

    procedure SetSubItem(Item, Index: Integer; const Value: string);
    begin
      with ColumnsList.Items[Item].SubItems do
        if Strings[Index] <> Value then
        begin
          Strings[Index] := Value;
          Inc(C);
        end;
    end;

    procedure UpdateItem(Item: Integer);
    var
      R: TRect;
    begin
      with ColumnsList do
      begin
        R := Items[Item].DisplayRect(drBounds);
        InvalidateRect(Handle, @R, False);
      end;
    end;

  begin
    with ColumnsList do
    begin
      if Items.Count > FColumns.Count then
      begin
        I := Items.Count;
        while I > FColumns.Count do
        begin
          Dec(I);
          Items.Delete(I);
        end;
      end
      else if Items.Count < FColumns.Count then
      begin
        I := Items.Count;
        while I < FColumns.Count do
        begin
          Inc(I);
          with Items.Add do
          begin
            SubItems.Add('');
          end;
        end;
        if Items.Count > 0 then
        begin
          ItemFocused := Items[Items.Count - 1];
          Selected := Items[Items.Count - 1];
          Selected.MakeVisible(True);
        end;
      end;
    end;
    with FColumns do
    begin
      for I := 0 to Count - 1 do
      begin
        C := 0;
        with Columns[I] do
        begin
          SetCaption(I, IntToStr(I));
          SetSubItem(I, 0, Caption);
        end;
        if C <> 0 then UpdateItem(I);
      end;
    end;
  end;

  procedure RefreshControls;
  begin
    with ColumnsList do
    begin
      if Items.IndexOf(ItemFocused) <> -1 then
      begin
        PropertiesGroup.Enabled := True;
        DeleteButton.Enabled := True;
        GetColumn;
      end
      else
      begin
        VisibleCheck.Checked := False;
        TabStopCheck.Checked := False;
        WantReturnsCheck.Checked := False;
        WordWrapCheck.Checked := False;
        ReadOnlyCheck.Checked := False;
        FixedSizeCheck.Checked := False;
        EditStyleCombo.ItemIndex := -1;
        CheckKindCombo.ItemIndex := -1;
        MaxLengthEdit.Text := '';
        AlignmentCombo.ItemIndex := -1;
        MaxWidthEdit.Text := '';
        MinWidthEdit.Text := '';
        WidthEdit.Text := '';
        CaptionEdit.Text := '';
        IndexEdit.Text := '';
        DeleteButton.Enabled := False;
        PropertiesGroup.Enabled := False;
      end;
    end;
  end;

begin
  BeginRefresh;
  try
    RefreshListView;
    RefreshControls;
  finally
    Endrefresh;
  end;
end;

function TColumnsEditorForm.Execute(Grid: TCustomGridView): Boolean;
begin
  FGrid := Grid;
  GetParams;
  Result := ShowModal = mrOK;
end;

procedure TColumnsEditorForm.EnableApply(Sender: TObject);
begin
  OKButton.Default := False;
  ApplyButton.Enabled := True;
  ApplyButton.Default := True;
end;

procedure TColumnsEditorForm.DisableApply(Sender: TObject);
begin
  ApplyButton.Default := False;
  ApplyButton.Enabled := False;
  OKButton.Default := True;
end;

procedure TColumnsEditorForm.FormCreate(Sender: TObject);
const
  LVM_SETEXTSTYLE = $1000 + 54;
  LVS_EX_FULLROWSELECT = $00000020;
begin
  SendMessage(ColumnsList.Handle, LVM_SETEXTSTYLE, 0, LVS_EX_FULLROWSELECT);
  FColumns := TGridColumns.Create(nil);
end;

procedure TColumnsEditorForm.FormDestroy(Sender: TObject);
begin
  FColumns.Free;
end;

procedure TColumnsEditorForm.ColumnsListChange(Sender: TObject; Item: TListItem; Change: TItemChange);
begin
  if not (csDestroying in ComponentState) then RefreshView;
end;

procedure TColumnsEditorForm.ColumnsListChanging(Sender: TObject; Item: TListItem; Change: TItemChange; var AllowChange: Boolean);
begin
  if not (csDestroying in ComponentState) then
  begin
    try
      CheckColumn;
      PutColumn;
      RefreshView;
    except
      if FExceptCount = 0 then Application.HandleException(Self);
      Inc(FExceptCount);
    end;
    AllowChange := FExceptCount = 0;
  end;
end;

procedure TColumnsEditorForm.ColumnsListEnter(Sender: TObject);
begin
  FExceptCount := 0;
end;

procedure TColumnsEditorForm.ColumnsListKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Shift = [] then
    case Key of
      VK_INSERT:
        begin
          with AddButton do if Enabled then Click;
          Key := 0;
        end;
      VK_DELETE:
        begin
          with DeleteButton do if Enabled then Click;
          Key := 0;
        end;
    end;
  if Shift = [ssCtrl] then
    case Key of
      VK_DELETE:
        begin
          DeleteAllColumns;
          Key := 0;
        end;
      VK_UP:
        begin
          CheckColumn;
          PutColumn;
          MoveColumnUp;
          Key := 0;
        end;
      VK_DOWN:
        begin
          CheckColumn;
          PutColumn;
          MoveColumnDown;
          Key := 0;
        end;
    end;
end;

procedure TColumnsEditorForm.IndexEditKeyPress(Sender: TObject; var Key: Char);
begin
  if not CharInSet(Key, [#8, '0'..'9']) then
  begin
    MessageBeep(0);
    Key := #0;
  end;
end;

procedure TColumnsEditorForm.OKButtonClick(Sender: TObject);
begin
  ApplyButtonClick(ApplyButton);
  ModalResult := mrOK;
end;

procedure TColumnsEditorForm.ApplyButtonClick(Sender: TObject);
begin
  CheckColumn;
  PutColumn;
  PutParams;
  GetParams;
  DisableApply(nil);
  Inc(FChangeCount);
end;

procedure TColumnsEditorForm.AddButtonClick(Sender: TObject);
begin
  CheckColumn;
  PutColumn;
  AddColumn;
end;

procedure TColumnsEditorForm.DeleteButtonClick(Sender: TObject);
begin
  CheckColumn;
  PutColumn;
  DeleteColumn;
end;

end.


