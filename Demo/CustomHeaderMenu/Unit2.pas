unit Unit2;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Ex_Grid, Vcl.ExtCtrls, Vcl.StdCtrls;

type
  TGridColumnsDlg = class(TForm)
    Label1: TLabel;
    ColumnsGrid: TGridView;
    Label2: TLabel;
    ShowButton: TButton;
    HideButton: TButton;
    Bevel1: TBevel;
    OKButton: TButton;
    CancelButton: TButton;
    procedure FormShow(Sender: TObject);
    procedure ColumnsGridCheckClick(Sender: TObject; Cell: TGridCell);
    procedure ColumnsGridGetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure ColumnsGridResize(Sender: TObject);
    procedure ColumnsGridGetCheckStateEx(Sender: TObject; Cell: TGridCell;
      var CheckState: TCheckBoxState; var CheckEnabled: Boolean);
    procedure ShowButtonClick(Sender: TObject);
    procedure OKButtonClick(Sender: TObject);
  private
    FColumns: TGridColumns;
    FCheckList: array of Boolean;
  protected
    procedure UpdateActions; override;
  public
    { Public declarations }
  end;

function ShowGridColumnsDlg(AGrid: TCustomGridView): Boolean;

implementation

{$R *.dfm}

function ShowGridColumnsDlg(AGrid: TCustomGridView): Boolean;
begin
  with TGridColumnsDlg.Create(Application) do
  try
    FColumns := AGrid.Columns;
    Result := (AGrid <> nil) and (ShowModal = mrOK);
  finally
    Free;
  end;
end;

procedure TGridColumnsDlg.ColumnsGridCheckClick(Sender: TObject;
  Cell: TGridCell);
begin
  if FColumns[Cell.Row].DefaultPopup then
  begin
    FCheckList[Cell.Row] := not FCheckList[Cell.Row];
    ColumnsGrid.InvalidateCell(Cell);
  end;
end;

procedure TGridColumnsDlg.ColumnsGridGetCellText(Sender: TObject;
  Cell: TGridCell; var Value: string);
begin
  Value := FColumns[Cell.Row].Caption2;
end;

procedure TGridColumnsDlg.ColumnsGridGetCheckStateEx(Sender: TObject;
  Cell: TGridCell; var CheckState: TCheckBoxState; var CheckEnabled: Boolean);
begin
  CheckState := TCheckBoxState(Ord(FCheckList[Cell.Row]));
  CheckEnabled := FColumns[Cell.Row].DefaultPopup;
end;

procedure TGridColumnsDlg.ColumnsGridResize(Sender: TObject);
begin
  ColumnsGrid.Columns[0].Width := ColumnsGrid.ClientWidth - 1;
end;

procedure TGridColumnsDlg.FormShow(Sender: TObject);
var
  I: Integer;
begin
  SetLength(FCheckList, FColumns.Count);
  for I := 0 to FColumns.Count - 1 do FCheckList[I] := FColumns[I].Visible;
  ColumnsGrid.Rows.Count := FColumns.Count;
  ColumnsGrid.CellFocused := GridCell(0, 0);
  ColumnsGrid.CellSelected := True;
  SelectFirst;
end;

procedure TGridColumnsDlg.OKButtonClick(Sender: TObject);
var
  I: Integer;
begin
  FColumns.BeginUpdate;
  try
    for I := 0 to FColumns.Count - 1 do
      FColumns[I].Visible := FCheckList[I];
  finally
    FColumns.EndUpdate;
  end;
  ModalResult := mrOK;
end;

procedure TGridColumnsDlg.ShowButtonClick(Sender: TObject);
begin
  ColumnsGridCheckClick(nil, ColumnsGrid.CellFocused);
end;

procedure TGridColumnsDlg.UpdateActions;
begin
  inherited;
  with ColumnsGrid do
  begin
    ShowButton.Enabled := CellSelected and FColumns[Row].DefaultPopup and (not FCheckList[Row]);
    HideButton.Enabled := CellSelected and FColumns[Row].DefaultPopup and FCheckList[Row];
  end;
  ShowButton.Default := ShowButton.Enabled;
  HideButton.Default := HideButton.Enabled;
  OKButton.Default := (not ShowButton.Default) and (not HideButton.Default);
end;

end.

