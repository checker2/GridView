unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Ex_Grid, System.ImageList, Vcl.ImgList,
  Vcl.StdCtrls;

type
  TForm1 = class(TForm)
    GridView1: TGridView;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure GridView1GetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure GridView1GetCellImageEx(Sender: TObject; Cell: TGridCell;
      var ImageIndex, OverlayIndex: Integer);
    procedure GridView1GetCheckState(Sender: TObject; Cell: TGridCell;
      var CheckState: TCheckBoxState);
    procedure GridView1CheckClick(Sender: TObject; Cell: TGridCell);
  private
    FShareState: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
begin
  FShareState := 3;
  { assign an overlay index to the first image }
  ImageList1.Overlay(0, TOverlay(0));
end;

procedure TForm1.GridView1CheckClick(Sender: TObject; Cell: TGridCell);
begin
  if Cell.Col = 3 then
  begin
    FShareState := FShareState xor (1 shl Cell.Row);
    GridView1.InvalidateRow(Cell.Row);
  end;
end;

procedure TForm1.GridView1GetCellImageEx(Sender: TObject; Cell: TGridCell;
  var ImageIndex, OverlayIndex: Integer);
begin
  if Cell.Col = 0 then
  begin
    if Cell.Row = 0 then ImageIndex := 2;
    if FShareState and (1 shl Cell.Row) = 0 then OverlayIndex := 0;
  end;
end;

procedure TForm1.GridView1GetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  case Cell.Col of
    0: Value := Char(Ord('C') + Cell.Row) + '$';
    1: Value := Char(Ord('C') + Cell.Row) + ':\';
    2: Value := 'Default share';
  end;
end;

procedure TForm1.GridView1GetCheckState(Sender: TObject; Cell: TGridCell;
  var CheckState: TCheckBoxState);
begin
  if Cell.Col = 3 then
  begin
    if FShareState and (1 shl Cell.Row) <> 0 then CheckState := cbChecked;
  end;
end;

end.
