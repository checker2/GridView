unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Ex_Grid;

type
  TForm1 = class(TForm)
    GridView1: TGridView;
    procedure GridView1GetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure FormCreate(Sender: TObject);
    procedure GridView1SetEditText(Sender: TObject; Cell: TGridCell;
      var Value: string);
  private
    FStrings: array of string;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
begin
  SetLength(FStrings, GridView1.Rows.Count * GridView1.Columns.Count);
end;

procedure TForm1.GridView1GetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  if Cell.Col = 0 then
    Value := IntToStr(Cell.Row)
  else
    Value := FStrings[Cell.Row * GridView1.Columns.Count + Cell.Col];
end;

procedure TForm1.GridView1SetEditText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  FStrings[Cell.Row * GridView1.Columns.Count + Cell.Col] := Value;
end;

end.
