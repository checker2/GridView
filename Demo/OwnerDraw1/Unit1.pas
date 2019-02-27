unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Ex_Grid;

type
  TForm1 = class(TForm)
    GridView1: TGridView;
    procedure FormCreate(Sender: TObject);
    procedure GridView1GetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure GridView1GetCellColors(Sender: TObject; Cell: TGridCell;
      Canvas: TCanvas);
    procedure GridView1MouseMove(Sender: TObject; Shift: TShiftState; X,
      Y: Integer);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

type
  TTestRec = record
    ID: Integer;
    Summary: string;
    AssignTo: string;
    Executed: string;
    Status: Boolean;
  end;

const
  TestList: array[0..5] of TTestRec = (
    (ID: 415071; Summary: 'Verify the app can be installed'; AssignTo: 'Admin'; Executed: '24.02.2019 18:04'; Status: True),
    (ID: 415072; Summary: 'Verify the app can be launched'; AssignTo: 'Admin'; Executed: '24.02.2019 18:10'; Status: True),
    (ID: 415073; Summary: 'Verify the app can be closed'; AssignTo: 'Admin'; Executed: '24.02.2019 18:11'; Status: False),
    (ID: 415074; Summary: 'Verify the app can be upgraded'; AssignTo: 'Admin'; Executed: '24.02.2019 18:12'; Status: False),
    (ID: 415074; Summary: 'Verify the app can be repaired'; AssignTo: 'Admin'; Executed: '24.02.2019 18:14'; Status: False),
    (ID: 415075; Summary: 'Verify the app can be uninstalled'; AssignTo: 'Admin'; Executed: '24.02.2019 18:15'; Status: True));

procedure TForm1.FormCreate(Sender: TObject);
begin
  GridView1.Rows.Count := Length(TestList);
end;

procedure TForm1.GridView1GetCellColors(Sender: TObject; Cell: TGridCell;
  Canvas: TCanvas);
const
  StatusColor: array[Boolean] of TColor = (clRed, clGreen);
begin
  case Cell.Col of
    0, 2:
      begin
        Canvas.Font.Color := clHighlight;
        Canvas.Font.Style := [fsUnderline];
      end;
    4: Canvas.Font.Color := StatusColor[TestList[Cell.Row].Status];
  end;
end;

procedure TForm1.GridView1GetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
const
  SStatusStr: array[Boolean] of string = ('Failed', 'Passed');
begin
  case Cell.Col of
    0: Value := 'T' + IntToStr(TestList[Cell.Row].ID);
    1: Value := TestList[Cell.Row].Summary;
    2: Value := TestList[Cell.Row].AssignTo;
    3: Value := TestList[Cell.Row].Executed;
    4: Value := SStatusStr[TestList[Cell.Row].Status];
  end;
end;

procedure TForm1.GridView1MouseMove(Sender: TObject; Shift: TShiftState; X,
  Y: Integer);
var
  Cell: TGridCell;
begin
  Cell := GridView1.GetCellAt(X, Y);
  if Cell.Col in [0, 2] then GridView1.Cursor := crHandPoint
  else GridView1.Cursor := crDefault;
end;

end.
