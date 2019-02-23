unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Ex_Grid;

type
  TForm1 = class(TForm)
    GridView1: TGridView;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure GridView1GetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure GridView1GetSortDirection(Sender: TObject;
      Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
    procedure GridView1HeaderClick(Sender: TObject;
      Section: TGridHeaderSection);
  private
    FEnvironment: TStringList;
    FSortColumn: Integer;
    FSortDirection: TGridSortDirection;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure GetEnvironmentStrings(Strings: TStrings);
var
  Env, EnvVar: PChar;
begin
  Env := Winapi.Windows.GetEnvironmentStrings();
  if Env <> nil then
  try
    EnvVar := Env;
    while EnvVar^ <> #0 do
    begin
      Strings.Add(EnvVar);
      Inc(EnvVar, StrLen(EnvVar) + 1);
    end;
  finally
    FreeEnvironmentStrings(Env);
  end;
end;

function CompareEnvironment(List: TStringList; Index1, Index2: Integer): Integer;
var
  S1, S2: string;
begin
  if Form1.FSortColumn = 0 then
  begin
    S1 := List.Names[Index1];
    S2 := List.Names[Index2];
  end
  else
  begin
    S1 := List.ValueFromIndex[Index1];
    S2 := List.ValueFromIndex[Index2];
  end;
  Result := CompareText(S1, S2);
  if Form1.FSortDirection = gsDescending then Result := Result * -1;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FEnvironment := TStringList.Create;
  GetEnvironmentStrings(FEnvironment);
  FEnvironment.CustomSort(CompareEnvironment);
  GridView1.Rows.Count := FEnvironment.Count;
  FSortColumn := 0;
  FSortDirection := gsAscending;
end;

procedure TForm1.FormDestroy(Sender: TObject);
begin
  FEnvironment.Free;
end;

procedure TForm1.GridView1GetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);
begin
  case Cell.Col of
    0: Value := FEnvironment.Names[Cell.Row];
    1: Value := FEnvironment.ValueFromIndex[Cell.Row];
  end;
end;

procedure TForm1.GridView1GetSortDirection(Sender: TObject;
  Section: TGridHeaderSection; var SortDirection: TGridSortDirection);
begin
  if Section.ColumnIndex <> FSortColumn then
    SortDirection := gsNone
  else
    SortDirection := FSortDirection;
end;

procedure TForm1.GridView1HeaderClick(Sender: TObject;
  Section: TGridHeaderSection);
begin
  if Section.ColumnIndex <> FSortColumn then FSortColumn := Section.ColumnIndex
  else if FSortDirection = gsAscending then FSortDirection := gsDescending
  else FSortDirection := gsAscending;
  GridView1.InvalidateHeader;
  FEnvironment.CustomSort(CompareEnvironment);
  GridView1.InvalidateGrid;
end;

end.
