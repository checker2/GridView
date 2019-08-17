unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes, Vcl.Graphics,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Ex_Grid, Vcl.Menus, Vcl.ExtCtrls,
  System.ImageList, Vcl.ImgList;

type
  TForm1 = class(TForm)
    GridView1: TGridView;
    PopupMenu1: TPopupMenu;
    SizeColumnToFit1: TMenuItem;
    SizeAllColumnsToFit1: TMenuItem;
    ImageList1: TImageList;
    procedure FormCreate(Sender: TObject);
    procedure SizeColumnToFit1Click(Sender: TObject);
    procedure SizeAllColumnsToFit1Click(Sender: TObject);
    procedure GridView1HeaderDetailsClick(Sender: TObject);
    procedure GridView1GetCellText(Sender: TObject; Cell: TGridCell;
      var Value: string);
    procedure GridView1GetCellImage(Sender: TObject; Cell: TGridCell;
      var ImageIndex: Integer);
  private
    FFileList: array of TSearchRec;
    FFileCount: Integer;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

uses
  Unit2;

{$R *.dfm}

procedure TForm1.FormCreate(Sender: TObject);
var
  SearchRec: TSearchRec;
  Status: Integer;
begin
  SetLength(FFileList, 256);
  Status := FindFirst('C:\*', faAnyFile, SearchRec);
  while (Status = 0) and (FFileCount < 256) do
  begin
    if (SearchRec.Name <> '.') and (SearchRec.Name <> '..') then
    begin
      FFileList[FFileCount] := SearchRec;
      Inc(FFileCount);
    end;
    Status := FindNext(SearchRec);
  end;
  FindClose(SearchRec);
  GridView1.Rows.Count := FFileCount;
end;

procedure TForm1.GridView1GetCellImage(Sender: TObject; Cell: TGridCell;
  var ImageIndex: Integer);
begin
  if Cell.Col = 0 then
  begin
    ImageIndex := Ord(FFileList[Cell.Row].Attr and faDirectory <> 0);
    if FFileList[Cell.Row].Attr and faHidden <> 0 then Inc(ImageIndex, 2);
  end
  else
    ImageIndex := -1;
end;

procedure TForm1.GridView1GetCellText(Sender: TObject; Cell: TGridCell;
  var Value: string);

  function AttrToStr(Attr: Integer): string;
  begin
    Result := '';
    //if Attr and faHidden <> 0 then Result := Result + 'Hidden ';
    if Attr and faReadOnly <> 0 then Result := Result + 'ReadOnly ';
    Result := Trim(Result);
  end;

begin
  case Cell.Col of
    0: Value := FFileList[Cell.Row].Name;
    1: Value := DateTimeToStr(FFileList[Cell.Row].TimeStamp);
    2: Value := ExtractFileExt(FFileList[Cell.Row].Name);
    3: if FFileList[Cell.Row].Attr and faDirectory = 0 then Value := IntToStr(FFileList[Cell.Row].Size);
    4: Value := AttrToStr(FFileList[Cell.Row].Attr);
  end;
end;

procedure TForm1.GridView1HeaderDetailsClick(Sender: TObject);
begin
  ShowGridColumnsDlg(GridView1);
end;

procedure TForm1.SizeAllColumnsToFit1Click(Sender: TObject);
begin
  GridView1.SizeAllColumnsToFit;
end;

procedure TForm1.SizeColumnToFit1Click(Sender: TObject);
begin
  GridView1.SizeColumnToFit(GridView1.ContextPopupCol);
end;

end.
