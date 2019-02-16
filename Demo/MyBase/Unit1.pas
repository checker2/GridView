unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants, System.Classes,
  Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Data.DB, Datasnap.DBClient,
  Ex_Grid, Ex_DBGrid;

type
  TForm1 = class(TForm)
    DBGridView1: TDBGridView;
    DataSource1: TDataSource;
    ClientDataSet1: TClientDataSet;
    OpenDialog1: TOpenDialog;
    procedure DBGridView1DblClick(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }
  end;

var
  Form1: TForm1;

implementation

{$R *.dfm}

procedure TForm1.DBGridView1DblClick(Sender: TObject);
begin
  if OpenDialog1.Execute then
  begin
    ClientDataSet1.LoadFromFile(OpenDialog1.FileName);
    ClientDataSet1.Active := True;
  end;
end;

end.
