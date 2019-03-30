{
  Библиотека дополнительных компонентов

  Регистрация компонента TGridView в Delphi IDE

  © Роман М. Мочалов, 1997-2019
  E-mail: checker@mail.ru

  Исправления для Delphi 6: Компанец Илья, iluha@convex.ru
}

unit Ex_RegGrid;

interface

uses
  Windows, SysUtils, Classes, Forms, Dialogs, TypInfo, DesignIntf,
  DesignEditors, WideStrings;

type

{ TGridEditor }

  TGridEditor = class(TDefaultEditor)
  private
    FCollection: TCollection;
    procedure FindCollectionEditor(const PropertyEditor: IProperty);
  protected
    procedure EditProperty(const PropertyEditor: IProperty; var Continue: Boolean); override;
    procedure ShowCollectionEditor(ACollection: TCollection);
  public
    procedure ExecuteVerb(Index: Integer); override;
    function GetVerb(Index: Integer): string; override;
    function GetVerbCount: Integer; override;
  end;

{ TGridColumnsProperty }

  TGridColumnsProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TGridHeaderProperty }

  TGridHeaderProperty = class(TClassProperty)
  public
    procedure Edit; override;
    function GetAttributes: TPropertyAttributes; override;
  end;

{ TDBGridFieldNameProperty }

  TDBGridFieldNameProperty = class(TStringProperty)
  public
    function GetAttributes: TPropertyAttributes; override;
    procedure GetValues(Proc: TGetStrProc); override;
  end;

procedure Register;

implementation

uses
  Ex_Grid, Ex_GridC, Ex_GridH, Ex_DBGrid;

{$R *.dcr}

{ TGridEditor }

procedure TGridEditor.FindCollectionEditor(const PropertyEditor: IProperty);
var
  P: PTypeInfo;
begin
  if FCollection <> nil then
  begin
    P := PropertyEditor.GetPropType;
    if (P <> nil) and (P.Kind = tkClass) and (CompareText(string(P.Name), FCollection.ClassName) = 0) then
    begin
      PropertyEditor.Edit;
      FCollection := nil;
    end;
  end;
{  if FContinue then
    EditProperty(Prop, FContinue);!!!}
end;

procedure TGridEditor.EditProperty(const PropertyEditor: IProperty; var Continue: Boolean);
begin
  if CompareText(PropertyEditor.GetName, 'ONGETCELLTEXT') = 0 then
  begin
    PropertyEditor.Edit;
    Continue := False;
  end;
end;

procedure TGridEditor.ShowCollectionEditor(ACollection: TCollection);
var
  List: IDesignerSelections;
begin
  FCollection := ACollection;
  List := TDesignerSelections.Create;
  List.Add(Self.Component);
  GetComponentProperties(List, [tkClass], Self.Designer, FindCollectionEditor);
end;

procedure TGridEditor.ExecuteVerb(Index: Integer);
begin
  case Index of
    0: ShowCollectionEditor(TCustomGridView(Component).Columns);
    1: if EditGridHeader(TCustomGridView(Component)) then Designer.Modified;
  end;
end;

function TGridEditor.GetVerb(Index: Integer): string;
begin
  case Index of
    0: Result := 'Columns Editor...';
    1: Result := 'Header Editor...';
  end;
end;

function TGridEditor.GetVerbCount: Integer;
begin
  Result := 2;
end;

{ TGridColumnsProperty }

procedure TGridColumnsProperty.Edit;
begin
  if EditGridColumns(TGridColumns(GetOrdValue).Grid) then Modified;
end;

function TGridColumnsProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TGridHeaderProperty }

procedure TGridHeaderProperty.Edit;
begin
  if EditGridHeader(TGridHeaderSections(GetOrdValue).Header.Grid) then Modified;
end;

function TGridHeaderProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paDialog, paReadOnly];
end;

{ TDBGridFieldNameProperty }

function TDBGridFieldNameProperty.GetAttributes: TPropertyAttributes;
begin
  Result := [paValueList, paSortList, paMultiSelect];
end;

procedure TDBGridFieldNameProperty.GetValues(Proc: TGetStrProc);
var
  Grid: TCustomDBGridView;
  Values: TStringList;
  I: Integer;
begin
  Grid := TDBGridColumn(GetComponent(0)).Grid;
  if (Grid <> nil) and (Grid.DataLink.DataSet <> nil) then
  begin
    Values := TStringList.Create;
    try
      Grid.DataLink.DataSet.GetFieldNames(Values);
      for I := 0 to Values.Count - 1 do Proc(Values[I]);
    finally
      Values.Free;
    end;
  end;
end;

procedure Register;
begin
  RegisterComponents('GridView', [TGridView, TDBGridView]);

  RegisterComponentEditor(TGridView, TGridEditor);
  //RegisterPropertyEditor(TypeInfo(TGridColumns), TGridView, 'Columns', TGridColumnsProperty);
  //RegisterPropertyEditor(TypeInfo(TGridHeaderSections), TGridHeader, 'Sections', TGridHeaderProperty);

  RegisterComponentEditor(TDBGridView, TGridEditor);
  RegisterPropertyEditor(TypeInfo(string), TDBGridColumn, 'FieldName', TDBGridFieldNameProperty);
  //RegisterPropertyEditor(TypeInfo(TGridHeaderSections), TDBGridHeader, 'Sections', TGridHeaderProperty);
end;

end.
