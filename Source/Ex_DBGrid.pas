{
  TDBGridView component (data-aware grid)

  (C) Roman M. Mochalov, 1997-2019
  E-mail: checker@mail.ru

  License: MIT
}

unit Ex_DBGrid;

interface

uses
  Windows, Messages, SysUtils, CommCtrl, Classes, Controls, Graphics, Forms,
  Dialogs, StdCtrls, Math, ImgList, Ex_Grid, Db, DBCtrls, Types, UITypes;

const
  DBGRID_BOF = -MaxInt;
  DBGRID_EOF = MaxInt;

type
  TCustomDBGridView = class;

{ TDBGridHeader }

  TDBGridHeader = class(TCustomGridHeader)
  published
    property AutoHeight;
    property Color;
    property Images;
    property Flat;
    property Font;
    property GridColor;
    property GridFont;
    property PopupMenu;
    property Sections;
    property SectionHeight;
  end;

{ TDBGridColumn }

  {
    TDBGridColumn represents the data binding of a column in the grid.
    Automatically gets the name, alignment, mask and type of inplace edit,
    maximum length, width and readonly state from the specified field.

    Methods:

    RestoreDefaults - Restores the column's default settings.

    Properties:

    DefaultColumn -   Indicates whether a default field properties are used
                      for the column properties.
    Field -           Indicates the TField instance represented by the column.
    FieldName -       Indicates the name of the field represented by the column.
  }

  TDBGridColumn = class(TCustomGridColumn)
  private
    FField: TField;
    FFieldName: string;
    FDefaultColumn: Boolean;
    function GetGrid: TCustomDBGridView;
    function IsNondefaultColumn: Boolean;
    function GetField: TField;
    procedure SetDefaultColumn(Value: Boolean);
    procedure SetField(Value: TField);
    procedure SetFieldName(const Value: string);
  protected
    function GetDisplayName: string; override;
    procedure SetAlignment(Value: TAlignment); override;
    procedure SetCaption(const Value: string); override;
    procedure SetEditMask(const Value: string); override;
    procedure SetEditStyle(Value: TGRidEditStyle); override;
    procedure SetMaxLength(Value: Integer); override;
    procedure SetReadOnly(Value: Boolean); override;
    procedure SetWidth(Value: Integer); override;
  public
    constructor Create(Collection: TCollection); override;
    procedure Assign(Source: TPersistent); override;
    procedure RestoreDefaults; virtual;
    property Field: TField read GetField write SetField;
    property Grid: TCustomDBGridView read GetGrid;
  published
    property AlignEdit;
    property Alignment stored IsNondefaultColumn;
    property AllowClick;
    property Caption stored IsNondefaultColumn;
    property CheckKind;
    property DefaultPopup;
    property EditMask stored IsNondefaultColumn;
    property EditStyle stored IsNondefaultColumn;
    property EditWordWrap;
    property FixedSize default False;
    property MaxLength stored IsNondefaultColumn;
    property MaxWidth;
    property MinWidth;
    property PickList;
    property ReadOnly stored IsNondefaultColumn;
    property TabStop;
    property Tag;
    property Visible default True;
    property WantReturns;
    property Width default 64;
    property WordWrap;
    property DefWidth stored IsNondefaultColumn;
    property FieldName: string read FFieldName write SetFieldName stored True;
    property DefaultColumn: Boolean read FDefaultColumn write SetDefaultColumn default True;
  end;

{ TDBGridColumns }

  TDBGridColumns = class(TGridColumns)
  private
    function GetColumn(Index: Integer): TDBGridColumn;
    function GetGrid: TCustomDBGridView;
    procedure SetColumn(Index: Integer; Value: TDBGridColumn);
  protected
    procedure Update(Item: TCollectionItem); override;
  public
    function Add: TDBGridColumn;
    property Columns[Index: Integer]: TDBGridColumn read GetColumn write SetColumn; default;
    property Grid: TCustomDBGridView read GetGrid;
  end;

{ TDBGridRows }

  TDBGridRows = class(TCustomGridRows)
  private
    FRowsFromGrid: Integer;
    function GetGrid: TCustomDBGridView;
  protected
    procedure Change; override;
    procedure SetCount(Value: Integer); override;
  public
    property Grid: TCustomDBGridView read GetGrid;
  published
    property AutoHeight;
    property Height;
  end;

{ TDBGridFixed }

  TDBGridFixed = class(TCustomGridFixed)
  private
    FDefCount: Integer;
    function GetGrid: TCustomDBGridView;
    procedure SetDefCount(Value: Integer);
  public
    property Grid: TCustomDBGridView read GetGrid;
  published
    property Color;
    property Count: Integer read FDefCount write SetDefCount default 0;
    property Flat;
    property Font;
    property GridColor;
    property GridFont;
    property ShowDivider;
  end;

{ TDBGridScrollBar }

  TDBGridScrollBar = class(TGridScrollBar)
  private
    FRowMin: Integer;
    FRowMax: Integer;
    function GetGrid: TCustomDBGridView;
  protected
    procedure ScrollMessage(var Message: TWMScroll); override;
    procedure SetParams(AMin, AMax, APageStep, ALineStep: Integer); override;
    procedure SetPositionEx(Value: Integer; ScrollCode: Integer); override;
    procedure Update; override;
  public
    property Grid: TCustomDBGridView read GetGrid;
  end;

{ TDBGridEdit }

  TDBGridListBox = class(TPopupDataList)
  private
    FLookupSource: TDataSource;
  public
    constructor Create(AOwner: TComponent); override;
    property LookupSource: TDataSource read FLookupSource;
  end;

  TDBGridEdit = class(TCustomGridEdit)
  private
    FDataList: TDBGridListBox;
    function GetGrid: TCustomDBGridView;
    procedure WMKillFocus(var Message: TWMSetFocus); message WM_KILLFOCUS;
  protected
    function GetDropList: TWinControl; override;
    procedure UpdateListBounds; override;
    procedure UpdateListItems; override;
    procedure UpdateListValue(Accept: Boolean); override;
    procedure UpdateStyle; override;
  public
    procedure CloseUp(Accept: Boolean); override;
    property Grid: TCustomDBGridView read GetGrid;
  end;

{ TDBGridDataLink }

  TDBGridDataLink = class(TDataLink)
  private
    FGrid: TCustomDBGridView;
    FModified: Boolean;
    FInUpdateData: Integer;
  protected
    procedure ActiveChanged; override;
    procedure DataSetChanged; override;
    procedure DataSetScrolled(Distance: Integer); override;
    procedure EditingChanged; override;
    procedure FocusControl(Field: TFieldRef); override;
    function GetActiveRecord: Integer; override;
    procedure LayoutChanged; override;
    procedure RecordChanged(Field: TField); override;
    procedure SetActiveRecord(Value: Integer); override;
    procedure UpdateData; override;
    property Grid: TCustomDBGridView read FGrid;
    property InUpdateData: Integer read FInUpdateData;
  public
    constructor Create(AGrid: TCustomDBGridView);
    procedure Modified;
    function MoveBy(Distance: Integer): Integer; override;
    procedure Reset;
  end;

{ TDBGridSelectedRows }

  TDBGridSelectedRows = class(TStringList)
  private
    FGrid: TCustomDBGridView;
    FCache: string;
    FCacheIndex: Integer;
    FCacheFind: Boolean;
    FSelectionMark: string;
    FSelecting: Boolean;
    function GetBookmark(Index: Integer): TBookmark;
    function GetCurrentRow: string;
    function GetCurrentRowSelected: Boolean;
    procedure SetCurrentRowSelected(Value: Boolean);
  protected
    procedure Changed; override;
    function CompareStrings(const S1, S2: string): Integer; override;
  public
    constructor Create(AGrid: TCustomDBGridView);
    procedure Clear; override;
    function Compare(const S1, S2: string): Integer;
    function Find(const S: string; var Index: Integer): Boolean; override;
    function IndexOf(const S: string): Integer; override;
    procedure UpdateSelectionMark;
    property Bookmarks[Index: Integer]: TBookmark read GetBookmark; default;
    property CurrentRow: string read GetCurrentRow;
    property CurrentRowSelected: Boolean read GetCurrentRowSelected write SetCurrentRowSelected;
    property SelectionMark: string read FSelectionMark;
    property Selecting: Boolean read FSelecting;
  end;

{ TCustomDBGridView }

  {
    TDBGridView is a data-aware grid to display and edit the records from a
    data sets.

    Methods:

    ApplyEdit -          Writes text from the inplace editor to the active field
                         and hides the inplace editor.
    CancelEdit -         Cancels modifications to the active field and hides
                         the inplace editor.
    ChangeEditText -     Use ChangeEditText to manually change the inplace editor
                         text to the value selected by clicking the ellipsis
                         button.
    ClearSelection -     Deselects all selected rows in the grid.
    Delete -             Deletes the active record.
    Insert -             Inserts a new, empty record in the dataset.
    LockLayout -         Increments the LayoutLock property when the column
                         bindings or fields in the dataset change.
    SelectAll -          Selects all rows in the grid.
    UnLockLayout -       Decrements the LayoutLock property and recomputes the
                         Columns property if LayoutLock reaches 0.

    Properties:

    AllowInsertRecord -  Determines whether a user can insert records in the grid.
    AllowDeleteRecord -  Determines whether a user can delete records in the grid.
    CancelOnDeactivate - When the user exits the grid from an inserted record to
                         which the user made no modifications, the inserted record
                         is not posted to the dataset. This prevents the
                         inadvertent posting of empty records.
    DataSource -         Identifies the link to the dataset where the data-aware
                         grid finds its data.
    DefaultLayout -      Determines whether the grid uses the default column
                         layout defined by the list of dataset fields.
    EditColumn -         Specifies the column for inplace editor of currently
                         selected cell in the grid.
    EditField -          Specifies the field component for inplace editor of
                         currently selected cell in the grid.
    IndicatorImages -    Determines which image list is associated with the
                         indicator. The list should contain the following
                         images:
                           0 - The current row.
                           1 - The row that is being edited.
                           2 - The row that is inserted.
                           3 - The row that is multiselected.
                           4 - Reserved.
    IndicatorWidth -     Specifies the width of the indicator column. Set 0 to
                         automatically set the column width to the size of the
                         scroller button.
    LayoutLock -         Counts the number of times LockLayout has been called
                         without an UnlockLayout call.
    MultiSelect -        Determines whether the user can select more than one
                         rows at a time.
    SelectedField -      Specifies the field component for the currently
                         selected cell in the grid.
    SelectedRows -       Specifies a set of bookmarks for all the records in the
                         dataset that correspond to rows selected in the grid.
    ShowIndicator -      Indicates whether to show the indicator column.

    Events:

    OnDataChange -       Occurs when the dataset is changed.
    OnDataDeleteRecord - Occurs before delete the current record by pressing
                         the DELETE key.
    OnDataEditError -    Occurs when the user attempts to modify field or
                         insert a record and an exception is raised.
    OnDataInsertRecord - Occurs before inserting a record by pressing the
                         INSERT key on any record or pressing the DOWN key
                         on the last record.
    OnDataUpdateError -  Occurs when the user attempts to update a field
                         value and an exception is raised.
    OnDataUpdateField -  Occurs after the user has updated the field value by
                         entering text in the inplace editor or by selecting
                         a lookup list item.
    OnSetFieldText -     Occurs when the grid needs to write new text from the
                         inplace editor to the field in the dataset. To set
                         field value within event use the Columns[Cell.Col].Field
                         property.
  }

  TDBGridDataAction = (gdaFail, gdaAbort);

  TDBGridDataErrorEvent = procedure(Sender: TObject; E: Exception; var Action: TDBGridDataAction) of object;
  TDBGridDataInsertEvent = procedure(Sender: TObject; var AllowInsert: Boolean) of object;
  TDBGridDataDeleteEvent = procedure(Sender: TObject; var AllowDelete: Boolean) of object;
  TDBGridDataUpdateEvent = procedure(Sender: TObject; Field: TField) of object;
  TDBGridTextEvent = procedure(Sender: TObject; Field: TField; const Text: string) of object;

  TDBGridIndicatorImageEvent = procedure(Sender: TObject; DataRow: Integer; var ImageIndex: Integer) of object;

  TCustomDBGridView = class(TCustomGridView)
  private
    FDataLink: TDBGridDataLink;
    FDefaultLayout: Boolean;
    FShowIndicator: Boolean;
    FIndicatorImages: TImageList;
    FIndicatorsLink: TChangeLink;
    FIndicatorsDef: TImageList;
    FIndicatorWidth: Integer;
    FAllowInsertRecord: Boolean;
    FAllowDeleteRecord: Boolean;
    FLayoutLock: Integer;
    FScrollLock: Integer;
    FScrollCell: TGridCell;
    FScrollSelected: Boolean;
    FCursorFromDataSet: Integer;
    FFieldText: string;
    FMultiSelect: Boolean;
    FSelectedRows: TDBGridSelectedRows;
    FCancelOnDeactivate: Boolean;
    FOnDataChange: TNotifyEvent;
    FOnDataEditError: TDBGridDataErrorEvent;
    FOnDataUpdateError: TDBGridDataErrorEvent;
    FOnDataInsertRecord: TDBGridDataInsertEvent;
    FOnDataDeleteRecord: TDBGridDataDeleteEvent;
    FOnDataUpdateField: TDBGridDataUpdateEvent;
    FOnSetFieldText: TDBGridTextEvent;
    FOnGetIndicatorImage: TDBGridIndicatorImageEvent;
    function GetCol: Longint;
    function GetColumns: TDBGridColumns;
    function GetEditColumn: TDBGridColumn;
    function GetEditField: TField;
    function GetFixed: TDBGridFixed;
    function GetHeader: TDBGridHeader;
    function GetRows: TDBGridRows;
    function GetSelectedField: TField;
    function GetSelectedRows: TDBGridSelectedRows;
    procedure IndicatorsChange(Sender: TObject);
    function IsColumnsStored: Boolean;
    procedure SetCol(Value: Longint);
    procedure SetColumns(Value: TDBGridColumns);
    procedure SetDataSource(Value: TDataSource);
    procedure SetDefaultLayout(Value: Boolean);
    procedure SetFixed(Value: TDBGridFixed);
    procedure SetHeader(Value: TDBGridHeader);
    procedure SetIndicatorImages(Value: TImageList);
    procedure SetIndicatorWidth(Value: Integer);
    procedure SetMultiSelect(Value: Boolean);
    procedure SetRows(Value: TDBGridRows);
    procedure SetSelectedField(Value: TField);
    procedure SetShowIndicator(Value: Boolean);
    procedure ReadColumns(Reader: TReader);
    procedure WriteColumns(Writer: TWriter);
    procedure CMExit(var Message: TMessage); message CM_EXIT;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
  protected
    FSelectPending: Boolean;
    FSelectPos: TPoint;
    FSelectShift: TShiftState;
    function AcquireLockLayout: Boolean;
    procedure CancelOrUpdateData;
    procedure ChangeIndicator; virtual;
    procedure ChangeScale(M, D: Integer); override;
    function CreateColumns: TGridColumns; override;
    function CreateDataLink: TDBGridDataLink; virtual;
    function CreateFixed: TCustomGridFixed; override;
    function CreateHeader: TCustomGridHeader; override;
    function CreateRows: TCustomGridRows; override;
    function CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar; override;
    procedure DataEditError(E: Exception; var Action: TDBGridDataAction); virtual;
    procedure DataFieldUpdated(Field: TField); virtual;
    procedure DataLayoutChanged; virtual;
    procedure DataLinkActivate(Active: Boolean); virtual;
    procedure DataRecordChanged(Field: TField); virtual;
    procedure DataSetChanged; virtual;
    procedure DataSetScrolled(Distance: Integer); virtual;
    procedure DataUpdateError(E: Exception; var Action: TDBGridDataAction); virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean; override;
    function EditCanModify(Cell: TGridCell): Boolean; override;
    function EditCanShow(Cell: TGridCell): Boolean; override;
    procedure GetCellColors(Cell: TGridCell; Canvas: TCanvas); override;
    function GetCellText(Cell: TGridCell): string; override;
    function GetColumnClass: TGridColumnClass; override;
    function GetDataSource: TDataSource; virtual;
    function GetEditClass(Cell: TGridCell): TGridEditClass; override;
    function GetEditText(Cell: TGridCell): string; override;
    procedure HideCursor; override;
    procedure InvalidateIndicator;
    procedure InvalidateIndicatorImage(DataRow: Integer);
    procedure InvalidateSelected;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MoveBy(Distance: Integer; Shift: TShiftState);
    procedure MoveByXY(X, Y: Integer; Shift: TShiftState);
    procedure MoveTo(RecNo: Integer; Shift: TShiftState);
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure PaintCell(Cell: TGridCell; Rect: TRect); override;
    procedure PaintIndicatorFixed; virtual;
    procedure PaintIndicatorGridLines; virtual;
    procedure PaintIndicatorHeader; virtual;
    procedure PaintIndicatorImage(Rect: TRect; DataRow: Integer); virtual;
    procedure SetEditText(Cell: TGridCell; var Value: string); override;
    procedure SetFieldText(Field: TField; const Text: string); virtual;
    procedure Resize; override;
    procedure ShowCursor; override;
    procedure UpdateData; virtual;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure ApplyEdit; override;
    procedure CancelEdit; override;
    procedure ChangeEditText(const S: string); virtual;
    procedure ClearSelection;
    procedure Delete; virtual;
    function FindText(const FindText: string; Options: TFindOptions): Boolean; override;
    function GetGridRect: TRect; override;
    function GetHeaderRect: TRect; override;
    function GetIndicatorHeaderRect: TRect; virtual;
    function GetIndicatorFixedRect: TRect; virtual;
    function GetIndicatorImage(DataRow: Integer): Integer; virtual;
    function GetIndicatorImageRect(DataRow: Integer): TRect; virtual;
    function GetIndicatorWidth: Integer;
    procedure InvalidateGrid; override;
    procedure InvalidateRow(Row: Integer); override;
    procedure Insert(AppendMode: Boolean); virtual;
    function IsCellReadOnly(Cell: TGridCell): Boolean; override;
    function IsEvenRow(Cell: TGridCell): Boolean; override;
    function IsRowHighlighted(Row: Integer): Boolean; override;
    function IsRowMultiselected(Row: Integer): Boolean;
    procedure LockLayout;
    procedure LockScroll;
    procedure MakeCellVisible(Cell: TGridCell; PartialOK: Boolean); override;
    procedure ResetEdit; override;
    procedure SelectAll;
    procedure SetCursor(Cell: TGridCell; Selected, Visible: Boolean); override;
    procedure UnLockLayout(CancelChanges: Boolean);
    procedure UnLockScroll(CancelScroll: Boolean);
    procedure UpdateCursorPos(ShowCursor: Boolean); virtual;
    procedure UpdateEditText; override;
    procedure UpdateLayout; virtual;
    procedure UpdateRowCount; virtual;
    procedure UpdateSelection(var Cell: TGridCell; var Selected: Boolean); override;
    property AllowDeleteRecord: Boolean read FAllowDeleteRecord write FAllowDeleteRecord default True;
    property AllowEdit default True;
    property AllowInsertRecord: Boolean read FAllowInsertRecord write FAllowInsertRecord default True;
    property CancelOnDeactivate: Boolean read FCancelOnDeactivate write FCancelOnDeactivate default False;
    property Col: Longint read GetCol write SetCol;
    property ColumnClick default False;
    property Columns: TDBGridColumns read GetColumns write SetColumns stored IsColumnsStored;
    property CursorKeys default [gkArrows, gkTabs, gkMouse, gkMouseWheel];
    property CursorLock: Integer read FScrollLock;
    property DataLink: TDBGridDataLink read FDataLink;
    property DataSource: TDataSource read GetDataSource write SetDataSource;
    property DefaultLayout: Boolean read FDefaultLayout write SetDefaultLayout default True;
    property Header: TDBGridHeader read GetHeader write SetHeader;
    property Fixed: TDBGridFixed read GetFixed write SetFixed; { <- must be after Header !!! }
    property LayoutLock: Integer read FLayoutLock;
    property EditColumn: TDBGridColumn read GetEditColumn;
    property EditField: TField read GetEditField;
    property IndicatorImages: TImageList read FIndicatorImages write SetIndicatorImages;
    property IndicatorWidth: Integer read FIndicatorWidth write SetIndicatorWidth default 0;
    property MultiSelect: Boolean read FMultiSelect write SetMultiSelect default False;
    property Rows: TDBGridRows read GetRows write SetRows;
    property SelectedField: TField read GetSelectedField write SetSelectedField;
    property SelectedRows: TDBGridSelectedRows read GetSelectedRows;
    property ShowIndicator: Boolean read FShowIndicator write SetShowIndicator default True;
    property OnDataChange: TNotifyEvent read FOnDataChange write FOnDataChange;
    property OnDataDeleteRecord: TDBGridDataDeleteEvent read FOnDataDeleteRecord write FOnDataDeleteRecord;
    property OnDataInsertRecord: TDBGridDataInsertEvent read FOnDataInsertRecord write FOnDataInsertRecord;
    property OnDataEditError: TDBGridDataErrorEvent read FOnDataEditError write FOnDataEditError;
    property OnDataUpdateError: TDBGridDataErrorEvent read FOnDataUpdateError write FOnDataUpdateError;
    property OnDataUpdateField: TDBGridDataUpdateEvent read FOnDataUpdateField write FOnDataUpdateField;
    property OnGetIndicatorImage: TDBGridIndicatorImageEvent read FOnGetIndicatorImage write FOnGetIndicatorImage;
    property OnSetFieldText: TDBGridTextEvent read FOnSetFieldText write FOnSetFieldText;
  end;

  TDBGridView = class(TCustomDBGridView)
  published
    property Align;
    property AllowDeleteRecord;
    property AllowEdit;
    property AllowInsertRecord;
    property AllowSelect;
    property AlwaysEdit;
    property AlwaysSelected;
    property Anchors;
    property BorderStyle;
    property CancelOnExit;
    property CancelOnDeactivate;
    property CheckBoxes;
    property CheckStyle;
    property Color;
    property ColumnClick;
    property Columns;
    property ColumnsFullDrag;
    property Constraints;
    property Ctl3D;
    property CursorKeys;
    property DataSource;
    property DefaultEditMenu;
    property DefaultHeaderMenu;
    property DefaultLayout;
    property DragCursor;
    property DragMode;
    property DoubleBuffered default True;
    property Enabled;
    property EndEllipsis;
    property Fixed;
    property FlatBorder;
    property FocusOnScroll;
    property Font;
    property GrayReadOnly;
    property GridColor;
    property GridHint;
    property GridHintColor;
    property GridLines;
    property GridStyle;
    property Header;
    property HideSelection;
    property HighlightEvenRows;
    property HighlightFocusCol;
    property HighlightFocusRow;
    property Hint;
    property HorzScrollBar;
    property ImageIndexDef;
    property ImageHighlight;
    property Images;
    property IndicatorImages;
    property IndicatorWidth;
    property MultiSelect;
    property ParentColor default False;
    property ParentCtl3D;
    property ParentFont;
    property ParentShowHint;
    property PopupMenu;
    property ReadOnly;
    property RightClickSelect;
    property Rows;
    property RowSelect;
    property ShowCellTips;
    property ShowIndicator;
    property ShowFocusRect;
    property ShowGridHint;
    property ShowHeader;
    property ShowHint;
    property TabOrder;
    property TabStop default True;
    property VertScrollBar;
    property Visible;
    property OnCellAcceptCursor;
    property OnCellClick;
    property OnCellTips;
    property OnChange;
    property OnChangeColumns;
    property OnChangeEditing;
    property OnChangeEditMode;
    property OnChangeFixed;
    property OnChangeRows;
    property OnChanging;
    property OnCheckClick;
    property OnClick;
    property OnColumnAutoSize;
    property OnColumnResizing;
    property OnColumnResize;
    property OnContextPopup;
    property OnDblClick;
    property OnDataChange;
    property OnDataDeleteRecord;
    property OnDataEditError;
    property OnDataInsertRecord;
    property OnDataUpdateField;
    property OnDataUpdateError;
    property OnDragDrop;
    property OnDragOver;
    property OnDraw;
    property OnDrawCell;
    property OnDrawHeader;
    property OnEditAcceptKey;
    property OnEditButtonPress;
    property OnEditCanceled;
    property OnEditCanModify;
    property OnEditCanShow;
    property OnEditChange;
    property OnEditCloseUp;
    property OnEditCloseUpEx;
    property OnEditSelectNext;
    property OnEndDrag;
    property OnEnter;
    property OnExit;
    property OnGetCellColors;
    property OnGetCellImage;
    property OnGetCellImageEx;
    property OnGetCellImageIndent;
    property OnGetCellHintRect;
    property OnGetCellReadOnly;
    property OnGetCellText;
    property OnGetCellTextIndent;
    property OnGetCheckAlignment;
    property OnGetCheckImage;
    property OnGetCheckIndent;
    property OnGetCheckKind;
    property OnGetCheckState;
    property OnGetCheckStateEx;
    property OnGetEditList;
    property OnGetEditListBounds;
    property OnGetEditListIndex;
    property OnGetEditMask;
    property OnGetEditStyle;
    property OnGetEditText;
    property OnGetGridHint;
    property OnGetGridColor;
    property OnGetHeaderColors;
    property OnGetHeaderImage;
    property OnGetIndicatorImage;
    property OnGetSortDirection;
    property OnGetSortImage;
    property OnGetTipsRect;
    property OnGetTipsText;
    property OnHeaderClick;
    property OnHeaderClicking;
    property OnHeaderDetailsClick;
    property OnMouseDown;
    property OnMouseMove;
    property OnMouseUp;
    property OnMouseWheelDown;
    property OnMouseWheelUp;
    property OnKeyDown;
    property OnKeyPress;
    property OnKeyUp;
    property OnResize;
    property OnSetEditText;
    property OnSetFieldText;
    property OnStartDrag;
  end;

{ Bookmark utilities }

function StrToBookmark(const S: string): TBookmark;
function BookmarkToStr(const Bookmark: TBookmark): string;

implementation

uses
  Themes, DBConsts;

{$R *.RES}

{ TDBGridColumn }

constructor TDBGridColumn.Create(Collection: TCollection);
begin
  inherited;
  FDefaultColumn := True;
end;

procedure TDBGridColumn.Assign(Source: TPersistent);
begin
  if Source is TDBGridColumn then
  begin
    FieldName := TDBGridColumn(Source).FieldName;
    DefaultColumn := TDBGridColumn(Source).DefaultColumn;
  end;
  inherited Assign(Source);
end;

function TDBGridColumn.IsNondefaultColumn: Boolean;
begin
  Result := not DefaultColumn;
end;

function TDBGridColumn.GetField: TField;
begin
  if (FField = nil) and (Length(FFieldName) > 0) then
    if Assigned(Grid) and Assigned(Grid.DataLink.DataSet) then
      with Grid.Datalink.Dataset do
        if Active or (lcPersistent in Fields.LifeCycles) then
          SetField(FindField(FFieldName));
  Result := FField;
end;

function TDBGridColumn.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridColumn.SetDefaultColumn(Value: Boolean);
begin
  if FDefaultColumn <> Value then
  begin
    if Value then RestoreDefaults;
    FDefaultColumn := Value;
  end;
end;

procedure TDBGridColumn.SetField(Value: TField);
begin
  if FField <> Value then
  begin
    if (FField <> nil) and (Grid <> nil) then FField.RemoveFreeNotification(Grid);
    FField := Value;
    if FField <> nil then
    begin
      if Grid <> nil then FField.FreeNotification(Grid);
      FFieldName := FField.FullName;
    end;
  end;
end;

procedure TDBGridColumn.SetFieldName(const Value: string);
var
  AField: TField;
begin
  AField := nil;
  if Length(Value) > 0 then
    if Assigned(Grid) and (not (csLoading in Grid.ComponentState)) then
      if Assigned(Grid.DataLink.DataSet) then
        AField := Grid.DataLink.DataSet.FindField(Value);
  FFieldName := Value;
  SetField(AField);
  if FDefaultColumn then
  begin
    RestoreDefaults;
    FDefaultColumn := True;
  end;
  Changed(False);
end;

function TDBGridColumn.GetDisplayName: string;
begin
  Result := FFieldName;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TDBGridColumn.SetAlignment(Value: TAlignment);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetCaption(const Value: string);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetEditMask(const Value: string);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetEditStyle(Value: TGRidEditStyle);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetMaxLength(Value: Integer);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetReadOnly(Value: Boolean);
begin
  FDefaultColumn := False;
  inherited;
end;

procedure TDBGridColumn.SetWidth(Value: Integer);
begin
  if FWidthLock = 0 then FDefaultColumn := False;
  inherited;
end;

function IsLookupField(Field: TField; var MasterField: TField): Boolean; overload;
begin
  if (Field <> nil) and (Field.FieldKind = fkLookup) and (Field.DataSet <> nil) then
  begin
    { restriction: multiple key fields are not supported }
    MasterField := Field.DataSet.FindField(Field.KeyFields);
    Result := (MasterField <> nil) and MasterField.CanModify;
  end
  else
    Result := False;
end;

function IsLookupField(Field: TField): Boolean; overload;
var
  Dummy: TField;
begin
  Result := IsLookupField(Field, Dummy);
end;

function IsReadOnlyField(Field: TField): Boolean;
const
  fkReadOnly = [fkLookup, fkCalculated];
begin
  Result := (Field = nil) or Field.ReadOnly or (Field.FieldKind in fkReadOnly) or
    ((Field.DataType in ftNonTextTypes) and (not Assigned(Field.OnSetText)));
end;

procedure TDBGridColumn.RestoreDefaults;
var
  R: TRect;

  function AllowLookup: Boolean;
  begin
    Result := IsLookupField(Field) and (Grid <> nil) and
      Grid.DataLink.Active and (not Grid.Datalink.ReadOnly);
  end;

begin
  if Field <> nil then
  begin
    Alignment := Field.Alignment;
    Caption := Field.DisplayLabel;
    EditMask := Field.EditMask;
    Visible := Field.Visible;
    { the geEllipsis inplace editor style is only allowed in a custom layout,
      so no need to change it }
    if EditStyle <> geEllipsis then
      if AllowLookup then
        EditStyle := geDataList
      else if PickListCount > 0 then
        EditStyle := gePickList
      else
        EditStyle := geSimple;
    ReadOnly := IsReadOnlyField(Field);
    MaxLength := 0;
    if Field.DataType in [ftString, ftWideString] then MaxLength := Field.Size;
    { like DBGrid, the default column width is determined by the width
      of the field name }
    if Grid <> nil then
    begin
      Grid.GetCellColors(GridCell(Self.Index, 0), Grid.Canvas);
      Width := Grid.GetFontWidth(Grid.Canvas.Font, Field.DisplayWidth);
      with Grid do
        R := GetTextRect(Canvas, Rect(0, 0, 0, 0), TextLeftIndent, TextTopIndent,
          Self.Alignment, False, False, Self.Caption);
      Width := MaxIntValue([DefWidth, R.Right - R.Left]);
    end;
  end;
end;

{ TDBGridColumns }

function TDBGridColumns.GetColumn(Index: Integer): TDBGridColumn;
begin
  Result := TDBGridColumn(inherited GetItem(Index));
end;

function TDBGridColumns.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridColumns.SetColumn(Index: Integer; Value: TDBGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TDBGridColumns.Update(Item: TCollectionItem);
begin
  if (Grid <> nil) and (Grid.LayoutLock = 0) {and (not Grid.ColResizing) }then Grid.DefaultLayout := False;
  inherited;
end;

function TDBGridColumns.Add: TDBGridColumn;
begin
  Result := TDBGridColumn(inherited Add);
end;

{ TDBGridListBox }

constructor TDBGridListBox.Create(AOwner: TComponent);
begin
  inherited;
  FLookupSource := TDataSource.Create(Self);
end;

{ TDBGridEdit }

function TDBGridEdit.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridEdit.CloseUp(Accept: Boolean);
begin
  if DropListVisible and (ActiveList <> nil) and (ActiveList is TDBGridListBox) then
    with TDBGridListBox(ActiveList) do
    begin
      ListSource := nil;
      LookupSource.DataSet := nil;
    end;
  inherited;
end;

function TDBGridEdit.GetDropList: TWinControl;
begin
  if (EditStyle = geDataList) and (Grid <> nil) and IsLookupField(Grid.EditField) then
  begin
    if FDataList = nil then FDataList := TDBGridListBox.Create(Self);
    Result := FDataList;
  end
  else
    Result := inherited GetDropList;
end;

procedure TDBGridEdit.UpdateListBounds;
var
  I, ItemHeight: Integer;
  R, Rect: TRect;
  P: TPoint;
  Monitor: TMonitor;
begin
  inherited;
  if (ActiveList <> nil) and (ActiveList is TDBGridListBox) then
    with TDBGridListBox(ActiveList) do
    begin
      if Self.DropDownCount <> 0 then
        I := Self.DropDownCount
      else
      begin
        Canvas.Font := Font;
        ItemHeight := Canvas.TextHeight('Wq');
        if ItemHeight <> 0 then
        begin
          R := Self.ClientRect;
          P := Self.ClientOrigin;
          OffsetRect(R, P.X, P.Y);
          Monitor := Screen.MonitorFromRect(R);
          if Monitor <> nil then
            Rect := Monitor.WorkareaRect
          else
            Rect := Screen.WorkareaRect;
          I := ((Rect.Bottom - Rect.Top) div 3) div ItemHeight;
          if (ListSource <> nil) and (ListSource.DataSet <> nil) and
            ListSource.DataSet.Active and (I > ListSource.DataSet.RecordCount) then
            I := ListSource.DataSet.RecordCount;
        end
        else
          I := 0;
      end;
      if I < 7 then I := 7;
      RowCount := I;
    end;
end;

procedure TDBGridEdit.UpdateListItems;
var
  Field: TField;
  ListBox: TDBGridListBox;
begin
  if (ActiveList = nil) or (not (ActiveList is TDBGridListBox)) then
  begin
    inherited;
    Exit;
  end;
  if (Grid = nil) or (Grid.EditField = nil) then Exit;
  { setup lookup list }
  Field := Grid.EditField;
  ListBox := TDBGridListBox(ActiveList);
  ListBox.ListSource := nil; // <- to avoid the exception in the next line
  ListBox.LookupSource.DataSet := Field.LookupDataSet;
  ListBox.KeyField := Field.LookupKeyFields;
  ListBox.ListField := Field.LookupResultField;
  ListBox.ListSource := ListBox.LookupSource;
  ListBox.KeyValue := Field.DataSet.FieldByName(Field.KeyFields).Value;
end;

procedure TDBGridEdit.UpdateListValue(Accept: Boolean);
var
  ListValue: Variant;
  MasterField: TField;
begin
  if (ActiveList <> nil) and Accept and (Grid <> nil) then
  begin
    if ActiveList is TDBGridListBox then
      with TDBGridListBox(ActiveList) do
      begin
        ListValue := KeyValue;
        ListSource := nil;
        LookupSource.DataSet := nil;
        if IsLookupField(Grid.EditField, MasterField) and Grid.DataLink.Edit then
        begin
          MasterField.Value := ListValue;
          Grid.EditField.Value := SelectedItem;
        end;
      end
    else if ActiveList is TGridListBox then
      if EditCanModify then
      begin
        inherited;
        Grid.DataLink.Modified;
      end;
  end
  else
    inherited;
end;

procedure TDBGridEdit.UpdateStyle;
begin
  inherited UpdateStyle;
  { remove the lookup list button and the ellipsis button from the inplace
    editor for cells that cannot be modified }
  if (EditStyle <> geSimple) and (Grid <> nil) then
    if (not Grid.DataLink.Active) or Grid.DataLink.ReadOnly then
      EditStyle := geSimple
    else if (Grid.DataLink.DataSet <> nil) and (not Grid.DataLink.DataSet.CanModify) then
      EditStyle := geSimple
    else if EditStyle = geDataList then
    begin
      if not IsLookupField(Grid.EditField) then EditStyle := geSimple;
    end
    else if Grid.IsCellReadOnly(Grid.EditCell) then
      EditStyle := geSimple;
end;

procedure TDBGridEdit.WMKillFocus(var Message: TWMSetFocus);
begin
  inherited;
  { if the focus is lost, except when the dialog box opens when user click
    the ellipsis button, try to finish editing the cell and enter data from
    the inplace editor to the data set }
  if not (ClosingUp or Pressing) then
    if (Grid <> nil) and Grid.CancelOnDeactivate and IsWindowVisible(Handle) then
    try
      Grid.CancelOrUpdateData;
    except
      Application.HandleException(Self);
    end;
end;

{ TDBGridRows }

function TDBGridRows.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridRows.Change;
begin
  if Grid <> nil then Grid.UpdateRowCount;
  inherited;
end;

procedure TDBGridRows.SetCount(Value: Integer);
begin
  { only the grid can change the number of rows }
  if FRowsFromGrid <> 0 then inherited;
end;

{ TDBGridFixed }

function TDBGridFixed.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridFixed.SetDefCount(Value: Integer);
begin
  FDefCount := Value;
  SetCount(Value);
end;

{ TDBGridScrollBar }

function TDBGridScrollBar.GetGrid: TCustomDBGridView;
begin
  Result := TCustomDBGridView(inherited Grid);
end;

procedure TDBGridScrollBar.ScrollMessage(var Message: TWMScroll);
var
  ScrollInfo: TScrollInfo;
  DataSet: TDataSet;
  PageStep: Integer;
  Shift: TShiftState;

  procedure DoThumbPos(Pos: Integer);
  begin
    if DataSet.IsSequenced then
    begin
      if Pos <= 1 then Grid.MoveBy(DBGRID_BOF, Shift)
      else if Pos >= DataSet.RecordCount then Grid.MoveBy(DBGRID_EOF, Shift)
      else Grid.MoveTo(Pos, Shift);
    end
    else
      case Pos of
        0: Grid.MoveBy(DBGRID_BOF, Shift);
        1: Grid.MoveBy(-PageStep, Shift);
        2: Exit;
        3: Grid.MoveBy(PageStep, Shift);
        4: Grid.MoveBy(DBGRID_EOF, Shift);
      end;
  end;

begin
  if (Grid = nil) or (not Grid.DataLink.Active) then Exit;
  { get the scroller position }
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_TRACKPOS;
  if not GetScrollInfo(Grid.Handle, FBarCode, ScrollInfo) then
  begin
    inherited;
    Exit;
  end;
  { handle scroll message }
  DataSet := Grid.DataLink.DataSet;
  PageStep := Grid.VisSize.Row;
  Shift := KeyboardStateToShiftState - [ssShift];
  case Message.ScrollCode of
    SB_LINEUP: Grid.MoveBy(-1, Shift);
    SB_LINEDOWN: Grid.MoveBy(1, Shift);
    SB_PAGEUP: Grid.MoveBy(-PageStep, Shift);
    SB_PAGEDOWN: Grid.MoveBy(PageStep, Shift);
    SB_THUMBPOSITION: DoThumbPos(ScrollInfo.nTrackPos);
    SB_THUMBTRACK: if Tracking and DataSet.IsSequenced then DoThumbPos(ScrollInfo.nTrackPos);
    SB_BOTTOM: Grid.MoveBy(DBGRID_EOF, Shift);
    SB_TOP: Grid.MoveBy(DBGRID_BOF, Shift);
  end;
end;

procedure TDBGridScrollBar.SetParams(AMin, AMax, APageStep, ALineStep: Integer);
begin
  inherited SetParams(0, 0, 0, 0);
  Update;
end;

procedure TDBGridScrollBar.SetPositionEx(Value: Integer; ScrollCode: Integer);
begin
  inherited SetPositionEx(0, ScrollCode);
  Update;
end;

procedure TDBGridScrollBar.Update;
var
  NewPage, NewPos: Integer;
  DataSet: TDataSet;
  SI: TScrollInfo;
begin
  if (Grid <> nil) and (Grid.HandleAllocated) and (UpdateLock = 0) then
  begin
    NewPage := 0;
    NewPos := 0;
    if Grid.DataLink.Active then
    begin
      DataSet := Grid.DataLink.DataSet;
      { for data set that support sequence numbers, set the position of the
        scroll bar to the active record, otherwise setup the scroll bar
        to move up and down }
      if DataSet.IsSequenced then
      begin
        if not (DataSet.State in [dsInactive, dsBrowse, dsEdit]) then
        begin
          SI.cbSize := SizeOf(SI);
          SI.fMask := SIF_ALL;
          GetScrollInfo(Grid.Handle, SB_VERT, SI);
          NewPos := SI.nPos;
        end
        else
          NewPos := DataSet.RecNo;
        FRowMin := 1;
        NewPage := Grid.VisSize.Row;
        FRowMax := DataSet.RecordCount + NewPage - 1;
      end
      else
      begin
        FRowMin := 0;
        FRowMax := 4;
        NewPage := 0;
        if DataSet.BOF then NewPos := 0
        else if DataSet.EOF then NewPos := 4
        else NewPos := 2;
      end;
    end
    else
    begin
      FRowMin := 0;
      FRowMax := 0;
    end;
    FillChar(SI, SizeOf(SI), 0);
    SI.cbSize := SizeOf(SI);
    SI.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    if FRowMax <> FRowMin then
    begin
      SI.nMin := FRowMin;
      SI.nMax := FRowMax;
      SI.nPage := NewPage;
      SI.nPos := NewPos;
    end;
    SetScrollInfo(Grid.Handle, SB_VERT, SI, True);
  end;
end;

{ TDBGridDataLink }

constructor TDBGridDataLink.Create(AGrid: TCustomDBGridView);
begin
  inherited Create;
  VisualControl := True;
  FGrid := AGrid;
end;

procedure TDBGridDataLink.ActiveChanged;
begin
  Grid.DataLinkActivate(Active);
  FModified := False;
end;

procedure TDBGridDataLink.DataSetChanged;
begin
  FGrid.DataSetChanged;
  FModified := False;
end;

procedure TDBGridDataLink.DataSetScrolled(Distance: Integer);
begin
  FGrid.DataSetScrolled(Distance);
end;

function TDBGridDataLink.GetActiveRecord: Integer;
begin
  { TDataLink.GetActiveRecord throws an AV exception if the data source
    does not have a data set }
  Result := 0;
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    Result := inherited GetActiveRecord;
end;

procedure TDBGridDataLink.LayoutChanged;
begin
  FGrid.DataLayoutChanged;
  inherited;
end;

procedure TDBGridDataLink.EditingChanged;
begin
  FGrid.InvalidateIndicatorImage(FGrid.Row);
  if not Editing then FGrid.HideEdit;
end;

procedure TDBGridDataLink.FocusControl(Field: TFieldRef);
begin
  if Assigned(Field) and Assigned(Field^) then
  begin
    FGrid.SelectedField := Field^;
    if (FGrid.SelectedField = Field^) and FGrid.AcquireFocus then
    begin
      Field^ := nil;
      FGrid.ShowEdit;
    end;
  end;
end;

function TDBGridDataLink.MoveBy(Distance: Integer): Integer;
begin
  Result := Distance;
  if Result <> 0 then Result := inherited MoveBy(Distance);
end;

procedure TDBGridDataLink.RecordChanged(Field: TField);
begin
  FGrid.DataRecordChanged(Field);
  FModified := False;
end;

procedure TDBGridDataLink.SetActiveRecord(Value: Integer);
begin
  if (DataSource <> nil) and (DataSource.DataSet <> nil) then
    inherited;
end;

procedure TDBGridDataLink.UpdateData;
begin
  if FModified and (FInUpdateData = 0) then
  begin
    Inc(FInUpdateData);
    try
      FGrid.UpdateData;
    finally
      Dec(FInUpdateData);
    end;
    FModified := False;
  end;
end;

procedure TDBGridDataLink.Modified;
begin
  FModified := True;
end;

procedure TDBGridDataLink.Reset;
begin
  if FModified then RecordChanged(nil) else Dataset.Cancel;
end;

{ TDBGridSelectedRows }

function StrToBookmark(const S: string): TBookmark;
begin
  Result := BytesOf(S);
end;

function BookmarkToStr(const Bookmark: TBookmark): string;
begin
  Result := StringOf(Bookmark);
end;

constructor TDBGridSelectedRows.Create(AGrid: TCustomDBGridView);
begin
  inherited Create;
  FGrid := AGrid;
end;

procedure TDBGridSelectedRows.Changed;
begin
  inherited;
  FCache := '';
  FCacheIndex := -1;
end;

procedure TDBGridSelectedRows.Clear;
begin
  if Count > 0 then
  begin
    inherited Clear;
    FSelecting := False;
    FGrid.InvalidateGrid;
  end;
end;

function TDBGridSelectedRows.Compare(const S1, S2: string): Integer;
var
  Bookmark1, Bookmark2: TBookmark;
begin
  if FGrid.DataLink.Active then
  begin
    Bookmark1 := StrToBookmark(S1);
    Bookmark2 := StrToBookmark(S2);
    Result := FGrid.DataLink.DataSet.CompareBookmarks(Bookmark1, Bookmark2);
  end
  else
    Result := 0;
end;

function TDBGridSelectedRows.CompareStrings(const S1, S2: string): Integer;
begin
  Result := Compare(S1, S2);
end;

function TDBGridSelectedRows.Find(const S: string; var Index: Integer): Boolean;
begin
  if (S = FCache) and (FCacheIndex >= 0) then
  begin
    Index := FCacheIndex;
    Result := FCacheFind;
    Exit;
  end;
  Result := inherited Find(S, Index);
  FCache := S;
  FCacheIndex := Index;
  FCacheFind := Result;
end;

function TDBGridSelectedRows.GetBookmark(Index: Integer): TBookmark;
begin
  Result := StrToBookmark(inherited Get(Index));
end;

function TDBGridSelectedRows.GetCurrentRow: string;
begin
  if FGrid.DataLink.Active then
    Result := BookmarkToStr(FGrid.DataLink.DataSet.Bookmark)
  else
    Result := '';
end;

function TDBGridSelectedRows.GetCurrentRowSelected: Boolean;
begin
  Result := IndexOf(CurrentRow) <> -1;
end;

function TDBGridSelectedRows.IndexOf(const S: string): Integer;
begin
  if not Find(S, Result) then Result := -1;
end;

procedure TDBGridSelectedRows.SetCurrentRowSelected(Value: Boolean);
var
  Index: Integer;
  Current: string;
begin
  if not FGrid.DataLink.Active then Exit;
  Current := CurrentRow;
  if Find(Current, Index) = Value then Exit;
  if Value then
    inherited Insert(Index, Current)
  else
    inherited Delete(Index);
end;

procedure TDBGridSelectedRows.UpdateSelectionMark;
begin
  FSelectionMark := CurrentRow;
  FSelecting := True;
end;

{ TCustomDBGridView }

constructor TCustomDBGridView.Create(AOwner: TComponent);
begin
  FDataLink := CreateDataLink;
  FDefaultLayout := True;
  FShowIndicator := True;
  FIndicatorsLink := TChangeLink.Create;
  FIndicatorsLink.OnChange := IndicatorsChange;
  FIndicatorsDef := TImageList.CreateSize(16, 16);
  FIndicatorsDef.BkColor := clFuchsia;
  FIndicatorsDef.ResInstLoad(HInstance, rtBitmap, 'BM_GRIDVIEW_DB', clFuchsia);
  FAllowDeleteRecord := True;
  FAllowInsertRecord := True;
  FSelectedRows := TDBGridSelectedRows.Create(Self);
  inherited;
  AllowEdit := True;
  ColumnClick := False;
  CursorKeys := [gkArrows, gkMouse, gkTabs, gkMouseWheel];
  DoubleBuffered := True;
end;

destructor TCustomDBGridView.Destroy;
begin
  inherited;
  FreeAndNil(FSelectedRows);
  FreeAndNil(FIndicatorsLink);
  FreeAndNil(FIndicatorsDef);
  FreeAndNil(FDataLink);
end;

function TCustomDBGridView.GetCol: Longint;
begin
  Result := inherited Col;
end;

function TCustomDBGridView.GetColumns: TDBGridColumns;
begin
  Result := TDBGridColumns(inherited Columns);
end;

function TCustomDBGridView.GetDataSource: TDataSource;
begin
  Result := DataLink.DataSource;
end;

function TCustomDBGridView.GetEditColumn: TDBGridColumn;
begin
  Result := TDBGridColumn(inherited EditColumn);
end;

function TCustomDBGridView.GetEditField: TField;
begin
  Result := nil;
  if EditColumn <> nil then Result := EditColumn.Field;
end;

function TCustomDBGridView.GetFixed: TDBGridFixed;
begin
  Result := TDBGridFixed(inherited Fixed);
end;

function TCustomDBGridView.GetHeader: TDBGridHeader;
begin
  Result := TDBGridHeader(inherited Header);
end;

function TCustomDBGridView.GetRows: TDBGridRows;
begin
  Result := TDBGridRows(inherited Rows);
end;

function TCustomDBGridView.GetSelectedField: TField;
begin
  Result := nil;
  if (Col >= Fixed.Count) and (Col < Columns.Count) then Result := Columns[Col].Field;
end;

function TCustomDBGridView.GetSelectedRows: TDBGridSelectedRows;
begin
  Result := FSelectedRows;
end;

procedure TCustomDBGridView.IndicatorsChange(Sender: TObject);
begin
  if FShowIndicator then InvalidateIndicator;
end;

procedure TCustomDBGridView.Insert(AppendMode: Boolean);
var
  AllowInsert: Boolean;
begin
  if not Datalink.Active then Exit;
  with Datalink.DataSet do
    if (State <> dsInsert) and CanModify and (not ReadOnly) and (not RowSelect) then
    begin
      AllowInsert := FAllowInsertRecord;
      if Assigned(FOnDataInsertRecord) then FOnDataInsertRecord(Self, AllowInsert);
      if AllowInsert then
      begin
        if AppendMode then Append else Insert;
        Editing := True;
      end;
    end;
end;

function TCustomDBGridView.IsColumnsStored: Boolean;
begin
  Result := False;
end;

procedure TCustomDBGridView.SelectAll;
var
  OldCurrent: TBookmark;
begin
  if not Datalink.Active then Exit;
  if (not MultiSelect) or Editing then Exit;
  with DataLink.DataSet, FSelectedRows do
  begin
    CheckBrowseMode;
    DisableControls;
    try
      Clear;
      OldCurrent := Bookmark;
      First;
      while not EOF do
      begin
        CurrentRowSelected := True;
        Next;
      end;
      Bookmark := OldCurrent;
      UpdateSelectionMark;
    finally
      EnableControls;
    end;
  end;
  InvalidateGrid;
end;

procedure TCustomDBGridView.SetCol(Value: Longint);
begin
  inherited Col := Value;
end;

procedure TCustomDBGridView.SetColumns(Value: TDBGridColumns);
begin
  Columns.Assign(Value);
end;

procedure TCustomDBGridView.SetDataSource(Value: TDataSource);
begin
  if DataLink.DataSource <> Value then
  begin
    if DataLink.DataSource <> nil then DataLink.DataSource.RemoveFreeNotification(Self);
    DataLink.DataSource := Value;
    if DataLink.DataSource <> nil then DataLink.DataSource.FreeNotification(Self);
    DataLayoutChanged;
  end;
end;

procedure TCustomDBGridView.SetDefaultLayout(Value: Boolean);
begin
  if FDefaultLayout <> Value then
  begin
    FDefaultLayout := Value;
    DataLayoutChanged;
    Invalidate;
  end;
end;

procedure TCustomDBGridView.SetFieldText(Field: TField; const Text: string);
begin
  if Assigned(FOnSetFieldText) then FOnSetFieldText(Self, Field, Text)
  else Field.Text := Text;
end;

procedure TCustomDBGridView.SetFixed(Value: TDBGridFixed);
begin
  Fixed.Assign(Value);
end;

procedure TCustomDBGridView.SetHeader(Value: TDBGridHeader);
begin
  Header.Assign(Value);
end;

procedure TCustomDBGridView.SetIndicatorImages(Value: TImageList);
begin
  if FIndicatorImages <> Value then
  begin
    if Assigned(FIndicatorImages) then FIndicatorImages.UnRegisterChanges(FIndicatorsLink);
    FIndicatorImages := Value;
    if Assigned(FIndicatorImages) then
    begin
      FIndicatorImages.RegisterChanges(FIndicatorsLink);
      FIndicatorImages.FreeNotification(Self);
    end;
    ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.SetIndicatorWidth(Value: Integer);
begin
  if FIndicatorWidth <> Value then
  begin
    FIndicatorWidth := Value;
    ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.SetMultiSelect(Value: Boolean);
begin
  if FMultiSelect <> Value then
  begin
    FMultiSelect := Value;
    if not Value then
    begin
      FSelectedRows.Clear;
      InvalidateGrid;
    end;
  end;
end;

procedure TCustomDBGridView.SetRows(Value: TDBGridRows);
begin
  Rows.Assign(Value);
end;

procedure TCustomDBGridView.SetSelectedField(Value: TField);
var
  I: Integer;
begin
  if Value <> nil then
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Field = Value then
      begin
        Col := I;
        Break;
      end;
end;

procedure TCustomDBGridView.SetShowIndicator(Value: Boolean);
begin
  if FShowIndicator <> Value then
  begin
    FShowIndicator := Value;
    ChangeIndicator;
  end;
end;

procedure TCustomDBGridView.ShowCursor;
begin
  inherited;
  InvalidateIndicatorImage(Row);
end;

procedure TCustomDBGridView.ReadColumns(Reader: TReader);
begin
  Columns.Clear;
  Reader.ReadValue;
  Reader.ReadCollection(Columns);
end;

procedure TCustomDBGridView.WriteColumns(Writer: TWriter);
begin
  Writer.WriteCollection(Columns);
end;

procedure TCustomDBGridView.CMExit(var Message: TMessage);
begin
  if CancelOnExit then
  try
    CancelOrUpdateData;
  except
    AcquireFocus;
    raise;
  end;
  inherited;
end;

procedure TCustomDBGridView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if FSelectedRows.Count > 0 then InvalidateSelected;
end;

procedure TCustomDBGridView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if FSelectedRows.Count > 0 then InvalidateSelected;
end;

function TCustomDBGridView.AcquireLockLayout: Boolean;
begin
  Result := (UpdateLock = 0) and (FLayoutLock = 0);
  if Result then LockLayout;
end;

procedure TCustomDBGridView.ChangeIndicator;
begin
  UpdateHeader;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursorPos(False);
  UpdateEdit(Editing);
  Invalidate;
end;

procedure TCustomDBGridView.ChangeScale(M, D: Integer);
begin
  inherited ChangeScale(M, D);
  if M <> D then FIndicatorWidth := MulDiv(FIndicatorWidth, M, D);
end;

procedure TCustomDBGridView.ClearSelection;
begin
  SelectedRows.Clear;
end;

function TCustomDBGridView.CreateColumns: TGridColumns;
begin
  Result := TDBGridColumns.Create(Self);
end;

function TCustomDBGridView.CreateDataLink: TDBGridDataLink;
begin
  Result := TDBGridDataLink.Create(Self);
end;

function TCustomDBGridView.CreateFixed: TCustomGridFixed;
begin
  Result := TDBGridFixed.Create(Self);
end;

function TCustomDBGridView.CreateHeader: TCustomGridHeader;
begin
  Result := TDBGridHeader.Create(Self);
end;

function TCustomDBGridView.CreateRows: TCustomGridRows;
begin
  Result := TDBGridRows.Create(Self);
end;

function TCustomDBGridView.CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar;
begin
  if Kind = sbVertical then
    Result := TDBGridScrollBar.Create(Self, Kind)
  else
    Result := inherited CreateScrollBar(Kind);
end;

procedure TCustomDBGridView.DataEditError(E: Exception; var Action: TDBGridDataAction);
begin
  if Assigned(FOnDataEditError) then FOnDataEditError(Self, E, Action);
end;

procedure TCustomDBGridView.DataFieldUpdated(Field: TField);
begin
  if Assigned(FOnDataUpdateField) then FOnDataUpdateField(Self, Field);
end;

procedure TCustomDBGridView.DataLayoutChanged;
begin
  if AcquireLockLayout then UnLockLayout(False);
end;

procedure TCustomDBGridView.DataLinkActivate(Active: Boolean);
begin
  FSelectedRows.Clear;
  ResetClickPos;
  DataLayoutChanged;
  UpdateScrollBars;
  UpdateScrollPos;
  UpdateCursorPos(True);
  UpdateEdit(Editing);
  { when changing the activity of the source, you must always redraw the
    grid, otherwise if the new data source (SQL query) gives the same
    columns and number of rows as the old source, then the grid will not
    be automatically redrawn and we will not see the new data }
  Invalidate;
end;

procedure TCustomDBGridView.DataRecordChanged(Field: TField);
var
  I: Integer;
  CField: TField;
begin
  if Field <> nil then
  begin
    for I := 0 to Columns.Count - 1 do
      if Columns[I].Field = Field then InvalidateColumn(I);
    { redraw the current row, because other calculated fields of the row
      may depend on the value of the current field }
    InvalidateRow(CellFocused.Row);
    { update the inplace editor if its field has changed }
    CField := EditField;
    if (CField = Field) and (CField.Text <> FFieldText) then
    begin
      UpdateEditContents(False);
      if Edit <> nil then Edit.Deselect;
    end;
  end
  else
    InvalidateGrid;
end;

procedure TCustomDBGridView.DataSetChanged;
begin
  ResetClickPos;
  UpdateRowCount;
  UpdateScrollBars;
  UpdateCursorPos(False);
  UpdateEditContents(False);
  ValidateRect(Handle, nil);
  Invalidate;
  if Assigned(FOnDataChange) then FOnDataChange(Self);
end;

procedure TCustomDBGridView.DataSetScrolled(Distance: Integer);
var
  R: TRect;
begin
  HideCursor;
  try
    if DataLink.ActiveRecord >= Rows.Count then UpdateRowCount;
    UpdateScrollBars;
    UpdateCursorPos(True);
    if Distance <> 0 then
    begin
      if Abs(Distance) <= VisSize.Row then
      begin
        R := GetRowsRect(0, VisSize.Row - 1);
        ScrollWindowEx(Handle, 0, - Distance * Rows.Height, @R, @R, 0, nil, SW_INVALIDATE);
      end
      else
        InvalidateGrid;
    end;
  finally
    ShowCursor;
  end;
end;

procedure TCustomDBGridView.DataUpdateError(E: Exception; var Action: TDBGridDataAction);
begin
  if Assigned(FOnDataUpdateError) then FOnDataUpdateError(Self, E, Action);
end;

procedure TCustomDBGridView.DefineProperties(Filer: TFiler);
var
  HasColumns: Boolean;
  AGrid: TCustomDBGridView;
begin
  HasColumns := not DefaultLayout;
  if HasColumns and (Filer.Ancestor <> nil) then
  begin
    AGrid := TCustomDBGridView(Filer.Ancestor);
    if not AGrid.DefaultLayout then
      HasColumns := not CollectionsEqual(Columns, AGrid.Columns, nil, nil);
  end;
  Filer.DefineProperty('Columns', ReadColumns, WriteColumns, HasColumns);
end;

procedure TCustomDBGridView.Delete;
const
  SDeleteMsg: array[Boolean] of string = (SDeleteRecordQuestion, SDeleteMultipleRecordsQuestion);
const
  Flags = MB_ICONQUESTION or MB_YESNO;
var
  AllowDelete: Boolean;
  Msg: string;
  I: Integer;
begin
  if not Datalink.Active then Exit;
  with Datalink.DataSet do
    if (State <> dsInsert) and (not IsEmpty) and CanModify and (not ReadOnly) and
      (not MultiSelect or (FSelectedRows.Count > 0)) then
    begin
      AllowDelete := FAllowDeleteRecord;
      if not Assigned(FOnDataDeleteRecord) then
      begin
        Msg := SDeleteMsg[FSelectedRows.Count > 1];
        with Application do
          AllowDelete := AllowDelete and (MessageBox(PChar(Msg), PChar(Title), Flags) = ID_YES);
      end
      else
        FOnDataDeleteRecord(Self, AllowDelete);
      if AllowDelete then
      begin
        DisableControls;
        try
          if not MultiSelect then
            Delete
          else
            for I := FSelectedRows.Count - 1 downto 0 do
            begin
              Bookmark := FSelectedRows.Bookmarks[I];
              Delete;
              FSelectedRows.Delete(I);
            end;
        finally
          EnableControls;
        end;
      end;
    end;
end;

function TCustomDBGridView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  KeepSelected: Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if Result and (gkMouseWheel in CursorKeys) and DataLink.Active then
  begin
    if not (ssShift in Shift) then
    begin
      MoveBy(1, Shift);
      KeepSelected := FSelectedRows.CurrentRowSelected;
      SetCursor(CellFocused, KeepSelected, True);
    end
    else if not RowSelect then
    begin
      MoveBy(0, Shift - [ssShift]);
      KeepSelected := FSelectedRows.CurrentRowSelected;
      SetCursor(CellFocused, KeepSelected, True)
    end;
  end;
end;

function TCustomDBGridView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
var
  KeepSelected: Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if Result and (gkMouseWheel in CursorKeys) and DataLink.Active then
  begin
    if not (ssShift in Shift) then
    begin
      MoveBy(-1, Shift);
      KeepSelected := FSelectedRows.CurrentRowSelected;
      SetCursor(CellFocused, KeepSelected, True)
    end
    else if not RowSelect then
    begin
      MoveBy(0, Shift - [ssShift]);
      KeepSelected := FSelectedRows.CurrentRowSelected;
      SetCursor(CellFocused, KeepSelected, True)
    end;
  end;
end;

function TCustomDBGridView.EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean;
begin
  Result := False;
  if IsCellValid(Cell) and (Cell.Col >= Fixed.Count) and DataLink.Active then
  begin
    with Columns[Cell.Col] do
      Result := (Field <> nil) and Field.IsValidChar(Key);
    if Assigned(OnEditAcceptKey) then OnEditAcceptKey(Self, Cell, Key, Result);
  end;
end;

function TCustomDBGridView.EditCanModify(Cell: TGridCell): Boolean;
var
  Action: TDBGridDataAction;
begin
  Result := inherited EditCanModify(Cell) and DataLink.Active and
    (not Datalink.ReadOnly) and (not IsReadOnlyField(EditField)) and
    (EditField <> nil) and (EditField.CanModify);
  if Result then
  try
    if not Datalink.Editing then Result := DataLink.Edit;
    if Result then Datalink.Modified;
  except
    on E: Exception do
    begin
      if not (E is EAbort) then
      begin
        Action := gdaFail;
        DataEditError(E, Action);
      end
      else
        Action := gdaAbort;
      if Action = gdaFail then raise;
      if Action = gdaAbort then SysUtils.Abort;
    end;
  end;
end;

function TCustomDBGridView.EditCanShow(Cell: TGridCell): Boolean;
begin
  Result := DataLink.Active and inherited EditCanShow(Cell);
end;

function TCustomDBGridView.FindText(const FindText: string; Options: TFindOptions): Boolean;

  function CompareCell(Col, Row: Integer): Boolean;
  var
    C: TGridCell;
    T: string;
  begin
    Result := False;
    { skip hidden columns }
    if Columns[Col].Width > 0 then
    begin
      C := GridCell(Col, Row);
      T := GetCellText(C);
      if CompareStrings(FindText, T, frWholeWord in Options, frMatchCase in Options) then
      begin
        SetCursor(C, True, True);
        Result := True;
      end;
    end;
  end;

var
  Bookmark: TBookmark;
  I: Integer;
begin
  if DataLink.Active then
  begin
    DataLink.DataSet.DisableControls;
    try
      Result := True;
      Bookmark := DataLink.DataSet.Bookmark;
      if frDown in Options then
      begin
        { search forward: iterate the cells down from left to right, starting
          with next cell relative to the current one }
        I := CellFocused.Col + 1;
        while not DataLink.EOF do
        begin
          while I <= Columns.Count - 1 do
          begin
            if CompareCell(I, DataLink.ActiveRecord) then Exit;
            Inc(I);
          end;
          DataLink.MoveBy(1);
          I := 0;
        end;
      end
      else
      begin
        { search backward: iterate the cells up from right to left, starting
          with previous cell relative to the current one }
        I := CellFocused.Col - 1;
        { special case: see comment in the TCustomGridView.FindText }
        while (I >= 0) and (Columns[I].Width = 0) do Dec(I);
        if (I < Fixed.Count) and (not DataLink.BOF) then
        begin
          DataLink.MoveBy(-1);
          I := Columns.Count - 1;
        end;
        while not DataLink.BOF do
        begin
          while I >= 0 do
          begin
            if CompareCell(I, DataLink.ActiveRecord) then Exit;
            Dec(I);
          end;
          DataLink.MoveBy(-1);
          I := Columns.Count - 1;
        end;
      end;
      { text not found - restore data set position }
      DataLink.DataSet.Bookmark := Bookmark;
    finally
      DataLink.DataSet.EnableControls;
    end;
    { text not found event }
    DoTextNotFound(FindText);
  end;
  Result := False;
end;

procedure TCustomDBGridView.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
var
  OldActive: Integer;
begin
  if DataLink.Active and IsCellValidEx(Cell, True, False) then
  begin
    OldActive := DataLink.ActiveRecord;
    try
      DataLink.ActiveRecord := Cell.Row;
      inherited;
    finally
      DataLink.ActiveRecord := OldActive;
    end;
  end
  else
    inherited;
end;

function TCustomDBGridView.GetCellText(Cell: TGridCell): string;
var
  OldActive: Integer;
  Field: TField;
begin
  if DataLink.Active and IsCellValidEx(Cell, True, False) then
  begin
    OldActive := DataLink.ActiveRecord;
    try
      DataLink.ActiveRecord := Cell.Row;
      Field := Columns[Cell.Col].Field;
      if (Field <> nil) and (Field.DataSet <> nil) then Result := Field.DisplayText;
      if Assigned(OnGetCellText) then OnGetCellText(Self, Cell, Result);
    finally
      DataLink.ActiveRecord := OldActive;
    end;
  end
  else
    Result := inherited GetCellText(Cell);
end;

function TCustomDBGridView.GetColumnClass: TGridColumnClass;
begin
  Result := TDBGridColumn;
end;

function TCustomDBGridView.GetEditClass(Cell: TGridCell): TGridEditClass;
begin
  Result := TDBGridEdit;
end;

function TCustomDBGridView.GetEditText(Cell: TGridCell): string;
begin
  Result := inherited GetEditText(Cell);
  { the value displayed in the inplace editor may differ from the value
    of the its field. therefore, to check the change, we need to remember
    the value of the field instead of the value that the OnGetEditText
    event returned. }
  if EditField <> nil then FFieldText := EditField.Text
  else FFieldText := '';
end;

procedure TCustomDBGridView.HideCursor;
begin
  inherited;
  InvalidateIndicatorImage(Row);
end;

procedure TCustomDBGridView.InvalidateGrid;
begin
  inherited;
  InvalidateIndicator;
end;

procedure TCustomDBGridView.InvalidateIndicator;
begin
  InvalidateRect(GetIndicatorHeaderRect);
  InvalidateRect(GetIndicatorFixedRect);
end;

procedure TCustomDBGridView.InvalidateIndicatorImage(DataRow: Integer);
begin
  InvalidateRect(GetIndicatorImageRect(DataRow));
end;

procedure TCustomDBGridView.InvalidateRow(Row: Integer);
begin
  inherited;
  InvalidateIndicatorImage(Row);
end;

procedure TCustomDBGridView.InvalidateSelected;
begin
  InvalidateGrid;
end;

procedure TCustomDBGridView.KeyDown(var Key: Word; Shift: TShiftState);
var
  KeyScroll, CtrlSelected, KeepSelected: Boolean;
begin
  KeyScroll := Key in [VK_UP, VK_DOWN, VK_PRIOR, VK_NEXT, VK_HOME, VK_END,
    VK_TAB, VK_RETURN, VK_ESCAPE, VK_INSERT, VK_DELETE];
  { lock cursor movement in the TCustomGridView.KeyDown handler }
  if KeyScroll then LockScroll;
  try
    inherited;
    if not DataLink.Active then Exit;
    { before changing the active record, the MoveBy method automatically calls
      the DataLink.UpdateRecord method, in which an exception may occur. This
      exception should be intercepted and not allowed to change the position
      of the cursor. Otherwise, the UnLockScroll call will attempt to move the
      active record to the new cursor position, which again will call
      DataLink.UpdateRecord and generate a second error message. }
    try
      { moving with the arrow keys }
      if gkArrows in CursorKeys then
      begin
        CtrlSelected := MultiSelect and (Shift = [ssCtrl]);
        case Key of
          VK_LEFT:
            begin
              MoveBy(0, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(CellFocused, KeepSelected, True);
            end;
          VK_RIGHT:
            begin
              MoveBy(0, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(CellFocused, KeepSelected, True);
            end;
          VK_UP:
            { cancel insertion and move to the previous record }
            begin
              MoveBy(-1, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(CellFocused, KeepSelected, True);
            end;
          VK_DOWN:
            { move to the next record or insert new record }
            begin
              MoveBy(1, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(CellFocused, KeepSelected, True);
            end;
          VK_PRIOR:
            begin
              MoveBy(-VisSize.Row, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(FScrollCell, KeepSelected, True);
            end;
          VK_NEXT:
            begin
              MoveBy(VisSize.Row, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(FScrollCell, KeepSelected, True);
            end;
          VK_HOME:
            if ssCtrl in Shift then
            begin
              MoveBy(DBGRID_BOF, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(FScrollCell, KeepSelected, True);
            end;
          VK_END:
            if ssCtrl in Shift then
            begin
              MoveBy(DBGRID_EOF, Shift);
              KeepSelected := (not CtrlSelected) or FSelectedRows.CurrentRowSelected;
              SetCursor(FScrollCell, KeepSelected, True);
            end;
          VK_SPACE:
            if ssCtrl in Shift then
            begin
              MoveBy(0, Shift + [ssMiddle]);
              KeepSelected := FSelectedRows.CurrentRowSelected;
              SetCursor(CellFocused, KeepSelected, True);
            end;
        end;
      end;
      { moving with the TAB key }
      if (gkTabs in CursorKeys) and (Key = VK_TAB) and (not RowSelect) then
      begin
        { move to the next record by pressing the TAB key on the last column }
        if (CellFocused.Col = Columns.Count - 1) and (not (ssShift in Shift)) then
        begin
          MoveBy(1, []);
          SetCursor(GetCursorCell(CellFocused, goHome), True, True);
        end;
        { move to the prior record by pressing the SHIFT+TAB key on the
          first column }
        if (CellFocused.Col = Fixed.Count) and (ssShift in Shift) then
        begin
          MoveBy(-1, []);
          SetCursor(GetCursorCell(CellFocused, goEnd), True, True);
        end;
      end;
      { other keys }
      case Key of
        VK_ESCAPE:
          begin
            CancelEdit;
            SetCursor(CellFocused, True, True);
          end;
        VK_INSERT:
          if (Shift = []) and (not Editing) then Insert(False);
        VK_DELETE:
          if (Shift = []) and (not Editing) then
          begin
            Delete;
            SetCursor(CellFocused, True, True);
          end;
      end;
    except
      SetCursor(CellFocused, True, True);
      raise;
    end;
  finally
    if KeyScroll then UnLockScroll(False);
  end;
end;

procedure TCustomDBGridView.KeyPress(var Key: Char);
begin
  if (Key = #13) and (gkReturn in CursorKeys) and Editing then
  begin
    LockScroll;
    try
      inherited;
      { move to the next record by pressing the RETURN key on the last column }
      if CellFocused.Col = Columns.Count - 1 then
      begin
        MoveBy(1, []);
        SetCursor(GetCursorCell(CellFocused, goHome), True, True);
      end;
    finally
      UnLockScroll(False);
    end;
  end
  else
    inherited;
end;

procedure TCustomDBGridView.Loaded;
begin
  inherited;
  DataLayoutChanged;
end;

procedure TCustomDBGridView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  if not (ssDouble in Shift) then
  begin
    if ssCtrl in Shift then Shift := Shift + [ssMiddle]; // <- SPACE key simulation
    if (not (ssShift in Shift)) and IsCellHighlighted(GetCellAt(X, Y)) then
    begin
      { we cannot start the selection immediately by clicking on the selected
        cell, otherwise the grid will not be able to start dragging }
      FSelectPending := True;
      FSelectPos := Point(X, Y);
      FSelectShift := Shift;
    end
    else
      MoveByXY(X, Y, Shift);
  end;
  inherited;
end;

procedure TCustomDBGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
begin
  { mouse offset cancels pendign selection }
  if FSelectPending then
  begin
    FSelectPending := False;
  end;
  { ignore cell selection when changing column widths }
  if (gkMouseMove in CursorKeys) and (not ColResizing) then
    MoveByXY(X, Y, Shift);
  inherited;
end;

procedure TCustomDBGridView.MouseUp(Button: TMouseButton; Shift: TShiftState; X,
  Y: Integer);
begin
  if FSelectPending then
  begin
    FSelectPending := False;
    MoveByXY(FSelectPos.X, FSelectPos.Y, FSelectShift);
  end;
  inherited;
end;

procedure TCustomDBGridView.MoveBy(Distance: Integer; Shift: TShiftState);
var
  RowCount, Direction, Direction2: Integer;
  OldCurrent: TBookmark;
  I, Index1, Index2: Integer;
begin
  if not Datalink.Active then Exit;
  { cancel insertion of a record when user presses the DOWN key in the
    first row or the UP key in the last row of the grid, in other rows the
    insert will be canceled automatically when moving the cursor }
  with DataLink.DataSet do
    if (State = dsInsert) and (not Modified) and (not Datalink.FModified) then
      if Distance > 0 then
      begin
        if not EOF then CancelOrUpdateData;
        Exit;
      end
      else if (Distance < 0) and EOF then
      begin
        CancelOrUpdateData;
        Exit;
      end;
  if not MultiSelect then
  begin
    { when MultiSelect is disabled, just move the cursor }
    DataLink.MoveBy(Distance);
  end
  else if ssRight in Shift then
  begin
    { right-click (works only when RightClickSelect is on) outside the selected
      rows resets the multiple selection in any state of CTRL and SHIFT keys,
      clicking on the selected row does nothing }
    InvalidateRow(CellFocused.Row);
    DataLink.MoveBy(Distance);
    if not FSelectedRows.CurrentRowSelected then
    begin
      FSelectedRows.Clear;
      FSelectedRows.CurrentRowSelected := True;
      InvalidateRow(CellFocused.Row);
    end;
  end
  else if Shift * [ssShift, ssCtrl] = [] then
  begin
    { moving the cursor without pressing the SHIFT and CTRL keys resets the
      current multiple selection and selects a new row }
    InvalidateRow(CellFocused.Row);
    FSelectedRows.Clear;
    DataLink.MoveBy(Distance);
    FSelectedRows.CurrentRowSelected := True;
    FSelectedRows.UpdateSelectionMark;
    InvalidateRow(CellFocused.Row);
  end
  else if ssShift in Shift then
  begin
    Direction := Sign(Distance);
    RowCount := Abs(Distance);
    with DataLink.DataSet, FSelectedRows do
    begin
      DisableControls;
      try
        if not Selecting then
        begin
          { if there was no selection, then assign the current row as
            a selection marker }
          UpdateSelectionMark;
        end
        else if (Distance <> 0) and not (ssCtrl in Shift) then
        begin
          { like Expolrer, the grid should allow to select several groups of
            rows while holding down the CTRL key, but if the CTRL key is not
            pressed, then all selected groups except the current one should
            be reset }
          Find(SelectionMark, Index1);
          Find(CurrentRow, Index2);
          for I := Count - 1 downto Max(Index1, Index2) + 1 do Delete(I);
          for I := Min(Index1, Index2) - 1 downto 0 do Delete(I);
        end;
        { moving the cursor to the marker removes the selection of lines,
          but not further than the marker }
        Direction2 := Compare(SelectionMark, CurrentRow);
        if Direction2 = Direction then
          while (Direction2 <> 0) and (RowCount > 0) do
          begin
            CurrentRowSelected := ssCtrl in Shift; // <- CTRL does not reset selection
            DataLink.MoveBy(Direction);
            Dec(RowCount);
            if BOF or EOF then Break;
            Direction2 := Compare(SelectionMark, CurrentRow);
          end;
        { if the cursor is moved to the marker, but it has not reached,
          or if the cursor is moved from the marker, there are rows between
          it and the marker, and they must always be selected }
        Direction2 := Compare(SelectionMark, CurrentRow);
        if Direction2 <> 0 then
        begin
          OldCurrent := Bookmark;
          while Direction2 <> 0 do
          begin
            DataLink.MoveBy(Direction2);
            CurrentRowSelected := True;
            if BOF or EOF then Break;
            Direction2 := Compare(SelectionMark, CurrentRow);
          end;
          Bookmark := OldCurrent;
        end;
        { otherwise, cursor is moved from the marker and always selects rows }
        while RowCount > 0 do
        begin
          CurrentRowSelected := True;
          DataLink.MoveBy(Direction);
          Dec(RowCount);
          if BOF or EOF then Break;
        end;
        { the cursor after moving is always selected }
        CurrentRowSelected := True;
      finally
        EnableControls;
      end;
    end
  end
  else // ssCtrl in Shift
  begin
    { moving the cursor while pressing CTRL does not change the current
      selection }
    InvalidateRow(CellFocused.Row);
    DataLink.MoveBy(Distance);
    { exception: CTRL+SPACE inverts the selection of a curent row and
      puts focus to it }
    if ssMiddle in Shift then
    begin
      FSelectedRows.CurrentRowSelected := not FSelectedRows.CurrentRowSelected;
      FSelectedRows.UpdateSelectionMark;
    end;
    InvalidateRow(CellFocused.Row);
  end;
  { insert a new record at the beginning of the data set when the user presses
    the UP key on the first record; add a new record to the end of the data set
    when the user presses the DOWN key on the last record }
  with DataLink.DataSet do
    if (State <> dsInsert) then
    begin
      //if BOF and (Distance = -1) then Self.Insert(False);
      if EOF and (Distance = 1) then Self.Insert(True);
    end;
end;

procedure TCustomDBGridView.MoveByXY(X, Y: Integer; Shift: TShiftState);
var
  C: TGridCell;
  KeepSelected: Boolean;

  function IsLeftButtonPressed: Boolean;
  begin
    Result := (ssLeft in Shift) or ((ssRight in Shift) and RightClickSelect);
  end;

begin
  if (gkMouse in CursorKeys) and IsLeftButtonPressed then
  begin
    if PtInRect(GetIndicatorFixedRect, Point(X, Y)) then
    begin
      C.Col := CellFocused.Col;
      C.Row := GetRowAt(X, Y);
    end
    else if PtInRect(GetGridRect, Point(X, Y)) then
      C := GetCellAt(X, Y)
    else
      Exit;
    { if the grid has no cells or click the free space, then the click cell
      will be equal to (-1, -1) }
    if C.Row = -1 then
    begin
      if Shift * [ssShift, ssCtrl] = [] then FSelectedRows.Clear;
      Exit;
    end;
    LockScroll;
    try
      MoveBy(C.Row - CellFocused.Row, Shift);
      KeepSelected := (not (ssCtrl in Shift)) or FSelectedRows.CurrentRowSelected;
      SetCursor(C, KeepSelected, True);
    finally
      UnLockScroll(False);
    end;
  end;
end;

procedure TCustomDBGridView.MoveTo(RecNo: Integer; Shift: TShiftState);
begin
  if DataLink.Active then
  begin
    DataLink.DataSet.RecNo := RecNo;
    MoveBy(0, Shift);
  end;
end;

procedure TCustomDBGridView.Notification(AComponent: TComponent; Operation: TOperation);
var
  I: Integer;
  NeedRefresh: Boolean;
begin
  inherited;
  if Operation = opRemove then
    if AComponent is TField then
    begin
      NeedRefresh := False;
      LockUpdate;
      try
        if Columns <> nil then
          for I := Columns.Count - 1 downto 0 do
            if Columns[I].FField = AComponent then
            begin
              Columns[I].FField := nil;
              NeedRefresh := True;
            end;
      finally
        UnlockUpdate(NeedRefresh);
      end;
    end
    else if AComponent = DataSource then
      DataSource := nil
    else if AComponent = FIndicatorImages then
      IndicatorImages := nil;
end;

procedure TCustomDBGridView.Paint;
var
  R: TRect;
begin
  if ShowIndicator then
  begin
    PaintIndicatorHeader;
    PaintIndicatorFixed;
    if GridLines then PaintIndicatorGridLines;
    R := GetIndicatorHeaderRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    R := GetIndicatorFixedRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited;
end;

procedure TCustomDBGridView.PaintCell(Cell: TGridCell; Rect: TRect);
var
  OldActive: Integer;
begin
  OldActive := DataLink.ActiveRecord;
  try
    DataLink.ActiveRecord := Cell.Row;
    inherited;
  finally
    DataLink.ActiveRecord := OldActive;
  end;
end;

procedure TCustomDBGridView.PaintIndicatorFixed;
var
  J: Integer;
  R: TRect;
begin
  R := GetIndicatorFixedRect;
  R.Bottom := GetRowRect(VisOrigin.Row).Top;
  { indicator pseudo cells }
  for J := 0 to VisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    if RectVisible(Canvas.Handle, R) then
    begin
      Canvas.Brush.Color := Fixed.Color;
      Canvas.FillRect(R);
      PaintIndicatorImage(R, J);
    end;
  end;
  { field to the bottom of the indicator }
  R.Top := R.Bottom;
  R.Bottom := GetIndicatorFixedRect.Bottom + 2;
  if not (gsListViewLike in GridStyle) then
    Canvas.Brush.Color := Color
  else
    Canvas.Brush.Color := Fixed.Color;
  Canvas.FillRect(R);
  { vertical separator line on the right }
  if Fixed.Flat or StyleServices.Enabled then
  begin
    R := GetIndicatorFixedRect;
    if not (IsFixedVisible or (gsListViewLike in GridStyle) or
      (gsFullVertLine in GridStyle)) then
    begin
      if VisSize.Row = 0 then Exit;
      R.Bottom := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
    end;
    if Fixed.GridColor or StyleServices.Enabled then
    begin
      if not (gsDotLines in GridStyle) then
      begin
        Canvas.Pen.Color := GetFixedDividerColor;
        Canvas.Pen.Width := GridLineWidth;
        Canvas.MoveTo(R.Right - 1, R.Bottom - 1);
        Canvas.LineTo(R.Right - 1, R.Top - 1);
      end
      else
      begin
        R.Left := R.Right - 1;
        PaintDotGridLines(@R, 2);
      end;
    end
    else
      with Canvas do
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Right - 2, R.Top - 1);
        LineTo(R.Right - 2, R.Bottom - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Right - 1, R.Bottom - 1);
        LineTo(R.Right - 1, R.Top - 1);
      end;
  end;
end;

procedure TCustomDBGridView.PaintIndicatorGridLines;
var
  Points: array of TPoint;
  PointCount: Integer;
  StrokeList: array of DWORD;
  StrokeCount: Integer;
  I, L, R, Y, C: Integer;
  Rect: TRect;

  procedure ShiftGridPoints(DX, DY: Integer);
  var
    I: Integer;
  begin
    for I := 0 to PointCount - 1 do
      Points[I].Y := Points[I].Y + DY;
  end;

  procedure Paint3DCells(Rect: TRect);
  var
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    while R.Bottom < Rect.Bottom do
    begin
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, BF_RECT);
    end;
  end;

  procedure PaintHorz3DLines(Rect: TRect);
  var
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    repeat
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, BF_RECT);
    until R.Bottom >= Rect.Bottom;
  end;

  procedure PaintBottom3DMargin(Rect: TRect);
  begin
    if RectVisible(Canvas.Handle, Rect) then
      Paint3DFrame(Rect, BF_LEFT or BF_TOP or BF_RIGHT);
  end;

begin
  if StyleServices.Enabled or Fixed.Flat then
  begin
    { the number of grid lines is equal to the number of visible rows }
    StrokeCount := 0;
    if gsHorzLine in GridStyle then
    begin
      if gsListViewLike in GridStyle then StrokeCount := GetGridHeight div Rows.Height
      else StrokeCount := VisSize.Row;
    end;
    if StrokeCount > 0 then
    begin
      { malloc two points on each line }
      SetLength(Points, StrokeCount * 2);
      SetLength(StrokeList, StrokeCount);
      for I := 0 to StrokeCount - 1 do StrokeList[I] := 2;
      { fill the points of horisontal lines }
      Rect := GetIndicatorFixedRect;
      PointCount := 0;
      if gsHorzLine in GridStyle then
      begin
        L := Rect.Left;
        R := Rect.Right;
        Y := GetRowRect(VisOrigin.Row).Top;
        if gsListViewLike in GridStyle then C := GetGridHeight div Rows.Height
        else C := VisSize.Row;
        for I := 0 to C - 1 do
        begin
          Inc(Y, Rows.Height);
          Points[PointCount].X := L;
          Points[PointCount].Y := Y - 1;
          Inc(PointCount);
          Points[PointCount].X := R - 1;
          Points[PointCount].Y := Y - 1;
          Inc(PointCount);
        end;
      end;
      { if the color of the fixed cells does not differ from the color of
        the grid, then the grid lines are the same, with the themes enabled
        always draw single lines, with the themes turned off on a gray
        background we draw double lines }
      if Fixed.GridColor or StyleServices.Enabled then
      begin
        { shift the lines (they are calculated for the double line) }
        //ShiftGridPoints(1, 1);
        { draw the single line }
        if not (gsDotLines in GridStyle) then
        begin
          Canvas.Pen.Color := GetFixedGridColor;
          Canvas.Pen.Width := GridLineWidth;
          PolyPolyLine(Canvas.Handle, Pointer(Points)^, Pointer(StrokeList)^, PointCount shr 1);
        end
        else
          PaintDotGridLines(Pointer(Points), PointCount);
      end
      else
      begin
        Canvas.Pen.Color := clBtnShadow;
        Canvas.Pen.Width := 1;
        PolyPolyLine(Canvas.Handle, Pointer(Points)^, Pointer(StrokeList)^, PointCount shr 1);
        ShiftGridPoints(1, 1);
        Canvas.Pen.Color := clBtnHighlight;
        PolyPolyLine(Canvas.Handle, Pointer(Points)^, Pointer(StrokeList)^, PointCount shr 1);
      end;
    end;
  end
  else if gsHorzLine in GridStyle then
  begin
    { only horizontal 3D lines }
    Rect := GetIndicatorFixedRect;
    if not (gsListViewLike in GridStyle) then Rect.Bottom := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
    Paint3DCells(Rect);
  end
  else
  begin
    { only 3D frame around indicator }
    Rect := GetIndicatorFixedRect;
    if not IsFixedVisible then
    begin
      Rect.Bottom := GetRowRect(VisOrigin.Row + VisSize.Row).Top;
      Paint3DFrame(Rect, BF_RECT);
    end
    else
      PaintBottom3DMargin(Rect);
  end;
end;

procedure TCustomDBGridView.PaintIndicatorHeader;
const
  PaintState: array[Boolean] of TGridPaintStates = ([], [psFlat]);
begin
  PaintHeaderBackground(GetIndicatorHeaderRect, Header.Color, PaintState[Header.Flat]);
end;

procedure TCustomDBGridView.PaintIndicatorImage(Rect: TRect; DataRow: Integer);
var
  I, X, Y, W, H: Integer;
  IL: TImageList;
begin
  I := GetIndicatorImage(DataRow);
  if I = -1 then Exit;
  IL := FIndicatorImages;
  if IL = nil then IL := FIndicatorsDef;
  W := IL.Width;
  H := IL.Height;
  X := Rect.Right - Rect.Left - W;
  X := Rect.Left + X div 2 - Ord(Fixed.Flat);
  Y := Rect.Bottom - Rect.Top - H;
  Y := Rect.Top + Y div 2 - Ord(Fixed.Flat);
  IL.BkColor := Canvas.Brush.Color;
  IL.Draw(Canvas, X, Y, I, True);
end;

procedure TCustomDBGridView.ChangeEditText(const S: string);
begin
  if Editing and EditCanModify(EditCell) then
  begin
    Edit.Text := S;
    DataLink.Modified;
  end;
end;

function TCustomDBGridView.IsCellReadOnly(Cell: TGridCell): Boolean;
begin
  { lookup and non-text fields are read only }
  Result := inherited IsCellReadOnly(Cell) or IsReadOnlyField(Columns[Cell.Col].Field);
end;

function TCustomDBGridView.IsEvenRow(Cell: TGridCell): Boolean;
begin
  { alternating rows can be highlighted only for data sources whose number
    of records is known (for example, Paradox, ClientDataSet) }
  if DataLink.Active and DataLink.DataSet.IsSequenced then
    Result := DataLink.DataSet.RecNo mod 2 <> 0
  else
    Result := inherited IsEvenRow(Cell);
end;

function TCustomDBGridView.IsRowHighlighted(Row: Integer): Boolean;
begin
  if (not MultiSelect) or (Row < 0) or (Rows.Count = 0) then
    Result := inherited IsRowHighlighted(Row)
  else
    Result := IsRowMultiSelected(Row);
end;

function TCustomDBGridView.IsRowMultiselected(Row: Integer): Boolean;
var
  OldActive: Integer;
begin
  if DataLink.Active and (Row >= 0) and
    ((FSelectedRows.Count > 1) or (Row <> CellFocused.Row)) then
  begin
    OldActive := DataLink.ActiveRecord;
    try
      DataLink.ActiveRecord := Row;
      Result := FSelectedRows.CurrentRowSelected;
    finally
      DataLink.ActiveRecord := OldActive;
    end;
  end
  else
    Result := False;
end;

procedure TCustomDBGridView.SetEditText(Cell: TGridCell; var Value: string);
begin
  if IsCellEqual(Cell, EditCell) and DataLink.Active and EditCanModify(Cell) and
    (Edit <> nil) then
  begin
    Edit.Text := Value;
    DataLink.UpdateData;
  end;
end;

procedure TCustomDBGridView.Resize;
begin
  UpdateRowCount;
  UpdateCursorPos(False);
  UpdateScrollPos;
  inherited;
end;

procedure TCustomDBGridView.UpdateData;
var
  CField: TField;
  Value: string;
  Action: TDBGridDataAction;
begin
  { if the UpdateData is called when the input line is hidden due to a loss
    of focus, the EditField will be reset inside SetEditText, so we need to
    remember it to generate the OnDataFieldUpdated event }
  CField := EditField;
  { only DataLink can set a new field value }
  if DataLink.Active and (DataLink.InUpdateData <> 0) and (not ReadOnly) and
    (CField <> nil) and (not IsReadOnlyField(CField)) and (Edit <> nil) then
  begin
    try
      Value := Edit.Text;
      inherited SetEditText(EditCell, Value);
      SetFieldText(CField, Value);
    except
      on E: Exception do
      begin
        UpdateCursorPos(True);
        if not (E is EAbort) then
        begin
          Action := gdaFail;
          DataUpdateError(E, Action);
        end
        else
          Action := gdaAbort;
        if Action = gdaFail then raise;
        if Action = gdaAbort then SysUtils.Abort;
      end;
    end;
    DataFieldUpdated(CField);
  end;
end;

procedure TCustomDBGridView.UpdateEditText;
begin
  if Datalink.Active then FDataLink.UpdateData;
end;

procedure TCustomDBGridView.ApplyEdit;
begin
  { the update of the data source is called in SetEditText }
  inherited;
end;

procedure TCustomDBGridView.CancelEdit;
begin
  DataLink.Reset;
  inherited;
end;

procedure TCustomDBGridView.CancelOrUpdateData;
begin
  if Datalink.Active then
    with Datalink.Dataset do
      if (State = dsInsert) and (not Modified) and (not Datalink.FModified) then
        Cancel
      else
        DataLink.UpdateData;
end;

function TCustomDBGridView.GetGridRect: TRect;
begin
  Result := inherited GetGridRect;
  if ShowIndicator then Inc(Result.Left, GetIndicatorWidth);
end;

function TCustomDBGridView.GetHeaderRect: TRect;
begin
  Result := inherited GetHeaderRect;
  if ShowIndicator then Inc(Result.Left, GetIndicatorWidth);
end;

function TCustomDBGridView.GetIndicatorHeaderRect: TRect;
begin
  Result := GetHeaderRect;
  Result.Right := Result.Left;
  Result.Left := Result.Left - GetIndicatorWidth;
end;

function TCustomDBGridView.GetIndicatorFixedRect: TRect;
begin
  Result := GetGridRect;
  Result.Right := Result.Left;
  Result.Left := Result.Left - GetIndicatorWidth;
end;

function TCustomDBGridView.GetIndicatorImage(DataRow: Integer): Integer;
begin
  Result := -1;
  if DataRow = DataLink.ActiveRecord then
  begin
    Result := 0;
    if DataLink.DataSet <> nil then
      case DataLink.DataSet.State of
        dsEdit: Result := 1;
        dsInsert: Result := 2;
      end;
  end
  else if IsRowHighlighted(DataRow) then
  begin
    Result := 3;
  end;
  if Assigned(FOnGetIndicatorImage) then FOnGetIndicatorImage(Self, DataRow, Result);
end;

function TCustomDBGridView.GetIndicatorImageRect(DataRow: Integer): TRect;
begin
  Result := GetIndicatorFixedRect;
  Result.Top := GetRowTopBottom(DataRow).Top;
  Result.Bottom := GetRowTopBottom(DataRow).Bottom;
end;

function TCustomDBGridView.GetIndicatorWidth: Integer;
begin
  Result := FIndicatorWidth;
  if Result < 1 then Result := GetSystemMetrics(SM_CXHSCROLL);
end;

procedure TCustomDBGridView.LockLayout;
begin
  LockUpdate;
  Inc(FLayoutLock);
  if FLayoutLock = 1 then Columns.BeginUpdate;
end;

procedure TCustomDBGridView.LockScroll;
begin
  Inc(FScrollLock);
  if FScrollLock = 1 then
  begin
    FScrollCell := CellFocused;
    FScrollSelected := CellSelected;
  end;
end;

procedure TCustomDBGridView.MakeCellVisible(Cell: TGridCell; PartialOK: Boolean);
begin
  { a cell that is not equal to the current record is not visible }
  if Cell.Row = CellFocused.Row then
    inherited MakeCellVisible(Cell, PartialOK);
end;

procedure TCustomDBGridView.SetCursor(Cell: TGridCell; Selected, Visible: Boolean);
var
  IC: TGridCell;
begin
  IC := CellFocused;
  { if the cursor scrolling is locked, the new position should be
    stored in the buffer }
  if (FScrollLock <> 0) and (FCursorFromDataSet = 0) then
  begin
    FScrollCell := Cell;
    FScrollSelected := Selected;
    Exit;
  end;
  { vertical movements in the grid are allowed only when navigating data
    source records }
  if FCursorFromDataSet = 0 then Cell.Row := CellFocused.Row;
  inherited SetCursor(Cell, Selected, Visible);
end;

procedure TCustomDBGridView.ResetEdit;
begin
  if DataLink.FModified then DataLink.Reset;
end;

procedure TCustomDBGridView.UnLockLayout(CancelChanges: Boolean);
begin
  if FLayoutLock = 1 then
  begin
    if not CancelChanges then UpdateLayout;
    Columns.EndUpdate;
  end;
  Dec(FLayoutLock);
  UnlockUpdate(False);
end;

procedure TCustomDBGridView.UnLockScroll(CancelScroll: Boolean);
begin
  Dec(FScrollLock);
  if (FScrollLock = 0) and ((not IsCellEqual(FScrollCell, CellFocused)) or
    (FScrollSelected <> CellSelected)) then
  begin
    SetCursor(GridCell(FScrollCell.Col, CellFocused.Row), FScrollSelected, True);
    if (not CancelScroll) and (FScrollCell.Row <> CellFocused.Row) then
    begin
      if DataLink.Active then
        DataLink.MoveBy(FScrollCell.Row - CellFocused.Row);
    end;
  end;
end;

procedure TCustomDBGridView.UpdateCursorPos(ShowCursor: Boolean);
var
  Cell: TGridCell;
begin
  Inc(FCursorFromDataSet);
  try
    if DataLink.Active then
    begin
      Cell.Col := CellFocused.Col;
      Cell.Row := DataLink.ActiveRecord;
    end
    else
      Cell := GridCell(0, 0);
    SetCursor(Cell, CellSelected, ShowCursor);
  finally
    Dec(FCursorFromDataSet);
  end;
end;

procedure TCustomDBGridView.UpdateLayout;
var
  I: Integer;
  List: TList;
  Column: TDBGridColumn;

  procedure GetFields(Fields: TFields);
  var
    I: Integer;
    Field: TField;
  begin
    for I := 0 to Fields.Count - 1 do
    begin
      Field := Fields[I];
      if Field.Visible then
      begin
        List.Add(Field);
        if Field.DataType in [ftADT, ftArray] then
          GetFields((Field as TObjectField).Fields);
      end;
    end;
  end;

begin
  if ([csLoading, csDestroying] * ComponentState) <> [] then Exit;
  if FDefaultLayout then
  begin
    if DataLink.Active then
    begin
      List := TList.Create;
      try
        GetFields(DataLink.DataSet.Fields);
        { avoid clearing the list of columns here, as this causes an AV
          exception when editing columns in Design mode }
        while (List.Count > 0) and (Columns.Count < List.Count) do Columns.Add;
        while (Columns.Count > 0) and (Columns.Count > List.Count) do Columns[0].Free;
        for I := 0 to List.Count - 1 do
        begin
          Column := Columns[I];
          Column.FieldName := TField(List[I]).FullName;
          Column.Field := nil;
          Column.RestoreDefaults;
          Column.FDefaultColumn := True;
        end;
      finally
        List.Free;
      end;
    end
    else
      Columns.Clear;
    Header.SynchronizeSections;
  end
  else
  begin
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
      Column.Field := nil;
      if Column.DefaultColumn then
      begin
        Column.RestoreDefaults;
        Column.FDefaultColumn := True;
      end;
    end;
  end;
  Fixed.SetCount(Fixed.Count);
  UpdateRowCount;
  UpdateCursorPos(True);
end;

procedure TCustomDBGridView.UpdateRowCount;

  procedure SetRowsCount(Value: Integer);
  begin
    with TDBGridRows(Rows) do
    begin
      Inc(FRowsFromGrid);
      try
        SetCount(Value);
      finally
        Dec(FRowsFromGrid);
      end;
    end;
  end;

begin
  if DataLink.Active then
  begin
    { DataLink buffer size should be equal to the number of visible lines }
    DataLink.BufferCount := GetGridHeight div Rows.Height;
    SetRowsCount(DataLink.RecordCount);
  end
  else
    SetRowsCount(0);
end;

procedure TCustomDBGridView.UpdateSelection(var Cell: TGridCell; var Selected: Boolean);
begin
  inherited;
  { in any way, the selected row must be equal to the active record }
  Cell.Row := DataLink.ActiveRecord;
end;

end.

