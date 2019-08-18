{
  TGridView component (grid)

  (C) Roman M. Mochalov, 1997-2019
  E-mail: checker@mail.ru

  License: MIT
}

unit Ex_Grid;

interface

uses
  Windows, Messages, SysUtils, CommCtrl, Classes, Forms, Controls, Graphics,
  Dialogs, StdCtrls, Math, Mask, ImgList, Types, Menus, StrUtils, UITypes;

const
  CN_THEMECHANGED = CN_BASE + WM_THEMECHANGED;

var
  crHeaderSplit: TCursor = crHSplit;

type
  TGridHeaderSections = class;
  TCustomGridHeader = class;
  TCustomGridColumn = class;
  TGridColumns = class;
  TCustomGridRows = class;
  TCustomGridFixed = class;
  TCustomGridView = class;

  TGridEditStyle = (geSimple, geEllipsis, gePickList, geDataList, geUserDefine);

  TGridCheckKind = (gcNone, gcCheckBox, gcRadioButton, gcUserDefine);
  TGridCheckStyle = (csFlat, cs3D, csWin95);

{ TGridHeaderSection }

  {
    Public Properties:

    BoundsRect -        Returns the section bounds.
    ColumnIndex -       The index of the corresponding column. For a multilevel
                        section, this is the index corresponding to the column
                        index of the rightmost subsection.
    FirstColumnIndex -  Returns the index of the leftmost column of a section.
    FixedColumn -       Specifies whether the section is sizeable.
    Header -            Indicates the header to which the section instance
                        belongs.
    Level -             Indicates section level. The top level section is level 0.
    Parent -            Refers the section one level up that contains the section.
                        The top level section has no parent section.
    ResizeColumnIndex - Returns the index of the column whose size is changed
                        when the width of the section is changed with the mouse.
                        For a multilevel section, this corresponds to the index
                        of the rightmost subsection.
    Visible -           Indicates the section is visible. A section is visible
                        if at least one of the subsections is visible or if the
                        corresponding column is visible.
    Width -             The width of the section. Equal to the sum of the width
                        of the subsections or the width of the corresponding
                        column.

    Published Properties:

    Alignment -         Determines how the text is aligned within the section.
    Caption -           Specified the title of the section. If not specified,
                        the column caption is used.
    Sections -          Lists the subsections.
    WordWrap -          Determines whether the section inserts soft carriage
                        returns so text wraps at the right margin of the section.
  }

  TGridHeaderSection = class(TCollectionItem)
  private
    FSections: TGridHeaderSections;
    FCaption: string;
    FAlignment: TAlignment;
    FWordWrap: Boolean;
    FBoundsRect: TRect;
    FColumnIndex: Integer;
    function IsSectionsStored: Boolean;
    function GetAllowClick: Boolean;
    function GetBoundsRect: TRect;
    function GetDisplayText: string;
    function GetFirstColumnIndex: Integer;
    function GetFixedColumn: Boolean;
    function GetHeader: TCustomGridHeader;
    function GetLevel: Integer;
    function GetParent: TGridHeaderSection;
    function GetParentSections: TGridHeaderSections;
    function GetResizeColumnIndex: Integer;
    function GetSections: TGridHeaderSections;
    function GetVisible: Boolean;
    function GetWidth: Integer;
    procedure ReadWidth(Reader: TReader);
    procedure SetAlignment(Value: TAlignment);
    procedure SetCaption(const Value: string);
    procedure SetSections(Value: TGridHeaderSections);
    procedure SetWordWrap(Value: Boolean);
  protected
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
  public
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AllowClick: Boolean read GetAllowClick;
    property BoundsRect: TRect read GetBoundsRect;
    property ColumnIndex: Integer read FColumnIndex;
    property DisplayText: string read GetDisplayText;
    property FirstColumnIndex: Integer read GetFirstColumnIndex;
    property FixedColumn: Boolean read GetFixedColumn;
    property Header: TCustomGridHeader read GetHeader;
    property Level: Integer read GetLevel;
    property Parent: TGridHeaderSection read GetParent;
    property ParentSections: TGridHeaderSections read GetParentSections;
    property ResizeColumnIndex: Integer read GetResizeColumnIndex;
    property Visible: Boolean read GetVisible;
    property Width: Integer read GetWidth;
  published
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property Caption: string read FCaption write SetCaption;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property Sections: TGridHeaderSections read GetSections write SetSections stored IsSectionsStored;
  end;

{ TGridHeaderSections }

  {
    Public Methods:

    Add -          Creates a new TGridHeaderSection instance and adds it to the
                   Sections array.

    Public Properties:

    Header -       Indicates the header to which the sections instance belongs.
    MaxColumn -    Indicates the maximum column index of the subsections.
    MaxLevel -     Indicates the maximum level of the subsections. The top level
                   section is level 0.
    OwnerSection - Refers the section one level up that contains the section.
                   The top level section has no owner.
    Sections -     Lists the subsections.
  }

  TGridHeaderSections = class(TCollection)
  private
    FHeader: TCustomGridHeader;
    FOwnerSection: TGridHeaderSection;
    function GetMaxColumn: Integer;
    function GetMaxLevel: Integer;
    function GetSection(Index: Integer): TGridHeaderSection;
    procedure SetSection(Index: Integer; Value: TGridHeaderSection);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
  public
    constructor Create(AHeader: TCustomGridHeader; AOwnerSection: TGridHeaderSection); virtual;
    function Add: TGridHeaderSection;
    property Header: TCustomGridHeader read FHeader;
    property MaxColumn: Integer read GetMaxColumn;
    property MaxLevel: Integer read GetMaxLevel;
    property OwnerSection: TGridHeaderSection read FOwnerSection;
    property Sections[Index: Integer]: TGridHeaderSection read GetSection write SetSection; default;
  end;

{ TGridHeader }

  {
    Public Properties:

    Grid -          Indicates the grid control to which the header instance
                    belongs.
    MaxColumn -     Indicates the maximum column index of the subsections.
    MaxLevel -      Indicates the maximum level of the subsections. The top
                    level section is level 0.

    Published Properties:

    AutoHeight -    Determines whether the height of the header sections
                    automatically resizes to accommodate the text.
    Color -         Specifies the background color of the header.
    Images -        Provides a list of icon images to display for sections
                    of the header. To determinate image index of the section
                    use OnGetHeaderImage event.
    Flat -          Specifies whether the sections of the header are flat.
    Font -          Controls the attributes of text written on the header.
    GridColor -     Determines where a header looks for its color information.
    GridFont -      Determines where a header looks for its font information
    PopupMenu -     User pop-up menu of the header. For more information
                    see TGridView.DefaultHeaderMenu property.
    SectionHeight - Determines the height of the header sections.
    Sections -      Lists the header sections (column headings).
  }

  TCustomGridHeader = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FSections: TGridHeaderSections;
    FSectionHeight: Integer;
    FAutoHeight: Boolean;
    FColor: TColor;
    FGridColor: Boolean;
    FFont: TFont;
    FGridFont: Boolean;
    FImages: TImageList;
    FImagesLink: TChangeLink;
    FFlat: Boolean;
    FPopupMenu: TPopupMenu;
    FOnChange: TNotifyEvent;
    procedure ImagesChange(Sender: TObject);
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    function IsSectionHeightStored: Boolean;
    function IsSectionsStored: Boolean;
    procedure FontChange(Sender: TObject);
    function GetHeight: Integer;
    function GetMaxColumn: Integer;
    function GetMaxLevel: Integer;
    function GetWidth: Integer;
    procedure ReadFullSynchronizing(Reader: TReader);
    procedure SetAutoHeight(Value: Boolean);
    procedure SetColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetGridColor(Value: Boolean);
    procedure SetGridFont(Value: Boolean);
    procedure SetImages(Value: TImageList);
    procedure SetSections(Value: TGridHeaderSections);
    procedure SetSectionHeight(Value: Integer);
    procedure SetPopupMenu(const Value: TPopupMenu);
  protected
    procedure Change; virtual;
    procedure DefineProperties(Filer: TFiler); override;
    function GetOwner: TPersistent; override;
    procedure GridColorChanged(NewColor: TColor); virtual;
    procedure GridFontChanged(NewFont: TFont); virtual;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    function GetNamePath: string; override;
    procedure SynchronizeSections;
    procedure UpdateSections; virtual;
    property Grid: TCustomGridView read FGrid;
    property Height: Integer read GetHeight;
    property MaxColumn: Integer read GetMaxColumn;
    property MaxLevel: Integer read GetMaxLevel;
    property Width: Integer read GetWidth;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Color: TColor read FColor write SetColor stored IsColorStored default clBtnFace;
    property Images: TImageList read FImages write SetImages;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property GridColor: Boolean read FGridColor write SetGridColor default False;
    property GridFont: Boolean read FGridFont write SetGridFont default True;
    property PopupMenu: TPopupMenu read FPopupMenu write SetPopupMenu;
    property Sections: TGridHeaderSections read FSections write SetSections stored IsSectionsStored;
    property SectionHeight: Integer read FSectionHeight write SetSectionHeight stored IsSectionHeightStored;
  end;

  TGridHeader = class(TCustomGridHeader)
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

{ TGridColumn }

  {
    Public Properties:

    Caption2 -       Return a fully qualified column header for a multi-level
                     header.
                     The result string consists of all top-level section headers,
                     separated by a dash.
    Columns -        Refers the list that contains the column.
    HeaderSection -  Refers to the header section of the column.

    Published Properties:

    AlignEdit -      Determines whether the text of the inplace editor will be
                     aligned same as the text of the column.
    Alignment -      Determines how the text is aligned within the cells.
    AllowClick -     Allows the column section to respond to mouse clicks at
                     runtime.
    AllowEdit -      Determines whether the inplace editor can be displayed
                     for a column.
    Caption -        Indicates the Title that represents the column header.
    CheckAlignment - Determines how the check box is aligned within the cell text.
    CheckKind -      Determines the style of the check box:
                       gcNone -        No check box.
                       gcCheckBox -    Like TCheckBox.
                       gcRadioButton - Like TRadioButton.
                       gcUserDefine -  User defined check image.
    DefWidth -       The real width of the column.
    EditMask -       Specifies the mask that represents what text is valid for
                     the inplace editor.
    EditStyle -      Determines the style of the inplace editor:
                       geSimple - Edit line.
                       geEllipsis - Edit line with ellipsis button.
                       gePickList - Edit line with drop-down list button.
                       geDataList - Edit line with lookup list button.
    EditWordWrap -   Determines whether the inplace editor inserts soft carriage
                     returns so text wraps at the right margin of the cell.
    FixedSize -      Specifies whether the column is sizeable.
    MaxLength -      Specifies the maximum number of characters the user can
                     enter into the inplace editor.
    MaxWidth -       Specifies the maximum width (in pixels) of the column.
    MinWidth -       Specifies the minimal width (in pixels) of the column.
    PickList -       Contains the lines of text in the inplace editor drop-down
                     list.
    ReadOnly  -      Determines whether data displayed in the column is editable.
    TabStop -        Determines if the user can tab to a column.
    Visible -        Specifies whether the column is visible in the grid.
    WantReturns -    Determines whether the user can insert return characters
                     into the text in the inplace editor.
    WordWrap -       Determines whether the grid inserts soft carriage returns
                     so text wraps at the right margin of the cell.
    Width -          The width of the column or 0 for invisible column.
  }

  TGridEditWordWrap = (ewAuto, ewEnabled, ewDisabled);

  TGridColumnClass = class of TCustomGridColumn;

  TCustomGridColumn = class(TCollectionItem)
  private
    FColumns: TGridColumns;
    FCaption: string;
    FWidth: Integer;
    FMinWidth: Integer;
    FMaxWidth: Integer;
    FFixedSize: Boolean;
    FMaxLength: Integer;
    FAlignment: TAlignment;
    FReadOnly: Boolean;
    FWantReturns: Boolean;
    FWordWrap: Boolean;
    FEditStyle: TGridEditStyle;
    FEditMask: string;
    FEditWordWrap: TGridEditWordWrap;
    FCheckKind: TGridCheckKind;
    FCheckAlignment: TAlignment;
    FTabStop: Boolean;
    FVisible: Boolean;
    FPickList: TStrings;
    FAllowClick: Boolean;
    FTag: Integer;
    FAlignEdit: Boolean;
    FAllowEdit: Boolean;
    FDefaultPopup: Boolean;
    function GetCaption2: string;
    function GetEditAlignment: TAlignment;
    function GetGrid: TCustomGridView;
    function GetHeaderSection: TGridHeaderSection;
    function GetPickList: TStrings;
    function GetPickListCount: Integer;
    function GetWidth: Integer;
    function IsPickListStored: Boolean;
    procedure ReadMultiline(Reader: TReader);
    procedure SetAlignEdit(Value: Boolean);
    procedure SetAllowEdit(Value: Boolean);
    procedure SetCheckAlignment(Value: TAlignment);
    procedure SetCheckKind(Value: TGridCheckKind);
    procedure SetEditWordWrap(Value: TGridEditWordWrap);
    procedure SetMaxWidth(Value: Integer);
    procedure SetMinWidth(Value: Integer);
    procedure SetPickList(Value: TStrings);
    procedure SetTabStop(Value: Boolean);
    procedure SetWantReturns(Value: Boolean);
    procedure SetWordWrap(Value: Boolean);
  protected
    FWidthLock: Integer;
    procedure DefineProperties(Filer: TFiler); override;
    function GetDisplayName: string; override;
    procedure SetAlignment(Value: TAlignment); virtual;
    procedure SetCaption(const Value: string); virtual;
    procedure SetEditMask(const Value: string); virtual;
    procedure SetEditStyle(Value: TGridEditStyle); virtual;
    procedure SetFixedSize(Value: Boolean); virtual;
    procedure SetMaxLength(Value: Integer); virtual;
    procedure SetReadOnly(Value: Boolean); virtual;
    procedure SetVisible(Value: Boolean); virtual;
    procedure SetWidth(Value: Integer); virtual;
  public
    constructor Create(Collection: TCollection); override;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AlignEdit: Boolean read FAlignEdit write SetAlignEdit default False;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property AllowClick: Boolean read FAllowClick write FAllowClick default True;
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit default True;
    property Caption: string read FCaption write SetCaption;
    property Caption2: string read GetCaption2;
    property CheckAlignment: TAlignment read FCheckAlignment write SetCheckAlignment default taLeftJustify;
    property CheckKind: TGridCheckKind read FCheckKind write SetCheckKind default gcNone;
    property Columns: TGridColumns read FColumns;
    property DefaultPopup: Boolean read FDefaultPopup write FDefaultPopup default True;
    property EditAlignment: TAlignment read GetEditAlignment;
    property EditMask: string read FEditMask write SetEditMask;
    property EditStyle: TGridEditStyle read FEditStyle write SetEditStyle default geSimple;
    property EditWordWrap: TGridEditWordWrap read FEditWordWrap write SetEditWordWrap default ewAuto;
    property FixedSize: Boolean read FFixedSize write SetFixedSize;
    property Grid: TCustomGridView read GetGrid;
    property HeaderSection: TGridHeaderSection read GetHeaderSection;
    property MaxLength: Integer read FMaxLength write SetMaxLength default 0;
    property MaxWidth: Integer read FMaxWidth write SetMaxWidth default 10000;
    property MinWidth: Integer read FMinWidth write SetMinWidth default 0;
    property PickList: TStrings read GetPickList write SetPickList stored IsPickListStored;
    property PickListCount: Integer read GetPickListCount;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property TabStop: Boolean read FTabStop write SetTabStop default True;
    property Tag: Integer read FTag write FTag default 0;
    property Visible: Boolean read FVisible write SetVisible;
    property WantReturns: Boolean read FWantReturns write SetWantReturns default False;
    property Width: Integer read GetWidth write SetWidth stored False;
    property WordWrap: Boolean read FWordWrap write SetWordWrap default False;
    property DefWidth: Integer read FWidth write SetWidth;
  end;

  TGridColumn = class(TCustomGridColumn)
  published
    property AlignEdit;
    property Alignment;
    property AllowClick;
    property AllowEdit;
    property Caption;
    property CheckAlignment;
    property CheckKind;
    property DefaultPopup;
    property EditMask;
    property EditStyle;
    property EditWordWrap;
    property FixedSize default False;
    property MaxLength;
    property MaxWidth;
    property MinWidth;
    property PickList;
    property ReadOnly;
    property TabStop;
    property Tag;
    property Visible default True;
    property WantReturns;
    property Width default 64;
    property WordWrap;
    property DefWidth default 64;
  end;

{ TGridColumns }

  {
    Public Methods:

    Add -     Creates a new TGridColumn instance and adds it to the
              Columns array.

    Public Properties:

    Columns - Lists the columns in the collection.
    Layout -  Returns a string with a comma-separated list of column widths.
              Suitable for saving column layouts in the registry or INI file.
    Grid -    Indicates the grid control to which the columns instance
              belongs.
  }

  TGridColumns = class(TCollection)
  private
    FGrid: TCustomGridView;
    FOnChange: TNotifyEvent;
    function GetColumn(Index: Integer): TGridColumn;
    function GetLayout: string;
    procedure SetColumn(Index: Integer; Value: TGridColumn);
    procedure SetLayout(const Value: string);
  protected
    function GetOwner: TPersistent; override;
    procedure Update(Item: TCollectionItem); override;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    function Add: TGridColumn;
    property Columns[Index: Integer]: TGridColumn read GetColumn write SetColumn; default;
    property Grid: TCustomGridView read FGrid;
    property Layout: string read GetLayout write SetLayout;
  end;

{ TGridRows }

  {
    Public Properties:

    Grid -       Indicates the grid control to which the rows instance belongs.
    MaxCount -   The maximum number of rows allowed in a grid. Depends on
                 row height.

    Published Properties:

    AutoHeight - Determines whether the height of the rows automatically
                 resizes to accommodate the grid font.
    Count -      Specifies the number of rows in the grid.
    Height -     Gives the height (in pixels) of all rows in the grid.
  }

  TCustomGridRows = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FCount: Integer;
    FHeight: Integer;
    FAutoHeight: Boolean;
    FOnChange: TNotifyEvent;
    function GetMaxCount: Integer;
    function IsHeightStored: Boolean;
    procedure SetAutoHeight(Value: Boolean);
    procedure SetHeight(Value: Integer);
  protected
    procedure Change; virtual;
    procedure SetCount(Value: Integer); virtual;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property AutoHeight: Boolean read FAutoHeight write SetAutoHeight default True;
    property Count: Integer read FCount write SetCount default 0;
    property Grid: TCustomGridView read FGrid;
    property Height: Integer read FHeight write SetHeight stored IsHeightStored;
    property MaxCount: Integer read GetMaxCount;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGridRows = class(TCustomGridRows)
  published
    property AutoHeight;
    property Count;
    property Height;
  end;

{ TGridFixed }

  {
    Public Properties:

    Grid -       Indicates the grid control to which the fixed instance belongs.

    Published Properties:

    Color -       Specifies the background color of the fixed columns.
    Count -       Specifies the number of columns on the left of the grid that
                  cannot be scrolled.
    Flat -        Specifies whether the fixed cells are flat.
    Font -        Controls the attributes of text written in the fixed columns.
    GridColor -   Determines where a fixed columns looks for its color information.
    GridFont -    Determines where a fixed columns looks for its font information.
    ShowDivider - Determines whether to draw a vertical dividing line between
                  fixed cells and scrollable cells.
  }

  TCustomGridFixed = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FCount: Integer;
    FColor: TColor;
    FGridColor: Boolean;
    FFont: TFont;
    FGridFont: Boolean;
    FFlat: Boolean;
    FShowDivider: Boolean;
    FOnChange: TNotifyEvent;
    function IsColorStored: Boolean;
    function IsFontStored: Boolean;
    procedure FontChange(Sender: TObject);
    procedure SetColor(Value: TColor);
    procedure SetFlat(Value: Boolean);
    procedure SetFont(Value: TFont);
    procedure SetGridColor(Value: Boolean);
    procedure SetGridFont(Value: Boolean);
    procedure SetShowDivider(Value: Boolean);
  protected
    procedure Change; virtual;
    procedure GridColorChanged(NewColor: TColor); virtual;
    procedure GridFontChanged(NewFont: TFont); virtual;
    procedure SetCount(Value: Integer); virtual;
  public
    constructor Create(AGrid: TCustomGridView); virtual;
    destructor Destroy; override;
    procedure Assign(Source: TPersistent); override;
    property Color: TColor read FColor write SetColor stored IsColorStored default clBtnFace;
    property Count: Integer read FCount write SetCount default 0;
    property Flat: Boolean read FFlat write SetFlat default True;
    property Font: TFont read FFont write SetFont stored IsFontStored;
    property Grid: TCustomGridView read FGrid;
    property GridColor: Boolean read FGridColor write SetGridColor default False;
    property GridFont: Boolean read FGridFont write SetGridFont default True;
    property ShowDivider: Boolean read FShowDivider write SetShowDivider default True;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
  end;

  TGridFixed = class(TCustomGridFixed)
  published
    property Color;
    property Count;
    property Flat;
    property Font;
    property GridColor;
    property GridFont;
    property ShowDivider;
  end;

{ TGridCell }

  TGridCell = record
    Col: Longint;
    Row: Longint;
  end;

{ TGridScrollBar }

  {
    Public Properties:

    Grid -     Indicates the grid control to which the scroll bar
               instance belongs.
    Kind -     Specifies whether the scroll bar is horizontal or vertical.
    LineStep - Determines how much Position changes when the user clicks
               the arrow buttons on the scroll bar or presses the arrow
               keys on the keyboard.
    LineSize - Determines how far the grid moves when the user clicks one
               of the small end arrows on the scroll bar.
    Min -      Specifies the minimum position represented by the scroll bar.
    Max -      Specifies the maximum position represented by the scroll bar.
    PageStep - Determines how much Position changes when the user clicks
               the scroll bar on either side of the thumb tab or presses
               PgUp or PgDn.
    Position - Indicates the current position of the scroll bar.

    Published Properties:

    Tracking - Determines whether the grid moves before the thumb tab
               is released.
    Visible -  Determines whether the scroll bar appears on the grid.
  }

  TGridScrollEvent = procedure(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer) of object;

  TGridScrollBar = class(TPersistent)
  private
    FGrid: TCustomGridView;
    FKind: TScrollBarKind;
    FPosition: Integer;
    FMin: Integer;
    FMax: Integer;
    FPageStep: Integer;
    FLineStep: Integer;
    FLineSize: Integer;
    FTracking: Boolean;
    FVisible: Boolean;
    FUpdateLock: Integer;
    FOnScroll: TGridScrollEvent;
    FOnChange: TNotifyEvent;
    FOnChangeParams: TNotifyEvent;
    procedure SetLineSize(Value: Integer);
    procedure SetLineStep(Value: Integer);
    procedure SetMax(Value: Integer);
    procedure SetMin(Value: Integer);
    procedure SetPageStep(Value: Integer);
    procedure SetVisible(Value: Boolean);
  protected
    FBarCode: Integer;
    procedure Change; virtual;
    procedure ChangeParams; virtual;
    procedure Scroll(ScrollCode: Integer; var ScrollPos: Integer); virtual;
    procedure ScrollMessage(var Message: TWMScroll); virtual;
    procedure SetParams(AMin, AMax, APageStep, ALineStep: Integer); virtual;
    procedure SetPosition(Value: Integer);
    procedure SetPositionEx(Value: Integer; ScrollCode: Integer); virtual;
    procedure Update; virtual;
  public
    constructor Create(AGrid: TCustomGridView; AKind: TScrollBarKind); virtual;
    procedure Assign(Source: TPersistent); override;
    procedure LockUpdate;
    procedure UnLockUpdate;
    property Grid: TCustomGridView read FGrid;
    property Kind: TScrollBarKind read FKind;
    property LineStep: Integer read FLineStep write SetLineStep;
    property LineSize: Integer read FLineStep write SetLineSize;
    property Max: Integer read FMax write SetMax;
    property Min: Integer read FMin write SetMin;
    property PageStep: Integer read FPageStep write SetPageStep;
    property Position: Integer read FPosition write SetPosition;
    property UpdateLock: Integer read FUpdateLock;
    property OnChange: TNotifyEvent read FOnChange write FOnChange;
    property OnChangeParams: TNotifyEvent read FOnChangeParams write FOnChangeParams;
    property OnScroll: TGridScrollEvent read FOnScroll write FOnScroll;
  published
    property Tracking: Boolean read FTracking write FTracking default True;
    property Visible: Boolean read FVisible write SetVisible default True;
  end;

{ TGridListBox }

  TGridListBox = class(TCustomListBox)
  private
    FGrid: TCustomGridView;
    FSearchText: String;
    FSearchTime: Longint;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure CreateWnd; override;
    procedure KeyPress(var Key: Char); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
  public
    constructor Create(AOwner: TComponent); override;
    property Grid: TCustomGridView read FGrid;
  end;

{ TGridEdit }

  {
    Protected Methods:

    GetDropList -      Creates the popup list instance.

    Public Methods:

    CloseUp -          Call CloseUp to programmatically close the popup list
                       of the edit. The Accept parameter determines whether to
                       modify the cell value with the value that corresponds to
                       the selected value in the drop-down list.
    DropDown -         Opens popup list so that the user can choose a lookup
                       value. Only for editor with style gePickList, geDataList.
    Press -            Use Press to handle button presses. Called when the user
                       hit Ctrl+Enter or presses a button with the mouse. By
                       default generates an OnEditButtonPress event.
    SelectNext -       Use SelectNext to select the next popup list value.
                       Called when user hit Ctrl+Enter or double click on editor
                       with closed popup list. By default generates an
                       OnEditSelectNext event.

    Public Properties:

    DropDownCount -    Specifies the number of lines displayed in the popup list.
    DropListVisible -  Determines whether the popup list is currently in the
                       open (dropped-down) position.
    EditStyle -        Indicates the current style of the editor:
                         geSimple -   Edit line.
                         geEllipsis - Edit line with ellipsis button.
                         gePickList - Edit line with drop-down list button.
                         geDataList - Edit line with lookup list button.
    Grid -             Indicates the grid control to which the editor instance
                       belongs.
    PickList -         Indicated the current popup list instance.
    WantReturns -      Determines whether the user can insert return characters
                       into the text in the editor.
    WordWrap -         Determines whether the editor inserts soft carriage
                       returns so text wraps at the right margin of the edit.
  }

  TGridEditClass = class of TCustomGridEdit;

  TCustomGridEdit = class(TCustomMaskEdit)
  private
    FGrid: TCustomGridView;
    FClickTime: Longint;
    FEditStyle: TGridEditStyle;
    FWantReturns: Boolean;
    FWordWrap: Boolean;
    FDropDownCount: Integer;
    FDropListVisible: Boolean;
    FCloseUpCount: Integer;
    FPressCount: Integer;
    FPickList: TGridListBox;
    FActiveList: TWinControl;
    FButtonWidth: Integer;
    FButtonTracking: Boolean;
    FButtonPressed: Boolean;
    FButtonHot: Boolean;
    FDefocusing: Boolean;
    FAlignment: TAlignment;
    function GetButtonRect: TRect;
    function GetClosingUp: Boolean;
    function GetLineCount: Integer;
    function GetPressing: Boolean;
    function GetVisible: Boolean;
    procedure ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
    procedure SetAlignment(Value: TAlignment);
    procedure SetButtonWidth(Value: Integer);
    procedure SetDropListVisible(Value: Boolean);
    procedure SetEditStyle(Value: TGridEditStyle);
    procedure SetWordWrap(Value: Boolean);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMPaste(var Message); message WM_PASTE;
    procedure WMCut(var Message); message WM_CUT;
    procedure WMClear(var Message); message WM_CLEAR;
    procedure WMUndo(var Message); message WM_UNDO;
    procedure WMCancelMode(var Message: TMessage); message WM_CANCELMODE;
    procedure WMKillFocus(var Message: TMessage); message WM_KILLFOCUS;
    procedure WMWindowPosChanged(var Message: TWMWindowPosChanged); message WM_WINDOWPOSCHANGED;
    procedure WMPaint(var Message: TWMPaint); message WM_PAINT;
    procedure WMLButtonDown(var Message: TWMLButtonDown); message WM_LBUTTONDOWN;
    procedure WMLButtonDblClk(var Message: TWMLButtonDblClk); message WM_LBUTTONDBLCLK;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMShowingChanged(var Message: TMessage); message CM_SHOWINGCHANGED;
    procedure WMContextMenu(var Message: TMessage); message WM_CONTEXTMENU;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMMouseEnter(var Message: TMessage); message CM_MOUSEENTER;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
  protected
    procedure Change; override;
    procedure CreateParams(var Params: TCreateParams); override;
    procedure DblClick; override;
    function DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean; override;
    function EditCanModify: Boolean; override;
    function GetDropList: TWinControl; virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure KeyUp(var Key: Word; Shift: TShiftState); override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure PaintButton(DC: HDC; Rect: TRect); virtual;
    procedure PaintWindow(DC: HDC); override;
    procedure StartButtonTracking(X, Y: Integer);
    procedure StepButtonTracking(X, Y: Integer);
    procedure StopButtonTracking;
    procedure UpdateBounds(Showing, ScrollCaret: Boolean); virtual;
    procedure UpdateColors; virtual;
    procedure UpdateContents; virtual;
    procedure UpdateList; virtual;
    procedure UpdateListBounds; virtual;
    procedure UpdateListItems; virtual;
    procedure UpdateListValue(Accept: Boolean); virtual;
    procedure UpdateStyle; virtual;
    procedure WndProc(var Message: TMessage); override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure CloseUp(Accept: Boolean); virtual;
    procedure Deselect;
    procedure DropDown;
    procedure Invalidate; override;
    procedure InvalidateButton;
    procedure Hide;
    procedure Press;
    procedure SelectNext;
    procedure SetFocus; override;
    procedure Show;
    property ActiveList: TWinControl read FActiveList write FActiveList;
    property Alignment: TAlignment read FAlignment write SetAlignment default taLeftJustify;
    property ButtonRect: TRect read GetButtonRect;
    property ButtonWidth: Integer read FButtonWidth write SetButtonWidth;
    property ClosingUp: Boolean read GetClosingUp;
    property Color;
    property DropDownCount: Integer read FDropDownCount write FDropDownCount;
    property DropListVisible: Boolean read FDropListVisible write SetDropListVisible;
    property EditStyle: TGridEditStyle read FEditStyle write SetEditStyle;
    property Font;
    property Grid: TCustomGridView read FGrid;
    property LineCount: Integer read GetLineCount;
    property MaxLength;
    property PickList: TGridListBox read FPickList;
    property Pressing: Boolean read GetPressing;
    property Visible: Boolean read GetVisible;
    property WantReturns: Boolean read FWantReturns write FWantReturns;
    property WordWrap: Boolean read FWordWrap write SetWordWrap;
  end;

  TGridEdit = class(TCustomGridEdit);

{ TGridTipsWindow }

  TGridTipsWindowClass = class of TGridTipsWindow;

  TGridTipsWindow = class(THintWindow)
  private
    FGrid: TCustomGridView;
    procedure CMTextChanged(var Message: TMessage); message CM_TEXTCHANGED;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
  protected
    procedure CreateParams(var Params: TCreateParams); override;
    procedure NCPaint(DC: HDC); override;
    procedure Paint; override;
  public
    constructor Create(AOwner: TComponent); override;
    procedure ActivateHint(Rect: TRect; const AHint: string); override;
    procedure ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer); override;
    function CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect; override;
  end;

{ TGridFindDialog }

  TGridFindDialog = class(TFindDialog)
  private
    FVisible: Boolean;
  protected
    procedure DoClose; override;
    procedure DoShow; override;
  public
    procedure ShowModal(FindAsItemNo: Boolean = False);
    property Visible: Boolean read FVisible;
  end;

{ TCustomGridView }

  {
    Protected Methods:

    CancelCellTips -       Cancels the display of a hint for a cell.
    CellClick -            Respond to user cell click.
    Change -               Generates an OnChange event. Called after the
                           selected cell or selected state change
    Changing -             Generates an OnChanging event. Called before the
                           selected cell or selected state change.
    CheckClick -           Generates an OnCheckClick event. Called when user
                           click the check box with mouse or the SPACE key.
    ColumnSizeToFit -      Generates an OnColumnSizeToFit event. Called when
                           the user double-clicks the right side of the header
                           before automatically change the width of the column
                           to fit its contents.
    ColumnResize -         Generates an OnColumnResize event. Called after
                           resizing a column.
    ColumnResizing -       Generates an OnColumnResizing event. Called when the
                           column is resized with the mouse.
    CreateColumn -         Creates the TGridColumn object. Called internally
                           to add new column to the Columns list. To define you
                           own column class overwrite the GetColumnClass method.
    CreateColumns -        Creates the TGridColumns object that stores the
                           column descriptors for the grid. Called internally
                           to create Columns instance. Overwrite this method if
                           you need to substitute a descendant of TGridColumns
                           to manage your own column descriptors.
    CreateEdit -           Creates the inplace editor that allows the cells of
                           the grid to be edited. Called internally the first
                           time the grid goes into edit mode. To define you own
                           class of inplace editor for the specified cell
                           overwrite GetEditClass method.
    CreateFixed -          Creates the TGridFixed object. Called internally
                           to create Fixed instance. Overwrite this method if
                           you need to substitute a descendant of TGridFixed.
    CreateHeader -         Creates the TGridHeader object. Called internally
                           to create Header instance. Overwrite this method if
                           you need to substitute a descendant of TGridHeader.
    CreateHeaderSection -  Creates the TGridHeaderSection object. Called
                           internally to add new section to the Header.
                           Overwrite this method if you need to substitute a
                           descendant of TGridHeaderSection.
    CreateRows -           Creates the TGridRows object. Called internally
                           to create Rows instance. Overwrite this method if
                           you need to substitute a descendant of TGridRows.
    CreateScrollBar -      Creates the TGridScrollBar object. Called internally
                           to create HorzScrollBar and VertScrollBar instances.
                           Overwrite this method if you need to substitute a
                           descendant of TGridScrollBar.
    EditButtonPress -      Generates an OnEditButtonPress event. Called when
                           the user hit Ctrl+Enter in the inplace editor or
                           presses ellipsis button with the mouse.
    EditCanAcceptKey -     Generates an OnEditCanAcceptKey event. Provides a
                           simple screening of keyboard input while the user
                           is editing the contents of a cell.
    EditCanceled -         Generates an OnEditCanceled event. Called when the
                           inplace editor is hidden by pressing ESCAPE key.
    EditCanModify -        Generates an OnEditModify event. Called before any
                           text changes in the inplace editor.
    EditCanShow -          Generates an OnEditCanShow event. Determines whether
                           the current cell in the grid can be edited.
    EditCanUndo -          Determines whether the current cell in the grid
                           can handle Undo operation.
    EditChange -           Generates an OnEditChange event. Called when the text
                           in the inplace editor has changed.
    EditCloseUp -          Generates an OnEditCloseUp and OnEditCloseUpEx events.
                           Called when the drop-down list closes up due to
                           select list item.
    EditSelectNext -       Generates an OnEditSelectNext event. Called when user
                           hit Ctrl+Enter or double click on inplace editor with
                           closed popup list.
    GetCellImage -         Generages an OnGetCellImage event to determine
                           if there is an image for the specified cell.
                           Returns image index or -1 is cell has no image.
    GetCellImageRect -     Returns the coordinates of a image in the grid.
    GetCellHintRect -      Cell hint is shown only if the cell text gets out of
                           hint rectangle. By default returns the position of
                           the cell inplace editor.
    GetCellText -          Generated OnGetCellText event.
    GetCellColors -        Sets the default color, brush and font for the
                           specified cell on the specified canvas then
                           generates OnGetCellColors event.
    GetCellTextBounds -    Returns the text coordinates of the specified cell
                           in the grid.
    GetCellTextIndent -    Generates OnGetCellTextIndent event. By default,
                           the horizontal indent of the text is 2 pixels for
                           cells with check box or an image and TextLeftIndent
                           for other cells.
    GetCheckAlignment -    Generates OnGetCheckAlignment event.
    GetCheckImage -        Generates OnGetCheckImage event. Returns the image
                           index for specified cell or -1 if cell have no image.
    GetCheckKind -         Generages an OnGetCheckKind event. Returns the type
                           of the check box for specified cell:
                             gcNone -        No check box.
                             gcCheckBox -    Like TCheckBox.
                             gcRadioButton - Like TRadioButton.
                             gcUserDefine -  User defined check box image.
    GetCheckRect -         Returns the coordinates of a check box in the grid.
                           Returns (0,0,0,0) if cell dows not have a check box.
    GetCheckState -        Generates OnGetCheckState and OnGetCheckStateEx events.
                           Returns the state of a check box.
    GetCheckStateEx -      Returns the state of a check box and its enabled state.
    GetColumnClass -       Returns the class type of the column. Descendant
                           classes can override this method to define their own
                           column class (for example, a TDBGridColumn).
    GetCursorCell -        Finds a cell in the specified direction where the
                           cursor can be positioned. Direction can be:
                             goLeft -       One column to the left.
                             goRight -      One column to the right.
                             goUp -         One row to the up.
                             goDown -       One row to the down.
                             goPageUp -     One page to the up.
                             goPageDown -   One page to the down.
                             goHome -       First cell of specified row.
                             goEnd -        Last cell of specified row.
                             goGridHome -   Upper left grid cell.
                             goGridEnd -    Bottom right grid cell.
                             goGridTop -    First row of the current column.
                             goGridBottom - Last row of the current column.
                             goSelect -     Nearest available cell.
                             goFirst -      First available cell in the grid.
                             goNext -       Next cell to the end of the grid.
                             goPrev -       Previous cell to the beginning of
                                            the grid.
    GetEditClass -         Returns the class type of the inplace editor for
                           the specified cell. Descendant classes can override
                           this method to define their own editor class (for
                           example, a TDBGridEdit).
    GetEditList -          Assigns the pick list items of the current column
                           (if present) to the drop-down list and generates
                           OnGetEditList event.
    GetEditListBounds -    Generates OnGetEditListBounds event.
    GetEditListIndex -     Generates OnGetEditListIndex event.
    GetEditMask -          Returns an input mask for the specified cell.
    GetEditStyle -         Indicates whether a specified cell has an ellipsis
                           button or drop-down arrow.
    GetEditText -          Returns the value of the specified cell formatted
                           for editing. By default returns cell text.
    GetFixedDividerColor - Returns the color of the dividing line of fixed and
                           scrollable columns depending on the background color
                           of the grid.
    GetFixedGridColor -    Returns the color of grid lines of fixed columns
                           depending on the background color of fixed columns.
    GetHeaderColors -      Sets the default header brush and font on the specified
                           canvas then generates OnGetHeaderColors event.
    GetHeaderImage -       Generages an OnGetHeaderImage event.
    GetSortArrowSize -     Calculates the size of the sort image depending on
                           the Windows theme.
    GetSortDirection -     Generates OnGetSortDirection event.
    GetSortImage -         Generates OnGetSortImage event.
    GetTextRect -          Calculates the coordinates of the text in the cell.
    GetTipsRect -          Calculates the coordinates of the cell hint in the
                           grid and generates OnGetTipsRect event.
    GetTipsText -          Generates OnGetTipsText event.
    GetTipsWindowClass -   Returns the class type of the hint window class.
    HeaderClick -          Generated OnHeaderClick event. Called when the user
                           clicks the header.
    HeaderClicking -       Generated OnHeaderClicking event. Called when the
                           user start clicking the header.
    HideCursor -           Hides the focus rectangle or inplace editor.
    HideEdit -             Hides the inplace editor.
    HideFocus -            Hides the focus rectangle.
    PaintCell -            Generates an OnDrawCell event and draws the cell
                           using DefaultDrawCell method.
    PaintCells -           Draws scrollable cells.
    PaintCheck -           Draws a check box.
    PaintFixed -           Draws fixed cells.
    PaintFixedGridLines -  Draws grid lines between fixed cells.
    PaintFocus -           Draws focus rectangle.
    PaintFreeField -       Draws the area outside the cells.
    PaintGridLines -       Draws grid lines between scrollable cells.
    PaintHeader -          Draws a header section.
    PaintHeaderBackground - Paints the header section background.
    PaintHeaders -         Draws the grid header.
    PaintResizeLine -      Draws a vertical resize line.
    PaintText -            Universal text drawing procedure. Used internally
                           to draw the cells, header and cell tips.
    SetEditText -          Generates an OnSetEditText event.
    ShowCursor -           Displays the focus rectangle or inplace editor.
    ShowEdit -             Displays the inplace editor when the grid enters
                           edit mode.
    ShowEditChar -         Displays the inplace editor, with its text set to
                           the specified character.
    ShowFocus -            Shows a focus rectangle.
    UpdateListBounds -     Calculates the bounds rectangle of drop-down list.
                           Called internally before inplace editor's drop-down
                           list is displayed.
    UpdateListItems -      Fill the drop-down item list and sets the index of
                           the selected item.
    UpdateListValue -      Calls EditCloseUp and sets selected item text to
                           the inplace edtior. Called internally when the
                           drop-down list closes up.

    Public Methods:

    AcquireFocus -         Changes focus to the grid, if possible. Returns False
                           if grid can not be focused.
    ApplyEdit -            Applies text changes from the inplace editor.
    ApplyEditText -        Calls SetEditText method and handles event exceptions.
                           Called internally before changing the current cell or
                           hiding inlace editor.
    CancelEdit -           Cancels editind, discards changes in the inplace
                           editor and hides it. Called when AlwaysEdit is
                           set to False and user has pressed the ESCAPE key.
    DefaultDrawCell -      Draws the contents of a cell in the grid.
    DefaultDrawHeader -    Draws the contents of a header section in the grid.
    FindText -             Searches text from selected cell up or down and sets
                           the cursor on the cell with the found text. Returns
                           True if the cell is found.
    GetCellAt -            Returns the cell located at a specified position
                           within the grid. If there is no cell located at
                           the specified position returns (-1, -1).
    GetCellRect -          Returns the coordinates of a cell in the grid.
    GetCellsRect -         Returns the coordinates of a cell range in
                           the grid.
    GetColumnAt -          Returns the column located at a specified position
                           within the grid. If there is no column located at
                           the specified position returns -1.
    GetColumnLeftRight -   Returns left and right coordinates of the column in
                           the grid.
    GetColumnMaxWidth -    Returns the maximum width of the text of the specified
                           column in visible rows. Called to automatically change
                           the width of column to fit its contents.
    GetColumnRect -        Returns the coordinates of the column in the grid.
    GetColumnsRect -       Returns the coordinates of the column range in
                           the grid.
    GetColumnsWidth -      Returns the width in pixels of the column range.
    GetEditRect -          Returns the coordinates of the inplace editor in the
                           grid. By default, it is equal to the rectangle of the
                           cell minus the check box and the image on the left.
    GetFirstImageColumn -  Returns the index of the first visible scrollable
                           column.
    GetFixedRect -         Returns the coordinates of the fixed columns in the
                           grid.
    GetFixedWidth -        Returns the width of the fixed columns in pixels.
    GetFocusRect -         Returns the coordinates of the focus rectangle in
                           the grid.
    GetGridHeight -        Returns the height of the visible cells in pixels.
    GetGridLineColor -     Generates OnGetGridColor event.
    GetGridOrigin -        Returns the offset in pixels of cells relative to the
                           upper left corner of the grid. Determined by the
                           position of the scrollers.
    GetGridRect -          Returns the rectangle of the visible cells in the
                           grid.
    GetHeaderHeight -      Same as Header.Height.
    GetHeaderRect -        Returns the coordinates of the header in the grid.
    GetHeaderSection -     Returns the header section of the specified level for
                           the specified column. If the level is -1, it returns
                           the lowest section.
    GetResizeSectionAt -   Returns the header section with the right margin
                           located at a specified position within the grid.
                           Called internally to find a section for resizing.
    GetRowAt -             Returns the row located at a specified position
                           within the grid. If there is no row located at
                           the specified position returns -1.
    GetRowRect -           Returns the coordinates of the row in the grid.
    GetRowsRect -          Returns the coordinates of the row range in the grid.
    GetRowsHeight -        Returns the height in pixels of the row range.
    GetRowTopBottom -      Returns top and bottom coordinates of the row in the
                           grid.
    GetSectionAt -         Returns the header section located at a specified
                           position within the grid. If there is no section
                           located at the specified position returns null.
    InvalidateCell -       Invalidates the region occupied by a cell.
    InvalidateCheck -      Invalidates the region occupied by a check box of
                           specified cell.
    InvalidateColumn -     Invalidates the region occupied by a column.
    InvalidateColumns -    Invalidates the region occupied by the column range.
    InvalidateEdit -       Invalidates the region occupied by the inplace editor
                           if visible.
    InvalidateFixed -      Invalidates the region occupied by a fixed columns.
    InvalidateFocus -      Invalidates the region occupied by a current cell.
    InvalidateGrid -       Invalidates the region occupied by the scrollable columns.
    InvalidateHeader -     Invalidates the region occupied by the header.
    InvalidateRect -       Invalidates the region occupied by specified rectangle.
    InvalidateRow -        Invalidates the region occupied by a row.
    InvalidateRows -       Invalidates the region occupied by a row range.
    InvalidateSection -    Invalidates the region occupied by the header
                           of the specified column.
    IsActiveControl -      Determines whether the grid or its inplace editor
                           has input focus.
    IsCellAcceptCursor -   Generates an OnCellAcceptCursor event.
    IsCellEditing -        Indicates whether the specified cell is currently
                           being edited.
    IsCellHighlighted -    Indicates whether the specified cell is selected
                           and focused and must be highlighted by color.
    IsCellHasCheck -       Indicates whether the specified cell has a check box.
    IsCellHasImage -       Indicates whether the specified cell has an image.
    IsCellFocused -        Indicates whether the specified cell is equal to
                           the focused cell.
    IsCellReadOnly -       Indicates whether the specified cell can be edited.
                           Generates an OnGetCellReadOnly event.
    IsCellValid -          Indicates that the column and row of the specified
                           cell are within the grid.
    IsCellValidEx -        Indicates that the column and row of the specified
                           cell are within the grid and column is visible.
    IsCellVisible -        Indicates whether the specified cell is currently
                           visible in the grid image.
    IsColumnVisible -      Indicates whether the specified column is currently
                           visible in the grid image.
    IsEvenRow -            Indicates whether the row of the specified cell
                           is even. Called internally to calculate cell colors.
    IsFixedVisible -       Indicates whether the fixed columns are visible.
                           Called internally while drawing grid.
    IsFocusAllowed -       Indicates whether the focus rectangle can be displayed.
    IsGridHintVisible -    Indicates whether to draw a hint for an empty grid.
    IsHeaderHasImage -     Indicates whether the specified header has an image.
    IsHeaderPressed -      Returns the state of specified header section.
                           Called internally when drawing a header.
    IsRowHighlighted -     Indicates whether the specified row is selected
                           and must be highlighted by color.
    IsRowVisible -         Indicates whether the specified row is currently
                           visible in the grid image.
    LockUpdate -           Disables grid redraw.
    MakeCellVisible -      Makes the specified cell visible to the user,
                           scrolling the grid as necessary.
    ResetEdit -            Resets the inplace editor. Called when AlwaysEdit is
                           set to True and user has pressed the ESCAPE key.
                           By default sends WM_UNDO to the inplace editor to
                           undo the last operation.  Override this method if you
                           need to perform additional actions after cancel editing.
                           For example, cancel changes to the data source.
    SetCursor -            Changes the current cell to a specified cell.
    SizeAllColumnsToFit -  Changes the width of all columns to fit their contents.
    SizeColumnToFit -      Changes the width of column to fit its contents.
    UnLockUpdate -         Enables grid redraw.
    UpdateCursor -         Verifies that the current cell can accept the cursor,
                           then updates the position of the cursor if necessary.
    UpdateEdit -           Creates an instance of the editor for the selected
                           cell, updates its style, font, text, then displays it.
    UpdateEditContents -   Refreshes the inplace editor. Use this method to
                           update the text of the editor if cells data have
                           changed during editing.
    UpdateFocus -          Sets focus to the grid, if possible, then show the
                           inplace edit, if necessary.
    UpdateSelection -      Determines whether the specified cell can be selected,
                           returns the closest available cell to select. Called
                           internaly before change the cursor position.
    UpdateText -           Same as ApplyEditText.

    Public Properties:

    AllowEdit -            Determines whether the user can edit data using
                           inplace editor. AlowEdit is ignored if RowSelect
                           is set to True.
    AllowSelect -          Determines whether the selected cell displays the
                           focus rectangle even when the grid does not have focus.
    AlwaysEdit -           Determines whether the grid is always in edit mode
                           and inplace editor is always visible. AlwaysEdit
                           is ignored if AlowEdit is set to False.
    AlwaysSelected -       Some strange property.
    CellFocused -          Indicates the focused cell in the grid.
    Cells -                Lists the strings for each cell in the grid.
    CellSelected -         Indicates the focused cell is in selected state.
    CheckBoxes -           Specifies whether check boxes appear next to the
                           cells in the grid.
    Checked -              Specifies whether the check box of the cell is checked.
    CheckEnabled -         Specifies whether the check box of the cell is enabled.
    CheckHeight -          Indicates the height of the check boxes. Do not modify
                           this property.
    CheckStyle -           Specifies the style of the check box border:
                             csFlat -  Flat border.
                             cs3D -    3D border.
                             csWin95 - Windows 95 style border.
                           If Windows themes are enabled, the check box is
                           displayed using the current theme.
    CheckWidth -           Indicates the width of check boxes. Do not modify
                           this property.
    Col -                  Specifies the index of the column that contains
                           the selected cell. Same as CellFocused.Col.
    ColumnClick -          Determines whether the column header behaves like
                           a button.
    Columns -              Describes the properties of the columns in the grid.
    ColumnsFullDrag -      Specifies whether columns are repainted when they
                           are dragged.
    ContextPopupCol -      Specifies the index of the column above which the
                           context menu is displayed.
    ContextPopupRow -      Specifies the index of the row above which the
                           context menu is displayed.
    CursorKeys -           Defines the keys that can be used to move the cursor
                           in a grid:
                             gkArrows -     Move the cursor with arrows.
                             gkTabs -       Move the cursor with TAB key.
                             gkReturn -     Move to the next cell after pressing
                                            the ENTER key.
                             gkMouse -      Select the cells with the mouse.
                             gkMouseMove -  Select the cells while mouse moving
                                            with the left mouse button is pressed.
                             gkMouseWheel - Select cells with the mouse wheel.
    DefaultEditMenu -      Determines whether the Windows popup menu should be
                           displayed for the inplace editor instead of grid
                           popup menu.
    DefaultHeaderMenu -    Show Explorer-like popup menu for the header to
                           quick show and hide columns. If there is a user
                           popup menu assigned to the header, the header menu
                           items will be added to it.
    Edit -                 Designates the edit control the grid uses to allow
                           users to edit the contents of the selected cell.
    EditCell -             Indicates the edit cell in the grid.
    EditColumn -           Indicates the column of the edit cell in the grid.
    EditDropDown -         Indicates whether the inplace editor drop-down list
                           is being visible.
    Editing -              Indicates whether any cell in the grid is being
                           edited.
    EndEllipsis -          Indicates whether cell text should be replaced at
                           the end with an ellipsis (...) to fit cell borders.
                           Works only for single-line text with left alignment.
    Fixed -                Represents the fixed columns for the grid.
    FlatBorder -           Determines whether a single line border is drawn
                           around the grid.
    FocusOnScroll -        Determines whether the grid should capture the input
                           focus when scrolling.
    GridColor -            Specifies the color of the lines that separate the
                           cells of the grid.
    GridHint -             Specifies the hint for an empty grid (grid without
                           rows, like "This folder is empty" for empty folder
                           in Explorer).
    GridHintColor -        Specifies the color of the hint for an empty grid.
    GridLines -            Determines whether lines are drawn separating cells
                           in the grid.
    GridStyle -            Determines the style of the grid lines:
                             gsHorzLine -     Show horizontal lines.
                             gsVertLine -     Show vertical lines.
                             gsFullHorzLine - Draw horizontal lines to the right
                                              side of the grid, but not to the
                                              last column.
                             gsFullVertLine - Draw vertical lines to the bottom
                                              of the grid, but not to the last
                                              row (only if Rows.Count > 0).
                             gsListViewLike - TListView like grid lines.
                             gsDotLines -     Draw dotted lines.
    Header -               Represents the header for the grid.
    HideSelection -        Determines whether a selected cell appears selected
                           when the focus shifts to another control.
    HighlightEvenRows -    Enable highlight every alternate row.
    HighlightFocusCol -    Indicates whether a focused column should be
                           highlighted when it is drawn.
    HighlightFocusRow -    Indicates whether a focused row should be highlighted
                           when it is drawn.
    HorzScrollBar -        Represents the horizontal scroll bar for the grid.
    Images -               Determines which image list is associated with the grid.
    ImageIndexDef -        Determines the default image that appears on the first
                           column of the grid.
    ImageHighlight -       Indicates whether the selected cell image should be
                           highlighted too.
    LeftCol -              Specifies the index of the first visible (may be
                           partial visible) scrollable column in the grid.
                           Same as VisOrigin.Col.
    RightClickSelect -     Determines whether the CellFocused property returns
                           cells that are selected using the right mouse button.
    Row -                  Specifies the index of the row that contains the
                           selected cell. Same as CellFocused.Row.
    Rows -                 Represents the rows parameters for the grid.
    RowSelect -            Specifies whether the entire row of the selected cell
                           is highlighted.
    ShowCellTips -         Determines whether the grid displays a tooltip for a
                           cell with long text when the mouse pointer rests
                           momentarily on the cell. When ShowCellTips is True
                           the standard control hint is now working.
    ShowFocusRect -        Indicates whether to draw a focus rectangle around
                           the current cell when it has input focus.
    ShowGridHint -         Indicates whether to display hint for an empty grid.
    ShowHeader -           Indicates whether to display header of the grid.
    TextLeftIndent -       Specifies the indent of cells text from the left edge.
    TextRightIndent -      Specifies the indent of cells text from the right edge.
    TextTopIndent -        Specifies the indent of cells text from the top edge.
    TipsCell -             Indicates the cell for which tooltip is displayed.
    TipsText -             Indicates the cell's tooltip text that is displayed.
    TopRow -               Specifies the index of the first visible scrollable
                           row in the grid. Same as VisOrigin.Row.
    VertScrollBar -        Represents the vertical scroll bar for the grid.
    VisibleColCount -      Indicates the number of scrollable columns visible
                           in the grid. Same as VisSize.Col.
    VisibleRowCount -      Indicates the number of scrollable rows visible
                           in the grid. Same as VisSize.Row.
    VisOrigin -            Specifies the index of the first visible cell in
                           the grid.
    VisSize -              Indicates the number of scrollable cells visible
                           in the grid.

    Public Events:

    OnCellAcceptCursor -   Occurs when the grid checks for permission to move
                           the cursor to the selected cell.
    OnCellClick -          Occurs when the user presses the left mouse button
                           in one of the cells of the grid.
    OnCellTips -           Occurs before displaying the cell hint.
    OnChange -             Occurs after a cell in the grid is selected.
    OnChangeColumns -      Occurs after change columns.
    OnChangeEditing -      Occurs when the inplace editor is shown or hidden.
    OnChangeEditMode -     Occurs when the value of AllowEdit property changes.
    OnChangeFixed -        Occurs after changing fixed cells.
    OnChangeRows -         Occurs after changing rows.
    OnChanging -           Occurs before a cell in the grid is selected.
    OnCheckClick -         Occurs when the user clicks cell check box.
    OnColumnSizeToFit -    Occurs before the with of a column changes to fit
                           its contents.
    OnColumnResize -       Occurs after the with of a column changes.
    OnColumnResizing -     Occurs before changing the column with.
    OnDraw -               Occurs when a grid needs to be drawn.
    OnDrawCell -           Occurs when a cell needs to be drawn. If you just
                           need to change the colors of the cell but not the
                           drawing, use OnGetCellColors event.
    OnDrawHeader -         Occurs when a section of the header needs to be
                           drawn. If you just need to change the colors of
                           the section but not the drawing, use OnGetCellColors
                           event.
    OnEditAcceptKey -      Called to check whether the Key can be entered into
                           the current cell.
    OnEditButtonPress -    Occurs when the user presses the ellipsis button
                           in a inplace editor.
    OnEditCanceled -       Occurs when the inplace editor is hidden by
                           pressing ESCAPE key.
    OnEditCanModify -      Called to check whether the user can change
                           the text in the inplace editor.
    OnEditChange -         Occurs when the text in the inplace editor has
                           changed.
    OnEditCloseUp -        Occurs when the drop-down list closes up due to
                           select list item.
    OnEditCloseUpEx -      Extended version of OnEditCloseUp event.
    OnEditSelectNext -     Occurs when user hit Ctrl+Enter or double click
                           on inplace editor with closed popup list.
    OnGetCellColors -      Occurs when the grid requests the colors and font
                           of a cell.
    OnGetCellImage -       Occurs when the grid requests the image index
                           of a cell.
    OnGetCellImageEx -     Extended version OnGetCellImage event for images
                           with overlay.
    OnGetCellImageIndent - Occurs during drawing the cell when the grid
                           requests the indent of cell image.
    OnGetCellReadOnly -    Called to check whether the user can change
                           the value of the cell.
    OnGetCellText -        Occurs when the grid requests a value for a cell.
    OnGetCellTextIndent -  Occurs during drawing the cell when the grid
                           requests indent of the cell text.
    OnGetCheckAlignment -  Occurs during drawing the cell when the grid
                           requires alignment of the cell check box.
    OnGetCheckImage -      Occurs during drawing the cell when the grid
                           requires the user defined check box image. Check box
                           image must be a transparent bitmap with 16x16 pixels
                           size.
    OnGetCheckIndent -     Occurs during drawing the cell when the grid
                           requires indent of the cell check box.
    OnGetCheckKind -       Occurs when the grid requests the kind of
                           a check box of a cell.
    OnGetCheckState -      Occurs during drawing the cell when the grid
                           requires the state of a check box of a cell.
    OnGetCheckStateEx -    Extended version OnGetCheckState event for cells
                           with disabled check boxes.
    OnGetEditList -        Occurs when the grid needs to fill drop-down list
                           before open it.
    OnGetEditListBounds -  Occurs when the grid needs to adjusts the size and
                           position of the drop-down list before open it.
    OnGetEditListIndex -   Occurs when the grid needs to select an item in the
                           f drop-down list before open it.
    OnGetEditMask -        Occurs when the inplace editor requests an edit mask.
    OnGetEditStyle -       Occurs when the grid requests the style of inplace
                           editor of the cell.
    OnGetEditText -        Occurs when the inplace editor requests the value
                           of a cell. By default, returns cell text.
    OnGetGridHint -        Occurs when an empty grid's ToolTip text is needed.
                           For example: "This folder is empty".
    OnGetGridColor -       Occurs during drawing when grid requires the
                           colors of the grid lines.
    OnGetHeaderColors -    Occurs when the grid requests the colors and font
                           of a header section.
    OnGetHeaderImage -     Occurs when the drawing the header whe grid requires
                           an image of the header section.
    OnGetSortDirection -   Occurs during drawing the header when the grid needs
                           to determine whether to draw the column sort image.
    OnGetSortImage -       Occurs during drawing the header when grid requires
                           an image of the sort direction.
    OnHeaderClick -        Occurs after the user clicks a column header.
    OnHeaderClicking -     Occurs before the user clicks the column header
                           when Header.Flat is False.
    OnHeaderDetailsClick - Occurs when the "Details..." item is selected in
                           the popup menu of the header.
    OnResize -             Occurs after the control is resized.
    OnSetEditText -        Occurs when the user edits the value of a cell by
                           pressing the ENTER key or by moving to another cell.
                           If the new value is not valid, an exception should
                           be raised to abort.
  }

  TGridStyle = (gsHorzLine, gsVertLine, gsFullHorzLine, gsFullVertLine,
    gsListViewLike, gsDotLines);
  TGridStyles = set of TGridStyle;

  TGridCursorKey = (gkArrows, gkTabs, gkReturn, gkMouse, gkMouseMove, gkMouseWheel);
  TGridCursorKeys = set of TGridCursorKey;

  TGridCursorOffset = (goLeft, goRight, goUp, goDown, goPageUp, goPageDown,
    goHome, goEnd, goGridHome, goGridEnd, goSelect, goFirst, goNext, goPrev,
    goGridTop, goGridBottom);

  TGridSortDirection = (gsNone, gsAscending, gsDescending);

  TGridPaintState = (psPressed, psHot, psSorted, psFlat, psDontCare);
  TGridPaintStates = set of TGridPaintState;

  TGridTextEvent = procedure(Sender: TObject; Cell: TGridCell; var Value: string) of object;
  TGridRectEvent = procedure(Sender: TObject; Cell: TGridCell; var Rect: TRect) of object;
  TGridCellColorsEvent = procedure(Sender: TObject; Cell: TGridCell; Canvas: TCanvas) of object;
  TGridCellImageEvent = procedure(Sender: TObject; Cell: TGridCell; var ImageIndex: Integer) of object;
  TGridCellImageExEvent = procedure(Sender: TObject; Cell: TGridCell; var ImageIndex, OverlayIndex: Integer) of object;
  TGridCellClickEvent = procedure(Sender: TObject; Cell: TGridCell; Shift: TShiftState; X, Y: Integer) of object;
  TGridCellAcceptCursorEvent = procedure(Sender: TObject; Cell: TGridCell; var Accept: Boolean) of object;
  TGridCellNotifyEvent = procedure(Sender: TObject; Cell: TGridCell) of object;
  TGridCellIndentEvent = procedure(Sender: TObject; Cell: TGridCell; var Indent: TPoint) of object;
  TGridCellTipsEvent = procedure(Sender: TObject; Cell: TGridCell; var AllowTips: Boolean) of object;
  TGridCellreadOnlyEvent = procedure(Sender: TObject; Cell: TGridCell; var CellReadOnly: Boolean) of object;
  TGridHeaderColorsEvent = procedure(Sender: TObject; Section: TGridHeaderSection; Canvas: TCanvas) of object;
  TGridHeaderImageEvent = procedure(Sender: TObject; Section: TGridHeaderSection; var ImageIndex: Integer) of object;
  TGridDrawEvent = procedure(Sender: TObject; var DefaultDrawing: Boolean) of object;
  TGridDrawCellEvent = procedure(Sender: TObject; Cell: TGridCell; var Rect: TRect; var DefaultDrawing: Boolean) of object;
  TGridDrawHeaderEvent = procedure(Sender: TObject; Section: TGridHeaderSection; Rect: TRect; var DefaultDrawing: Boolean) of object;
  TGridColumnResizeEvent = procedure(Sender: TObject; Column: Integer; var Width: Integer) of object;
  TGridHeaderClickEvent = procedure(Sender: TObject; Section: TGridHeaderSection) of object;
  TGridHeaderClickingEvent = procedure(Sender: TObject; Section: TGridHeaderSection; var AllowClick: Boolean) of object;
  TGridChangingEvent = procedure(Sender: TObject; var Cell: TGridCell; var Selected: Boolean) of object;
  TGridChangedEvent = procedure(Sender: TObject; Cell: TGridCell; Selected: Boolean) of object;
  TGridEditStyleEvent = procedure(Sender: TObject; Cell: TGridCell; var Style: TGridEditStyle) of object;
  TGridEditListEvent = procedure(Sender: TObject; Cell: TGridCell; Items: TStrings) of object;
  TGridEditListIndexEvent = procedure(Sender: TObject; Cell: TGridCell; Items: TStrings; const ItemText: string; var ItemIndex: Integer) of object;
  TGridEditCloseUpEvent = procedure(Sender: TObject; Cell: TGridCell; ItemIndex: Integer; var Accept: Boolean) of object;
  TGridEditCloseUpExEvent = procedure(Sender: TObject; Cell: TGridCell; Items: TStrings; ItemIndex: Integer; var ItemText: string; var Accept: Boolean) of object;
  TGridEditCanModifyEvent = procedure(Sender: TObject; Cell: TGridCell; var CanModify: Boolean) of object;
  TGridEditCanShowEvent = procedure(Sender: TObject; Cell: TGridCell; var CanShow: Boolean) of object;
  TGridAcceptKeyEvent = procedure(Sender: TObject; Cell: TGridCell; Key: Char; var Accept: Boolean) of object;
  TGridCheckKindEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckKind: TGridCheckKind) of object;
  TGridCheckStateEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckState: TCheckBoxState) of object;
  TGridCheckStateExEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckState: TCheckBoxState; var CheckEnabled: Boolean) of object;
  TGridCheckImageEvent = procedure(Sender: TObject; Cell: TGridCell; CheckImage: TBitmap) of object;
  TGridCheckAlignmentEvent = procedure(Sender: TObject; Cell: TGridCell; var CheckAlignment: TAlignment) of object;
  TGridSortDirectionEvent = procedure(Sender: TObject; Section: TGridHeaderSection; var SortDirection: TGridSortDirection) of object;
  TGridSortImageEvent = procedure(Sender: TObject; Section: TGridHeaderSection; SortImage: TBitmap) of object;
  TGridHintEvent = procedure(Sender: TObject; var Value: string) of object;
  TGridColorEvent = procedure(Sender: TObject; var Color: TColor) of object;
  TGridFindTextEvent = procedure(Sender: TObject; const FindText: string) of object;

  TCustomGridView = class(TCustomControl)
  private
    FHorzScrollBar: TGridScrollBar;
    FVertScrollBar: TGridScrollBar;
    FHeader: TCustomGridHeader;
    FColumns: TGridColumns;
    FRows: TCustomGridRows;
    FFixed: TCustomGridFixed;
    FImages: TImageList;
    FImagesLink: TChangeLink;
    FImageLeftIndent: Integer;
    FImageTopIndent: Integer;
    FImageHighlight: Boolean;
    FImageIndexDef: Integer;
    FCellFocused: TGridCell;
    FCellSelected: Boolean;
    FVisOrigin: TGridCell;
    FVisSize: TGridCell;
    FBorderStyle: TBorderStyle;
    FFlatBorder: Boolean;
    FHideSelection: Boolean;
    FShowHeader: Boolean;
    FGridLines: Boolean;
    FGridLineWidth: Integer;
    FGridStyle: TGridStyles;
    FGridColor: TColor;
    FEndEllipsis: Boolean;
    FShowFocusRect: Boolean;
    FAlwaysSelected: Boolean;
    FRowSelect: Boolean;
    FRightClickSelect: Boolean;
    FAllowSelect: Boolean;
    FFocusOnScroll: Boolean;
    FCursorKeys: TGridCursorKeys;
    FTextLeftIndent: Integer;
    FTextRightIndent: Integer;
    FTextTopIndent: Integer;
    FHitTest: TPoint;
    FClickPos: TGridCell;
    FColumnsResize: Boolean;
    FColumnsFullDrag: Boolean;
    FColumnClick: Boolean;
    FColResizing: Boolean;
    FColResizeSection: TGridHeaderSection;
    FColResizeLevel: Integer;
    FColResizeIndex: Integer;
    FColResizeOffset: Integer;
    FColResizeRect: TRect;
    FColResizePos: Integer;
    FColResizeMinWidth: Integer;
    FColResizeMaxWidth: Integer;
    FColResizeCount: Integer;
    FHeaderClickSection: TGridHeaderSection;
    FHeaderClickRect: TRect;
    FHeaderClickState: Boolean;
    FHeaderClicking: Boolean;
    FHotSection: TGridHeaderSection;
    FHotColumn: Integer;
    FHotLevel: Integer;
    FUpdateLock: Integer;
    FAllowEdit: Boolean;
    FAlwaysEdit: Boolean;
    FReadOnly: Boolean;
    FEdit: TCustomGridEdit;
    FEditCell: TGridCell;
    FEditing: Boolean;
    FShowCellTips: Boolean;
    FTipsCell: TGridCell;
    FTipsText: string;
    FCheckBoxes: Boolean;
    FCheckStyle: TGridCheckStyle;
    FCheckWidth: Integer;
    FCheckHeight: Integer;
    FCheckLeftIndent: Integer;
    FCheckTopIndent: Integer;
    FCheckBuffer: TBitmap;
    FSortLeftIndent: Integer;
    FSortTopIndent: Integer;
    FSortBuffer: TBitmap;
    FPatternBitmap: TBitmap;
    FCancelOnExit: Boolean;
    FDefaultEditMenu: Boolean;
    FHighlightEvenRows: Boolean;
    FHighlightFocusCol: Boolean;
    FHighlightFocusRow: Boolean;
    FOnGetCellText: TGridTextEvent;
    FOnGetCellTextIndent: TGridCellIndentEvent;
    FOnGetCellColors: TGridCellColorsEvent;
    FOnGetCellImage: TGridCellImageEvent;
    FOnGetCellImageEx: TGridCellImageExEvent;
    FOnGetCellImageIndent: TGridCellIndentEvent;
    FOnGetCellReadOnly: TGridCellreadOnlyEvent;
    FOnGetHeaderColors: TGridHeaderColorsEvent;
    FOnGetHeaderImage: TGridHeaderImageEvent;
    FOnDraw: TGridDrawEvent;
    FOnDrawCell: TGridDrawCellEvent;
    FOnDrawHeader: TGridDrawHeaderEvent;
    FOnColumnSizeToFit: TGridColumnResizeEvent;
    FOnColumnResizing: TGridColumnResizeEvent;
    FOnColumnResize: TGridColumnResizeEvent;
    FOnHeaderClick: TGridHeaderClickEvent;
    FOnHeaderClicking: TGridHeaderClickingEvent;
    FOnChangeColumns: TNotifyEvent;
    FOnChangeRows: TNotifyEvent;
    FOnChangeFixed: TNotifyEvent;
    FOnCellAcceptCursor: TGridCellAcceptCursorEvent;
    FOnChanging: TGridChangingEvent;
    FOnChange: TGridChangedEvent;
    FOnCellClick: TGridCellClickEvent;
    FOnGetEditStyle: TGridEditStyleEvent;
    FOnGetEditMask: TGridTextEvent;
    FOnGetEditText: TGridTextEvent;
    FOnSetEditText: TGridTextEvent;
    FOnGetEditList: TGridEditListEvent;
    FOnGetEditListIndex: TGridEditListIndexEvent;
    FOnGetEditListBounds: TGridRectEvent;
    FOnEditCanModify: TGridEditCanModifyEvent;
    FOnEditCanShow: TGridEditCanShowEvent;
    FOnEditAcceptKey: TGridAcceptKeyEvent;
    FOnEditButtonPress: TGridCellNotifyEvent;
    FOnEditSelectNext: TGridTextEvent;
    FOnEditCloseUp: TGridEditCloseUpEvent;
    FOnEditCloseUpEx: TGridEditCloseUpExEvent;
    FOnEditChange: TGridCellNotifyEvent;
    FOnEditCanceled: TGridCellNotifyEvent;
    FOnGetCheckKind: TGridCheckKindEvent;
    FOnGetCheckState: TGridCheckStateEvent;
    FOnGetCheckStateEx: TGridCheckStateExEvent;
    FOnGetCheckImage: TGridCheckImageEvent;
    FOnGetCheckAlignment: TGridCheckAlignmentEvent;
    FOnGetCheckIndent: TGridCellIndentEvent;
    FOnCheckClick: TGridCellNotifyEvent;
    FOnGetSortDirection: TGridSortDirectionEvent;
    FOnGetSortImage: TGridSortImageEvent;
    FOnChangeEditing: TNotifyEvent;
    FOnChangeEditMode: TNotifyEvent;
    FOnGetCellHintRect: TGridRectEvent;
    FOnCellTips: TGridCellTipsEvent;
    FOnGetTipsRect: TGridRectEvent;
    FOnGetTipsText: TGridTextEvent;
    FGridHint: string;
    FGridHintColor: TColor;
    FShowGridHint: Boolean;
    FOnGetGridHint: TGridHintEvent;
    FGrayReadOnly: Boolean;
    FDefaultHeaderMenu: Boolean;
    FHeaderPopupMenu: TPopupMenu;
    FOnHeaderDetailsClick: TNotifyEvent;
    FOnGetGridColor: TGridColorEvent;
    FFindDialog: TGridFindDialog;
    FOnTextNotFound: TGridFindTextEvent;
    FContextPopupCol: Integer;
    FContextPopupRow: Integer;
    function GetCell(Col, Row: Longint): string;
    function GetChecked(Col, Row: Longint): Boolean;
    function GetCheckBoxEnabled(Col, Row: Longint): Boolean;
    function GetCheckBoxState(Col, Row: Longint): TCheckBoxState;
    function GetCol: Longint;
    function GetFindDialog: TGridFindDialog;
    function GetFixed: TGridFixed;
    function GetEdit: TGridEdit;
    function GetEditColumn: TGridColumn;
    function GetEditDropDown: Boolean;
    function GetEditing: Boolean;
    function GetEditFocused: Boolean;
    function GetHeader: TGridHeader;
    function GetLeftCol: Longint;
    function GetRow: Longint;
    function GetRows: TGridRows;
    function GetTopRow: Longint;
    function GetVisibleColCount: Longint;
    function GetVisibleRowCount: Longint;
    procedure HeaderMenuClick(Sender: TObject);
    procedure HorzScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
    procedure HorzScrollChange(Sender: TObject);
    procedure SetAllowEdit(Value: Boolean);
    procedure SetAllowSelect(Value: Boolean);
    procedure SetAlwaysEdit(Value: Boolean);
    procedure SetAlwaysSelected(Value: Boolean);
    procedure SetBorderStyle(Value: TBorderStyle);
    procedure SetCell(Col, Row: Longint; Value: string);
    procedure SetCellFocused(Value: TGridCell);
    procedure SetCellSelected(Value: Boolean);
    procedure SetCheckBoxes(Value: Boolean);
    procedure SetCheckLeftIndent(Value: Integer);
    procedure SetCheckStyle(Value: TGridCheckStyle);
    procedure SetCheckTopIndent(Value: Integer);
    procedure SetCol(Value: Longint);
    procedure SetColumns(Value: TGridColumns);
    procedure SetCursorKeys(Value: TGridCursorKeys);
    procedure SetEditDropDown(Value: Boolean);
    procedure SetEditing(Value: Boolean);
    procedure SetEndEllipsis(Value: Boolean);
    procedure SetFlatBorder(Value: Boolean);
    procedure SetFixed(Value: TGridFixed);
    procedure SetGrayReadOnly(const Value: Boolean);
    procedure SetGridColor(Value: TColor);
    procedure SetGridHint(const Value: string);
    procedure SetGridHintColor(Value: TColor);
    procedure SetGridLines(Value: Boolean);
    procedure SetGridStyle(Value: TGridStyles);
    procedure SetHeader(Value: TGridHeader);
    procedure SetHideSelection(Value: Boolean);
    procedure SetHighlightEvenRows(const Value: Boolean);
    procedure SetHighlightFocusCol(const Value: Boolean);
    procedure SetHighlightFocusRow(const Value: Boolean);
    procedure SetHorzScrollBar(Value: TGridScrollBar);
    procedure SetImageIndexDef(Value: Integer);
    procedure SetImageHighlight(Value: Boolean);
    procedure SetImageLeftIndent(Value: Integer);
    procedure SetImages(Value: TImageList);
    procedure SetImageTopIndent(Value: Integer);
    procedure SetLeftCol(Value: Longint);
    procedure SetReadOnly(Value: Boolean);
    procedure SetRow(Value: Longint);
    procedure SetRows(Value: TGridRows);
    procedure SetRowSelect(Value: Boolean);
    procedure SetShowCellTips(Value: Boolean);
    procedure SetShowFocusRect(Value: Boolean);
    procedure SetShowGridHint(Value: Boolean);
    procedure SetShowHeader(Value: Boolean);
    procedure SetSortLeftIndent(Value: Integer);
    procedure SetSortTopIndent(Value: Integer);
    procedure SetTextLeftIndent(Value: Integer);
    procedure SetTextRightIndent(Value: Integer);
    procedure SetTextTopIndent(Value: Integer);
    procedure SetTopRow(Value: Longint);
    procedure SetVertScrollBar(Value: TGridScrollBar);
    procedure SetVisOrigin(Value: TGridCell);
    procedure VertScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
    procedure VertScrollChange(Sender: TObject);
    procedure WMGetDlgCode(var Message: TWMGetDlgCode); message WM_GETDLGCODE;
    procedure WMKillFocus(var Message: TWMKillFocus); message WM_KILLFOCUS;
    procedure WMSetFocus(var Message: TWMSetFocus); message WM_SETFOCUS;
    procedure WMLButtonDown(var Message: TMessage); message WM_LBUTTONDOWN;
    procedure WMChar(var Msg: TWMChar); message WM_CHAR;
    procedure WMHScroll(var Message: TWMHScroll); message WM_HSCROLL;
    procedure WMVScroll(var Message: TWMVScroll); message WM_VSCROLL;
    procedure WMNCHitTest(var Message: TWMNCHitTest); message WM_NCHITTEST;
    procedure WMSetCursor(var Message: TWMSetCursor); message WM_SETCURSOR;
    procedure WMEraseBkgnd(var Message: TWMEraseBkgnd); message WM_ERASEBKGND;
    procedure WMThemeThanged(var Message: TMessage); message WM_THEMECHANGED;
    procedure WMTimer(var Message: TWMTimer); message WM_TIMER;
    procedure CNThemeThanged(var Message: TMessage); message CN_THEMECHANGED;
    procedure CMCancelMode(var Message: TCMCancelMode); message CM_CANCELMODE;
    procedure CMEnabledChanged(var Message: TMessage); message CM_ENABLEDCHANGED;
    procedure CMCtl3DChanged(var Message: TMessage); message CM_CTL3DCHANGED;
    procedure CMFontChanged(var Message: TMessage); message CM_FONTCHANGED;
    procedure CMColorChanged(var Message: TMessage); message CM_COLORCHANGED;
    procedure CMShowHintChanged(var Message: TMessage); message CM_SHOWHINTCHANGED;
    procedure CMHintShow(var Message: TMessage); message CM_HINTSHOW;
    procedure CMMouseLeave(var Message: TMessage); message CM_MOUSELEAVE;
    procedure CMWinIniChange(var Message: TWMWinIniChange); message CM_WININICHANGE;
  protected
    FEditPending: Boolean;
    procedure CancelCellTips; virtual;
    procedure CellClick(Cell: TGridCell; Shift: TShiftState; X, Y: Integer); virtual;
    procedure CellTips(Cell: TGridCell; var AllowTips: Boolean); virtual;
    procedure Change(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure ChangeColumns; virtual;
    procedure ChangeEditing; virtual;
    procedure ChangeEditMode; virtual;
    procedure ChangeFixed; virtual;
    procedure ChangeRows; virtual;
    procedure ChangeScale(M, D: Integer); override;
    procedure Changing(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure CheckClick(Cell: TGridCell); virtual;
    procedure ColumnResize(Column: Integer; var Width: Integer); virtual;
    procedure ColumnResizing(Column: Integer; var Width: Integer); virtual;
    procedure ColumnsChange(Sender: TObject); virtual;
    procedure ColumnSizeToFit(Column: Integer; var Width: Integer); virtual;
    function CompareStrings(const S1, S2: string; WholeWord, MatchCase: Boolean): Boolean; virtual;
    function CreateColumn(Columns: TGridColumns): TCustomGridColumn; virtual;
    function CreateColumns: TGridColumns; virtual;
    function CreateEdit(EditClass: TGridEditClass): TCustomGridEdit; virtual;
    function CreateFixed: TCustomGridFixed; virtual;
    function CreateHeader: TCustomGridHeader; virtual;
    function CreateHeaderSection(Sections: TGridHeaderSections): TGridHeaderSection; virtual;
    procedure CreateParams(var Params: TCreateParams); override;
    function CreateRows: TCustomGridRows; virtual;
    function CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar; virtual;
    procedure CreateWnd; override;
    procedure DoContextPopup(MousePos: TPoint; var Handled: Boolean); override;
    procedure DoExit; override;
    procedure DoHeaderPopup(MousePos: TPoint; var Handled: Boolean); virtual;
    function DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    function DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean; override;
    procedure DoStartDrag(var DragObject: TDragObject); override;
    procedure DoTextNotFound(const FindText: string); virtual;
    procedure EditButtonPress(Cell: TGridCell); virtual;
    function EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean; virtual;
    procedure EditCanceled(Cell: TGridCell); virtual;
    function EditCanModify(Cell: TGridCell): Boolean; virtual;
    function EditCanShow(Cell: TGridCell): Boolean; virtual;
    function EditCanUndo(Cell: TGridCell): Boolean; virtual;
    procedure EditChange(Cell: TGridCell); virtual;
    procedure EditCloseUp(Cell: TGridCell; Items: TStrings; ItemIndex: Integer;
      var ItemText: string; var Accept: Boolean); virtual;
    procedure EditSelectNext(Cell: TGridCell; var Value: string); virtual;
    procedure FixedChange(Sender: TObject); virtual;
    procedure GetCellColors(Cell: TGridCell; Canvas: TCanvas); virtual;
    function GetCellImage(Cell: TGridCell; var OverlayIndex: Integer): Integer; virtual;
    function GetCellImageIndent(Cell: TGridCell): TPoint; virtual;
    function GetCellImageRect(Cell: TGridCell): TRect; virtual;
    function GetCellHintRect(Cell: TGridCell): TRect; virtual;
    function GetCellText(Cell: TGridCell): string; virtual;
    function GetCellTextBounds(Cell: TGridCell): TRect; virtual;
    function GetCellTextIndent(Cell: TGridCell): TPoint; virtual;
    function GetCheckAlignment(Cell: TGridCell): TAlignment; virtual;
    procedure GetCheckImage(Cell: TGridCell; CheckImage: TBitmap); virtual;
    function GetCheckIndent(Cell: TGridCell): TPoint; virtual;
    function GetCheckKind(Cell: TGridCell): TGridCheckKind; virtual;
    function GetCheckRect(Cell: TGridCell): TRect; virtual;
    function GetCheckState(Cell: TGridCell): TCheckBoxState; virtual;
    function GetCheckStateEx(Cell: TGridCell; var CheckEnabled: Boolean): TCheckBoxState; virtual;
    function GetClientOrigin: TPoint; override;
    function GetClientRect: TRect; override;
    function GetColumnClass: TGridColumnClass; virtual;
    function GetCursorCell(Cell: TGridCell; Offset: TGridCursorOffset): TGridCell; virtual;
    function GetEditClass(Cell: TGridCell): TGridEditClass; virtual;
    procedure GetEditList(Cell: TGridCell; Items: TStrings); virtual;
    procedure GetEditListBounds(Cell: TGridCell; var Rect: TRect); virtual;
    function GetEditListIndex(Cell: TGridCell; Items: TStrings; const ItemText: string): Integer; virtual;
    function GetEditMask(Cell: TGridCell): string; virtual;
    function GetEditStyle(Cell: TGridCell): TGridEditStyle; virtual;
    function GetEditText(Cell: TGridCell): string; virtual;
    function GetFixedDividerColor: TColor; virtual;
    function GetFixedGridColor: TColor; virtual;
    function GetFontHeight(Font: TFont): Integer;
    function GetFontWidth(Font: TFont; TextLength: Integer): Integer;
    function GetGridHint: string; virtual;
    function GetHeaderImage(Section: TGridHeaderSection): Integer; virtual;
    procedure GetHeaderColors(Section: TGridHeaderSection; Canvas: TCanvas); virtual;
    function GetSortArrowSize: TSize; virtual;
    function GetSortDirection(Section: TGridHeaderSection): TGridSortDirection; virtual;
    procedure GetSortImage(Section: TGridHeaderSection; SortImage: TBitmap); virtual;
    function GetTextRect(Canvas: TCanvas; Rect: TRect; LeftIndent,
      TopIndent: Integer; Alignment: TAlignment; WantReturns, WordWrap: Boolean;
      const Text: string): TRect; virtual;
    function GetTipsRect(Cell: TGridCell; const TipsText: string): TRect; virtual;
    function GetTipsText(Cell: TGridCell): string; virtual;
    function GetTipsWindowClass: TGridTipsWindowClass; virtual;
    procedure HeaderChange(Sender: TObject); virtual;
    procedure HeaderClick(Section: TGridHeaderSection); virtual;
    procedure HeaderClicking(Section: TGridHeaderSection; var AllowClick: Boolean); virtual;
    procedure HideCursor; virtual;
    procedure HideEdit; virtual;
    procedure HideFocus; virtual;
    procedure ImagesChange(Sender: TObject); virtual;
    procedure KeyDown(var Key: Word; Shift: TShiftState); override;
    procedure KeyPress(var Key: Char); override;
    procedure Loaded; override;
    procedure MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure MouseMove(Shift: TShiftState; X, Y: Integer); override;
    procedure MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer); override;
    procedure Notification(AComponent: TComponent; Operation: TOperation); override;
    procedure Paint; override;
    procedure Paint3DFrame(Rect: TRect; SideFlags: Longint); virtual;
    procedure PaintCell(Cell: TGridCell; Rect: TRect); virtual;
    procedure PaintCells; virtual;
    procedure PaintCheck(Rect: TRect; CheckKind: TGridCheckKind; CheckState: TCheckBoxState; CheckEnabled: Boolean); virtual;
    procedure PaintDotGridLines(Points: Pointer; Count: Integer);
    procedure PaintFixed; virtual;
    procedure PaintFixedGridLines; virtual;
    procedure PaintFreeField; virtual;
    procedure PaintFocus; virtual;
    procedure PaintGridLines; virtual;
    procedure PaintHeader(Section: TGridHeaderSection; Rect: TRect); virtual;
    procedure PaintHeaderBackground(Rect: TRect; Color: TColor; PaintState: TGridPaintStates); virtual;
    procedure PaintHeaders(DrawFixed: Boolean); virtual;
    procedure PaintHeaderSections(Sections: TGridHeaderSections; DrawFixed: Boolean); virtual;
    procedure PaintResizeLine;
    procedure PaintText(Canvas: TCanvas; Rect: TRect; LeftIndent, TopIndent: Integer; Alignment: TAlignment; WantReturns, WordWrap: Boolean; const Text: string); virtual;
    procedure PreparePatternBitmap(Canvas: TCanvas; FillColor: TColor; Remove: Boolean); virtual;
    procedure ResetClickPos; virtual;
    procedure Resize; override;
    procedure RowsChange(Sender: TObject); virtual;
    procedure SetEditText(Cell: TGridCell; var Value: string); virtual;
    procedure ShowCursor; virtual;
    procedure ShowEdit; virtual;
    procedure ShowEditChar(C: Char); virtual;
    procedure ShowFocus; virtual;
    procedure StartColResize(Section: TGridHeaderSection; X, Y: Integer);
    procedure StartHeaderClick(Section: TGridHeaderSection; X, Y: Integer);
    procedure StepColResize(X, Y: Integer);
    procedure StepHeaderClick(X, Y: Integer);
    procedure StopColResize(Abort: Boolean);
    procedure StopHeaderClick(Abort: Boolean);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function AcquireFocus: Boolean; virtual;
    procedure ApplyEdit; virtual;
    procedure ApplyEditText; virtual;
    procedure CancelEdit; virtual;
    procedure DefaultDrawCell(Cell: TGridCell; Rect: TRect); virtual;
    procedure DefaultDrawHeader(Section: TGridHeaderSection; Rect: TRect); virtual;
    procedure DrawDragRect(Cell: TGridCell); virtual;
    function FindText(const FindText: string; Options: TFindOptions): Boolean; virtual;
    function GetCellAt(X, Y: Integer): TGridCell; virtual;
    function GetCellRect(Cell: TGridCell): TRect;
    function GetCellsRect(Cell1, Cell2: TGridCell): TRect;
    function GetColumnAt(X, Y: Integer): Integer; virtual;
    function GetColumnLeftRight(Column: Integer): TRect;
    function GetColumnMaxWidth(Column: Integer): Integer;
    function GetColumnRect(Column: Integer): TRect;
    function GetColumnsRect(Column1, Column2: Integer): TRect;
    function GetColumnsWidth(Column1, Column2: Integer): Integer;
    function GetEditRect(Cell: TGridCell): TRect; virtual;
    function GetFirstImageColumn: Integer;
    function GetFixedRect: TRect; virtual;
    function GetFixedWidth: Integer;
    function GetFocusRect: TRect; virtual;
    function GetGridHeight: Integer;
    function GetGridLineColor(BkColor: TColor): TColor; virtual;
    function GetGridOrigin: TPoint;
    function GetGridRect: TRect; virtual;
    function GetHeaderHeight: Integer;
    function GetHeaderRect: TRect; virtual;
    function GetHeaderSection(ColumnIndex, Level: Integer): TGridHeaderSection;
    function GetLightenColor(Color: TColor; Amount: Integer): TColor;
    function GetResizeSectionAt(X, Y: Integer): TGridHeaderSection;
    function GetRowAt(X, Y: Integer): Integer; virtual;
    function GetRowRect(Row: Integer): TRect;
    function GetRowsRect(Row1, Row2: Integer): TRect;
    function GetRowsHeight(Row1, Row2: Integer): Integer;
    function GetRowTopBottom(Row: Integer): TRect;
    function GetSectionAt(X, Y: Integer): TGridHeaderSection;
    procedure HandlerFind(Sender: TObject);
    procedure HandlerFindNext(Sender: TObject);
    procedure HandlerFindPrev(Sender: TObject);
    procedure HandlerFindMenu(Sender: TObject);
    procedure Invalidate; override;
    procedure InvalidateCell(Cell: TGridCell);
    procedure InvalidateCheck(Cell: TGridCell);
    procedure InvalidateColumn(Column: Integer);
    procedure InvalidateColumns(Column1, Column2: Integer);
    procedure InvalidateEdit;
    procedure InvalidateFixed;
    procedure InvalidateFocus; virtual;
    procedure InvalidateGrid; virtual;
    procedure InvalidateHeader;
    procedure InvalidateRect(Rect: TRect);
    procedure InvalidateRow(Row: Integer); virtual;
    procedure InvalidateRows(Row1, Row2: Integer);
    procedure InvalidateSection(Section: TGridHeaderSection); overload;
    procedure InvalidateSection(ColumnIndex, Level: Integer); overload;
    function IsActiveControl: Boolean;
    function IsCellAcceptCursor(Cell: TGridCell): Boolean; virtual;
    function IsCellEditing(Cell: TGridCell): Boolean;
    function IsCellHighlighted(Cell: TGridCell): Boolean; virtual;
    function IsCellHasCheck(Cell: TGridCell): Boolean; virtual;
    function IsCellHasImage(Cell: TGridCell): Boolean; virtual;
    function IsCellFocused(Cell: TGridCell): Boolean;
    function IsCellReadOnly(Cell: TGridCell): Boolean; virtual;
    function IsCellValid(Cell: TGridCell): Boolean;
    function IsCellValidEx(Cell: TGridCell; CheckPosition, CheckVisible: Boolean): Boolean;
    function IsCellVisible(Cell: TGridCell; PartialOK: Boolean): Boolean;
    function IsColumnVisible(Column: Integer): Boolean;
    function IsEvenRow(Cell: TGridCell): Boolean; virtual;
    function IsFixedVisible: Boolean;
    function IsFocusAllowed: Boolean;
    function IsGridHintVisible: Boolean; virtual;
    function IsHeaderHasImage(Section: TGridHeaderSection): Boolean; virtual;
    function IsHeaderPressed(Section: TGridHeaderSection): Boolean; virtual;
    function IsRowHighlighted(Row: Integer): Boolean; virtual;
    function IsRowVisible(Row: Integer): Boolean;
    procedure LockUpdate;
    procedure MakeCellVisible(Cell: TGridCell; PartialOK: Boolean); virtual;
    procedure ResetEdit; virtual;
    procedure SetCursor(Cell: TGridCell; Selected, Visible: Boolean); virtual;
    procedure SizeAllColumnsToFit;
    procedure SizeColumnToFit(ColumnIndex: Integer);
    procedure UnLockUpdate(Redraw: Boolean);
    procedure UpdateCursor; virtual;
    procedure UpdateColors; virtual;
    procedure UpdateEdit(Activate: Boolean); virtual;
    procedure UpdateEditContents(SaveText: Boolean); virtual;
    procedure UpdateFixed; virtual;
    procedure UpdateFocus; virtual;
    procedure UpdateFonts; virtual;
    procedure UpdateHeader; virtual;
    procedure UpdateRows; virtual;
    procedure UpdateScrollBars; virtual;
    procedure UpdateScrollPos; virtual;
    procedure UpdateSelection(var Cell: TGridCell; var Selected: Boolean); virtual;
    procedure UpdateText; virtual;
    procedure UpdateVisOriginSize; virtual;
    property AllowEdit: Boolean read FAllowEdit write SetAllowEdit default False;
    property AllowSelect: Boolean read FAllowSelect write SetAllowSelect default True;
    property AlwaysEdit: Boolean read FAlwaysEdit write SetAlwaysEdit default False;
    property AlwaysSelected: Boolean read FAlwaysSelected write SetAlwaysSelected default False;
    property BorderStyle: TBorderStyle read FBorderStyle write SetBorderStyle default bsSingle;
    property CancelOnExit: Boolean read FCancelOnExit write FCancelOnExit default True;
    property Canvas;
    property Cells[Col, Row: Longint]: string read GetCell write SetCell;
    property CellFocused: TGridCell read FCellFocused write SetCellFocused;
    property CellSelected: Boolean read FCellSelected write SetCellSelected;
    property CheckBoxes: Boolean read FCheckBoxes write SetCheckBoxes default False;
    property Checked[Col, Row: Longint]: Boolean read GetChecked;
    property CheckEnabled[Col, Row: Longint]: Boolean read GetCheckBoxEnabled;
    property CheckState[Col, Row: Longint]: TCheckBoxState read GetCheckBoxState;
    property CheckHeight: Integer read FCheckHeight write FCheckHeight default 16;
    property CheckLeftIndent: Integer read FCheckLeftIndent write SetCheckLeftIndent default 0;
    property CheckStyle: TGridCheckStyle read FCheckStyle write SetCheckStyle default csWin95;
    property CheckTopIndent: Integer read FCheckTopIndent write SetCheckTopIndent default 0;
    property CheckWidth: Integer read FCheckWidth write FCheckWidth default 16;
    property Col: Longint read GetCol write SetCol;
    property ColResizing: Boolean read FColResizing;
    property ColumnClick: Boolean read FColumnClick write FColumnClick default True;
    property Columns: TGridColumns read FColumns write SetColumns;
    property ColumnsFullDrag: Boolean read FColumnsFullDrag write FColumnsFullDrag default False;
    property ColumnsResize: Boolean read FColumnsResize write FColumnsResize default True;
    property ContextPopupCol: Integer read FContextPopupCol;
    property ContextPopupRow: Integer read FContextPopupRow;
    property CursorKeys: TGridCursorKeys read FCursorKeys write SetCursorKeys default [gkArrows, gkMouse, gkMouseWheel];
    property DefaultEditMenu: Boolean read FDefaultEditMenu write FDefaultEditMenu default False;
    property DefaultHeaderMenu: Boolean read FDefaultHeaderMenu write FDefaultHeaderMenu default False;
    property Edit: TGridEdit read GetEdit;
    property EditCell: TGridCell read FEditCell;
    property EditColumn: TGridColumn read GetEditColumn;
    property EditDropDown: Boolean read GetEditDropDown write SetEditDropDown;
    property Editing: Boolean read GetEditing write SetEditing;
    property EditFocused: Boolean read GetEditFocused;
    property EndEllipsis: Boolean read FEndEllipsis write SetEndEllipsis default True;
    property FindDialog: TGridFindDialog read GetFindDialog;
    property GrayReadOnly: Boolean read FGrayReadOnly write SetGrayReadOnly default False;
    property GridColor: TColor read FGridColor write SetGridColor default clWindow;
    property GridHint: string read FGridHint write SetGridHint;
    property GridHintColor: TColor read FGridHintColor write SetGridHintColor default clGrayText;
    property GridLines: Boolean read FGridLines write SetGridLines default True;
    property GridLineWidth: Integer read FGridLineWidth;
    property GridStyle: TGridStyles read FGridStyle write SetGridStyle default [gsHorzLine, gsVertLine];
    property Fixed: TGridFixed read GetFixed write SetFixed;
    property FlatBorder: Boolean read FFlatBorder write SetFlatBorder default False;
    property FocusOnScroll: Boolean read FFocusOnScroll write FFocusOnScroll default False;
    property Header: TGridHeader read GetHeader write SetHeader;
    property HideSelection: Boolean read FHideSelection write SetHideSelection default False;
    property HighlightEvenRows: Boolean read FHighlightEvenRows write SetHighlightEvenRows default False;
    property HighlightFocusCol: Boolean read FHighlightFocusCol write SetHighlightFocusCol default False;
    property HighlightFocusRow: Boolean read FHighlightFocusRow write SetHighlightFocusRow default False;
    property HorzScrollBar: TGridScrollBar read FHorzScrollBar write SetHorzScrollBar;
    property ImageIndexDef: Integer read FImageIndexDef write SetImageIndexDef default 0;
    property ImageHighlight: Boolean read FImageHighlight write SetImageHighlight default True;
    property ImageLeftIndent: Integer read FImageLeftIndent write SetImageLeftIndent default 2;
    property Images: TImageList read FImages write SetImages;
    property ImageTopIndent: Integer read FImageTopIndent write SetImageTopIndent default 1;
    property LeftCol: Longint read GetLeftCol write SetLeftCol;
    property ReadOnly: Boolean read FReadOnly write SetReadOnly default False;
    property RightClickSelect: Boolean read FRightClickSelect write FRightClickSelect default True;
    property Row: Longint read GetRow write SetRow;
    property Rows: TGridRows read GetRows write SetRows;
    property RowSelect: Boolean read FRowSelect write SetRowSelect default False;
    property ShowCellTips: Boolean read FShowCellTips write SetShowCellTips;
    property ShowFocusRect: Boolean read FShowFocusRect write SetShowFocusRect default True;
    property ShowGridHint: Boolean read FShowGridHint write SetShowGridHint default False;
    property ShowHeader: Boolean read FShowHeader write SetShowHeader default True;
    property SortLeftIndent: Integer read FSortLeftIndent write SetSortLeftIndent default 4;
    property SortTopIndent: Integer read FSortTopIndent write SetSortTopIndent default 0;
    property TextLeftIndent: Integer read FTextLeftIndent write SetTextLeftIndent default 6;
    property TextRightIndent: Integer read FTextRightIndent write SetTextRightIndent default 6;
    property TextTopIndent: Integer read FTextTopIndent write SetTextTopIndent default 2;
    property TipsCell: TGridCell read FTipsCell;
    property TipsText: string read FTipsText;
    property TopRow: Longint read GetTopRow write SetTopRow;
    property UpdateLock: Integer read FUpdateLock;
    property VertScrollBar: TGridScrollBar read FVertScrollBar write SetVertScrollBar;
    property VisibleColCount: Longint read GetVisibleColCount;
    property VisibleRowCount: Longint read GetVisibleRowCount;
    property VisOrigin: TGridCell read FVisOrigin write SetVisOrigin;
    property VisSize: TGridCell read FVisSize;
    property OnCellAcceptCursor: TGridCellAcceptCursorEvent read FOnCellAcceptCursor write FOnCellAcceptCursor;
    property OnCellClick: TGridCellClickEvent read FOnCellClick write FOnCellClick;
    property OnCellTips: TGridCellTipsEvent read FOnCellTips write FOnCellTips;
    property OnChange: TGridChangedEvent read FOnChange write FOnChange;
    property OnChangeColumns: TNotifyEvent read FOnChangeColumns write FOnChangeColumns;
    property OnChangeEditing: TNotifyEvent read FOnChangeEditing write FOnChangeEditing;
    property OnChangeEditMode: TNotifyEvent read FOnChangeEditMode write FOnChangeEditMode;
    property OnChangeFixed: TNotifyEvent read FOnChangeFixed write FOnChangeFixed;
    property OnChangeRows: TNotifyEvent read FOnChangeRows write FOnChangeRows;
    property OnChanging: TGridChangingEvent read FOnChanging write FOnChanging;
    property OnCheckClick: TGridCellNotifyEvent read FOnCheckClick write FOnCheckClick;
    property OnColumnResizing: TGridColumnResizeEvent read FOnColumnResizing write FOnColumnResizing;
    property OnColumnResize: TGridColumnResizeEvent read FOnColumnResize write FOnColumnResize;
    property OnColumnSizeToFit: TGridColumnResizeEvent read FOnColumnSizeToFit write FOnColumnSizeToFit;
    property OnDraw: TGridDrawEvent read FOnDraw write FOnDraw;
    property OnDrawCell: TGridDrawCellEvent read FOnDrawCell write FOnDrawCell;
    property OnDrawHeader: TGridDrawHeaderEvent read FOnDrawHeader write FOnDrawHeader;
    property OnEditAcceptKey: TGridAcceptKeyEvent read FOnEditAcceptKey write FOnEditAcceptKey;
    property OnEditButtonPress: TGridCellNotifyEvent read FOnEditButtonPress write FOnEditButtonPress;
    property OnEditCanceled: TGridCellNotifyEvent read FOnEditCanceled write FOnEditCanceled;
    property OnEditCanModify: TGridEditCanModifyEvent read FOnEditCanModify write FOnEditCanModify;
    property OnEditCanShow: TGridEditCanShowEvent read FOnEditCanShow write FOnEditCanShow;
    property OnEditChange: TGridCellNotifyEvent read FOnEditChange write FOnEditChange;
    property OnEditCloseUp: TGridEditCloseUpEvent read FOnEditCloseUp write FOnEditCloseUp;
    property OnEditCloseUpEx: TGridEditCloseUpExEvent read FOnEditCloseUpEx write FOnEditCloseUpEx;
    property OnEditSelectNext: TGridTextEvent read FOnEditSelectNext write FOnEditSelectNext;
    property OnGetCellColors: TGridCellColorsEvent read FOnGetCellColors write FOnGetCellColors;
    property OnGetCellImage: TGridCellImageEvent read FOnGetCellImage write FOnGetCellImage;
    property OnGetCellImageEx: TGridCellImageExEvent read FOnGetCellImageEx write FOnGetCellImageEx;
    property OnGetCellImageIndent: TGridCellIndentEvent read FOnGetCellImageIndent write FOnGetCellImageIndent;
    property OnGetCellHintRect: TGridRectEvent read FOnGetCellHintRect write FOnGetCellHintRect;
    property OnGetCellReadOnly: TGridCellreadOnlyEvent read FOnGetCellReadOnly write FOnGetCellReadOnly;
    property OnGetCellText: TGridTextEvent read FOnGetCellText write FOnGetCellText;
    property OnGetCellTextIndent: TGridCellIndentEvent read FOnGetCellTextIndent write FOnGetCellTextIndent;
    property OnGetCheckAlignment: TGridCheckAlignmentEvent read FOnGetCheckAlignment write FOnGetCheckAlignment;
    property OnGetCheckImage: TGridCheckImageEvent read FOnGetCheckImage write FOnGetCheckImage;
    property OnGetCheckIndent: TGridCellIndentEvent read FOnGetCheckIndent write FOnGetCheckIndent;
    property OnGetCheckKind: TGridCheckKindEvent read FOnGetCheckKind write FOnGetCheckKind;
    property OnGetCheckState: TGridCheckStateEvent read FOnGetCheckState write FOnGetCheckState;
    property OnGetCheckStateEx: TGridCheckStateExEvent read FOnGetCheckStateEx write FOnGetCheckStateEx;
    property OnGetEditList: TGridEditListEvent read FOnGetEditList write FOnGetEditList;
    property OnGetEditListBounds: TGridRectEvent read FOnGetEditListBounds write FOnGetEditListBounds;
    property OnGetEditListIndex: TGridEditListIndexEvent read FOnGetEditListIndex write FOnGetEditListIndex;
    property OnGetEditMask: TGridTextEvent read FOnGetEditMask write FOnGetEditMask;
    property OnGetEditStyle: TGridEditStyleEvent read FOnGetEditStyle write FOnGetEditStyle;
    property OnGetEditText: TGridTextEvent read FOnGetEditText write FOnGetEditText;
    property OnGetGridHint: TGridHintEvent read FOnGetGridHint write FOnGetGridHint;
    property OnGetGridColor: TGridColorEvent read FOnGetGridColor write FOnGetGridColor;
    property OnGetHeaderColors: TGridHeaderColorsEvent read FOnGetHeaderColors write FOnGetHeaderColors;
    property OnGetHeaderImage: TGridHeaderImageEvent read FOnGetHeaderImage write FOnGetHeaderImage;
    property OnGetSortDirection: TGridSortDirectionEvent read FOnGetSortDirection write FOnGetSortDirection;
    property OnGetSortImage: TGridSortImageEvent read FOnGetSortImage write FOnGetSortImage;
    property OnGetTipsRect: TGridRectEvent read FOnGetTipsRect write FOnGetTipsRect;
    property OnGetTipsText: TGridTextEvent read FOnGetTipsText write FOnGetTipsText;
    property OnHeaderClick: TGridHeaderClickEvent read FOnHeaderClick write FOnHeaderClick;
    property OnHeaderClicking: TGridHeaderClickingEvent read FOnHeaderClicking write FOnHeaderClicking;
    property OnHeaderDetailsClick: TNotifyEvent read FOnHeaderDetailsClick write FOnHeaderDetailsClick;
    property OnSetEditText: TGridTextEvent read FOnSetEditText write FOnSetEditText;
    property OnTextNotFound: TGridFindTextEvent read FOnTextNotFound write FOnTextNotFound;
  end;

{ TGridView }

  TGridView = class(TCustomGridView)
  published
    property Align;
    property AllowEdit;
    property AllowSelect;
    property AlwaysEdit;
    property AlwaysSelected;
    property Anchors;
    property BevelEdges;
    property BevelInner;
    property BevelOuter;
    property BevelKind default bkNone;
    property BevelWidth;
    property BorderStyle;
    property CancelOnExit;
    property CheckBoxes;
    property CheckStyle;
    property Color;
    property ColumnClick;
    property Columns;
    property ColumnsFullDrag;
    property Constraints;
    property Ctl3D;
    property CursorKeys;
    property DefaultEditMenu;
    property DefaultHeaderMenu;
    property DragCursor;
    property DragMode;
    property DoubleBuffered default False;
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
    property OnColumnResizing;
    property OnColumnResize;
    property OnColumnSizeToFit;
    property OnContextPopup;
    property OnDblClick;
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
    property OnStartDrag;
  end;

{ Cell utilities }

function GridCell(Col, Row: Longint): TGridCell;

function IsCellEqual(Cell1, Cell2: TGridCell): Boolean;
function IsCellEmpty(Cell: TGridCell): Boolean;

function OffsetCell(Cell: TGridCell; C, R: Longint): TGridCell;

implementation

uses
  Themes, UxTheme;

resourcestring
  SHeaderDetails = '&Details...';
  STextNotFound = 'Can not find "%s".';

function GridCell(Col, Row: Longint): TGridCell;
begin
  Result.Col := Col;
  Result.Row := Row;
end;

function IsCellEqual(Cell1, Cell2: TGridCell): Boolean;
begin
  Result := (Cell1.Col = Cell2.Col) and (Cell1.Row = Cell2.Row);
end;

function IsCellEmpty(Cell: TGridCell): Boolean;
begin
  Result := (Cell.Col = -1) or (Cell.Row = -1);
end;

function OffsetCell(Cell: TGridCell; C, R: Longint): TGridCell;
begin
  Result.Col := Cell.Col + C;
  Result.Row := Cell.Row + R;
end;

{ TGridHeaderSection }

procedure TGridHeaderSection.DefineProperties(Filer: TFiler);
begin
  inherited;
  { backward compatibility }
  Filer.DefineProperty('Width', ReadWidth, nil, False);
end;

destructor TGridHeaderSection.Destroy;
begin
  inherited Destroy;
  FreeAndNil(FSections);
end;

function TGridHeaderSection.IsSectionsStored: Boolean;
begin
  Result := (FSections <> nil) and (FSections.Count > 0);
end;

procedure TGridHeaderSection.ReadWidth(Reader: TReader);
begin
  Reader.ReadInteger;
end;

function TGridHeaderSection.GetAllowClick: Boolean;
var
  I: Integer;
begin
  Result := False;
  { column AllowClick property can disable header click }
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Result := Header.Grid.Columns[I].AllowClick;
  end;
end;

function TGridHeaderSection.GetBoundsRect: TRect;
var
  R: TRect;
begin
  if (Header = nil) or (Header.Grid = nil) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  { get bounds relative to the upper-left corner of the header }
  Result := FBoundsRect;
  R := Header.Grid.GetHeaderRect;
  OffsetRect(Result, R.Left, R.Top);
  { non-fixed sections should scroll as their columns }
  if not FixedColumn then
    OffsetRect(Result, Header.Grid.GetGridOrigin.X, 0);
end;

function TGridHeaderSection.GetDisplayName: string;
begin
  Result := DisplayText;
  if Result = '' then Result := inherited GetDisplayName;
end;

function TGridHeaderSection.GetDisplayText: string;
var
  I: Integer;
begin
  Result := Caption;
  if (Length(Result) = 0) and (not IsSectionsStored) and (Header <> nil) and
    (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
      Result := Header.Grid.Columns[I].Caption;
  end;
end;

function TGridHeaderSection.GetFirstColumnIndex: Integer;
begin
  if Sections.Count > 0 then
  begin
    Result := Sections[0].FirstColumnIndex;
    Exit;
  end;
  Result := ColumnIndex;
end;

function TGridHeaderSection.GetFixedColumn: Boolean;
begin
  if Sections.Count > 0 then
  begin
    Result := Sections[0].FixedColumn;
    Exit;
  end;
  if (Header = nil) or (Header.Grid = nil) then
  begin
    Result := False;
    Exit;
  end;
  Result := ColumnIndex < Header.Grid.Fixed.Count;
end;

function TGridHeaderSection.GetHeader: TCustomGridHeader;
begin
  if ParentSections <> nil then
  begin
    Result := ParentSections.Header;
    Exit;
  end;
  Result := nil;
end;

function TGridHeaderSection.GetLevel: Integer;
begin
  if Parent <> nil then
  begin
    Result := Parent.Level + 1;
    Exit;
  end;
  Result := 0
end;

function TGridHeaderSection.GetParent: TGridHeaderSection;
begin
  if ParentSections <> nil then
  begin
    Result := ParentSections.OwnerSection;
    Exit;
  end;
  Result := nil;
end;

function TGridHeaderSection.GetParentSections: TGridHeaderSections;
begin
  if Collection <> nil then
  begin
    Result := TGridHeaderSections(Collection);
    Exit;
  end;
  Result := nil;
end;

function TGridHeaderSection.GetSections: TGridHeaderSections;
begin
  if FSections = nil then
    FSections := TGridHeaderSections.Create(Header, Self);
  Result := FSections;
end;

function TGridHeaderSection.GetResizeColumnIndex: Integer;
var
  I: Integer;
begin
  { the resize column is the column of the rightmost subsection }
  for I := Sections.Count - 1 downto 0 do
    if Sections[I].Visible then
    begin
      Result := Sections[I].ResizeColumnIndex;
      Exit;
    end;
  Result := FColumnIndex;
end;

function TGridHeaderSection.GetVisible: Boolean;
var
  I: Integer;
begin
  if Sections.Count > 0 then
    for I := 0 to Sections.Count - 1 do
      if Sections[I].Visible then
      begin
        Result := True;
        Exit;
      end;
  { section visibility is the visibility of the corresponding column }
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
    begin
      Result := Header.Grid.Columns[I].Visible;
      Exit;
    end;
  end;
  { sections without columns (nonsense) are always visible }
  Result := True;
end;

function TGridHeaderSection.GetWidth: Integer;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  if Sections.Count > 0 then
  begin
    Result := 0;
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      Result := Result + S.Width;
    end;
    Exit;
  end;
  { section with is the with of the corresponding column }
  if (Header <> nil) and (Header.Grid <> nil) then
  begin
    I := ColumnIndex;
    if I < Header.Grid.Columns.Count then
    begin
      Result := Header.Grid.Columns[I].Width;
      Exit;
    end;
  end;
  { sections without columns (nonsense) do not have width }
  Result := 0;
end;

procedure TGridHeaderSection.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.SetSections(Value: TGridHeaderSections);
begin
  Sections.Assign(Value);
end;

procedure TGridHeaderSection.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

procedure TGridHeaderSection.Assign(Source: TPersistent);
begin
  if Source is TGridHeaderSection then
  begin
    Sections := TGridHeaderSection(Source).Sections;
    Caption := TGridHeaderSection(Source).Caption;
    Alignment := TGridHeaderSection(Source).Alignment;
    WordWrap := TGridHeaderSection(Source).WordWrap;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridHeaderSections }

constructor TGridHeaderSections.Create(AHeader: TCustomGridHeader; AOwnerSection: TGridHeaderSection);
begin
  inherited Create(TGridHeaderSection);
  FHeader := AHeader;
  FOwnerSection := AOwnerSection;
end;

function TGridHeaderSections.GetMaxColumn: Integer;
begin
  if Count > 0 then
  begin
    Result := Sections[Count - 1].ColumnIndex;
    Exit;
  end;
  Result := 0;
end;

function TGridHeaderSections.GetMaxLevel: Integer;

  procedure DoGetMaxLevel(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      if Result < S.Level then Result := S.Level;
      DoGetMaxLevel(S.Sections);
    end;
  end;

begin
  Result := 0;
  DoGetMaxLevel(Self);
end;

function TGridHeaderSections.GetSection(Index: Integer): TGridHeaderSection;
begin
  Result := TGridHeaderSection(inherited GetItem(Index));
end;

procedure TGridHeaderSections.SetSection(Index: Integer; Value: TGridHeaderSection);
begin
  inherited SetItem(Index, Value);
end;

function TGridHeaderSections.GetOwner: TPersistent;
begin
  Result := OwnerSection;
  if Result = nil then Result := Header;
end;

procedure TGridHeaderSections.Update(Item: TCollectionItem);
begin
  if Header <> nil then Header.Change;
end;

function TGridHeaderSections.Add: TGridHeaderSection;
begin
  if (Header = nil) or (Header.Grid = nil) then
  begin
    Result := TGridHeaderSection(inherited Add);
    Exit;
  end;
  Result := Header.Grid.CreateHeaderSection(Self);
end;

{ TCustomGridHeader }

constructor TCustomGridHeader.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FAutoHeight := True;
  FColor := clBtnFace;
  FSections := TGridHeaderSections.Create(Self, nil);
  FSectionHeight := 17;
  FColor := clBtnFace;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FGridFont := True;
  FImagesLink := TChangeLink.Create;
  FImagesLink.OnChange := ImagesChange;
  FFLat := True;
end;

destructor TCustomGridHeader.Destroy;
begin
  FOnChange := nil;
  FreeAndNil(FImagesLink);
  inherited Destroy;
  FreeAndNil(FSections);
  FreeAndNil(FFont);
end;

procedure TCustomGridHeader.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { backward compatibility }
  Filer.DefineProperty('FullSynchronizing', ReadFullSynchronizing, nil, False);
  Filer.DefineProperty('Synchronized', ReadFullSynchronizing, nil, False);
  Filer.DefineProperty('AutoSynchronize', ReadFullSynchronizing, nil, False);
end;

function TCustomGridHeader.IsColorStored: Boolean;
begin
  Result := not GridColor;
end;

function TCustomGridHeader.IsFontStored: Boolean;
begin
  Result := not GridFont;
end;

function TCustomGridHeader.IsSectionHeightStored: Boolean;
begin
  Result := (not FAutoHeight) and (FSectionHeight <> 17);
end;

function TCustomGridHeader.IsSectionsStored: Boolean;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  for I := 0 to Sections.Count - 1 do
  begin
    S := Sections[I];
    if (S.Caption <> '') or (S.Alignment <> taLeftJustify) or S.WordWrap or
      ((S.FSections <> nil) and (S.Sections.Count > 0)) then
    begin
      Result := True;
      Exit;
    end;
  end;
  Result := False;
end;

procedure TCustomGridHeader.ImagesChange(Sender: TObject);
begin
  Change;
end;

procedure TCustomGridHeader.FontChange(Sender: TObject);
begin
  FGridFont := False;
  SetSectionHeight(SectionHeight);
  Change;
end;

function TCustomGridHeader.GetHeight: Integer;
begin
  Result := (GetMaxLevel + 1) * SectionHeight;
end;

function TCustomGridHeader.GetMaxColumn: Integer;
begin
  Result := Sections.MaxColumn;
end;

function TCustomGridHeader.GetMaxLevel: Integer;
begin
  Result := Sections.MaxLevel;
end;

function TCustomGridHeader.GetNamePath: string;
begin
  if FGrid <> nil then
    Result := FGrid.Name + '.Header'
  else
    Result := inherited GetNamePath;
end;

function TCustomGridHeader.GetOwner: TPersistent;
begin
  Result := FGrid;
end;

function TCustomGridHeader.GetWidth: Integer;
var
  I: Integer;
  S: TGridHeaderSection;
begin
  Result := 0;
  for I := 0 to Sections.Count - 1 do
  begin
    S := Sections[I];
    Result := Result + S.Width;
  end;
end;

procedure TCustomGridHeader.ReadFullSynchronizing(Reader: TReader);
begin
  Reader.ReadBoolean;
end;

procedure TCustomGridHeader.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;
    if Value then SetSectionHeight(SectionHeight);
  end;
end;

procedure TCustomGridHeader.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGridColor := False;
    Change;
  end;
end;

procedure TCustomGridHeader.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then FImages.UnRegisterChanges(FImagesLink);
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImagesLink);
      if Grid <> nil then FImages.FreeNotification(Grid);
    end;
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetPopupMenu(const Value: TPopupMenu);
begin
  if FPopupMenu <> Value then
  begin
    FPopupMenu := Value;
    if (Value <> nil) and (Grid <> nil) then Value.FreeNotification(Grid);
  end;
end;

procedure TCustomGridHeader.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    { fixed columns and header must have the same 3D effects }
    if Value and (Grid <> nil) then
      Grid.Fixed.Flat := True;
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomGridHeader.SetGridColor(Value: Boolean);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if Grid <> nil then GridColorChanged(Grid.Color);
    SetSectionHeight(SectionHeight);
    Change;
  end;
end;

procedure TCustomGridHeader.SetGridFont(Value: Boolean);
begin
  if FGridFont <> Value then
  begin
    FGridFont := Value;
    if Grid <> nil then GridFontChanged(Grid.Font);
    Change;
  end;
end;

procedure TCustomGridHeader.SetSections(Value: TGridHeaderSections);
begin
  FSections.Assign(Value);
end;

procedure TCustomGridHeader.SetSectionHeight(Value: Integer);
var
  TH, IH: Integer;
begin
  if AutoHeight then
  begin
    TH := Grid.GetFontHeight(Font) + 2 * 2;
    IH := 0;
    if Images <> nil then
    begin
      IH := Images.Height + 2{+ 1};
      if not GridColor then Inc(IH, 1);
      if not Flat then Inc(IH, 1);
    end;
    Value := MaxIntValue([0, TH, IH]);
    { take into account borders (conditions are made in the order of drawing
      the header background in PaintHeaderBackground() }
    if not StyleServices.Enabled then
    begin
      { double line or 3D frame }
      if Flat then Inc(Value, 2)
      else Inc(Value, 4)
    end
    else if Grid <> nil then
    begin
      { in Windows XP with themes enabled, we need additional space at the
        bottom of the header for rounding, and in Windows Vista - for
        the sort arrow }
      if CheckWin32Version(6, 0) then
        Inc(Value, Grid.GetSortArrowSize.cy) // + 2
      else
        Inc(Value, 3);
    end;
  end;
  if Value < 0 then Value := 0;
  if FSectionHeight <> Value then
  begin
    FSectionHeight := Value;
    Change;
  end;
end;

procedure TCustomGridHeader.Change;
begin
  UpdateSections;
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomGridHeader.GridColorChanged(NewColor: TColor);
begin
  if FGridColor then
  begin
    SetColor(NewColor);
    FGridColor := True;
  end;
end;

procedure TCustomGridHeader.GridFontChanged(NewFont: Tfont);
begin
  if FGridFont then
  begin
    SetFont(NewFont);
    FGridFont := True;
  end;
end;

procedure TCustomGridHeader.Assign(Source: TPersistent);
begin
  if Source is TCustomGridHeader then
  begin
    Sections := TCustomGridHeader(Source).Sections;
    SectionHeight := TCustomGridHeader(Source).SectionHeight;
    Color := TCustomGridHeader(Source).Color;
    GridColor := TCustomGridHeader(Source).GridColor;
    Font := TCustomGridHeader(Source).Font;
    GridFont := TCustomGridHeader(Source).GridFont;
    Exit;
  end;
  inherited Assign(Source);
end;

procedure TCustomGridHeader.SynchronizeSections;
var
  C: Integer;

  procedure DoAddSections(Column: Integer);
  var
    R: TRect;
  begin
    R.Left := Grid.GetColumnLeftRight(Column).Left;
    R.Right := R.Left;
    R.Top := Grid.ClientRect.Top;
    R.Bottom := R.Top + Height;
    while Column < Grid.Columns.Count do
    begin
      R.Left := R.Right;
      R.Right := R.Left + Grid.Columns[Column].Width;
      with Sections.Add do
      begin
        FColumnIndex := Column;
        FBoundsRect := R;
      end;
      Inc(Column);
    end;
  end;

  procedure DoDeleteSections(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count - 1 downto 0 do
    begin
      S := Sections[I];
      DoDeleteSections(S.Sections);
      if (S.Sections.Count = 0) and (S.ColumnIndex > Grid.Columns.Count - 1) then S.Free;
    end;
  end;

  procedure DoSynchronizeSections(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count - 1 downto 0 do
    begin
      S := Sections[I];
      if S.Sections.Count = 0 then
      begin
        C := S.ColumnIndex;
      end
      else
        DoSynchronizeSections(S.Sections);
    end;
  end;

begin
  if (Grid <> nil) and (Grid.ComponentState * [csReading, csLoading] = [])
    and (Grid.Columns <> nil) then
  begin
    Sections.BeginUpdate;
    try
      UpdateSections;
      if Sections.Count = 0 then
      begin
        DoAddSections(0);
        Exit;
      end;
      C := Sections[Sections.Count - 1].ColumnIndex;
      if C < Grid.Columns.Count - 1 then
        DoAddSections(C + 1)
      else if C > Grid.Columns.Count - 1 then
        DoDeleteSections(Sections);
      DoSynchronizeSections(Sections);
    finally
      Sections.EndUpdate;
    end;
  end;
end;

procedure TCustomGridHeader.UpdateSections;
var
  R: TRect;
  C: Integer;

  procedure DoUpdateColumnIndex(Sections: TGridHeaderSections);
  var
    I: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      if S.Sections.Count = 0 then
      begin
        S.FColumnIndex := C;
        Inc(C);
      end
      else
      begin
        DoUpdateColumnIndex(S.Sections);
        S.FColumnIndex := S.Sections[S.Sections.Count - 1].FColumnIndex;
      end;
    end;
  end;

  procedure DoUpdateSecionsBounds(Sections: TGridHeaderSections; Rect: TRect);
  var
    I: Integer;
    S: TGridHeaderSection;
    R, SR: TRect;
  begin
    R := Rect;
    R.Right := R.Left;
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      R.Left := R.Right;
      R.Right := R.Left + S.Width;
      SR := R;
      if S.Sections.Count > 0 then SR.Bottom := R.Top + SectionHeight;
      S.FBoundsRect := SR;
      if S.Sections.Count > 0 then
      begin
        SR.Top := SR.Bottom;
        SR.Bottom := R.Bottom;
        DoUpdateSecionsBounds(S.Sections, SR);
      end;
    end;
  end;

begin
  if (Grid <> nil) and (Grid.ComponentState * [csReading, csLoading] = [])
    and (Grid.Columns <> nil) then
  begin
    C := 0;
    DoUpdateColumnIndex(Sections);
    R.Left := Grid.ClientRect.Left;
    R.Right := R.Left + Grid.GetColumnsWidth(0, Grid.Columns.Count - 1);
    R.Top := Grid.ClientRect.Top;
    R.Bottom := R.Top + Height;
    DoUpdateSecionsBounds(Sections, R);
  end;
end;

{ TCustomGridColumn }

constructor TCustomGridColumn.Create(Collection: TCollection);
begin
  inherited Create(Collection);
  FColumns := TGridColumns(Collection);
  FWidth := 64;
  FMinWidth := 0;
  FMaxWidth := 10000;
  FAlignment := taLeftJustify;
  FTabStop := True;
  FVisible := True;
  FAllowClick := True;
  FAllowEdit := True;
  FCheckAlignment := taLeftJustify;
  FDefaultPopup := True;
end;

destructor TCustomGridColumn.Destroy;
begin
  inherited;
  FreeAndNil(FPickList);
end;

function TCustomGridColumn.GetWidth: Integer;
begin
  if FVisible then
    Result := FWidth
  else
    Result := 0;
end;

function TCustomGridColumn.GetCaption2: string;
var
  S: TGridHeaderSection;
begin
  Result := Caption;
  S := HeaderSection;
  if S <> nil then
  begin
    if Length(S.Caption) <> 0 then Result := S.Caption;
    S := S.Parent;
    while S <> nil do
    begin
      Result := S.Caption + ' - ' + Result;
      S := S.Parent;
    end;
  end;
end;

function TCustomGridColumn.GetEditAlignment: TAlignment;
begin
  if AlignEdit then Result := Alignment else Result := taLeftJustify;
end;

function TCustomGridColumn.GetGrid: TCustomGridView;
begin
  Result := nil;
  if Columns <> nil then Result := TCustomGridView(Columns.Grid);
end;

function TCustomGridColumn.GetHeaderSection: TGridHeaderSection;
begin
  Result := nil;
  if Grid <> nil then Result := Grid.GetHeaderSection(Index, -1);
end;

function TCustomGridColumn.GetPickList: TStrings;
begin
  if FPickList = nil then FPickList := TStringList.Create;
  Result := FPickList;
end;

function TCustomGridColumn.GetPickListCount: Integer;
begin
  Result := 0;
  if FPickList <> nil then Result := FPickList.Count;
end;

function TCustomGridColumn.IsPickListStored: Boolean;
begin
  Result := GetPickListCount <> 0;
end;

procedure TCustomGridColumn.ReadMultiline(Reader: TReader);
begin
  WantReturns := Reader.ReadBoolean;
end;

procedure TCustomGridColumn.SetAlignEdit(Value: Boolean);
begin
  if FAlignEdit <> Value then
  begin
    FAlignEdit := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetAllowEdit(Value: Boolean);
begin
  if AllowEdit <> Value then
  begin
    FAllowEdit := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCheckAlignment(Value: TAlignment);
begin
  if FCheckAlignment <> Value then
  begin
    FCheckAlignment := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCheckKind(Value: TGridCheckKind);
begin
  if FCheckKind <> Value then
  begin
    FCheckKind := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditWordWrap(Value: TGridEditWordWrap);
begin
  if FEditWordWrap <> Value then
  begin
    FEditWordWrap := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetFixedSize(Value: Boolean);
begin
  if FFixedSize <> Value then
  begin
    FFixedSize := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetMaxWidth(Value: Integer);
begin
  if Value < FMinWidth then Value := FMinWidth;
  if Value > 10000 then Value := 10000;
  FMaxWidth := Value;
  SetWidth(FWidth);
end;

procedure TCustomGridColumn.SetMinWidth(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > FMaxWidth then Value := FMaxWidth;
  FMinWidth := Value;
  { patch: prevent TDBGridColumn.DefaultColumn property reset when setting
    minimum width }
  Inc(FWidthLock);
  try
    SetWidth(FWidth);
  finally
    Dec(FWidthLock);
  end;
end;

procedure TCustomGridColumn.SetPickList(Value: TStrings);
begin
  if Value = nil then
    FreeAndNil(FPickList)
  else
    PickList.Assign(Value);
end;

procedure TCustomGridColumn.SetTabStop(Value: Boolean);
begin
  if FTabStop <> Value then
  begin
    FTabStop := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetWantReturns(Value: Boolean);
begin
  if FWantReturns <> Value then
  begin
    FWantReturns := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    Changed(False);
  end;
end;

function TCustomGridColumn.GetDisplayName: string;
begin
  Result := FCaption;
  if Result = '' then Result := inherited GetDisplayName;
end;

procedure TCustomGridColumn.SetAlignment(Value: TAlignment);
begin
  if Alignment <> Value then
  begin
    FAlignment := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetCaption(const Value: string);
begin
  if FCaption <> Value then
  begin
    FCaption := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditMask(const Value: string);
begin
  if FEditMask <> Value then
  begin
    FEditMask := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetEditStyle(Value: TGridEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetMaxLength(Value: Integer);
begin
  if FMaxLength <> Value then
  begin
    FMaxLength := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    Changed(False);
  end;
end;

procedure TCustomGridColumn.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Changed(True);
  end;
end;

procedure TCustomGridColumn.SetWidth(Value: Integer);
begin
  if Value < FMinWidth then Value := FMinWidth;
  if Value > FMaxWidth then Value := FMaxWidth;
  if FWidth <> Value then
  begin
    FWidth := Value;
    Changed(True);
  end;
end;

procedure TCustomGridColumn.DefineProperties(Filer: TFiler);
begin
  inherited DefineProperties(Filer);
  { backward compatibility }
  Filer.DefineProperty('Multiline', ReadMultiline, nil, False);
end;

procedure TCustomGridColumn.Assign(Source: TPersistent);
begin
  if Source is TCustomGridColumn then
  begin
    Caption := TCustomGridColumn(Source).Caption;
    DefWidth := TCustomGridColumn(Source).DefWidth;
    MinWidth := TCustomGridColumn(Source).MinWidth;
    MaxWidth := TCustomGridColumn(Source).MaxWidth;
    FixedSize := TCustomGridColumn(Source).FixedSize;
    MaxLength := TCustomGridColumn(Source).MaxLength;
    Alignment := TCustomGridColumn(Source).Alignment;
    ReadOnly := TCustomGridColumn(Source).ReadOnly;
    EditStyle := TCustomGridColumn(Source).EditStyle;
    EditMask := TCustomGridColumn(Source).EditMask;
    CheckKind := TCustomGridColumn(Source).CheckKind;
    CheckAlignment := TCustomGridColumn(Source).CheckAlignment;
    WantReturns := TCustomGridColumn(Source).WantReturns;
    WordWrap := TCustomGridColumn(Source).WordWrap;
    TabStop := TCustomGridColumn(Source).TabStop;
    Visible := TCustomGridColumn(Source).Visible;
    PickList := TCustomGridColumn(Source).FPickList;
    Tag := TCustomGridColumn(Source).Tag;
    AllowClick := TCustomGridColumn(Source).AllowClick;
    AllowEdit := TCustomGridColumn(Source).AllowEdit;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridColumns }

constructor TGridColumns.Create(AGrid: TCustomGridView);
var
  AClass: TGridColumnClass;
begin
  AClass := TGridColumn;
  if AGrid <> nil then AClass := AGrid.GetColumnClass;
  inherited Create(AClass);
  FGrid := AGrid;
end;

function TGridColumns.GetColumn(Index: Integer): TGridColumn;
begin
  Result := TGridColumn(inherited GetItem(Index));
end;

function TGridColumns.GetLayout: string;
var
  I, W: Integer;
  Strings: TStringList;
begin
  Strings := TStringList.Create;
  try
    for I := 0 to Count - 1 do
    begin
      { negative width means invisible column }
      W := Columns[I].DefWidth;
      if not Columns[I].Visible then W := W * (-1);
      Strings.Add(IntToStr(W));
    end;
    { layout is comma-delimited string }
    Result := Strings.CommaText;
  finally
    Strings.Free;
  end;
end;

procedure TGridColumns.SetColumn(Index: Integer; Value: TGridColumn);
begin
  inherited SetItem(Index, Value);
end;

procedure TGridColumns.SetLayout(const Value: string);
var
  I, W: Integer;
  Strings: TStringList;
begin
  BeginUpdate;
  try
    Strings := TStringList.Create;
    try
      Strings.CommaText := Value;
      for I := 0 to Strings.Count - 1 do
      begin
        if I > Count - 1 then Break;
        { see comment in SetMinWidth() }
        Inc(Columns[I].FWidthLock);
        try
          W := StrToIntDef(Strings[I], Columns[I].DefWidth);
          Columns[I].DefWidth := Abs(W);
          Columns[I].Visible := W > 0;
        finally
          Dec(Columns[I].FWidthLock);
        end;
      end;
    finally
      Strings.Free;
    end;
  finally
    EndUpdate;
  end;
end;

function TGridColumns.GetOwner: TPersistent;
begin
  Result := FGrid; 
end;

procedure TGridColumns.Update(Item: TCollectionItem);
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

function TGridColumns.Add: TGridColumn;
begin
  if Grid = nil then
  begin
    Result := TGridColumn(inherited Add);
    Exit;
  end;
  Result := TGridColumn(Grid.CreateColumn(Self));
end;

{ TCustomGridRows }

constructor TCustomGridRows.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FHeight := 17;
  FAutoHeight := True;
end;

destructor TCustomGridRows.Destroy;
begin
  FOnChange := nil;
  SetCount(0);
  inherited Destroy;
end;

function TCustomGridRows.GetMaxCount: Integer;
begin
  Result := MaxInt - 2;
  if Height > 0 then Result := Result div Height - 2;
end;

function TCustomGridRows.IsHeightStored: Boolean;
begin
  Result := (not FAutoHeight) and (FHeight <> 17);
end;

procedure TCustomGridRows.SetAutoHeight(Value: Boolean);
begin
  if FAutoHeight <> Value then
  begin
    FAutoHeight := Value;            
    if Value then SetHeight(Height);
  end;
end;

procedure TCustomGridRows.SetHeight(Value: Integer);
var
  TH, FH, CH, IH, GH: Integer;
begin
  if AutoHeight and (Grid <> nil) then
  begin
    { the height of text }
    TH := Grid.GetFontHeight(Grid.Font) + Grid.TextTopIndent + 1;
    FH := Grid.GetFontHeight(Grid.Fixed.Font) + Grid.TextTopIndent + 1;
    { the height of check boxes }
    if not Grid.CheckBoxes then CH := 0
    else CH := Grid.CheckHeight + Grid.CheckTopIndent + 1;
    { the height of images }
    if Grid.Images = nil then IH := 0
    else IH := Grid.Images.Height + Grid.ImageTopIndent + 1;
    { grid lines }
    if not (Grid.GridLines and (gsHorzLine in Grid.GridStyle)) then GH := 0
    else
    begin
      GH := Grid.FGridLineWidth;
      if (Grid.Fixed.Count > 0) and (not Grid.Fixed.Flat) and
        (not StyleServices.Enabled) then Inc(GH, 1); // <- 3D
    end;
    Value := MaxIntValue([0, TH, FH, CH, IH]) + GH + 1;
  end;
  if Value < 0 then Value := 0;
  if FHeight <> Value then
  begin
    FHeight := Value;
    if Count > MaxCount then SetCount(Count) else Change;
  end;
end;

procedure TCustomGridRows.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomGridRows.SetCount(Value: Integer);
begin
  if Value < 0 then Value := 0;
  if Value > MaxCount then Value := MaxCount;
  if FCount <> Value then
  begin
    FCount := Value;
    Change;
  end;
end;

procedure TCustomGridRows.Assign(Source: TPersistent);
begin
  if Source is TCustomGridRows then
  begin
    Count := TCustomGridRows(Source).Count;
    Height := TCustomGridRows(Source).Height;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TCustomGridFixed }

constructor TCustomGridFixed.Create(AGrid: TCustomGridView);
begin
  inherited Create;
  FGrid := AGrid;
  FColor := clBtnFace;
  FFont := TFont.Create;
  FFont.OnChange := FontChange;
  FGridFont := True;
  FFLat := True;
  FShowDivider := True;
end;

destructor TCustomGridFixed.Destroy;
begin
  FOnChange := nil;
  inherited Destroy;
  FreeandNil(FFont);
end;

function TCustomGridFixed.IsColorStored: Boolean;
begin
  Result := not GridColor;
end;

function TCustomGridFixed.IsFontStored: Boolean;
begin
  Result := not GridFont;
end;

procedure TCustomGridFixed.FontChange(Sender: TObject);
begin
  FGridFont := False;
  Change;
end;

procedure TCustomGridFixed.SetColor(Value: TColor);
begin
  if FColor <> Value then
  begin
    FColor := Value;
    FGridColor := False;
    Change;
  end;
end;

procedure TCustomGridFixed.SetFlat(Value: Boolean);
begin
  if FFlat <> Value then
  begin
    FFlat := Value;
    { fixed columns and header must have the same 3D effects }
    if (not Value) and (Grid <> nil) then
      Grid.Header.Flat := False;
    Change;
  end;
end;

procedure TCustomGridFixed.SetFont(Value: TFont);
begin
  FFont.Assign(Value);
end;

procedure TCustomGridFixed.SetGridColor(Value: Boolean);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    if Grid <> nil then GridColorChanged(Grid.Color);
    Change;
  end;
end;

procedure TCustomGridFixed.SetGridFont(Value: Boolean);
begin
  if FGridFont <> Value then
  begin
    FGridFont := Value;
    if Grid <> nil then GridFontChanged(Grid.Font);
    Change;
  end;
end;

procedure TCustomGridFixed.SetShowDivider(Value: Boolean);
begin
  if FShowDivider <> Value then
  begin
    FShowDivider := Value;
    Change;
  end;
end;

procedure TCustomGridFixed.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TCustomGridFixed.GridColorChanged(NewColor: TColor);
begin
  if FGridColor then
  begin
    SetColor(NewColor);
    FGridColor := True;
  end;
end;

procedure TCustomGridFixed.GridFontChanged(NewFont: TFont);
begin
  if FGridFont then
  begin
    SetFont(NewFont);
    FGridFont := True;
  end;
end;

procedure TCustomGridFixed.SetCount(Value: Integer);
begin
  if (Grid <> nil) and (Value > Grid.Columns.Count - 1) then
    Value := Grid.Columns.Count - 1;
  if Value < 0 then Value := 0;
  if FCount <> Value then
  begin
    FCount := Value;
    Change;
  end;
end;

procedure TCustomGridFixed.Assign(Source: TPersistent);
begin
  if Source is TCustomGridFixed then
  begin
    Count := TCustomGridFixed(Source).Count;
    Color := TCustomGridFixed(Source).Color;
    GridColor := TCustomGridFixed(Source).GridColor;
    Font := TCustomGridFixed(Source).Font;
    GridFont := TCustomGridFixed(Source).GridFont;
    Exit;
  end;
  inherited Assign(Source);
end;

{ TGridScrollBar }

constructor TGridScrollBar.Create(AGrid: TCustomGridView; AKind: TScrollBarKind);
begin
  inherited Create;
  FGrid := AGrid;
  FKind := AKind;
  if AKind = sbHorizontal then FBarCode := SB_HORZ else FBarCode := SB_VERT;
  FPageStep := 100;
  FLineStep := 8;
  FLineSize := 1;
  FTracking := True;
  FVisible := True;
end;

procedure TGridScrollBar.SetLineSize(Value: Integer);
begin
  if Value < 1 then FLineSize := 1 else FLineSize := Value;
end;

procedure TGridScrollBar.SetLineStep(Value: Integer);
begin
  SetParams(Min, Max, PageStep, Value);
end;

procedure TGridScrollBar.SetMax(Value: Integer);
begin
  SetParams(Min, Value, PageStep, LineStep);
end;

procedure TGridScrollBar.SetMin(Value: Integer);
begin
  SetParams(Value, Max, PageStep, LineStep);
end;

procedure TGridScrollBar.SetPageStep(Value: Integer);
begin
  SetParams(Min, Max, Value, LineStep);
end;

procedure TGridScrollBar.SetVisible(Value: Boolean);
begin
  if FVisible <> Value then
  begin
    FVisible := Value;
    Update;
  end;
end;

procedure TGridScrollBar.Change;
begin
  if Assigned(FOnChange) then FOnChange(Self);
end;

procedure TGridScrollBar.ChangeParams;
begin
  if Assigned(FOnChangeParams) then FOnChangeParams(Self);
end;

procedure TGridScrollBar.Scroll(ScrollCode: Integer; var ScrollPos: Integer);
begin
  if Assigned(FOnScroll) then FOnScroll(Self, ScrollCode, ScrollPos);
end;

procedure TGridScrollBar.ScrollMessage(var Message: TWMScroll);
var
  ScrollInfo: TScrollInfo;
begin
  FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
  ScrollInfo.cbSize := SizeOf(ScrollInfo);
  ScrollInfo.fMask := SIF_TRACKPOS;
  if GetScrollInfo(FGrid.Handle, FBarCode, ScrollInfo) then
  begin
    LockUpdate;
    try
      case Message.ScrollCode of
        SB_LINELEFT: SetPositionEx(Position - LineStep, Message.ScrollCode);
        SB_LINERIGHT: SetPositionEx(Position + LineStep, Message.ScrollCode);
        SB_PAGELEFT: SetPositionEx(Position - PageStep, Message.ScrollCode);
        SB_PAGERIGHT: SetPositionEx(Position + PageStep, Message.ScrollCode);
        SB_THUMBPOSITION: SetPositionEx(ScrollInfo.nTrackPos, Message.ScrollCode);
        SB_THUMBTRACK: if Tracking then SetPositionEx(ScrollInfo.nTrackPos, Message.ScrollCode);
        SB_ENDSCROLL: SetPositionEx(Position, Message.ScrollCode);
      end;
    finally
      UnLockUpdate;
    end;
  end
  else
    inherited;
end;

procedure CheckScrollPos(Min, Max, PageStep: Integer; var Position: Integer);
begin
  if Position > Max - PageStep + 1 then Position := Max - PageStep + 1;
  if Position < Min then Position := Min;
end;

procedure TGridScrollBar.SetParams(AMin, AMax, APageStep, ALineStep: Integer);
begin
  if AMax < AMin then AMax := AMin;
  if APageStep > AMax - AMin + 1 then APageStep := AMax - AMin + 1;
  if APageStep < 0 then APageStep := 0;
  if ALineStep < 0 then ALineStep := 0;
  if (FMin <> AMin) or (FMax <> AMax) or (FPageStep <> APageStep) or (FLineStep <> ALineStep) then
  begin
    FMin := AMin;
    FMax := AMax;
    FPageStep := APageStep;
    FLineStep := ALineStep;
    CheckScrollPos(FMin, FMax, FPageStep, FPosition);
    Update;
    ChangeParams;
  end;
end;

procedure TGridScrollBar.SetPosition(Value: Integer);
begin
  SetPositionEx(Value, SB_ENDSCROLL);
end;

procedure TGridScrollBar.SetPositionEx(Value: Integer; ScrollCode: Integer);
var
  R: TRect;
begin
  CheckScrollPos(FMin, FMax, FPageStep, Value);
  if Value <> FPosition then
  begin
    Scroll(ScrollCode, Value);
    CheckScrollPos(FMin, FMax, FPageStep, Value);
  end;
  if Value <> FPosition then
  begin
    with FGrid do
    begin
      { focus rect should be hidden while scrolling grid }
      HideFocus;
      if FKind = sbHorizontal then
      begin
        UnionRect(R, GetHeaderRect, GetGridRect);
        R.Left := GetFixedRect.Right;
        ScrollWindowEx(Handle, (FPosition - Value) * FLineSize, 0, @R, @R, 0, nil, SW_INVALIDATE);
      end
      else
      begin
        R := GetGridRect;
        ScrollWindowEx(Handle, 0, (FPosition - Value) * FLineSize, @R, @R, 0, nil, SW_INVALIDATE);
      end;
      FPosition :=  Value;
      ShowFocus;
    end;
    Update;
    Change;
  end;
end;

procedure TGridScrollBar.Update;
var
  ScrollInfo: TScrollInfo;
begin
  if FGrid.HandleAllocated and (FUpdateLock = 0) then
  begin
    FillChar(ScrollInfo, SizeOf(ScrollInfo), 0);
    ScrollInfo.cbSize := SizeOf(ScrollInfo);
    ScrollInfo.fMask := SIF_RANGE or SIF_PAGE or SIF_POS;
    if Visible and (Max <> Min) then
    begin
      ScrollInfo.nMin := Min;
      ScrollInfo.nMax := Max;
      ScrollInfo.nPage := PageStep;
      ScrollInfo.nPos := Position;
    end;
    SetScrollInfo(FGrid.Handle, FBarCode, ScrollInfo, True);
  end;
end;

procedure TGridScrollBar.Assign(Source: TPersistent);
begin
  if Source is TGridScrollBar then
  begin
    LockUpdate;
    try
      PageStep := TGridScrollBar(Source).PageStep;
      LineStep := TGridScrollBar(Source).LineStep;
      Min := TGridScrollBar(Source).Min;
      Max := TGridScrollBar(Source).Max;
      Position := TGridScrollBar(Source).Position;
      Tracking := TGridScrollBar(Source).Tracking;
      Visible := TGridScrollBar(Source).Visible;
      Exit;
    finally
      UnLockUpdate;
    end;
  end;
  inherited Assign(Source);
end;

procedure TGridScrollBar.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TGridScrollBar.UnLockUpdate;
begin
  Dec(FUpdateLock);
  if FUpdateLock = 0 then Update;
end;

{ TGridListBox }

constructor TGridListBox.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ParentShowHint := False;
  ShowHint := False;
end;

procedure TGridListBox.CreateParams(var Params: TCreateParams);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_BORDER;
    ExStyle := WS_EX_TOOLWINDOW or WS_EX_TOPMOST;
    WindowClass.Style := CS_SAVEBITS;
  end;
end;

procedure TGridListBox.CreateWnd;
begin
  inherited CreateWnd;
  Windows.SetParent(Handle, 0);
  CallWindowProc(DefWndProc, Handle, WM_SETFOCUS, 0, 0);
end;

procedure TGridListBox.KeyPress(var Key: Char);
var
  TickCount: Integer;
begin
  case Key of
    #8, #27:
      { discard search text  }
      FSearchText := '';
    #32..#255:
      { search a list box }
      begin
        TickCount := Longint(GetTickCount);
        if Abs(TickCount - FSearchTime) > 2000 then FSearchText := '';
        FSearchTime := TickCount;
        if Length(FSearchText) < 32 then FSearchText := FSearchText + Key;
        SendMessage(Handle, LB_SELECTSTRING, WORD(-1), Longint(PChar(FSearchText)));
        Key := #0;
      end;
  end;
  inherited Keypress(Key);
end;

procedure TGridListBox.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  inherited MouseUp(Button, Shift, X, Y);
  if (Grid <> nil) and (Grid.Edit <> nil) then
    Grid.Edit.CloseUp((X >= 0) and (Y >= 0) and (X < Width) and (Y < Height));
end;

{ TCustomGridEdit }

constructor TCustomGridEdit.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FEditStyle := geSimple;
  FDropDownCount := 0;
  FButtonWidth := GetSystemMetrics(SM_CXVSCROLL);
  ParentCtl3D := False;
  Ctl3D := False;
  TabStop := False;
  BorderStyle := bsNone;
  ParentShowHint := False;
  ShowHint := False;
end;

function TCustomGridEdit.GetButtonRect: TRect;
begin
  Result := Rect(Width - FButtonWidth, 0, Width, Height);
end;

function TCustomGridEdit.GetClosingUp: Boolean;
begin
  Result := FCloseUpCount <> 0;
end;

function TCustomGridEdit.GetLineCount: Integer;
var
  P: PChar;
begin
  Result := 0;
  P := Pointer(Text);
  while P^ <> #0 do
  begin
    while not CharInSet(P^, [#0, #10, #13]) do Inc(P);
    Inc(Result);
    if P^ = #13 then Inc(P);
    if P^ = #10 then Inc(P);
  end;
end;

function TCustomGridEdit.GetPressing: Boolean;
begin
  Result := FPressCount <> 0;
end;

function TCustomGridEdit.GetVisible: Boolean;
begin
  Result := IsWindowVisible(Handle);
end;

procedure TCustomGridEdit.ListMouseUp(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (FActiveList <> nil) then CloseUp(PtInRect(FActiveList.ClientRect, Point(X, Y)));
end;

procedure TCustomGridEdit.SetAlignment(Value: TAlignment);
begin
  if FAlignment <> Value then
  begin
    FAlignment := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridEdit.SetButtonWidth(Value: Integer);
begin
  if FButtonWidth <> Value then
  begin
    FButtonWidth := Value;
    Repaint;
  end;
end;

procedure TCustomGridEdit.SetDropListVisible(Value: Boolean);
begin
  if Value then DropDown else CloseUp(False);
end;

procedure TCustomGridEdit.SetEditStyle(Value: TGridEditStyle);
begin
  if FEditStyle <> Value then
  begin
    FEditStyle := Value;
    Repaint;
  end;
end;

procedure TCustomGridEdit.SetWordWrap(Value: Boolean);
begin
  if FWordWrap <> Value then
  begin
    FWordWrap := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridEdit.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  inherited;
  with Message do
  begin
    Result := Result or DLGC_WANTARROWS or DLGC_WANTCHARS;
    if (Grid <> nil) and (gkTabs in Grid.CursorKeys) then
      Result := Result or DLGC_WANTTAB;
  end;
end;

procedure TCustomGridEdit.WMCancelMode(var Message: TMessage);
begin
  StopButtonTracking;
  inherited;
end;

procedure TCustomGridEdit.WMKillFocus(var Message: TMessage);
begin
  inherited;
  { ignore the focus message if the dialog box appears after selecting
    a drop-down list item, for example, a ColorDialog when selecting the
    "More Color ..." item in color list }
  if ClosingUp or Pressing then Exit;
  { stop editing the cell text when focus is lost }
  try
    CloseUp(False);
  except
    Application.HandleException(Self);
  end;
end;

procedure TCustomGridEdit.WMWindowPosChanged(var Message: TWMWindowPosChanged);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridEdit.WMPaint(var Message: TWMPaint);
begin
  PaintHandler(Message);
end;

procedure TCustomGridEdit.WMLButtonDown(var Message: TWMLButtonDown);
begin
  SendCancelMode(Self);
  with Message do
    { to avoid hiding text selection at the click of a button, we need to
      process the click manually }
    if (EditStyle <> geSimple) and PtInrect(ButtonRect, Point(XPos, YPos)) then
    begin
      if csCaptureMouse in ControlStyle then MouseCapture := True;
      MouseDown(mbLeft, KeysToShiftState(Keys), XPos, YPos);
    end
    else
    begin
      CloseUp(False);
      inherited;
    end;
end;

procedure TCustomGridEdit.WMLButtonDblClk(var Message: TWMLButtonDblClk);
var
  P: TPoint;
begin
  with Message do
  begin
    P := Point(XPos, YPos);
    if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, P) then Exit;
  end;
  inherited;
end;

procedure TCustomGridEdit.WMSetCursor(var Message: TWMSetCursor);
begin
  if (FEditStyle <> geSimple) and PtInRect(GetButtonRect, ScreenToClient(Mouse.CursorPos)) then
  begin
    Windows.SetCursor(LoadCursor(0, IDC_ARROW));
    Exit;
  end;
  inherited;
end;

procedure TCustomGridEdit.WMPaste(var Message);
begin
  if EditCanModify then inherited;
end;

procedure TCustomGridEdit.WMCut(var Message);
begin
  if EditCanModify then inherited;
end;

procedure TCustomGridEdit.WMClear(var Message);
begin
  if EditCanModify then inherited;
end;

procedure TCustomGridEdit.WMUndo(var Message);
begin
  if (Grid = nil) or Grid.EditCanUndo(Grid.EditCell) then inherited;
end;

procedure TCustomGridEdit.CMCancelMode(var Message: TCMCancelMode);
begin
  if (Message.Sender <> Self) and (Message.Sender <> FActiveList) then CloseUp(False);
end;

procedure TCustomGridEdit.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridEdit.CMShowingChanged(var Message: TMessage);
begin
  { ignore visibility change through Visible property change }
end;

procedure TCustomGridEdit.WMContextMenu(var Message: TMessage);
begin
  { if the DefaultPopupMenu property of the grid is True, then the standard
    popup menu of the inplace editor should be displayed, otherwise the grid
    popup menu will be displayed }
  if (Grid <> nil) and Grid.DefaultEditMenu and Assigned(Grid.PopupMenu) then
    with Message do
      Result := CallWindowProc(DefWndProc, Handle, Msg, WParam, LParam)
  else
    inherited;
end;

procedure TCustomGridEdit.CMFontChanged(var Message: TMessage);
begin
  inherited;
  { in Borland C ++ Builder, after changing the font, the position of the
    inplace editor is shifted, it must be corrected }
  UpdateBounds(Visible, False);
end;

procedure TCustomGridEdit.CMMouseEnter(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and not FButtonHot then
  begin
    FButtonHot := True;
    InvalidateButton;
  end;
end;

procedure TCustomGridEdit.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if StyleServices.Enabled and FButtonHot then
  begin
    FButtonHot := False;
    InvalidateButton;
  end;
end;

procedure TCustomGridEdit.Change;
begin
  if Grid <> nil then Grid.EditChange(Grid.EditCell);
end;

procedure TCustomGridEdit.CreateParams(var Params: TCreateParams);
const
  WordWraps: array[Boolean] of DWORD = (0, ES_AUTOHSCROLL);
  Aligns: array[TAlignment] of DWORD = (ES_LEFT, ES_RIGHT, ES_CENTER);
begin
  inherited CreateParams(Params);
  with Params do
    Style := Style and (not WordWraps[FWordWrap]) or ES_MULTILINE or
      Aligns[FAlignment];
end;

procedure TCustomGridEdit.DblClick;
begin
  if Grid <> nil then Grid.DblClick;
  { double click works like a button click }
  case EditStyle of
    geEllipsis: Press;
    gePickList, geDataList: if not FDropListVisible then SelectNext;
  end;
end;

function TCustomGridEdit.DoMouseWheel(Shift: TShiftState; WheelDelta: Integer; MousePos: TPoint): Boolean;
begin
  Result := (Grid <> nil) and Grid.DoMouseWheel(Shift, WheelDelta, MousePos);
end;

function TCustomGridEdit.EditCanModify: Boolean;
begin
  Result := (Grid <> nil) and Grid.EditCanModify(Grid.EditCell);
end;

function TCustomGridEdit.GetDropList: TWinControl;
begin
  if FPickList = nil then
  begin
    FPickList := TGridListBox.Create(Self);
    FPickList.IntegralHeight := True;
    FPickList.FGrid := Grid;
  end;
  Result := FPickList;
end;

procedure TCustomGridEdit.KeyDown(var Key: Word; Shift: TShiftState);

  procedure SendToParent;
  begin
    if Grid <> nil then
    begin
      Grid.KeyDown(Key, Shift);
      Key := 0;
    end;
  end;

  procedure ParentEvent;
  var
    GridKeyDown: TKeyEvent;
  begin
    if Grid <> nil then
    begin
      GridKeyDown := Grid.OnKeyDown;
      if Assigned(GridKeyDown) then GridKeyDown(Grid, Key, Shift);
    end;
  end;

begin
  case Key of
    VK_UP,
    VK_DOWN:
      if (Shift = [ssCtrl]) or ((Shift = []) and (not (WantReturns or WordWrap))) then SendToParent;
    VK_PRIOR,
    VK_NEXT:
      SendToParent;
    VK_ESCAPE:
      SendToParent;
    VK_DELETE:
      if not EditCanModify then SendToParent;
    VK_INSERT:
      if (not EditCanModify) or (Shift = []) then SendToParent;
//  VK_LEFT,
//  VK_RIGHT,
    VK_HOME,
    VK_END:
      if Shift = [ssCtrl] then SendToParent;
    VK_TAB:
      if not (ssAlt in Shift) then SendToParent;
  end;
  if Key <> 0 then
  begin
    ParentEvent;
    inherited KeyDown(Key, Shift);
  end;
end;

procedure TCustomGridEdit.KeyPress(var Key: Char);
begin
  if Grid <> nil then
  begin
    Grid.KeyPress(Key);
    { char can be dropped by grid OnEditAcceptKey event  }
    if CharInSet(Key, [#32..#255]) and (not Grid.EditCanAcceptKey(Grid.EditCell, Key)) then
    begin
      Key := #0;
      MessageBeep(0);
    end;
    case Key of
      #9, #27:
        Key := #0;
      ^H, ^V, ^X, #32..#255:
        { drop BACKSPACE and chars when cell is readonly }
        if not EditCanModify then
        begin
          Key := #0;
          MessageBeep(0);
        end;
    end;
  end;
  if Key <> #0 then inherited KeyPress(Key);
end;

procedure TCustomGridEdit.KeyUp(var Key: Word; Shift: TShiftState);
begin
  if Grid <> nil then Grid.KeyUp(Key, Shift);
end;

procedure TCustomGridEdit.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if (Button = mbLeft) and (EditStyle <> geSimple) and PtInrect(ButtonRect, Point(X, Y)) then
  begin
    if FDropListVisible then
      CloseUp(False)
    else
    begin
      StartButtonTracking(X, Y);
      if EditStyle <> geEllipsis then DropDown;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGridEdit.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  P: TPoint;
  M: TSmallPoint;
begin
  if FButtonTracking then
  begin
    StepButtonTracking(X, Y);
    { stop button tracking when moving over drop-down list }
    if FDropListVisible then
    begin
      P := FActiveList.ScreenToClient(ClientToScreen(Point(X, Y)));
      if PtInRect(FActiveList.ClientRect, P) then
      begin
        StopButtonTracking;
        { emulate a click on the drop-down list to close it when you release
          the mouse button }
        M := PointToSmallPoint(P);
        SendMessage(FActiveList.Handle, WM_LBUTTONDOWN, 0, Integer(M));
        Exit;
      end;
    end;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGridEdit.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  P: Boolean;
begin
  P := FButtonPressed;
  StopButtonTracking;
  if (Button = mbLeft) and (EditStyle = geEllipsis) and P then Press;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGridEdit.PaintButton(DC: HDC; Rect: TRect);
var
  Detail1: TThemedButton;
  Detail2: TThemedComboBox;
  X, Y, Flags, DX: Integer;
begin
  case EditStyle of
    geEllipsis:
      if StyleServices.Enabled then
      begin
        if FButtonPressed then Detail1 := tbPushButtonPressed
        //else if Hot then Detail1 := tbPushButtonHot
        else Detail1 := tbPushButtonNormal;
        with StyleServices do
          DrawElement(DC, GetElementDetails(Detail1), Rect);
        //X := (Rect.Right - Rect.Left) div 2 - 1 + Ord(Pressed);
        //Y := (Rect.Bottom - Rect.Top) div 2 - 1 + Ord(Pressed);
        X := (Rect.Right - Rect.Left) div 2 + Ord(FButtonPressed and (Win32MajorVersion > 6));
        Y := Rect.Bottom - 7 + Ord(FButtonPressed and (Win32MajorVersion > 6));
        PatBlt(DC, Rect.Left + X, Rect.Top + Y, 1, 2, BLACKNESS);
        PatBlt(DC, Rect.Left + X - 3, Rect.Top + Y, 1, 2, BLACKNESS);
        PatBlt(DC, Rect.Left + X + 3, Rect.Top + Y, 1, 2, BLACKNESS);
      end
      else
      begin
        Flags := 0;
        if FButtonPressed then Flags := BF_FLAT;
        DrawEdge(DC, Rect, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
        Flags := (Rect.Right - Rect.Left) div 2 - 1 + Ord(FButtonPressed);
        PatBlt(DC, Rect.Left + Flags, Rect.Top + Flags, 2, 2, BLACKNESS);
        PatBlt(DC, Rect.Left + Flags - 3, Rect.Top + Flags, 2, 2, BLACKNESS);
        PatBlt(DC, Rect.Left + Flags + 3, Rect.Top + Flags, 2, 2, BLACKNESS);
      end;
    gePickList, geDataList:
      if StyleServices.Enabled then
      begin
        if FButtonPressed then Detail2 := tcDropDownButtonPressed
        //else if Hot then Detail2 := tcDropDownButtonHot
        else Detail2 := tcDropDownButtonNormal;
        with StyleServices do
          DrawElement(DC, GetElementDetails(Detail2), Rect);
      end
      else
      begin
        Flags := 0;
        if FButtonPressed then Flags := DFCS_FLAT;
        DrawEdge(DC, Rect, EDGE_RAISED, BF_RECT or BF_MIDDLE or Flags);
        Flags := (Rect.Right - Rect.Left) div 2 - 1 + Ord(FButtonPressed);
        DX := (Rect.Right - Rect.Left) mod 2 - 1;
        PatBlt(DC, Rect.Left + Flags - 2 + DX, Rect.Top + Flags - 1, 7, 1, BLACKNESS);
        PatBlt(DC, Rect.Left + Flags - 1 + DX, Rect.Top + Flags + 0, 5, 1, BLACKNESS);
        PatBlt(DC, Rect.Left + Flags - 0 + DX, Rect.Top + Flags + 1, 3, 1, BLACKNESS);
        PatBlt(DC, Rect.Left + Flags + 1 + DX, Rect.Top + Flags + 2, 1, 1, BLACKNESS);
      end;
  end;
end;

procedure TCustomGridEdit.PaintWindow(DC: HDC);
var
  R: TRect;
begin
  if (EditStyle <> geSimple) then
  begin
    R := GetButtonRect;
    PaintButton(DC, R);
    ExcludeClipRect(DC, R.Left, R.Top, R.Right, R.Bottom);
  end;
  inherited;
end;

procedure TCustomGridEdit.StartButtonTracking(X, Y: Integer);
begin
  MouseCapture := True;
  FButtonTracking := True;
  StepButtonTracking(X, Y);
end;

procedure TCustomGridEdit.StepButtonTracking(X, Y: Integer);
var
  R: TRect;
  P: Boolean;
begin
  R := GetButtonRect;
  P := PtInRect(R, Point(X, Y));
  if FButtonPressed <> P then
  begin
    FButtonPressed := P;
    InvalidateRect(Handle, @R, False);
  end;
end;

procedure TCustomGridEdit.StopButtonTracking;
begin
  if FButtonTracking then
  begin
    StepButtonTracking(-1, -1);
    FButtonTracking := False;
    MouseCapture := False;
  end;
end;

type
  THackWinControl = class(TWinControl);

procedure TCustomGridEdit.UpdateBounds(Showing, ScrollCaret: Boolean);
const
  Flags: array[Boolean] of Integer = (0, SWP_SHOWWINDOW or SWP_NOREDRAW);
var
  R, F: TRect;
  L, T, W, H: Integer;
  TI: TPoint;
begin
  if Grid <> nil then
  begin
    { set the inplace editor bounds }
    R := Grid.GetEditRect(Grid.EditCell);
    F := R;
    with Grid.GetFixedRect do
    begin
      if R.Left < Right then R.Left := Right;
      if R.Right < Right then R.Right := Right;
    end;
    with Grid.GetHeaderRect do
    begin
      if R.Top < Bottom then R.Top := Bottom;
      if R.Bottom < Bottom then R.Bottom := Bottom;
    end;
    W := R.Right - R.Left;
    H := R.Bottom - R.Top;
    SetWindowPos(Handle, HWND_TOP, R.Left, R.Top, W, H, Flags[Showing]);
    { set the text formatting rectangle (the text in inplace editor should
      be placed exactly above the cell text) }
    L := F.Left - R.Left;
    T := F.Top - R.Top;
    W := F.Right - F.Left;
    H := F.Bottom - F.Top;
    TI := Grid.GetCellTextIndent(Grid.EditCell);
    if EditStyle <> geSimple then Dec(W, ButtonWidth + 4) else Dec(W, Grid.TextRightIndent);
    R := Bounds(L + TI.X, T + TI.Y, W - TI.X + Ord(Alignment = taRightJustify), H);
    SendMessage(Handle, EM_SETRECTNP, 0, LongInt(@R));
    { set cursor to the end of text }
    if ScrollCaret then SendMessage(Handle, EM_SCROLLCARET, 0, 0);
    { when scrolling the grid horizontally, inplace editor should be redrawn
      immediately, otherwise a black rectangle (garbage) will flash
      in its place if grid Doublebuffered is True }
    if Grid.DoubleBuffered then UpdateWindow(Handle);
  end
end;

procedure TCustomGridEdit.UpdateColors;
var
  Canvas: TCanvas;
begin
  if Grid <> nil then
  begin
    Canvas := TCanvas.Create;
    try
      Grid.GetCellColors(Grid.EditCell, Canvas);
      Color := Canvas.Brush.Color;
      Font := Canvas.Font;
    finally
      Canvas.Free;
    end;
  end;
end;

procedure TCustomGridEdit.UpdateContents;
begin
  if (Grid = nil) or (not Grid.IsCellValid(Grid.EditCell)) then Exit;
  with Grid do
  begin
    Self.MaxLength := Columns[EditCell.Col].MaxLength;
    Self.ReadOnly := IsCellReadOnly(EditCell) or (Self.MaxLength = -1);
    Self.WantReturns := Columns[EditCell.Col].WantReturns;
    case Columns[EditCell.Col].EditWordWrap of
      ewAuto: Self.WordWrap := Columns[EditCell.Col].WordWrap;
      ewEnabled: Self.WordWrap := True;
    else
      Self.WordWrap := False;
    end;
    Self.Alignment := Columns[EditCell.Col].EditAlignment;
    Self.EditMask := GetEditMask(EditCell);
    Self.Text := GetEditText(EditCell);
  end;
end;

procedure TCustomGridEdit.UpdateList;
begin
  if FActiveList <> nil then
  begin
    FActiveList.Visible := False;
    FActiveList.Parent := Self;
    THackWinControl(FActiveList).OnMouseUp := ListMouseUp;
    THackWinControl(FActiveList).Font := Font;
  end;
end;

procedure TCustomGridEdit.UpdateListBounds;
var
  I, X, W: Integer;
  R, Rect: TRect;
  P: TPoint;
  Monitor: TMonitor;
begin
  if (Grid = nil) or (FActiveList = nil) then Exit;
  { get an active monitor, it will be used below to determine the height of
    the screen instead of the Screen }
  R := Self.ClientRect;
  P := Self.ClientOrigin;
  OffsetRect(R, P.X, P.Y);
  Monitor := Screen.MonitorFromRect(R);
  if Monitor <> nil then Rect := Monitor.WorkareaRect
  else Rect := Screen.WorkareaRect;
  { define bounds only for TGridListBox and descendants }
  if FActiveList is TGridListBox then
    with TGridListBox(FActiveList) do
    begin
      Canvas.Font := Font;
      if Items.Count > 0 then
      begin
        W := 0;
        for I := 0 to Items.Count - 1 do
        begin
          X := Canvas.TextWidth(Items[I]);
          if W < X then W := X;
        end;
        ClientWidth := W + 6;
      end
      else
        ClientWidth := 100;
      { if the maximum number of items displayed in the drop-down list is not
        specified, then make it equal to 1/3 of the screen height }
      if Items.Count = 0 then
        ClientHeight := ItemHeight
      else
      begin
        I := FDropDownCount;
        if I = 0 then
        begin
          I := ((Rect.Bottom - Rect.Top) div 3) div ItemHeight;
          if I < 1 then I := 1;
        end;
        if I > Items.Count then I := Items.Count;
        ClientHeight := I * ItemHeight;
      end;
    end;
  with FActiveList do
  begin
    R := Grid.GetCellRect(Grid.EditCell);
    Width := Max(Width, R.Right - R.Left);
    Left := P.X + Self.Width - Width;
    Top := P.Y + Self.Height;
    if Top + Height > Rect.Bottom - Rect.Top then Top := P.Y - Height;
    { list bounds can be redefined in OnGetEditListBounds event }
    R := BoundsRect;
    Grid.GetEditListBounds(Grid.EditCell, R);
    BoundsRect := R;
  end;
end;

procedure TCustomGridEdit.UpdateListItems;
begin
  if (Grid = nil) or (FActiveList = nil) or (not (FActiveList is TGridListBox)) then Exit;
  with TGridListBox(FActiveList) do
  begin
    Items.Clear;
    Grid.GetEditList(Grid.EditCell, Items);
    ItemIndex := Grid.GetEditListIndex(Grid.EditCell, Items, Self.Text);
  end;
end;

procedure TCustomGridEdit.UpdateListValue(Accept: Boolean);
var
  I: Integer;
  Items: TStrings;
  ItemText: string;
begin
  if (FActiveList <> nil) and (FActiveList is TGridListBox) then
  begin
    { selected text can be rejected, accepted or modified using OnEditCloseUp
      grid event }
    I := TGridListBox(FActiveList).ItemIndex;
    Items := TGridListBox(FActiveList).Items;
    if I <> -1 then ItemText := Items[I] else ItemText := '';
    if Grid <> nil then Grid.EditCloseUp(Grid.EditCell, Items, I, ItemText, Accept);
    if Accept and (I <> -1) then
    begin
      Text := ItemText;
      SendMessage(Handle, EM_SETSEL, 0, -1);
    end;
  end;
end;

procedure TCustomGridEdit.UpdateStyle;
var
  Style: TGridEditStyle;
begin
  Style := geSimple;
  if (Grid <> nil) and (not Grid.ReadOnly) then
    Style := Grid.GetEditStyle(Grid.EditCell);
  EditStyle := Style;
end;

{
  Delete the requested message from the queue, but throw back
  any WM_QUIT msgs that PeekMessage may also return.
}
procedure KillMessage(Wnd: HWND; Msg: Integer);
var
  M: TMsg;
begin
  M.Message := 0;
  if PeekMessage(M, Wnd, Msg, Msg, PM_REMOVE) and (M.Message = WM_QUIT) then PostQuitMessage(M.wParam);
end;

procedure TCustomGridEdit.WndProc(var Message: TMessage);

  procedure DoDropDownKeys(var Key: Word; Shift: TShiftState);
  begin
    case Key of
      VK_UP, VK_DOWN:
        if ssAlt in Shift then
        begin
          if FDropListVisible then CloseUp(True) else DropDown;
          Key := 0;
        end;
      VK_RETURN, VK_ESCAPE:
        if (not (ssAlt in Shift)) and FDropListVisible then
        begin
          KillMessage(Handle, WM_CHAR);
          CloseUp(Key = VK_RETURN);
          Key := 0;
        end;
    end;
  end;

  procedure DoButtonKeys(var Key: Word; Shift: TShiftState);
  begin
    if (Key = VK_RETURN) and (Shift = [ssCtrl]) then
    begin
      KillMessage(Handle, WM_CHAR);
      Key := 0;
      { Ctrl+Enter works like a button click }
      case EditStyle of
        geEllipsis: Press;
        gePickList, geDataList: if not FDropListVisible then SelectNext;
      end;
    end;
  end;

var
  Form: TCustomForm;
begin
  case Message.Msg of
    WM_KEYDOWN,
    WM_SYSKEYDOWN,
    WM_CHAR:
        with TWMKey(Message) do
        begin
          { opening the drop-down list }
          if EditStyle in [gePickList, geDataList] then
          begin
            DoDropDownKeys(CharCode, KeyDataToShiftState(KeyData));
            if (CharCode <> 0) and FDropListVisible then
            begin
              with TMessage(Message) do SendMessage(FActiveList.Handle, Msg, WParam, LParam);
              Exit;
            end;
          end;
          { button click }
          if not WantReturns then
          begin
            DoButtonKeys(CharCode, KeyDataToShiftState(KeyData));
            if CharCode = 0 then Exit;
          end;
        end;
    WM_SETFOCUS:
      begin
        Form := GetParentForm(Self);
        if (Form = nil) or Form.SetFocusedControl(Grid) then Dispatch(Message);
        Exit;
      end;
    WM_LBUTTONDOWN:
      with TWMLButtonDown(Message) do
      begin
        { mouse double click }
        if (EditStyle = geSimple) or (not PtInrect(ButtonRect, Point(XPos, YPos))) then
          if UINT(GetMessageTime - FClickTime) < GetDoubleClickTime then
            Message.Msg := WM_LBUTTONDBLCLK;
        FClickTime := 0;
      end;
  end;
  inherited WndProc(Message);
end;

procedure TCustomGridEdit.CloseUp(Accept: Boolean);
const
  Flags = SWP_NOZORDER or SWP_NOMOVE or SWP_NOSIZE or SWP_NOACTIVATE or SWP_HIDEWINDOW;
begin
  if FDropListVisible then
  begin
    if GetCapture <> 0 then
      SendMessage(GetCapture, WM_CANCELMODE, 0, 0);
    SetWindowPos(FActiveList.Handle, 0, 0, 0, 0, 0, Flags);
    FDropListVisible := False;
    Invalidate;
    { set ClosingUp state for WMKillFocus }
    Inc(FCloseUpCount);
    try
      UpdateListValue(Accept);
      { return focus to the inplace editor in case the OnEditCloseUp event
        handler displayed a dialog, for example, a color dialog }
      SetFocus;
    finally
      Dec(FCloseUpCount);
    end;
  end;
end;

procedure TCustomGridEdit.Deselect;
begin
  SendMessage(Handle, EM_SETSEL, $7FFFFFFF, Longint($FFFFFFFF));
end;

procedure TCustomGridEdit.DropDown;
const
  Flags = SWP_NOSIZE or SWP_NOACTIVATE or SWP_SHOWWINDOW;
begin
  if (not FDropListVisible) and (Grid <> nil) and (EditStyle in [gePickList, geDataList]) then
  begin
    FActiveList := GetDropList;
    if FActiveList <> nil then
    begin
      UpdateList;
      UpdateListItems;
      { protection against exotic errors: if, when retrieving a list the user
        decides to cancel editing, then the list should not be shown }
      if not Grid.Editing then
      begin
        StopButtonTracking;
        Exit;
      end;
      UpdateListBounds;
      SetWindowPos(FActiveList.Handle, HWND_TOP, FActiveList.Left, FActiveList.Top, 0, 0, Flags);
      FDropListVisible := True;
      Invalidate;
      Windows.SetFocus(Handle);
    end;
  end;
end;

procedure TCustomGridEdit.Invalidate;
var
  Cur: TRect;
begin
  if Grid = nil then
  begin
    inherited Invalidate;
    Exit;
  end;
  ValidateRect(Handle, nil);
  InvalidateRect(Handle, nil, True);
  { grid under the inplace editor must be invalidated too }
  Windows.GetClientRect(Handle, Cur);
  MapWindowPoints(Handle, Grid.Handle, Cur, 2);
  ValidateRect(Grid.Handle, @Cur);
  InvalidateRect(Grid.Handle, @Cur, False);
end;

procedure TCustomGridEdit.InvalidateButton;
var
  R: TRect;
begin
  R := GetButtonRect;
  InvalidateRect(Handle, @R, False);
end;

procedure TCustomGridEdit.Hide;
const
  Flags = SWP_HIDEWINDOW or SWP_NOZORDER or SWP_NOREDRAW;
begin
  if (Grid <> nil) and HandleAllocated and Visible then
  begin
    Grid.FEditing := False;
    Invalidate;
    SetWindowPos(Handle, 0, 0, 0, 0, 0, Flags);
    { the grid should not lose focus }
    if Focused then
    begin
      FDefocusing := True;
      try
        Windows.SetFocus(Grid.Handle);
      finally
        FDefocusing := False;
      end;
    end;
  end;
end;

procedure TCustomGridEdit.Press;
begin
  // if EditCanModify then <- a read-only editor can have an ellipsis button
  begin
    Inc(FPressCount);
    try
      Grid.EditButtonPress(Grid.EditCell);
      { after closing the dialog box (if any), the focus should be restored
        to the inplace editor }
      SetFocus;
    finally
      Dec(FPressCount);
    end;
  end;
end;

procedure TCustomGridEdit.SelectNext;
var
  OldText, NewText: string;
begin
  if Grid <> nil then
  begin
    OldText := Text;
    NewText := OldText;
    Grid.EditSelectNext(Grid.EditCell, NewText);
    if NewText <> OldText then
    begin
      Text := NewText;
      SendMessage(Handle, EM_SETSEL, 0, -1);
    end;
  end;
end;

procedure TCustomGridEdit.SetFocus;
begin
  if IsWindowVisible(Handle) then Windows.SetFocus(Handle);
end;

procedure TCustomGridEdit.Show;
var
  CursorPos: TPoint;
  ScrollCaret: Boolean;
begin
  if Grid <> nil then
  begin
    ScrollCaret := not Grid.FEditing;
    Grid.FEditing := True;
    Grid.FCellSelected := True;
    { colors must be set before the bounds are set, as the bounds are set
      depending on the font size }
    UpdateColors;
    UpdateBounds(True, ScrollCaret);
    { hot flag to draw a button when themes are enabled }
    if Windows.GetCursorPos(CursorPos) then
    begin
      CursorPos := ScreenToClient(CursorPos);
      FButtonHot := PtInRect(Bounds(0, 0, Width, Height), CursorPos);
    end
    else
      FButtonHot := False;
    if Grid.Focused then Windows.SetFocus(Handle);
  end;
end;

{ TGridTipsWindow }

constructor TGridTipsWindow.Create(AOwner: TComponent);
begin
  inherited;
  { Delphi hint color is different from Windows hit color }
  Color := clInfoBk;
end;

procedure TGridTipsWindow.ActivateHint(Rect: TRect; const AHint: string);
type
  TAnimationStyle = (atSlideNeg, atSlidePos, atBlend);
const
  Flags: array[TAnimationStyle] of Integer = (AW_VER_NEGATIVE, AW_VER_POSITIVE, AW_BLEND);
var
  Monitor: TMonitor;
  R: TRect;
  X, Y: Integer;
  Animate: BOOL;
  Style: TAnimationStyle;
  P: TPoint;
begin
  Caption := AHint;
  UpdateBoundsRect(Rect);
  { do not show a hint behind the screen }
  Monitor := Screen.MonitorFromPoint(Rect.TopLeft);
  if Monitor <> nil then
  begin
    R := Monitor.BoundsRect;
    X := Rect.Left;
    if Rect.Right > R.Right then X := X - (Rect.Right - R.Right);
    if X < R.Left then X := Monitor.Left;
    Y := Rect.Top;
    if Rect.Bottom > R.Bottom then Y := Y - (Rect.Bottom - R.Bottom);
    if Y < R.Top then Y := R.Top;
    OffsetRect(Rect, X - Rect.Left, Y - Rect.Top);
  end;
  { showing hint with or without animation }
  SystemParametersInfo(SPI_GETTOOLTIPANIMATION, 0, @Animate, 0);
  if Animate and Assigned(AnimateWindowProc) then
  begin
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height, SWP_NOACTIVATE);
    SystemParametersInfo(SPI_GETTOOLTIPFADE, 0, @Animate, 0);
    if Animate then
      Style := atBlend
    else
    begin
      Windows.GetCursorPos(P);
      if P.Y > Rect.Top then
        Style := atSlideNeg
      else
        Style := atSlidePos;
    end;
    AnimateWindowProc(Handle, 100, Flags[Style] or AW_SLIDE);
    ShowWindow(Handle, SW_SHOWNOACTIVATE);
  end
  else
    SetWindowPos(Handle, HWND_TOPMOST, Rect.Left, Rect.Top, Width, Height,
      SWP_SHOWWINDOW or SWP_NOACTIVATE);
  Invalidate;
end;

procedure TGridTipsWindow.ActivateHintData(Rect: TRect; const AHint: string; AData: Pointer);
begin
  FGrid := AData;
  inherited ActivateHintData(Rect, AHint, AData);
end;

function TGridTipsWindow.CalcHintRect(MaxWidth: Integer; const AHint: string; AData: Pointer): TRect;
var
  R: TRect;
begin
  FGrid := AData;
  if FGrid = nil then
  begin
    Result := inherited CalcHintRect(MaxWidth, AHint, AData);
    InflateRect(Result, 2, 2); // <- see CreateParams()
    Exit;
  end;
  { the hint rectangle matches the cell rect with border }
  R := FGrid.GetTipsRect(FGrid.FTipsCell, AHint);
  OffsetRect(R, -R.Left, -R.Top);
  Result := R;
end;

procedure TGridTipsWindow.CMTextChanged(var Message: TMessage);
begin
  { ignore message to prevent flickering hint window }
end;

procedure TGridTipsWindow.CreateParams(var Params: TCreateParams);
begin
  inherited;
  { the smooth animation function always draws a rectangular frame, but
    in Windows Vista with themes enabled it should have rounded edges,
    so we will draw the window frame manually }
  with Params do
  begin
    Style := WS_POPUP and not WS_BORDER;
    ExStyle := ExStyle and not WS_EX_CLIENTEDGE;
  end;
end;

procedure TGridTipsWindow.NCPaint(DC: HDC);
begin
  { frame is drawing manually in WMEraseBkgnd }
end;

procedure TGridTipsWindow.Paint;
var
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
  R: TRect;
begin
  if FGrid = nil then
  begin
    inherited;
    Exit;
  end;
  FGrid.GetCellColors(FGrid.FTipsCell, Canvas);
  { theme text color is not supported }
  Canvas.Font.Color := Screen.HintFont.Color;
  { use grid PaintText() function to draw hint text like cell }
  with FGrid do
  begin
    TI := GetCellTextIndent(FTipsCell);
    A := Columns[FTipsCell.Col].Alignment;
    WR := Pos(#13, FTipsText) <> 0; // Columns[FTipsCell.Col].WantReturns;
    WW := Columns[FTipsCell.Col].WordWrap;
    T := FTipsText;
  end;
  R := ClientRect;
  InflateRect(R, -1, -1);
  FGrid.PaintText(Canvas, R, TI.X, TI.Y, A, WR, WW, T);
end;

procedure TGridTipsWindow.WMEraseBkgnd(var Message: TWMEraseBkgnd);
var
  R: TRect;
  Details: TThemedElementDetails;
  C: COLORREF;
begin
  inherited;
  { when themes are enabled in Windows Vista and above, draw the background
    with the frame using the current theme, with the themes disabled, draw
    a solid frame like the explorer (the background is already filled by
    inherited call) }
  if StyleServices.Enabled and CheckWin32Version(6, 0) then
  begin
    Details := StyleServices.GetElementDetails(tttStandardNormal);
    StyleServices.DrawElement(Message.DC, Details, ClientRect);
    { patch for Window 7 and higher: the standard theme draw the bottom
      right pixel with a light color, so a bright dot is visible on the
      shadow background }
    if (Win32MajorVersion = 6) and (Win32MinorVersion < 2) then
    begin
      C := GetPixel(Message.DC, 1, 0);
      SetPixel(Message.DC, Width - 1, Height - 1, C);
    end;
  end
  else
  begin
    R := ClientRect;
    Windows.DrawEdge(Message.DC, R, BDR_RAISEDOUTER, BF_RECT or BF_MONO);
  end;
end;

{ TGridFindDialog }

procedure TGridFindDialog.DoClose;
begin
  inherited;
  FVisible := False;
end;

procedure TGridFindDialog.DoShow;
begin
  inherited;
  FVisible := True;
end;

procedure TGridFindDialog.ShowModal(FindAsItemNo: Boolean);
var
  ActiveWindow: HWnd;
  WindowList: Pointer;
begin
  ActiveWindow := GetActiveWindow;
  WindowList := DisableTaskWindows(0);
  try
    if Execute then
      repeat
        Application.HandleMessage;
        if Application.Terminated then FVisible := False;
      until not FVisible;
  finally
    EnableTaskWindows(WindowList);
    SetActiveWindow(ActiveWindow);
  end;
end;

{ TCustomGridView }

constructor TCustomGridView.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  ControlStyle := ControlStyle + [csOpaque, csNeedsBorderPaint];
  Width := 185;
  Height := 105;
  Color := clWindow;
  ParentColor := False;
  TabStop := True;
  FHorzScrollBar := CreateScrollBar(sbHorizontal);
  FHorzScrollBar.OnScroll := HorzScroll;
  FHorzScrollBar.OnChange := HorzScrollChange;
  FVertScrollBar := CreateScrollBar(sbVertical);
  FVertScrollBar.LineSize := 17;
  FVertScrollBar.OnScroll := VertScroll;
  FVertScrollBar.OnChange := VertScrollChange;
  FHeader := CreateHeader;
  FHeader.OnChange := HeaderChange;
  FColumns := CreateColumns;
  FColumns.OnChange := ColumnsChange;
  FRows := CreateRows;
  FRows.OnChange := RowsChange;
  FFixed := CreateFixed;
  FFixed.OnChange := FixedChange;
  FImagesLink := TChangeLink.Create;
  FImagesLink.OnChange := ImagesChange;
  FBorderStyle := bsSingle;
  FShowHeader := True;
  FGridLines := True;
  FGridLineWidth := 1; { <- do not modify !!! }
  FGridStyle := [gsHorzLine, gsVertLine];
  FGridColor := clWindow;
  FEndEllipsis := True;
  FImageLeftIndent := 2;
  FImageTopIndent := 1;
  FImageHighlight := True;
  FTextLeftIndent := 5;
  FTextRightIndent := 5;
  FTextTopIndent := 2;
  FShowFocusRect := True;
  FRightClickSelect := True;
  FAllowSelect := True;
  FCursorKeys := [gkArrows, gkMouse, gkMouseWheel];
  FColumnsResize := True;
  FColumnClick := True;
  FEditCell := GridCell(-1, -1);
  FCheckStyle := csWin95;
  FCheckWidth := 16;
  FCheckHeight := 16;
  FCheckLeftIndent := 0;
  FCheckTopIndent := 0;
  FCheckBuffer := TBitmap.Create;
  FSortLeftIndent := 4;
  FSortTopIndent := 0;
  FSortBuffer := TBitmap.Create;
  FPatternBitmap := TBitmap.Create;
  FPatternBitmap.Width := 2;
  FPatternBitmap.Height := 2;
  FCancelOnExit := True;
  FGridHintColor := clGrayText;
end;

destructor TCustomGridView.Destroy;
begin
  FreeandNil(FPatternBitmap);
  FreeandNil(FSortBuffer);
  FreeandNil(FCheckBuffer);
  FreeandNil(FImagesLink);
  inherited Destroy;
  FreeandNil(FFixed);
  FreeandNil(FRows);
  FreeandNil(FColumns);
  FreeandNil(FHeader);
  FreeandNil(FHorzScrollBar);
  FreeandNil(FVertScrollBar);
end;

function TCustomGridView.GetCell(Col, Row: Longint): string;
begin
  Result := GetCellText(GridCell(Col, Row));
end;

function TCustomGridView.GetChecked(Col, Row: Longint): Boolean;
begin
  Result := GetCheckState(GridCell(Col, Row)) in [cbChecked, cbGrayed];
end;

function TCustomGridView.GetCheckBoxEnabled(Col, Row: Longint): Boolean;
begin
  GetCheckStateEx(GridCell(Col, Row), Result);
end;

function TCustomGridView.GetCheckBoxState(Col, Row: Longint): TCheckBoxState;
begin
  Result := GetCheckState(GridCell(Col, Row));
end;

function TCustomGridView.GetCol: Longint;
begin
  Result := CellFocused.Col;
end;

function TCustomGridView.GetFixed: TGridFixed;
begin
  Result := TGridFixed(FFixed);
end;

function TCustomGridView.GetEdit: TGridEdit;
begin
  Result := TGridEdit(FEdit);
end;

function TCustomGridView.GetEditColumn: TGridColumn;
begin
  Result := nil;
  if (EditCell.Col >= 0) and (EditCell.Col < Columns.Count) then Result := Columns[EditCell.Col];
end;

function TCustomGridView.GetEditDropDown: Boolean;
begin
  Result := (Edit <> nil) and Edit.DropListVisible;
end;

function TCustomGridView.GetEditFocused: Boolean;
begin
  Result := Editing and FEdit.Focused;
end;

function TCustomGridView.GetEditing: Boolean;
begin
  Result := FEditing and (FEdit <> nil);
end;

function TCustomGridView.GetHeader: TGridHeader;
begin
  Result := TGridHeader(FHeader);
end;

function TCustomGridView.GetLeftCol: Longint;
begin
  Result := VisOrigin.Col;
end;

function TCustomGridView.GetLightenColor(Color: TColor; Amount: Integer): TColor;
var
  R, G, B: Integer;
begin
  if Color < 0 then Color := GetSysColor(Color and $000000FF);
  R := Color and $FF + Amount;
  G := Color shr 8 and $FF + Amount;
  B := Color shr 16 and $FF + Amount;
  if R < 0 then R := 0 else if R > 255 then R := 255;
  if G < 0 then G := 0 else if G > 255 then G := 255;
  if B < 0 then B := 0 else if B > 255 then B := 255;
  Result := R or (G shl 8) or (B shl 16);
  { return system color from system palette (actual for a 8-bit palette) }
  Result := GetNearestColor(Canvas.Handle, Result);
end;

function TCustomGridView.GetRow: Longint;
begin
  Result := CellFocused.Row;
end;

function TCustomGridView.GetRows: TGridRows;
begin
  Result := TGridRows(FRows);
end;

function TCustomGridView.GetTopRow: Longint;
begin
  Result := VisOrigin.Row;
end;

function TCustomGridView.GetVisibleColCount: Longint;
begin
  Result := VisSize.Col;
end;

function TCustomGridView.GetVisibleRowCount: Longint;
begin
  Result := VisSize.Row;
end;

procedure TCustomGridView.ColumnsChange(Sender: TObject);
begin
  if [csReading, csLoading] * ComponentState = [] then
  begin
    UpdateFixed;
    UpdateHeader;
  end;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeColumns;
end;

function TCustomGridView.CompareStrings(const S1, S2: string; WholeWord,
  MatchCase: Boolean): Boolean;
begin
  if WholeWord then
    if MatchCase then
      Result := AnsiSameStr(S1, S2)
    else
      Result := AnsiSameText(S1, S2)
  else
    if MatchCase then
      Result := AnsiContainsStr(S2, S1)
    else
      Result := AnsiContainsText(S2, S1);
end;

procedure TCustomGridView.FixedChange(Sender: TObject);
begin
  UpdateRows;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeFixed; 
end;

procedure TCustomGridView.HandlerFind(Sender: TObject);
begin
  FindText(FindDialog.FindText, FindDialog.Options);
end;

procedure TCustomGridView.HandlerFindMenu(Sender: TObject);
begin
  FindDialog.ShowModal(False);
end;

procedure TCustomGridView.HandlerFindNext(Sender: TObject);
begin
  with FindDialog do
    if Length(FindText) = 0 then ShowModal(False)
    else Self.FindText(FindText, Options + [frDown]);
end;

procedure TCustomGridView.HandlerFindPrev(Sender: TObject);
begin
  with FindDialog do
    if Length(FindText) = 0 then ShowModal(False)
    else Self.FindText(FindText, Options - [frDown]);
end;

procedure TCustomGridView.HeaderChange(Sender: TObject);
begin
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateEdit(Editing);
  UpdateCursor;
  Invalidate;
end;

procedure TCustomGridView.HorzScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
begin
  CancelCellTips;
  if FocusOnScroll then UpdateFocus;
end;

procedure TCustomGridView.HorzScrollChange(Sender: TObject);
begin
  UpdateVisOriginSize;
  UpdateEdit(Editing);
end;

procedure TCustomGridView.ImagesChange(Sender: TObject);
begin
  InvalidateGrid;
  UpdateRows;
end;

procedure TCustomGridView.RowsChange(Sender: TObject);
begin
  CancelCellTips;
  UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateCursor;
  UpdateEdit(Editing);
  Invalidate;
  ChangeRows;
end;

procedure TCustomGridView.SetAllowEdit(Value: Boolean);
begin
  if FAllowEdit <> Value then
  begin
    FAllowEdit := Value;
    if not Value then
    begin
      AlwaysEdit := False;
      HideEdit;
    end
    else
      RowSelect := False;
    ChangeEditMode;
  end;
end;

procedure TCustomGridView.SetAllowSelect(Value: Boolean);
begin
  if FAllowSelect <> Value then
  begin
    FAllowSelect := Value;
    RowSelect := FRowSelect or (not Value);
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetAlwaysEdit(Value: Boolean);
begin
  if FAlwaysEdit <> Value then
  begin
    FAlwaysEdit := Value;
    if Value then
    begin
      AllowEdit := True;
      Editing := True;
    end
    else
      HideEdit;
  end;
end;

procedure TCustomGridView.SetAlwaysSelected(Value: Boolean);
begin
  if FAlwaysSelected <> Value then
  begin
    FAlwaysSelected := Value;
    FCellSelected := FCellSelected or Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetBorderStyle(Value: TBorderStyle);
begin
  if FBorderStyle <> Value then
  begin
    FBorderStyle := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridView.SetCell(Col, Row: Longint; Value: string);
begin
  SetEditText(GridCell(Col, Row), Value);
end;

procedure TCustomGridView.SetCellFocused(Value: TGridCell);
begin
  SetCursor(Value, CellSelected, True);
end;

procedure TCustomGridView.SetCellSelected(Value: Boolean);
begin
  SetCursor(CellFocused, Value, True);
end;

procedure TCustomGridView.SetCheckBoxes(Value: Boolean);
begin
  if FCheckBoxes <> Value then
  begin
    FCheckBoxes := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckLeftIndent(Value: Integer);
begin
  if FCheckLeftIndent <> Value then
  begin
    FCheckLeftIndent := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckStyle(Value: TGridCheckStyle);
begin
  if FCheckStyle <> Value then
  begin
    FCheckStyle := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCheckTopIndent(Value: Integer);
begin
  if FCheckTopIndent <> Value then
  begin
    FCheckTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetCol(Value: Longint);
begin
  CellFocused := GridCell(Value, CellFocused.Row);
end;

procedure TCustomGridView.SetColumns(Value: TGridColumns);
begin
  FColumns.Assign(Value);
end;

procedure TCustomGridView.SetCursorKeys(Value: TGridCursorKeys);
begin
  if gkMouseMove in Value then Include(Value, gkMouse);
  if not (gkMouse in Value) then Exclude(Value, gkMouseMove);
  FCursorKeys := Value;
end;

procedure TCustomGridView.SetEditDropDown(Value: Boolean);
begin
  Editing := True;
  if Edit <> nil then Edit.DropListvisible := True;
end;

procedure TCustomGridView.SetEditing(Value: Boolean);
var
  WasEditing: Boolean;
begin
  WasEditing := Editing;
  if Value and AllowEdit then
  begin
    if AcquireFocus then
    begin
      CancelDrag;
      ShowEdit;
    end;
  end
  else if (not Value) and FEditing then
  begin
    ApplyEditText;
    if not AlwaysEdit then HideEdit;
  end;
  if WasEditing <> Editing then ChangeEditing;
end;

procedure TCustomGridView.SetEndEllipsis(Value: Boolean);
begin
  if FEndEllipsis <> Value then
  begin
    FEndEllipsis := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetFlatBorder(Value: Boolean);
begin
  if FFlatBorder <> Value then
  begin
    FFlatBorder := Value;
    RecreateWnd;
  end;
end;

procedure TCustomGridView.SetFixed(Value: TGridFixed);
begin
  FFixed.Assign(Value);
end;

procedure TCustomGridView.SetHeader(Value: TGridHeader);
begin
  FHeader.Assign(Value);
end;

procedure TCustomGridView.SetHideSelection(Value: Boolean);
begin
  if FHideSelection <> Value then
  begin
    FHideSelection := Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetHighlightEvenRows(const Value: Boolean);
begin
  if FHighlightEvenRows <> Value then
  begin
    FHighlightEvenRows := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetHighlightFocusCol(const Value: Boolean);
begin
  if FHighlightFocusCol <> Value then
  begin
    FHighlightFocusCol := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetHighlightFocusRow(const Value: Boolean);
begin
  if FHighlightFocusRow <> Value then
  begin
    FHighlightFocusRow := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetHorzScrollBar(Value: TGridScrollBar);
begin
  FHorzScrollBar.Assign(Value);
end;

procedure TCustomGridView.SetGrayReadOnly(const Value: Boolean);
begin
  if FGrayReadOnly <> Value then
  begin
    FGrayReadOnly := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridColor(Value: TColor);
begin
  if FGridColor <> Value then
  begin
    FGridColor := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridHint(const Value: string);
begin
  if FGridHint <> Value then
  begin
    FGridHint := Value;
    if IsGridHintVisible then Invalidate;
  end;
end;

procedure TCustomGridView.SetGridHintColor(Value: TColor);
begin
  if FGridHintColor <> Value then
  begin
    FGridHintColor := Value;
    if IsGridHintVisible then Invalidate;
  end;
end;

procedure TCustomGridView.SetGridLines(Value: Boolean);
begin
  if FGridLines <> Value then
  begin
    FGridLines := Value;
    UpdateRows;
    UpdateEdit(Editing);
    Invalidate;
  end;
end;

procedure TCustomGridView.SetGridStyle(Value: TGridStyles);
begin
  if FGridStyle <> Value then
  begin
    FGridStyle := Value;
    UpdateRows;
    UpdateEdit(Editing);
    Invalidate;
  end;
end;

procedure TCustomGridView.SetImageIndexDef(Value: Integer);
begin
  if FImageIndexDef <> Value then
  begin
    FImageIndexDef := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageHighlight(Value: Boolean);
begin
  if FImageHighlight <> Value then
  begin
    FImageHighlight := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageLeftIndent(Value: Integer);
begin
  if FImageLeftIndent <> Value then
  begin
    FImageLeftIndent := Value;
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImages(Value: TImageList);
begin
  if FImages <> Value then
  begin
    if Assigned(FImages) then FImages.UnRegisterChanges(FImagesLink);
    FImages := Value;
    if Assigned(FImages) then
    begin
      FImages.RegisterChanges(FImagesLink);
      FImages.FreeNotification(Self);
    end;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetImageTopIndent(Value: Integer);
begin
  if FImageTopIndent <> Value then
  begin
    FImageTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetLeftCol(Value: Longint);
begin
  VisOrigin := GridCell(Value, VisOrigin.Row);
end;

procedure TCustomGridView.SetReadOnly(Value: Boolean);
begin
  if FReadOnly <> Value then
  begin
    FReadOnly := Value;
    UpdateEditContents(True);
  end;
end;

procedure TCustomGridView.SetRow(Value: Longint);
begin
  CellFocused := GridCell(CellFocused.Col, Value);
end;

procedure TCustomGridView.SetRows(Value: TGridRows);
begin
  FRows.Assign(Value);
end;

procedure TCustomGridView.SetRowSelect(Value: Boolean);
begin
  if FRowSelect <> Value then
  begin
    FRowSelect := Value;
    if Value then AllowEdit := False;
    AllowSelect := AllowSelect or (not Value);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetShowCellTips(Value: Boolean);
begin
  if FShowCellTips <> Value then
  begin
    FShowCellTips := Value;
    ShowHint := ShowHint or Value;
  end;
end;

procedure TCustomGridView.SetShowFocusRect(Value: Boolean);
begin
  if FShowFocusRect <> Value then
  begin
    FShowFocusRect := Value;
    InvalidateFocus;
  end;
end;

procedure TCustomGridView.SetShowGridHint(Value: Boolean);
begin
  if FShowGridHint <> Value then
  begin
    FShowGridHint := Value;
    Invalidate;
  end;
end;

procedure TCustomGridView.SetShowHeader(Value: Boolean);
begin
  if FShowHeader <> Value then
  begin
    FShowHeader := Value;
    HeaderChange(Header);
  end;
end;

procedure TCustomGridView.SetSortLeftIndent(Value: Integer);
begin
  if FSortLeftIndent <> Value then
  begin
    FSortLeftIndent := Value;
    UpdateHeader;
    InvalidateHeader;
  end;
end;

procedure TCustomGridView.SetSortTopIndent(Value: Integer);
begin
  if FSortTopIndent <> Value then
  begin
    FSortTopIndent := Value;
    UpdateHeader;
    InvalidateHeader;
  end;
end;

procedure TCustomGridView.SetTextLeftIndent(Value: Integer);
begin
  if FTextLeftIndent <> Value then
  begin
    FTextLeftIndent := Value;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTextRightIndent(Value: Integer);
begin
  if FTextRightIndent <> Value then
  begin
    FTextRightIndent := Value;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTextTopIndent(Value: Integer);
begin
  if FTextTopIndent <> Value then
  begin
    FTextTopIndent := Value;
    UpdateRows;
    UpdateEdit(Editing);
    InvalidateGrid;
  end;
end;

procedure TCustomGridView.SetTopRow(Value: Longint);
begin
  VisOrigin := GridCell(VisOrigin.Col, Value);
end;

procedure TCustomGridView.SetVertScrollBar(Value: TGridScrollBar);
begin
  FVertScrollBar.Assign(Value);
end;

procedure TCustomGridView.SetVisOrigin(Value: TGridCell);
begin
  if (FVisOrigin.Col <> Value.Col) or (FVisOrigin.Row <> Value.Row) then
  begin
    FVisOrigin := Value;
    UpdateScrollPos;
    UpdateVisOriginSize;
    Invalidate;
  end;
end;

procedure TCustomGridView.VertScroll(Sender: TObject; ScrollCode: Integer; var ScrollPos: Integer);
begin
  CancelCellTips;
  if FocusOnScroll then UpdateFocus;
end;

procedure TCustomGridView.VertScrollChange(Sender: TObject);
begin
  UpdateVisOriginSize;
  UpdateEdit(Editing);
end;

procedure TCustomGridView.WMGetDlgCode(var Message: TWMGetDlgCode);
begin
  with Message do
  begin
    Result := DLGC_WANTARROWS;
    if not RowSelect then
    begin
      if gkTabs in CursorKeys then Result := Result or DLGC_WANTTAB;
      if AllowEdit then Result := Result or DLGC_WANTCHARS;
    end;
  end;
end;

procedure TCustomGridView.WMKillFocus(var Message: TWMKillFocus);
begin
  inherited;
  if Rows.Count > 0 then
  begin
    InvalidateFocus;
    if (FEdit <> nil) and (Message.FocusedWnd <> FEdit.Handle) then HideCursor;
  end;
end;

procedure TCustomGridView.WMSetFocus(var Message: TWMSetFocus);
begin
  inherited;
  if Rows.Count > 0 then
  begin
    InvalidateFocus;
    if (FEdit = nil) or ((Message.FocusedWnd <> FEdit.Handle) or (not FEdit.FDefocusing)) then ShowCursor;
  end;
end;

procedure TCustomGridView.WMLButtonDown(var Message: TMessage);
begin
  inherited;
  if FEdit <> nil then FEdit.FClickTime := GetMessageTime;
end;

procedure TCustomGridView.WMChar(var Msg: TWMChar);
var
  Shift: TShiftState;
begin
  if AllowEdit and (CharInSet(Char(Msg.CharCode), [^H]) or (Char(Msg.CharCode) >= #32)) then
  begin
    Shift := KeyDataToShiftState(Msg.KeyData);
    if Shift * [ssCtrl, ssAlt] = [] then
    begin
      ShowEditChar(Char(Msg.CharCode));
      Exit;
    end;
  end;
  inherited;
end;

procedure TCustomGridView.WMHScroll(var Message: TWMHScroll);
begin
  if Message.ScrollBar = 0 then
    FHorzScrollBar.ScrollMessage(Message)
  else
    inherited;
end;

procedure TCustomGridView.WMVScroll(var Message: TWMVScroll);
begin
  if Message.ScrollBar = 0 then
    FVertScrollBar.ScrollMessage(Message)
  else
    inherited;
end;

procedure TCustomGridView.WMNCHitTest(var Message: TWMNCHitTest);
begin
  inherited;
  FHitTest := ScreenToClient(SmallPointToPoint(Message.Pos));
end;

procedure TCustomGridView.WMSetCursor(var Message: TWMSetCursor);
begin
  with Message, FHitTest do
    if not (csDesigning in ComponentState) then
    begin
      if FColResizing then
      begin
        Windows.SetCursor(Screen.Cursors[crHeaderSplit]);
        Exit;
      end;
      if (HitTest = HTCLIENT) and ShowHeader then
        if PtInRect(GetHeaderRect, FHitTest) and (GetResizeSectionAt(X, Y) <> nil) then
        begin
          Windows.SetCursor(Screen.Cursors[crHeaderSplit]);
          Exit;
        end;
    end;
  inherited;
end;

procedure TCustomGridView.WMEraseBkgnd(var Message: TWMEraseBkgnd);
begin
  Message.Result := 1;
end;

procedure TCustomGridView.WMThemeThanged(var Message: TMessage);
begin
  inherited;
  { immediate processing of a Windows theme change message is not allowed,
    because ThemesServices.ThemesEnabled property has not yet been updated }
  PostMessage(Handle, CN_THEMECHANGED, 0, 0);
end;

procedure TCustomGridView.WMTimer(var Message: TWMTimer);
begin
  KillTimer(Handle, 1);
  Editing := True;
end;

procedure TCustomGridView.CNThemeThanged(var Message: TMessage);
begin
  inherited;
  { the style of the sort arrow and the height of the header change in
    Windows Vista when the theme changes }
  UpdateHeader;
end;

procedure TCustomGridView.CMCancelMode(var Message: TCMCancelMode);
begin
  if FEdit <> nil then FEdit.WndProc(TMessage(Message));
  inherited;
end;

procedure TCustomGridView.CMEnabledChanged(var Message: TMessage);
begin
  inherited;
  Invalidate;
end;

procedure TCustomGridView.CMCtl3DChanged(var Message: TMessage);
begin
  if NewStyleControls and (FBorderStyle = bsSingle) then RecreateWnd;
  inherited;
end;

procedure TCustomGridView.CMFontChanged(var Message: TMessage);
begin
  Canvas.Font := Font;
  UpdateFonts;
  UpdateRows;
  inherited;
end;

procedure TCustomGridView.CMColorChanged(var Message: TMessage);
begin
  Brush.Color := Color;
  UpdateColors;
  inherited;
end;

procedure TCustomGridView.CMShowHintChanged(var Message: TMessage);
begin
  ShowCellTips := ShowCellTips and ShowHint;
end;

procedure TCustomGridView.CMHintShow(var Message: TMessage);
var
  AllowTips: Boolean;
  R, TR: TRect;
  W: Integer;
begin
  with Message, PHintInfo(LParam)^ do
  begin
    if not ShowCellTips then
    begin
      inherited;
      Exit;
    end;
    if not PtInRect(GetGridRect, CursorPos) then
    begin
      Result := 1;
      Exit;
    end;
    FTipsCell := GetCellAt(CursorPos.X, CursorPos.Y);
    if IsCellEmpty(FTipsCell) then
    begin
      Result := 1;
      Exit;
    end;
    { hint is not allowed when editing cell text }
    if IsCellEditing(FTipsCell) then
    begin
      Result := 1;
      Exit;
    end;
    { hint can be disabled by OnCellTips event }
    CellTips(FTipsCell, AllowTips);
    if not AllowTips then
    begin
      Result := 1;
      Exit;
    end;
    { hint is shown only if the cell text gets out of cell bounds }
    R := GetCellHintRect(FTipsCell);
    TR := GetCellTextBounds(FTipsCell);
    W := TR.Right - TR.Left;
    case Columns[FTipsCell.Col].Alignment of
      taCenter:
        begin
          TR.Left := R.Left - (W - (R.Right - R.Left)) div 2;
          TR.Right := TR.Left + W;
        end;
      taRightJustify:
        begin
          TR.Right := R.Right;
          TR.Left := TR.Right - W;
        end;
    else
      TR.Left := R.Left;
      TR.Right := TR.Left + W;
    end;
    IntersectRect(R, R, ClientRect);
    if ShowHeader then SubtractRect(R, R, GetHeaderRect);
    if FTipsCell.Col >= Fixed.Count then SubtractRect(R, R, GetFixedRect);
    if (TR.Left >= R.Left) and (TR.Right <= R.Right) and
      (TR.Bottom - TR.Top <= R.Bottom - R.Top) then
    begin
      Result := 1;
      Exit;
    end;
    { prepare the string to be displayed }
    FTipsText := GetTipsText(FTipsCell);
    { prepare hint window position in screen coordinates }
    R := GetTipsRect(FTipsCell, FTipsText);
    HintPos := ClientToScreen(R.TopLeft);
    HintStr := FTipsText;
    { prepare the rectangle the user's mouse pointer must be in for the
      hint window to appear }
    R := GetCellRect(FTipsCell);
    if FTipsCell.Col < Fixed.Count then
    begin
      R.Left := Max(R.Left, GetFixedRect.Left);
      R.Right := Min(R.Right, GetFixedRect.Right);
    end
    else
    begin
      R.Left := Max(R.Left, GetFixedRect.Right);
      R.Right := Min(R.Right, GetGridRect.Right);
    end;
    InflateRect(R, 1, 1);
    CursorRect := R;
    { set hint window class }
    HintWindowClass := GetTipsWindowClass;
    HintData := Self;
    { hint is allowed }
    Result := 0;
  end;
end;

procedure TCustomGridView.CMMouseLeave(var Message: TMessage);
begin
  inherited;
  if FHotSection <> nil then
  begin
    FHotSection := nil;
    InvalidateSection(FHotColumn, FHotLevel);
  end;
end;

procedure TCustomGridView.CMWinIniChange(var Message: TWMWinIniChange);
begin
  inherited;
  { i don't remember why, but the inplace editor needs to be updated
    after changing Windows settings }
  UpdateEditContents(True);
end;

function TCustomGridView.AcquireFocus: Boolean;
begin
  Result := True;
  { if the focus is now on the input line, then grid is in focus too }
  if (FEdit <> nil) and FEdit.Focused then Exit;
  if not (csDesigning in ComponentState) and CanFocus then
  begin
    UpdateFocus;
    Result := IsActiveControl;
  end;
end;

procedure TCustomGridView.CancelCellTips;
var
  HintControl: TControl;

  function GetHintControl(Control: TControl): TControl;
  begin
    Result := Control;
    while (Result <> nil) and not Result.ShowHint do Result := Result.Parent;
    if (Result <> nil) and (csDesigning in Result.ComponentState) then Result := nil;
  end;

begin
  if ShowCellTips then
  begin
    HintControl := GetHintControl(FindDragTarget(Mouse.CursorPos, False));
    if HintControl = Self then Application.CancelHint;
  end;
end;

procedure TCustomGridView.CellClick(Cell: TGridCell; Shift: TShiftState; X, Y: Integer);
begin
  if Assigned(FOnCellClick) then FOnCellClick(Self, Cell, Shift, X, Y);
end;

procedure TCustomGridView.CellTips(Cell: TGridCell; var AllowTips: Boolean);
begin
  AllowTips := True;
  if Assigned(FOnCellTips) then FOnCellTips(self, Cell, AllowTips);
end;

procedure TCustomGridView.Change(var Cell: TGridCell; var Selected: Boolean);
begin
  if Assigned(FOnChange) then FOnChange(Self, Cell, Selected);
end;

procedure TCustomGridView.ChangeColumns;
begin
  if Assigned(FOnChangeColumns) then FOnChangeColumns(Self);
end;

procedure TCustomGridView.ChangeEditing;
begin
  if Assigned(FOnChangeEditing) then FOnChangeEditing(Self);
end;

procedure TCustomGridView.ChangeEditMode;
begin
  if Assigned(FOnChangeEditMode) then FOnChangeEditMode(Self);
end;

procedure TCustomGridView.ChangeFixed;
begin
  if Assigned(FOnChangeFixed) then FOnChangeFixed(Self);
end;

procedure TCustomGridView.ChangeRows;
begin
  if Assigned(FOnChangeRows) then FOnChangeRows(Self);
end;

procedure TCustomGridView.ChangeScale(M, D: Integer);
var
  I: Integer;
begin
  inherited ChangeScale(M, D);
  if M <> D then
  begin
    with Columns do
    begin
      BeginUpdate;
      try
        for I := 0 to Count - 1 do
        begin
          Columns[I].FMaxWidth := MulDiv(Columns[I].FMaxWidth, M, D);
          Columns[I].FMinWidth := MulDiv(Columns[I].FMinWidth, M, D);
          Columns[I].DefWidth := MulDiv(Columns[I].DefWidth, M, D);
        end;
      finally
        EndUpdate;
      end;
    end;
    with Rows do
      Height := MulDiv(Height, M, D);
    with Header do
    begin
      SectionHeight := MulDiv(SectionHeight, M, D);
      if not GridFont then Font.Size := MulDiv(Font.Size, M, D);
    end;
    with Fixed do
      if not GridFont then Font.Size := MulDiv(Font.Size, M, D);
  end;
end;

procedure TCustomGridView.Changing(var Cell: TGridCell; var Selected: Boolean);
begin
  if Assigned(FOnChanging) then FOnChanging(Self, Cell, Selected);
end;

procedure TCustomGridView.CheckClick(Cell: TGridCell);
begin
  if Assigned(FOnCheckClick) then
  begin
    FOnCheckClick(Self, Cell);
    { usually the state of the check box (and the cell text) changes
      in the event, so it needs to be redrawn }
    InvalidateCell(Cell);
  end;
end;

procedure TCustomGridView.ColumnResize(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnResize) then FOnColumnResize(Self, Column, Width);
end;

procedure TCustomGridView.ColumnResizing(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnResizing) then FOnColumnResizing(Self, Column, Width);
end;

procedure TCustomGridView.ColumnSizeToFit(Column: Integer; var Width: Integer);
begin
  if Assigned(FOnColumnSizeToFit) then FOnColumnSizeToFit(Self, Column, Width);
end;

function TCustomGridView.CreateColumn(Columns: TGridColumns): TCustomGridColumn;
begin
  Result := GetColumnClass.Create(Columns);
end;

function TCustomGridView.CreateColumns: TGridColumns;
begin
  Result := TGridColumns.Create(Self);
end;

function TCustomGridView.CreateEdit(EditClass: TGridEditClass): TCustomGridEdit;
begin
  if EditClass = nil then EditClass := TGridEdit;
  Result := EditClass.Create(Self);
end;

function TCustomGridView.CreateFixed: TCustomGridFixed;
begin
  Result := TGridFixed.Create(Self);
end;

function TCustomGridView.CreateHeader: TCustomGridHeader;                 
begin
  Result := TGridHeader.Create(Self);
end;

function TCustomGridView.CreateHeaderSection(Sections: TGridHeaderSections): TGridHeaderSection;
begin
  Result := TGridHeaderSection.Create(Sections);
end;

procedure TCustomGridView.CreateParams(var Params: TCreateParams);
const
  BorderStyles: array[TBorderStyle] of DWORD = (0, WS_BORDER);
  FlatBorders: array[Boolean] of DWORD = (WS_EX_CLIENTEDGE, WS_EX_STATICEDGE);
begin
  inherited CreateParams(Params);
  with Params do
  begin
    Style := Style or WS_TABSTOP;
    Style := Style or BorderStyles[FBorderStyle];
    if Ctl3D and NewStyleControls and (FBorderStyle = bsSingle) then
    begin
      Style := Style and not WS_BORDER;
      ExStyle := ExStyle or FlatBorders[FFlatBorder];
    end;
  end;
end;

function TCustomGridView.CreateRows: TCustomGridRows;
begin
  Result := TGridRows.Create(Self);
end;

function TCustomGridView.CreateScrollBar(Kind: TScrollBarKind): TGridScrollBar;
begin
  Result := TGridScrollBar.Create(Self, Kind);
end;

procedure TCustomGridView.CreateWnd;
begin
  inherited;
  { window scroll bars must be updated after create window }
  FHorzScrollBar.Update;
  FVertScrollBar.Update;
  { use the explorer theme to draw the selected cell (only if the uxtheme.dll
    library loaded, from which the SetWindowTheme function is imported) }
  if StyleServices.Available then
    SetWindowTheme(Handle, 'explorer', nil);
end;

procedure TCustomGridView.DoContextPopup(MousePos: TPoint;
  var Handled: Boolean);
begin
  FContextPopupCol := GetColumnAt(MousePos.X, MousePos.Y);
  FContextPopupRow := GetRowAt(MousePos.X, MousePos.Y);
  inherited DoContextPopup(MousePos, Handled);
  if not Handled then DoHeaderPopup(MousePos, Handled);
end;

procedure TCustomGridView.DoExit;
begin
  ResetClickPos;
  if CancelOnExit then Editing := False;
  inherited DoExit;
end;

procedure TCustomGridView.DoHeaderPopup(MousePos: TPoint; var Handled: Boolean);
var
  Menu: TPopupMenu;
  ClickEvent: TNotifyEvent;
  I, H, N: Integer;
  Column: TGridColumn;
  Item: TMenuItem;
  Monitor: TMonitor;
  Rect: TRect;
  Pt: TPoint;
begin
  if ShowHeader and PtInRect(GetHeaderRect, MousePos) and
    (DefaultHeaderMenu or (Header.PopupMenu <> nil)) then
  begin
    if DefaultHeaderMenu then
    begin
      ClickEvent := HeaderMenuClick;
      if Header.PopupMenu <> nil then
      begin
        Menu := Header.PopupMenu;
        for I := Menu.Items.Count - 1 downto 0 do
        begin
          Item := Menu.Items[I];
          if @Item.OnClick = @ClickEvent then Item.Free;
        end;
        if Menu.Items.Count <> 0 then
        begin
          Item := NewItem(cLineCaption, 0, True, True, ClickEvent, 0, '');
          Menu.Items.Add(Item);
        end;
      end
      else
      begin
        if FHeaderPopupMenu = nil then
        begin
          FHeaderPopupMenu := TPopupMenu.Create(Self);
          FHeaderPopupMenu.AutoLineReduction := maManual;
        end;
        Menu := FHeaderPopupMenu;
        Menu.Items.Clear;
      end;
      { for each column add menu item }
      for I := 0 to Columns.Count - 1 do
      begin
        Column := Columns[I];
        Item := NewItem(Column.Caption2, 0, Column.Visible, Column.DefaultPopup, ClickEvent, 0, '');
        Item.Tag := I;
        Menu.Items.Add(Item);
      end;
      { for OnHeaderDetailsClick event handler add "Details..." item }
      if Assigned(FOnHeaderDetailsClick) then
      begin
        if Menu.Items.Count <> 0 then
        begin
          Item := NewItem(cLineCaption, 0, True, True, ClickEvent, 0, '');
          Menu.Items.Add(Item);
        end;
        Item := NewItem(SHeaderDetails, 0, False, Columns.Count <> 0, ClickEvent, 0, '');
        Item.Tag := -1;
        Menu.Items.Add(Item);
      end;
      { if there are many columns, and the screen resolution is small,
        then the menu will not fully fit on the screen, so we will divide
        the menu into several columns for which we need to calculate,
        how many menu items fit on the current monitor }
      if Menu.Items.Count <> 0 then
      begin
        Monitor := Screen.MonitorFromPoint(ClientToScreen(MousePos));
        if Monitor <> nil then Rect := Monitor.WorkareaRect
        else Rect := Screen.WorkareaRect;
        H := GetSystemMetrics(SM_CYMENUSIZE);
        if H = 0 then N := 1024 else N := (Rect.Bottom - Rect.Top) div H;
        for I := 0 to Menu.Items.Count - 1 do
        begin
          Item := Menu.Items[I];
          if (I > 1) and (I mod N = 0) then Item.Break := mbBarBreak
          else Item.Break := mbNone;
        end;
      end
      else
        Menu := nil;
    end
    else
      Menu := Header.PopupMenu;
    if Menu <> nil then
    begin
      SendCancelMode(Self);
      Menu.PopupComponent := Self;
      Pt := ClientToScreen(MousePos);
      Menu.Popup(Pt.X, Pt.Y);
      Handled := True;
    end;
  end;
end;

function TCustomGridView.DoMouseWheelDown(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelDown(Shift, MousePos);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    if not (ssShift in Shift) then
      SetCursor(GetCursorCell(CellFocused, goDown), True, True)
    else if not RowSelect then
      SetCursor(GetCursorCell(CellFocused, goRight), True, True)
    else
      with HorzScrollBar do Position := Position + LineStep * LineSize;
    Result := True;
  end;
end;

function TCustomGridView.DoMouseWheelUp(Shift: TShiftState; MousePos: TPoint): Boolean;
begin
  Result := inherited DoMouseWheelUp(Shift, MousePos);
  if (not Result) and (gkMouseWheel in CursorKeys) then
  begin
    if not (ssShift in Shift) then
      SetCursor(GetCursorCell(CellFocused, goUp), True, True)
    else if not RowSelect then
      SetCursor(GetCursorCell(CellFocused, goLeft), True, True)
    else
      with HorzScrollBar do Position := Position - LineStep * LineSize;
    Result := True;
  end;
end;

procedure TCustomGridView.DoStartDrag(var DragObject: TDragObject);
begin
  KillTimer(Handle, 1);
  inherited;
end;

procedure TCustomGridView.DoTextNotFound(const FindText: string);
var
  S, Caption: string;
  Form: TCustomForm;
begin
  if Assigned(FOnTextNotFound) then
    FOnTextNotFound(Self, FindText)
  else
  begin
    S := Format(STextNotFound, [FindText]);
    Form := GetParentForm(Self);
    if Form <> nil then Caption := Form.Caption
    else Caption := Application.Title;
    Application.MessageBox(PChar(S), PChar(Caption), MB_ICONINFORMATION);
  end;
end;

procedure TCustomGridView.EditButtonPress(Cell: TGridCell);
begin
  if Assigned(FOnEditButtonPress) then FOnEditButtonPress(Self, Cell);
end;

procedure TCustomGridView.EditCanceled(Cell: TGridCell);
begin
  if Assigned(FOnEditCanceled) then FOnEditCanceled(Self, Cell);
end;

function TCustomGridView.EditCanModify(Cell: TGridCell): Boolean;
begin
  Result := not IsCellReadOnly(Cell);
  if Assigned(FOnEditCanModify) then FOnEditCanModify(Self, Cell, Result);
end;

function TCustomGridView.EditCanAcceptKey(Cell: TGridCell; Key: Char): Boolean;
begin
  Result := IsCellValid(Cell);
  if Assigned(FOnEditAcceptKey) then FOnEditAcceptKey(Self, Cell, Key, Result);
end;

function TCustomGridView.EditCanShow(Cell: TGridCell): Boolean;
begin
  if [csReading, csLoading, csDesigning, csDestroying] * ComponentState <> [] then
  begin
    Result := False;
    Exit;
  end;
  if (Columns.Count - Fixed.Count = 0) or (Rows.Count = 0) then
  begin
    Result := False;
    Exit;
  end;
  Result := HandleAllocated and AllowEdit and (AlwaysEdit or IsActiveControl);
  if (Cell.Col >= Fixed.Count) and (Cell.Col < Columns.Count) then
    Result := Result and Columns[Cell.Col].AllowEdit;
  if Result and Assigned(FOnEditCanShow) then
    FOnEditCanShow(Self, Cell, Result);
end;

function TCustomGridView.EditCanUndo(Cell: TGridCell): Boolean;
begin
  Result := EditCanModify(Cell);
end;

procedure TCustomGridView.EditChange(Cell: TGridCell);
begin
  if Assigned(FOnEditChange) then FOnEditChange(Self, Cell);
end;

procedure TCustomGridView.EditCloseUp(Cell: TGridCell; Items: TStrings;
  ItemIndex: Integer; var ItemText: string; var Accept: Boolean);
begin
  if Assigned(FOnEditCloseUp) then
    FOnEditCloseUp(Self, Cell, ItemIndex, Accept);
  if Assigned(FOnEditCloseUpEx) then
    FOnEditCloseUpEx(Self, Cell, Items, ItemIndex, ItemText, Accept);
end;

procedure TCustomGridView.EditSelectNext(Cell: TGridCell; var Value: string);
begin
  if Assigned(FOnEditSelectNext) then FOnEditSelectNext(Self, Cell, Value);
end;

procedure TCustomGridView.GetCellColors(Cell: TGridCell; Canvas: TCanvas);
var
  Theme: HTHEME;
begin
  if Cell.Col < Fixed.Count then
  begin
    Canvas.Font := Fixed.Font;
    Canvas.Brush.Color := Fixed.Color;
    { if the button face color is used for a fixed cell, then with themes
      enabled it should be slightly discolored to match the highlighted
      row with the HighlightFocusRow turned on }
    if (Fixed.Color = clBtnFace) and StyleServices.Enabled then
      Canvas.Brush.Color := GetLightenColor(Canvas.Brush.Color, 8);
    { highlight every other row }
    if Fixed.GridColor and StyleServices.Enabled and
      HighlightEvenRows and IsEvenRow(Cell) then
        Canvas.Brush.Color := GetLightenColor(Canvas.Brush.Color, -8);
    { highlight row with focused cell }
    if Fixed.GridColor and HighlightFocusRow then
      if (Cell.Row = CellFocused.Row) and ((Cell.Col <> CellFocused.Col) or not Editing) then
        if StyleServices.Enabled then
          Canvas.Brush.Color := GetLightenColor(clBtnFace, 8)
        else
          Canvas.Brush.Color := clBtnFace;
    { set gray text color for read-only cell }
    if GrayReadOnly and IsCellReadOnly(Cell) then
      Canvas.Font.Color := clGrayText;
  end
  else
  begin
    Canvas.Brush.Color := Self.Color;
    Canvas.Font := Self.Font;
    { set gray text color for disabled and read-only cells }
    if not Enabled then
      Canvas.Font.Color := clGrayText
    else if GrayReadOnly and IsCellReadOnly(Cell) then
      Canvas.Font.Color := clGrayText;
    { focused cell }
    if Enabled and IsCellHighlighted(Cell) and (not IsCellEditing(Cell)) then
    begin
      { OpenThemeData function is available only if the uxtheme.dll
        library loaded and only in Windows Vista and above }
      if StyleServices.Enabled and CheckWin32Version(6, 0) then
        Theme := OpenThemeData(Handle, 'TREEVIEW')
      else
        Theme := 0;
      { do not change focused cell color on Windows Vista with themes enabled }
      if Theme <> 0 then
      begin
        CloseThemeData(Theme);
      end
      else if Focused or EditFocused then
      begin
        Canvas.Brush.Color := clHighlight;
        Canvas.Font.Color := clHighlightText;
      end
      else if not HideSelection then
      begin
        Canvas.Brush.Color := clBtnFace;
        Canvas.Font.Color := Font.Color;
      end;
    end
    else
    begin
      { highlight every other row without windows theme }
      if StyleServices.Enabled then
        if HighlightEvenRows and IsEvenRow(Cell) then
          Canvas.Brush.Color := GetLightenColor(Canvas.Brush.Color, -8);
      { highlight row with focused cell }
      if HighlightFocusRow then
        if (Cell.Row = CellFocused.Row) and ((Cell.Col <> CellFocused.Col) or not Editing) then
          if StyleServices.Enabled then
            Canvas.Brush.Color := GetLightenColor(clBtnFace, 8)
          else
            Canvas.Brush.Color := clBtnFace;
    end;
  end;
  if Assigned(FOnGetCellColors) then FOnGetCellColors(Self, Cell, Canvas);
end;

function TCustomGridView.GetCellImage(Cell: TGridCell; var OverlayIndex: Integer): Integer;
begin
  OverlayIndex := -1;
  Result := -1;
  if not Assigned(Images) then Exit;
  { only the first column has default image }
  if Cell.Col = GetFirstImageColumn then Result := ImageIndexDef;
  if Assigned(FOnGetCellImage) then FOnGetCellImage(Self, Cell, Result);
  if Assigned(FOnGetCellImageEx) then FOnGetCellImageEx(Self, Cell, Result, OverlayIndex);
end;

function TCustomGridView.GetCellImageIndent(Cell: TGridCell): TPoint;
begin
  Result.X := ImageLeftIndent;
  Result.Y := ImageTopIndent;
  if GridLines and (Fixed.Count > 0) and (not Fixed.Flat) and
    (not StyleServices.Enabled) then Inc(Result.Y, 1);
  if Assigned(FOnGetCellImageIndent) then FOnGetCellImageIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCellImageRect(Cell: TGridCell): TRect;
var
  II: TPoint;
  R: TRect;
begin
  if not IsCellHasImage(Cell) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  R := GetCellRect(Cell);
  if IsCellHasCheck(Cell) then
    Inc(R.Left, CheckWidth + GetCheckIndent(Cell).X);
  with Result do
  begin
    II := GetCellImageIndent(Cell);
    Left := R.Left + II.X;
    Right := Min(Left + Images.Width, R.Right);
    Top := R.Top + II.Y;
    Bottom := R.Top + Images.Height;
  end;
end;

function TCustomGridView.GetCellHintRect(Cell: TGridCell): TRect;
begin
  Result := GetEditRect(Cell);
  if Assigned(FOnGetCellHintRect) then FOnGetCellHintRect(Self, Cell, Result);
end;

function TCustomGridView.GetCellText(Cell: TGridCell): string;
begin
  Result := '';
  if Assigned(FOnGetCellText) then FOnGetCellText(Self, Cell, Result);
end;

function TCustomGridView.GetCellTextBounds(Cell: TGridCell): TRect;
var
  R: TRect;
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
begin
  if (Cell.Col < 0) or (Cell.Col > Columns.Count - 1) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  { bounds depend on cell font }
  if (Cell.Row >= 0) and (Cell.Row < Rows.Count) then
  begin
    GetCellColors(Cell, Canvas);
    TI := GetCellTextIndent(Cell);
    T := GetCellText(Cell);
  end;
  R := Rect(0, 0, 0, 0);
  if Columns[Cell.Col].WordWrap then
  begin
    R := GetEditRect(Cell);
    OffsetRect(R, -R.Left, -R.Top);
    R.Bottom := R.Top;
  end;
  A := Columns[Cell.Col].Alignment;
  WR := Columns[Cell.Col].WantReturns;
  WW := Columns[Cell.Col].WordWrap;
  Result := GetTextRect(Canvas, R, TI.X, TI.Y, A, WR, WW, T);
  OffsetRect(Result, -Result.Left, -Result.Top);
end;

function TCustomGridView.GetCellTextIndent(Cell: TGridCell): TPoint;
begin
  Result.X := TextLeftIndent;
  Result.Y := TextTopIndent;
  if IsCellHasCheck(Cell) or IsCellHasImage(Cell) then Result.X := 2;
  if GridLines and (Fixed.Count > 0) and (not Fixed.Flat) and
    (not StyleServices.Enabled) then Inc(Result.Y, 1);
  if Assigned(FOnGetCellTextIndent) then FOnGetCellTextIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCheckAlignment(Cell: TGridCell): TAlignment;
begin
  Result := taLeftJustify;
  if CheckBoxes and (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].CheckAlignment;
    if Assigned(FOnGetCheckAlignment) then FOnGetCheckAlignment(Self, Cell, Result);
  end;
end;

procedure TCustomGridView.GetCheckImage(Cell: TGridCell; CheckImage: TBitmap);
begin
  if Assigned(FOnGetCheckImage) then FOnGetCheckImage(Self, Cell, CheckImage);
end;

function TCustomGridView.GetCheckIndent(Cell: TGridCell): TPoint;
begin
  Result.X := CheckLeftIndent;
  Result.Y := CheckTopIndent;
  if GridLines and (Fixed.Count > 0) and (not Fixed.Flat) and
    (not StyleServices.Enabled) then Inc(Result.Y, 1);
  if GetCheckAlignment(Cell) = taCenter then Result.X := (Columns[Cell.Col].Width - CheckWidth) div 2 - 1;
  if Assigned(FOnGetCheckIndent) then FOnGetCheckIndent(Self, Cell, Result);
end;

function TCustomGridView.GetCheckKind(Cell: TGridCell): TGridCheckKind;
begin
  Result := gcNone;
  if CheckBoxes and (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].CheckKind;
    if Assigned(FOnGetCheckKind) then FOnGetCheckKind(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetCheckRect(Cell: TGridCell): TRect;
var
  IC: TPoint;
  R: TRect;
begin
  if not IsCellHasCheck(Cell) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  R := GetCellRect(Cell);
  with Result do
  begin
    IC := GetCheckIndent(Cell);
    Left := R.Left + IC.X;
    Right := Min(Left + CheckWidth, R.Right);
    Top := R.Top + IC.Y;
    Bottom := R.Top + CheckHeight;
  end;
end;

function TCustomGridView.GetCheckState(Cell: TGridCell): TCheckBoxState;
var
  Dummy: Boolean;
begin
  Result := GetCheckStateEx(Cell, Dummy);
end;

function TCustomGridView.GetCheckStateEx(Cell: TGridCell;
  var CheckEnabled: Boolean): TCheckBoxState;
begin
  CheckEnabled := True;
  Result := cbUnchecked;
  if Assigned(FOnGetCheckStateEx) then
    FOnGetCheckStateEx(Self, Cell, Result, CheckEnabled)
  else if Assigned(FOnGetCheckState) then
    FOnGetCheckState(Self, Cell, Result);
end;

function TCustomGridView.GetClientOrigin: TPoint;
begin
  if Parent = nil then
    Result := Point(0, 0)
  else
    Result := inherited GetClientOrigin;
end;

function TCustomGridView.GetClientRect: TRect;
begin
  if (Parent = nil) or (not HandleAllocated) then
    Result := Bounds(0, 0, Width, Height)
  else
    Result := inherited GetClientRect;
end;

function TCustomGridView.GetColumnClass: TGridColumnClass;
begin
  Result := TGridColumn;
end;

function TCustomGridView.GetCursorCell(Cell: TGridCell; Offset: TGridCursorOffset): TGridCell;

  function DoMoveLeft(O: Integer): TGridCell;
  var
    I: Integer;
    C: TGridCell;
  begin
    I := Max(Cell.Col - O, Fixed.Count);
    while I >= Fixed.Count do
    begin
      C := GridCell(I, Cell.Row);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Dec(I);
    end;
    Result := Cell;
  end;

  function DoMoveRight(O: Integer): TGridCell;
  var
    I: Integer;
    C: TGridCell;
  begin
    I := Min(Cell.Col + O, Columns.Count - 1);
    while I <= Columns.Count - 1 do
    begin
      C := GridCell(I, Cell.Row);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Inc(I);
    end;
    Result := Cell;
  end;

  function DoMoveUp(O: Integer): TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    J := Max(Cell.Row - O, 0);
    while J >= 0 do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Dec(J);
    end;
    Result := Cell;
  end;

  function DoMoveDown(O: Integer): TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    J := Min(Cell.Row + O, Rows.Count - 1);
    while J <= Rows.Count - 1 do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Inc(J);
    end;
    Result := Cell;
  end;

  function DoMoveHome: TGridCell;
  var
    C: TGridCell;
  begin
    C := Cell;
    try
      Cell.Col := Fixed.Count;
      Result := DoMoveRight(0);
    finally
      Cell := C;
    end;
  end;

  function DoMoveEnd: TGridCell;
  var
    C: TGridCell;
  begin
    C := Cell;
    try
      Cell.Col := Columns.Count - 1;
      Result := DoMoveLeft(0);
    finally
      Cell := C;
    end;
  end;

  function DoMoveGridHome: TGridCell;
  var
    I, J: Integer;
    C: TGridCell;
  begin
    I := Fixed.Count;
    while I <= Cell.Col do
    begin
      J := 0;
      while J <= Cell.Row do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(J);
      end;
      Inc(I);
    end;
    Result := Cell;
  end;

  function DoMoveGridEnd: TGridCell;
  var
    I, J: Integer;
    C: TGridCell;
  begin
    I := Columns.Count - 1;
    while I >= Cell.Col do
    begin
      J := Rows.Count - 1;
      while J >= Cell.Row do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Dec(J);
      end;
      Dec(I);
    end;
    Result := Cell;
  end;

  function DoMoveGridTop: TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    J := 0;
    while J <= Cell.Row do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Inc(J);
    end;
    Result := Cell;
  end;

  function DoMoveGridBottom: TGridCell;
  var
    J: Integer;
    C: TGridCell;
  begin
    J := Rows.Count - 1;
    while J >= Cell.Row do
    begin
      C := GridCell(Cell.Col, J);
      if IsCellAcceptCursor(C) then
      begin
        Result := C;
        Exit;
      end;
      Dec(J);
    end;
    Result := Cell;
  end;

  function DoSelect: TGridCell;

    function DoSelectLeft: TGridCell;
    var
      I: Integer;
      C: TGridCell;
    begin
      I := Max(Cell.Col, Fixed.Count);
      while I <= CellFocused.Col do
      begin
        C := GridCell(I, Cell.Row);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(I);
      end;
      Result := Cell;
    end;

    function DoSelectRight: TGridCell;
    var
      I: Integer;
      C: TGridCell;
    begin
      I := Min(Cell.Col, Columns.Count - 1);
      while I >= CellFocused.Col do
      begin
        C := GridCell(I, Cell.Row);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Dec(I);
      end;
      Result := Cell;
    end;

    function DoSelectUp: TGridCell;
    var
      J: Integer;
      C: TGridCell;
    begin
      J := Max(Cell.Row, 0);
      while J <= CellFocused.Row do
      begin
        C := GridCell(Cell.Col, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(J);
      end;
      Result := Cell;
    end;

    function DoSelectDown: TGridCell;
    var
      J: Integer;
      C: TGridCell;
    begin
      J := Min(Cell.Row, Rows.Count - 1);
      while J >= CellFocused.Row do
      begin
        C := GridCell(Cell.Col, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Dec(J);
      end;
      Result := Cell;
    end;

  begin
    if IsCellAcceptCursor(Cell) then
    begin
      Result := Cell;
      Exit;
    end;
    if Cell.Col < CellFocused.Col then
    begin
      Result := DoSelectLeft;
      if IsCellAcceptCursor(Result) then Exit;
    end;
    if Cell.Col > CellFocused.Col then
    begin
      Result := DoSelectRight;
      if IsCellAcceptCursor(Result) then Exit;
    end;
    if Cell.Row < CellFocused.Row then
    begin
      Result := DoSelectUp;
      if IsCellAcceptCursor(Result) then Exit;
    end;
    if Cell.Row > CellFocused.Row then
    begin
      Result := DoSelectDown;
      if IsCellAcceptCursor(Result) then Exit;
    end;
    Result := CellFocused;
  end;

  function DoFirst: TGridCell;
  var
    C: TGridCell;
    I, J: Integer;
  begin
    J := 0;
    while J <= Rows.Count - 1 do
    begin
      I := Fixed.Count;
      while I <= Columns.Count - 1 do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) then
        begin
          Result := C;
          Exit;
        end;
        Inc(I);
      end;
      Inc(J);
    end;
    Result := CellFocused;
  end;

  function DoNext: TGridCell;
  var
    C: TGridCell;
    I, J: Integer;
  begin
    I := Cell.Col + 1;
    J := Cell.Row;
    while J <= Rows.Count - 1 do
    begin
      while I <= Columns.Count - 1 do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) and ((not RowSelect) or (C.Row <> Cell.Row)) then
        begin
          Result := C;
          Exit;
        end;
        Inc(I);
      end;
      I := Fixed.Count;
      Inc(J);
    end;
    Result := CellFocused;
  end;

  function DoPrev: TGridCell;
  var
    C: TGridCell;
    I, J: Integer;
  begin
    I := Cell.Col - 1;
    J := Cell.Row;
    while J >= 0 do
    begin
      while I >= Fixed.Count do
      begin
        C := GridCell(I, J);
        if IsCellAcceptCursor(C) and ((not RowSelect) or (C.Row <> Cell.Row)) then
        begin
          Result := C;
          Exit;
        end;
        Dec(I);
      end;
      I := Columns.Count - 1;
      Dec(J);
    end;
    Result := CellFocused;
  end;

begin
  case Offset of
    goLeft: Result := DoMoveLeft(1);
    goRight: Result := DoMoveRight(1);
    goUp: Result := DoMoveUp(1);
    goDown: Result := DoMoveDown(1);
    goPageUp: Result := DoMoveUp(VisSize.Row - 1);
    goPageDown: Result := DoMoveDown(VisSize.Row - 1);
    goHome: Result := DoMoveHome;
    goEnd: Result := DoMoveEnd;
    goGridHome: Result := DoMoveGridHome;
    goGridEnd: Result := DoMoveGridEnd;
    goGridTop: Result := DoMoveGridTop;
    goGridBottom: Result := DoMoveGridBottom;
    goSelect: Result := DoSelect;
    goFirst: Result := DoFirst;
    goNext: Result := DoNext;
    goPrev: Result := DoPrev;
  else
    Result := Cell;
  end;
end;

function TCustomGridView.GetEditClass(Cell: TGridCell): TGridEditClass;
begin
  Result := TGridEdit;
end;

procedure TCustomGridView.GetEditList(Cell: TGridCell; Items: TStrings);
begin
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    with Columns[Cell.Col] do
      if (EditStyle = gePickList) and (FPickList <> nil) then Items.Assign(FPickList);
    if Assigned(FOnGetEditList) then FOnGetEditList(Self, Cell, Items);
  end;
end;

procedure TCustomGridView.GetEditListBounds(Cell: TGridCell; var Rect: TRect);
begin
  if Assigned(FOnGetEditListBounds) then FOnGetEditListBounds(Self, Cell, Rect);
end;

function TCustomGridView.GetEditListIndex(Cell: TGridCell; Items: TStrings;
  const ItemText: string): Integer;
begin
  Result := -1;
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Items.IndexOf(ItemText);
    if Assigned(FOnGetEditListIndex) then
      FOnGetEditListIndex(Self, Cell, Items, ItemText, Result);
  end;
end;

function TCustomGridView.GetEditMask(Cell: TGridCell): string;
begin
  Result := '';
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].EditMask;
    if Assigned(FOnGetEditMask) then FOnGetEditMask(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetEditStyle(Cell: TGridCell): TGridEditStyle;
begin
  Result := geSimple;
  if (Cell.Col >= 0) and (Cell.Col < Columns.Count) then
  begin
    Result := Columns[Cell.Col].EditStyle;
    if Assigned(FOnGetEditStyle) then FOnGetEditStyle(Self, Cell, Result);
  end;
end;

function TCustomGridView.GetEditText(Cell: TGridCell): string;
begin
  Result := GetCellText(Cell);
  if Assigned(FOnGetEditText) then FOnGetEditText(Self, Cell, Result);
end;

function TCustomGridView.GetFixedDividerColor: TColor;
begin
  Result := GetFixedGridColor;
end;

function TCustomGridView.GetFixedGridColor: TColor;
begin
  if Fixed.GridColor then Result := GetGridLineColor(Color)
  else if CheckWin32Version(6, 0) then Result := cl3DLight
  else Result := clActiveBorder;
end;

procedure TCustomGridView.GetHeaderColors(Section: TGridHeaderSection; Canvas: TCanvas);
begin
  Canvas.Brush.Color := Header.Color;
  Canvas.Font := Header.Font;
  if not Enabled then Canvas.Font.Color := clGrayText;
  if Assigned(FOnGetHeaderColors) then FOnGetHeaderColors(Self, Section, Canvas);
end;

function TCustomGridView.GetGridHint: string;
begin
  Result := FGridHint;
  if Assigned(FOnGetGridHint) then FOnGetGridHint(Self, Result); 
end;

function TCustomGridView.GetGridLineColor(BkColor: TColor): TColor;
begin
  if StyleServices.Enabled then
    Result := GetLightenColor(Self.Color, -24)
  else
  begin
    Result := FGridColor;
    if ColorToRGB(Result) = ColorToRGB(BkColor) then Result := GetLightenColor(Result, -64);
  end;
  if Assigned(FOnGetGridColor) then FOnGetGridColor(Self, Result);
end;

function TCustomGridView.GetHeaderImage(Section: TGridHeaderSection): Integer;
begin
  if not Assigned(Header.Images) then
  begin
    Result := -1;
    Exit;
  end;
  { the default image index is the column index }
  Result := Section.ColumnIndex;
  if Assigned(FOnGetHeaderImage) then FOnGetHeaderImage(Self, Section, Result);
end;

function TCustomGridView.GetSortArrowSize: TSize;
begin
  if StyleServices.Enabled and CheckWin32Version(6, 0) then
  begin
    Result.cx := 13;
    Result.cy := 5;
  end
  else
  begin
    Result.cx := 8;
    Result.cy := 5;
  end;
end;

function TCustomGridView.GetSortDirection(Section: TGridHeaderSection): TGridSortDirection;
begin
  Result := gsNone;
  if Assigned(FOnGetSortDirection) then FOnGetSortDirection(Self, Section, Result);
end;

procedure TCustomGridView.GetSortImage(Section: TGridHeaderSection; SortImage: TBitmap);
begin
  if Assigned(FOnGetSortImage) then FOnGetSortImage(Self, Section, SortImage);
end;

function TCustomGridView.GetTextRect(Canvas: TCanvas; Rect: TRect;
  LeftIndent, TopIndent: Integer; Alignment: TAlignment;
  WantReturns, WordWrap: Boolean; const Text: string): TRect;
var
  R: TRect;
  P: TDrawTextParams;
  F, W, H, I: Integer;
begin
  { there are different ways to draw text: using DrawTextEx for multiline
    text and TextOut for single line text (see PaintText) }
  if WantReturns or WordWrap or EndEllipsis then
  begin
    FillChar(P, SizeOf(P), 0);
    P.cbSize := SizeOf(P);
    P.iLeftMargin := LeftIndent;
    P.iRightMargin := TextRightIndent;
    F := DT_NOPREFIX;
    case Alignment of
      taLeftJustify: F := F or DT_LEFT;
      taCenter: F := F or DT_CENTER;
      taRightJustify: F := F or DT_RIGHT;
    end;
    if not (WantReturns or WordWrap) then
    begin
      F := F or DT_SINGLELINE or DT_VCENTER; // <- ellipsis style
    end;
    if WordWrap then F := F or DT_WORDBREAK;
    R := Rect;
    DrawTextEx(Canvas.Handle, PChar(Text), Length(Text), R, F or DT_CALCRECT, @P);
    W := Max(Rect.Right - Rect.Left, R.Right - R.Left);
    H := Max(Rect.Bottom - Rect.Top, R.Bottom - R.Top);
  end
  else
  begin
    I := LeftIndent;
    W := Max(Rect.Right - Rect.Left, I + Canvas.TextWidth(Text) + TextRightIndent);
    H := Max(Rect.Bottom - Rect.Top, Canvas.TextHeight(Text));
  end;
  case Alignment of
    taCenter:
      begin
        R.Left := Rect.Left - (W - (Rect.Right - Rect.Left)) div 2;
        R.Right := R.Left + W;
      end;
    taRightJustify:
      begin
        R.Right := Rect.Right;
        R.Left := R.Right - W;
      end;
  else
    R.Left := Rect.Left;
    R.Right := R.Left + W;
  end;
  R.Top := Rect.Top;
  R.Bottom := R.Top + H;
  Result := R;
end;

function TCustomGridView.GetTipsRect(Cell: TGridCell; const TipsText: string): TRect;
var
  R: TRect;
  TI: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
begin
  if not IsCellValid(Cell) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  with GetTipsWindowClass.Create(Self) do
  try
    GetCellColors(Cell, Canvas);
    R := GetEditRect(Cell);
    TI := GetCellTextIndent(Cell);
    A := Columns[Cell.Col].Alignment;
    WR := Pos(#13, TipsText) <> 0; // Columns[Cell.Col].WantReturns;
    WW := Columns[Cell.Col].WordWrap;
    R := GetTextRect(Canvas, R, TI.X, TI.Y, A, WR, WW, TipsText);
  finally
    Free;
  end;
  if R.Bottom - R.Top > Rows.Height then
    { correction if text height is greater than the height of the row }
    Inc(R.Bottom, TextTopIndent * 2); {!}
  InflateRect(R, 1, 1); // <- border
  Result := R;
  if Assigned(FOnGetTipsRect) then FOnGetTipsRect(Self, Cell, Result);
end;

function TCustomGridView.GetTipsText(Cell: TGridCell): string;
begin
  Result := GetCellText(Cell);
  if Assigned(FOnGetTipsText) then FOnGetTipsText(Self, Cell, Result); 
end;

function TCustomGridView.GetTipsWindowClass: TGridTipsWindowClass;
begin
  Result := TGridTipsWindow;
end;

procedure TCustomGridView.HeaderClick(Section: TGridHeaderSection);
begin
  if Assigned(FOnHeaderClick) then FOnHeaderClick(Self, Section);
end;

procedure TCustomGridView.HeaderClicking(Section: TGridHeaderSection; var AllowClick: Boolean);
begin
  { by default, only the bottom sections can be pressed }
  AllowClick := ColumnClick and Section.AllowClick and (Section.Sections.Count = 0);
  if Assigned(FOnHeaderClicking) then FOnHeaderClicking(Self, Section, AllowClick);
end;

procedure TCustomGridView.HeaderMenuClick(Sender: TObject);
var
  C: Integer;
begin
  if Sender is TComponent then
  begin
    C := TComponent(Sender).Tag;
    if (C >= 0) and (C < Columns.Count) then
      Columns[C].Visible := not Columns[C].Visible
    else if Assigned(FOnHeaderDetailsClick) then
      FOnHeaderDetailsClick(Self);
  end;
end;

procedure TCustomGridView.HideCursor;
begin
  if IsFocusAllowed then InvalidateFocus else HideEdit;
end;

procedure TCustomGridView.HideEdit;
begin
  if FEdit <> nil then
  begin
    FEditCell := GridCell(-1, -1);
    FEdit.Hide;
  end;
end;

procedure TCustomGridView.HideFocus;
begin
  if IsFocusAllowed then PaintFocus;
end;

procedure TCustomGridView.KeyDown(var Key: Word; Shift: TShiftState);
const
  HomeOffsets: array[Boolean] of TGridCursorOffset = (goHome, goGridHome);
  EndOffsets: array[Boolean] of TGridCursorOffset = (goEnd, goGridEnd);
  TabOffsets: array[Boolean] of TGridCursorOffset = (goNext, goPrev);
var
  Cell: TGridCell;
begin
  inherited KeyDown(Key, Shift);
  if gkArrows in CursorKeys then
    case Key of
      VK_LEFT:
        begin
           SetCursor(GetCursorCell(CellFocused, goLeft), True, True);
          { scroll grid left if selection is not allowed or whole row
            selection enabled }
          if RowSelect then
            with HorzScrollBar do SetPosition(Position - LineStep);
        end;
      VK_RIGHT:
        begin
          SetCursor(GetCursorCell(CellFocused, goRight), True, True);
          { scroll grid right if selection is not allowed or whole row
            selection enabled }
          if RowSelect then
            with HorzScrollBar do SetPosition(Position + LineStep);
        end;
      VK_UP:
        begin
          { scroll grid up if selection is not allowed }
          if not AllowSelect then Cell := VisOrigin else Cell := CellFocused;
          SetCursor(GetCursorCell(Cell, goUp), True, True);
        end;
      VK_DOWN:
        begin
          { scroll grid down if selection is not allowed }
          if not AllowSelect then
          begin
            Cell := GridCell(VisOrigin.Col, VisOrigin.Row + VisSize.Row - 1);
            if not IsCellVisible(Cell, False) then Dec(Cell.Row);
          end
          else
            Cell := CellFocused;
          SetCursor(GetCursorCell(Cell, goDown), True, True)
        end;
      VK_PRIOR:
        begin
          Cell := GetCursorCell(CellFocused, goPageUp);
          SetCursor(Cell, True, True);
        end;
      VK_NEXT:
        begin
          Cell := GetCursorCell(CellFocused, goPageDown);
          SetCursor(Cell, True, True);
        end;
      VK_HOME:
        begin
          Cell := GetCursorCell(CellFocused, HomeOffsets[ssCtrl in Shift]);
          SetCursor(Cell, True, True);
        end;
      VK_END:
        begin
          Cell := GetCursorCell(CellFocused, EndOffsets[ssCtrl in Shift]);
          SetCursor(Cell, True, True);
        end;
    end;
  if (gkTabs in CursorKeys) and (Key = VK_TAB) then
  begin
    SetCursor(GetCursorCell(CellFocused, TabOffsets[ssShift in Shift]), True, True);
  end;
  case Key of
    VK_SPACE:
      { if row selection is enabled then click check box of the first column }
      if CheckBoxes and (not EditCanShow(CellFocused) or (ssCtrl in Shift)) then
      begin
        Cell := CellFocused;
        if RowSelect then Cell.Col := Fixed.Count;
        if GetCheckKind(Cell) <> gcNone then
        begin
          SetCursor(Cell, True, True);
          CheckClick(Cell);
        end;
      end;
    VK_F2:
      Editing := True;
    VK_ADD:
      if Shift >= [ssCtrl, ssShift] then
      begin
        SizeAllColumnsToFit;
        Key := 0;
      end;
  end;
end;

procedure TCustomGridView.KeyPress(var Key: Char);
begin
  inherited KeyPress(Key);
  { ENTER: stop or start editing }
  if (Key = #13) and CellSelected then
  begin
    Key := #0;
    if Editing then
    begin
      ApplyEdit;
      if gkReturn in CursorKeys then
        SetCursor(GetCursorCell(CellFocused, goNext), True, True);
    end
    else if not AlwaysEdit then
    begin
      SetCursor(CellFocused, True, True); {?}
      Editing := True;
    end;
  end;
  { ESCAPE: stop editing }
  if Key = #27 then
  begin
    Key := #0;
    if Editing then
      if not AlwaysEdit then
        CancelEdit
      else
        ResetEdit;
  end;
end;

procedure TCustomGridView.Loaded;
begin
  inherited Loaded;
  UpdateFixed;
  UpdateHeader;
  UpdateRows;
  UpdateColors;
  UpdateFonts;
  UpdateEdit(AlwaysEdit);
  FCellSelected := AlwaysSelected;
  UpdateCursor;
end;

procedure TCustomGridView.MouseDown(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
var
  S: TGridHeaderSection;
  C, P: TGridCell;
  AllowClicking: Boolean;
begin
  KillTimer(Handle, 1);
  if not AcquireFocus then
  begin
    MouseCapture := False;
    Exit;
  end;
  { check clicking on the header }
  if Button = mbLeft then
    if ShowHeader and PtInRect(GetHeaderRect, Point(X, Y)) then
    begin
      S := GetResizeSectionAt(X, Y);
      if S <> nil then
      begin
        if ssDouble in Shift then
          SizeColumnToFit(S.ResizeColumnIndex)
        else
          StartColResize(S, X, Y);
      end
      else if not (ssDouble in Shift) then
      begin
        S := GetSectionAt(X, Y);
        if S <> nil then
        begin
          if not Header.Flat then
          begin
            AllowClicking := True;
            { user can prevent the header from clicking by using the
              OnHeaderClick event }
            HeaderClicking(S, AllowClicking);
          end
          else
            AllowClicking := False;
          if AllowClicking then
            if Header.Flat then HeaderClick(S)
            else StartHeaderClick(S, X, Y);
        end;
      end
      else
      begin
        { there is not OnHeaderDblCkick event yet }
      end;
      Exit;
    end;
  { select new cell by mouse }
  if (Button = mbLeft) or ((Button = mbRight) and RightClickSelect) then
    if (gkMouse in CursorKeys) and (PtInRect(GetGridRect, Point(X, Y))) then
    begin
      C := GetCellAt(X, Y);
      { reset position of pending editing }
      P := FClickPos;
      ResetClickPos;
      { select cell or clear selection }
      if IsCellEmpty(C) then
      begin
        Editing := False;
        SetCursor(CellFocused, False, False);
      end
      else
      begin
        SetCursor(C, True, True);
        CellClick(C, Shift, X, Y);
        { check clicking on the check box }
        if PtInRect(GetCheckRect(C), Point(X, Y)) then
        begin
          CheckClick(C);
          Exit;
        end;
        { start editing }
        if (Button = mbLeft) and IsCellEqual(C, CellFocused) and AllowEdit then
          { editing by repeated single click on the same cell is controlled
            with a timer, so as not to confuse the double click and single
            click }
          if (Shift * [ssCtrl, ssShift, ssDouble] = []) and IsCellEqual(C, P) then
            FEditPending := True;
      end;
      { save the position for pending editing }
      FClickPos := C;
    end;
  { abort actions }
  if Button = mbRight then
  begin
    if FColResizing then
    begin
      StopColResize(True);
      Exit;
    end;
    if FHeaderClicking then
    begin
      StopHeaderClick(True);
      Exit;
    end;
  end;
  inherited MouseDown(Button, Shift, X, Y);
end;

procedure TCustomGridView.MouseMove(Shift: TShiftState; X, Y: Integer);
var
  C: TGridCell;
  S: TGridHeaderSection;
  AllowClicking: Boolean;
begin
  { highlight "hot" header section }
  if StyleServices.Enabled and ShowHeader and (not Header.Flat) then
  begin
    S := nil;
    if FColResizing then S := FColResizeSection
    else if PtInRect(GetHeaderRect, Point(X, Y)) then
    begin
      S := GetResizeSectionAt(X, Y);
      if S = nil then S := GetSectionAt(X, Y);
    end;
    if S <> nil then
    begin
      AllowClicking := True;
      HeaderClicking(S, AllowClicking);
      if not AllowClicking then S := nil;
    end;
    if FHotSection <> S then
    begin
      { the user can delete section at runtime, and the FHotSection pointer
        will become invalid, so you need to use a column and a section level
        instead of FHotSection to redraw }
      if FHotSection <> nil then
        InvalidateSection(FHotColumn, FHotLevel);
      FHotSection := S;
      if FHotSection <> nil then
      begin
        FHotColumn := FHotSection.ColumnIndex;
        FHotLevel := FHotSection.Level;
        InvalidateSection(FHotColumn, FHotLevel);
      end;
    end;
  end;
  if FColResizing then
  begin
    StepColResize(X, Y);
    Exit;
  end;
  if FHeaderClicking then
  begin
    StepHeaderClick(X, Y);
    Exit;
  end;
  if (ssLeft in Shift) or ((ssRight in Shift) and RightClickSelect) then
    if gkMouseMove in CursorKeys then
    begin
      C := GetCellAt(X, Y);
      if (not IsCellEmpty(C)) and (not IsCellEqual(C, CellFocused)) then
      begin
        SetCursor(C, True, True);
        if IsCellEqual(C, CellFocused) and AlwaysEdit then
        begin
          Editing := True;
          if Editing then Exit;
        end;
      end;
    end;
  { abort pending editing on mouse move }
  if FEditPending then
  begin
    FEditPending := False;
  end;
  inherited MouseMove(Shift, X, Y);
end;

procedure TCustomGridView.MouseUp(Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  if FColResizing then
  begin
    StopColResize(False);
    Exit;
  end;
  if FHeaderClicking then
  begin
    StopHeaderClick(False);
    Exit;
  end;
  { delayed start editing }
  if FEditPending and IsCellEqual(FClickPos, CellFocused) then
  begin
    FEditPending := False;
    if SetTimer(Handle, 1, GetDoubleClickTime, nil) = 0 then Editing := True;
  end;
  inherited MouseUp(Button, Shift, X, Y);
end;

procedure TCustomGridView.Notification(AComponent: TComponent; Operation: TOperation);
begin
  inherited Notification(AComponent, Operation);
  if Operation = opRemove then
  begin
    if AComponent = FImages then FImages := nil;
    if Header <> nil then
    begin
      if AComponent = Header.FImages then Header.FImages := nil;
      if AComponent = Header.FPopupMenu then Header.FPopupMenu := nil;
    end;
    if AComponent = FEdit then
    begin
      FEdit := nil;
      FEditCell := GridCell(-1, -1);
      FEditing := False;
    end;
    if AComponent = FHeaderPopupMenu then FHeaderPopupMenu := nil;
    if AComponent = FFindDialog then FFindDialog := nil;
  end;
end;

procedure TCustomGridView.Paint;
var
  DefDraw: Boolean;
  R: TRect;
  S: string;
begin
  { user draw }
  DefDraw := True;
  try
    if Assigned(FOnDraw) then FOnDraw(Self, DefDraw);
  except
    Application.HandleException(Self);
  end;
  if not DefDraw then Exit;
  { draw header }
  if ShowHeader and RectVisible(Canvas.Handle, GetHeaderRect) then
  begin
    PaintHeaders(True);
    R := GetHeaderRect;
    R.Right := GetFixedRect.Right;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
    PaintHeaders(False);
    R := GetHeaderRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  { field to the right and bottom of the cells }
  PaintFreeField;
  { fixed cells }
  if (Fixed.Count > 0) and RectVisible(Canvas.Handle, GetFixedRect) then
  begin
    PaintFixed;
    if GridLines then PaintFixedGridLines;
    R := GetFixedRect;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
  end;
  { cells }
  if (VisSize.Col > 0) and (VisSize.Row > 0) then
  begin
//  if Editing then
//  begin
//    R := GetEditRect(EditCell);
//    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R.Bottom);
//  end;
    PaintCells;
    if IsFocusAllowed then PaintFocus;
  end;
  { grid lines over the cells }
  if GridLines then PaintGridLines;
  { hint for empty grid }
  if IsGridHintVisible then
  begin
    R := GetGridRect;
    R.Left := GetColumnLeftRight(Fixed.Count).Left;
    S := GetGridHint;
    Canvas.Brush.Style := bsClear;
    Canvas.Font.Color := GridHintColor;
    Canvas.TextOut(R.Left + 3, R.Top + 4, S);
  end;
  { column resizing line }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

function GetRGBColor(Value: TColor): DWORD;
begin
  Result := ColorToRGB(Value);
  case Result of
    clNone: Result := CLR_NONE;
    clDefault: Result := CLR_DEFAULT;
  end;
end;

procedure TCustomGridView.Paint3DFrame(Rect: TRect; SideFlags: Longint);
begin
  with Canvas do
  begin
    DrawEdge(Handle, Rect, BDR_RAISEDOUTER, SideFlags and (not BF_TOPLEFT));
    if SideFlags and BF_BOTTOM <> 0 then Dec(Rect.Bottom);
    if SideFlags and BF_RIGHT <> 0 then Dec(Rect.Right);
    DrawEdge(Handle, Rect, BDR_RAISEDINNER, SideFlags and (not BF_BOTTOMRIGHT));
    if SideFlags and BF_TOP <> 0 then Inc(Rect.Top);
    if SideFlags and BF_LEFT <> 0 then Inc(Rect.Left);
    DrawEdge(Handle, Rect, BDR_RAISEDINNER, SideFlags and (not BF_TOPLEFT));
    if SideFlags and BF_BOTTOM <> 0 then Dec(Rect.Bottom);
    if SideFlags and BF_RIGHT <> 0 then Dec(Rect.Right);
    DrawEdge(Handle, Rect, BDR_RAISEDOUTER, SideFlags and (not BF_BOTTOMRIGHT));
  end;
end;

procedure TCustomGridView.PaintCell(Cell: TGridCell; Rect: TRect);
var
  DefDraw: Boolean;
begin
  GetCellColors(Cell, Canvas);
  { user draw cells }
  DefDraw := True;
  try
    if Assigned(FOnDrawCell) then FOnDrawCell(Self, Cell, Rect, DefDraw);
  except
    Application.HandleException(Self);
  end;
  { default draw }
  if DefDraw then DefaultDrawCell(Cell, Rect);
end;

procedure TCustomGridView.PaintCells;
var
  I, J: Integer;
  L, T, W: Integer;
  R: TRect;
  C: TGridCell;
begin
  L := GetColumnLeftRight(VisOrigin.Col).Left;
  T := GetRowTopBottom(VisOrigin.Row).Top;
  R.Bottom := T;
  for J := 0 to FVisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    R.Right := L;
    for I := 0 to FVisSize.Col - 1 do
    begin
      C := GridCell(VisOrigin.Col + I, VisOrigin.Row + J);
      W := Columns[C.Col].Width;
      { hidden columns have zero width }
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then PaintCell(C, R);
      end;
    end;
  end;
end;

procedure TCustomGridView.PaintCheck(Rect: TRect; CheckKind: TGridCheckKind;
  CheckState: TCheckBoxState; CheckEnabled: Boolean);
var
  CR: TRect;
  DrawState: UINT;
  Detail: TThemedButton;
begin
  with Canvas do
  begin
    CR := Rect;
    InflateRect(CR, -1, -1); Inc(CR.Left); Inc(CR.Top);
    if not StyleServices.Enabled then
    begin
      if CheckKind = gcCheckBox then
      begin
        DrawState := DFCS_BUTTONCHECK;
        case CheckState of
          cbChecked: DrawState := DrawState or DFCS_CHECKED;
          cbGrayed: DrawState := DrawState or DFCS_CHECKED or DFCS_BUTTON3STATE;
        end;
      end
      else
      begin
        DrawState := DFCS_BUTTONRADIO;
        if CheckState <> cbUnchecked then DrawState := DrawState or DFCS_CHECKED;
      end;
      if not CheckEnabled then DrawState := DrawState or DFCS_INACTIVE;
      if CheckStyle <> cs3D then DrawState := DrawState or DFCS_FLAT;
      DrawFrameControl(Handle, CR, DFC_BUTTON, DrawState);
    end
    else
    begin
      if CheckKind = gcCheckBox then
        if CheckEnabled then
          case CheckState of
            cbChecked: Detail := tbCheckBoxCheckedNormal;
            cbGrayed: Detail := tbCheckBoxMixedNormal;
          else
            Detail := tbCheckBoxUncheckedNormal;
          end
        else
          case CheckState of
            cbChecked: Detail := tbCheckBoxCheckedDisabled;
            cbGrayed: Detail := tbCheckBoxMixedDisabled;
          else
            Detail := tbCheckBoxUncheckedDisabled;
          end
      else
        if CheckEnabled then
          case CheckState of
            cbChecked: Detail := tbRadioButtonCheckedNormal;
            cbGrayed: Detail := tbRadioButtonCheckedNormal;
          else
            Detail := tbRadioButtonUncheckedNormal;
          end
        else
          case CheckState of
            cbChecked: Detail := tbRadioButtonCheckedDisabled;
            cbGrayed: Detail := tbRadioButtonCheckedDisabled;
          else
            Detail := tbRadioButtonUncheckedDisabled;
          end;
      with StyleServices do
        DrawElement(Handle, GetElementDetails(Detail), CR);
    end
  end
end;

procedure TCustomGridView.PaintDotGridLines(Points: Pointer; Count: Integer);
type
  TIntArray = array of Integer;
  PIntArray = ^TIntArray;
var
  P: PIntArray absolute Points;
  I: Integer;
  R: TRect;
begin
  PreparePatternBitmap(Canvas, GetGridLineColor(Color), False);
  try
    I := 0;
    while I < Count * 2 do
    begin
      R.Left := P^[I];
      Inc(I);
      R.Top := P^[I];
      Inc(I);
      R.Right := P^[I];
      Inc(I);
      R.Bottom := P^[I];
      Inc(I);
      { FillRect will not work if the width or height of the rectangle is zero }
      if (R.Left = R.Right) and (R.Top <> R.Bottom) then Inc(R.Right)
      else if (R.Left <> R.Right) and (R.Top = R.Bottom) then Inc(R.Bottom);
      Canvas.FillRect(R);
    end;
  finally
    PreparePatternBitmap(Canvas, GetGridLineColor(Color), True);
  end;
end;

procedure TCustomGridView.PaintFixed;
var
  I, J, W, Y: Integer;
  R: TRect;
  C: TGridCell;
begin
  R.Bottom := GetRowTopBottom(VisOrigin.Row).Top;
  for J := 0 to FVisSize.Row - 1 do
  begin
    R.Top := R.Bottom;
    R.Bottom := R.Bottom + Rows.Height;
    R.Right := GetGridRect.Left;
    for I := 0 to Fixed.Count - 1 do
    begin
      C := GridCell(I, VisOrigin.Row + J);
      W := Columns[C.Col].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then PaintCell(C, R);
      end;
    end;
  end;
  { vertical separator line on the right }
  if (Fixed.Flat or StyleServices.Enabled) and
    (Fixed.ShowDivider or (gsFullVertLine in GridStyle)) then
  begin
    R := GetFixedRect;
    { if the colors of fixed cells and regular cells are the same - draw a
      separator from one line, otherwise draw a double line }
    if Fixed.GridColor or StyleServices.Enabled then
    begin
      if not (gsDotLines in GridStyle) then
      begin
        Canvas.Pen.Color := GetFixedDividerColor;
        Canvas.Pen.Width := FGridLineWidth;
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
    begin
      Y := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
      with Canvas do
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Right - 2, R.Top - 1);
        LineTo(R.Right - 2, Y);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Right - 1, Y - 1);
        LineTo(R.Right - 1, R.Top - 1);
        Pen.Color := GetGridLineColor(Color);
        MoveTo(R.Right - 2, Y);
        LineTo(R.Right - 2, R.Bottom);
      end;
    end;
  end;
  { column resizing line }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintFixedGridLines;
var
  Points: array of TPoint;
  PointCount: Integer;
  StrokeList: array of DWORD;
  StrokeCount: Integer;
  I, L, R, T, B, X, Y, C, W: Integer;
  Rect: TRect;

  procedure ShiftGridPoints(DX, DY: Integer);
  var
    I: Integer;
  begin
    I := 0;
    while I < (Fixed.Count - 1) * Ord(gsVertLine in GridStyle) * 2 do
    begin
      Points[I].X := Points[I].X + DX;
      Inc(I);
    end;
    while I < PointCount do
    begin
      Points[I].Y := Points[I].Y + DY;
      Inc(I);
    end;
  end;

  procedure Paint3DCells(Rect: TRect);
  var
    I: Integer;
    R: TRect;
  begin
    R := Rect;
    R.Bottom := R.Top;
    while R.Bottom < Rect.Bottom do
    begin
      R.Top := R.Bottom;
      R.Bottom := R.Bottom + Rows.Height;
      R.Right := GetFixedRect.Left;
      for I := 0 to Fixed.Count - 1 do
      begin
        W := Columns[I].Width;
        if W > 0 then
        begin
          R.Left := R.Right;
          R.Right := R.Right + W;
          if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, BF_RECT);
        end;
      end;
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

  procedure PaintVert3DLines(Rect: TRect; DrawBottomLine: Boolean);
  const
    Flags: array[Boolean] of Longint = (BF_TOPLEFT or BF_RIGHT, BF_RECT);
  var
    I: Integer;
    R: TRect;
  begin
    R := Rect;
    R.Right := R.Left;
    for I := 0 to Fixed.Count - 1 do
    begin
      W := Columns[I].Width;
      if W > 0 then
      begin
        R.Left := R.Right;
        R.Right := R.Right + W;
        if RectVisible(Canvas.Handle, R) then Paint3DFrame(R, Flags[DrawBottomLine]);
      end;
    end
  end;

  procedure PaintBottom3DMargin(Rect: TRect);
  begin
    if RectVisible(Canvas.Handle, Rect) then
      Paint3DFrame(Rect, BF_LEFT or BF_TOP or BF_RIGHT);
  end;

begin
  if StyleServices.Enabled or Fixed.Flat then
  begin
    { the number of grid lines is equal to the number of visible rows plus
      one line to the right of each fixed column }
    StrokeCount := 0;
    if gsHorzLine in GridStyle then
    begin
      if gsListViewLike in GridStyle then StrokeCount := GetGridHeight div Rows.Height
      else StrokeCount := VisSize.Row;
    end;
    if gsVertLine in GridStyle then
      StrokeCount := StrokeCount + Fixed.Count;
    if StrokeCount > 0 then
    begin
      { malloc two points on each line }
      SetLength(Points, StrokeCount * 2);
      SetLength(StrokeList, StrokeCount);
      for I := 0 to StrokeCount - 1 do StrokeList[I] := 2;
      { fill the points of vertical lines }
      Rect := GetFixedRect;
      PointCount := 0;
      if gsVertLine in GridStyle then
      begin
        T := Rect.Top;
        B := Rect.Bottom;
        if [gsFullVertLine, gsListViewLike] * GridStyle = [] then
          B := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
        X := Rect.Left;
        for I := 0 to Fixed.Count - 2 do
        begin
          X := X + Columns[I].Width;
          Points[PointCount].X := X - 2;
          Points[PointCount].Y := T;
          Inc(PointCount);
          Points[PointCount].X := X - 2;
          Points[PointCount].Y := B;
          Inc(PointCount);
        end;
      end;
      { fill the points of horisontal lines }
      if gsHorzLine in GridStyle then
      begin
        L := Rect.Left;
        R := Rect.Right;
        Y := GetRowTopBottom(VisOrigin.Row).Top;
        C := FVisSize.Row;
        if gsListViewLike in GridStyle then C := GetGridHeight div Rows.Height;
        for I := 0 to C - 1 do
        begin
          Y := Y + Rows.Height;
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
        ShiftGridPoints(1, 0);
        if not (gsDotLines in GridStyle) then
        begin
          Canvas.Pen.Color := GetFixedGridColor;
          Canvas.Pen.Width := FGridLineWidth;
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
  else if (gsHorzLine in GridStyle) and (gsVertLine in GridStyle) then
  begin
    { all cells are 3D }
    Rect := GetFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := Rect.Top + FVisSize.Row * Rows.Height;
    Paint3DCells(Rect);
  end
  else if (gsHorzLine in GridStyle) and (not (gsVertLine in GridStyle)) then
  begin
    { only horizontal 3D lines }
    Rect := GetFixedRect;
    if not (gsListViewLike in GridStyle) then
      Rect.Bottom := Rect.Top + FVisSize.Row * Rows.Height;
    PaintHorz3DLines(Rect);
    if not (gsListViewLike in GridStyle) then
    begin
      Rect.Top := Rect.Bottom;
      Rect.Bottom := GetFixedRect.Bottom;
      PaintBottom3DMargin(Rect);
    end;
  end
  else if (not (gsHorzLine in GridStyle)) and (gsVertLine in GridStyle) then
  begin
    { only vertical 3D lines }
    Rect := GetFixedRect;
    PaintVert3DLines(Rect, False);
  end
  else
  begin
    { no 3D lines }
    Rect := GetFixedRect;
    PaintBottom3DMargin(Rect);
  end;
end;

procedure TCustomGridView.PaintFocus;
var
  R, R2: TRect;
begin
  if ShowFocusRect and Focused and (VisSize.Row > 0) and (not Editing) and
    (UpdateLock = 0) then
  begin
    { clip the header and fixed cells (these are all around the cells to
      allow for the indicator from TDBGridView) }
    R := ClientRect;
    R2 := GetGridRect;
    R2.Left := R2.Left + GetFixedWidth;
    ExcludeClipRect(Canvas.Handle, R.Left, R.Top, R.Right, R2.Top);
    ExcludeClipRect(Canvas.Handle, R.Left, R2.Top, R2.Left, R2.Bottom);
    ExcludeClipRect(Canvas.Handle, R.Left, R2.Bottom, R.Right, R.Bottom);
    ExcludeClipRect(Canvas.Handle, R2.Right, R2.Top, R.Right, R2.Bottom);
    { focus rectangle should not include grid lines }
    R := GetFocusRect;
    if GridLines then
    begin
      if gsVertLine in GridStyle then Dec(R.Right, FGridLineWidth);
      if gsHorzLine in GridStyle then Dec(R.Bottom, FGridLineWidth);
    end;
    { focus by default is displayed as a dotted rectangle }
    with Canvas do
    begin
      SetTextColor(Handle, ColorToRGB(clWhite));
      SetBkColor(Handle, ColorToRGB(clBlack));
      SetBkMode(Handle, OPAQUE);
      SetRop2(Handle, R2_COPYPEN);
      DrawFocusRect(R);
    end;
  end;
end;

procedure TCustomGridView.PaintFreeField;
var
  X, Y: Integer;
  R: TRect;
begin
  { field to the right of the cells }
  X := GetColumnLeftRight(VisOrigin.Col + VisSize.Col).Left;
  R := GetGridRect;
  if X < R.Right then
  begin
    R.Left := X;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
  end;
  { field to the bottom of cells }
  Y := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
  R := GetGridRect;
  if Y < R.Bottom then
  begin
    R.Left := GetFixedRect.Right;
    R.Top := Y;
    Canvas.Brush.Color := Color;
    Canvas.FillRect(R);
    { field below fixed cells }
    R.Right := R.Left;
    R.Left := GetFixedRect.Left;
    Inc(R.Bottom, 2);
    if (gsListViewLike in GridStyle) then Canvas.Brush.Color := Fixed.Color;
    Canvas.FillRect(R);
  end;
  { column resizing line }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintGridLines;
var
  Points: array of TPoint;
  PointCount: Integer;
  StrokeList: array of DWORD;
  StrokeCount: Integer;
  I: Integer;
  L, R, T, B, X, Y, C: Integer;
  Rect: TRect;
begin
  { the number of grid lines is equal to the number of visible rows plus
    one line to the right of each fixed column }
  StrokeCount := 0;
  if gsHorzLine in GridStyle then
  begin
    if gsListViewLike in GridStyle then StrokeCount := GetGridHeight div Rows.Height
    else StrokeCount := VisSize.Row;
  end;
  if gsVertLine in GridStyle then
    StrokeCount := StrokeCount + VisSize.Col;
  if StrokeCount > 0 then
  begin
    { malloc two points on each line }
    SetLength(Points, StrokeCount * 2);
    SetLength(StrokeList, StrokeCount);
    for I := 0 to StrokeCount - 1 do StrokeList[I] := 2;
    { fill the points of vertical lines }
    Rect := GetGridRect;
    PointCount := 0;
    if gsVertLine in GridStyle then
    begin
      T := Rect.Top;
      B := Rect.Bottom;
      if [gsFullVertLine, gsListViewLike] * GridStyle = [] then
        B := GetRowTopBottom(VisOrigin.Row + VisSize.Row).Top;
      X := GetColumnLeftRight(VisOrigin.Col).Left;
      for I := 0 to VisSize.Col - 1 do
      begin
        X := X + Columns[VisOrigin.Col + I].Width;
        Points[PointCount].X := X - 1;
        Points[PointCount].Y := T;
        Inc(PointCount);
        Points[PointCount].X := X - 1;
        Points[PointCount].Y := B;
        Inc(PointCount);
      end;
    end;
    { fill the points of horisontal lines }
    if gsHorzLine in GridStyle then
    begin
      L := Rect.Left + GetFixedWidth;
      R := Rect.Right;
      if [gsFullHorzLine, gsListViewLike] * GridStyle = [] then
        R := GetColumnLeftRight(VisOrigin.Col + VisSize.Col).Left;
      Y := GetRowTopBottom(VisOrigin.Row).Top;
      C := VisSize.Row;
      if gsListViewLike in GridStyle then C := GetGridHeight div Rows.Height;
      for I := 0 to C - 1 do
      begin
        Y := Y + Rows.Height;
        Points[PointCount].X := L;
        Points[PointCount].Y := Y - 1;
        Inc(PointCount);
        Points[PointCount].X := R;
        Points[PointCount].Y := Y - 1;
        Inc(PointCount);
      end;
    end;
    if not (gsDotLines in GridStyle) then
    begin
      Canvas.Pen.Color := GetGridLineColor(Color);
      Canvas.Pen.Width := FGridLineWidth;
      PolyPolyLine(Canvas.Handle, Pointer(Points)^, Pointer(StrokeList)^, PointCount shr 1);
    end
    else
      PaintDotGridLines(Pointer(Points), PointCount);
  end;
end;

procedure TCustomGridView.PaintHeader(Section: TGridHeaderSection; Rect: TRect);
var
  DefDraw: Boolean;
begin
  GetHeaderColors(Section, Canvas);
  { user draw header }
  DefDraw := True;
  try
    if Assigned(FOnDrawHeader) then FOnDrawHeader(Self, Section, Rect, DefDraw);
  except
    Application.HandleException(Self);
  end;
  { default draw }
  if DefDraw then DefaultDrawHeader(Section, Rect);
end;

procedure TCustomGridView.PaintHeaderBackground(Rect: TRect; Color: TColor;
  PaintState: TGridPaintStates);
var
  Detail: TThemedHeader;
  Details: TThemedElementDetails;
begin
  with Canvas do
    if not StyleServices.Enabled then
    begin
      Brush.Color := Color;
      FillRect(Rect);
      if psFlat in PaintState then
      begin
        Pen.Width := 1;
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Left, Rect.Bottom - 2);
        LineTo(Rect.Right - 1, Rect.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Left, Rect.Bottom - 1);
        LineTo(Rect.Right - 1, Rect.Bottom - 1);
        Pen.Color := clBtnShadow;
        MoveTo(Rect.Right - 2, Rect.Top);
        LineTo(Rect.Right - 2, Rect.Bottom - 1);
        Pen.Color := clBtnHighlight;
        MoveTo(Rect.Right - 1, Rect.Top);
        LineTo(Rect.Right - 1, Rect.Bottom);
      end
      else if psPressed in PaintState then
        DrawEdge(Handle, Rect, BDR_SUNKENOUTER, BF_RECT or BF_FLAT)
      else
        Paint3DFrame(Rect, BF_RECT);
    end
    else
    begin
      if (psSorted in PaintState) and CheckWin32Version(6, 0) then
      begin
        Details := StyleServices.GetElementDetails(thHeaderItemNormal);
        if psPressed in PaintState then Details.State := HIS_SORTEDPRESSED
        else if psHot in PaintState then Details.State := HIS_SORTEDHOT
        else Details.State := HIS_SORTEDNORMAL;
      end
      else
      begin
        if psPressed in PaintState then Detail := thHeaderItemPressed
        else if psHot in PaintState then Detail := thHeaderItemHot
        else if psDontCare in PaintState then Detail := thHeaderDontCare
        else Detail := thHeaderItemNormal;
        Details := StyleServices.GetElementDetails(Detail);
      end;
      StyleServices.DrawElement(Handle, Details, Rect);
    end;
end;

procedure TCustomGridView.PaintHeaders(DrawFixed: Boolean);
const
  PaintState: array[Boolean] of TGridPaintStates = ([psDontCare], [psFlat, psDontCare]);
var
  R: TRect;
begin
  PaintHeaderSections(Header.Sections, DrawFixed);
  { field to the right of the header }
  R := GetHeaderRect;
  R.Left := GetGridRect.Left + Header.Width + GetGridOrigin.X;
  if R.Left < R.Right then
  begin
    Dec(R.Left, Ord(StyleServices.Enabled)); // <- double-line artifact with themes enabled
    Inc(R.Right, 2);
    PaintHeaderBackground(R, Header.Color, PaintState[Header.Flat]);
  end;
  { gray line at the bottom of the header }
  if Header.Flat and (not StyleServices.Enabled) then
  begin
    if DrawFixed then
    begin
      R.Left := GetFixedRect.Left;
      R.Right := GetFixedRect.Right;
    end
    else
    begin
      R.Left := GetFixedRect.Right;
      R.Right := GetGridRect.Right;
    end;
    with Canvas do
      if Header.GridColor then
      begin
        Pen.Color := GetGridLineColor(Color);
        Pen.Width := FGridLineWidth;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end
      else
      begin
        Pen.Color := clBtnShadow;
        Pen.Width := 1;
        MoveTo(R.Left, R.Bottom - 2);
        LineTo(R.Right, R.Bottom - 2);
        Pen.Color := clBtnHighlight;
        MoveTo(R.Left, R.Bottom - 1);
        LineTo(R.Right, R.Bottom - 1);
      end;
  end;
  { column resizing line }
  if FColResizing and (FColResizeCount > 0) and not FColumnsFullDrag then
    PaintResizeLine;
end;

procedure TCustomGridView.PaintHeaderSections(Sections: TGridHeaderSections; DrawFixed: Boolean);
var
  I: Integer;
  S: TGridHeaderSection;
  R, SR: TRect;
begin
  for I := 0 to Sections.Count - 1 do
  begin
    S := Sections[I];
    { the header sections are drawn in two stages: the sections of fixed cells
      and the section of regular cells }
    if DrawFixed = S.FixedColumn then
    begin
      R := S.BoundsRect;
      if R.Right > R.Left then
      begin
        SR := R;
        if S.Sections.Count > 0 then SR.Bottom := GetHeaderRect.Bottom;
        { to speed up redrawing, skip sections that do not need to be redrawn }
        if RectVisible(Canvas.Handle, SR) then
        begin
          PaintHeader(S, R);
          PaintHeaderSections(S.Sections, DrawFixed);
        end;
      end;
    end
    else
      { multi-level header can have both fixed and non-fixed sections (although
        this is wrong), so let's try drawing them too }
      PaintHeaderSections(S.Sections, DrawFixed);
  end;
end;

procedure TCustomGridView.PaintResizeLine;
var
  OldPen: TPen;
begin
  OldPen := TPen.Create;
  try
    with Canvas do
    begin
      OldPen.Assign(Pen);
      try
        Pen.Color := clWhite;
        Pen.Style := psSolid;
        Pen.Mode := pmXor;
        Pen.Width := 1;
        with FColResizeRect do
        begin
          MoveTo(FColResizePos, Top);
          LineTo(FColResizePos, Bottom);
        end;
      finally
        Pen := OldPen;
      end;
    end;
  finally
    OldPen.Free;
  end;
end;

procedure TCustomGridView.PaintText(Canvas: TCanvas; Rect: TRect; LeftIndent, TopIndent: Integer;
  Alignment: TAlignment; WantReturns, WordWrap: Boolean; const Text: string);
var
  P: TDrawTextParams;
  F, DX: Integer;
  A: UINT;
begin
  { draw text using DrawTextEx for multiline text, draw text using TextOut
    for single line text }
  if WantReturns or WordWrap or EndEllipsis then
  begin
    FillChar(P, SizeOf(P), 0);
    P.cbSize := SizeOf(P);
    P.iLeftMargin := LeftIndent;
    P.iRightMargin := TextRightIndent;
    F := DT_NOPREFIX;
    case Alignment of
      taLeftJustify: F := F or DT_LEFT;
      taCenter: F := F or DT_CENTER;
      taRightJustify: F := F or DT_RIGHT;
    end;
    if not (WantReturns or WordWrap) then
    begin
      F := F or DT_SINGLELINE;
      if Alignment = taLeftJustify then F := F or DT_END_ELLIPSIS
    end;
    if WordWrap then F := F or DT_WORDBREAK;
    Inc(Rect.Top, TopIndent);
    with Canvas do
    begin
      SetBkMode(Handle, TRANSPARENT);
      DrawTextEx(Handle, PChar(Text), Length(Text), Rect, F, @P);
    end;
  end
  else
  begin
    case Alignment of
      taCenter:
        begin
          DX := LeftIndent + (Rect.Right - Rect.Left) div 2;
          A := TA_CENTER;
        end;
      taRightJustify:
        begin
          DX := (Rect.Right - Rect.Left) - TextRightIndent;
          A := TA_RIGHT;
        end;
    else
      DX := LeftIndent;
      A := TA_LEFT;
    end;
    with Canvas do
    begin
      SetBkMode(Handle, TRANSPARENT);
      SetTextAlign(Handle, A);
      ExtTextOut(Handle, Rect.Left + DX, Rect.Top + TopIndent, ETO_CLIPPED, @Rect, PChar(Text), Length(Text), nil);
      SetTextAlign(Handle, TA_LEFT);
    end;
  end;
end;

procedure TCustomGridView.PreparePatternBitmap(Canvas: TCanvas; FillColor: TColor; Remove: Boolean);
begin
  if Remove then
  begin
    if Canvas.Brush.Bitmap = nil then Exit;
    Canvas.Brush.Bitmap := nil;
  end
  else
  begin
    if Canvas.Brush.Bitmap = FPatternBitmap then Exit;
    { Dotted lines are drawn using dotted fill 2x2. The fill format is set
      immediately to the entire canvas relative to the upper left corner of
      the component. Therefore, even filling in the line shifted by 1 pixel
      from the previous one, we still get an alternating fill. When the grid
      is horizontally shifted by 1 pixel, the old fill is shifted too, and
      to draw a new fill alternating with the old one, you also need to shift
      the fill format. }
    if HorzScrollBar.Position mod 2 = 0 then
    begin
      FPatternBitmap.Canvas.Pixels[0, 0] := Color;
      FPatternBitmap.Canvas.Pixels[1, 1] := Color;
      FPatternBitmap.Canvas.Pixels[0, 1] := FillColor;
      FPatternBitmap.Canvas.Pixels[1, 0] := FillColor;
    end
    else
    begin
      FPatternBitmap.Canvas.Pixels[0, 0] := FillColor;
      FPatternBitmap.Canvas.Pixels[1, 1] := FillColor;
      FPatternBitmap.Canvas.Pixels[0, 1] := Color;
      FPatternBitmap.Canvas.Pixels[1, 0] := Color;
    end;
    Canvas.Brush.Bitmap := FPatternBitmap;
  end;
  Canvas.Refresh;
end;

procedure TCustomGridView.ResetClickPos;
begin
  FClickPos := GridCell(-1, -1);
end;

procedure TCustomGridView.Resize;
begin
  if UpdateLock = 0 then UpdateScrollBars;
  UpdateVisOriginSize;
  UpdateEdit(Editing);
  inherited Resize;
end;

procedure TCustomGridView.SetEditText(Cell: TGridCell; var Value: string);
begin
  if Assigned(FOnSetEditText) then FOnSetEditText(Self, Cell, Value);
end;

procedure TCustomGridView.ShowCursor;
begin
  if IsFocusAllowed then InvalidateFocus else ShowEdit;
end;

procedure TCustomGridView.ShowEdit;
begin
  UpdateEdit(True);
end;

procedure TCustomGridView.ShowEditChar(C: Char);
begin
  Editing := True;
  if (Edit <> nil) and Editing then
    PostMessage(Edit.Handle, WM_CHAR, Word(C), 0);
end;

procedure TCustomGridView.ShowFocus;
begin
  if IsFocusAllowed then PaintFocus;
end;

procedure TCustomGridView.SizeAllColumnsToFit;
var
  I: Integer;
  Column: TGridColumn;
begin
  if ColumnsResize then
    for I := 0 to Columns.Count - 1 do
    begin
      Column := Columns[I];
      if Column.Visible and not Column.FixedSize then SizeColumnToFit(I);
    end;
end;

procedure TCustomGridView.SizeColumnToFit(ColumnIndex: Integer);
var
  W: Integer;
begin
  if (ColumnIndex >= 0) and (ColumnIndex < Columns.Count) then
  begin
    W := Min(Columns[ColumnIndex].MaxWidth, GetColumnMaxWidth(ColumnIndex));
    ColumnSizeToFit(ColumnIndex, W);
    ColumnResize(ColumnIndex, W);
    FColResizing := True;
    try
      Columns[ColumnIndex].Width := W;
    finally
      FColResizing := False;
    end;
  end;
end;

procedure TCustomGridView.StartColResize(Section: TGridHeaderSection; X, Y: Integer);
begin
  FColResizeSection := Section;
  FColResizeIndex := Section.ResizeColumnIndex;
  FColResizeLevel := Section.Level;
  with FColResizeSection do
  begin
    if FColResizeIndex <= Columns.Count - 1 then
    begin
      FColResizeRect := GetColumnRect(FColResizeIndex);
      FColResizeRect.Bottom := GetGridRect.Bottom;
      FColResizeMinWidth := Columns[FColResizeIndex].MinWidth;
      FColResizeMaxWidth := Columns[FColResizeIndex].MaxWidth;
    end
    else
    begin
      FColResizeRect := BoundsRect;
      FColResizeRect.Bottom := GetGridRect.Bottom;
      FColResizeMinWidth := 0;
      FColResizeMaxWidth := 10000;
    end;
    FColResizeRect.Top := Level * Header.SectionHeight;
    FColResizeRect.Bottom := Height;
  end;
  FColResizePos := FColResizeRect.Right;
  FColResizeOffset := FColResizePos - X;
  FColResizeCount := 0;
  FColResizing := True;
  MouseCapture := True;
end;

procedure TCustomGridView.StepColResize(X, Y: Integer);
var
  W: Integer;
  R: TRect;
  S: TGridHeaderSection;
begin
  if FColResizing then
  begin
    { get the current resize line position and new column width }
    X := X + FColResizeOffset;
    W := X - FColResizeRect.Left;
    { update new width }
    if W < FColResizeMinWidth then W := FColResizeMinWidth;
    if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
    ColumnResizing(FColResizeIndex, W);
    if W < FColResizeMinWidth then W := FColResizeMinWidth;
    if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
    { calculate new resize line position }
    X := FColResizeRect.Left + W;
    if FColResizePos <> X then
    begin
      { hide the line in the old position }
      if (FColResizeCount > 0) and not FColumnsFullDrag then PaintResizeLine;
      Inc(FColResizeCount);
      FColResizePos := X;
      if FColumnsFullDrag and (FColResizeIndex < Columns.Count) then
      begin
        { calculate and update the changing part of the grid cells }
        UnionRect(R, GetHeaderRect, GetGridRect);
        R.Left := GetColumnLeftRight(FColResizeIndex).Left;
        if FColResizeIndex >= Fixed.Count then
        begin
          { if a regular column is partially closed fixed, then this closed
            part is not necessary to redraw }
          R.Left := Max(R.Left, GetFixedRect.Right);
        end;
        if (W < Columns[FColResizeIndex].Width) and
           (R.Right >= GetColumnLeftRight(Columns.Count - 1).Right) and
           (HorzScrollBar.Max - HorzScrollBar.Min > HorzScrollBar.PageStep) then
        begin
          { if the horizontal scrollbar is in the rightmost position, then
            decreasing the width of the column should shift to the right all
            the columns that are to the left of the current }
          R.Left := GetFixedRect.Right;
          R.Right := GetColumnLeftRight(FColResizeIndex + 1).Left;
        end;
        InvalidateRect(R);
        { if the column has a multi-level header, then you need to redraw
          the topmost section }
        S := GetHeaderSection(FColResizeIndex, 0);
        if S <> nil then
        begin
          R := S.BoundsRect;
          R.Bottom := GetHeaderRect.Bottom;
          InvalidateRect(R);
        end;
        LockUpdate;
        try
          Columns[FColResizeIndex].Width := W;
        finally
          { update grid instead of invalidate (blinks less) }
          UnlockUpdate(False);
          Update;
        end;
      end
      else
        { hide the line in the new position }
        PaintResizeLine;
    end
    else
    begin
      { show the resize line }
      if (FColResizeCount = 0) and not FColumnsFullDrag then PaintResizeLine;
      Inc(FColResizeCount);
    end;
  end;
end;

procedure TCustomGridView.StopColResize(Abort: Boolean);
var
  W: Integer;
begin
  if FColResizing then
  try
    MouseCapture := False;
    { a simple click on the right sizde of the section does not change the size }
    if FColResizeCount > 0 then
    begin
      if not FColumnsFullDrag then PaintResizeLine;
      if Abort then Exit;
      with FColResizeSection do
      begin
        W := FColResizePos - FColResizeRect.Left;
        if W < FColResizeMinWidth then W := FColResizeMinWidth;
        if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
        ColumnResize(FColResizeIndex, W);
        if W < FColResizeMinWidth then W := FColResizeMinWidth;
        if W > FColResizeMaxWidth then W := FColResizeMaxWidth;
        if FColResizeIndex < Columns.Count then
          Columns[FColResizeIndex].Width := W;
      end;
    end;
  finally
    FColResizing := False;
  end;
end;

procedure TCustomGridView.StartHeaderClick(Section: TGridHeaderSection; X, Y: Integer);
begin
  FHeaderClickSection := Section;
  FHeaderClickRect := Section.BoundsRect;
  FHeaderClickState := False;
  FHeaderClicking := True;
  MouseCapture := True;
  StepHeaderClick(X, Y);
end;

procedure TCustomGridView.StepHeaderClick(X, Y: Integer);
var
  P: Boolean;
begin
  if FHeaderClicking then
  begin
    P := PtInRect(FHeaderClickRect, Point(X, Y));
    if FHeaderClickState <> P then
    begin
      FHeaderClickState := P;
      InvalidateRect(FHeaderClickRect);
    end;
  end;
end;

procedure TCustomGridView.StopHeaderClick(Abort: Boolean);
var
  P: Boolean;
begin
  if FHeaderClicking then
  begin
    P := FHeaderClickState;
    StepHeaderClick(-1, -1);
    FHeaderClicking := False;
    MouseCapture := False;
    if (not Abort) and P then HeaderClick(FHeaderClickSection);
  end;
end;

procedure TCustomGridView.ApplyEdit;
begin
  Editing := False;
end;

procedure TCustomGridView.ApplyEditText;
var
  EditFocused: Boolean;
  EditText: string;
begin
  if (not ReadOnly) and (Edit <> nil) and (not IsCellReadOnly(EditCell)) then
  begin
    EditFocused := Editing;
    { text input can be canceled by throwing an exception in the
      OnSetEditText event }
    try
      EditText := Edit.Text;
      try
        SetEditText(EditCell, EditText);
      finally
        Edit.Text := EditText;
      end;
    except
      on E: Exception do
      begin
        MakeCellVisible(CellFocused, False);
        { if the input line is visible, then put the focus on it, otherwise
          it will be hidden after opening the error message box }
        if EditFocused then Edit.SetFocus;
        raise;
      end;
    end;
  end;
end;

procedure TCustomGridView.CancelEdit;
var
  Cell: TGridCell;
begin
  if Editing then
  begin
    { save the edit cell because it will be reset after hiding the
      inplace editor }
    Cell := EditCell;
    if not AlwaysEdit then
    begin
      HideEdit;
      ChangeEditing;
    end
    else
      UpdateEditContents(False);
    { OnEditCanceled event }
    EditCanceled(Cell);
  end;
end;

procedure TCustomGridView.DefaultDrawCell(Cell: TGridCell; Rect: TRect);
const
  ILDS: array[Boolean] of Longint = (ILD_NORMAL, ILD_SELECTED);
  TVDS: array[Boolean] of Longint = (TREIS_SELECTEDNOTFOCUS, TREIS_SELECTED);
var
  DefRect: TRect;
  CK: TGridCheckKind;
  CS: TCheckBoxState;
  CE: Boolean;
  CI, OI, X, Y, W, H: Integer;
  IDS: Longint;
  BKC, BLC: DWORD;
  R: TRect;
  RS, RH, CH, IH, SH: Boolean;
  IC, II, IT: TPoint;
  A: TAlignment;
  WR, WW: Boolean;
  T: string;
  Theme: HTHEME;
begin
  DefRect := Rect;
  CK := GetCheckKind(Cell);
  CI := GetCellImage(Cell, OI);
  RS := IsRowHighlighted(Cell.Row);
  RH := RS and (Cell.Col = Fixed.Count) and (Cell.Row = CellFocused.Row);
  CH := (not RS) and IsCellEqual(Cell, CellFocused);
  SH := (Canvas.Brush.Color = clHighlight) or (Canvas.Brush.Color = clBtnFace); {?}
  IH := (not ImageHighlight) and (RH or CH) and SH;
  IC := GetCheckIndent(Cell);
  II := GetCellImageIndent(Cell);
  { cell background }
  R := Rect;
  if IH then
  begin
    if CK <> gcNone then Inc(R.Left, IC.X + CheckWidth);
    if CI <> -1 then Inc(R.Left, II.X + Images.Width);
  end;
  Canvas.FillRect(R);
  { Window Vista themed focus }
  if Enabled and IsCellHighlighted(Cell) and (not IsCellEditing(Cell)) and
    StyleServices.Enabled and CheckWin32Version(6, 0) and
    (Focused or EditFocused or (not HideSelection)) then
  begin
    Theme := OpenThemeData(Self.Handle, 'TREEVIEW');
    if Theme <> 0 then
    begin
      if RS then
      begin
        R := GetRowRect(Cell.Row);
        R.Left := GetColumnLeftRight(Fixed.Count).Left;
      end;
      if (GridLines and (gsVertLine in GridStyle)) then Dec(R.Right, FGridLineWidth);
      if (GridLines and (gsHorzLine in GridStyle)) then Dec(R.Bottom, FGridLineWidth);
      DrawThemeBackground(Theme, Canvas.Handle, TVP_TREEITEM, TVDS[Focused or EditFocused], R, @Rect);
      CloseThemeData(Theme);
    end;
  end;
  { draw check box }
  if CK <> gcNone then
  begin
    if IH then Canvas.Brush.Color := Color;
    R := Rect;
    R.Right := Min(R.Left + CheckWidth + IC.X, Rect.Right);
    if R.Left < Rect.Right then
    begin
      with Canvas do
      begin
        X := R.Left + IC.X;
        Y := R.Top + IC.Y;
        W := CheckWidth;
        if X + W > R.Right then W := R.Right - X;
        H := CheckHeight;
        if Y + H > R.Bottom then H := R.Bottom - Y;
        if CK <> gcUserDefine then
        begin
          CS := GetCheckStateEx(Cell, CE);
          PaintCheck(Bounds(X, Y, W, H), CK, CS, CE);
        end
        else
        begin
          FCheckBuffer.Width := W;
          FCheckBuffer.Height := H;
          { user can define own check image using OnGetCheckImage event }
          GetCheckImage(Cell, FCheckBuffer);
          if not (FCheckBuffer.Empty or (FCheckBuffer.Width < 1) or
            (FCheckBuffer.Height < 1)) then Draw(X, Y, FCheckBuffer);
        end;
      end;
      { exclude check rect from text rect }
      Rect.Left := R.Right;
    end;
  end;
  { cell image }
  if (CI <> -1) and (Images <> nil) then
  begin
    R := Rect;
    R.Right := Min(R.Left + Images.Width + II.X, Rect.Right);
    if R.Left < Rect.Right then
    begin
      X := R.Left + II.X;
      Y := R.Top + II.Y;
      W := Images.Width;
      if X + W > R.Right then W := R.Right - X;
      H := Images.Height;
      if Y + H > R.Bottom then H := R.Bottom - Y;
      IDS := ILDS[IsCellHighlighted(Cell) and (Focused or EditFocused) and IH];
      if OI <> -1 then IDS := ILD_OVERLAYMASK and IndexToOverlayMask(OI + 1);
      BKC := GetRGBColor(Images.BkColor);
      BLC := GetRGBColor(Images.BlendColor);
      ImageList_DrawEx(Images.Handle, CI, Canvas.Handle, X, Y, W, H, BKC, BLC, IDS);
      { exclude image rect from text rect }
      Rect.Left := R.Right;
    end;
  end;
  { cell text }
  if not (IsCellEqual(Cell, FEditCell) and (not IsFocusAllowed)) and
    (Rect.Left < Rect.Right) then
  begin
    Rect.Top := DefRect.Top;
    IT := GetCellTextIndent(Cell);
    A := Columns[Cell.Col].Alignment;
    WR := Columns[Cell.Col].WantReturns;
    WW := Columns[Cell.Col].WordWrap;
    T := GetCellText(Cell);
    PaintText(Canvas, Rect, IT.X, IT.Y, A, WR, WW, T);
  end;
end;

procedure TCustomGridView.DefaultDrawHeader(Section: TGridHeaderSection; Rect: TRect);
var
  PaintState: TGridPaintStates;
  IsPressed: Boolean;
  I, X, Y, W, H: Integer;
  BKC, BLC: DWORD;
  T: string;
  TL: Integer;
  R: TRect;
  SD: TGridSortDirection;
  SS: TSize;
  SR: TRect;
  Points: array[0..2] of TPoint;
  ElementDetails: TThemedElementDetails;
  PS: TPenStyle;
  IT: TPoint;
begin
  IsPressed := IsHeaderPressed(Section);
  SD := gsNone;
  if Section.Sections.Count = 0 then SD := GetSortDirection(Section);
  { fill in the field to the right of the header, do not change the background
    color, as it could have been changed in the OnDrawHeader event }
  PaintState := [];
  if IsPressed then Include(PaintState, psPressed);
  if (not FHeaderClicking) and (Section = FHotSection) then Include(PaintState, psHot);
  if SD <> gsNone then Include(PaintState, psSorted);
  if Header.Flat then Include(PaintState, psFlat);
  if HighlightFocusCol and IsCellValidEx(CellFocused, True, False) and
    (Section.Sections.Count = 0) and (Section.ColumnIndex = CellFocused.Col) then
    Include(PaintState, psHot);
  PaintHeaderBackground(Rect, Canvas.Brush.Color, PaintState);
  { section pressing imitation }
  if IsPressed then OffsetRect(Rect, 1, 1);
  { some section have an image }
  I := GetHeaderImage(Section);
  if I <> -1 then
  begin
    R := Rect;
    R.Right := Min(R.Left + Header.Images.Width + 2, Rect.Right);
    if R.Left < Rect.Right then
    begin
      with Canvas do
      begin
        X := R.Left + 2;
        Y := R.Top + 1 + Ord(not Header.Flat);
        W := Header.Images.Width;
        if X + W > R.Right then W := R.Right - X;
        H := Header.Images.Height;
        if Y + H > R.Bottom then H := R.Bottom - Y;
        BKC := GetRGBColor(Header.Images.BkColor);
        BLC := GetRGBColor(Header.Images.BlendColor);
        ImageList_DrawEx(Header.Images.Handle, I, Canvas.Handle, X, Y, W, H, BKC, BLC, ILD_NORMAL);
      end;
      { image is always on the left of the section rectangle }
      Rect.Left := R.Right;
    end;
  end;
  if Rect.Left < Rect.Right then
  begin
    T := Section.DisplayText;
    TL := Length(T);
    { draw sort image before section text }
    if SD <> gsNone then
    begin
      SS := GetSortArrowSize;
      if StyleServices.Enabled and CheckWin32Version(6, 0) then
      begin
        SR.Left := Rect.Left + (Rect.Right - Rect.Left - SS.cx) div 2;
        SR.Right := SR.Left + SS.cx;
        SR.Top := Rect.Top;
        SR.Bottom := SR.Top + SS.cy;
        { sort image is never "pressed" (like in Explorer) }
        if IsPressed then OffsetRect(SR, -1, -1);
        if SD = gsAscending then
          ElementDetails := StyleServices.GetElementDetails(thHeaderSortArrowSortedUp)
        else
          ElementDetails := StyleServices.GetElementDetails(thHeaderSortArrowSortedDown);
        StyleServices.DrawElement(Canvas.Handle, ElementDetails, SR);
      end
      else
      begin
        SR := Bounds(0, 0, SS.cx, SS.cy);
        OffsetRect(SR, Rect.Right - 10 - SS.cx,
          Rect.Top + ((Rect.Bottom - Rect.Top) - SS.cy) div 2 + SortTopIndent);
        { sort image is never "pressed" (like in Explorer) }
        if IsPressed then OffsetRect(SR, -1, -1);
        if SD = gsAscending then
        begin
          OffsetRect(SR, 0, -1);
          Points[0] := Point(SR.Left, SR.Bottom);
          Points[1] := Point(SR.Left + SS.cx div 2, SR.Top);
          Points[2] := Point(SR.Right, SR.Bottom);
        end
        else
        begin
          OffsetRect(SR, 0, 1);
          Points[0] := Point(SR.Left + 1, SR.Top);
          Points[1] := Point(SR.Right, SR.Top);
          Points[2] := Point(SR.Left + SS.cx div 2, SR.Bottom - 1);
        end;
        PS := Canvas.Pen.Style;
        Canvas.Pen.Style := psClear;
        Canvas.Brush.Color := clGrayText;
        Canvas.Polygon(Points);
        Canvas.Pen.Style := PS;
        { sort image is always on the right when on disabled themes }
        Rect.Right := SR.Left - SortLeftIndent;
      end;
    end;
    if (TL > 0) and (Rect.Left < Rect.Right) then
    begin
      R := Rect;
      R.Top := R.Top + 2;
      { section text indent is the same as cells text indent }
      IT.X := TextLeftIndent;
      IT.Y := TextTopIndent;
      if I <> -1 then Inc(IT.X, 4);
      PaintText(Canvas, R, IT.X, IT.Y, Section.Alignment, False, Section.WordWrap, T);
    end;
  end;
end;

procedure TCustomGridView.DrawDragRect(Cell: TGridCell);
var
  R: TRect;
begin
  if IsCellVisible(Cell, True) then
  begin
    R := GetEditRect(Cell);
    GetCellColors(CellFocused, Canvas);
    with Canvas do
    begin
      { cannot drag on fixed cells }
      R := GetGridRect;
      IntersectClipRect(Handle, GetFixedRect.Right, R.Top, R.Right, R.Bottom);
      DrawFocusRect(R);
    end;
  end;
end;

function TCustomGridView.FindText(const FindText: string; Options: TFindOptions): Boolean;

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
  I, R: Integer;
begin
  if Rows.Count > 0 then
  begin
    Result := True;
    if frDown in Options then
    begin
      { search forward: iterate the cells down from left to right, starting
        with next cell relative to the current one }
      I := CellFocused.Col + 1;
      R := CellFocused.Row;
      while R <= Rows.Count - 1 do
      begin
        while I <= Columns.Count - 1 do
        begin
          if CompareCell(I, R) then Exit;
          Inc(I);
        end;
        Inc(R);
        I := 0;
      end;
    end
    else
    begin
      { search backward: iterate the cells up from right to left, starting
        with previous cell relative to the current one }
      I := CellFocused.Col - 1;
      R := CellFocused.Row;
      { special case: during backward search, a fixed cell should be skipped,
        since it cannot be selected, the cell to the right will be selected
        instead of it; the next reverse search will again detect this fixed
        cell, and the cell to the right will be selected again, and so on.
        outwardly, it will look as if the search is frozen in one cell }
      while (I >= 0) and (Columns[I].Width = 0) do Dec(I);
      if (I < Fixed.Count) and (R >= 0) then
      begin
        Dec(R);
        I := Columns.Count - 1;
      end;
      while R >= 0 do
      begin
        while I >= 0 do
        begin
          if CompareCell(I, R) then Exit;
          Dec(I);
        end;
        Dec(R);
        I := Columns.Count - 1;
      end;
    end;
    { text not found event }
    DoTextNotFound(FindText);
  end;
  Result := False;
end;

function TCustomGridView.GetCellAt(X, Y: Integer): TGridCell;
var
  C, R: Integer;
begin
  C := GetColumnAt(X, Y);
  R := GetRowAt(X, Y);
  if (C <> -1) and (R <> -1) then
  begin
    Result.Col := C;
    Result.Row := R;
  end
  else
  begin
    Result.Col := -1;
    Result.Row := -1;
  end;
end;

function TCustomGridView.GetCellRect(Cell: TGridCell): TRect;
var
  CR, RR: TRect;
begin
  { a simple intersection of a rectangle of a column and a row is not
    quite correct, because the left edge of the row rectangle cannot
    be to the left of the last fixed column }
  CR := GetColumnLeftRight(Cell.Col);
  Result.Left := CR.Left;
  Result.Right := CR.Right;
  RR := GetRowTopBottom(Cell.Row);
  Result.Top := RR.Top;
  Result.Bottom := RR.Bottom;
end;

function TCustomGridView.GetCellsRect(Cell1, Cell2: TGridCell): TRect;
var
  CR, RR: TRect;
begin
  if (Cell2.Col < Cell1.Col) or (Cell2.Row < Cell1.Row) then
  begin
    Result := Rect(0, 0, 0, 0);
    Exit;
  end;
  CR := GetColumnRect(Cell1.Col);
  if Cell2.Col > Cell1.Col then CR.Right := GetColumnRect(Cell2.Col).Right;
  RR := GetRowRect(Cell1.Row);
  if Cell2.Row > Cell1.Row then RR.Bottom := GetRowRect(Cell2.Row).Bottom;
  Result.Left := CR.Left;
  Result.Right := CR.Right;
  Result.Top := CR.Top; // <- RR.Top ???
  Result.Bottom := CR.Bottom; // <- RR.Bottom ???
end;

function TCustomGridView.GetColumnAt(X, Y: Integer): Integer;
var
  L, R: Integer;
begin
  Result := 0;
  L := GetGridRect.Left;
  while Result <= Fixed.Count - 1 do
  begin
    R := L + Columns[Result].Width;
    if (R <> L) and (X >= L) and (X < R) then Exit;
    L := R;
    Inc(Result);
  end;
  L := L + GetGridOrigin.X;
  while Result <= Columns.Count - 1 do
  begin
    R := L + Columns[Result].Width;
    if (R <> L) and (X >= L) and (X < R) then Exit;
    L := R;
    Inc(Result);
  end;
  Result := -1;
end;

function TCustomGridView.GetColumnLeftRight(Column: Integer): TRect;
begin
  if Columns.Count = 0 then
  begin
    Result.Left := GetGridRect.Left;
    Result.Right := Result.Left;
  end
  else if Column < 0 then
  begin
    Result := GetColumnLeftRight(0);
    Result.Right := Result.Left;
  end
  else if Column > Columns.Count - 1 then
  begin
    Result := GetColumnLeftRight(Columns.Count - 1);
    Result.Left := Result.Right;
  end
  else
  begin
    Result.Left := GetGridRect.Left + GetColumnsWidth(0, Column - 1);
    if Column >= Fixed.Count then Inc(Result.Left, GetGridOrigin.X);
    Result.Right := Result.Left + Columns[Column].Width;
  end;
end;

function TCustomGridView.GetColumnMaxWidth(Column: Integer): Integer;
var
  I, W: Integer;
  C: TGridCell;
  R: TRect;
begin
  if (Column < 0) or (Column > Columns.Count - 1) then
  begin
    Result := 0;
    Exit;
  end;
  if FVisSize.Row = 0 then
  begin
    Result := Columns[Column].DefWidth;
    Exit;
  end;
  { calculate autosize for visible rows only }
  Result := 0;
  for I := 0 to FVisSize.Row - 1 do
  begin
    C := GridCell(Column, VisOrigin.Row + I);
    R := GetCellTextBounds(C);
    W := R.Right - R.Left;
    if IsCellHasCheck(C) then Inc(W, CheckWidth + GetCheckIndent(C).X);
    if IsCellHasImage(C) then Inc(W, Images.Width + GetCellImageIndent(C).X);
    if GridLines and (gsVertLine in GridStyle) then Inc(W, FGridLineWidth);
    if Result < W then Result := W;
  end;
end;

function TCustomGridView.GetColumnRect(Column: Integer): TRect;
begin
  Result := GetColumnLeftRight(Column);
  Result.Top := GetRowTopBottom(0).Top;
  Result.Bottom := GetRowTopBottom(Rows.Count - 1).Bottom;
end;

function TCustomGridView.GetColumnsRect(Column1, Column2: Integer): TRect;
var
  R1, R2: TRect;
begin
  R1 := GetColumnRect(Column1);
  R2 := GetColumnRect(Column2);
  UnionRect(Result, R1, R2); {!}
end;

function TCustomGridView.GetColumnsWidth(Column1, Column2: Integer): Integer;
var
  I: Integer;
begin
  Result := 0;
  Column1 := Max(Column1, 0);
  Column2 := Min(Column2, Columns.Count - 1);
  for I := Column1 to Column2 do Inc(Result, Columns[I].Width);
end;

function TCustomGridView.GetEditRect(Cell: TGridCell): TRect;
begin
  Result := GetCellRect(Cell);
  if IsCellHasCheck(Cell) then Inc(Result.Left, CheckWidth + GetCheckIndent(Cell).X);
  if IsCellHasImage(Cell) then Inc(Result.Left, Images.Width + GetCellImageIndent(Cell).X);
  if GridLines then
  begin
    if gsVertLine in GridStyle then Dec(Result.Right, FGridLineWidth);
    if gsHorzLine in GridStyle then Dec(Result.Bottom, FGridLineWidth);
  end;
  if Result.Left > Result.Right then Result.Left := Result.Right;
end;

function TCustomGridView.GetHeaderHeight: Integer;
begin
  Result := Header.Height;
end;

function TCustomGridView.GetHeaderRect: TRect;
begin
  Result := GetClientRect;
  Result.Bottom := Result.Top;
  if ShowHeader then Inc(Result.Bottom, GetHeaderHeight);
end;

function TCustomGridView.GetFindDialog: TGridFindDialog;
begin
  if FFindDialog = nil then
  begin
    FFindDialog := TGridFindDialog.Create(Self);
    FFindDialog.OnFind := HandlerFind;
  end;
  Result := FFindDialog;
end;

function TCustomGridView.GetFirstImageColumn: Integer;
var
  I: Integer;
begin
  for I := Fixed.Count to Columns.Count - 1 do
    if Columns[I].Visible then
    begin
      Result := I;
      Exit;
    end;
  Result := -1;
end;

function TCustomGridView.GetFixedRect: TRect;
begin
  Result := GetGridRect;
  Result.Right := Result.Left + GetFixedWidth;
end;

function TCustomGridView.GetFixedWidth: Integer;
begin
  Result := GetColumnsWidth(0, Fixed.Count - 1);
end;

function TCustomGridView.GetFocusRect: TRect;
var
  C: TGridCell;
  L: Integer;
begin
  if RowSelect then
  begin
    C := GridCell(Fixed.Count, CellFocused.Row);
    Result := GetRowRect(CellFocused.Row);
    Result.Left := GetColumnLeftRight(Fixed.Count).Left;
  end
  else
  begin
    C := CellFocused;
    Result := GetCellRect(CellFocused);
  end;
  if not ImageHighlight then
  begin
    { neither the picture nor the check box are highlighted }
    if IsCellHasCheck(C) then Inc(Result.Left, CheckWidth + GetCheckIndent(C).X);
    if IsCellHasImage(C) then Inc(Result.Left, Images.Width + GetCellImageIndent(C).X);
  end;
  L := GetColumnLeftRight(C.Col).Right;
  if Result.Left > L then Result.Left := L;
end;

function TCustomGridView.GetFontHeight(Font: TFont): Integer;
var
  DC: HDC;
  SaveFont: HFont;
  Metrics: TTextMetric;
begin
  DC := GetDC(0);
  SaveFont := SelectObject(DC, Font.Handle);
  GetTextMetrics(DC, Metrics);
  SelectObject(DC, SaveFont);
  ReleaseDC(0, DC);
  Result := Metrics.tmHeight;
end;

function TCustomGridView.GetFontWidth(Font: TFont; TextLength: Integer): Integer;
var
  DC: HDC;
  Canvas: TCanvas;
  TM: TTextMetric;
begin
  DC := GetDC(0);
  try
    Canvas := TCanvas.Create;
    try
      Canvas.Handle := DC;
      Canvas.Font := Font;
      GetTextMetrics(DC, TM);
      Result := TextLength * (Canvas.TextWidth('0') - TM.tmOverhang) + TM.tmOverhang + 4;
    finally
      Canvas.Free;
    end;
  finally
    ReleaseDC(0, DC);
  end;
end;

function TCustomGridView.GetGridHeight: Integer;
begin
  with GetGridRect do Result := Bottom - Top;
end;

function TCustomGridView.GetGridOrigin: TPoint;
begin
  Result.X := - HorzScrollBar.Position;
  Result.Y := - VertScrollBar.Position * Rows.Height;
end;

function TCustomGridView.GetGridRect: TRect;
begin
  Result := ClientRect;
  Result.Top := GetHeaderRect.Bottom;
end;

function TCustomGridView.GetHeaderSection(ColumnIndex, Level: Integer): TGridHeaderSection;

  function DoGetSection(Sections: TGridHeaderSections): TGridHeaderSection;
  var
    I, L: Integer;
    S: TGridHeaderSection;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      L := S.Level;
      if (S.ColumnIndex >= ColumnIndex) and
        (((Level = -1) and (S.Sections.Count = 0)) or (L = Level)) then
      begin
        Result := S;
        Exit;
      end;
      { recursion for nested sections }
      S := DoGetSection(S.Sections);
      if S <> nil then
      begin
        Result := S;
        Exit;
      end;
    end;
    Result := nil;
  end;

begin
  Result := DoGetSection(Header.Sections);
end;

function TCustomGridView.GetResizeSectionAt(X, Y: Integer): TGridHeaderSection;

  function FindSection(Sections: TGridHeaderSections; var Section: TGridHeaderSection): Boolean;
  var
    I, C, DL, DR: Integer;
    R: TRect;
    S: TGridHeaderSection;
  begin
    for I := Sections.Count - 1 downto 0 do
    begin
      S := Sections[I];
      if S.Visible then
      begin
        C := S.ResizeColumnIndex;
        R := S.BoundsRect;
        { hit accuracy }
        with R do
        begin
          DL := 7;
          if R.Right - R.Left < 20 then DL := 3;
          if R.Right - R.Left < 10 then DL := 1;
          DR := 5;
          if C < Columns.Count - 1 then
          begin
            if Columns[C + 1].DefWidth < 20 then DR := 3;
            if Columns[C + 1].DefWidth < 10 then DR := 1;
          end;
          if R.Right > R.Left then Left := Right - DL;
          Right := Right + DR;
        end;
        if PtInRect(R, Point(X, Y)) then
        begin
          { some columns cannot resize }
          if (C < Columns.Count) and (Columns[C].FixedSize or (not ColumnsResize)) then
          begin
            Section := nil;
            Result := False;
          end
          else
          begin
            Section := S;
            Result := True;
          end;
          Exit;
        end;
        { recursion for nested sections }
        if FindSection(S.Sections, Section) then
        begin
          Result := True;
          Exit;
        end;
      end;
    end;
    Section := nil;
    Result := False;
  end;

begin
  FindSection(Header.Sections, Result);
end;

function TCustomGridView.GetRowAt(X, Y: Integer): Integer;
var
  Row: Integer;
  GRT, GOY: Integer;
begin
  Result := -1;
  GRT := GetGridRect.Top;
  GOY := GetGridOrigin.Y;
  if Y - GRT - GOY < 0 then exit;    // <-- !!!
  if Rows.Height > 0 then
  begin
    Row := (Y - GRT - GOY) div Rows.Height;
    if (Row >= 0) and (Row < Rows.Count) then Result := Row;
  end;
end;

function TCustomGridView.GetRowRect(Row: Integer): TRect;
begin
  Result := GetRowTopBottom(Row);
  Result.Left := Min(GetGridRect.Left, GetColumnLeftRight(Fixed.Count).Left);
  Result.Right := GetColumnLeftRight(Columns.Count - 1).Right;
end;

function TCustomGridView.GetRowsRect(Row1, Row2: Integer): TRect;
var
  R1, R2: TRect;
begin
  R1 := GetRowRect(Row1);
  R2 := GetRowRect(Row2);
  UnionRect(Result, R1, R2);
end;

function TCustomGridView.GetRowsHeight(Row1, Row2: Integer): Integer;
begin
  Result := 0;
  if Row2 >= Row1 then Result := (Row2 - Row1 + 1) * Rows.Height;
end;

function TCustomGridView.GetRowTopBottom(Row: Integer): TRect;
begin
  { all rows have some height }
  Result.Top := GetGridRect.Top + GetRowsHeight(0, Row - 1) + GetGridOrigin.Y;
  Result.Bottom := Result.Top + Rows.Height;
end;

function TCustomGridView.GetSectionAt(X, Y: Integer): TGridHeaderSection;

  function FindSection(Sections: TGridHeaderSections; var Section: TGridHeaderSection): Boolean;
  var
    I: Integer;
    S: TGridHeaderSection;
    R: TRect;
  begin
    for I := 0 to Sections.Count - 1 do
    begin
      S := Sections[I];
      if S.Visible then
      begin
        R := S.BoundsRect;
        if PtInRect(R, Point(X, Y)) then
        begin
          Section := S;
          Result := True;
          Exit;
        end;
      end;
      { recursion for nested sections }
      if FindSection(S.Sections, Section) then
      begin
        Result := True;
        Exit;
      end;
    end;
    Section := nil;
    Result := False;
  end;

begin
  FindSection(Header.Sections, Result);
end;

procedure TCustomGridView.Invalidate;
begin
  if (Parent <> nil) and (FUpdateLock = 0) then inherited;
end;

procedure TCustomGridView.InvalidateCell(Cell: TGridCell);
var
  R: TRect;
begin
  R := GetCellRect(Cell);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateCheck(Cell: TGridCell);
var
  R: TRect;
begin
  R := GetCheckRect(Cell);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateColumn(Column: Integer);
var
  R: TRect;
begin
  R := GetColumnRect(Column);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateColumns(Column1, Column2: Integer);
var
  R: TRect;
begin
  R := GetColumnsRect(Column1, Column2);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateEdit;
begin
  if Editing then Edit.Invalidate;
end;

procedure TCustomGridView.InvalidateFixed;
begin
  InvalidateRect(GetFixedRect);
end;

procedure TCustomGridView.InvalidateFocus;
var
  Rect: TRect;
begin
  Rect := GetFocusRect;
  { cell image not included in focus rectangle }
  if IsRowHighlighted(CellFocused.Row) then
    UnionRect(Rect, Rect, GetCellRect(GridCell(Fixed.Count, CellFocused.Row)))
  else
    UnionRect(Rect, Rect, GetCellRect(CellFocused));
  { selected row not included in focus rectangle }
  if HighlightFocusRow then
  begin
    Rect.Left := 0;
    Rect.Right := Width;
  end;
  InvalidateRect(Rect);
  { invalidate header section too }
  if HighlightFocusCol and ShowHeader and StyleServices.Enabled then
  begin
    Rect := GetColumnRect(CellFocused.Col);
    Rect.Top := 0;
    Rect.Bottom := GetHeaderHeight;
    InvalidateRect(Rect);
  end;
end;

procedure TCustomGridView.InvalidateGrid;
begin
  InvalidateEdit;
  InvalidateRect(GetGridRect);
end;

procedure TCustomGridView.InvalidateHeader;
begin
  if ShowHeader then InvalidateRect(GetHeaderRect);
end;

procedure TCustomGridView.InvalidateRect(Rect: TRect);
begin
  if (FUpdateLock = 0) and HandleAllocated and Visible then
    Windows.InvalidateRect(Handle, @Rect, False);
end;

procedure TCustomGridView.InvalidateRow(Row: Integer);
var
  R: TRect;
begin
  R := GetRowRect(Row);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateRows(Row1, Row2: Integer);
var
  R: TRect;
begin
  R := GetRowsRect(Row1, Row2);
  if IntersectRect(R, R, GetGridRect) then InvalidateRect(R);
end;

procedure TCustomGridView.InvalidateSection(Section: TGridHeaderSection);
begin
  if ShowHEader and (Section <> nil) then InvalidateRect(Section.BoundsRect)
end;

procedure TCustomGridView.InvalidateSection(ColumnIndex, Level: Integer);
begin
  InvalidateSection(GetHeaderSection(ColumnIndex, Level));
end;

function TCustomGridView.IsActiveControl: Boolean;
var
  Form: TCustomForm;
  H: HWND;
begin
  Form := GetParentForm(Self);
  if (Form <> nil) and (Form.ActiveControl = Self) then
  begin
    Result := True;
    Exit;
  end;
  H := GetFocus;
  while IsWindow(H) do
  begin
    if H = WindowHandle then
    begin
      Result := True;
      Exit;
    end;
    H := GetParent(H);
  end;
  Result := False;
end;

function TCustomGridView.IsCellAcceptCursor(Cell: TGridCell): Boolean;
begin
  if not IsCellValid(Cell) then
  begin
    Result := False;
    Exit;
  end;
  Result := (Cell.Col >= Fixed.Count) and Columns[Cell.Col].TabStop;
  if Assigned(FOnCellAcceptCursor) then FOnCellAcceptCursor(Self, Cell, Result);
end;

function TCustomGridView.IsCellEditing(Cell: TGridCell): Boolean;
begin
  Result := IsCellEqual(EditCell, Cell) and Editing;
end;

function TCustomGridView.IsCellHighlighted(Cell: TGridCell): Boolean;
begin
  Result := (Cell.Col >= Fixed.Count) and
    ((CellSelected and IsCellFocused(Cell) and IsFocusAllowed) or IsRowHighlighted(Cell.Row));
end;

function TCustomGridView.IsCellHasCheck(Cell: TGridCell): Boolean;
begin
  Result := IsCellValid(Cell) and CheckBoxes and (GetCheckKind(Cell) <> gcNone);
end;

function TCustomGridView.IsCellHasImage(Cell: TGridCell): Boolean;
var
  Dummy: Integer;
begin
  Result := IsCellValid(Cell) and Assigned(Images) and (GetCellImage(Cell, Dummy) <> -1);
end;

function TCustomGridView.IsCellFocused(Cell: TGridCell): Boolean;
begin
  Result := ((Cell.Col = CellFocused.Col) or RowSelect) and
    (Cell.Row = CellFocused.Row) and (Cell.Col >= Fixed.Count);
end;

function TCustomGridView.IsCellReadOnly(Cell: TGridCell): Boolean;
begin
  Result := True;
  if IsCellValid(Cell) then
  begin
    Result := ReadOnly or (Cell.Col < Fixed.Count) or Columns[Cell.Col].ReadOnly;
    if Assigned(FOnGetCellReadOnly) then FOnGetCellReadOnly(Self, Cell, Result);
  end;
end;

function TCustomGridView.IsCellValid(Cell: TGridCell): Boolean;
begin
  Result := IsCellValidEx(Cell, True, True);
end;

function TCustomGridView.IsCellValidEx(Cell: TGridCell; CheckPosition, CheckVisible: Boolean): Boolean;
var
  C, R, V: Boolean;
begin
  with Cell do
  begin
    C := (Col >= 0) and (Col < Columns.Count);
    R := (Row >= 0) and (Row < Rows.Count);
    V := C and Columns[Col].Visible and (Columns[Col].Width > 0);
  end;
  Result := ((not CheckPosition) or (C and R)) and ((not CheckVisible) or V);
end;

function TCustomGridView.IsCellVisible(Cell: TGridCell; PartialOK: Boolean): Boolean;
var
  CR, GR, R: TRect;
begin
  CR := GetCellRect(Cell);
  GR := GetGridRect;
  if (Fixed.Count > 0) and (Cell.Col >= Fixed.Count) then
    GR.Left := GetFixedRect.Right;
  Result := IntersectRect(R, CR, GR);
  if not PartialOK then Result := EqualRect(R, CR);
end;

function TCustomGridView.IsColumnVisible(Column: Integer): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, GetColumnRect(Column), GetGridRect);
end;

function TCustomGridView.IsEvenRow(Cell: TGridCell): Boolean;
begin
  Result := Cell.Row mod 2 <> 0;
end;

function TCustomGridView.IsFixedVisible: Boolean;
begin
  Result := (Columns.Count > 0) and (Fixed.Count > 0);
end;

function TCustomGridView.IsFocusAllowed: Boolean;
begin
  Result := (RowSelect or (not (Editing or AlwaysEdit))) and AllowSelect;
end;

function TCustomGridView.IsGridHintVisible: Boolean;
begin
  Result := FShowGridHint and (Rows.Count = 0);
end;

function TCustomGridView.IsHeaderHasImage(Section: TGridHeaderSection): Boolean;
begin
  Result := Assigned(Header.Images) and (GetHeaderImage(Section) <> -1);
end;

function TCustomGridView.IsHeaderPressed(Section: TGridHeaderSection): Boolean;
begin
  Result := ((Section = nil) or (Section = FHeaderClickSection)) and FHeaderClickState;
end;

function TCustomGridView.IsRowHighlighted(Row: Integer): Boolean;
begin
  Result := CellSelected and RowSelect and (Row = CellFocused.Row);
end;

function TCustomGridView.IsRowVisible(Row: Integer): Boolean;
var
  R: TRect;
begin
  Result := IntersectRect(R, GetRowRect(Row), GetGridRect);
end;

procedure TCustomGridView.LockUpdate;
begin
  Inc(FUpdateLock);
end;

procedure TCustomGridView.MakeCellVisible(Cell: TGridCell; PartialOK: Boolean);
var
  DX, DY, X, Y: Integer;
  R: TRect;
begin
  if not IsCellVisible(Cell, PartialOK) then
  begin
    DX := 0;
    DY := 0;
    with GetGridRect do
    begin
      // if not RowSelect then
      begin
        R := GetColumnRect(Cell.Col);
        X := Left + GetFixedWidth;
        if R.Right > Right then DX := Right - R.Right;
        if R.Left < X then DX := X - R.Left;
        if R.Right - R.Left > Right - X then DX := X - R.Left;
      end;
      if Rows.Height > 0 then
      begin
        R := GetRowRect(Cell.Row);
        if R.Bottom > Bottom then DY := Bottom - R.Bottom;
        if R.Top < Top then DY := Top - R.Top;
        if R.Bottom - R.Top > Bottom - Top then DY := Top - R.Top;
        Y := DY div Rows.Height;
        if (FVisSize.Row > 1) and (DY mod Rows.Height <> 0) then Dec(Y);
        DY := Y;
      end;
    end;
    { make cell visible using scrollbars }
    with VertScrollBar do Position := Position - DY;
    with HorzScrollBar do Position := Position - DX;
  end;
end;

procedure TCustomGridView.SetCursor(Cell: TGridCell; Selected, Visible: Boolean);
var
  PartialOK: Boolean;
begin
  UpdateSelection(Cell, Selected);
  if (not IsCellEqual(FCellFocused, Cell)) or (FCellSelected <> Selected) then
  begin
    Changing(Cell, Selected);
    if not IsCellEqual(FCellFocused, Cell) then
    begin
      CancelCellTips;
      Editing := False;
      HideCursor;
      PartialOK := RowSelect or (FCellFocused.Col = Cell.Col);
      FCellFocused := Cell;
      FCellSelected := Selected;
      if Visible then MakeCellVisible(CellFocused, PartialOK);
      ShowCursor;
    end
    else if FCellSelected <> Selected then
    begin
      if Editing then ShowEdit;
      if not Editing then
      begin
        HideCursor;
        FCellSelected := Selected;
        if Visible then MakeCellVisible(CellFocused, True);
        ShowCursor;
      end;
    end;
    Change(FCellFocused, FCellSelected);
  end
  else
    { the position of the cursor and the state of the selection do not
      change - make the cursor visible }
    if Visible then MakeCellVisible(CellFocused, False);
end;

procedure TCustomGridView.ResetEdit;
begin
  if (FEdit <> nil) and EditCanUndo(EditCell) then FEdit.Perform(WM_UNDO, 0, 0);
end;

procedure TCustomGridView.UnLockUpdate(Redraw: Boolean);
begin
  Dec(FUpdateLock);
  if (FUpdateLock = 0) and Redraw then Invalidate;
end;

procedure TCustomGridView.UpdateCursor;
var
  Cell: TGridCell;
  IsValidCell, Dummy: Boolean;
begin
  Cell := CellFocused;
  IsValidCell := IsCellValid(Cell) and IsCellAcceptCursor(Cell);
  { if the current cell is not available, then search for an available
    cell around then search the first available cell }
  if not IsValidCell then
  begin
    UpdateSelection(Cell, Dummy);
    if IsCellEqual(Cell, CellFocused) then Cell := GetCursorCell(Cell, goFirst);
  end;
  SetCursor(Cell, CellSelected, not IsValidCell);
end;

procedure TCustomGridView.UpdateColors;
begin
  Header.GridColorChanged(Color);
  Fixed.GridColorChanged(Color);
end;

procedure TCustomGridView.UpdateEdit(Activate: Boolean);

  procedure DoValidateEdit;
  var
    EditClass: TGridEditClass;
  begin
    EditClass := GetEditClass(FCellFocused);
    if (FEdit = nil) or (FEdit.ClassType <> EditClass) then
    begin
      FEdit.Free;
      FEdit := CreateEdit(EditClass);
      FEdit.Parent := Self;
      FEdit.FGrid := Self;
    end;
  end;

  procedure DoUpdateEdit;
  begin
    FEditCell := FCellFocused;
    FEdit.Updating;
    FEdit.UpdateContents;
    FEdit.UpdateStyle;
    FEdit.Updated;
    FEdit.SelectAll;
  end;

begin
  if Activate and EditCanShow(FCellFocused) then
  begin
    if FEdit = nil then
    begin
      DoValidateEdit;
      DoUpdateEdit;
    end
    else if not IsCellEditing(FCellFocused) then
    begin
      Activate := Activate or Editing or AlwaysEdit;
      HideEdit;
      DoValidateEdit;
      DoUpdateEdit;
    end;
    if Activate then
    begin
      CancelCellTips;
      FEdit.Show;
    end;
  end
  else
    HideEdit;
end;

procedure TCustomGridView.UpdateEditContents(SaveText: Boolean);
var
  EditText: string;
begin
  if Editing then
  begin
    EditText := Edit.Text;
    { inplace editor must be hidden to update completely }
    HideEdit;
    UpdateEdit(True);
    if SaveText then Edit.Text := EditText;
  end;
end;

procedure TCustomGridView.UpdateFixed;
begin
  Fixed.SetCount(Fixed.Count);
end;

procedure TCustomGridView.UpdateFocus;
begin
  if csDesigning in ComponentState then Exit;
  { if the grid is already active, focus on it again, otherwise there
    may be problems with MDI forms }
  if IsActiveControl then
  begin
    Windows.SetFocus(Handle);
    if GetFocus = Handle then Perform(CM_UIACTIVATE, 0, 0);
  end
  else if IsWindowVisible(Handle) and TabStop and
    (CanFocus or (GetParentForm(Self) = nil)) then
  begin
    Show;
    SetFocus;
    if AlwaysEdit and (Edit <> nil) then UpdateEdit(True);
  end;
end;

procedure TCustomGridView.UpdateFonts;
begin
  Header.GridFontChanged(Font);
  Fixed.GridFontChanged(Font);
end;

procedure TCustomGridView.UpdateHeader;
begin
  with Header do
  begin
    SynchronizeSections;
    SetSectionHeight(SectionHeight);
  end;
end;

procedure TCustomGridView.UpdateRows;
begin
  Rows.SetHeight(Rows.Height);
end;

procedure TCustomGridView.UpdateScrollBars;

  procedure UpdateVertScrollBar;
  var
    R, P, L: Integer;
  begin
    if (Rows.Count > 0) and (Rows.Height > 0) then
    begin
      R := Rows.Count - 1;
      with GetGridRect do P := (Bottom - Top) div Rows.Height;
      L := 1;
    end
    else
    begin
      R := 0;
      P := 0;
      L := 0;
    end;
    with VertScrollBar do
    begin
      SetLineSize(Rows.Height);
      SetParams(0, R, P, L);
    end;
  end;

  procedure UpdateHorzScrollBar;
  var
    R, P, L: Integer;
  begin
    if Columns.Count > 0 then
    begin
      R := GetColumnsWidth(0, Columns.Count - 1) - GetFixedWidth;
      with GetGridRect do P := (Right - Left) - GetFixedWidth;
      L := 8;
    end
    else
    begin
      R := 0;
      P := 0;
      L := 0;
    end;
    with HorzScrollBar do
    begin
      SetLineSize(1);
      SetParams(0, R, P, L);
    end;
  end;

begin
  { when updating scrollers, the window will resize several times and each
    resizing will require redrawing, it needs to be blocked and redrawn
    once at the end }
  LockUpdate;
  try
    UpdateVertScrollBar;
    UpdateHorzScrollBar;
    UpdateVertScrollBar;
  finally
    UnLockUpdate(False);
  end;
end;

procedure TCustomGridView.UpdateScrollPos;
begin
  VertScrollBar.Position := FVisOrigin.Row;
  HorzScrollBar.Position := GetColumnsWidth(Fixed.Count, FVisOrigin.Col - 1);
end;

procedure TCustomGridView.UpdateSelection(var Cell: TGridCell; var Selected: Boolean);
begin
  { update selection state }
  Selected := Selected or FAlwaysSelected;
  Selected := Selected and (Rows.Count > 0) and (Columns.Count > 0);
  { update cell index }
  with Cell do
  begin
    if Col < Fixed.Count then Col := Fixed.Count;
    if Col < 0 then Col := 0;
    if Col > Columns.Count - 1 then Col := Columns.Count - 1;
    if Row < 0 then Row := 0;
    if Row > Rows.Count - 1 then Row := Rows.Count - 1;
  end;
  { get the nearest cell, which can be selected }
  Cell := GetCursorCell(Cell, goSelect);
end;

procedure TCustomGridView.UpdateText;
begin
  ApplyEditText;
end;

procedure TCustomGridView.UpdateVisOriginSize;
var
  R: TRect;
  I, X, H: Integer;
begin
  if Columns.Count > 0 then
  begin
    { search the first visible column by the scroller position }
    X := GetGridRect.Left + GetFixedWidth - HorzScrollBar.Position;
    R := GetFixedRect;
    I := Fixed.Count;
    while I < Columns.Count - 1 do
    begin
      X := X + Columns[I].Width;
      if X > R.Right then Break;
      Inc(I);
    end;
    FVisOrigin.Col := I;
    { the last column may be partially visible }
    R := GetGridRect;
    while I < Columns.Count - 1 do
    begin
      if X >= R.Right then Break;
      Inc(I);
      X := X + Columns[I].Width;
    end;
    FVisSize.Col := I - FVisOrigin.Col + 1;
  end
  else
  begin
    FVisOrigin.Col := 0;
    FVisSize.Col := 0;
  end;
  if (Rows.Count > 0) and (Rows.Height > 0) then
  begin
    { first visible row is the position of vertical scrollbar }
    FVisOrigin.Row := VertScrollBar.Position;
    { the last row may be partially visible }
    H := GetGridHeight;
    FVisSize.Row := H div Rows.Height + Ord(H mod Rows.Height > 0);
    if FVisSize.Row + FVisOrigin.Row  > Rows.Count then
      FVisSize.Row := Rows.Count - FVisOrigin.Row;
    if FVisSize.Row < 0 then FVisSize.Row := 0;
  end
  else
  begin
    FVisOrigin.Row := 0;
    FVisSize.Row := 0;
  end;
end;

end.

