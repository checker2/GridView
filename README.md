# TGridView component

(C) 1997-2019 Roman M. Mochalov
email: checker@mail.ru


## Description

TGridView is a Delphi VCL grid component for displaying records, arrays, collections, and any other data in the form of tables. 
It is a native and very fast virtual grid designed to display external data using event handlers without internal data storage like Delphi TStringGrid.


## Feautres

- Up to 2147483647 rows.
- Multiline header.
- Windows themes support.
- Unicode support.
- DB version included with multiselecting.


## How to install

1) Compile "Packages\Delphi\GridViewDR.dproj" package.
2) Compile "Packages\Delphi\GridViewDD.dproj" package.
3) Install "GridViewDD.dproj" package.
4) Add "Source\\" library path.


## How to use

1) Place TGridView component on form.
2) Define columns using "Columns" property.
3) Set row count using "Rows.Count" property.
4) Use OnGetCellText event to define cell text using your data.
5) Use OnSetEditText to get text into your data.


## License

[MIT](https://github.com/checker2/GridView/blob/master/LICENSE)
