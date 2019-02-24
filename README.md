# GridView

GridView is a Delphi VCL grid component for displaying records, arrays, collections, and any other data in the form of tables. It is a native and fast virtual grid without internal data storage designed to display external data using event handlers.


## Feautres

- Event-driven architecture.
- Up to 2147483647 rows.
- Multiline header with column click support.
- Windows themes support.
- Unicode support.
- DB version included with multiselecting.


## How to install

1. Compile "Packages\Delphi\GridViewDR.dproj" package.
2. Compile "Packages\Delphi\GridViewDD.dproj" package.
3. Install "GridViewDD.dproj" package.
4. Add "Source\\" library path.


## How to use

1. Place TGridView component on form.
2. Define columns using "Columns" property.
3. Set row count using "Rows.Count" property.
4. Use OnGetCellText event to define cell text using your data.
5. Use OnSetEditText to get text into your data.


## Author

Roman M. Mochalov (<checker@mail.ru>)


## License

[MIT](https://github.com/checker2/GridView/blob/master/LICENSE)
