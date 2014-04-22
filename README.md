CSV Table Scripts
=================

* Data.CSV.Table.Types
  + `Table`
  + `Row`
  + `Col`

* Data.CSV.Table.Parse
  + parse   :: `FilePath -> IO Table`
  
* Data.CSV.Table.Utils
  + forRow  :: `Table -> (Row -> IO ()) -> IO ()`
  + joinBy  :: `Col   -> Table   -> Table -> Table`
  + average :: `Col   -> Table   -> Int`
  + addCol  :: `Table -> Formula -> Table`
  + email   :: `Table -> [Col]`
  + sortBy  :: `Col   -> Table   -> Table`
  + mailRow :: `Table -> (Row -> Email) -> IO ()`

