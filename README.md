CSV Table Scripts
=================

* Data.CSV.Table.Types
  + `Table`
  + `Row`
  + `Col`
  + parse   :: `FilePath -> IO Table`
  
* Data.CSV.Table.Utils
  + forRow    :: `Table -> (Row    -> IO ()) -> IO ()`
  + isUnique  :: `Table -> Col     -> Bool`
  + joinBy    :: `Col   -> Table   -> Table -> Table`
  + average   :: `Col   -> Table   -> Int`
  + addCol    :: `Table -> Formula -> Table`
  + email     :: `Table -> [Col]`
  + sortBy    :: `Col   -> Table   -> Table`
  + mailRow   :: `Table -> (Row -> Email) -> IO ()`

