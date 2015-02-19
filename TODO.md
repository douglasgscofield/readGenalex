# For 1.0

o `createGenalex()`, for creating a de-novo class `'genalex'` data frame.  This requires a `createGenalexLocus` or some generalisation of the current locus-handling functions, to add a new locus to the "end".
o functions for converting from/to some other genetic formats
o When I do the dispersalDiversity package, since that will require readGenalex, should that be based on a 1.0-type readGenalex?
o check on any implicit version dependencies (e.g., do need to use paste0() which was introduced in R 2.15.0)
o document version dependencies
o drawing on writeGenalex documentation about differences when writing, be more explicit about what happens with extra columns and with extra column names on input
o straighten out test for writeGenalex with and without quote=
o read from and write to Excel files
o add more tests
o implement makefile rule for building RData in data/
o exchange fake data for real data from DataDryad
o how to add citation instructions to data?  see data/datasource for that info
o complete null allele check integration
o make null allele demo using createGenalex(), and also using adult and pericarp Q. agrifolia data
x create an S3 class "genalex" based on data.frame and modify is.genalex to check for that (or could we then use the generic?)  I ended up creating as.genalex and using this internally where the class will be returned.
x Implementing class "genalex" needs to bump version to 1.0, since there would be API changes.
x For 1.0, recognise pre-1.0 genalex data frames and convert automatically (on option of course) to 1.0 class.
x Did above by adding as.genalex
x implement `quote=` for `writeGenalex`
x add null allele check to this package

## internal work

x concatenate all files into one R source file
x rethink method strategy
