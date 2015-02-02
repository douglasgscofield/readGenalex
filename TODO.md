# For 1.0

* `createGenalex()`, for creating a de-novo `is.genalex()`-able data.frame.  This requires a createGenalexLocus or some generalisation of the current locus-handling functions, to add a new locus to the "end".
* functions for converting from/to some other genetic formats
* create an S3 class "genalex" based on data.frame and modify is.genalex to check for that (or could we then use the generic?)
* Implementing class "genalex" needs to bump version to 1.0, since there would be API changes.
* When I do the dispersalDiversity package, since that will require readGenalex, should that be based on a 1.0-type readGenalex?
* For 1.0, recognise pre-1.0 genalex data frames and convert automatically (on option of course) to 1.0 class.

# For 0.5

* drawing on writeGenalex documentation about differences when writing, be more explicit about what happens with extra columns and with extra column names on input
* implement `quote=` for `writeGenalex`
* read from and write to Excel files
* add more tests
* implement makefile rule for building RData in data/
* exchange fake data for real data from DataDryad
