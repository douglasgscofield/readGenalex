# For 1.0

o Consider switching to a full-generic interface (reduceGenalexPloidy -> reducePloidy.genalex).  I lose the ability to "silently" convert pre-1.0 data frames to class genalex, but if I make the old names stubs with instructions to convert with as.genalex and then use the new interface, that might be fine.  I don't have that many users anyway.
o Add used papers to the data description?
o Functions for converting from/to some other genetic formats, attaching them to as.genalex
o If I am converting from those other formats, do I need to have the specific packages loaded?
o Initial dispersalDiversity package should that be based on class 'genalex'-enabled readGenalex
o check on any implicit version dependencies
o document version dependencies
o read from and write to Excel files
o add more tests
o complete checkNullAllele integration
o add checkNullAllele return of matrices indicating possible null alleles
o make null allele demo using createGenalex(), and also using adult and pericarp Q. agrifolia data?
x `createGenalex()`, for creating a de-novo class `'genalex'` data frame
x drawing on writeGenalex documentation about differences when writing, be more explicit about what happens with extra columns and with extra column names on input
x straighten out test for writeGenalex with and without quote=
x GenAlEx says all genotype data is numeric, so code it as numeric after reading
x Is `as` a generic and `as.genalex` a method?
x Add summary.genalex method
x Update README for new data sets
x Fix WARNING during make check, probably because of the following:
x Split data up into two separate RData
x implement makefile rule for building RData in data/
x exchange fake data for real data from DataDryad
x how to add citation instructions to data?  see data/datasource for that info
x create an S3 class "genalex" based on data.frame and modify is.genalex to check for that (or could we then use the generic?)  I ended up creating as.genalex and using this internally where the class will be returned.
x Implementing class "genalex" needs to bump version to 1.0, since there would be API changes.
x For 1.0, recognise pre-1.0 genalex data frames and convert automatically (on option of course) to 1.0 class.
x Did above by adding as.genalex
x implement `quote=` for `writeGenalex`
x add null allele check to this package

## internal work

x concatenate all files into one R source file
x rethink method strategy
