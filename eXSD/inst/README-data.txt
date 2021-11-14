
Exported data should be in data/
Remember that R saves the name as well:

sampleDataPath <- sprintf("DEXplorer/data/sample_data.Rdata", packageDir)
sample_data <- data.frame(
  var1=c(9,10),
  var2=c(111,122)
)
save(sample_data, file=sampleDataPath)

To document the file, add an entry in /R/data.R
  
