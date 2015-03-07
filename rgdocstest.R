require(RCurl)
mycsv <- getURL("https://docs.google.com/spreadsheet/pub?key=0AjCA21_nXUYYdHNHMU1BdmJSLXM1ZnhnRkN2dnB0V0E&single=true&gid=0&range=A1%3AC6&output=csv")
mydata <- read.csv(textConnection(mycsv))