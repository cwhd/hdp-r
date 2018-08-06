# this example shows how to import and export JSON definition models
#install.packages("C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/hdpr_0.0.1.zip", repos = NULL, type="source")

require(rjson)

jsonFile = "C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/examples/testFiles/exampleModel.json"
hdpMod <- hdpr::hdp.model.import(jsonFile)
print(hdpMod)

hdpr::hdp.model.export(hdpMod, "whatevs")

library(httr)
require(jsonlite)
jsonFile <- "C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/examples/testFiles/exampleModel.json"
modelFrame <- fromJSON(jsonFile)
head(modelFrame)
dim(modelFrame[1])

#use dim to find number of rows and cols

res <- POST("https://portlandstate.qualtrics.com/API/v3/surveys", add_headers("x-api-token" = "5NcMTFwikrMy7GoTu1y8rjaHoCZY8Ghx7ygqgTX2"), type="multipart/form-data", body = upload_file("C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/examples/testFiles/advancedTextFormat.txt"))
http_status(res)
content(res, "text")

install.packages("httpRequest")
library(httpRequest)
port <- 443 
params <- list(
 "name"="testapitxt", 
 "file"="C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/examples/testFiles/advancedTextFormat.txt") 
postToHost("https://portlandstate.qualtrics.com", "/API/v3/surveys", params, port=port)


install.packages("crul")
library("crul")
(x <- HttpClient$new(
  url = "https://portlandstate.qualtrics.com/API/v3/surveys",
  opts = list(
    timeout = 1
  ),
  headers = list(
    "x-api-token" = "5NcMTFwikrMy7GoTu1y8rjaHoCZY8Ghx7ygqgTX2",
    "content-type"="multipart/form-data",
    "content-disposition"="application/vnd.qualtrics.survey.txt"
  )
))

res <- x$post(
  path = "/API/v3/surveys", 
  body = list(
    file = "@advancedTextFormat.txt;type=application/vnd.qualtrics.survey.txt",
    "content-type"="application/vnd.qualtrics.survey.txt",
    name="testarooniroo"          
    )
)

res$parse()
