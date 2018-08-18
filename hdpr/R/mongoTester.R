
#################################################
# -> mongo stuff
#################################################
options(mongodb = list(
  "host" = "localhost:27017",
  "username" = "dev",
  "password" = "Password1"
))
databaseName <- "hdp"
collectionName <- "models"

saveData <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Insert the data into the mongo collection as a data.frame
  #data <- as.data.frame(t(data))
  #print("Saving data structure...")
  #str(data)
  db$insert(data)
}

loadModel <- function(modelId) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # get by Object Id: https://jeroen.github.io/mongolite/query-data.html#select-by-id
  data <- db$find(query = paste0('{"_id" : {"$oid":"',modelId,'"}}'))
  data
}

#load up all the models ids and names for the list
loadAllModels <- function() {
  db <- mongo(collection = collectionName,
              url = sprintf(
                "mongodb://%s:%s@%s/%s",
                options()$mongodb$username,
                options()$mongodb$password,
                options()$mongodb$host,
                databaseName))
  # Read all the entries
  data <- db$find(
    query = "{}",
    fields = '{ "modelName" : true }'
  )
  data
}

#################################################
# END mongo stuff
#################################################


#5b72148ffccdf9560000373d
testDf <- loadModel("5b72148ffccdf9560000373d")
froms <- eval(parse(text = testDf$model$from))
tos <- eval(parse(text = testDf$model$to))
pathStrings <- eval(parse(text = testDf$model$pathString))

goodDf <- data.frame(froms,tos,pathStrings)

  
  
