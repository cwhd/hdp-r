#################################################
# -> mongo stuff
# function to get and save from the DB
#################################################

options(mongodb = list(
  "host" = "localhost:27017",
  "username" = "dev",
  "password" = "Password1"
))
databaseName <- "hdp"
collectionName <- "models"

dataUri <- "mongodb://hdpdb/hdp"

saveData <- function(data) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = dataUri)
  db$insert(data)
}

loadModel <- function(modelId) {
  # Connect to the database
  db <- mongo(collection = collectionName,
              url = dataUri)
  # get by Object Id: https://jeroen.github.io/mongolite/query-data.html#select-by-id
  data <- db$find(query = paste0('{"_id" : {"$oid":"',modelId,'"}}'))
  data
}

#load up all the models ids and names for the list
loadAllModels <- function() {
  db <- mongo(collection = collectionName,
              url = dataUri)
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