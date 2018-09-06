#################################################
# -> Helper functions to get and save stuff from the DB
#################################################

saveEvaluation <- function(data, expertId, modelId) {
  dbInsert <- tryCatch({
    db <-   getDbConnection("evaluations")

    db$update(query = paste0('{ "expertId" : "',expertId,'", "modelId":"',modelId,'" }') ,
              update = paste0('{ "$set" : ',data,'}'),
              upsert = TRUE)
  },
  error = function(e) {
    print("ERROR inserting record!")
    print(e)
  })
}

#pass in all the experts as JSON with a modelID to update the DB
saveExperts <- function(expertsJson, modelId) {
  dbInsert <- tryCatch({
    db <- getDbConnection()
    print("-----saving experts")
    print(paste0("modelId: ",modelId ))
    print(paste0("experts: ",expertsJson))
    db$update(query = paste0('{ "modelId":"',modelId,'" }') ,
              update = paste0('{ "$set" : { "experts":',expertsJson,'}}'))


  }, error = function(e) {
    print(paste0("ERROR saving experts! ",e))
  })

}

#save all data from the admin app
saveData <- function(data, id, collection) {
  dbInsert <- tryCatch({
    db <- if(missing(collection)) {
      getDbConnection()
    } else {
      getDbConnection(collection)
    }
    if(!is.null(id)) {
      db$update(query = paste0('{ "_id" : {"$oid" : "',id,'"}}') ,
                update = paste0('{ "$set" : ',data,'}'))
    } else {
      db$insert(data)
    }
  },
  error = function(e) {
    print("ERROR inserting record!")
    print(e)
  })
}

#load up a model for the admin app
loadModel <- function(modelId) {
  # Connect to the database
  model <- tryCatch({
    db <- getDbConnection()
    data <- db$find(query = paste0('{"_id" : {"$oid":"',modelId,'"}}'))
    data
  },
  error=function(e) {
    print(paste0("ERROR loading model!",modelId))
    print(e)
    #TODO this is going to blow up, still need to handle it
    ""
  })
  model
}

loadResults <- function(modelId, expertId) {
  evaluations <- tryCatch({
    db <- getDbConnection("evaluations")
    if(missing(expertId)) {
      data <- db$find(query = paste0('{"modelId" : "',modelId,'"}'))
      data
    } else {
      data <- db$find(query = paste0('{"modelId" : "',modelId,'", "expertId":"',expertId,'"}'))
      data
    }
  },
  error=function(e) {
    print(paste0("ERROR loading model!",modelId))
    print(e)
    #TODO this is going to blow up, still need to handle it
    ""
  })
  evaluations
}

loadResultsIterator <- function(modelId) {
  evaluations <- tryCatch({
    db <- getDbConnection("evaluations")
    data <- db$iterate(query = paste0('{"modelId" : "',modelId,'"}'))
    data
  },
  error=function(e) {
    print(paste0("ERROR loading model!",modelId))
    print(e)
    #TODO this is going to blow up, still need to handle it
    ""
  })
  evaluations
}

#load up all the models ids and names for the list
loadAllModels <- function() {
  allModels <- tryCatch({
    db <- getDbConnection()
    # Read all the entries
    data <- db$find(
      query = "{}",
      fields = '{ "modelName" : true }'
    )
    data
  }, error=function(e) {
    print("ERROR loading all models")
    print(e)
    ""
  })
  allModels
}


getDbConnection <- function(collection) {
  collectionName <- if(missing(collection)) {
    "models"
  } else {
    collection
  }
  #local
  dataUri <- "mongodb://localhost/hdp"
  #for docker
  #dataUri <- "mongodb://hdpdb/hdp"
  db <- mongo(collection = collectionName,
              url = dataUri)
}

rebuildDataFrameForTree <- function(mod) {
  froms <- eval(parse(text = mod$model$from))
  tos <- eval(parse(text = mod$model$to))
  pathStrings <- eval(parse(text = mod$model$pathString))

  goodDf <- data.frame(froms,tos,pathStrings)

}

#################################################
# END mongo stuff
# if you want to use authentication with accounts, use this below
#  db <- mongo(collection = collectionName,
#              url = sprintf(
#                "mongodb://%s:%s@%s/%s",
#                options()$mongodb$username,
#                options()$mongodb$password,
#                options()$mongodb$host,
#                databaseName))
# AND put this up top:
#options(mongodb = list(
#  "host" = "localhost:27017",
#  "username" = "dev",
#  "password" = "Password1"
#))
#databaseName <- "hdp"
#
# I wanted to get ENV variables working in Docker, not sure why they don't:
#   #print(paste0("MONGO_URI:", Sys.getenv("MONGO_URI")))
#################################################
