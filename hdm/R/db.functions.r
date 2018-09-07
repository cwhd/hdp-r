##########################################################
# -> Helper functions to get and save stuff from the DB <-
##########################################################

#'Save an expert's evaluation to the DB
#'
#'Once an expert evaluates all of their options this function will save them
#'so we can aggregate them and use them later.
#'
#'@param data the results from the evaluation
#'@param expertId the ID of the expert to associate this evaluation with
#'@param modelId the ID of the model being evaluated
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

#'Save experts to MongoDB
#'
#'In HDM a group of domain experts are used to evaluate options. This function
#'will associate experts with a model and save them to a MongoDB.
#'
#'@param expertsJson the experts represented in JSON format
#'@param modelId the id of the model you're associating experts with
#'pass in all the experts as JSON with a modelID to update the DB
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

#'Save all data pertaining to the definition of a model
#'
#'@param data the full set of data to save in JSON format
#'@param id the ID of the model to update. If no ID is passed we will insert a record
#'@param collection TODO delete this I think
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

#'Load expert evaluations from a MongoDB
#'
#'@param modelId the ID of the model you want to load
#'@param expertId OPTIONAL get results from a specific expert
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

#' Utility to load all the models ids and names
#'
#' This is used in the Shiny Admin app to get all of the models so we can
#' Load existing models by ID
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

#'Utility to get a connection to a MongoDb
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

#'Return a data.frame that can be converted to a tree
#'with your model based on data retrieved from somewhere else.
#'
#'@param model definition from database
#'@return A \code{\link{data.tree}} containing the model
#'
#'@rdname RebuildDataFrameForHDMTree
#'@export
RebuildDataFrameForHDMTree <- function(mod) {
  froms <- eval(parse(text = mod$model$from))
  tos <- eval(parse(text = mod$model$to))
  pathStrings <- eval(parse(text = mod$model$pathString))

  goodDf <- data.frame(froms,tos,pathStrings)
}
