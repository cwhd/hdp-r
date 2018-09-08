##########################################################
# -> Helper functions to get or save data <-

# TODO I need to update these so the HDM module just worries about
# passing JSON back and forth. This should abstract away the DB
# so I can get this thing up into CRAN
##########################################################

#' Get an expert's evaluation in tree form
#'
#' Given an expertId and modelId, get the evaluation for that expert
#' and return it in a tree context
#'
#' @rdname getExpertResultsAsTree
#'
#' @example
#'getExpertResultsAsTree("5b85894efccdf91528004090","davis36@pdx.edu")
#'
#' @param modelId the modelId for the mode you're looking for
#' @param expertId the expertId for the expert you're looking for
#' @export
getExpertResultsAsTree <- function(modelId, expertId) {
  evalValues <- loadResults(modelId, expertId)
  expertValues.exists <- FALSE

  #if we have an eval, preload it
  if(nrow(evalValues) > 0) {
    froms <- eval(parse(text = evalValues$results$from))
    tos <- eval(parse(text = evalValues$results$to))
    pathStrings <- eval(parse(text = evalValues$results$pathString))
    evalWeights <- eval(parse(text = evalValues$results$weight))
    evalNorms <- eval(parse(text = evalValues$results$norm))
    sliderValues <- eval(parse(text = evalValues$results$sliderValues))

    goodDf <- data.frame(froms,tos,pathStrings, evalWeights, evalNorms, sliderValues)
    tree <- FromDataFrameNetwork(goodDf)

    tree

    } else {

    NULL
  }
}

#' Get the model in tree form with Alternatives as the bottom leaves
#'
#' Get a model in tree form. If there are alternatives add them to the bottom
#' of the tree.
#'
#' @rdname getModelAsTree
#'
#' @example
#' getModelAsTree("5b85894efccdf91528004090")
#'
#' @param modelId the modelId you're looking for
#' @export
getModelAsTreeWithAlternatives <- function(modelId) {
  alternatives <- NULL
  mod <- loadModel(modelId)
  #rebuildDataFrameForHDMTree(mod)
  alternatives <- eval(parse(text = mod$alternatives))

  tree <- FromDataFrameNetwork(rebuildDataFrameForHDMTree(mod))

  if(!is.null(alternatives)) {
    print("----adding new level of nodes to the tree...")
    bottomNodes <- getNodesAtLevel(tree, tree$height)
    lapply(1:nrow(bottomNodes),function(i) {
      lapply(1:length(alternatives), function(j) {
        FindNode(node=tree,name = bottomNodes[[i,"name"]])$AddChildNode(child=Node$new(trim(alternatives[[j]])))
      })
    })
  }

  tree
}

#' Get a full HDM model with alternatives and experts
#'
#' Based on a modelId get an ad-hoc class that has everything we
#' need for an HDM based model
#'
#' @param modelId the modelId that you want to get
getFullHDMModel <- function(modelId) {

  mod <- loadModel(modelId)

  alternatives <- eval(parse(text = mod$alternatives))
  modelName <- mod$modelName
  tree <- FromDataFrameNetwork(rebuildDataFrameForHDMTree(mod))

  experts <- if(!is.null(mod$experts)) {
    eval(parse(text = mod$experts))
  } else {
    NULL
  }

  hdm <- list(
    alternatives = alternatives,
    modelName = modelName,
    tree = tree,
    experts = experts
  )

  class(hdm) <- append(class(hdm),"HDM")

  return(hdm)
}

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
#'@rdname rebuildDataFrameForHDMTree
#'@export
rebuildDataFrameForHDMTree <- function(mod) {
  froms <- eval(parse(text = mod$model$from))
  tos <- eval(parse(text = mod$model$to))
  pathStrings <- eval(parse(text = mod$model$pathString))

  goodDf <- data.frame(froms,tos,pathStrings)
}
