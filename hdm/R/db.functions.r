##########################################################
# -> Helper functions to get or save data <-

# TODO I need to update these so the HDM module just worries about
# passing JSON back and forth. This should abstract away the DB
# so I can get this thing up into CRAN
##########################################################

#' Get an expert's evaluation in tree form
#'
#' Given an expertId and modelId, get the evaluation for that expert
#' from the DB and return it in a tree context
#'
#' @rdname getExpertResultsAsTreeFromDb
#'
#' @example
#'getExpertResultsAsTreeFromDb("5b85894efccdf91528004090","davis36@pdx.edu")
#'
#' @param modelId the modelId for the mode you're looking for
#' @param expertId the expertId for the expert you're looking for
#' @export
getExpertResultsAsTreeFromDb <- function(modelId, expertId) {
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
#' @rdname getModelAsTreeWithAlternativesFromDb
#'
#' @example
#' getModelAsTreeWithAlternativesFromDb("5b85894efccdf91528004090")
#'
#' @param modelId the modelId you're looking for
#' @export
getModelAsTreeWithAlternativesFromDb <- function(modelId) {
  alternatives <- NULL
  mod <- loadHDMModel(modelId)
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
#' @rdname getFullHDMModelFromDb
#'
#' @param modelId the modelId that you want to get
#' @export
getFullHDMModelFromDb <- function(modelId) {

  mod <- loadHDMModel(modelId)

  alternatives <- eval(parse(text = mod$alternatives))
  modelName <- mod$modelName
  tree <- FromDataFrameNetwork(rebuildDataFrameForHDMTree(mod))
  userEmail <- mod$userEmail
  pin <- mod$pin

  experts <- if(!is.null(mod$experts)) {
    if(length(mod$experts) < 1) {
      eval(parse(text = mod$experts))
    } else {
      mod$experts
    }
  } else {
    NULL
  }

  hdm <- list(
    alternatives = alternatives,
    modelName = modelName,
    tree = tree,
    experts = experts,
    userEmail = userEmail,
    pin = pin
  )

  class(hdm) <- append(class(hdm),"HDM")

  return(hdm)
}

#' Save an expert's evaluation to the DB
#'
#' Once an expert evaluates all of their options this function will save them
#' so we can aggregate them and use them later.
#'
#' @rdname saveHdmEvaluationToDb
#'
#' @param data the results from the evaluation
#' @param expertId the ID of the expert to associate this evaluation with
#' @param modelId the ID of the model being evaluated
#' @export
saveHdmEvaluationToDb <- function(data, expertId, modelId) {
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

#'Save all data pertaining to the definition of a model to MongoDB
#'
#'Send in JSON data, this will save it to MongoDB. I love MongoDB because it's
#'web scale: https://www.youtube.com/watch?v=b2F-DItXtZs If you don't for some
#'reason then feel free to submit a PR to this repo for something else.
#'
#'@param data the full set of data to save in JSON format
#'@param id the ID of the model to update. If no ID is passed we will insert a record
#'@export
saveDataToMongoDb <- function(data, id) {
  dbInsert <- tryCatch({
    db <- getDbConnection()
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

#' Get all the expert results and roll them up for display
#'
#' Get all of the experts, calculate the summary stats across them,
#' return a nice matrix for display.
#'
#' @rdname getExpertEvaluationRollup
#'
#' @param experts collection of experts to get data for
#' @param modelId modelId that you're working with
#' @export
getExpertEvaluationRollup <- function(experts, modelId) {
  expertFlatResults <- lapply(1:length(experts), function(i) {
    evaluations <- loadResults(modelId, experts[i])

    if(nrow(evaluations) > 0) {
      nodes <- eval(parse(text = evaluations$flatResults$pathString))
      evalWeights <- eval(parse(text = evaluations$flatResults$weight))
      evalNorms <- eval(parse(text = evaluations$flatResults$norm))
      sliderValues <- eval(parse(text = evaluations$flatResults$sliderValues))
      #TODO add means and stuff to the end of this data table...

      goodDf <- data.frame(nodes, evalWeights)
      colnames(goodDf) <- c("Criteria")
      goodDf
    }
  })
  expertFlatResults <- compact(expertFlatResults) #remove any missing ones
  print(expertFlatResults)
  #build the matrix for the final results
  flippedExpertResults <- lapply(1:length(expertFlatResults), function(i) {
    f <- t(expertFlatResults[[i]][-1])
    colnames(f) <- expertFlatResults[[i]][,1]
    rownames(f) <- experts[i]
    f
  })

  flippedExpertResults
}

#' Get the combo frames for an expert
#'
#' Use this when you want to get the bitwise comparisons for an expert.
#'
#' @param expertId the expert you're looking for
#' @param modelId the model you're looking for
getExpertEvaluationComboFrames <- function(expertId, modelId) {
  evaluations <- loadResults(modelId, expertId)
  comboFrames <- evaluations$comboFrames
  comboFrames
}

#' Load a model from the DB
#'
#' Internal function that makes the MongoDB Query
#'
#' @param modelId the modelId that you're looking for
loadHDMModel <- function(modelId) {
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
#' Internal function to load up raw resuls from the DB
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

#' Load modelIds and names for a particular user.
#'
#' Use this to get model identifiers  for models created by a particular user.
#' Note that I'm not
#' using real paswords, I'm not encrypting anything, and I'm not enforcing
#' much security. This is really just to keep users from messing with each
#' other's models on a shared tenant. If you want better security then you might
#' consider putting a proper authentication server in front of a user app or
#' something like that.
#'
#' @rdname loadMyModelsFromDb
#'
#' @param userEmail the email of the user who build the models
#' @param pin acts like a password, only is not really secure
#' @export
loadMyModelsFromDb <- function(userEmail, pin) {

      allModels <- tryCatch({
      db <- getDbConnection()
      # Read all the entries
      data <- db$find(
        query = paste0('{ "userEmail" : "',userEmail,'", "pin":"',pin,'" }'),
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

#' Utility to load all the models ids and names
#'
#' Even though I use this, it's best not to expose it because you don't want
#' user's messing with each other's models. I think it's ok in an evironment of
#' total trust.
#'
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
