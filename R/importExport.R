# manage input and output of models
# uses the rjson lib: https://cran.r-project.org/web/packages/rjson/index.html
require(rjson)

#' Import HDP Models into R from JSON
#'
#' This function allows you to import an HDP model defined in JSON. Please
#' check out th documentation on github.
#' @param json_file The file you're importing.
#' @keywords json, hdp, hdm
#' @export
#' @examples
#' hdp.model.import("hdpModel.json")
hdp.model.import <- function(jsonFile) {
  print("importing...")
  json_data <- fromJSON(file=jsonFile)
  return(json_data)
  #print(json_data)
}

#' Export HDP Models from R to JSON
#'
#' This function allows you to export an HDP model as a JSON file. Please
#' check out th documentation on github.
#' @param hdpModel An object representing your heirarchical model
#' @param file_name Name of the file to save. Will append .json to the end of it.
#' @keywords json, hdp, hdm
#' @export
#' @examples
#' hdp.model.export(hdpModel, "hdpModel.json")
hdp.model.export <- function(hdpModel, fileName) {
  print(paste("saving to ", fileName))
  jsonOutput <- toJSON(hdpMod)
  write(jsonOutput, paste(fileName, ".json"))
}
