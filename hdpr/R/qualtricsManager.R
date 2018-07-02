#' Create a survey in qualtrics
#'
#' Create a new survey in qualtrics based on an HDP model
#' @param model the model to create
#' @param apiKey your qualtrics API key
#' @keywords qualtrics
#' @export
#' @examples
#' hdp.qualtrics.createSurvey(hdpModel, 824752849jk4h49)
hdp.qualtrics.createSurvey <- function(jsonFile) {
  print("creating survey...")
  #use this API to create a new survey
}

#' Get data from a qualtrics survey for your HDP model
#'
#' @param model model to get data for
#' @param apiKey your qualtrics API key
#' @keywords qualtrics
#' @export
#' @examples
#' hdp.qualtrics.getSurveyData(hdpModel, 824752849jk4h49)
hdp.qualtrics.getSurveyData <- function() {
  #get data out of qualtrics. I think we can use this: qualtRics
  #https://cran.r-project.org/web/packages/qualtRics/index.html
}
