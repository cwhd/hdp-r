#testing qualtrics import
# qualtrics API Key:
#"x-api-token" = "5NcMTFwikrMy7GoTu1y8rjaHoCZY8Ghx7ygqgTX2",

#https://portlandstate.qualtrics.com/API/v3/surveys


library(httr)

res <- GET("https://portlandstate.qualtrics.com/API/v3/surveys/SV_5zqlabeOGygUMCN", 
           add_headers("x-api-token" = "5NcMTFwikrMy7GoTu1y8rjaHoCZY8Ghx7ygqgTX2")
           )
http_status(res)

devtools::install_github("hrbrmstr/curlconverter")
library(curlconverter)
curlTest = "curl -H 'X-API-TOKEN: 5NcMTFwikrMy7GoTu1y8rjaHoCZY8Ghx7ygqgTX2' 'https://portlandstate.qualtrics.com/API/v3/surveys/SV_5zqlabeOGygUMCN' "
resp <- make_req(straighten(curlTest))
resp

curlPost = "curl -H 'X-API-TOKEN: 5NcMTFwikrMy7GoTu1y8rjaHoCZY8Ghx7ygqgTX2' -F 'name=Test' -F 'file=@/path/to/MySurvey.qsf;type=application/vnd.qualtrics.survey.qsf' 'https://portlandstate.qualtrics.com/API/v3/surveys'"
postRes <- make_req(curlPost)
postRes