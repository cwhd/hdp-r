# this example shows how to import and export JSON definition models
#install.packages("C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/hdpr_0.0.1.zip", repos = NULL, type="source")



require(rjson)

jsonFile = "C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/examples/testFiles/exampleModel.json"

hdpMod <- hdpr::hdp.model.import(jsonFile)

hdpr::hdp.model.export(hdpMod, "whatevs")
