# manage input and output of models
#
# models are represented in JSON like this:
#{
#  "title": "model title",
#  "levels": [{
#    "level": 1,
#    "title": "level 1 title: Mission",
#    "description": "level 1 description",
#    "nodes": [{
#      "name": "select best technology"
#    }]
#  },
#  {
#    "level": 2,
#    "title": "level 2 title: Objective",
#    "description": "level 2 description",
#    "nodes": [{
#      "name": "Cost",
#      "description": "how much stuff costs"
#    }]
#  }
#}

hdp.model.import <- function() {
  print("importing...")
  #https://cran.r-project.org/web/packages/rjson/index.html
  #json_data <- fromJSON(file=json_file)
}

hdp.model.export <- function() {
  print("exporting...");
}
