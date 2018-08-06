#Take a look at the Python version of the tool, get stuff out of it's DB


# Connect to th SQL Lite DB that the python tool uses
#install.packages("RSQLite") #perhaps needed
#install.packages("formattable")

# docs: https://cran.r-project.org/web/packages/RSQLite/RSQLite.pdf
library("RSQLite")
sqlite    <- dbDriver("SQLite")
pythonHdmDb <- dbConnect(sqlite,"C:/Users/chrdavi/source/repos/hdm_project/db.sqlite3")
dbListTables(pythonHdmDb)
hdm_evaluation <- dbReadTable(pythonHdmDb, "hdm_evaluation")
hdm_hdm <- dbReadTable(pythonHdmDb, "hdm_hdm")
#other examples:
#dbGetQuery(db, "SELECT * FROM CO2 WHERE conc < 100")
dbDisconnect(pythonHdmDb)

# format table: https://renkun-ken.github.io/formattable/
library(formattable)
formattable(hdm_hdm)
formattable(hdm_evaluation)

