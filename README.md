# HDP-r - Hierarchical Decision Process, implemented in R

This is being build out as a project for the [ETM department](https://www.pdx.edu/engineering-technology-management/) of [Portland State University (PSU)](https://www.pdx.edu/).

The original plan is up in [OneDrive](https://1drv.ms/w/s!AuSk0mi8WN0UgRMD_wWLMWE8LFDW), but I'll end up replacing that with this documentation by the end of the summer. 

This application will allow a user to:

-	Define an HDM model using R through the command line and a Shiny interface
-	Collect survey pairwise data from experts using Qualtrics
-	Evaluate and visualize results using R through command line and through a 
Shiny interface

Go [here](https://en.wikipedia.org/wiki/Hierarchical_decision_process) to read more about HDP.

## Usage

Documentation is generated with [roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) so you can ask R for help.  Also check out the examples folder for some working examples.

This section will be updated as more functionalty becomes usable.  

### Import and export models in JSON

HDP models can be defined in JSON (see definition documentation below). If you have a defined model import it. Once if you have a model that you're working with in R you can also export it. Here is an example:

```R
jsonFile = "C:/Users/chrdavi/source/repos/hdp-r/hdp-r-project/examples/testFiles/exampleModel.json"

hdpMod <- hdpr::hdp.model.import(jsonFile)
hdpr::hdp.model.export(hdpMod, "exportedExample")
```

## Model Definition

Models can be imported and exported using JSON.  The following JSON structure is used for model definition:

```JSON
{
  "title": "model title",
  "levels": [{
    "level": 1,
    "title": "level 1 title: Mission",
    "description": "level 1 description",
    "nodes": [{
      "name": "select best technology"
    }]
  },
  {
    "level": 2,
    "title": "level 2 title: Objective",
    "description": "level 2 description",
    "nodes": [{
      "name": "Cost",
      "description": "how much stuff costs"
    }]
  }
}
```

The model should have a title and optionally can have a description. The levels of bitwise comparison is an array, as are the nodes inside the levels. Each level and node has a mandatory title and an optional description. 

Note that the structure does not constrain the number of levels or nodes within each level.  However practically you likely want to limit the number of levels, as it will be hard to find experts that will spend the time evaluating your nodes if you put in to many.

## Qualtrics Integration

[Qualtrics](https://www.qualtrics.com/) is a great tool for managing surveys and is already used at many institutions and companies. Why would you build out a survey manager when one so awesome already exists? This project will export a defined HDP model to qualtrics and create a survey. You can then manage responses and experts there and import the results back into this tool.

There is already a [CRAN package the connects to Qualtrics](https://cran.r-project.org/web/packages/qualtRics/index.html) and qualtrics has a pretty extensive [API](https://api.qualtrics.com/).

## Previous work

There is [a tool](http://research1.etm.pdx.edu/hdm2/) used for HDP/HDM at PSU currently, however it's old and clunky, doesn't scale, and the code has been lost in time. This project borrows some of the interface structure from that project.

Not long ago [Jacob K. Yang](https://github.com/yajacob) built out a [Python version](https://github.com/yajacob/hdm_project) of HDM.  This project is borrowing the algorithms and some interface elements from that project as well.

