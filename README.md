# HDP-r - Hierarchical Decision Process, implemented in R

This is being build out as a project for the [ETM department](https://www.pdx.edu/engineering-technology-management/) of [Portland State University (PSU)](https://www.pdx.edu/).

This application will allow a user to:

-	Define an HDM model using R through the command line and a Shiny interface
-	Collect survey pairwise data from experts using Qualtrics
-	Evaluate and visualize results using R through command line and through a 
Shiny interface

Go [here](https://en.wikipedia.org/wiki/Hierarchical_decision_process) to read more about HDP.

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


## Previous work

There is [a tool](http://research1.etm.pdx.edu/hdm2/) used for HDP/HDM at PSU currently, however it's old and clunky, doesn't scale, and the code has been lost in time. This project borrows some of the interface structure from that project.

Not long ago [Jacob K. Yang](https://github.com/yajacob) built out a [Python version](https://github.com/yajacob/hdm_project) of HDM.  This project is borrowing the algorithms and some interface elements from that project as well.

