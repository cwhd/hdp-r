# HDP-r - Hierarchical Decision Process, implemented in R

This is being build out as a project for the [ETM department](https://www.pdx.edu/engineering-technology-management/) of [Portland State University (PSU)](https://www.pdx.edu/).

This application allows a user to:

-	Define an HDM model using a Shiny interface
-	Collect survey pairwise data from experts using a second shiny interface
-	Evaluate and visualize results 

Go [here](https://en.wikipedia.org/wiki/Hierarchical_decision_process) to read more about HDP.

## Usage

HDM allows researchers to define models consisting of a decision, critiera, factors, and alternatives. Once a model is defined it's validated and evaluated by experts in the field of the decision to be made; this allows the researcher to determine what is the best choice according to experts in the field. This tool manages the process from defining the model, to expert evaluation, to final calculation of results. 

Documentation is generated with [roxygen](https://cran.r-project.org/web/packages/roxygen2/vignettes/roxygen2.html) so you can ask R for help.  Also check out the examples folder for some working examples.

## Deployment

I used [docker](https://www.docker.com/) to make all the components deployable as a unit. Use the following commands to build the containers and get everything up and running:

- From the hdp-evaluate-shiny directory: docker build . -t hdp-eval
- From the hdp-admin-shiny directory: docker build . -t hdp-admin
- From the root: docker-compose up

MongoDB will be exposed inside the container cluster. 

If you don't know that much about Docker here are some useful commands:

- ssh into one of the nodes: docker exec -it hdp-admin bash

## Architecture

This is made of 3 parts as defined in the docker-compose file:

- hdp-admin: Where a user can define models and view results
- hdp-eval: where experts can evaluate models
- mongodb: I'm just using the default mongoDB container as a data store

## For testing:

http://127.0.0.1:7748/?modelId=5b85894efccdf91528004090&expertId=chris

## Previous work

There is [a tool](http://research1.etm.pdx.edu/hdm2/) used for HDP/HDM at PSU currently, however it's old and clunky, doesn't scale, and the code has been lost in time. This project borrows some of the interface structure from that project.

Not long ago [Jacob K. Yang](https://github.com/yajacob) built out a [Python version](https://github.com/yajacob/hdm_project) of HDM.  This project is borrowing some interface elements from that project.

