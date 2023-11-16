

![Text Depot in action](www/TD_demo_short.gif)

<hr/>

<img src="www/text_depot_icon/TextDepotIcon_TextBeside_M.jpg" width="60%">

Text Depot is a tool to search and analyze topics of interest within a large database of text data. The Text Depot dashboard (this repo) provides a front-end to a set of indexes in ElasticSearch. To use this repository, you must provide one or more [Elastic Search](http://www.elastic.co) indexes in a particular format.

## Setup

1. Setup Elastic Search Server
2. Create one or more index using Text Depot mappings.
3. Clone this repo.
4. Run `cp .configs_sample .configs` and fill in the relevant values.
5. Build and run docker container:
```
    DOCKER_BUILDKIT=1 docker build -t text_depot_dashboard . && docker run -it -p 8080:3838 text_depot_dashboard
```
6. Open the dashboard on your browser: [http://localhost:8080](http://localhost:8080)

## Elastic Search

Each data source should be stored in its own Elastic Search index. For more information on how to configure your Elastic Search server, see [elasticsearch/](elasticsearch/)

## Notes

Our workflow contained the following components:

![Overall Workflow](workflow.png)

This repository contains the dashboard code (Blue above) for Text Depot. The green components were scheduled with cron jobs, and keep the indexes up-to-date in the ElasticSearch Database. We wrote a custom Parser for each data source, and a single Annotator class that adds the `[nieghbourhoods, sentiment, embeddings]` fields to each document and inserts them. The orange components were added for authentication and embeddings-based search, and are optional components. 

