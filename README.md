[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3741896.svg)](https://doi.org/10.5281/zenodo.3741896)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

TELLme-vlab-OSMDataFromRelatedConcept
=================

## Description
Project TELLme Erasmus + Virtual Lab tool (shiny app). 
The app let the user to select and download data from Open Street Map on the basis of the TELLme framework.
Concepts expressed in the TELLme Glossary are associated to specific OSM Features. 
The tool facilitates the operations for obtaining TELLme-framework-related data used to compose maps in the project. 

## Usage
Select a Concept from the dropdown list. 
Select the bounding box of the zone of interest. 
Download all the OSM features related to the Concept in form of zipped shapefiles.

### Usage with DOCKER
## Build your own docker image
sudo docker build -t tellme-vlab-osmdatafromrelatedconcept .

or pull a ready-to-use image from docker hub registry

sudo docker pull ptagliolato/tellme-vlab-osmdatafromrelatedconcept[:<version>]

Prepare your env file: copy the env.example file in this folder, replace the variable values and save it with the name you prefer (e.g. "env")

Run a new container on port 80
sudo docker run --env-file=env -p 80:3838 [ptagliolato/]tellme-vlab-osmdatafromrelatedconcept[:<version>]

## Meta
* Please [provide any issue at](https://github.com/ptagliolato/TELLme-vlab-OSMDataFromRelatedConcept/edit/master/README.md), or email tagliolato.p(at)irea.cnr.it
* License: The app is being developed by Paolo Tagliolato, Alessandro Oggioni ([IREA-CNR](http://www.irea.cnr.it)), and Iacopo Neri ([PoliMi](https://www.polimi.it/)) and it is released under the [GNU General Public License version 3](https://www.gnu.org/licenses/gpl-3.0.html) (GPL‑3).
* Get citation information for TELLme-vlab-OSMDataFromRelatedConcept
``` bibtex
```
