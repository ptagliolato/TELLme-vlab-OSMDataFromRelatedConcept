[![DOI](https://zenodo.org/badge/DOI/10.5281/zenodo.3741895.svg)](https://doi.org/10.5281/zenodo.3741895)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](http://www.gnu.org/licenses/gpl-3.0)

TELLme-vlab-OSMDataFromRelatedConcept
=================

## Description
Project TELLme Erasmus + Virtual Lab tool (shiny app). 
The app let the user to select and download data from Open Street Map on the basis of the TELLme framework.
Concepts expressed in the TELLme Glossary are associated to specific OSM Features. 
The tool facilitates the operations for obtaining TELLme-framework-related data used to compose maps in the project. 

## Usage
1. Select a Concept from the dropdown list. 
2. Select the bounding box of the zone of interest. 
3. Download all the OSM features related to the Concept in form of zipped shapefiles.

### Usage with DOCKER
Build your own docker image

    sudo docker build -t tellme-vlab-osmdatafromrelatedconcept .

or pull a ready-to-use image from docker hub registry

    sudo docker pull ptagliolato/tellme-vlab-osmdatafromrelatedconcept[:<version>]

Prepare your env file: copy the env.example file in this folder, replace the variable values and save it with the name you prefer (e.g. "env")

Run a new container (exposing the app e.g. on port 8080, or choose a different one)

    sudo docker run --env-file=env -p 8080:3838 [ptagliolato/]tellme-vlab-osmdatafromrelatedconcept[:<version>]

### Guidelines for metadata lineage field of downloaded files
OpenStreetMap data relative to TELLme Project Glossary Keywords and Related Concepts, obtained via [Overpass API](https://wiki.openstreetmap.org/wiki/Overpass_API) through TELLme Project Virtual Lab tool 
(DOI: 10.5281/zenodo.3741895)

## Meta
* Please [provide any issue](https://github.com/ptagliolato/TELLme-vlab-OSMDataFromRelatedConcept/issues), or email tagliolato.p(at)irea.cnr.it

## Credits
The software is developed within the TELLme ERASMUS+ project Gran Agreement Number:2017-1-IT02-KA203-036974, output O4.

* The app is being developed by Paolo Tagliolato, Alessandro Oggioni ([IREA-CNR](http://www.irea.cnr.it)), 
* This code is released under the [GNU General Public License version 3](https://www.gnu.org/licenses/gpl-3.0.html) (GPL‑3).
* Please get citation information for TELLme-vlab-OSMDataFromRelatedConcept at [DOI:10.5281/zenodo.3741895](https://doi.org/10.5281/zenodo.3741895)
``` bibtex
```

### Acknowledgments
The authors wish to acknowledge Iacopo Neri and Valentina Galiulo ([PoliMi](https://www.polimi.it/)) for their insights and contribution.
Open Street Map
Data downloaded by this app are from [© OpenStreetMap contributors](https://www.openstreetmap.org/copyright) and are available under the [ODbL license](www.openstreetmap.org/copyright or www.opendatacommons.org/licenses/odbl). This product is not endorsed by or affiliated with the OpenStreetMap Foundation. 

