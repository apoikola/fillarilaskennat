# Code for getting weather data from the Finnish Meteorological Institute
# License: FreeBSD, http://en.wikipedia.org/wiki/BSD_licenses
# Copyright 2014 Antti Poikola, antti.poikola@gmail.com. All rights reserved.

# Follow the tutorial: (FMI) open data API client for R
# https://github.com/rOpenGov/fmi/blob/master/vignettes/fmi_tutorial.md

library(fmi)
request <- FMIWFSRequest(apiKey=apiKey)