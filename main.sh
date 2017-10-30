#!/bin/bash

Rscript queries/portal/pageviews-referrers.R $1
Rscript queries/maps/prevalence.R $1
Rscript queries/maps/tiles.R $1
Rscript queries/portal/eventlogging.R $1
Rscript queries/portal/pageviews-platform.R $1
# Rscript plot.R $1
