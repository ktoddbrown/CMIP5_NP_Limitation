Limiting CMIP5 land carbon cycle with nitrogen and phosphorus post hoc 
===================

This code base is associated with Wieder et al. Future productivity and carbon storage are limited by terrestrial nutrient availability. Nature Geoscience (2015) and the manuscript should be considered the primary citation.

Purpose
-----------------
The purpose of this code base was to impose a nutrient constraint on the land carbon component of Earth system models from CMIP5 (http://cmip-pcmdi.llnl.gov/cmip5/). 

This repo does not include any datasets used in this study, primarily because these were not a product of this study and can be obtained elsewhere. Included in this repo are the following primary scripts:

1. Python/
  + NtrInputs_Resampling.py: Generate the nutrient input estimations
2. R/
  + calcNPPLimit.R: Main script for imposing post hoc nutrient constraints on the land carbon cycle.
  + Fig1-S7_Wieder_NatGeo2015.R: Generate figures for the manuscript
3. NCL/
  + Fig2_S2-S5_S8-10_Weider.ncl: Generate figures for the manscript

Earth system model simulation outputs can be downloaded from here: http://cmip-pcmdi.llnl.gov/cmip5/ and processing scripts are in R/lib.

**Attributions**: W. Kolby Smith (wkolby) processed the nutrient input estimations. Katherine Todd-Brown (ktoddbrown) processed the CMIP5 simulation outputs. William Wieder (wwieder) developed the visualizations.

Licence
-----------------
The MIT License (MIT)

Copyright (c) 2015 Katherine Todd-Brown, William Wieder, W. Kolby Smith

Permission is hereby granted, free of charge, to any person obtaining a copy of this software and associated documentation files (the "Software"), to deal in the Software without restriction, including without limitation the rights to use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to whom the Software is furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.
