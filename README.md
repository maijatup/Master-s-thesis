# Master's thesis on oak regeneration
This repository contains the code for my Master's thesis on the long-term effects of conservation thinning on oak regeneration.
The thesis was done as part of the Swedish Oak Project at the University of Gothenburg (https://www.gu.se/en/research/the-swedish-oak-project).

## The scripts
**01_data_cleaning.R**: data cleaning and creating a new dataset used for the analysis

**02_exploratory_figures.R**: graphs to explore the data

**03_analysis_Q1.R**: modelling oak seedling density change over 22 years and the effect of conservation thinning (RQ1) using negative binomial GLMMs and estimated marginal means

**04_analysis_Q2.R**: modelling the effects of total basal area, oak basal area and canopy openness on oak seedling density (RQ2) using negative binomial GLMMs

**05_analysis_Q3.R**: modelling how the effects of total basal area, oak basal area and canopy openness differ between treatments (RQ2) using negative binomial GLMMs and estimated marginal means and trends

**06_analysis_Q4.R**: modelling the effects of competitor density and height on oak seedling density and height, and the effect of thinning (RQ3) using negative binomial GLMMs, LMMs and Gamma GLMMs

**07_general_results.R**: tables for general results section

Note: script numbering reflects the initial analysis structure, RQ2 in the thesis combines analyses from scripts 04 and 05.
