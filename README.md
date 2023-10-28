# Model-agnostic unsupervised detection of bots in a Likert-type questionnaire

Michael John Ilagan and Carl F. Falk

This repository contains computer code for the article with the following citation:  
Ilagan, M.J. & Falk, C.F. (2023). Model-agnostic unsupervised detection of bots in a Likert-type questionnaire. _Behavior Research Methods_.

## Abstract

To detect bots in online survey data, there is a wealth of literature on statistical detection using only responses to Likert-type items. There are two traditions in the literature. 
One tradition requires labeled data, forgoing strong model assumptions. 
The other tradition requires a measurement model, forgoing collection of labeled data. 
In the present article, we consider the problem where neither requirement is available, for an inventory that has the same number of Likert-type categories for all items. 
We propose a bot detection algorithm that is both model-agnostic and unsupervised. 
Our proposed algorithm involves a permutation test with leave-one-out calculations of outlier statistics. 
For each respondent, it outputs a p-value for the null hypothesis that the respondent is a bot. 
Such an algorithm offers nominal sensitivity calibration that is robust to the bot response distribution. In a simulation study, we found our proposed algorithm to improve upon naive alternatives in terms of 95% sensitivity calibration and, in many scenarios, in terms of classification accuracy.

## Data

The raw data used in the simulation study is answers to the Humor Styles Questionnaire from the Open Psychometrics Project.  
[http://openpsychometrics.org/_rawdata/HSQ.zip](http://openpsychometrics.org/_rawdata/HSQ.zip)

## Instructions

To reproduce the results in the article, have all the R scripts in your working directory as well as the data file `data.csv` from the Open Psychometrics Project dataset.
Run `sim.R` to produce the R workspace (`badrespunsuperv-20230408.RData`) as well as the console log (`sim.Rout`).
Then run `figs-sim.R` and `figs-nonsim.R` to produce the Figures in PDF format.

For a demonstration on how to use our proposed method on your own data, see the contents of `workingexample/`.
