# Common names sentiment
Sentiment analysis of IUCN Red List animal species English common names

This repository contains all the code used in the accepted manuscript:

Gregg, E. A., Bekessy, S. A., Martin, J. K., Garrard, G. E. "Many IUCN red list species have names that evoke negative emotions." *Human Dimensions of Wildlife* (Accepted, 2020) doi: [10.1080/10871209.2020.1753132]
(http://dx.doi.org/10.1080/10871209.2020.1753132)

**Abstract:** Species common names underpin communication between researchers, stakeholders and the public. Changing unappealing (e.g., rough-skinned horned toad), misleading (e.g., lesser bird of paradise) or even immemorable (e.g., little grassbird) species names could be an effective, and inexpensive, way to improve engagement with and support for threatened species. We use two sentiment lexicons to analyze the common names of 26,794 IUCN Red List animal species to understand which words drive sentiment in species names. Words driving common name sentiment varied across taxonomic class and threat status; highly frequent words associated with human emotions included anger, fear, disgust, and joy. We identified keywords for future targeted research on strategic name changes (e.g., greater, golden, least, lesser, false). This article provides essential grounding for future species common name research and improving public engagement with threatened species.

## Running the code

All analyses were run in 'R' v3.5.1. All code used to produce the results is included in this repository in the 'analysis.R' script, which sources some additional code and functions from R scipts in the 'R/' file.

The data used for the paper sourced from the IUCN Red List (2017) has been updated since analysis and is no longer available in the original format. The equivalent current dataset can be sourced directly from the IUCN Red List (https://www.iucnredlist.org/) according to its terms of use.

## Outline of repository items:

- 'analysis.R': R script used for data analysis
- 'R/': directory containing additional code and functions used in analysis (i.e. sourced within 'analysis.R' script)
- 'figs/': figures produced by the R code using original IUCN (2017) dataset
- 'LICENSE': license for the repository

## Citation

TBC
