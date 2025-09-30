# Food, Mammals, & Countries: <br> <br> Stimuli, their Pairwise Similarities and MDS-Dimension Values for Modeling Judgment, Categorization, and Memory in three Real-World Domains

<br>

## About this repository

This repository contains the data, materials, and the code to reproduce all analyses and figures in the project *Food, Mammals, & Countries: Stimuli, their Pairwise Similarities and MDS-Dimension Values for Modeling Judgment, Categorization, and Memory in three Real-World Domains*. The structure of this repository is as follows:

<br>

## Folder Structure:


<br>

- **Data/** contains the data files for each domain in the respective folder. Specifically:
    - The files **`data_similarity_*.csv`** contain the raw and unprocessed full data sets.
    - The files **`data_analysis_similarity_*.csv`** contain the filtered and cleaned data used for all analysis. These data sets are the results of the scripts **`prepare_data_*.R`**.
    - The files **`estimation_data.csv`** contains the processed data from the estimation experiment, where participants first learned to estimated a continuous criterion for each domain for some exemplars and then estimated this criterion for all 80 items (see **`XXX.pdf`** for more details).
  
  <br>
  See the corresponding codebooks for more details about each data set.

<br>

- **Materials/** contains all the stimuli used in the experiment. 
  - The files `stimlist_*.csv` list all the 80 stimuli of each domain and the corresponding image name.
  - The subfolders **Countries**/**Food**/**Mammals** contain the corresponding stimuli images of each domain.  The folder **Reduce Quality** contains copies with reduced quality of these images, mainly used for plotting.

<br>


- **Scripts/** holds the `R`and `Quarto` files used to conduct the analysis reported in the manuscript. Specifically:
    - **`helper_functions.R`** contains code of some useful R helper functions.
    - **`plot_settings.R`** contains the default ggplot-theme used for all figures.
    - **`0_analysis_DEMOGRAPHICS.qmd`** computes and reports the demographics of the final sample based on the `data_analysis_similarity_*.csv` data files.
    - **`1_analysis_DESCRIPTIVE_SIMILARITY.qmd`** computes the descriptive statistics of similarity ratings and produces the figure [pairwise_similarities.pdf](Figures/pairwise_similarities.pdf)
    - **`2_analysis_MDS.qmd`** runs the MDS analysis reported in the manuscript and produces figure [observed_predicted_dists.pdf](Figures/observed_predicted_dists.pdf) and [example_dimensions.pdf](Figures/observed_predicted_dists.pdf). Results from this script are strored inthe folder `Results`.
    - **`3_analysis_VALIDATION.qmd`** runs the proof-of-relevance analysis reported in the manuscript and produces figure [distr_adjR2.pdf](Figures/distr_adjR2.pdf)



<br>
    
- **Results/** contains the results of the MDS analysis (loo-cv and final configurations) based on the script `2_analysis_MDS.qmd`


<br>

- **Plots/** includes all figures in the manuscript. 

<br>


This work, including all figures, is licensed under a <a rel="license" href=" https://creativecommons.org/licenses/by-nc-sa/4.0/ ">CC BY-NC-SA 4.0</a>.  All code is licensed under the MIT License.

<br>

## Contributing Authors
David Izydorczyk & Arndt Bröder


## Abstract
Three comprehensive sets of naturalistic stimuli, each with pairwise similarity ratings and multidimensional scaling (MDS)–based feature representations, are presented and freely available under a CC BY-NC-ND license (https://github.com/dizyd/Similarity-Datasets). The sets include 80 representative items from three domains — foods, mammals, and countries — that can be used across cognitive research areas such as categorization, multiple-cue probability learning, judgment, decision-making, memory, and metamemory. Based on over 280,000 similarity judgments from $N = 1,798$ participants, we derived 14 feature dimensions for foods, 10 for mammals, and 10 for countries, which reconstructed the similarity spaces with high accuracy ($r \geq .93$). In a proof-of-concept study, these dimensions also explained substantial variance in participants’ numerical judgments of domain-specific criteria, underscoring their functional relevance beyond similarity tasks. By making these stimuli, similarity data, and derived feature spaces freely available, we provide researchers with tools for testing computational cognitive models in relevant real-world domains, each of which allows for examining a multitude of judgment or classification criteria.

## Publication
(work in progress)

## Funding
This research was funded by Grant IZ 96/1-1 provided to David Izydorczyk from the German Research Foundation (DFG) and supported by the University of Mannheim’s Graduate School of Economic and Social Sciences.




## Session Info
```
R version 4.4.2 (2024-10-31 ucrt)
Platform: x86_64-w64-mingw32/x64
Running under: Windows 11 x64 (build 26100)
  
packages:
 [1] ggimage_0.3.3      ggrepel_0.9.6      papaja_0.1.3       tinylabels_0.2.4  
 [5] correlation_0.8.6  smacof_2.1-7       e1071_1.7-16       colorspace_2.1-1  
 [9] plotrix_3.8-4      MDShelper_0.0-4    viridis_0.6.5      viridisLite_0.4.2 
[13] wordcloud_2.6      RColorBrewer_1.1-3 ggwordcloud_0.6.2  tm_0.7-15         
[17] NLP_0.3-2          parameters_0.24.1  psych_2.4.12       kableExtra_1.4.0  
[21] knitr_1.50         patchwork_1.3.0    lubridate_1.9.4    forcats_1.0.0     
[25] stringr_1.5.1      dplyr_1.1.4        purrr_1.0.2        readr_2.1.5       
[29] tidyr_1.3.1        tibble_3.2.1       ggplot2_3.5.2      tidyverse_2.0.0 

```
