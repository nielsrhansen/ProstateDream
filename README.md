# R code for *Survival prognosis and variable selection: A case study for metastatic castrate resistant prostate cancer patients*

A paper written in relation to [The Prostate Cancer DREAM Challenge](https://www.synapse.org/ ProstateCancerChallenge).

### Requirements

Run the following R code to install required packages.

```
install.packages(
  c("survival",
    "xtable",
    "ggplot2",
    "reshape2",
    "mice",
    "mgcv",
    "timeROC",
    "Bolstad2",
    "ROCR",
    "randomForestSRC",
    "lattice",
    "RColorBrewer",
    "glmnet")
)
```

One additional package is required, which is not on CRAN, but is a fork of 
the gbm R package available from GitHub.

```
install.packages("devtools")
devtools::install_github("uci-cbcl/GBMCI")
```

### Running the code

The Rmd file `main.Rmd` in the directory Main includes the implementation of the `trainModels` function. This is 
the main training function, which relies on the implementations of a number of different functions 
for the imputation and the training of the different models. These implementations are found in the other subdirectories,
and sourced in the beginning of `main.Rmd`. 

Running `main.Rmd` (using knitr) loads and preprocesses the data, computes various 
summaries and constructs descriptive figures. It then runs the 5-fold cross-validation, stores the results in a file, 
refits all models to the entire data set and makes predictions on the validation data. It finally constructs plots 
of the selection probabilities for stability selection.

There are instructions in `main.Rmd` for setting seeds to obtain the exact same results as in the paper. The results from
the cross-validation are included in the files `mainResults12`, `mainResults1213` and `mainResults121314`. The validation
predictions for the different methods are stored in the directories 
ValidationResults and ValidationResults2 (two different seeds were used). The results from the validation predictions 
are stored in the files `ValidationResults.txt` and 
`ValidationResults2.txt`. Note that these validation results were obtained by submission of the predictions via 
the web interface for the competition. Thus the results cannot be programmatically reproduced. 

The Rmd file `results.Rmd` in the directory Paper reads the cross-validation and validation results and constructs 
various figures including those in the paper. 

### Misc.

The directory `Write-up` contains the [original write-up](https://www.synapse.org/#!Synapse:syn4260742/wiki/234778) submitted for the competition.

