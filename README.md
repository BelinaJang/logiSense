# logiSense

`logiSense` is an R package designed to simplify and make sense of logistic regression results.  It provides concise and human-readable outputs for both continuous and categorical variables, making it easier to derive insights from your logistic regression models.

<div align="center">
  <img src="https://github.com/BelinaJang/logiSense/blob/main/images/logiSense-logo.png" alt="Logo" width="250">
</div>




## Motivation
Interpreting logistic regression results is challenging due to coefficients being presented on the log-odds scale, which can confuse non-statisticians when translating the results into probabilities or understanding predictor effects. From reviewing existing literature, a paper by [Rajeev Kumar Malhotra](https://doi.org/10.4103/ijcm.IJCM_16_20) highlights critical errors in interpreting odds ratios, including treating odds ratios as direct probabilities or relative risks, leading to exaggerated conclusions about associations.


## Installation
You can install the most recent version of `logiSense` from Github using the following commands:
```
install.packages("devtools")
library(devtools)
devtools::install_github("BelinaJang/logiSense")
library(logiSense)
```

## Functions
The package contains two functions:

### 1. `logis`
   - Provides logistic regression interpretation for models **without** interaction terms.

### 2. `logint`
   - Provides logistic regression interpretation for models with **one** interaction term.



## Arguments
The `logis` and `logint` functions in `logiSense` require specific arguments, including the model formula, dataset, and variables of interest, to provide clear and interpretable outputs for both continuous and categorical predictors.

### Description of Parameters for `logis`

| Parameter          | Definition                               | Type       |
|--------------------|------------------------------------------|------------|
| `formula`          | Description of the model to be fitted    | formula    |
| `data`             | Name of the data                        | data frame |
| `variable_interest`| Variable name interested for interpretation | character |
| `variable_type`    | Type of variable of interest             | character  |
| `sigfig`    | Number indicating the number of significant digits to be used | integer  |

### Description of Parameters for `logint`

| Parameter          | Definition                               | Type       |
|--------------------|------------------------------------------|------------|
| `formula`          | Description of the model to be fitted    | formula    |
| `data`             | Name of the data                        | data frame |
| `variable1`  | Name of the first variable in the interaction of interest | character |
| `vairiable2`   | Name of the second variable in the interaction of interest | character |
| `vairiable1_type`   | Type of the first variable in the interaction of interest | character |
| `vairiable2_type`   | Type of the second variable in the interaction of interest | character |
| `sigfig`    | Number indicating the number of significant digits to be used.| integer  |



## Example 
The examples below demonstrate the use of `logis` and `logint` functions with a [stroke prediction dataset](https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset) from Kaggle, showcasing both categorical and continuous variables.


### `logis` (no interaction term):
#### Continuous variable:
```
library(logiSense)
library(here)

test_data <- read.csv(here("data/test_data.csv"))

logis(formula = stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + smoking_status,
                    data = test_data,
                    variable_interest = "age",
                    variable_type = "continuous",
                    sigfig=4)
```
#### Categorical variable:
```
logis(formula = stroke ~ gender + age + hypertension + heart_disease + avg_glucose_level + smoking_status,
                    data = test_data,
                    variable_interest = "smoking_status",
                    variable_type = "categorical",
                    sigfig = 4)
```

### `logint` (two-way interaction):
#### Continuous &times; Categorical:
```
logint(formula = stroke ~ work_type * age,
                  variable1 = "age",
                  variable2 = "work_type",
                  variable1_type = "continuous",
                  variable2_type = "categorical",
                  data = test_data,
                  sigfig = 4)
```
#### Continuous &times; Continuous:
```
logint(formula = stroke ~ age * avg_glucose_level,
                  variable1 = "age",
                  variable2 = "avg_glucose_level",
                  variable1_type = "continuous",
                  variable2_type = "continuous",
                  data = test_data,
                  sigfig = 4)
```
#### Categorical &times; Categorical:
```
logint(formula = stroke ~ work_type * Residence_type,
                  variable1 = "work_type",
                  variable2 = "Residence_type",
                  variable1_type = "categorical",
                  variable2_type = "categorical",
                  data = test_data,
                  sigfig = 4)
```

## Comparison with Existing Packages
While existing R packages focus on model performance and visualization, `logiSense` uniquely addresses the gap in direct and comprehensive interpretation of logistic regression results, as summarized in the comparison below.

| Feature                        | modelsummary | sjPlot | gtsummary | performance | logiSense |
|--------------------------------|--------------|--------|-----------|-------------|-----------|
| Human-readable coefficients    | ✅            | ✅      | ✅         | ❌           | ✅         |
| Odds ratio interpretation       | ✅            | ✅      | ✅         | ❌           | ✅         |
| User-friendly outputs           | ❌            | ✅      | ✅         | ❌           | ✅         |
| Model performance diagnostics   | ❌            | ❌      | ❌         | ✅           | ❌         |
| Direct interpretation tools     | ❌            | ❌      | ❌         | ❌           | ✅         |



## Presentation
You can find the detailed presentation on how to use `logiSense` [here](https://rpubs.com/victoriatruong/logiSense).

## Credits
`logiSense` was developed by [Anna (Jingxuan) He](https://github.com/jingxuan-anna-he), [Belina Jang](https://github.com/BelinaJang), [Vanessa Liao](https://github.com/vnssyl), [Tina (Zhaoyu) Tan](https://github.com/ZhaoyuTan), & [Victoria Truong](https://github.com/victoriaktruong), as part of the course CHL 8010: Statistical Programming and Computation for Health Data, taught by Aya Mitani at the University of Toronto in 2024. Special thanks to Aya for her guidance and support!
