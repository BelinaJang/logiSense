#' Example data
#'
#' Data from a Kaggle dataset on stroke prediction.
#'
#' @docType data
#' @format A data frame with 5110 rows and 12 variables:
#' \describe{
#' \item{id}{unique identifier}
#' \item{gender}{"Male", "Female" or "Other"}
#' \item{age}{age of the patient}
#' \item{hypertension}{0 if the patient doesn't have hypertension, 1 if the patient has hypertension}
#' \item{heart_disease}{0 if the patient doesn't have any heart diseases, 1 if the patient has a heart disease}
#' \item{ever_married}{"No" or "Yes"}
#' \item{work_type}{"children", "Govt_jov", "Never_worked", "Private" or "Self-employed"}
#' \item{Residence_type}{"Rural" or "Urban"}
#' \item{avg_glucose_level}{average glucose level in blood}
#' \item{bmi}{body mass index}
#' \item{smoking_status}{"formerly smoked", "never smoked", "smokes" or "Unknown"}
#' \item{stroke}{1 if the patient had a stroke or 0 if not}
#' }
#' @source https://www.kaggle.com/datasets/fedesoriano/stroke-prediction-dataset
"test_data"
