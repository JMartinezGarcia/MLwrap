% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/table_utils.R
\name{table_shap_results}
\alias{table_shap_results}
\title{SHAP Summarized Results Table}
\usage{
table_shap_results(analysis_object, show_table = FALSE)
}
\arguments{
\item{analysis_object}{Fitted analysis_object with 'sensitivity_analysis(methods = "SHAP")'.}

\item{show_table}{Boolean. Whether to print the table.}
}
\value{
Tibble or list of tibbles (multiclass classification) with SHAP summarized results.
}
\description{
The \strong{table_shap_results()} function processes previously calculated SHAP (SHapley Additive exPlanations)
values and generates summarized metrics including mean absolute value, standard deviation of mean absolute
value, and a directional sensitivity value calculated as the covariance between feature values and SHAP
values divided by the variance of feature values. This directional metric provides information about the
nature of the relationship between each variable and model predictions. To summarize the SHAP values
calculated, three different metrics are computed:
\itemize{
\item \strong{Mean Absolute Value}
\item \strong{Standard Deviation of Mean Absolute Value}
\item \strong{Directional Sensitivity Value} (Cov(Feature values, SHAP values) / Var(Feature values))
}
}
\examples{
# Note: For obtaining the table with SHAP method results the user needs
# to complete till sensitivity_analysis() function of the
# MLwrap pipeline using the SHAP method.

\dontrun{

wrap_object <- preprocessing(df = sim_data,
                             formula = psych_well ~ depression + emot_intel + resilience,
                             task = "regression")
wrap_object <- build_model(wrap_object, "Random Forest")
wrap_object <- fine_tuning(wrap_object, "Bayesian Optimization")
wrap_object <- sensitivity_analysis(wrap_object, methods = "SHAP")

# And then, you can obtain the SHAP results table.

table_shap <- table_shap_results(wrap_object)

}

}
