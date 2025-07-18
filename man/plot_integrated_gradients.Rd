% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plotting_utils.R
\name{plot_integrated_gradients}
\alias{plot_integrated_gradients}
\title{Plotting Integrated Gradients Plots}
\usage{
plot_integrated_gradients(analysis_object, show_table = FALSE)
}
\arguments{
\item{analysis_object}{Fitted analysis_object with 'sensitivity_analysis(methods = "Integrated Gradients")'.}

\item{show_table}{Boolean. Whether to print Integrated Gradients summarized results table.}
}
\value{
analysis_object
}
\description{
The \strong{plot_integrated_gradients()} function replicates the SHAP visualization structure for integrated gradient
values, providing the same four graphical modalities adapted to this specific interpretability methodology for
neural networks. This function is particularly valuable for understanding feature importance in deep learning
architectures where gradients provide direct information about model sensitivity.
}
\examples{
# Note: For obtaining the Integrated Gradients plot the user needs to
# complete till sensitivity_analysis( ) function of the MLwrap pipeline
# using the Integrated Gradients method.

if (requireNamespace("torch", quietly = TRUE)) {

  \dontrun{

  wrap_object <- preprocessing(df = sim_data,
                             formula = psych_well ~ depression + emot_intel + resilience,
                             task = "regression")
  wrap_object <- build_model(wrap_object, "Neural Network")
  wrap_object <- fine_tuning(wrap_object, "Bayesian Optimization")
  wrap_object <- sensitivity_analysis(wrap_object, methods = "Integrated Gradients")

  # And then, you can obtain the Integrated Gradients plot.

  plot_integrated_gradients(wrap_object)

  }

}

}
