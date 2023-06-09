% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot_repsd.R
\name{plot_repsd}
\alias{plot_repsd}
\title{REPSD Null vs Observed Histogram}
\usage{
plot_repsd(repsd_values, null_values, pvalues, which_item, bins = 30)
}
\arguments{
\item{repsd_values}{A numerical vector of repsd values, the output of \code{repsd()$repsd_each_item}.}

\item{null_values}{A matrix of the repsd null distribution, the output of \code{null_repsd()}.}

\item{pvalues}{A numerical vector of the repds p-values, the output of \code{repsd_pval()$p.value}}

\item{which_item}{A numerical indicator of the specific item to plot.}

\item{bins}{A numerical indicator on the number of bins to output in the histogram.}
}
\value{
A \code{plot} of the REPSD null distribution for the indicated item with
the observed REPSD value as a red line and the observed p-value
}
\description{
REPSD Null vs Observed Histogram
}
\examples{
example_repsd <-
    repsd()
example_null <-
    null_repsd(iterations = 100)
example_pvals <-
    repsd_pval(
               alpha = .05,
               null_dist = example_null,
               items_repsd = example_repsd$repsd_each_item
               )
# Only one plot
plot_repsd(repsd_values = example_repsd$repsd_each_item,
           null_values = example_null,
           pvalues = example_pvals$p.value,
           which_item = 18,
           bins = 10)
# Multiple plots on the same plot
oldpar <- par()
par(mfrow = c(2,2))
for (i in c(1,8,16,18)) {
  plot_repsd(
             repsd_values = example_repsd$repsd_each_item,
             null_values = example_null,
             pvalues = example_pvals$p.value,
             which_item = 18,
             bins = 10
             )
}
par(mfrow = oldpar$mfrow)
}
