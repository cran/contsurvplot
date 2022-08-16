\name{curve_cont}
\alias{curve_cont}

\title{
Calculate Causal Survival or Failure Probabilities for Levels of a Continuous Variable
}
\description{
This function can be utilized to calculate survival curves or cumulative incidence functions (CIF) for specific values of a continuous covariate.
}
\usage{
curve_cont(data, variable, model, horizon,
           times, group=NULL, cause=1, cif=FALSE, n_cores=1,
           na.action=options()$na.action, ...)
}
\arguments{
  \item{data}{
A \code{data.frame} containing all required variables.
  }
  \item{variable}{
A single character string specifying the continuous variable of interest, for which the survival curves should be estimated. This variable has to be contained in the \code{data.frame} that is supplied to the \code{data} argument.
  }
  \item{model}{
A model describing the time-to-event process (such as an \code{coxph} model). Needs to include \code{variable} as an independent variable. It also has to have an associated \code{predictRisk} method. See \code{?predictRisk} for more details.
  }
  \item{horizon}{
A numeric vector containing a range of values of \code{variable} for which the survival curves should be calculated.
  }
  \item{times}{
A numeric vector containing points in time at which the survival probabilities should be calculated.
  }
  \item{group}{
An optional single character string specifying a factor variable in \code{data}. When used, the regression standardization is performed conditional on this factor variable, meaning that one estimate is returned for each level of the factor variable. See details for a better description. Set to \code{NULL} (default) to use no grouping variable.
  }
  \item{cause}{
The cause of interest. In standard survival data with only one event type, this should be kept at 1. For data with multiple failure types, this argument should be specified. In addition, the \code{cif} argument should be set to \code{TRUE} in those cases.
  }
  \item{cif}{
Whether to calculate the cumulative incidence (CIF) instead of the survival probability. If multiple failure types are present, the survival probability cannot be estimated in an unbiased way. In those cases, this argument should always be set to \code{TRUE}.
  }
  \item{n_cores}{
The number of processor cores to use when performing the calculations. If \code{n_cores=1} (default), single threaded processing is used. If \code{n_cores > 1} the \pkg{foreach} package and the \pkg{doParallel} package are used to run the calculations on \code{n_cores} in parallel. This might speed up the runtime considerably when it is initially slow.
  }
  \item{na.action}{
How missing values should be handled. Can be one of: \code{na.fail}, \code{na.omit}, \code{na.pass}, \code{na.exclude} or a user-defined custom function. Also accepts strings of the function names. See \code{?na.action} for more details. By default it uses the na.action which is set in the global options by the respective user.
  }
  \item{...}{
Further arguments passed to \code{\link[=riskRegression]{predictRisk}}.
  }
}
\details{
This function uses \emph{Direct Standardization}, also known as the \emph{Corrected Group Prognosis} method, \emph{G-Computation} or \emph{G-Formula}, to calculate the causal survival probability or CIF for values of a continuous variable. The resulting estimates can be interpreted as the average survival probability (or failure probability) that would have been observed if the value of \code{variable} had been set to a specific value for all individuals in \code{data}.

If the model supplied to \code{model} is correct, the resulting estimates will be unbiased. By including relevant confounders as independent covariates in the \code{model}, the resulting estimates are adjusted for the confounders as well. This function is used internally in all plot functions included in this R-package and generally does not need to be called directly by the user. It can however be used to get specific values or as a basis to create custom plots not included in this package.

It relies on the \code{\link[=riskRegression]{predictRisk}} function from the \pkg{riskRegression} package to create the covariate and time specific estimates of the probabilities. If there are multiple failure types in the status variable of the time-to-event outcome, this should be directly modeled using a suitable model, such as a Cause-Specific-Cox model or a Fine & Gray model. In this case, only the cause-specific CIF can be estimated and \code{cif=TRUE} should always be used accordingly.

If \code{group} is not \code{NULL}, the whole estimation will be performed once for each of the factor levels of \code{group}. This means that in addition to fixing the level of the continuous covariate at a certain value for the entire dataset, the grouping variable is fixed as well. The results can be interpreted similarly to the usual estimate, only that they are also conditional on the level of \code{group} used. All plots that support this argument will be facetted according to the \code{group} variable. To obtain useful estimates, it is crucial to include the \code{group} variable in the \code{model}.
}
\value{
Returns a \code{data.frame} containing the columns \code{time} (the point in time), \code{surv} (the estimated survival probability) and \code{cont} (the specific value of \code{variable} used). If \code{group} was used, it includes the additional \code{group} column, specifying the level of the grouping variable.
}
\references{
Brice Ozenne, Anne Lyngholm Sorensen, Thomas Scheike, Christian Torp-Pedersen and Thomas Alexander Gerds. riskRegression: Predicting the Risk of an Event using Cox Regression Models. The R Journal (2017) 9:2, pages 440-460.

I-Ming Chang, Rebecca Gelman, and Marcello Pagano. Corrected Group Prognostic Curves and Summary Statistics. Journal of Chronic Diseases (1982) 35, pages 669-674

James Robins. A New Approach to Causal Inference in Mortality Studies with a Sustained Exposure Period: Application to Control of the Healthy Worker Survivor Effect. Mathematical Modelling (1986) 7, pages 1393-1512.
}
\author{
Robin Denz
}
\seealso{
\code{\link[=riskRegression]{predictRisk}}
}
\examples{
library(contsurvplot)
library(riskRegression)
library(survival)

# using data from the survival package
data(nafld, package="survival")

# take a random sample to keep example fast
set.seed(42)
nafld1 <- nafld1[sample(nrow(nafld1), 150), ]

# fit cox-model with age
model <- coxph(Surv(futime, status) ~ age, data=nafld1, x=TRUE)

# estimate survival probability at some points in time, for
# a range of age values
plotdata <- curve_cont(data=nafld1,
                       variable="age",
                       model=model,
                       horizon=c(50, 60, 70, 80),
                       times=c(1000, 2000, 3000, 4000))

# estimate cumulative incidences instead
plotdata <- curve_cont(data=nafld1,
                       variable="age",
                       model=model,
                       horizon=c(50, 60, 70, 80),
                       times=c(1000, 2000, 3000, 4000),
                       cif=TRUE)
}