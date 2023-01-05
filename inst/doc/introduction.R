## ---- include=FALSE-----------------------------------------------------------
knitr::opts_chunk$set(
  collapse=TRUE,
  comment="#>"
)

## ----echo=TRUE, message=FALSE, warning=FALSE, eval=FALSE----------------------
#  install.packages("contsurvplot")

## ----echo=TRUE, message=FALSE, warning=FALSE, eval=FALSE----------------------
#  devtools::install_github("RobinDenz1/contsurvplot")

## ----setup, warning=FALSE, message=FALSE--------------------------------------
library(contsurvplot)
library(ggplot2)
library(dplyr)
library(rlang)
library(riskRegression)
library(survival)
library(pammtools)
library(gganimate)
library(transformr)
library(plotly)
library(reshape2)
library(knitr)
library(rmarkdown)

## ----echo=TRUE----------------------------------------------------------------
data(cancer)
colon$sex <- factor(colon$sex)

head(colon)

## ----echo=TRUE----------------------------------------------------------------
model <- coxph(Surv(time, status) ~ age + sex + nodes, data=colon, x=TRUE)

## ----echo=TRUE----------------------------------------------------------------
summary(model)

## ----echo=TRUE----------------------------------------------------------------
curve_cont(data=colon, variable="nodes", model=model,
           horizon=c(0, 10, 15), times=c(1000, 2000))

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_at_t(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               t=1000)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_at_t(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               t=c(100, 500, 1000, 1500, 2000))

## ----echo=TRUE, warning=FALSE, fig.show=TRUE, fig.width=7, fig.height=5-------
plot_surv_quantiles(time="time",
                    status="status",
                    variable="nodes",
                    data=colon,
                    model=model,
                    p=0.5)

## ----echo=TRUE, warning=FALSE, fig.show=TRUE, fig.width=7, fig.height=5-------
plot_surv_quantiles(time="time",
                    status="status",
                    variable="nodes",
                    data=colon,
                    model=model,
                    p=c(0.1, 0.25, 0.5, 0.75, 0.9))

## ----echo=TRUE, warning=FALSE, fig.show=TRUE, fig.width=7, fig.height=5-------
plot_surv_rmst(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               tau=1000)

## ----echo=TRUE, warning=FALSE, fig.show=TRUE, fig.width=7, fig.height=5-------
plot_surv_rmst(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               tau=c(500, 1000, 2000))

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_rmtl(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               tau=c(500, 1000, 2000))

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_lines(time="time",
                status="status",
                variable="nodes",
                data=colon,
                model=model,
                horizon=c(0, 5, 10, 15, 20, 25, 30))

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_area(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_area(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               discrete=TRUE)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_area(time="time",
               status="status",
               variable="nodes",
               data=colon,
               model=model,
               discrete=TRUE,
               bins=5,
               start_color="lightgrey",
               end_color="black")

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_heatmap(time="time",
                  status="status",
                  variable="nodes",
                  data=colon,
                  model=model)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_heatmap(time="time",
                  status="status",
                  variable="nodes",
                  data=colon,
                  model=model,
                  contour_lines=TRUE)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_contour(time="time",
                  status="status",
                  variable="nodes",
                  data=colon,
                  model=model)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_contour(time="time",
                  status="status",
                  variable="nodes",
                  data=colon,
                  model=model,
                  bins=5)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_matrix(time="time",
                 status="status",
                 variable="nodes",
                 data=colon,
                 model=model)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_matrix(time="time",
                 status="status",
                 variable="nodes",
                 data=colon,
                 model=model,
                 n_col=5,
                 n_row=5)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5----------------------
plot_surv_3Dsurface(time="time",
                    status="status",
                    variable="nodes",
                    data=colon,
                    model=model)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5, eval=FALSE----------
#  # NOT RUN, to keep the vignette size reasonable
#  plot_surv_3Dsurface(time="time",
#                      status="status",
#                      variable="nodes",
#                      data=colon,
#                      model=model,
#                      interactive=TRUE)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=5, eval=FALSE----------
#  # NOT RUN, to keep the vignette size reasonable
#  plot_surv_animated(time="time",
#                     status="status",
#                     variable="nodes",
#                     data=colon,
#                     model=model,
#                     slider=TRUE,
#                     horizon=seq(0, 30, 1))

## ----echo=TRUE, warning=FALSE, fig.show=TRUE, fig.width=7, fig.height=5, eval=FALSE----
#  # NOT RUN, to keep the vignette size reasonable
#  plot_surv_animated(time="time",
#                     status="status",
#                     variable="nodes",
#                     data=colon,
#                     model=model,
#                     slider=FALSE,
#                     horizon=seq(0, 30, 1))

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=4----------------------
plot_surv_contour(time="time",
                  status="status",
                  variable="nodes",
                  group="sex",
                  data=colon,
                  model=model)

## ----echo=TRUE, fig.show=TRUE, fig.width=7, fig.height=4----------------------
plot_surv_area(time="time",
               status="status",
               variable="nodes",
               group="sex",
               data=colon,
               model=model)

