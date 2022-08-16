
suppressMessages(requireNamespace("survival"))

sim_dat <- readRDS(system.file("testdata", "sim150.Rds",
                               package="contsurvplot"))
sim_dat$group <- factor(sim_dat$group)

model <- survival::coxph(survival::Surv(time, event) ~ x3 + group,
                         data=sim_dat, x=TRUE)

# NOTE: The expect_doppelganger() tests sometimes do not work properly
#       for contour plots for unknown reasons. The output looks the same,
#       but gets flagged as different by ubuntu for example. Skipped.

# to skip tests on github
skip_on_actions <- function() {
  if (!identical(Sys.getenv("GITHUB_ACTIONS"), "true")) {
    return(invisible(TRUE))
  }
  skip("On GitHub Actions")
}

test_that("plot, defaults", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, defaults", fig=plt)
})

test_that("plot, with group", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           group="group", data=sim_dat, model=model)
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, with group", fig=plt)
})

test_that("plot, cif", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, cif=TRUE)
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, cif", fig=plt)
})

test_that("plot, change horizon", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model,  horizon=seq(30, 60, 1))
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, change horizon", fig=plt)
})

test_that("plot, at t", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, fixed_t=seq(0, 30, 1))
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, at t", fig=plt)
})

test_that("plot, custom colors", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, bins=5,
                           custom_colors=c("black", "grey", "red", "yellow",
                                           "green"))
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, custom colors", fig=plt)
})

test_that("plot, panel_border / axis_dist", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, panel_border=TRUE,
                           axis_dist=0.1, na.action=na.omit)
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, panel_border / axis_dist", fig=plt)
})

test_that("plot, lots of stuff", {
  plt <- plot_surv_contour(time="time", status="event", variable="x3",
                           data=sim_dat, model=model, start_color="grey",
                           end_color="black", cif=TRUE, contour_lines=TRUE,
                           contour_size=1, contour_linetype="dotdash",
                           title="Title", subtitle="Subtitle",
                           xlab="x", ylab="y",
                           legend.title="legend", legend.position="bottom",
                           gg_theme=ggplot2::theme_classic())
  expect_s3_class(plt, "ggplot")
  skip_on_cran()
  skip_on_covr()
  skip_on_actions()
  vdiffr::expect_doppelganger("plot, lots of stuff", fig=plt)
})
