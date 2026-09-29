# Helpers to make three versions of each figure:
#   all  = chlamydia, gonorrhoea and syphilis (as before)
#   noTP = without syphilis (Treponema pallidum, Germ == "TREPAL")
#   TP   = syphilis only
#
# Source right after source("prep_figure.R"): uses SSC, defined there.


# output folder and languages, the same for all figure scripts
dir_figs <- paste0(dirname(getwd()),"/results_figures_report/")
langs <- c('nl', 'fr', 'en')


# germ colours, named by germ so a germ keeps its colour when others are filtered out
SSC_STI <- setNames(SSC[1:3], c("CHLTRA", "NEIGON", "TREPAL"))


# split the plot data into the three versions
split_inout <- function(dat) {
  list(
    all  = dat,
    noTP = dplyr::filter(dat, Germ != "TREPAL"),
    TP   = dplyr::filter(dat, Germ == "TREPAL")
  )
}


# make the three versions of a figure
# fig: plotting function that takes the plot data as its first argument
# ...: other arguments passed on to fig (labels, ...)
fig_inout <- function(fig, dat, ...) {
  lapply(split_inout(dat), function(d) fig(d, ...))
}


# facet layout for figures faceted by sex (Gender) and germ (Germ):
# one column per germ, one row per sex; with a single germ, one row with one panel per sex
facet_layout_germ <- function(dat) {
  n_germ   <- dplyr::n_distinct(dat$Germ)
  n_gender <- dplyr::n_distinct(dat$Gender)
  if (n_germ == 1) {
    c(nrow = 1, ncol = n_gender)
  } else {
    c(nrow = n_gender, ncol = n_germ)
  }
}


# figure sizes (cm) for the three versions of a figure faceted with facet_layout_germ(),
# keeping the panel size of the full figure (width x height) constant.
# margin_w, margin_h: space (cm) taken by axis titles, axis labels and legend, i.e. not by panels
facet_dims_inout <- function(dat, width, height, margin_w = 3, margin_h = 2) {
  lay <- lapply(split_inout(dat), facet_layout_germ)
  panel_w <- (width  - margin_w) / lay$all[["ncol"]]
  panel_h <- (height - margin_h) / lay$all[["nrow"]]
  list(
    width  = sapply(lay, function(l) margin_w + l[["ncol"]] * panel_w),
    height = sapply(lay, function(l) margin_h + l[["nrow"]] * panel_h)
  )
}


# save the three versions of a figure
# p_list: list(all = , noTP = , TP = ), as returned by fig_inout()
# fp: file path of the full figure; the other versions get the suffix added before the extension
# width, height (cm): one value for all versions, or a vector named like suffixes
# ...: passed on to ggsave (e.g. type = "cairo")
ggsave_figs <- function(p_list, fp, suffixes = c(all = '', noTP = '_noTP', TP = '_TP'), width = 16, height = 9, ...){

  for (pname in names(suffixes)){
    s <- suffixes[pname]
    w <- if (length(width)  > 1) width[[pname]]  else width
    h <- if (length(height) > 1) height[[pname]] else height
    fp_out <- fs::path_ext_set(
      paste0(fs::path_ext_remove(fp), s),
      fs::path_ext(fp)
    )
    ggsave(p_list[[pname]], filename = fp_out,
           dpi = 300,
           width = w, height = h, units = "cm", ...)
  }
}
