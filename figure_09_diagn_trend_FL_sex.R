# Figure: epi-trend Flemish region - by sex 
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, ggsave_figs, SSC_STI)
source("figure_helpers_TP.R")

y_nl <- "Geschatte aantal diagnoses \n per 100 000 inw. in Vlaanderen\n"
y_fr <- "Nombre estimé de diagnostics \n par 100 000 hab. en Flandre\n"
y_en <- "Estimated number of diagnoses \n per 100,000 inh in Flanders\n"

legend_labels_nl <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorroe", TREPAL = "Syfilis")
legend_labels_fr <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhée", TREPAL = "Syphilis")
legend_labels_en <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhoea", TREPAL = "Syphilis")

facet_labels_nl <- labeller(Gender = c( "F" = "Vrouwen", "M" = "Mannen"))
facet_labels_fr <- labeller(Gender = c( "F" = "Femmes", "M" = "Hommes"))
facet_labels_en <- labeller(Gender = c( "F" = "Women", "M" = "Men"))



# Flanders per sex, all age groups)
Adj_Reg_FL <- AdjInc %>%
  filter(Gender != "All" & Region == "FL" & Age == "All")



# Belgium per sex, all age groups (added as dotted lines in figure 09a)
Adj_BEL_sex <- AdjInc %>%
  filter(Gender != "All" & Region == "BEL" & Age == "All")


#CHECK if preset y-limits are ok (Flemish and Belgian estimates)
y_max <- y_max_diag
max_inc <- max(c(Adj_Reg_FL$Inc_est, Adj_BEL_sex$Inc_est), na.rm = TRUE)

if (max_inc > y_max) {
  stop(sprintf(
    "Maximum inc estimate exceeds upper limit of y-axis. adjust limit in prep_figure.R.",
    max_inc, y_max
  ))
}



fig <- function(dat, ylab, facet_labels, legend_labels) {
  
  m <- ggplot(dat, aes(Year, Inc_est, color = Germ)) +
    facet_wrap( ~ Gender,labeller = facet_labels)+
    geom_line(lwd = 1) +
    labs(x = "", y = ylab) +
    scale_x_continuous(breaks = seq(2016, year_of_interest_diagn, 1)) +
    scale_y_continuous(
      breaks = seq(0, 500, 100),
      limits = c(0, y_max)
    )+
    scale_color_manual(
      name = NULL,
      values = SSC_STI,
      labels = legend_labels
    )+

    sti_theme() +
    theme(
      axis.text.x = element_text( hjust = 1, vjust = 0.5 ,angle = 45)
    ) +
      theme_ytitle_wrap()
  return(m)
}


fig_FL_BEL_sex <- function(dat, ylab, facet_labels, legend_labels) {
  
  n <- ggplot(dat, aes(Year, Inc_est, color = Germ)) +
    facet_wrap( ~ Gender,labeller = facet_labels)+
    geom_line(lwd = 1) +
    labs(x = "", y = ylab) +
    scale_x_continuous(breaks = seq(2016, year_of_interest_diagn, 1)) +
    scale_y_continuous(
      breaks = seq(0, 500, 100),
      limits = c(0, y_max)
    )+
    scale_color_manual(
      name = NULL,
      values = SSC_STI,
      labels = legend_labels
    )+
    
    ### add belgian Inc estimates (same germs as in dat)
        geom_line(
          data = filter(Adj_BEL_sex, Germ %in% unique(dat$Germ)),
          aes(x = Year, y = Inc_est, color = Germ),
          linewidth = 0.8,
          linetype = "dotted",
          inherit.aes = FALSE
        )+
    ###

    sti_theme() +
    theme(
      axis.text.x = element_text( hjust = 1, vjust = 0.5 ,angle = 45)
    ) +
      theme_ytitle_wrap()
  return(n)
}


# define which language for the graph 
#NL
m_nl <- fig_inout(fig, Adj_Reg_FL, y_nl, facet_labels_nl, legend_labels_nl)
m_fr <- fig_inout(fig, Adj_Reg_FL, y_fr, facet_labels_fr, legend_labels_fr)
m_en <- fig_inout(fig, Adj_Reg_FL, y_en, facet_labels_en, legend_labels_en)


# ADD BELGIAN INC ESTIMATES 
n_nl <- fig_inout(fig_FL_BEL_sex, Adj_Reg_FL, y_nl, facet_labels_nl, legend_labels_nl)
n_fr <- fig_inout(fig_FL_BEL_sex, Adj_Reg_FL, y_fr, facet_labels_fr, legend_labels_fr)
n_en <- fig_inout(fig_FL_BEL_sex, Adj_Reg_FL, y_en, facet_labels_en, legend_labels_en)



# save Regional trends
ggsave_langs("m", "figure_09_diagn_trend_FL_sex")



# save regional + belgian trends
ggsave_langs("n", "figure_09a_diagn_trend_FL_BEL_sex",
             type = "cairo")
