# Figure: epi-trend Brussels region
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, ggsave_figs, SSC_STI)
source("figure_helpers_TP.R")

y_nl <- "Geschatte aantal diagnoses\n per 100 000 inw. in Brussel"
y_fr <- "Nombre estimé de diagnostics\n par 100 000 hab à Bruxelles"
y_en <- "Estimated number of diagnoses\n per 100,000 inh in Brussels"

legend_labels_nl <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorroe", TREPAL = "Syfilis")
legend_labels_fr <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhée", TREPAL = "Syphilis")
legend_labels_en <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhoea", TREPAL = "Syphilis")

facet_labels_nl <- labeller(Gender = c( "F" = "Vrouwen", "M" = "Mannen"))
facet_labels_fr <- labeller(Gender = c( "F" = "Femmes", "M" = "Hommes"))
facet_labels_en <- labeller(Gender = c( "F" = "Women", "M" = "Men"))

Region_fig <- "BXL"
# Brussels, both sexes, all age groups)
Adj_Reg_BXL <- AdjInc %>%
  filter(Gender != "All" & Region == Region_fig & Age == "All")



#CHECK if preset y-limits are ok
y_max <- y_max_diag_BXL
max_inc <- max(Adj_Reg_BXL$Inc_est, na.rm = TRUE)

if (max_inc > y_max) {
  stop(sprintf(
    "Maximum inc estimate exceeds upper limit of y-axis. adjust limit in prep_figure.R.",
    max_inc, y_max
  ))
}



fig_BXL <- function(dat, ylab, facet_labels, legend_labels) {
  
  m <- ggplot(dat, aes(Year, Inc_est, color = Germ)) +
    facet_wrap( ~ Gender,labeller = facet_labels)+
    geom_line(lwd = 1) +
    labs(x = "", y = ylab) +
    scale_x_continuous(breaks = seq(2016, year_of_interest_diagn, 1)) +
    scale_y_continuous(
      breaks = seq(0, 1000, 100),
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


# decide which language for the graph 
m_nl <- fig_inout(fig_BXL, Adj_Reg_BXL, y_nl, facet_labels_nl, legend_labels_nl)
m_fr <- fig_inout(fig_BXL, Adj_Reg_BXL, y_fr, facet_labels_fr, legend_labels_fr)
m_en <- fig_inout(fig_BXL, Adj_Reg_BXL, y_en, facet_labels_en, legend_labels_en)

# plot 
# m_nl
# m_fr 
# m_en



# save
for (lang in langs){
  print(lang)
  m_lang <- paste0('m_', lang)
  ggsave_figs(get(m_lang), fp = sprintf("%s/figure_12_diagn_trend_BXL_sex_%s.png", dir_figs, lang))
}

