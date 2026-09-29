# Figure: test trends
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, ggsave_figs, SSC_STI)
source("figure_helpers_TP.R")


# tests for Belgium as a whole 

Tests_fig <- Coverage %>%
  dplyr::filter( Gender == "All" & Region == "BEL" & Year <= year_of_interest_test)%>%
  distinct()%>%
  
  left_join(
    BelgianPop_STI %>% 
      dplyr::filter(Age == "All"),
    by = c("Year", "Region", "Gender")
  )%>%
  distinct()%>%
  mutate( test_pop = (NTests_all / Population) *1000)


# labels for the plots
ylab_nl <- "Aantal terugbetaalde tests \n per 1000 inw. in België"
ylab_fr <- "Nombre de tests remboursés \n par 1000 hab. en Belgique"
ylab_en <- "Number of reimbursed tests \n per 1000 inh. in Belgium"


legend_labels_nl <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorroe", TREPAL = "Syfilis")
legend_labels_fr <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhée", TREPAL = "Syphilis")
legend_labels_en <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhoea", TREPAL = "Syphilis")

y_max <- y_max_test

#check if preset y-limits are ok
    max_test <- max(Tests_fig$test_pop, na.rm = TRUE)
    if (max_test > y_max) {
      stop(sprintf(
        "Maximum inc estimate exceeds upper limit of y-axis. adjust limit in prep_figure.R.",
        max_test, y_max
      ))
    }

fig <- function(dat, ylab, legend_labels) {
  
  m <- ggplot(dat, aes(Year, test_pop, color = Germ)) +
    geom_line(lwd = 1, linetype = "longdash") +
    labs(x = "", y = ylab) +
    scale_x_continuous(
      breaks = seq(
        start, 
        year_of_interest_test, 
        1)
    ) + #test data as available/received from RIZIV/INAMI, start = as defined in EpilaboSTI.R
    
    ylim(0,y_max)+
    scale_color_manual(
      name = NULL,
      values = SSC_STI,
      labels = legend_labels
    )+
    sti_theme() +
    theme(
      axis.text.x = element_text( angle = 45, hjust = 1, vjust = 0.5),
      legend.text = element_text( size = 9)
      
    ) +
      theme_ytitle_wrap()
  return(m)
}

# decide which language for the graph 
m_nl <- fig_inout(fig, Tests_fig, ylab_nl, legend_labels_nl)
m_fr <- fig_inout(fig, Tests_fig, ylab_fr, legend_labels_fr)
m_en <- fig_inout(fig, Tests_fig, ylab_en, legend_labels_en)

# save
for (lang in langs){
  print(lang)
  m_lang <- paste0('m_', lang)
  ggsave_figs(get(m_lang), fp = sprintf("%s/figure_01_test_trend_BE_%s.png", dir_figs, lang))
}
