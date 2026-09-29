# Figure: test trends
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")


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

SSC_STI <- SSC[1:3]
names(SSC_STI) <- names(legend_labels_nl)

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
      
    )
  return(m)
}

fig_inout <- function(dat, ylab, legend_labels){
  
  # #assumes syphilis is element 3
  # stopifnot(legend_labels[3] %in% c('Syfilis', 'Syphilis'))
  # legend_labels_nl_noTP <- legend_labels[!legend_labels %in%]
  
  m <- fig(dat, ylab, legend_labels)
  m_noTP <- fig(filter(dat, Germ != 'TREPAL'), ylab, legend_labels)
  m_TP <- fig(filter(dat, Germ == 'TREPAL'), ylab, legend_labels)
  
  return(list(all = m, noTP = m_noTP, TP = m_TP))
}

# decide which language for the graph 
m_nl <- fig_inout(Tests_fig, ylab_nl, legend_labels_nl)
m_fr <- fig_inout(Tests_fig, ylab_fr, legend_labels_fr)
m_en <- fig_inout(Tests_fig, ylab_en, legend_labels_en)

# save
dir_figs <- paste0(dirname(getwd()),"/results_figures_report/")

ggsave_figs <- function(p_list, fp, suffixes = c(all = '', noTP = '_noTP', TP = '_TP'), width = 16, height = 9){
  
  for (pname in names(suffixes)){
    s <- suffixes[pname]
    fp_out <- fs::path_ext_set(
      paste0(fs::path_ext_remove(fp), s),
      path_ext(fp)
    )
    ggsave(p_list[[pname]], filename = fp_out,
           dpi = 300, 
           width = width, height = height, units = "cm")
  }
}

langs <- c('nl', 'fr', 'en')
for (lang in langs){
  print(lang)
  m_lang <- paste0('m_', lang)
  ggsave_figs(get(m_lang), fp = sprintf("%s/figure_01_test_trend_BE_%s.png", dir_figs, lang))
}
