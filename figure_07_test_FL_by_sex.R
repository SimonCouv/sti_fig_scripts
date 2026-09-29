# Figure: test trends
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, ggsave_figs, SSC_STI)
source("figure_helpers_TP.R")

# tests for Flanders by sex 
Region_fig <- "FL" 


Tests_fig <- Coverage %>%
  dplyr::filter( Gender != "All" & 
                 Region == Region_fig & 
                 Year <= year_of_interest_test)%>%
  distinct()%>%
  left_join(
    BelgianPop_STI %>% 
      dplyr::filter(Age == "All"),
    by = c("Year", "Region", "Gender")
  )%>%
  distinct()%>%
  mutate( test_pop = (NTests_all / Population) *1000)

#CHECK if preset y-limits are ok
max_inc <- max(Tests_fig$test_pop, na.rm = TRUE)
y_max <- y_max_test

if (max_inc > y_max) {
  stop(sprintf(
    "Maximum inc estimate exceeds upper limit of y-axis. adjust limit in prep_figure.R.",
    max_inc, y_max
  ))
}

# labels for the plots
y_nl <- "Aantal terugbetaalde testen\n per 1000 inw. in Vlaanderen"
y_fr <- "Nombre de tests remboursés\n par 1000 hab. en Flandre"
y_en <- "Number of reimbursed tests\n per 1000 inh. in Flanders"

legend_labels_nl <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorroe", TREPAL = "Syfilis")
legend_labels_fr <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhée", TREPAL = "Syphilis")
legend_labels_en <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhoea", TREPAL = "Syphilis")

facet_labels_nl <- labeller(Gender = c( "F" = "Vrouwen", "M" = "Mannen"))
facet_labels_fr <- labeller(Gender = c( "F" = "Femmes", "M" = "Hommes"))
facet_labels_en <- labeller(Gender = c( "F" = "Women", "M" = "Men"))



fig <- function(dat, ylab, legend_labels, facet_labels) {
  
  m <- ggplot(dat, aes(Year, test_pop, color = Germ)) +
    facet_wrap(~ Gender,labeller = facet_labels)+
    geom_line(lwd = 1, linetype = "longdash") +
    labs(x = "", y = ylab) +
    scale_x_continuous(
      breaks = seq(
        start, 
        year_of_interest_test, 1)) + #test data as available/received from RIZIV/INAMI, start = as defined in EpilaboSTI.R
    ylim(0,y_max)+
    scale_color_manual(
      name = NULL,
      values = SSC_STI,
      labels = legend_labels
    )+
    sti_theme() +
  theme(
    # legend
   legend.text = element_text(family = 'TrebuchetMS', size = 9, face = 'plain'),

    axis.text.x = element_text( angle = 45, hjust = 1, vjust = 0.5)
  ) +
    theme_ytitle_wrap()
  return(m)
}


m_nl <- fig_inout(fig, Tests_fig, y_nl, legend_labels_nl, facet_labels_nl)
m_fr <- fig_inout(fig, Tests_fig, y_fr, legend_labels_fr, facet_labels_fr)
m_en <- fig_inout(fig, Tests_fig, y_en, legend_labels_en, facet_labels_en)



# save
for (lang in langs){
  print(lang)
  m_lang <- paste0('m_', lang)
  ggsave_figs(get(m_lang), fp = sprintf("%s/figure_07_tests_sex_FL_%s.png", dir_figs, lang))
}


# 1# narrative ---------------------------------------------------------------
# 
# 
# # Total tests per sex, total tests per 1000 people, % change vs 2024 and vs 2023
# Tests_fig %>%
#   arrange(Germ, Gender, Year) %>%
#   group_by(Germ, Region, Age, Gender)%>%
#   mutate(
#     
#     # Year-over-year percentage difference
#     pct_diff_1_year = (test_pop - lag(test_pop)) / lag(test_pop),
#     
#     # Difference between 2024 and 2016 (only shown on 2024 row)
#     pct_diff_2024_2016 = if_else(
#       Year == 2024,
#       (test_pop - first(test_pop[Year == 2016])) /
#         first(test_pop[Year == 2016]),
#       NA_real_
#     ),
#     
#     # Average yearly percentage difference between 2016 and 2024
#     # CAGR-style calculation
#     avg_yearly_pct_diff_2016_2024 = if_else(
#       Year == 2024,
#       (
#         (test_pop / first(test_pop[Year == 2016]))^(1 / (2024 - 2016)) - 1
#       ),
#       NA_real_
#     )
#     
#   ) %>%
#   ungroup()%>%
#   select(-c(NTests_epi, coverage, coverage_lag, coverage_trend, Age, Population))%>%
#   print(n=Inf)
# 
