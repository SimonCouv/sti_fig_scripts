# Figure: epi-trend (new diagnoses; historical trend)
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")


y_nl <- "Geschatte aantal diagnoses \n per 100 000 inw. in België"
y_fr <- "Nombre estimé de diagnostics \n par 100 000 hab en Belgique"
y_en <- "Estimated number of diagnoses \n per 100,000 inh in Belgium"

legend_labels_nl <- c("Chlamydia", "Gonorroe", "Syfilis")
legend_labels_fr <- c("Chlamydia", "Gonorrhée", "Syphilis")
legend_labels_en <- c("Chlamydia", "Gonorrhoea", "Syphilis")

y_max <- y_max_diag

# select overall incidence estimates (BEL, both sexes, all age groups)
AdjInc_fig <- AdjInc %>%
  filter(Gender == "All" & Region == "BEL" & Age == "All")

#check if preset y-limits are ok
max_inc <- max(AdjInc_fig$Inc_est, na.rm = TRUE)

if (max_inc > y_max) {
  stop(sprintf(
    "Maximum inc estimate exceeds upper limit of y-axis. adjust limit in prep_figure.R.",
    max_inc, y_max
  ))
}

fig <- function(ylab, legend_labels) {
  
  m <- ggplot(AdjInc_fig, aes(Year, Inc_est, color = Germ)) +
    geom_line(lwd = 1) +
    labs(x = "", y = ylab) +
    ylim(0,y_max)+
    
    scale_x_continuous(breaks = seq(start, year_of_interest_diagn, 1)) + #start = as defined in EpilaboSTI.R
    scale_color_manual(
      name = NULL,
      values = SSC,
      labels = legend_labels
    )+
    sti_theme() +
    theme(
      axis.text.x = element_text( hjust = 1, vjust = 0.5)
    )
  return(m)
}


# decide which language for the graph 
m_nl <- fig(y_nl, legend_labels_nl)
m_fr <- fig(y_fr, legend_labels_fr)
m_en <- fig(y_en, legend_labels_en)


# save
foldr <- paste0(dirname(getwd()),"/results_figures_report/")

ggsave(m_nl, filename = paste0(foldr,"figure_05_diagn_trend_BE_nl.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")

ggsave(m_fr, filename = paste0(foldr,"figure_05_diagn_trend_BE_fr.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")

ggsave(m_en, filename = paste0(foldr,"figure_05_diagn_trend_BE_en.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")



# 1# narrative ---------------------------------------------------------------
# 
# 
# # % change vs 2024 and vs 2023
# AdjInc_fig %>%
#   arrange(Germ, Year) %>%
#   group_by(Germ, Region, Age)%>%
#   mutate(
#     # Year-over-year percentage difference
#   pct_diff_1_year = (Inc_est - lag(Inc_est)) / lag(Inc_est),
#   
#   # Difference between 2024 and 2016 (only shown on 2024 row)
#   pct_diff_2016_2025 = if_else(
#     Year == 2025,
#     (Inc_est - first(Inc_est[Year == 2016])) /
#       first(Inc_est[Year == 2016]),
#     NA_real_
#   ),
#   
#   # Average yearly percentage difference between 2016 and 2024
#   # CAGR-style calculation
#   avg_yearly_pct_diff_2016_2024 = if_else(
#     Year == 2025,
#     (
#       (Inc_est / first(Inc_est[Year == 2016]))^(1 / (2025 - 2016)) - 1
#     ),
#     NA_real_
#   )) %>%
#   ungroup()%>%
#   select(-c(NCases, coverage, NCases_corr, Population))%>%
#   print(n=Inf)
# 
