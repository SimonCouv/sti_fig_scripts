# Figure: epi-trend Flemish region - by sex 
rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")

y_nl <- "Geschatte aantal diagnoses \n per 100 000 inw. in Vlaanderen\n"
y_fr <- "Nombre estimé de diagnostics \n par 100 000 hab. en Flandre\n"
y_en <- "Estimated number of diagnoses \n per 100,000 inh in Flanders\n"

legend_labels_nl <- c("Chlamydia", "Gonorroe", "Syfilis")
legend_labels_fr <- c("Chlamydia", "Gonorrhée", "Syphilis")
legend_labels_en <- c("Chlamydia", "Gonorrhoea", "Syphilis")

facet_labels_nl <- labeller(Gender = c( "F" = "Vrouwen", "M" = "Mannen"))
facet_labels_fr <- labeller(Gender = c( "F" = "Femmes", "M" = "Hommes"))
facet_labels_en <- labeller(Gender = c( "F" = "Women", "M" = "Men"))



# Flanders per sex, all age groups)
Adj_Reg_FL <- AdjInc %>%
  filter(Gender != "All" & Region == "FL" & Age == "All")



#CHECK if preset y-limits are ok
y_max <- y_max_diag
max_inc <- max(Adj_Reg_FL$Inc_est, na.rm = TRUE)

if (max_inc > y_max) {
  stop(sprintf(
    "Maximum inc estimate exceeds upper limit of y-axis. adjust limit in prep_figure.R.",
    max_inc, y_max
  ))
}



fig <- function(ylab, facet_labels, legend_labels) {
  
  m <- ggplot(Adj_Reg_FL, aes(Year, Inc_est, color = Germ)) +
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
      values = SSC,
      labels = legend_labels
    )+

    sti_theme() +
    theme(
      axis.text.x = element_text( hjust = 1, vjust = 0.5 ,angle = 45)
    )
  return(m)
}


# fig_FL_BEL_sex <- function(ylab, facet_labels, legend_labels) {
#   
#   n <- ggplot(Adj_Reg_FL, aes(Year, Inc_est, color = Germ)) +
#     facet_wrap( ~ Gender,labeller = facet_labels)+
#     geom_line(lwd = 1) +
#     labs(x = "", y = ylab) +
#     scale_x_continuous(breaks = seq(2016, year_of_interest, 1)) +
#     scale_y_continuous(
#       breaks = seq(0, 500, 100),
#       limits = c(0, 505)
#     )+
#     scale_color_manual(
#       name = NULL,
#       values = SSC,
#       labels = legend_labels
#     )+
#     
#     ### add belgian Inc estimates
#         geom_line(
#           data = subset(AdjInc, Region == "BEL" & Gender != "All" & Age == "All"),
#           aes(x = Year, y = Inc_est, color = Germ),
#           linewidth = 0.8,
#           linetype = "dotted",
#           inherit.aes = FALSE
#         )+
#     ###
# 
#     sti_theme() +
#     theme(
#       axis.text.x = element_text( hjust = 1, vjust = 0.5 ,angle = 45)
#     )
#   return(n)
# }


# define which language for the graph 
#NL
m_nl <- fig(y_nl, facet_labels_nl, legend_labels_nl)
m_fr <- fig(y_fr, facet_labels_fr, legend_labels_fr)
m_en <- fig(y_en, facet_labels_en, legend_labels_en)


# ADD BELGIAN INC ESTIMATES 
# n_nl <- fig_FL_BEL_sex(y_nl, facet_labels_nl, legend_labels_nl)
# n_fr <- fig_FL_BEL_sex(y_fr, facet_labels_fr, legend_labels_fr)
# n_en <- fig_FL_BEL_sex(y_en, facet_labels_en, legend_labels_en)



# save Regional trends
foldr <- paste0(dirname(getwd()),"/results_figures_report/")

ggsave(m_nl, filename = paste0(foldr,"figure_09_diagn_trend_FL_sex_nl.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")
ggsave(m_fr, filename = paste0(foldr,"figure_09_diagn_trend_FL_sex_fr.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")
ggsave(m_en, filename = paste0(foldr,"figure_09_diagn_trend_FL_sex_en.png"), 
       dpi = 300,
       width = 16, height = 9, units = "cm")



# save regional + belgian trends
# ggsave(n_nl, filename = paste0(foldr,"figure_09a_diagn_trend_FL_BEL_sex_nl.png"), 
#        dpi = 300, type = "cairo",
#        width = 16, height = 9, units = "cm")
# 
# ggsave(n_fr, filename = paste0(foldr,"figure_09a_diagn_trend_FL_BEL_sex_fr.png"), 
#        dpi = 300, type = "cairo",
#        width = 16, height = 9, units = "cm")
# 
# ggsave(n_en, filename = paste0(foldr,"figure_09a_diagn_trend_FL_BEL_sex_en.png"), 
#        dpi = 300, type = "cairo",
#        width = 16, height = 9, units = "cm")

