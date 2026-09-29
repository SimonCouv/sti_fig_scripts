rm(list = ls())

#LOAD DATA , STI THEME GGPLOT, specify years (until when - test/diagnoses), define Y-limits
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, ggsave_figs, facet_layout_germ, facet_dims_inout)
source("figure_helpers_TP.R")

facet_labels_nl <- labeller(Gender = c(#"All" = "Beide geslachten", 
                                       "F" = "Vrouwen", 
                                       "M" = "Mannen"), 
                            Germ = c("CHLTRA" = "Chlamydia", 
                                     "NEIGON" = "Gonorroe", 
                                     "TREPAL" = "Syfilis" ) )


facet_labels_fr <- labeller(Gender = c(#"All" = "Deux sexes", 
                                        "F" = "Femmes", 
                                        "M" = "Hommes"), 
                            Germ = c("CHLTRA" = "Chlamydia", 
                                     "NEIGON" = "Gonorrhée", 
                                     "TREPAL" = "Syphilis" ) )

facet_labels_en <- labeller(Gender = c(#"All" = "Both sexes", 
                                        "F" = "Women", 
                                        "M" = "Men"), 
                            Germ = c("CHLTRA" = "Chlamydia", 
                                     "NEIGON" = "Gonorrhoea", 
                                     "TREPAL" = "Syphilis" ) )

xlab_nl = "Jaar"
xlab_fr = "Année"
xlab_en = "Year"

ylab_nl = "Aantal terugbetaalde tests\n per 1000 inw. in Vlaanderen"
ylab_fr = "Nombre de tests remboursés\n pour 1 000 habitants en Flandre"
ylab_en ="Number of reimbursed tests\n per 1,000 inhabitants in Flanders"

ylab2_nl = "Dekkingsgraden"
ylab2_fr = "Couvertures"
ylab2_en = "Coverages"



## define specific region 
Region_fig <- "FL" 

Coverage_fig <- Coverage %>%
  filter(Region == Region_fig)%>% 
  filter(Gender != "All")

scale_factor <- max(Coverage_fig$NTests_all, na.rm = TRUE ) / max(Coverage_fig$coverage , na.rm = TRUE )


fig <- function(dat, xlab, ylab, ylab2, facet_labels) {

# one column per germ, one row per sex; syphilis only: one row
lay <- facet_layout_germ(dat)

m <- dat %>%
  ggplot(aes(x = Year)) +
  
  facet_wrap(Gender ~ Germ, labeller = facet_labels,
             nrow = lay[["nrow"]], ncol = lay[["ncol"]]) +
  
  geom_col(aes(y = NTests_all), fill = sc1) +
  
  geom_line(
    aes(y = coverage * scale_factor),
    color = sc2,
    linewidth = 0.7
  ) +
  
  scale_y_continuous(
    name = ylab,
    sec.axis = sec_axis(
      ~ . / scale_factor,
      name = ylab2,
      labels = scales::label_percent()
    )
  ) +
  
  scale_x_continuous(
    name = xlab,
    breaks = seq(
      min(Coverage$Year, na.rm = TRUE),
      max(Coverage$Year, na.rm = TRUE),
      by = 1
    )
  ) +
  
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    
    # left axis
    axis.title.y = element_text(color = sc1),
    axis.ticks.y = element_line(color = sc1),
    axis.text.y = element_text(color = sc1),
    
    # right axis
    axis.title.y.right = element_text(color = sc2),
    axis.ticks.y.right = element_line(color = sc2),
    axis.text.y.right = element_text(color = sc2)
  )
return(m)
} 


# decide which language for the graph 
m_nl <- fig_inout(fig, Coverage_fig, xlab_nl, ylab_nl, ylab2_nl, facet_labels_nl)
m_fr <- fig_inout(fig, Coverage_fig, xlab_fr, ylab_fr, ylab2_fr, facet_labels_fr)
m_en <- fig_inout(fig, Coverage_fig, xlab_en, ylab_en, ylab2_en, facet_labels_en)

#plot
# m_nl 
# m_fr
# m_en 

# save
# sizes (cm) per version: panel size of the full figure (16 x 9 cm) kept constant
dims <- facet_dims_inout(Coverage_fig, width = 16, height = 9)

ggsave_langs("m", "figure_08_coverage_FL",
             width = dims$width, height = dims$height)


# # Narrative --------------------------------------------------------
# Coverage_fig %>%
#   select(Germ, Gender, Region, Year, coverage)%>%
#   filter(Year %in% c(2016,2025))%>%
#   print(n= Inf)
