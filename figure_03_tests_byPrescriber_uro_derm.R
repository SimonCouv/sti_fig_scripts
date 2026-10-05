# Import and pre-process the STI testing data from RIZIV.
rm(list = ls())

#specify years (until when - test/diagnoses)
source("prep_figure.R")



ylab_nl <- "Proportie van het aantal terugbetaalde tests\n"
ylab_fr <- "Proportion du nombre de tests remboursés \n"
ylab_en <- "Proportion of the number of reimbursed tests\n"


x_labels_nl <- c("Chlamydia", "Gonorroe", "Syfilis")
x_labels_fr <- c("Chlamydia", "Gonorrhée", "Syphilis")
x_labels_en <- c("Chlamydia", "Gonorrhoea", "Syphilis")

legend_labels_nl <- c("Dermatologen","Acute en Spoedartsen", "Huisartsen", "Gynaecologen & \n Verloskundigen", "Internisten", "Urologen", "Andere")
legend_labels_fr <- c( "Dermatologues","Médecine aiguë & Urg.", "Médecins généralistes", "Gynécol. & Obst", "Médecins internistes", 
                       "Urologues", "Autres")
legend_labels_en <- c( "Dermatologists","Acute Medicine & ER", "General Practitioners", "Gynecology & OB", "Internists", 
                        "Urologists", "Other")


fig <- function(ylab, x_labels, legend_labels) {
    
  m <-  ggplot(Tests_MD_Germ, aes(x=Germ, y = pct, fill = MD_type))+
  geom_col(width = 0.8, position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels= x_labels, expand = expansion(mult = c(0.15, 0.15))) +
  scale_y_continuous(limits = c(0, 0.6), labels = percent_format(accuracy = 1)) +
  
  scale_fill_manual(
    name = NULL,
    values = SSC
    , 
    labels = legend_labels
  )+
  labs(x = "", y = ylab)+ 
  sti_theme()+
  theme(
    legend.text = element_text(size = 9)
  )
  
return(m)
}


m_nl <- fig(ylab_nl, x_labels_nl, legend_labels_nl)
m_fr <- fig(ylab_fr, x_labels_fr, legend_labels_fr)
m_en <- fig(ylab_en, x_labels_en, legend_labels_en)


# save
foldr <- paste0(dirname(getwd()),"/results_figures_report/")
ggsave(m_nl, filename = paste0(foldr,"figure_03_test_prescrib_uro_derm_nl.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")

ggsave(m_fr, filename = paste0(foldr,"figure_03_test_prescrib_uro_derm_fr.png"), 
       dpi = 300,
       width = 16, height = 9, units = "cm")


ggsave(m_en, filename = paste0(foldr,"figure_03_test_prescrib_uro_derm_en.png"), 
       dpi = 300, 
       width = 16, height = 9, units = "cm")

