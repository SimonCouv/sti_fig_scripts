# Import and pre-process the STI testing data from RIZIV.
rm(list = ls())

#specify years (until when - test/diagnoses)
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, germ_widths_inout, ggsave_langs)
source("figure_helpers_TP.R")



ylab_nl <- "Proportie van het aantal terugbetaalde tests\n"
ylab_fr <- "Proportion du nombre de tests remboursés \n"
ylab_en <- "Proportion of the number of reimbursed tests\n"


x_labels_nl <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorroe", TREPAL = "Syfilis")
x_labels_fr <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhée", TREPAL = "Syphilis")
x_labels_en <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhoea", TREPAL = "Syphilis")

legend_labels_nl <- c("Dermatologen","Acute en Spoedartsen", "Huisartsen", "Gynaecologen & \n Verloskundigen", "Internisten", "Urologen", "Andere")
legend_labels_fr <- c( "Dermatologues","Médecine aiguë & Urg.", "Médecins généralistes", "Gynécol. & Obst", "Médecins internistes", 
                       "Urologues", "Autres")
legend_labels_en <- c( "Dermatologists","Acute Medicine & ER", "General Practitioners", "Gynecology & OB", "Internists", 
                        "Urologists", "Other")

# legend labels and colours named by prescriber type (MD_type), in the order of the full figure
# (as ggplot2 orders them), so they stay with the right prescriber type if one has no tests in a
# version (e.g. syphilis only)
md_types <- if (is.factor(Tests_MD_Germ$MD_type)) {
  levels(droplevels(Tests_MD_Germ$MD_type))
} else {
  sort(unique(Tests_MD_Germ$MD_type))
}
stopifnot(length(md_types) == length(legend_labels_nl))
names(legend_labels_nl) <- md_types
names(legend_labels_fr) <- md_types
names(legend_labels_en) <- md_types
SSC_MD <- setNames(SSC[seq_along(md_types)], md_types)

y_max <- 0.6

#check if preset y-limits are ok
max_pct <- max(Tests_MD_Germ$pct, na.rm = TRUE)
if (max_pct > y_max) {
  stop(sprintf(
    "Maximum percentage (%.2f) exceeds upper limit of y-axis (%.2f). adjust y_max.",
    max_pct, y_max
  ))
}

fig <- function(dat, ylab, x_labels, legend_labels) {
    
  m <-  ggplot(dat, aes(x=Germ, y = pct, fill = MD_type))+
  geom_col(width = 0.8, position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels= x_labels, expand = expansion(mult = c(0.15, 0.15))) +
  scale_y_continuous(limits = c(0, y_max), labels = percent_format(accuracy = 1)) +
  
  scale_fill_manual(
    name = NULL,
    values = SSC_MD, 
    labels = legend_labels
  )+
  labs(x = "", y = ylab)+ 
  sti_theme()+
  theme(
    legend.text = element_text(size = 9)
  )
  
return(m)
}


# decide which language for the graph
m <- list(
  nl = fig_inout(fig, Tests_MD_Germ, ylab_nl, x_labels_nl, legend_labels_nl),
  fr = fig_inout(fig, Tests_MD_Germ, ylab_fr, x_labels_fr, legend_labels_fr),
  en = fig_inout(fig, Tests_MD_Germ, ylab_en, x_labels_en, legend_labels_en)
)


# save (in dir_figs, set in figure_helpers_TP.R)
# widths (cm) per version: proportional to the number of germs shown, so the bars keep their size
widths <- germ_widths_inout(Tests_MD_Germ, width = 16)

ggsave_langs(m, "figure_03_test_prescrib_uro_derm",
             width = widths, height = 9)
