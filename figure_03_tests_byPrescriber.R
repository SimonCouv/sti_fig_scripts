rm(list=ls())

# Import and pre-process the STI testing data from RIZIV.

# sciensano theme
source("sti_theme_ggplot.R")

#specify years (until when - test/diagnoses)
source("prep_figure.R")

# helpers for the versions with/without syphilis (dir_figs, langs, fig_inout, germ_widths_inout, ggsave_langs)
# (sourced before setwd() below, from the same folder as prep_figure.R)
source("figure_helpers_TP.R")


# leave the raw data from RIZIV within the R package, but perform analysis only within the ANALYSES folder
#- as this output (per age group / region) is on its own
setwd("X:/COMMUN/IST/ANALYSES/EpilaboSTI_Rpackage/EpilaboSTI")

# STI data RIZIV : by age group / sex /region
SOI_2016_24_Prescriber <- readxl::read_excel(
  "data-raw/Testing_STI/SOI_per_labo_2016-24_Region.xlsx",
  sheet = "tests_by_prescriber"
)


# rows with missing values in "qty": 1-5 => fill in 3
SOI_2016_24_Prescriber_imp <- SOI_2016_24_Prescriber %>%
  mutate(
    qty = coalesce(qty, 3)
  )



# remove nomen-codes that are not used in the STI programs
table(SOI_2016_24_Prescriber_imp$nomen_code, useNA = "always")

Tests_Prescriber_UsedNomen <- SOI_2016_24_Prescriber_imp %>%
  mutate(
    NomenCode = case_when(
      nomen_code %in% c("550255", "550266") ~ "CHLAM_solo",
      nomen_code %in% c("550911", "550922") ~ "GONO_solo",
      nomen_code %in% c("550196" , "550200") ~ "CHLAM_GONO",
      nomen_code %in% c("552731" , "552742") ~ "SYF"
    )
  ) %>%
  filter(!is.na(NomenCode))

# Q4Amaryl: 552716 - 552720 syphilis code not in older datasets
# => to be used? no
# Q4Amaryl: 550675 - 550686 chlamydia code not in new dataset
# => culture to be removed? yes


# summarise tests by pathogen
Tests_Prescriber_Pathogen <- Tests_Prescriber_UsedNomen %>%
  # Q: why are there per lab/nomen_code/gender/year/month > 1 row of data??
  # Q: why are there negative values in qty??
  # A: post-hoc corrections done by RIZIV
  summarise(
    .by = c(Year, prescriber_qual , NomenCode),
    qty = sum(qty, na.rm = T)
  ) %>%
  pivot_wider(values_from = qty, names_from = NomenCode, values_fill = 0) %>%
  # for CHLAMYDIA & GONO: add up the solo and combo tests
  mutate(
    CHLTRA = CHLAM_solo + CHLAM_GONO,
    NEIGON = GONO_solo + CHLAM_GONO,
    TREPAL = SYF
  ) %>%
  pivot_longer(c(CHLTRA, NEIGON, TREPAL), names_to = "Germ", values_to = "qty") %>%
  # keep only rows with tests
  filter(qty > 0) %>%
  select(Year, prescriber_qual, Germ, qty) 

# 
# # define levels to turn riziv numbers in factor
# referentie : https://www.riziv.fgov.be/SiteCollectionDocuments/bevoegdheidscodes_artsen.pdf
Tests_Prescriber_Pathogen <- Tests_Prescriber_Pathogen %>%
  mutate(MD_type = case_when(
    prescriber_qual %in% c("000","001","002","003","004","005","006","007", "008","009") ~ "GP",
    prescriber_qual %in% c("034","340", "348", "978") ~ "GYN",
    prescriber_qual %in% c("080","800", "090", "900") ~ "ER",
    prescriber_qual %in% c("018","058", "062", "065", "066", "073", "077", "079", 
                         "180", "182", "184", "192", 
                         "580", "582", "583", "584", "585", "586", "587", "589", 
                         "590", "591", "593", "597", "598", "599", "600", "601", 
                         "602", "603", "620", "623", "624", "628", "629", "650", "653", 
                         "659", "660", "668", "730", "734", "737", "738", "739", "985", "987", 
                         "989", "996") ~ "INT",
    
    prescriber_qual %in% c("010","014", "017", "021", "037", "041", "045", "048", "052", 
                         "055", "069", "078", "081", "083", "086", "087", "088", "093", 
                         "096", "097", 
                         "100" , "101", "109", "119", "140", "145", "149", "153", "170", 
                         "174", "192", "210", "222", "370", "374",  "378", "398", "402", "410", 
                         "414", "422", "450", "454", "480", "481", "489", "494", "496", 
                         "520", "550", "690", "691", "693", "694", "696", "698", 
                         "699", "760", "764", "770", "774", "779", "780", "784", "788", "790", 
                         "794", "795", "796", "798", "799", "810", "811", "830", "834", 
                         "836", "860", "861", "862", "867", "868", "870", "875", "880", 
                         "930", "939", "960", "970", "983", "988", "990", "991", "993", "994", "995", "997" , "999") ~ "Oth", 
    TRUE ~ NA_character_# keep existing value (or use NA_character_)
  ))

#check if all are completed
Tests_Prescriber_Pathogen %>%
  filter(is.na(MD_type ))




# summarise tests by pathogen & Germ
Tests_MD_Germ <- Tests_Prescriber_Pathogen %>%
  group_by(Year, Germ)%>%
  mutate(tot_germ = sum(qty, na.rm = T))%>%
  ungroup()%>%
  group_by(Year, MD_type, Germ) %>%
  mutate( tot_MD = sum(qty, na.rm = T), 
          pct = tot_MD/tot_germ) %>%
  select(Year, Germ, MD_type, tot_MD,tot_germ, pct)%>%
  distinct()

library(scales)

y_nl <- "Percentage tests per pathogeen"
y_fr <- "Pourcentage de tests par pathogène"
y_en <- "Percentage of tests per germ"

x_labels_nl <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorroe", TREPAL = "Syfilis")
x_labels_fr <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhée", TREPAL = "Syphilis")
x_labels_en <- c(CHLTRA = "Chlamydia", NEIGON = "Gonorrhoea", TREPAL = "Syphilis")

# legend labels and colours named by prescriber type (MD_type), so they stay with the right
# prescriber type if one has no tests in a version (e.g. syphilis only)
legend_labels_nl <- c(ER = "Acute Geneeskunde & Spoedgen.", GP = "Huisartsen", GYN = "Gynaecol. & Verlosk.", INT = "Internisten", Oth = "Andere")
legend_labels_fr <- c(ER = "Médecine aiguë & Urg.", GP = "Médecins généralistes", GYN = "Gynécol. & Obst", INT = "Médecins internistes",
  Oth = "Autres")
legend_labels_en <- c(ER = "Acute Medicine & ER", GP = "General Practitioners", GYN = "Gynecology & OB", INT = "Internists",
                           Oth = "Other" )

SSC_MD <- setNames(SSC[1:5], c("ER", "GP", "GYN", "INT", "Oth"))

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

m <- dat %>%
  ggplot(aes(x=Germ, y = pct, fill = MD_type))+
  geom_col(width = 0.8, position = position_dodge(width = 0.8)) +
  scale_x_discrete(labels= x_labels, expand = expansion(mult = c(0.15, 0.15))) +
  scale_y_continuous(limits = c(0, y_max), labels = percent_format(accuracy = 1)) +

  scale_fill_manual(
    name = NULL,
    values = SSC_MD, 
    labels = legend_labels
  )+
  labs(x = "", y = ylab)+ 
  sti_theme()
return(m)
}

# decide which language for the graph
m <- list(
  nl = fig_inout(fig, Tests_MD_Germ, y_nl, x_labels_nl, legend_labels_nl),
  fr = fig_inout(fig, Tests_MD_Germ, y_fr, x_labels_fr, legend_labels_fr),
  en = fig_inout(fig, Tests_MD_Germ, y_en, x_labels_en, legend_labels_en)
)

m$nl$all
setwd("X:/COMMUN/IST/ANALYSES/2026")

# save (in dir_figs, set in figure_helpers_TP.R)
# widths (cm) per version: proportional to the number of germs shown, so the bars keep their size
widths <- germ_widths_inout(Tests_MD_Germ, width = 16)

ggsave_langs(m, "figure_00_test_prescriber",
             width = widths, height = 9)




# #############################################################################################################
# #                                         Visualisations
# ##############################################################################################################
# tests_population_new_agegroups%>%
#   ggplot()+
#   facet_wrap(Gender ~ Germ )+
#   geom_line(aes(x =year_prest, y = BEL_tests_pop_1k, color = Age_cat_new))+
#   labs(title = "tests per population (age)")
# 
# 
# 
# 
# #TESTS region vs pathogen in women
# tests_population %>%
#   filter(Gender == "F" & Region != "UNK")%>%
#   ggplot()+
#   facet_wrap(Region ~ Germ )+
#   geom_line(aes(x =year_prest, y = Reg_tests_pop_1k, color = Age_cat))+
#   labs(title = "tests per population (age/region) - Women")
# 
# tests_population_new_agegroups%>%
#   filter(Gender == "F" & Region != "UNK")%>%
#   ggplot()+
#   facet_wrap(Region ~ Germ )+
#   geom_line(aes(x =year_prest, y = Reg_tests_pop_1k, color = Age_cat))+
#   labs(title = "tests per population (age/region) - Women")
# 
# #TESTS region vs pathogen in women
# tests_population %>%
#   filter(Gender == "M" & Region != "UNK")%>%
#   ggplot()+
#   facet_wrap(Region ~ Germ )+
#   geom_line(aes(x =year_prest, y = Reg_tests_pop_1k, color = Age_cat))+
#   labs(title = "tests per population (age/region) - Men")
# 
# tests_population_new_agegroups%>%
#   filter(Gender == "M" & Region != "UNK")%>%
#   ggplot()+
#   facet_wrap(Region ~ Germ )+
#   geom_line(aes(x =year_prest, y = Reg_tests_pop_1k, color = Age_cat))+
#   labs(title = "tests per population (age/region) - Men")
# 
# 
# 
