#------------------------------------------------------------------------------#
# Script 4 - Results
#------------------------------------------------------------------------------#

# Packages

plist <- c("tidyverse",
           "janitor",   
           "writexl",   
           "readstata13",
           "readxl",     
           "LaF",        
           "survey",    
           "srvyr",     
           "distrr",    
           "radiant.data",
           "lubridate",   
           "foreign",    
           "sandwich",   
           "lmtest",     
           "jtools",     
           "stargazer",  
           "tidyr",      
           "read.dbc",   
           "data.table",
           "readxl",
           "fixest",
           "knitr",
           "kableExtra",
           "marginaleffects",
           "Hmisc",
           "xtable"
)

novos <- plist[!(plist %in% installed.packages()[,"Package"])]
if(length(novos)) install.packages(novos)
lapply(plist, require, character.only = TRUE)
rm(plist, novos)

#------------------------------------------------------------------------------#
# Loading Datasets
#------------------------------------------------------------------------------#

setwd("")

hosp <- read.csv(file = "hosp.csv")
hosp_icu <- read.csv(file = "hosp_icu.csv")
sim_tr <- read.csv(file = "sim_tr.csv")
sim_home_tr <- read.csv(file = "sim_home_tr.csv")
sim_hosp_tr <- read.csv(file = "sim_hosp_tr.csv")
sim_out_tr <- read.csv(file = "sim_out_tr.csv")
sih_icu_h <- read.csv(file = "sih_icu_h.csv")
ans_icu_h <- read.csv(file = "ans_icu_h.csv")
sih_h <- read.csv(file = "sih_h.csv")
ans_h <- read.csv(file = "ans_h.csv")
sim_se_ag <- read.csv(file = "sim_se_ag.csv")
data_o <- read.csv(file = "data_o.csv")
sim_afh <- read.csv(file = "sim_afh.csv")
df_graph <- read.csv(file = "df_graph.csv")

#------------------------------------------------------------------------------#
# 1. Figures
#------------------------------------------------------------------------------#

# Code to reproduce Fig 1 - Evolution of Mortality and ICU hospitalizations in 
# Manaus

f1 <- df_graph %>%
  ggplot() +
  geom_line(mapping = aes(x = t, y = icu, color = type), size = 1) +
  ylab("Deaths/Hospitalizations \n (per 100,000 Population)") +
  xlab("Date") +
  scale_color_discrete(name = "Series") +
  theme_bw() +
  theme(legend.position = "bottom",
        legend.title = element_blank()) +
  theme(text = element_text(size = 12),
        axis.text.x = element_text(
          angle = 0,
          vjust = 0.5,
          hjust = 0.5
        ),
        plot.margin = margin(0.5,0.5,0,0, "cm")
  ) +
  guides(color = guide_legend(ncol = 1)) +
  scale_x_continuous(
    breaks = c(
      ym("2020-01"),
      ym("2020-07"),
      ym("2021-01"),
      ym("2021-07"),
      ym("2022-01")
    ),
    labels = c(
      "Jan\n2020",
      "Jul\n2020",
      "Jan\n2021",
      "Jul\n2021",
      "Jan\n2021"
    )
  )

# Exporting figure
ggsave(
  filename = paste(res_path, "Fig1.tiff", sep = "/"),
  plot = f1,
  dpi = 300,
  width = 12,
  height = 10,
  units = "cm"
)


#------------------------------------------------------------------------------#
# 2. Descriptive Statistics ----
#------------------------------------------------------------------------------#

# Code to reproduce Table 1 - Descriptive Statistics of Rates of Mortality and 
# Hospitalizations per 100 Thousand Population

# Hospitalizations data
dt_hosp <- hosp %>%
  dplyr::select(
    mcode,
    month,
    covid_tr, 
    all_r,
    pop
  ) %>%
  rename(
    covid_h = covid_tr,
    all_h = all_r
  )

# ICU hospitalizations data
dt_hosp_icu <- hosp_icu %>%
  dplyr::select(
    covid_tr_icu,
    all_r,
    mcode,
    month
  ) %>%
  rename(
    covid_h_icu = covid_tr_icu,
    all_h_icu = all_r
  )

# Mortality data
dt_deaths <- sim_tr %>%
  dplyr::select(
    covid, 
    all_r,
    mcode,
    month
  ) %>%
  rename(
    covid_d = covid,
    all_d = all_r
  )

# Dataset to construct table 1
dt <- dt_hosp %>%
  left_join(dt_hosp_icu, by = c("mcode", "month")) %>%
  left_join(dt_deaths, by = c("mcode", "month")) %>%
  setDT()

# Variables list for table 1
vlist <- c("covid_h",
           "all_h",
           "covid_h_icu",
           "all_h_icu",
           "covid_d",
           "all_d")

# Column 1
c1 <-
  dt[, lapply(.SD, function(x) {
    sum(!is.na(x))
  }), .SDcols = c(vlist, "pop")]

# Column 2
c2 <- dt[, lapply(.SD, wtd.mean, w = pop), .SDcols = vlist]
temp <- dt[, mean(pop)] / 1000
c2 <- c2 %>% bind_cols(temp)
rm(temp)

# Column 3
c3 <- dt[, lapply(.SD, wtd.sd, w = pop), .SDcols = vlist]
temp <- dt[, sd(pop)] / 1000
c3 <- c3 %>% bind_cols(temp)
rm(temp)

# Table 1
tab1 <- data.frame(lab = c("Hospitalizations by Covid-19 ",
                          "Hospitalizations by other causes",
                          "ICU Hospitalizations by Covid-19",
                          "ICU Hospitalizations by other causes",
                          "Deaths by Covid-19",
                          "Deaths by other causes",
                          "Population (1,000 pop.)")) %>%
  bind_cols(t(c1)) %>%
  bind_cols(round(t(c2),1)) %>%
  bind_cols(round(t(c3),1)) %>%
  rename("Obs." = 2,"Mean" = 3, "Std. Dev." = 4)
colnames(tab1)[1] <- ""

rm(dt,vlist,c1,c2,c3,dt_hosp,dt_hosp_icu,dt_deaths,dt)

#------------------------------------------------------------------------------#
# 3. Objects and Fixest Configurations ----
#------------------------------------------------------------------------------#

# Rates by population

dlist_pop <- c(
  "neoplasmas",
  "circulatorio",
  "respiratorio",
  "outros",
  "endonutrimeta",
  "nervoso",
  "digestivo",
  "geniturinario",
  "infecciosas",
  "ouvido",
  "sangue",
  "pele",
  "transtornos",
  "osteomuscular",
  "olho",
  "gravidez",
  "perinatal",
  "congenitas",
  "all_r",
  "all_c",
  "externas"
)

# Labels
setFixest_dict(
  c(
    neoplasmas = "Neoplasms",
    infecciosas = "Infectous",
    all_c = "All except Covid-19",
    olho = "Eye",
    osteomuscular = "Musculoskeletal",
    ouvido = "Ear",
    pele = "Skin",
    sangue = "Blood",
    circulatorio = "Circulatory",
    respiratorio = "Respiratory",
    endonutrimeta = "Endocrine",
    outros = "Other causes",
    nervoso = "Nervous",
    digestivo = "Digestive",
    geniturinario = "Genitourinary",
    transtornos = "Mental",
    externas = "External",
    gravidez = "Pregnancy",
    perinatal = "Perinatal",
    congenitas = "Malformations",
    todas_parto = "Pregnancy (All)",
    state_month = "Interacions State-Month",
    covid_tr = "Hospitalizations by COVID-19",
    covid_tr_icu = "ICU Hospitalizations by COVID-19",
    covid_sih_icu = "ICU Hospitalizations by COVID-19 (SUS)",
    covid_ans_icu = "ICU Hospitalizations by COVID-19 (ANS)",
    all_r = "All causes (except COVID-19 and respiratory)"
  ))


#------------------------------------------------------------------------------#
# 4. Regressions
#------------------------------------------------------------------------------#

# Codes to produce Tables 2 to 10, and Table S1

#------------------------------------------------------------------------------#
## 4.1. Regressions of Rates of Hospitalizations - Total ----
#------------------------------------------------------------------------------#


### i. Regressions ----

r_hosp_pop <-
  feols(
    data = hosp,
    fml = c(neoplasmas,
            circulatorio,
            respiratorio,
            outros,
            endonutrimeta,
            nervoso,
            digestivo,
            geniturinario,
            infecciosas,
            ouvido,
            sangue,
            pele,
            transtornos,
            osteomuscular,
            olho,
            gravidez,
            perinatal,
            congenitas,
            all_r,
            all_c,
            externas
    ) ~ covid_tr, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop,
    cluster = ~mcode
  )

### ii. Elasticities ----

el_hosp_pop <-
  elast(
    data = hosp,
    dlist = dlist_pop,
    res = r_hosp_pop,
    wt = "pop",
    covid.var = "covid_tr"
  )


#------------------------------------------------------------------------------#
## 4.2. Regressions of Rates of Hospitalizations - ICU ----
#------------------------------------------------------------------------------#

### i. Regressions ----
r_hosp_pop_icu <-
  feols(
    data = hosp_icu,
    fml = c(respiratorio,
            all_r
    ) ~ covid_tr_icu, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )

### ii. Elasticities ----
el_hosp_pop_icu <-
  elast(
    data = hosp_icu,
    dlist = dlist_pop,
    res = r_hosp_pop_icu,
    wt = "pop",
    covid.var = "covid_tr_icu"
  )

#------------------------------------------------------------------------------#
## 4.3. Regressions of Rates of Mortality ----
#------------------------------------------------------------------------------#

### i. Regressions ----

r_deaths_pop <-
  feols(
    data = sim_tr,
    fml = c(neoplasmas,
            circulatorio,
            respiratorio,
            outros,
            endonutrimeta,
            nervoso,
            digestivo,
            geniturinario,
            infecciosas,
            ouvido,
            sangue,
            pele,
            transtornos,
            osteomuscular,
            olho,
            gravidez,
            perinatal,
            congenitas,
            all_r,
            all_c,
            externas
    ) ~ covid_tr, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )

### ii. Elasticities ----

el_deaths_pop <-
  elast(
    data = sim_tr,
    dlist = dlist_pop,
    res = r_deaths_pop,
    wt = "pop",
    covid.var = "covid_tr"
  )

#------------------------------------------------------------------------------#
## 4.4 Regressions of Rates of Mortality by Place of Death ----
#------------------------------------------------------------------------------#

### i. Regressions - deaths at home ----

r_deaths_pop_home <-
  feols(
    data = sim_home_tr,
    fml = all_r ~ covid_tr, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )

### ii. Regressions - deaths at hospitals ----

r_deaths_pop_hosp <-
  feols(
    data = sim_hosp_tr,
    fml = all_r ~ covid_tr, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )

### iii. Elasticities - deaths at home ----

el_deaths_pop_home <-
  elast(
    data = sim_home_tr,
    dlist = "all_r",
    res = r_deaths_pop_home,
    wt = "pop",
    covid.var = "covid_tr"
  )

### iv. Elasticities - deaths at hospitals ----

el_deaths_pop_hosp <-
  elast(
    data = sim_hosp_tr,
    dlist = "all_r",
    res = r_deaths_pop_hosp,
    wt = "pop",
    covid.var = "covid_tr"
  )


#------------------------------------------------------------------------------#
## 4.5. Regressions of Rates of Mortality on the COVID-19 ICU Hospitalizations Rate ----
#------------------------------------------------------------------------------#

### i. Regressions ----

r_deaths_pop_icu <-
  feols(
    data = sim_tr,
    fml = all_r ~ covid_tr_icu, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )


### ii. Elasticities ----

el_deaths_pop_icu <-
  elast(
    data = sim_tr,
    dlist = "all_r",
    res = r_deaths_pop_icu,
    wt = "pop",
    covid.var = "covid_tr_icu"
  )

#------------------------------------------------------------------------------#
## 4.6. Regressions of Mortality Rate by Socioeconomic Groups ----
#------------------------------------------------------------------------------#

r_deaths_se <-
  feols(
    data = sim_se_ag,
    fml = c(
      all_r_ed1,
      all_r_ed2,
      all_r_ed3,
      all_r_ed4,
      all_r_age1,
      all_r_age2,
      all_r_age3,
      all_r_age4,
      all_r_bl.in,
      all_r_wh.as) ~ covid_tr, 
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )


#------------------------------------------------------------------------------#
## 4.7. Regressions of Mortality Rate Away from Home ----
#------------------------------------------------------------------------------#

r_deaths_afh <-
  feols(
    data = sim_afh,
    fml = c(
      afh,
      afh_hosp,
      afh_casa,
      afh_r,
      afh_r_hosp,
      afh_r_casa
    ) ~ covid_tr_icu,
    fixef = c("mcode", "state_month"),
    weights  = ~ pop
  )


#------------------------------------------------------------------------------#
## 4.8. Estimations with No Covariates ----
#------------------------------------------------------------------------------#

### i. Regression - Hospitalizations rate on COVID-19 Hospitalizations rate ----
r_hosp_pop_nw <-
  feols(
    data = hosp,
    fml = all_r ~ covid_tr,
    weights = ~pop,
    fixef = "mcode",
    cluster = ~mcode
  )

### ii. Regression - Mortality rate on COVID-19 Hospitalizations rate ----
r_deaths_pop_nw <-
  feols(
    data = sim_tr,
    fml = all_r ~ sw(covid_tr,covid_tr_icu),
    weights = ~pop,
    fixef = "mcode",
    cluster = ~mcode
  )


### iii. Elasticities - Hospitalizations rate on COVID-19 Hospitalizations rate ----
el_hosp_pop_nw <- 
  elast(
    data = hosp,
    dlist = "all_r",
    res = r_hosp_pop_nw,
    wt = "pop",
    covid.var = "covid_tr"
  )

### iv. Elasticities - Mortality rate on COVID-19 Hospitalizations rate ---- 
el_deaths_pop_nw1 <-
  elast(
    data = sim_tr,
    dlist = "all_r",
    res = r_deaths_pop_nw$`rhs: covid_tr`,
    wt = "pop",
    covid.var = "covid_tr"
  )

### v. Elasticities - Mortality rate on COVID-19 ICU Hospitalizations rate ----
el_deaths_pop_nw2 <-
  elast(
    data = sim_tr,
    dlist = "all_r",
    res = r_deaths_pop_nw$`rhs: covid_tr_icu`,
    wt = "pop",
    covid.var = "covid_tr_icu"
  )

#------------------------------------------------------------------------------#
# 4.9 Tables ----
#------------------------------------------------------------------------------#

setFixest_etable(
  digits = "s3",
  digits.stats = "s3",
  coefstat = "confint",
  se.below = T,
  ci = 0.95
)

# Table 2: Effects of Hospitalizations by COVID-19 on Mortality and Hospitalizations by Other Causes
etable(
  r_hosp_pop_nw,
  r_deaths_pop_nw$`rhs: covid_tr`,
  r_hosp_pop$`lhs: all_r`,
  r_deaths_pop$`lhs: all_r`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~ mcode,
  headers = list(
    ":_:" = list("Unadjusted" = 2, "Adjusted" = 2),
    `Dep. Var.` = rep(c("Hosp.", "Mortality"), times = 2)
  ),
  depvar = F,
  fixef.group=list("Municipality"="mcode"),
  extralines = list("_Elasticity" = c(
    signif(el_hosp_pop_nw$elast[el_hosp_pop_nw$disease == "all_r"], digits = 2),
    signif(el_deaths_pop_nw1$elast[el_deaths_pop_nw1$disease == "all_r"], digits = 2),
    signif(el_hosp_pop$elast[el_hosp_pop$disease == "all_r"], digits = 2),
    signif(el_deaths_pop$elast[el_deaths_pop$disease == "all_r"], digits = 2)
  ))
)

# Table 3: Effects of ICU Hospitalizations by COVID-19 on ICU Hospitalizations and Mortality by Other Causes
etable(
  r_hosp_pop_icu$`lhs: all_r`,
  r_deaths_pop_icu,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    `Dep. Var.` = c("ICU Hosp.", "Mortality")
  ),
  depvar = F,
  fixef.group=list("Municipality"="mcode"),
  extralines = list("_Elasticity" = c(
    signif(el_hosp_pop_icu$elast[el_hosp_pop_icu$disease == "all_r"], digits = 2),
    signif(el_deaths_pop_icu$elast[el_deaths_pop_icu$disease == "all_r"], digits = 2)
  ))
)


# Table 4: Hospitalizations by COVID-19 and Deaths by Other Causes (per 100 Thousand Population) by Place of Death
etable(
  r_deaths_pop_home,
  r_deaths_pop_hosp,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    `Dep. Var.` = c("Households", "Hospitals")
  ),
  depvar = F,
  fixef.group=list("Municipality"="mcode"),
  extralines = list("_Elasticity" = c(
    signif(el_deaths_pop_home$elast[el_deaths_pop_home$disease == "all_r"], digits = 2),
    signif(el_deaths_pop_hosp$elast[el_deaths_pop_hosp$disease == "all_r"], digits = 2)
  ))
)


# Table 5: Effects of ICU Hospitalizations by COVID-19 on the Percentage of Deaths Away from the Municipality of Residence
etable(
  r_deaths_afh$`lhs: afh_r`,
  r_deaths_afh$`lhs: afh_r_casa`,
  r_deaths_afh$`lhs: afh_r_hosp`,
  r_deaths_afh$`lhs: afh`,
  r_deaths_afh$`lhs: afh_casa`,
  r_deaths_afh$`lhs: afh_hosp`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    ":_:" = list("Non-COVID-19 Mort. Rate" = 3, "All-Cause Mort. Rate" = 3),
    `Dep. Var.` = rep(c("All", "Household","Hospitals"), times = 2)
  ),
  depvar = F,
  fixef.group=list("Municipality"="mcode")
)

# Table 6: Effects of Hospitalizations by COVID-19 on Deaths by Other Causes by Educational Levels
etable(
  r_deaths_se$`lhs: all_r_ed1`,
  r_deaths_se$`lhs: all_r_ed2`,
  r_deaths_se$`lhs: all_r_ed3`,
  r_deaths_se$`lhs: all_r_ed4`,
  r_deaths_pop$`lhs: all_r`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    `Dep. Var.` = c("Less than middle sch.", "Middle Sch.","High Sch.","College","Total")
  ),
  depvar = F,
  fixef.group=list("Municipality"="mcode")
)


# Table 7: Effects of Hospitalizations by COVID-19 on Deaths by Other Causes by Race
etable(
  r_deaths_se$`lhs: all_r_bl.in`,
  r_deaths_se$`lhs: all_r_wh.as`,
  r_deaths_pop$`lhs: all_r`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    `Dep. Var.` = c("Black or Indigenous", "White or Asian","Total")
  ),
  depvar = F,
  fixef.group=list("Municipality"="mcode")
)


# Table 8: Effects of Hospitalizations by COVID-19 on Deaths by Other Causes by Age Groups
etable(
  r_deaths_se$`lhs: all_r_age1`,
  r_deaths_se$`lhs: all_r_age2`,
  r_deaths_se$`lhs: all_r_age3`,
  r_deaths_se$`lhs: all_r_age4`,
  r_deaths_pop$`lhs: all_r`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    `Dep. Var.` = c(
      "Children (0 yrs to 15 yrs)",
      "Youth (16 yrs to 29 yrs)",
      "Adults (30 yrs to 59 yrs)",
      "Seniors (60 yrs or more)",
      "Total"
    )
  ),
  depvar = F,
  fixef.group = list("Municipality" = "mcode")
)


# Table 9: Hospitalizations by COVID-19 and Hospitalizations by Specific Causes (per 100 Thousand Population)

etable(
  r_hosp_pop$`lhs: digestivo`,
  r_hosp_pop$`lhs: circulatorio`,
  r_hosp_pop$`lhs: geniturinario`,
  r_hosp_pop$`lhs: externas`,
  r_hosp_pop$`lhs: neoplasmas`,
  r_hosp_pop$`lhs: osteomuscular`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  se.below = F,
  fixef.group=list("Municipality"="mcode")
) 


# Table 10: Hospitalizations by COVID-19 and Deaths by Specific Causes (per 100 Thousand Population)
etable(
  r_deaths_pop$`lhs: circulatorio`,
  r_deaths_pop$`lhs: endonutrimeta`,
  r_deaths_pop$`lhs: nervoso`,
  r_deaths_pop$`lhs: neoplasmas`,
  r_deaths_pop$`lhs: infecciosas`,
  r_deaths_pop$`lhs: externas`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~ mcode,
  se.below = F,
  fixef.group=list("Municipality"="mcode")
)


# Table S1: ICU Hospitalizations by COVID-19 and ICU Hospitalizations by Respiratory and Other Causes (per 100 Thousand Population)
etable(
  r_hosp_pop_icu$`lhs: respiratorio`,
  r_hosp_pop_icu$`lhs: all_r`,
  keepFactors = FALSE,
  signif.code = NA,
  cluster = ~mcode,
  headers = list(
    `Dep. Var.` = c(
      "Respiratory",
      "Other"
    )
  ),
  depvar = F,
  fixef.group = list("Municipality" = "mcode"),
  extralines = list("_Elasticity" = c(
    signif(el_hosp_pop_icu$elast[el_hosp_pop_icu$disease == "respiratorio"], digits = 2),
    signif(el_hosp_pop_icu$elast[el_hosp_pop_icu$disease == "all_r"], digits = 2)
  ))
)
