library(readr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(RColorBrewer)
library(grid)
library(sf)
library(gridExtra)
library(cowplot)
library(lme4)
library(lmerTest)
library(viridis)
library(ggpubr)
library(tidyverse)
library(broom) 
library(broom.mixed)
library(MASS)
library(scales)
library(Hmisc)
library(ggstance)
library(shadowtext)
library(ggtext)
library(ggnewscale)
library(caret)
options(tigris_use_cache = TRUE, tigris_class = "sf")

sf::sf_use_s2(FALSE)
source("./syntax/o_function.R")

tech <- c("PV","EV","HP")
ipt <- c("WH", "KWH", "BTUNG", "TOTALBTU", "DOLLAREL", "DOLLARNG", "TOTALDOL")
otcm1 <- c("Grid (MWh/HH)", "Elec (MWh/HH)","Fossil Fuel (MMBtu/HH)","Total Energy (MMBtu/HH)","Elec Bill (k$/HH)","Fuel Bill (k$/HH)","Total Bill (k$/HH)")
otcm <- c("Grid\n(MWh/HH)", "Elec\n(MWh/HH)","Fossil Fuel\n(MMBtu/HH)","Total Energy\n(MMBtu/HH)","Elec Bill\n(k$/HH)","Fuel Bill\n(k$/HH)","Total Bill\n(k$/HH)")
name_out <- c("WH" = "Grid\n(MWh/HH)",
              "KWH" = "Elec\n(MWh/HH)",
              "BTUNG" = "Fossil Fuel\n(MMBtu/HH)",
              "TOTALBTU" = "Total Energy\n(MMBtu/HH)",
              "DOLLAREL" = "Elec Bill\n(k$/HH)",
              "DOLLARNG" = "Fuel Bill\n(k$/HH)",
              "TOTALDOL" = "Total Bill\n(k$/HH)")

ipt_r <- c("Elec", "Gas", "Energy")
otcm_r <- c("Elec\n(MWh/HH)", "Fossil Fuel\n(MMBtu/HH)","Total Energy\n(MMBtu/HH)")
name_out_r <- c("Elec" = "Elec\n(MWh/HH)",
                "Gas" = "Fossil Fuel\n(MMBtu/HH)",
                "Energy" = "Total Energy\n(MMBtu/HH)")

northeast <- c("CT","ME", "NH", "RI", "VT", "MA", "NY", "PA", "DE", "NJ", "MD")
midwest <- c("ND", "SD", "NE", "KS", "MN", "IA", "MO", "WI", "IL", "MI", "IN", "OH", "OK") #moved OK to midwest
west <- c("CA", "OR", "WA", "ID", "UT", "AZ", "NM", "NV")
south <- c("AK", "LA", "AL", "GA", "FL", "SC", "NC", "VA", "WV", "KY", "TN", "MS", "AR")
texas <- c("TX")
mtwest <- c("CO", "WY", "MT")

group_map <- c(
  "CO4B" = "KS4A", "MD5A" = "PA5A", "WA6B" = "WY6B", "CA4C" = "WA4C", 
  "CO7"  = "WY6B", "WY7"  = "WY6B", "CA2B" = "AZ2B", "CO6B" = "WY6B", 
  "NC5A" = "TN4A", "CA5B" = "ID5B", "MI7"  = "ND7",  "WY5B" = "IA5A", 
  "ME7"  = "VT6A", "TX2B" = "TX2A", "KS5A" = "IA5A", "CA4B" = "ID5B", 
  "MO5A" = "IA5A", "UT3B" = "NV3B", "WI7"  = "ND7",  "AZ5B" = "CO5B", 
  "SD5A" = "IA5A", "PA6A" = "VT6A", "TX4B" = "KS4A", "UT6B" = "WY6B", 
  "AZ4B" = "CO5B", "AL2A" = "LA2A", "AZ3B" = "NV3B", "MS2A" = "LA2A", 
  "OH4A" = "KY4A", "OR5B" = "ID5B", "NM3B" = "NV3B", "GA4A" = "TN4A", 
  "MI6A" = "WI6A", "GA2A" = "FL2A", "NH6A" = "VT6A", "NM5B" = "CO5B", 
  "MN7"  = "ND7",  "AR4A" = "MO4A", "LA3A" = "AL3A", "IL4A" = "MO4A", 
  "ID6B" = "WY6B", "TX3B" = "NV3B", "IN4A" = "MO4A", "IA6A" = "SD6A",
  "NV5B" = "ID5B", "TN3A" = "AL3A", "NM4B" = "KS4A", "WA5B" = "ID5B", 
  "WV5A" = "PA5A", "WV4A" = "VA4A", "ND6A" = "SD6A", "FL1A" = "FL2A", 
  "NY6A" = "VT6A", "NH5A" = "MA5A", "MS3A" = "AL3A", "DE4A" = "MD4A",
  "GA3A"=	"SC3A", 
  
  
  "UT5B" = "CO5B", "MT6B" = "WY6B", "NE5A"	= "IA5A", "CT5A"= "NY5A",
  "RI5A" = "NY5A", "PA4A" = "NJ4A", "AR3A" =	"AL3A",
  "ME6A" =	"VT6A", "IA5A"=	"IA5A", "DC4A"=	"MD4A", "NC3A"=	"SC3A",
  "OK3A"=	"TX3A", "NC4A"=	"TN4A", "MN6A" =	"SD6A", "OR4C"=	"WA4C",
  "MI5A" =	"IN5A", "NJ5A"=	"NY5A","NY4A"=	"NJ4A",
  
  "AL4A" = "AL3A", "CA6B" = "ID5B", "CO4A" = "KS4A","OH5A"=	"PA5A",
  "CO5A" = "IA5A", "FL3A" = "FL2A", "ID7" = "WY6B", "KS3A" = "KS4A",
  "KS4B" = "KS4A", "KS5B" = "IA5A", "MA6A" = "MA5A", "MT5B" = "ID5B",
  "NE5B" = "IA5A", "NE6A" = "IA5A", "NM6B" = "WY6B", "NV4B" = "ID5B",
  "NV6B" = "ID5B", "OK4A" = "KS4A", "OK4B" = "KS4A", "SC4A" = "SC3A",
  "TN5A" = "TN4A", "UT7" = "WY6B", "VA5A" = "TN4A", "WY5A" = "IA5A"
)


# data
dat <- read_csv(file = "./data/dat.csv") %>% 
  mutate(
    # climate = str_extract(climate, "\\d+"),
    # climate = ifelse(climate == "1", "2", 
    #                  ifelse(climate == "7", "6", climate)),
    group = paste0(state, "", climate),
    group = recode(group, !!!group_map, .default = group),
    std_weight = NWEIGHT / mean(NWEIGHT)
  ) %>% 
  dplyr::select(-starts_with("NWEIGHT"))



fct <- c("state","climate","FUELHEAT","ACEQUIPM","FUELH2O","HOUSEHOLDER_RACE")
dat[,fct] <- data.frame(apply(dat[fct], 2, as.factor))


dat_rass <- read_csv(file = "./data/dat_rass.csv") %>% 
  mutate(W_heating = ifelse(W_heating == "3", "0", W_heating)) # combine other and none
fct <- c("Climate","Heating","W_heating","Cooling","Race","Rent")
dat_rass[,fct] <- data.frame(apply(dat_rass[fct], 2, as.factor))


sc_map <- st_read("./data/sc_map_group.shp")
cl <- st_read("./data/cl.shp")
US <- read_csv("./data/US.csv")

# count adpoters by region
# df <- sc_map %>% 
#   left_join(dat, by = "group") %>% 
#   filter(!is.na(group)) %>% 
#   group_by(group) %>% 
#   summarise(
#     pv = sum(PV),
#     ev = sum(EV),
#     hp = sum(HP),
#     geometry = st_union(geometry),
#     centroid = st_point_on_surface(geometry)
#   )
# 
# # Function to build each map
# make_map <- function(data, var, title) {
#   ggplot(data) +
#     geom_sf(fill = "gray98", color = "gray40", size = 0.2) +
#     geom_sf(aes(fill = .data[[var]]), color = "gray40", size = 0.2) +
#     # geom_sf_text(
#     #   aes(label = .data[[var]], geometry = centroid),
#     #   size = 2.5, fontface = "bold", color = "gray50"
#     # ) +
#     
#     geom_shadowtext(
#       aes(label = .data[[var]], geometry = centroid),
#       stat = "sf_coordinates",
#       color = "white",        # text color
#       bg.color = "black",     # outline color
#       bg.r = 0.15,            # outline thickness
#       size = 4
#     ) +
#     
#     scale_fill_viridis_c(option = "C") +
#     coord_sf(
#       crs = st_crs(2163),
#       xlim = c(-2500000, 2500000),
#       ylim = c(-2300000, 730000),
#       expand = FALSE,
#       datum = NA
#     ) +
#     labs(title = title, fill = "") +
#     theme_minimal(base_size = 11) +
#     theme(
#       panel.grid = element_blank(),
#       axis.text = element_blank(),
#       axis.title = element_blank(),
#       axis.ticks = element_blank(),
#       plot.title = element_text(size = 12, face = "bold"),
#       legend.position = "bottom"
#     )
# }
# 
# # Build each map
# p_pv <- make_map(df, "pv", "PV")
# p_ev <- make_map(df, "ev", "EV")
# p_hp <- make_map(df, "hp", "HP")
# 
# # Arrange in one row
# ggarrange(p_pv, p_ev, p_hp, nrow = 1)
# 
# 
# df <- cl %>% 
#   left_join(dat, by = "climate") %>% 
#   filter(!is.na(climate)) %>% 
#   group_by(climate) %>% 
#   summarise(
#     pv = sum(PV),
#     ev = sum(EV),
#     hp = sum(HP),
#     geometry = st_union(geometry),
#     centroid = st_point_on_surface(geometry)
#   )
# 
# 
# # Build each map
# p_pv <- make_map(df, "pv", "PV")
# p_ev <- make_map(df, "ev", "EV")
# p_hp <- make_map(df, "hp", "HP")
# 
# # Arrange in one row
# ggarrange(p_pv, p_ev, p_hp, nrow = 1)

rate <- dat %>% 
  mutate(E_rate = DOLLAREL/KWH*1000, # in MWh
         G_rate = DOLLARNG/BTUNG*1000, # in MMBtu
         rate = TOTALDOL/TOTALBTU*1000) %>% # in MMBtu
  dplyr::select(group,E_rate:rate) 

rate[is.na(rate)] <- 0

rate_data <- rate %>% 
  gather(IV, value, E_rate:rate) %>% 
  group_by(group,IV) %>% 
  summarise(value = mean(value))



### model
# RE
da <- dat %>% dplyr::select(-FUELHEAT)
model1vars <- names(da)[3:16]
sims <- 1000
m <- 24

dd_re <- data.frame()
dg_re <- data.frame()
dg_rs <- data.frame()
test <- data.frame()

for (i in 1:7) {
  fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+ PV:EV + EV:HP + PV:HP +
                           (PV+EV+HP|group)"))
  fit <- lmer(fvar, data = da,
              weights = std_weight)

  fvar1 <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+ PV:EV + EV:HP + PV:HP +
                           (1|group)"))
  fit1 <- lmer(fvar1, data = da,
               weights = std_weight)


  # LRT results
  lrt_tbl <- broom::tidy(anova(fit, fit1)) %>%
    mutate(IV = ipt[i]) %>%
    dplyr::rename(model = term) %>%
    dplyr::select(IV, model, npar, AIC, BIC, logLik, statistic, p.value)

  test <- rbind(lrt_tbl, test)


  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)

  results <- process_fit_recs_d(fit, simbetas, ipt[i])
  dd_re <- bind_rows(dd_re, results$dd)
  dg_re <- bind_rows(dg_re, results$dg)

  tp <- sqrt(attr(ranef(fit, condVar=T)$group, "postVar"))*1.96

  d <- as.data.frame(ranef(fit)$group) %>%
    tibble::rownames_to_column("group") %>%
    dplyr::select(-`(Intercept)`) %>%
    gather(tech, R_effect, PV:HP) %>%
    mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,])) %>%
    mutate(IV = ipt[i])

  dg_rs <- rbind(d ,dg_rs)

}



# RE rass
da <- dat_rass %>% 
  mutate(across(where(is.numeric) & !c("PV","EV","HP","Elec", "Gas", "Energy"), ~ scale(.))) %>% 
  dplyr::select(-sum,-adopter,-Heating)

model1vars <- names(da)[c(1:3,5:15)]
sims <- 1000
m <- 21

dd_r <- data.frame()
dc_r <- data.frame()

for (i in 1:3) {
  fvar <- as.formula(paste(ipt_r[i], " ~", paste(model1vars, collapse = " + "), "+ (1|Climate)"))
  fit <- lmer(fvar, data = da)
  
  # fit %>% tidy() %>% View
  
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  
  results <- process_fit_rass(fit, simbetas, ipt_r[i])
  dd_r <- bind_rows(dd_r, results$dd)
  dc_r <- bind_rows(dc_r, results$dc_r)
}

# # RS
# da <- dat %>% 
#   dplyr::select(-FUELHEAT)
# 
# dg_rs <- data.frame()
# 
# for(i in 1:7){
#   model1vars <- names(da)[3:16]
#   fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+ (PV+EV+HP|group)"))
#   fit <- lmer(fvar, data = da,
#               weights = std_weight)
#   
#   tp <- sqrt(attr(ranef(fit, condVar=T)$group, "postVar"))*1.96
#   
#   d <- as.data.frame(ranef(fit)$group) %>% 
#     tibble::rownames_to_column("group") %>% 
#     dplyr::select(-`(Intercept)`) %>% 
#     gather(tech, R_effect, PV:HP) %>% 
#     mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,])) %>% 
#     mutate(IV = ipt[i]) 
#   
#   
#   dg_rs <- rbind(d ,dg_rs)
#   
# }

# # RS1
# da <- dat %>% 
#   dplyr::select(-FUELHEAT)
# 
# dg_rs1 <- data.frame()
# 
# for(i in 1:7){
#   model1vars <- names(da)[6:16]
#   fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+ (PV+EV+HP|group)"))
#   fit <- lmer(fvar, data = da,
#               weights = std_weight)
#   
#   tp <- sqrt(attr(ranef(fit, condVar=T)$group, "postVar"))*1.96
#   
#   d <- as.data.frame(ranef(fit)$group) %>% 
#     tibble::rownames_to_column("group") %>% 
#     dplyr::select(-`(Intercept)`) %>% 
#     gather(tech, R_effect, PV:HP) %>% 
#     mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,])) %>% 
#     mutate(IV = ipt[i]) 
#   
#   
#   dg_rs1 <- rbind(d ,dg_rs1)
# }


building <- c("TYPEHUQ", "SQFTEST", "YEARMADERANGE", "TOTROOMS")
residents <- c("MONEYPY", "KOWNRENT", "EDUCATION", "HOUSEHOLDER_RACE", "NHSLDMEM")

name_mapping <- c(
  "PV" = "PV",
  "EV" = "EV",
  "HP" = "HP",
  
  "TYPEHUQ" = "Multifamily",
  "SQFTEST" = "Larger Size",
  "YEARMADERANGE" = "Newer",
  "TOTROOMS" = "More Rooms",
  "ACEQUIPM1" = "Central",
  "ACEQUIPM3" = "Room",
  "ACEQUIPM4" = "Portable",
  "ACEQUIPM6" = "Others",
  "FUELH2O1" = "Fuel",
  "FUELH2O5" = "Electricity",
  
  "MONEYPY" = "Higher Income",
  "KOWNRENT" = "Rent (vs. owner)",
  "EDUCATION" = "Higher Education",
  "HOUSEHOLDER_RACE2" = "Black (vs. White)",
  "HOUSEHOLDER_RACE3" = "Others (vs. White)",
  "HOUSEHOLDER_RACE4" = "Asian (vs. White)",
  "NHSLDMEM" = "More Members",
  "PV:EV" = "PV:EV",
  "EV:HP" = "EV:HP",
  "PV:HP" = "PV:HP"
)


f2a_data <- dd_re %>%
  mutate(domain = case_when(
    var %in% building ~ "Building\nfeature",
    var %in% dd_re$var[grep("ACEQUIPM", dd_re$var)] ~ "Cooling type\n(vs. none)",
    var %in% dd_re$var[grep("FUELH2O", dd_re$var)] ~ "Water heater\n(vs. others)",
    var %in% c("PV", "EV", "HP") ~ "Tech\nadoption",
    var %in% c("PV:EV", "EV:HP", "PV:HP") ~ "Interaction",
    TRUE ~ "Resident\nfeature"
  )) %>%
  mutate(domain = factor(domain, levels = c("Building\nfeature","Cooling type\n(vs. none)","Water heater\n(vs. others)",
                                            "Resident\nfeature","Tech\nadoption","Interaction"))) %>%
  mutate(var = recode(var, !!!name_mapping)) %>%
  mutate(IV = recode(IV, !!!name_out), IV = factor(IV, levels = otcm)) %>%
  mutate(color = factor(color, levels = c("Y", "N"))) 

# model1vars <- names(da)[6:16]
# fvar <- as.formula(paste(ipt[2], " ~", paste(model1vars, collapse = " + "), "+ climate + (PV+EV+HP|state)"))
# fit <- lmer(fvar, data = da,
#             weights = std_weight)
# 
# tp <- sqrt(attr(ranef(fit, condVar=T)$state, "postVar"))*1.96
# 
# 
# f2a1_data <- ranef(fit)$state %>% 
#   tibble::rownames_to_column() %>% 
#   filter(rowname == "CA") %>% 
#   dplyr::select(PV:HP) %>% 
#   gather(var, pe, PV:HP) %>% 
#   mutate(lwr = pe - c(tp[2,2,4], tp[3,3,4], tp[4,4,4]),
#          upr = pe + c(tp[2,2,4], tp[3,3,4], tp[4,4,4]),
#          color = ifelse(lwr * upr > 0, "Y", "N"),
#          IV = "Total Elec\n(MWh/HH)",
#          domain = "Tech\nadoption",
#          class = "California (RECS)")

# tech average slope
re_average <- f2a_data %>% 
  filter(domain == "Tech\nadoption") %>% 
  dplyr::select(var, pe, IV)


building  = c("Multifam","SQFT","New","NUMROOM") #building characteristics
residents = c("Income","Rent","Edu","Race","Mmber") #demo

name_mapping <- c(
  "PV" = "PV",
  "EV" = "EV",
  "HP" = "HP",
  
  "Multifam" = "Multifamily",
  "SQFT" = "Larger Size",
  "New" = "Newer",
  "NUMROOM" = "More Rooms",
  
  "W_heating1" = "Fuel",
  "W_heating2" = "Electricity",
  "W_heating3" = "Others",
  
  "Cooling1" = "Central",
  "Cooling2" = "Others",
  "Cooling3" = "Room",
  "Cooling4" = "Multitype",
  
  "Income" = "Higher Income",
  "Rent2" = "Rent (vs. owner)",
  "Edu" = "Higher Education",
  "Race3" = "Black (vs. White)",
  "Race4" = "Others (vs. White)",
  "Race2" = "Asian (vs. White)",
  "Mmber" = "More Members"
)


f2b_data <- dd_r %>%
  mutate(domain = case_when(
    var %in% building ~ "Building\nfeature",
    var %in% dd_r$var[grep("Cooling", dd_r$var)] ~ "Cooling type\n(vs. none)",
    var %in% dd_r$var[grep("W_heating", dd_r$var)] ~ "Water heater\n(vs. others)",
    var %in% c("PV", "EV", "HP") ~ "Tech\nadoption",
    TRUE ~ "Resident\nfeature"
  )) %>%
  mutate(domain = factor(domain, levels = c("Building\nfeature", "Cooling type\n(vs. none)", "Water heater\n(vs. others)", "Resident\nfeature", "Tech\nadoption"))) %>%
  mutate(var = recode(var, !!!name_mapping)) %>%
  mutate(IV = recode(IV, !!!name_out_r), IV = factor(IV, levels = otcm_r)) %>%
  mutate(color = factor(color, levels = c("Y", "N"))) 


### new model
m2 <- rate_data %>%
  pivot_wider(names_from = IV, values_from = value) %>% 
  mutate(group1 = group) %>% 
  separate(group1, into = c('state', 'climate'), sep = 2) %>% 
  separate(climate, into = c('temp', 'moist'), sep = 1) %>% 
  dplyr::select(-state, -rate)

# RE
da <- dat %>% dplyr::select(-FUELHEAT) %>% 
  left_join(m2, by = "group") %>% 
  mutate(E_rate = scale(E_rate),
         G_rate = scale(G_rate),
         moist = ifelse(temp == 7, "7", moist),
         temp = as.numeric(temp),
         temp = scale(temp))
model1vars <- names(da)[3:16]
sims <- 1000
m <- 39

dd_re1 <- data.frame()

for (i in 1:7) {
  fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+
                           PV:EV + EV:HP + PV:HP + (PV+EV+HP|group) + 
                           PV:E_rate + PV:G_rate + PV:temp + EV:E_rate + EV:G_rate + EV:temp + HP:E_rate + HP:G_rate + HP:temp + 
                           moist + E_rate + G_rate + temp"))
  fit <- lmer(fvar, data = da,
              weights = std_weight)
  
  
  
  # fvar1 <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+
  #                          PV:EV + EV:HP + PV:HP + (1|group) + 
  #                          PV:rate + PV:temp + EV:rate + EV:temp + HP:rate + HP:temp + 
  #                          moist"))
  # fit1 <- lmer(fvar1, data = da)
  
  
  # # LRT results
  # lrt_tbl <- broom::tidy(anova(fit, fit1)) %>% 
  #   mutate(IV = ipt[i]) %>% 
  #   dplyr::rename(model = term) %>% 
  #   dplyr::select(IV, model, npar, AIC, BIC, logLik, statistic, p.value)
  # 
  # test <- rbind(lrt_tbl, test)
  
  
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  
  results <- process_fit_recs_d(fit, simbetas, ipt[i])
  dd_re1 <- bind_rows(dd_re1, results$dd)
  # dg_re <- bind_rows(dg_re, results$dg)
  # 
  # 
  # tp <- sqrt(attr(ranef(fit, condVar=T)$group, "postVar"))*1.96
  # 
  # d <- as.data.frame(ranef(fit)$group) %>% 
  #   tibble::rownames_to_column("group") %>% 
  #   dplyr::select(-`(Intercept)`) %>% 
  #   gather(tech, R_effect, PV:HP) %>% 
  #   mutate(SE = c(tp[2,2,],tp[3,3,],tp[4,4,])) %>% 
  #   mutate(IV = ipt[i]) 
  # 
  # dg_rs <- rbind(d ,dg_rs)
  
}


# comparsion between state fixed vs. (1|region) and report it is robust.
dd <- data.frame()
for (i in c(2,5)) {
  fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+
                           PV:EV + EV:HP + PV:HP + (1|group)"))
  fit <- lmer(fvar, data = da,
              weights = std_weight)
  
  
  # fit %>% tidy() %>% View
  
  fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+
                           PV:EV + EV:HP + PV:HP + state"))
  fit1 <- lm(fvar, data = da,
             weights = std_weight)
  
  m <- 24
  pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  vc <- vcov(fit)
  simbetas <- mvrnorm(sims, pe, vc)
  
  results <- process_fit_recs_e(fit, simbetas, ipt[i])
  
  # fit1 %>% tidy() %>% View
  
  m <- 72
  pe <- fit1 %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
  pe <- pe[1:m,]
  vc <- vcov(fit1)
  simbetas <- mvrnorm(sims, pe, vc)
  
  results1 <- process_fit_recs_e(fit1, simbetas, ipt[i])
  
  d <- results %>% 
    mutate(model = "Region RE") %>% 
    rbind(results1 %>% 
            filter(var %in% (results$var %>% unique())) %>% 
            mutate(model = "State FE"))
  
  dd <- bind_rows(d, dd)
}


f5a_data <- dd_re1 %>%
  mutate(domain = case_when(
    var %in% building ~ "Building\nfeature",
    var %in% dd_re$var[grep("ACEQUIPM", dd_re$var)] ~ "Cooling type\n(vs. none)",
    var %in% dd_re$var[grep("FUELH2O", dd_re$var)] ~ "Water heater\n(vs. others)",
    var %in% c("PV", "EV", "HP") ~ "Tech\nadoption",
    var %in% c("PV:EV", "EV:HP", "PV:HP") ~ "Interaction",
    TRUE ~ "Resident\nfeature"
  )) %>%
  mutate(domain = factor(domain, levels = c("Building\nfeature","Cooling type\n(vs. none)","Water heater\n(vs. others)",
                                            "Resident\nfeature","Tech\nadoption","Interaction"))) %>%
  mutate(var = recode(var, !!!name_mapping)) %>%
  mutate(IV = recode(IV, !!!name_out), IV = factor(IV, levels = otcm)) %>%
  mutate(color = factor(color, levels = c("Y", "N"))) %>% 

  filter(domain != "Interaction") %>%
  filter(IV %in% otcm[c(2,4,5,7)]) 

f5a_data1 <- f5a_data %>%
  # Keep only interaction terms for PV, EV, HP with temp or E_rate
  filter(str_detect(var, "^(PV|EV|HP):(temp|E_rate|G_rate)$")) %>%
  
  # Create grouping variables
  mutate(
    feature = case_when(
      str_detect(var, ":temp$") ~ "Colder Climate",
      str_detect(var, ":E_rate$") ~ "Higher Elec Rate",
      str_detect(var, ":G_rate$") ~ "Higher Fossil Rate"
    ),
    tech = case_when(
      str_detect(var, "^PV:") ~ "PV",
      str_detect(var, "^EV:") ~ "EV",
      str_detect(var, "^HP:") ~ "HP"
    ),
    feature = factor(feature, levels = c("Colder Climate", "Higher Elec Rate", "Higher Fossil Rate")),
    tech = factor(tech, levels = c("PV", "EV", "HP"))
  ) %>%
  
  mutate(IV = str_replace(IV, "\\n\\(MWh/HH\\)", ""),
         IV = str_replace(IV, "\\n\\(MMBtu/HH\\)", ""),
         IV = str_replace(IV, "\\n\\(k\\$/HH\\)", ""),
         tech = factor(tech, levels = c("PV","EV","HP"))) %>% 
  mutate(fill = ifelse(pe > 0, "Y", "N")) %>% 
  mutate(
    fill_combined = case_when(
      fill == "Y" & color == "Y" ~ "Y_sig",
      fill == "Y" & color == "N" ~ "Y_nonsig",
      fill == "N" & color == "Y" ~ "N_sig",
      fill == "N" & color == "N" ~ "N_nonsig"
    )
  ) 

f5a_data2 <- f5a_data %>% 
  filter(IV %in% otcm[c(2, 4, 5, 7)]) %>% 
  mutate(fill = ifelse(pe > 0, "Y", "N")) %>% 
  mutate(
    fill_combined = case_when(
      fill == "Y" & color == "Y" ~ "Y_sig",
      fill == "Y" & color == "N" ~ "Y_nonsig",
      fill == "N" & color == "Y" ~ "N_sig",
      fill == "N" & color == "N" ~ "N_nonsig"
    )
  ) %>% 
  mutate(IV = as.character(IV),
         IV = ifelse(IV == "Elec Bill\n(k$/HH)", "EBill", IV))

f5b1 <- build_waterfall_parallel(f5a_data2, "HP", "Elec", "temp", baseline = dat %>% filter(HP == 0) %>% pull(KWH) %>%  mean()) %>% 
  mutate(step_num = as.numeric(factor(step, levels = step))) %>%
  mutate(
    ci_lower = end - (end - start) + lwr,
    ci_upper = end - (end - start) + upr
  ) %>% 
  mutate(item = "HP: Electricity Use (MWh/HH)") %>% 
  rbind(
    build_waterfall_parallel(f5a_data2, "HP", "Energy", "temp", baseline = dat %>% filter(HP == 0) %>% pull(TOTALBTU) %>%  mean()) %>% 
      mutate(step_num = as.numeric(factor(step, levels = step))) %>%
      mutate(
        ci_lower = end - (end - start) + lwr,
        ci_upper = end - (end - start) + upr
      ) %>% 
      mutate(item = "HP: Energy Use (MMBtu/HH)"),
    
    build_waterfall_parallel(f5a_data2, "HP", "EBill", "temp", baseline = dat %>% filter(HP == 0) %>% pull(DOLLAREL) %>%  mean()) %>% 
      mutate(step_num = as.numeric(factor(step, levels = step))) %>%
      mutate(
        ci_lower = end - (end - start) + lwr,
        ci_upper = end - (end - start) + upr
      ) %>% 
      mutate(item = "HP: Electricity Bill (k$/HH)")) %>% 
  
  mutate(item = factor(item, levels = c("HP: Electricity Use (MWh/HH)","HP: Energy Use (MMBtu/HH)",
                                        "HP: Electricity Bill (k$/HH)")))


f5b2 <- build_waterfall_parallel(f5a_data2, "PV", "Elec", "E_rate", baseline = dat %>% filter(PV == 0) %>% pull(KWH) %>%  mean()) %>% 
  mutate(step_num = as.numeric(factor(step, levels = step))) %>%
  mutate(
    ci_lower = end - (end - start) + lwr,
    ci_upper = end - (end - start) + upr
  ) %>% 
  mutate(item = "PV: Electricity Use (MWh/HH)") %>% 
  rbind(
    build_waterfall_parallel(f5a_data2, "PV", "EBill", "E_rate", baseline = dat %>% filter(PV == 0) %>% pull(DOLLAREL) %>%  mean()) %>% 
      mutate(step_num = as.numeric(factor(step, levels = step))) %>%
      mutate(
        ci_lower = end - (end - start) + lwr,
        ci_upper = end - (end - start) + upr
      ) %>% 
      mutate(item = "PV: Electricity Bill (k$/HH)"),
    
    build_waterfall_parallel(f5a_data2, "PV", "Total Bill", "E_rate", baseline = dat %>% filter(PV == 0) %>% pull(TOTALDOL) %>%  mean()) %>% 
      mutate(step_num = as.numeric(factor(step, levels = step))) %>%
      mutate(
        ci_lower = end - (end - start) + lwr,
        ci_upper = end - (end - start) + upr
      ) %>% 
      mutate(item = "PV: Energy Bill (k$/HH)")) %>% 
  
  mutate(item = factor(item, levels = c("PV: Electricity Use (MWh/HH)","PV: Electricity Bill (k$/HH)",
                                        "PV: Energy Bill (k$/HH)")))


f5b3 <- build_waterfall_total(f5a_data2, "HP", "EBill", baseline = dat %>% filter(HP == 0) %>% pull(DOLLAREL) %>%  mean()) %>% 
      mutate(step_num = as.numeric(factor(step, levels = step))) %>%
      mutate(
        ci_lower = end - (end - start) + lwr,
        ci_upper = end - (end - start) + upr
      ) %>% 
      mutate(item = "HP: Electricity Bill (k$/HH)") 


## make it warmer from colder
f5b4 <- f5a_data2 %>% 
  mutate(
    across(c(pe, lwr, upr),
           ~ ifelse(var %in% c("PV:temp","temp"), -1 * ., .)),
    color = ifelse(lwr * upr > 0, "Y", "N"),
    fill = ifelse(pe > 0, "Y", "N"),
    fill_combined = case_when(
      fill == "Y" & color == "Y" ~ "Y_sig",
      fill == "Y" & color == "N" ~ "Y_nonsig",
      fill == "N" & color == "Y" ~ "N_sig",
      fill == "N" & color == "N" ~ "N_nonsig"
    )
  ) %>% 
  build_waterfall_total("PV", "EBill", baseline = dat %>% filter(PV == 0) %>% pull(DOLLAREL) %>%  mean()) %>%
  mutate(step = str_replace_all(step, "Colder", "Warmer")) %>% 
  mutate(step_num = as.numeric(factor(step, levels = step))) %>%
  mutate(
    ci_lower = end - (end - start) + lwr,
    ci_upper = end - (end - start) + upr
  ) %>%
  mutate(item = "PV: Electricity Bill (k$/HH)")


# f5b4 <- f5a_data2 %>% 
#   mutate(
#     across(c(pe, lwr, upr),
#            ~ ifelse(var %in% c("PV:temp","temp"), -1 * ., .)),
#     color = ifelse(lwr * upr > 0, "Y", "N"),
#     fill = ifelse(pe > 0, "Y", "N"),
#     fill_combined = case_when(
#           fill == "Y" & color == "Y" ~ "Y_sig",
#           fill == "Y" & color == "N" ~ "Y_nonsig",
#           fill == "N" & color == "Y" ~ "N_sig",
#           fill == "N" & color == "N" ~ "N_nonsig"
#         )
#       ) %>% 
#   build_waterfall_total("PV", "Elec", baseline = dat %>% filter(PV == 0) %>% pull(KWH) %>%  mean()) %>% 
#   mutate(step_num = as.numeric(factor(step, levels = step))) %>%
#   mutate(
#     ci_lower = end - (end - start) + lwr,
#     ci_upper = end - (end - start) + upr
#   ) %>% 
#   mutate(item = "PV: Electricity use (MWh/HH)") 



s_data <- dd_re1 %>%
  mutate(domain = case_when(
    var %in% building ~ "Building\nfeature",
    var %in% dd_re$var[grep("ACEQUIPM", dd_re$var)] ~ "Cooling type\n(vs. none)",
    var %in% dd_re$var[grep("FUELH2O", dd_re$var)] ~ "Water heater\n(vs. others)",
    var %in% c("PV", "EV", "HP") ~ "Tech\nadoption",
    var %in% c("PV:EV", "EV:HP", "PV:HP") ~ "Interaction",
    TRUE ~ "Resident\nfeature"
  )) %>%
  mutate(domain = factor(domain, levels = c("Building\nfeature","Cooling type\n(vs. none)","Water heater\n(vs. others)",
                                            "Resident\nfeature","Tech\nadoption","Interaction"))) %>%
  mutate(var = recode(var, !!!name_mapping)) %>%
  mutate(IV = recode(IV, !!!name_out), IV = factor(IV, levels = otcm)) %>%
  mutate(color = factor(color, levels = c("Y", "N"))) %>% 
  
  filter(domain != "Interaction") %>%
  filter(IV %in% otcm[c(1)]) %>% 
  mutate(fill = ifelse(pe > 0, "Y", "N")) %>% 
  mutate(
    fill_combined = case_when(
      fill == "Y" & color == "Y" ~ "Y_sig",
      fill == "Y" & color == "N" ~ "Y_nonsig",
      fill == "N" & color == "Y" ~ "N_sig",
      fill == "N" & color == "N" ~ "N_nonsig"
    )
  ) 

s_data_si <- build_waterfall_parallel(s_data, "PV", "Grid", "temp", baseline = dat %>% filter(PV == 0) %>% pull(KWH) %>%  mean()) %>% 
  mutate(step_num = as.numeric(factor(step, levels = step))) %>%
  mutate(
    ci_lower = end - (end - start) + lwr,
    ci_upper = end - (end - start) + upr
  ) %>% 
  mutate(item = "PV: Grid demand (MWh/HH) in colder regions") %>% 
  rbind(
    build_waterfall_parallel(s_data, "PV", "Grid", "E_rate", baseline = dat %>% filter(PV == 0) %>% pull(KWH) %>%  mean()) %>% 
      mutate(step_num = as.numeric(factor(step, levels = step))) %>%
      mutate(
        ci_lower = end - (end - start) + lwr,
        ci_upper = end - (end - start) + upr
      ) %>% 
      mutate(item = "PV: Grid demand (MWh/HH) in higer rate regions"))


f5_data <- dat %>% 
  filter(adopter %in% c("PV","EV","HP","Non")) %>% 
  # remove confounders (PV, EV adopters) to energy expenditures
  
  gather(IV,value,KWH,BTUNG,TOTALBTU,DOLLAREL,DOLLARNG,TOTALDOL) %>% 
  # mutate(key = factor(key, levels = c("KWH","BTU","TOTALBTU","DOLLAREL","BTUDOL","TOTALDOL"))) %>% 
  filter(IV %in% c("KWH","BTUNG","TOTALBTU")) %>% 
  mutate(value = value) %>% 
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm),
         adopter = factor(adopter, levels = c("Non","PV","EV","HP")),
         climate = "RECS") %>% 
  dplyr::select(adopter, value, IV, climate) %>% 
  rbind(
    dat %>% 
      filter(adopter %in% c("PV","EV","HP","Non")) %>% 
      filter(climate %in% c("3C")) %>% 
      # remove confounders (PV, EV adopters) to energy expenditures
      
      gather(IV,value,KWH,BTUNG,TOTALBTU,DOLLAREL,DOLLARNG,TOTALDOL) %>% 
      # mutate(key = factor(key, levels = c("KWH","BTU","TOTALBTU","DOLLAREL","BTUDOL","TOTALDOL"))) %>% 
      filter(IV %in% c("KWH","BTUNG","TOTALBTU")) %>% 
      mutate(value = value) %>% 
      mutate(IV = recode(IV, !!!name_out),
             IV = factor(IV, levels = otcm),
             adopter = factor(adopter, levels = c("Non","PV","EV","HP")),
             climate = "RECS (3C)") %>% 
      dplyr::select(adopter, value, IV, climate),
    dat %>% 
      filter(adopter %in% c("PV","EV","HP","Non")) %>% 
      filter(state %in% c("CA")) %>% 
      # remove confounders (PV, EV adopters) to energy expenditures
      
      gather(IV,value,KWH,BTUNG,TOTALBTU,DOLLAREL,DOLLARNG,TOTALDOL) %>% 
      # mutate(key = factor(key, levels = c("KWH","BTU","TOTALBTU","DOLLAREL","BTUDOL","TOTALDOL"))) %>% 
      filter(IV %in% c("KWH","BTUNG","TOTALBTU")) %>% 
      mutate(value = value) %>% 
      mutate(IV = recode(IV, !!!name_out),
             IV = factor(IV, levels = otcm),
             adopter = factor(adopter, levels = c("Non","PV","EV","HP")),
             climate = "RECS (CA)") %>% 
      dplyr::select(adopter, value, IV, climate) 
  )

f5b_data <- dat %>% 
  # Label PV, EV, HP adopters
  mutate(PV = ifelse(PV == 1, "PV", ""),
         EV = ifelse(EV == 1, "EV", ""),
         HP = ifelse(HP == 1, "HP", "")) %>% 
  
  # Create interaction term without dots and replace empty with "none"
  mutate(interaction_term = as.character(interaction(PV, EV, HP, sep = "")),
         interaction_term = ifelse(interaction_term == "", "None", interaction_term)) %>%
  mutate(class = "RECS") %>% 
  dplyr::select(KWH, interaction_term, class) %>% 
  rbind(
    dat %>% 
      # Label PV, EV, HP adopters
      mutate(PV = ifelse(PV == 1, "PV", ""),
             EV = ifelse(EV == 1, "EV", ""),
             HP = ifelse(HP == 1, "HP", "")) %>% 
      filter(state == "CA") %>% 
      
      # Create interaction term without dots and replace empty with "none"
      mutate(interaction_term = as.character(interaction(PV, EV, HP, sep = "")),
             interaction_term = ifelse(interaction_term == "", "None", interaction_term)) %>%
      mutate(class = "RECS (CA)") %>% 
      dplyr::select(KWH, interaction_term, class)
  )

re_data <- dg_re %>% 
  filter(group %in% rate_data$group) %>% 
  dplyr::select(group,IV,R_effect) %>% 
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm))

rs_data <- dg_rs %>% 
  filter(group %in% rate_data$group) %>% 
  dplyr::select(tech,group,IV,R_effect) %>% 
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm))

rs_data_c <- rs_data %>% 
  left_join(re_data, by = c("group","IV")) %>% 
  mutate(R_effect = R_effect.x + R_effect.y) %>% 
  dplyr::select(-R_effect.x, -R_effect.y)


reffect <- create_rate_data(
  re_data,
  rate_data,
  otcm,
  tech_name = "Non-adoption"
) %>% 
  rbind(
    create_rate_data(
      rs_data,
      rate_data,
      otcm
    ) %>%
      mutate(tech = factor(tech, levels = c("PV", "EV", "HP")))
  ) 


reffect_c <- create_rate_data(
  re_data,
  rate_data,
  otcm,
  tech_name = "Non-adoption"
) %>% 
  rbind(
    create_rate_data(
      rs_data_c,
      rate_data,
      otcm
    ) %>%
      mutate(tech = factor(tech, levels = c("PV", "EV", "HP")))
  ) 


# d_RS <- reffect %>% 
#   mutate(group1 = group) %>% 
#   separate(group1, into = c('state', 'climate'), sep = 2) %>% 
#   dplyr::select(tech,group,IV, R_effect,E_rate,G_rate,climate) %>% 
#   separate(climate, into = c('temp', 'moist'), sep = 1) %>% 
#   generate_results(name_out, c("Regional Effect", tech))

tech_levels <- c("Non-adoption", "PV", "EV", "HP")

# d_RS <- reffect %>% 
#   mutate(group1 = group) %>% 
#   separate(group1, into = c('state', 'climate'), sep = 2) %>% 
#   dplyr::select(tech,group,IV, R_effect,rate,climate) %>% 
#   separate(climate, into = c('temp', 'moist'), sep = 1) %>% 
#   generate_results_e(name_out, c("Non-adoption", tech))
# 
# d_RS_c <- reffect_c %>% 
#   mutate(group1 = group) %>% 
#   separate(group1, into = c('state', 'climate'), sep = 2) %>% 
#   dplyr::select(tech,group,IV, R_effect,rate,climate) %>% 
#   separate(climate, into = c('temp', 'moist'), sep = 1) %>% 
#   generate_results_e(name_out, c("Non-adoption", tech))


### combined
dg_rs_c <- dg_rs %>% 
  dplyr::select(-SE) %>% 
  left_join(dd_re %>% 
              filter(var %in% c("PV","EV","HP")) %>% 
              dplyr::rename(tech = var) %>% 
              dplyr::select(-lwr:-color), by = c("tech","IV")) %>%
  mutate(R_effect = R_effect + pe) 


### total combined
dg_rs_t <- dg_rs %>% 
  dplyr::select(-SE) %>% 
  left_join(dg_re %>% 
              dplyr::select(-SE:-upper) , by = c("group","IV")) %>% 
  left_join(dd_re %>% 
              filter(var %in% c("PV","EV","HP")) %>% 
              dplyr::rename(tech = var) %>% 
              dplyr::select(-lwr:-color), by = c("tech","IV")) %>%
  mutate(R_effect = R_effect.x + R_effect.y + pe) 



# m2 <- rate_data %>%
#   pivot_wider(names_from = IV, values_from = value) %>% 
#   mutate(group1 = group) %>% 
#   separate(group1, into = c('state', 'climate'), sep = 2) %>% 
#   separate(climate, into = c('temp', 'moist'), sep = 1) %>% 
#   dplyr::select(-state, -rate)

  

# ### new model
# # RE
# da <- dat %>% dplyr::select(-FUELHEAT) %>% 
#   left_join(m2, by = "group") %>% 
#   mutate(rate = scale(rate),
#          moist = ifelse(temp == 7, "7", moist),
#          temp = as.numeric(temp),
#          temp = scale(temp))
# model1vars <- names(da)[3:16]
# sims <- 1000
# m <- 30
# 
# dd_re1 <- data.frame()
# dg_re1 <- data.frame()
# 
# for (i in 1:7) {
#   fvar <- as.formula(paste(ipt[i], " ~", paste(model1vars, collapse = " + "), "+
#                            (1|group) + 
#                            PV:rate + PV:temp + EV:rate + EV:temp + HP:rate + HP:temp + 
#                            moist"))
#   fit <- lmer(fvar, data = da,
#               weights = std_weight)
# 
#   pe <- fit %>% tidy() %>% dplyr::select(estimate) %>% as.matrix()
#   pe <- pe[1:m,]
#   vc <- vcov(fit)
#   simbetas <- mvrnorm(sims, pe, vc)
# 
#   results <- process_fit_recs_d(fit, simbetas, ipt[i])
#   dd_re1 <- bind_rows(dd_re1, results$dd)
#   dg_re1 <- bind_rows(dg_re1, results$dg)
# 
# }



hdd_sum <- process_eia("./data/MER_T01_11.csv", "Heating Degree-Days", "HDD")
cdd_sum <- process_eia("./data/MER_T01_12.csv", "Cooling Degree-Days", "CDD")
