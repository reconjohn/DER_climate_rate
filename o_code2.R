source("./syntax/o_data2.R")

# f0
# Run the function
maps1 <- sc_map %>% left_join(aggregate_data(dat, ipt[-1], mean,
                                             recode_list = name_out,
                                             factor_levels = otcm), by = "group") %>% 
  create_maps(otcm[2:7], "value", number_format(accuracy = 0.1))

f0a <- cowplot::plot_grid(
  plotlist = maps1,
  labels = otcm1[-1],
  label_size = 10, nrow = 2, label_x = 0.2, hjust = 0
)

# f0b <- cowplot::plot_grid(plotlist = rate_data %>% 
#                             mapping(c("E_rate","G_rate","rate")), labels = c("Electricity rate ($/MWh)","Fuel rate ($/MMBtu)","Energy rate ($/MMBtu)"), 
#                           label_size = 10, nrow = 1,
#                           label_x = .2, hjust = 0)


maps2 <- sc_map %>% left_join(aggregate_data(dat, c("PV", "EV", "HP"), sum) %>% 
                                mutate(rate = value / total * 100), by = "group") %>% 
  create_maps(c("PV", "EV", "HP"), "rate", number_format(accuracy = 1)) 

f0c <- cowplot::plot_grid(
  plotlist = maps2,
  labels = c("PV (%)", "EV (%)", "HP (%)"),
  label_size = 10, nrow = 1, label_x = 0.2, hjust = 0
)

# f1
# Plot consolidated data

f1a <- bind_rows(
  prepare_data(dat_rass, "RASS (CA)", TRUE),
  prepare_data(dat_rass %>% filter(Climate %in% c(1, 2, 6, 8)), "RASS (3C)", TRUE),
  prepare_data(dat, "RECS"),
  prepare_data(dat %>% filter(climate == "3C"), "RECS (3C)"),
  prepare_data(dat %>% filter(state == "CA"), "RECS (CA)")
) %>%
  mutate(climate = factor(climate, levels = c("RECS", "RECS (CA)", "RASS (CA)", "RECS (3C)", "RASS (3C)"))) %>%
  filter(climate %in% c("RECS", "RECS (CA)", "RASS (CA)")) %>% 
  filter(key != "Fossil Fuel\n(MMBtu/HH)")

# write_csv(f1a, file = "./data/derived/f1a.csv")

f1a <- f1a %>% 
  create_plot("adopter", "value", "climate", "climate", "key") + 
  scale_fill_manual(values = c("skyblue","gold2","gold4")) +
  labs(title = "Consumption")

f1b <- prepare_data(dat, "RECS", F, T) %>% 
  filter(key != "Fuel Bill\n(k$/HH)")

write_csv(f1b, file = "./data/derived/f1b.csv")

f1b <- create_plot(f1b,
                   "adopter", "value", "1", "climate", "key") +
  scale_fill_manual(values = "skyblue") +
  labs(title = "Expenditures")

f1c1 <- dat %>%
  mutate(
    PV = ifelse(PV == 1, "PV", ""),
    EV = ifelse(EV == 1, "EV", ""),
    HP = ifelse(HP == 1, "HP", ""),
    interaction_term = interaction(PV, EV, HP, sep = "") %>% as.character() %>% ifelse(. == "", "Non", .)
  ) %>%
  mutate(cat = ifelse(str_detect(climate, "^[1-3]"), "Warm",
                      ifelse(str_detect(climate, "^[4]"), "Mixed", "Cold")),
         cat = factor(cat, levels = c("Warm", "Mixed", "Cold"))) %>% 
  mutate(
    interaction_term = factor(interaction_term, levels = c("Non", "PV", "EV", "PVEV", "HP", "PVHP", "EVHP", "PVEVHP")),
    class = "Elec (MWh/HH)"
  ) 

write_csv(f1c1, file = "./data/derived/f1c1.csv")

f1c1 <- f1c1 %>% 
  create_plot1("interaction_term", "KWH", "cat", "cat", "class") +
  labs(title = "Electricity Consumption") 

f1c2 <- cl %>%
  climate_maps("cat") +
  scale_fill_manual(values = scales::hue_pal()(3))


# f2
f2a <- f2a_data %>% 
  filter(domain != "Interaction") %>%
  filter(IV %in% otcm[c(2,4,7)]) %>% 
  filter(!str_detect(var, ":temp$|:rate|moist")) 

write_csv(f2a, file = "./data/derived/f2a.csv")

f2a <- f2a %>% 
  reg_plot("var", "domain") +
  labs(title = "National (RECS)") +
  theme(plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.3))

f2b <- f2b_data %>% 
  filter(IV %in% otcm_r[c(1,3)]) 

write_csv(f2b, file = "./data/derived/f2b.csv")

f2b <- f2b %>% 
  reg_plot("var", "domain")+
  labs(title = "California (RASS)") +
  theme(plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.8))

# f2c <- f2a_data %>% 
#   filter(IV %in% otcm[c(2)]) %>% 
#   filter(domain %in% c("Tech\nadoption")) %>% 
#   mutate(class = "National (RECS)") %>% 
#   rbind(
#     f2a1_data,
#     f2b_data %>% 
#       filter(IV %in% otcm_r[c(1)]) %>% 
#       filter(domain %in% c("Tech\nadoption")) %>% 
#       mutate(class = "California (RASS)")
#   ) %>% 
#   mutate(var = factor(var, levels = tech),
#          class = factor(class, levels = c("National (RECS)","California (RECS)","California (RASS)"))) %>% 
#   
#   ggplot(aes(x = pe, y = class, xmin = lwr, xmax = upr)) +
#   geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray30") +
#   geom_pointrangeh(aes(fill = color), position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5, pch = 21) +
#   facet_grid( ~ var, scale = "free", switch = "y") +
#   theme_bw() +
#   labs(x = "", y = "", fill = "Significant", title = "Total Elec (MWh/HH)") +
#   scale_fill_manual(values = c("red", "gray")) +
#   theme(
#     panel.grid.minor = element_blank(),
#     panel.grid.major.x = element_blank(),
#     strip.background = element_rect(fill = "gray22", color = "gray22"),
#     strip.text = element_text(color = "white", family = "Franklin Gothic Book", size = 9, face = "bold"),
#     legend.position = "none",
#     axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
#     axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#     axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.35)
#   )



# f3
# f3a <- cowplot::plot_grid(plotlist = r_combine(dc_re, ds_re, T) %>% 
#             mapping(otcm[c(2,4,7)], color = T), labels = otcm1[c(2,4,7)], label_size = 10, nrow = 1,
#             label_x = .2, hjust = 0)
# 
# 
# f3b <- cowplot::plot_grid(plotlist = r_combine(dc_rs, ds_rs, F) %>%
#                    mapping(otcm[c(1,2,7)], T, tech = "PV"), labels = otcm1[c(1,2,7)], label_size = 10,
#                  nrow = 1,
#                  label_x = .2, hjust = 0)
# 
# f3c <- cowplot::plot_grid(plotlist = r_combine(dc_rs, ds_rs, F) %>% 
#                             mapping(otcm[c(2,3,7)], T, tech = "EV"), labels = otcm1[c(2,3,7)], label_size = 10, 
#                           nrow = 1,
#                           label_x = .2, hjust = 0)
# 
# f3d <- cowplot::plot_grid(plotlist = r_combine(dc_rs, ds_rs, F) %>% 
#                             mapping(otcm[c(2,3,7)], T, tech = "HP"), labels = otcm1[c(2,3,7)], label_size = 10, 
#                           nrow = 1,
#                           label_x = .2, hjust = 0)

### decoupling
y_space <- 0.9
f3a <- cowplot::plot_grid(plotlist = dg_re %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], color = T), labels = otcm1[c(2,3,4,7)], label_size = 10, nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)


f3b <- cowplot::plot_grid(plotlist = dg_rs %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect)%>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "PV"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

f3c <- cowplot::plot_grid(plotlist = dg_rs %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "EV"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

f3d <- cowplot::plot_grid(plotlist = dg_rs %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "HP"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

### combined
f3b_c <- cowplot::plot_grid(plotlist = dg_rs_c %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect)%>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "PV"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

f3c_c <- cowplot::plot_grid(plotlist = dg_rs_c %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "EV"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

f3d_c <- cowplot::plot_grid(plotlist = dg_rs_c %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "HP"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)


### total combined
f3b_t <- cowplot::plot_grid(plotlist = dg_rs_t %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect)%>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "PV"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

f3c_t <- cowplot::plot_grid(plotlist = dg_rs_t %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "EV"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)

f3d_t <- cowplot::plot_grid(plotlist = dg_rs_t %>% 
                            mutate(IV = recode(IV, !!!name_out),
                                   IV = factor(IV, levels = otcm)) %>% 
                            dplyr::rename(value = R_effect) %>% 
                            mapping(otcm[c(2,3,4,7)], T, tech = "HP"), labels = otcm1[c(2,3,4,7)], label_size = 10, 
                          nrow = 1,
                          label_x = .2, label_y = y_space, hjust = 0)


### specific
# RE elec
# pv1 <- cowplot::plot_grid(plotlist = dg_re %>% 
#                      mutate(IV = recode(IV, !!!name_out),
#                             IV = factor(IV, levels = otcm)) %>% 
#                      dplyr::rename(value = R_effect) %>% 
#                      mapping(otcm[c(2)], color = T, name = "CA3B"), labels = "Non-adoption", label_size = 12, nrow = 1,
#                      label_x = .2, label_y = 0.95, hjust = 0)
# 
# # PV RS elec
# pv2 <- cowplot::plot_grid(plotlist = dg_rs_c %>% 
#                               mutate(IV = recode(IV, !!!name_out),
#                                      IV = factor(IV, levels = otcm)) %>% 
#                               dplyr::rename(value = R_effect)%>% 
#                               mapping(otcm[c(2)], T, tech = "PV", name = "CA3B"), labels = "Tech\n(average + regional)", label_size = 12, 
#                             nrow = 1,
#                           label_x = .2, label_y = 1, hjust = 0)
# 
# # PV total elec
# pv3 <- cowplot::plot_grid(plotlist = dg_rs_t %>% 
#                               mutate(IV = recode(IV, !!!name_out),
#                                      IV = factor(IV, levels = otcm)) %>% 
#                               dplyr::rename(value = R_effect)%>% 
#                               mapping(otcm[c(2)], T, tech = "PV", name = "CA3B"), labels = "Final (Non-adoption + \nTech)", label_size = 12, 
#                             nrow = 1,
#                           label_x = .2, label_y = 1, hjust = 0)

df_combined <- bind_rows(
  dg_re   %>% mutate(panel = "Non-adoption"),
  dg_rs_c %>% mutate(panel = "Tech\n(average + regional)") %>% filter(tech == "PV"),
  dg_rs_t %>% mutate(panel = "Final (Non-adoption + \nTech)") %>% filter(tech == "PV")
  # Add dg_rs_4 here for pv4 if needed
) %>%
  mutate(
    IV = recode(IV, !!!name_out),
    IV = factor(IV, levels = otcm),
    panel = factor(panel, levels = c(
      "Non-adoption", 
      "Tech\n(average + regional)", 
      "Final (Non-adoption + \nTech)"
    ))
  ) %>%
  rename(value = R_effect)


write_csv(df_combined, file = "./data/derived/f3pv.csv")

pv123 <- mapping1(
  data = df_combined, 
  x_var = otcm[c(2)], 
  color = TRUE, 
  name = "CA3B"
) +
  facet_wrap(~ panel, nrow = 1) # Facet across panel types instead


write_csv(f2a_data, file = "./data/derived/f3.csv")

# PV effects elec
pv4 <- waterplot(outcome = otcm[2], 
                 te = "PV", 
                 region = "CA3B", 
                 title = "Electricity use in Pacific 3B",
                 yaxis = "Electricity use (MWh)")

# # RE PV bill
# pv5 <- cowplot::plot_grid(plotlist = dg_re %>% 
#                             mutate(IV = recode(IV, !!!name_out),
#                                    IV = factor(IV, levels = otcm)) %>% 
#                             dplyr::rename(value = R_effect) %>% 
#                             mapping(otcm[c(7)], color = T, name = "ID5B"), labels = "Non-adoption", label_size = 12, nrow = 1,
#                           label_x = .2, label_y = 0.95, hjust = 0)
# 
# # PV RS bill
# pv6 <- cowplot::plot_grid(plotlist = dg_rs_c %>% 
#                             mutate(IV = recode(IV, !!!name_out),
#                                    IV = factor(IV, levels = otcm)) %>% 
#                             dplyr::rename(value = R_effect)%>% 
#                             mapping(otcm[c(7)], T, tech = "PV", name = "ID5B"), labels = "Tech\n(average + regional)", label_size = 12, 
#                           nrow = 1,
#                           label_x = .2, label_y = 1, hjust = 0)
# 
# # PV total bill
# pv7 <- cowplot::plot_grid(plotlist = dg_rs_t %>% 
#                             mutate(IV = recode(IV, !!!name_out),
#                                    IV = factor(IV, levels = otcm)) %>% 
#                             dplyr::rename(value = R_effect)%>% 
#                             mapping(otcm[c(7)], T, tech = "PV", name = "ID5B"), labels = "Final (Non-adoption + \nTech)", label_size = 12, 
#                           nrow = 1,
#                           label_x = .2, label_y = 1, hjust = 0)


pv567 <- mapping1(
  data = df_combined, 
  x_var = otcm[c(7)], 
  color = TRUE, 
  name = "ID5B"
) +
  facet_wrap(~ panel, nrow = 1) # Facet across panel types instead

# CO PV effects bill
pv8 <- waterplot(outcome = otcm[7], 
                 te = "PV", 
                 region = "ID5B", 
                 title = "Energy cost in Northwest 5B",
                 yaxis = "Energy cost (k$)") +
  theme(
    legend.position = "bottom"
  )


# # RE energy
# hp1 <- cowplot::plot_grid(plotlist = dg_re %>% 
#                             mutate(IV = recode(IV, !!!name_out),
#                                    IV = factor(IV, levels = otcm)) %>% 
#                             dplyr::rename(value = R_effect) %>% 
#                             mapping(otcm[c(4)], color = T, name = "VT6A"), labels = "Non-adoption", label_size = 12, nrow = 1,
#                           label_x = .2, label_y = 0.95, hjust = 0)
# 
# # HP RS energy
# hp2 <- cowplot::plot_grid(plotlist = dg_rs_c %>% 
#                             mutate(IV = recode(IV, !!!name_out),
#                                    IV = factor(IV, levels = otcm)) %>% 
#                             dplyr::rename(value = R_effect)%>% 
#                             mapping(otcm[c(4)], T, tech = "HP", name = "VT6A"), labels = "Tech\n(average + regional)", label_size = 12, 
#                           nrow = 1,
#                           label_x = .2, label_y = 1, hjust = 0)
# 
# # HP total energy
# hp3 <- cowplot::plot_grid(plotlist = dg_rs_t %>% 
#                             mutate(IV = recode(IV, !!!name_out),
#                                    IV = factor(IV, levels = otcm)) %>% 
#                             dplyr::rename(value = R_effect)%>% 
#                             mapping(otcm[c(4)], T, tech = "HP", name = "VT6A"), labels = "Final (Non-adoption + \nTech)", label_size = 12, 
#                           nrow = 1,
#                           label_x = .2, label_y = 1, hjust = 0)

df_combined <- bind_rows(
  dg_re   %>% mutate(panel = "Non-adoption"),
  dg_rs_c %>% mutate(panel = "Tech\n(average + regional)") %>% filter(tech == "HP"),
  dg_rs_t %>% mutate(panel = "Final (Non-adoption + \nTech)") %>% filter(tech == "HP")
  # Add dg_rs_4 here for pv4 if needed
) %>%
  mutate(
    IV = recode(IV, !!!name_out),
    IV = factor(IV, levels = otcm),
    panel = factor(panel, levels = c(
      "Non-adoption", 
      "Tech\n(average + regional)", 
      "Final (Non-adoption + \nTech)"
    ))
  ) %>%
  rename(value = R_effect)


write_csv(df_combined, file = "./data/derived/f3hp.csv")

hp123 <- mapping1(
  data = df_combined, 
  x_var = otcm[c(4)], 
  color = TRUE, 
  name = "VT6A"
) +
  facet_wrap(~ panel, nrow = 1) # Facet across panel types instead

# CO HP effects energy
hp4 <- waterplot(outcome = otcm[4], 
                 te = "HP", 
                 region = "VT6A", 
                 title = "Energy use in New England 6A",
                 yaxis = "Energy use (MMBtu)") 


# f40 <- ggarrange(
#   relation_plot(otcm[2], "Total Elec (MWh/HH)"),
#   # relation_plot(otcm[3], "Fossil fuel use"),
#   relation_plot(otcm[4], "Total Energy (MMBtu/HH)"),
#   # relation_plot(otcm[5], "Electricity Bill (k$/HH)"),
#   # relation_plot(otcm[6], "Fuel cost"),
#   relation_plot(otcm[7], "Total Bill (k$/HH)"),
#   # common.legend = T,
#   # legend = "right",
#   nrow = 1,
#   align = "hv",
#   hjust = -0.5
# ) %>% 
#   annotate_figure(
#     # left   = text_grob("Technology effect", rot = 90, size = 14),
#     bottom = text_grob("Non-adoption effect", size = 14)
#   )


# f4

write_csv(reffect, file = "./data/derived/f4.csv")

f4a <- rate_plot2(reffect %>% 
                    mutate(IV = str_replace_all(IV, "\n", " "),
                           IV = factor(IV, levels = otcm1)),
                  otcm1[c(2,4,7)]) 

f4b1 <- climate_plot(reffect, "Non-adoption", otcm[c(2,4,7)], T)
f4b2 <- climate_plot(reffect, "PV", otcm[c(2,4,7)])
f4b3 <- climate_plot(reffect, "EV", otcm[c(2,4,7)])
f4b4 <- climate_plot(reffect, "HP", otcm[c(2,4,7)])


f4b <- ggarrange(f4b1,f4b2,f4b3,f4b4, nrow = 1, common.legend = T,
          legend = "bottom") %>% 
  annotate_figure(
    # left   = text_grob("Technology effect", rot = 90, size = 14),
    bottom = text_grob("Climate zone", size = 14)
  )


f4c1 <- cowplot::plot_grid(plotlist = rate_data %>% 
                            mapping_r(c("E_rate")), labels = c("Electricity rate ($/MWh)"), 
                          label_size = 14, nrow = 1,
                          label_x = .2, label_y = 0.95, hjust = 0)

f4c2 <- cl %>%
  filter(climate != 8) %>%
  separate(climate, into = c('temp', 'moist'), sep = 1) %>%
  climate_maps("temp") +
  scale_fill_viridis_d(begin = 0.1, end = 1) + 
  labs(title = "Climate zone") + 
  theme(plot.title=element_text(size=14, hjust = 0.25))

# f4c2 <- cl %>%
#   separate(climate, into = c('temp', 'moist'), sep = 1) %>%
#   mutate(
#     moist = factor(moist, levels = c("A", "B", "C"))
#   ) %>%
#   filter(temp != "8") %>%
#   climate_maps("moist") +
#   scale_fill_manual(values = c("#ffff99","#fdc086","#7fc97f"))


# f5a <- plot_effects(d_RS, filter_TE = "PV", filter_IV = otcm[c(1)], 
#                     title = "Impact on Grid Electricity (MWh/HH)", facet_var = "TE", x_var = "var")
# f5b <- plot_effects(d_RS, filter_TE = c("Regional Effect", "EV", "PV", "HP"), filter_IV = otcm[2], 
#                     title = "Impact on Electricity Consumption (MWh/HH)", facet_var = "var", x_var = "TE")
# f5c <- plot_effects(d_RS, filter_TE = c("Regional Effect", "EV", "PV", "HP"), filter_IV = otcm[3], 
#                     title = "Impact on Gas Consumption (MMBtu/HH)", facet_var = "var", x_var = "TE", legend_pos = "bottom")
# f5d <- plot_effects(d_RS, filter_TE = c("Regional Effect", "EV", "HP", "PV"), filter_IV = otcm[7], 
#                     title = "Impact on Expenditures (k$/HH)", facet_var = "var", x_var = "TE", legend_pos = "bottom")
# 
# # 
# f5a1 <- plot_effects(d_RS, filter_TE = c("Regional Effect", "EV", "HP", "PV"), filter_IV = otcm[c(1,2,3,7)], filter_RE = "temp",
#                      title = "Colder Climate Effect", facet_var = "IV", x_var = "TE", legend_pos = "bottom", facet = T)
# f5b1 <- plot_effects(d_RS, filter_TE = c("Regional Effect", "EV", "HP", "PV"), filter_IV = otcm[c(1,2,3,7)], filter_RE = "E_rate",
#                      title = "Higher Electricity Rate Effect", facet_var = "IV", x_var = "TE", legend_pos = "bottom", facet = T)




# f5c1 <- d_RS %>% 
#   filter(var != "G_rate") %>% 
#   filter(effect %in% otcm[c(1,2,3,4)]) %>% 
#   mutate(Estimate = ifelse(str_detect(effect, "Elec"), Estimate*3.412, Estimate), # MWh to MMBtu
#          SE = ifelse(str_detect(effect, "Elec"), SE*3.412, SE),
#          effect = ifelse(str_detect(effect, "Elec"),
#                          str_replace(effect, "MWh", "MMBtu"),
#                          effect)) %>%
#   filter(effect %in% c("Grid Elec\n(MMBtu/HH)", "Total Elec\n(MMBtu/HH)", "Fossil Fuel\n(MMBtu/HH)", "Total Energy\n(MMBtu/HH)")) %>% 
#   mutate(effect = str_replace(effect, "\\n\\(MMBtu/HH\\)", ""),
#          effect = factor(effect, levels = c("Grid Elec", "Total Elec","Fossil Fuel","Total Energy")),
#          var = ifelse(var == "E_rate", "Higher Rate", "Colder Climate"),
#          tech = factor(tech, levels = c("Non-adoption","PV","EV","HP")),
#          color = ifelse(Estimate > 0, 1, -1)) %>% 
#   filter(!(effect == "Grid Elec" & tech != "PV")) %>% 
#   ggplot(aes(y = Estimate, x = tech, fill = factor(color))) +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
#   
#   # Bars and error bars
#   geom_col(position = position_dodge(width = 0.9), width = 0.7) +
#   geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE),
#                 width = 0.2, position = position_dodge(width = 0.9), color = "black") +
#   
#   # Faceting
#   facet_grid(var ~ effect, scales = "free_y", switch = "y", drop = TRUE) +
#   scale_fill_manual(values = c("#0072B2", "#D55E00")) + 
#   
#   # Labels and theme
#   labs(y = "Estimate (MMBtu/HH)", x = NULL, title = "Climate and Rate Effect on Energy Consumption") +
#   theme_classic(base_size = 12) +
#   theme(
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 10),
#     strip.text = element_text(face = "bold", size = 12),
#     strip.placement = "outside", 
#     strip.text.y.left = element_text(angle = 0, hjust = 0.5),
#     panel.spacing = unit(1, "lines"),
#     legend.position = "none",
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.28)
#     
#   )
# 
# data <- d_RS %>% 
#   filter(var != "G_rate") %>%
#   filter(effect %in% otcm[c(1,2,3,4)]) %>%
#   mutate(Estimate = ifelse(str_detect(effect, "Elec"), Estimate*3.412, Estimate), # MWh to MMBtu
#          SE = ifelse(str_detect(effect, "Elec"), SE*3.412, SE),
#          effect = ifelse(str_detect(effect, "Elec"),
#                          str_replace(effect, "MWh", "MMBtu"),
#                          effect)) %>%
#   filter(effect %in% c("Grid Elec\n(MMBtu/HH)", "Total Elec\n(MMBtu/HH)", "Fossil Fuel\n(MMBtu/HH)", "Total Energy\n(MMBtu/HH)")) %>%
#   mutate(effect = str_replace(effect, "\\n\\(MMBtu/HH\\)", ""),
#          effect = factor(effect, levels = c("Grid Elec", "Total Elec","Fossil Fuel","Total Energy")),
#          var = ifelse(var == "E_rate", "Higher Rate", "Colder Climate"),
#          tech = factor(tech, levels = c("Non-adoption","PV","EV","HP")),
#          color = ifelse(Estimate > 0, 1, -1))
# 
# baselines <- data %>%
#   filter(tech == "Non-adoption") %>%
#   dplyr::select(var, effect, baseline_estimate = Estimate)
# 
# plot_data <- data %>%
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   left_join(baselines, by = c("var", "effect")) %>%
#   mutate(
#     # The 'top' of every bar for error bar placement
#     total_val = ifelse(tech == "Non-adoption", Estimate, baseline_estimate + Estimate)
#   ) %>% 
#   filter(!(effect == "Grid Elec" & tech != "PV")) %>% 
#   mutate(color = ifelse(baseline_estimate > 0, 1, -1)) 
# 
# non_adopt_data <- plot_data %>% filter(tech == "Non-adoption")  
# adopt_data <- plot_data %>% filter(tech != "Non-adoption") 
# 
# 
# f5c1 <- ggplot() +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
#   
#   # Layer 1: Standalone Non-adoption reference (Far Left)
#   geom_col(data = non_adopt_data, 
#            aes(x = tech, y = Estimate, 
#                fill = factor(color)), width = 0.7) +
#   
#   # Layer 2: Baseline "shadow" bars for PV, EV, HP
#   geom_col(data = adopt_data, 
#            aes(x = tech, y = baseline_estimate, 
#                fill = factor(color)), width = 0.7) +
#   
#   scale_fill_manual(values = c("-1" = "#0072B2", "1" = "#D55E00"), 
#                     labels = c("Decrease", "Increase"), name = "Climate/Rate Effect") +
#   
#   new_scale_fill() + 
#   
#   # Layer 3: Marginal Adoption Effects (Stacked on the baseline shadows)
#   # We use geom_rect for precise positioning on top of the gray bars
#   geom_rect(data = adopt_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = baseline_estimate,
#                 ymax = total_val,
#                 fill = "adoption"), alpha = 0.7,
#             color = NA, linewidth = 0.3) +
#   
#   scale_fill_manual(
#     values = c("adoption" = "gold"),
#     labels = "",
#     name = "Adoption Effect"
#   ) +
#   
#   # Layer 4: Aligned Error Bars (Centered on the top of each stack)
#   geom_errorbar(data = plot_data,
#                 aes(x = tech, ymin = total_val - SE, ymax = total_val + SE, color = "error"),
#                 width = 0.2) +
#   
#   scale_color_manual(
#     values = c("error" = "black"),
#     labels = "Final outcome",
#     name = "Error Bar"
#   ) +
#   
#   # Faceting and Theme
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   
#   labs(y = "Estimate (MMBtu/HH)", x = NULL,
#        title = "Climate and Rate Effect on Energy Consumption") +
#   theme_classic() +
#   theme(
#     # strip.placement = "outside",
#     # panel.grid.minor = element_blank(),
#     # strip.text = element_text(face = "bold"),
#     # legend.position = "bottom",
#     
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 9),
#     strip.text = element_text(face = "bold", size = 12),
#     strip.placement = "outside",
#     strip.text.y.left = element_text(angle = 0, hjust = 0.5),
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.3)
#   ) +
#   guides(
#     color = guide_legend(order = 1),  # National Average
#     fill  = guide_legend(order = 2),   # Climate/Rate Effect
#     fill_new = guide_legend(order = 3),   # Adoption Effect
#   )
# 
# f5c2 <- d_RS %>% 
#   filter(var != "G_rate") %>% 
#   filter(effect %in% otcm[c(5,6,7)]) %>% 
#   mutate(effect = str_replace(effect, "\\n\\(\\k$/HH\\)", ""),
#          # effect = factor(effect, levels = c("Elec Bill","Fuel Bill","Total Bill")),
#          var = ifelse(var == "E_rate", "Higher Rate", "Colder Climate"),
#          tech = factor(tech, levels = c("Non-adoption","PV","EV","HP")),
#          color = ifelse(Estimate > 0, 1, -1)) %>% 
#   ggplot(aes(y = Estimate, x = tech, fill = factor(color))) +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray50", linewidth = 0.6) +
#   
#   # Bars and error bars
#   geom_col(position = position_dodge(width = 0.9), width = 0.7) +
#   geom_errorbar(aes(ymin = Estimate - SE, ymax = Estimate + SE),
#                 width = 0.2, position = position_dodge(width = 0.9), color = "black") +
#   
#   # Faceting
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   scale_fill_manual(values = c("#0072B2", "#D55E00")) + 
#   
#   # Labels and theme
#   labs(y = "Estimate (k$/HH)", x = NULL, title = "Climate and Rate Effect on Energy Bill") +
#   theme_classic(base_size = 12) +
#   theme(
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 12),
#     strip.text = element_text(face = "bold", size = 12),
#     strip.placement = "outside", 
#     strip.text.y.left = element_text(angle = 0, hjust = 0.5),
#     panel.spacing = unit(1, "lines"),
#     legend.position = "none",
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.25)
#     
#   )
# 
# data <- d_RS %>% 
#   filter(var != "G_rate") %>% 
#   filter(effect %in% otcm[c(5,6,7)]) %>% 
#   mutate(effect = str_replace(effect, "\\n\\(\\k$/HH\\)", ""),
#          # effect = factor(effect, levels = c("Elec Bill","Fuel Bill","Total Bill")),
#          var = ifelse(var == "E_rate", "Higher Rate", "Colder Climate"),
#          tech = factor(tech, levels = c("Non-adoption","PV","EV","HP")),
#          color = ifelse(Estimate > 0, 1, -1))
# 
# baselines <- data %>%
#   filter(tech == "Non-adoption") %>%
#   dplyr::select(var, effect, baseline_estimate = Estimate)
# 
# plot_data <- data %>%
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   left_join(baselines, by = c("var", "effect")) %>%
#   mutate(
#     # The 'top' of every bar for error bar placement
#     total_val = ifelse(tech == "Non-adoption", Estimate, baseline_estimate + Estimate)
#   ) %>% 
#   filter(!(effect == "Grid Elec" & tech != "PV")) %>% 
#   mutate(color = ifelse(baseline_estimate > 0, 1, -1)) 
#   
# 
# non_adopt_data <- plot_data %>% filter(tech == "Non-adoption")  
# adopt_data <- plot_data %>% filter(tech != "Non-adoption") 
# 
# 
# f5c2 <- ggplot() +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
#   
#   # Layer 1: Standalone Non-adoption reference (Far Left)
#   geom_col(data = non_adopt_data, 
#            aes(x = tech, y = Estimate, 
#                fill = factor(color)), width = 0.7) +
#   
#   # Layer 2: Baseline "shadow" bars for PV, EV, HP
#   geom_col(data = adopt_data, 
#            aes(x = tech, y = baseline_estimate, 
#                fill = factor(color)), width = 0.7) +
#   
#   scale_fill_manual(values = c("-1" = "#0072B2", "1" = "#D55E00"), 
#                     labels = c("Decrease", "Increase"), name = "Climate/Rate Effect") +
#   
#   new_scale_fill() + 
#   
#   # Layer 3: Marginal Adoption Effects (Stacked on the baseline shadows)
#   # We use geom_rect for precise positioning on top of the gray bars
#   geom_rect(data = adopt_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = baseline_estimate,
#                 ymax = total_val,
#                 fill = "adoption"), alpha = 0.7,
#             color = NA, linewidth = 0.3) +
#   
#   scale_fill_manual(
#     values = c("adoption" = "gold"),
#     labels = "",
#     name = "Adoption Effect"
#   ) +
#   
#   # Layer 4: Aligned Error Bars (Centered on the top of each stack)
#   geom_errorbar(data = plot_data,
#                 aes(x = tech, ymin = total_val - SE, ymax = total_val + SE, color = "error"),
#                 width = 0.2) +
#   
#   scale_color_manual(
#     values = c("error" = "black"),
#     labels = "Final outcome",
#     name = "Error Bar"
#   ) +
#   
#   # Faceting and Theme
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   
#   labs(y = "Estimate (k$/HH)", x = NULL,
#        title = "Climate and Rate Effect on Energy Bill") +
#   theme_classic() +
#   theme(
#     # strip.placement = "outside",
#     # panel.grid.minor = element_blank(),
#     # strip.text = element_text(face = "bold"),
#     # legend.position = "bottom",
#     
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 9),
#     strip.text = element_text(face = "bold", size = 12),
#     strip.placement = "outside",
#     strip.text.y.left = element_text(angle = 0, hjust = 0.5),
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.25)
#   ) +
#   guides(
#     color = guide_legend(order = 1),  # National Average
#     fill  = guide_legend(order = 2),   # Climate/Rate Effect
#     fill_new = guide_legend(order = 3),   # Adoption Effect
#   )
# rate_data %>% 
#   pivot_wider(names_from = IV, values_from = value) %>% 
#   ggplot(aes(x = E_rate, y = G_rate)) +
#   geom_point() +
#   geom_smooth()
###
# data <- d_RS %>% 
#   filter(var != "G_rate") %>%
#   filter(effect %in% otcm[c(1,2,3,4)]) %>%
#   mutate(Estimate = ifelse(str_detect(effect, "Elec"), Estimate*3.412, Estimate), # MWh to MMBtu
#          SE = ifelse(str_detect(effect, "Elec"), SE*3.412, SE),
#          effect = ifelse(str_detect(effect, "Elec"),
#                          str_replace(effect, "MWh", "MMBtu"),
#                          effect)) %>%
#   filter(effect %in% c("Grid Elec\n(MMBtu/HH)", "Total Elec\n(MMBtu/HH)", "Fossil Fuel\n(MMBtu/HH)", "Total Energy\n(MMBtu/HH)")) %>%
#   mutate(effect = str_replace(effect, "\\n\\(MMBtu/HH\\)", ""),
#          effect = factor(effect, levels = c("Grid Elec", "Total Elec","Fossil Fuel","Total Energy")),
#          var = ifelse(var == "E_rate", "Higher Rate", "Colder Climate"),
#          tech = factor(tech, levels = c("Non-adoption","PV","EV","HP")),
#          color = ifelse(Estimate > 0, 1, -1)) %>% 
#   rbind(
#     d_RS %>% 
#       filter(var != "G_rate") %>% 
#       filter(effect %in% otcm[c(5,6,7)]) %>% 
#       mutate(effect = str_replace(effect, "\\n\\(\\k$/HH\\)", ""),
#              # effect = factor(effect, levels = c("Elec Bill","Fuel Bill","Total Bill")),
#              var = ifelse(var == "E_rate", "Higher Rate", "Colder Climate"),
#              tech = factor(tech, levels = c("Non-adoption","PV","EV","HP")),
#              color = ifelse(Estimate > 0, 1, -1))
#   )
# 
# baselines <- data %>%
#   filter(tech == "Non-adoption") %>%
#   dplyr::select(var, effect, baseline_estimate = Estimate)
# 
# plot_data <- data %>%
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   left_join(baselines, by = c("var", "effect")) %>%
#   mutate(
#     # The 'top' of every bar for error bar placement
#     total_val = ifelse(tech == "Non-adoption", Estimate, baseline_estimate + Estimate)
#   ) %>% 
#   mutate(color = ifelse(baseline_estimate > 0, 1, -1)) 
# 
# 
# f51 <- f5plot(plot_data, "HP", c("Total Elec","Fossil Fuel","Total Energy"), F)
# f52 <- f5plot(plot_data, "PV", c("Grid Elec","Total Elec","Total Energy"), T)


write_csv(f5b1, file = "./data/derived/f51.csv")
write_csv(f5b2, file = "./data/derived/f52.csv")
write_csv(f5b3, file = "./data/derived/f53.csv")
write_csv(f5b4, file = "./data/derived/f54.csv")

f51 <- f5pl(f5b1)
f52 <- f5pl(f5b2)
f53 <- f5pl(f5b3)
f54 <- f5pl(f5b4)


s41 <- dd %>% 
  mutate(domain = case_when(
    var %in% building ~ "Building\nfeature",
    var %in% dd$var[grep("ACEQUIPM", dd$var)] ~ "Cooling type\n(vs. none)",
    var %in% dd$var[grep("FUELH2O", dd$var)] ~ "Water heater\n(vs. others)",
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
  filter(!str_detect(var, ":temp$|:rate|moist")) %>% 
  
  ggplot(aes(x = pe, y = reorder(!!sym("var"), pe), xmin = lwr, xmax = upr, color = model)) +
  geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray30") +
  geom_pointrangeh(aes(fill = color), position = position_dodge2v(height = 0.8), fatten = 4, size = 1, pch = 21) +
  facet_grid(domain ~ IV, scale = "free", switch = "y") +
  
  theme_bw(base_size = 14) +
  # coord_flip() +
  labs(x = "", y = "", fill = "Significant", 
       title = "Model comparison: Region Random Effects vs. State Fixed Effects", color = "Model") +
  scale_fill_manual(values = c("gold1", "gray50")) +
  scale_color_manual(values = c("red", "blue")) +
  theme(
    panel.grid.minor = element_blank(),
    panel.grid.major.x = element_blank(),
    strip.background = element_rect(fill = "gray90", color = "gray70"),
    strip.text = element_text(color = "black", family = "Franklin Gothic Book", size = 12, face = "bold"),
    legend.position = "right",
    axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
    axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 12),
    axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 12),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 16, face = "bold", hjust = -1)
  ) 


p_hdd <- ggplot(hdd_sum, aes(y = Region)) +
  geom_errorbarh(
    aes(xmin = Mean_2010_2025 - SD_2010_2025,
        xmax = Mean_2010_2025 + SD_2010_2025),
    height = 0.25, linewidth = 0.8, color = "#6C8CD5"
  ) +
  geom_point(aes(x = Mean_2010_2025, color = "2010–2025 Mean (±1 SD)"), size = 2.8) +
  geom_point(aes(x = Val_2020, color = "2020 Survey Year"), size = 3.2) +
  geom_text(
    aes(x = pmin(Val_2020, Mean_2010_2025 - SD_2010_2025) - 180, label = Pct_Text),
    hjust = 1, size = 2.9, color = "grey20"
  ) +
  scale_color_manual(values = c(
    "2010–2025 Mean (±1 SD)" = "#3B62C4",
    "2020 Survey Year"       = "#D95F02"
  )) +
  scale_x_continuous(limits = c(600, 7800), breaks = seq(1000, 7000, 1500)) +
  labs(
    title = "a  Heating Degree-Days (2010–2025 vs. 2020)",
    x     = "Annual Heating Degree-Days (HDD, base 65 °F)"
  ) +
  theme_nature


p_cdd <- ggplot(cdd_sum, aes(y = Region)) +
  geom_errorbarh(
    aes(xmin = Mean_2010_2025 - SD_2010_2025,
        xmax = Mean_2010_2025 + SD_2010_2025),
    height = 0.25, linewidth = 0.8, color = "#6C8CD5"
  ) +
  geom_point(aes(x = Mean_2010_2025, color = "2010–2025 Mean (±1 SD)"), size = 2.8) +
  geom_point(aes(x = Val_2020, color = "2020 Survey Year"), size = 3.2) +
  geom_text(
    aes(x = pmax(Val_2020, Mean_2010_2025 + SD_2010_2025) + 90, label = Pct_Text),
    hjust = 0, size = 2.9, color = "grey20"
  ) +
  scale_color_manual(values = c(
    "2010–2025 Mean (±1 SD)" = "#3B62C4",
    "2020 Survey Year"       = "#D95F02"
  )) +
  scale_x_continuous(limits = c(0, 3300), breaks = seq(0, 3000, 500)) +
  labs(
    title = "b  Cooling Degree-Days (2010–2025 vs. 2020)",
    x     = "Annual Cooling Degree-Days (CDD, base 65 °F)"
  ) +
  theme_nature +
  theme(axis.text.y = element_blank())
