# Figures
ggsave <- function(..., bg = 'white') ggplot2::ggsave(..., bg = bg)
ggsave("./fig/f7.png", 
       ggarrange(f0a, f0c,
                 nrow = 2, 
                 heights = c(2, 1), 
                 labels = c("a", "b"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 font.label = list(size = 18, face = "bold")), width = 12, height = 8, dpi = 300) 


ggsave("./fig/f1.png", 
       ggarrange(ggarrange(f1a, f1b, nrow = 1, common.legend = TRUE, legend="bottom"),
                 ggarrange(f1c1, f1c2, nrow = 1, common.legend = TRUE, legend="bottom"), 
                 nrow=2, heights = c(1,1),
                 labels = c("a", "b"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = -0.1,
                 font.label = list(size = 16, face = "bold")) + 
         theme(plot.margin = margin(t = 20, r = 5, b = 5, l = 5)),
       width = 12, height = 8, dpi = 300) 


# ggsave("./fig/f2.png", 
#        ggarrange(
#          f2a,
#          ggarrange(
#            f2b, f2c,
#            heights = c(1, 0.3),
#            nrow = 2,
#            labels = c("B", "C"),
#            label.x = 0.05, label.y = 1, hjust = 0, vjust = 1,
#            font.label = list(size = 14, face = "bold"),
#            align = "v"
#          ),
#          nrow = 1, widths = c(1, 0.8),
#          common.legend = TRUE, legend = "bottom",
#          labels = c("A", "", ""),  # Avoid duplicate labels
#          label.x = 0.05, label.y = 1, hjust = 0, vjust = 1,
#          font.label = list(size = 14, face = "bold"),
#          align = "h"
#        ),
#          width = 12, height = 8, dpi = 300)
ggsave("./fig/f2.png", 
       ggarrange(
         f2a,
         f2b,
         nrow = 1, widths = c(1, 0.7),
         common.legend = TRUE, legend = "bottom",
         labels = c("a", "b"),  # Avoid duplicate labels
         label.x = 0.08, label.y = 1, hjust = 0, vjust = 1,
         font.label = list(size = 16, face = "bold")
         # align = "h"
       ),
       width = 12, height = 8, dpi = 300)




# ggsave("./fig/f3c.png", 
#        ggarrange(ggarrange(pv1,pv2,pv3, pv4, nrow = 1),
#                  ggarrange(hp1,hp2,hp3, hp4, nrow = 1),
#                  ggarrange(pv5,pv6,pv7, pv8, nrow = 1),
#                  nrow = 3,
#                  labels = c("A Electricity use variation by PV adoption (Total Elec, MWh/HH)", 
#                             "B Energy use variation by HP adoption (Total Energy, MMBtu/HH)",
#                             "C Energy cost variation by PV adoption (Total Bill, k$/HH)" 
#                             ),  # Adds labels to plots
#                  label.x = 0,        # Adjust horizontal position of labels
#                  label.y = 1,        # Adjust vertical position of labels
#                  hjust = 0, vjust = 0.4,
#                  font.label = list(size = 14, face = "bold"))+
#          theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 10)


ggsave("./fig/f3c.png", 
       ggarrange(ggarrange(pv123, pv4, nrow = 1, widths = c(3,1)),
                 ggarrange(hp123, hp4, nrow = 1, widths = c(3,1)),
                 ggarrange(pv567, pv8, nrow = 1, widths = c(3,1)),
                 nrow = 3,
                 labels = c("a Electricity use variation by PV adoption (Total Elec, MWh/HH)", 
                            "b Energy use variation by HP adoption (Total Energy, MMBtu/HH)",
                            "c Energy cost variation by PV adoption (Total Bill, k$/HH)" 
                 ),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 0.4,
                 font.label = list(size = 14, face = "bold"))+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), 
       width = 12, height = 10)

ggsave("./fig/f4.png", 
       ggarrange(f4a,
                 f4b,
                 ggarrange(f4c1,f4c2, nrow = 1,
                           widths = c(1,0.6)),
                 labels = c("a  Marginal effects vs. electricity rates", 
                            "b  Marginal effects vs. climate zone",
                            "c"),  # Adds labels to plots
                 label.x = 0.02,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 0,
                 font.label = list(size = 18, face = "bold"),
                 heights = c(1,1.3,0.8), 
                 nrow = 3) + 
         theme(plot.margin = margin(t = 20, r = 5, b = 5, l = 5)),
       width = 12, height = 12, dpi = 300)


# ggsave("./fig/f5.png", 
#        ggarrange(f51,
#                  f52, nrow = 2,
#                  common.legend = T,
#                  legend = "right",
#                  heights = c(1,1.2),
#                  labels = c("A Climate/electricity rate effect with HP adoption", 
#                             "B Climate/electricity rate effect with PV adoption"),  # Adds labels to plots
#                  label.x = 0,        # Adjust horizontal position of labels
#                  label.y = 1,        # Adjust vertical position of labels
#                  hjust = 0, 
#                  vjust = 0,
#                  font.label = list(size = 14, face = "bold")) + 
#          theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)),
#        width = 12, height = 12, dpi = 300)

ggsave("./fig/f5.png",
       ggarrange(f52,f51,
                 ggarrange(f54, f53, nrow = 1, 
                           common.legend = T,
                           legend = "none"),
                 nrow = 3, 
                 common.legend = T,
                 legend = "bottom",
                 labels = c("a  Marginal effect of electricity rates on PV households", 
                            "b  Marginal effect of climate on HP households",
                            "c  Aggregate marginal effect"),  
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 0.95,        # Adjust vertical position of labels
                 hjust = 0,
                 vjust = 0),
       width = 12, height = 13, dpi = 300)



# SI
# grouping
s0a <- cl %>%
  filter(climate != 8) %>%
  left_join(dat %>% 
              group_by(climate) %>% 
              summarise(count = n()),
            by = "climate") %>% 
  mutate(centroid = st_point_on_surface(geometry)) %>%   # safer than st_centroid
  ggplot() +
  geom_sf(fill = "white", color = "gray0") +
  geom_sf(aes(fill = climate), size = 0.3) +
  # geom_sf(aes(fill = .data[["climate]]), size = 0.3) +
  
  geom_sf_text(
    aes(label = count, geometry = centroid),
    size = 3,
    fontface = "bold",
    color = "gray20"
  ) +
  
  theme_minimal() +
  labs(title = "Sample size by climate zone", fill = "", x = "", y = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm'),
    strip.background = element_rect(fill = "gray22", color = "gray22"),
    strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 11),
    axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
    axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
    axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 11),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

US1 <- st_read("./data/US_population.shp")
s0b <- US1 %>%
  left_join(dat %>% 
              group_by(state) %>% 
              summarise(count = n()),
            by = "state") %>% 
  mutate(centroid = st_point_on_surface(geometry)) %>%   # safer than st_centroid
  ggplot() +
  geom_sf(fill = "gray98", color = "gray70", size = 0.3) +
  # geom_sf(aes(fill = .data[["climate]]), size = 0.3) +
  
  geom_sf_text(
    aes(label = count, geometry = centroid),
    size = 3,
    fontface = "bold",
    color = "gray20"
  ) +
  
  theme_minimal() +
  labs(title = "Sample size by state", fill = "", x = "", y = "") +
  coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
           ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
  theme(
    legend.position = "none",
    legend.text = element_text(size = 7),
    legend.key.size = unit(0.3, 'cm'),
    strip.background = element_rect(fill = "gray22", color = "gray22"),
    strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 11),
    axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
    axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
    axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 11),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold"),
    plot.margin = margin(10, 10, 10, 10)
  )

s0c <- sc_map %>% 
  left_join(dat %>% 
              group_by(group) %>% 
              summarise(count = n()),
            by = "group") %>% 
  mutate(centroid = st_point_on_surface(geometry)) %>%   # safer than st_centroid
  ggplot() +
  
  # Map polygons
  geom_sf(fill = "gray98", color = "gray70", size = 0.3) +
  
  # Division labels
  geom_sf_text(
    aes(label = count, geometry = centroid),
    size = 3,
    fontface = "bold",
    color = "gray10"
  ) +
  
  # Projection and limits
  coord_sf(
    crs = st_crs(2163),
    xlim = c(-2500000, 2500000),
    ylim = c(-2300000, 730000),
    expand = FALSE,
    datum = NA
  ) +
  
  # Minimal but polished theme
  theme_minimal(base_size = 11) +
  labs(title = "Sample size by regional group", fill = "") +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold")
  )


s0d <- sc_map %>% 
  mutate(centroid = st_point_on_surface(geometry)) %>%   # safer than st_centroid
  ggplot() +
  
  # Map polygons
  geom_sf(fill = "gray98", color = "gray70", size = 0.3) +
  
  # Division labels
  geom_sf_text(
    aes(label = group, geometry = centroid),
    size = 3,
    fontface = "bold",
    color = "gray10"
  ) +
  
  # Projection and limits
  coord_sf(
    crs = st_crs(2163),
    xlim = c(-2500000, 2500000),
    ylim = c(-2300000, 730000),
    expand = FALSE,
    datum = NA
  ) +
  
  # Minimal but polished theme
  theme_minimal(base_size = 11) +
  labs(title = "Regional group name", fill = "") +
  theme(
    panel.grid.major = element_line(color = "gray90", size = 0.2),
    panel.grid.minor = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    legend.position = "none",
    plot.margin = margin(10, 10, 10, 10),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold")
  )

ggsave("./fig/s0.png", 
       ggarrange(s0a, s0b, s0c, s0d, nrow = 2, ncol = 2), width = 12, height = 8)


tables2 <- test %>% 
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm)) %>% 
  mutate(model = ifelse(model == "fit", "Random slope", "Simpler"),
         across(where(is.numeric) & !p.value, ~ round(.x, 0)),
         p.value = ifelse(!is.na(p.value) & p.value < 1e-5,
                          "<0.00001",
                          as.character(round(p.value, 5)))
  )
write_csv(tables2, "./data/derived/ts2.csv")


# SI 
s1 <- dat %>% 
  filter(adopter %in% c("PV","EV","HP","Non")) %>% 
  filter(climate != 8) %>% 
  # remove confounders (PV, EV adopters) to energy expenditures
  
  gather(IV,value,KWH,BTUNG,TOTALBTU,DOLLAREL,DOLLARNG,TOTALDOL) %>% 
  # mutate(key = factor(key, levels = c("KWH","BTU","TOTALBTU","DOLLAREL","BTUDOL","TOTALDOL"))) %>% 
  mutate(value = value) %>% 
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm),
         adopter = factor(adopter, levels = c("Non","PV","EV","HP"))) %>% 
  
  ggplot()+
  geom_boxplot(aes(x = adopter, y = value, fill = adopter), outlier.shape = NA) + 
  # geom_point(data = tp, aes(x = adopter, y = mean/ 1000), color = "red") + 
  # 
  labs(title = "", x = "", y = "", fill = "") +
  
  facet_grid(IV ~ climate, scale = "free", switch = "y") +
  # ylim(c(-1,quantile(dat[[which(names(dat) == var)]]/1000, 0.99))) +
  
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/s1.png", 
       s1, width = 13, height = 9)


s2 <- dat_rass %>% 
  filter(adopter %in% c("PV","EV","HP","Non")) %>% 
  
  gather(key,value,Elec,Gas,Energy) %>% 
  mutate(key = factor(key, levels = c("Elec","Gas","Energy"))) %>% 
  mutate(key = recode(key, !!!name_out_r),
         key = factor(key, levels = otcm_r),
         adopter = factor(adopter, levels = c("Non","PV","EV","HP"))) %>% 
  
  ggplot()+
  geom_boxplot(aes(x = adopter, y = value, fill = adopter), outlier.shape = NA) + 
  # geom_point(data = tp, aes(x = adopter, y = mean/ 1000), color = "red") + 
  # 
  labs(title = "", x = "", y = "", fill = "") +
  
  facet_grid(key ~ as.numeric(Climate), scale = "free", switch = "y") +
  # ylim(c(-1,quantile(dat[[which(names(dat) == var)]]/1000, 0.99))) +
  
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/s2.png", 
       s2, width = 13, height = 6)

s3a <- dat_rass %>% 
  filter(adopter %in% c("PV","EV","HP","Non")) %>% 
  
  gather(key,value,Elec,Gas,Energy) %>% 
  mutate(key = factor(key, levels = c("Elec","Gas","Energy"))) %>% 
  mutate(key = recode(key, !!!name_out_r),
         key = factor(key, levels = otcm_r),
         adopter = factor(adopter, levels = c("Non","PV","EV","HP"))) %>% 
  dplyr::select(adopter, value, key) %>% 
  mutate(climate = "RASS (CA)") %>% 
  rbind(
    dat_rass %>% 
      filter(adopter %in% c("PV","EV","HP","Non")) %>% 
      filter(Climate %in% c(1,2,6,8)) %>% 
      
      gather(key,value,Elec,Gas,Energy) %>% 
      mutate(key = factor(key, levels = c("Elec","Gas","Energy"))) %>% 
      mutate(key = recode(key, !!!name_out_r),
             key = factor(key, levels = otcm_r),
             adopter = factor(adopter, levels = c("Non","PV","EV","HP"))) %>% 
      dplyr::select(adopter, value, key) %>% 
      mutate(climate = "RASS (3C)"),
    
    f5_data %>% 
      dplyr::rename(key = IV)
  ) %>% 
  mutate(climate = factor(climate, levels = c("RECS","RECS (CA)","RASS (CA)","RECS (3C)","RASS (3C)"))) %>% 
  
  ggplot()+
  geom_boxplot(aes(x = adopter, y = value, fill = adopter)) +
  
  labs(title = "", x = "", y = "", fill = "Adopter type") +
  
  facet_grid(key ~ climate, scale = "free", switch = "y") +
  
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


s3b <- dat_rass %>% 
  # Label PV, EV, HP adopters
  mutate(PV = ifelse(PV == 1, "PV", ""),
         EV = ifelse(EV == 1, "EV", ""),
         HP = ifelse(HP == 1, "HP", "")) %>% 
  
  # Create interaction term without dots and replace empty with "none"
  mutate(interaction_term = as.character(interaction(PV, EV, HP, sep = "")),
         interaction_term = ifelse(interaction_term == "", "None", interaction_term)) %>%
  
  mutate(class = "RASS (CA)") %>% 
  dplyr::select(Elec,interaction_term,class) %>% 
  rbind(f5b_data %>% 
          dplyr::rename(Elec = KWH)) %>% 
  
  mutate(interaction_term = factor(interaction_term,
                                   levels = c("None","PV","EV","PVEV","HP","PVHP","EVHP","PVEVHP"))) %>%
  mutate(class = factor(class, levels = c("RECS","RECS (CA)","RASS (CA)"))) %>% 
  
  ggplot() +
  geom_boxplot(aes(x = interaction_term, y = Elec, fill = interaction_term)) + # outlier.shape = NA
  labs(title = "", x = "Adopter type", y = "Electricity consumption (MWh)", fill = "") +
  facet_wrap(~class, scale = "free", nrow = 3) +
  scale_fill_viridis_d() +
  # coord_flip() +
  theme_classic() +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 


s3 <- ggarrange(s3a,s3b, nrow = 1, widths = c(1,0.5),
                labels = c("a", "b"),  # Adds labels to plots
                label.x = 0,        # Adjust horizontal position of labels
                label.y = 1,        # Adjust vertical position of labels
                font.label = list(size = 18, face = "bold"))


ggsave("./fig/s3.png", 
       s3, width = 12, height = 10)


ggsave("./fig/s4.png", 
       f2a_data %>% 
         # filter(domain != "Interaction") %>% 
         filter(!str_detect(var, ":temp$|:rate|moist")) %>% 
         filter(!IV %in% otcm[c(1)]) %>%  
         reg_plot("var", "domain"), width = 12, height = 9, dpi = 300)

ggsave("./fig/S4_1.png", 
       s41,
       width = 12, height = 10)

s_r <- dg_re %>% 
  # mutate(region = ifelse(state %in% northeast, "Northeast",
  #                        ifelse(state %in% midwest, "Midwest",
  #                               ifelse(state %in% west, "West",
  #                                      ifelse(state %in% south, "South",
  #                                             ifelse(state %in% texas, "Texas", "Mtwest"))))),
  #        region = factor(region, levels = c("Northeast","Midwest","Mtwest","West","Texas","South"))) %>% 
  
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm)) %>% 
  
  # mutate(state = factor(state, levels = ds_re$state %>% unique())) %>% 
  filter(IV != "Grid\n(MWh/HH)") %>% 
  
  ggplot(aes(x = R_effect, y = reorder(group, R_effect), xmin=lower, xmax=upper)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5) +
  
  facet_wrap( ~IV, nrow = 1, scale = "free") + 
  theme_bw() +
  
  labs(x = "", y ="",
       title = "Random effects") +
  
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=7),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=14)) 


ggsave("./fig/s5.png", 
       s_r, width = 12, height = 8)



ggsave("./fig/s6.png", 
       f2b_data %>% 
         reg_plot("var", "domain"), width = 12, height = 9, dpi = 300)

# # synergy
# syn <- f2a_data %>%
#   filter(IV %in% otcm[c(1)], var %in% c("PV","EV","PV:EV")) %>%
#   mutate(var = factor(var, levels = c("PV","EV","PV:EV"))) %>%
#   dplyr::select(-domain, -color)
# syn1 <- f2a_data %>%
#   filter(IV %in% otcm[c(7)], var %in% c("PV","EV","PV:EV")) %>%
#   mutate(var = factor(var, levels = c("PV","EV","PV:EV"))) %>%
#   dplyr::select(-domain, -color)

c_r <- dc_r %>% 
  
  mutate(IV = recode(IV, !!!name_out_r),
         IV = factor(IV, levels = otcm_r)) %>% 
  
  # mutate(Climate = factor(Climate, levels = seq(1:17))) %>% 
  
  ggplot(aes(x = R_effect, y = reorder(Climate, R_effect), xmin=lower, xmax=upper)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5) +
  
  facet_wrap(~IV, nrow = 1, scale = "free") + 
  theme_bw() +
  
  labs(x = "", y ="", fill = "Significant",
       title = "Climate random effects") +
  
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
        legend.position = "bottom",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
        plot.title=element_text(family="Franklin Gothic Demi", size=20)) 

ggsave("./fig/s7.png", 
       c_r, width = 12, height = 6, dpi = 300)


# s8 <- cowplot::plot_grid(plotlist = dg_re %>% 
#                            mutate(IV = recode(IV, !!!name_out),
#                                   IV = factor(IV, levels = otcm)) %>% 
#                            dplyr::rename(value = R_effect) %>% 
#                             mapping_r(otcm[-1], color = T), labels = otcm1[-1], label_size = 10, nrow = 2,
#                           label_x = .2, hjust = 0)
# 
# ggsave(paste0("./fig/", "s8", ".png"),
#        s8, width = 12, height = 5)


ggsave("./fig/s81.png", 
       ggarrange(f3a,f3b,f3b_c,f3b_t, nrow = 4,
                 labels = c("a Non-adoption Baseline", 
                            "b Solar PV Regional", 
                            "c Solar PV Regional + Average", 
                            "d Solar PV Final (Non-adoption + Regional + Average)"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 1.5,
                 font.label = list(size = 14, face = "bold"),
                 heights = c(1, 1, 1, 1) * 0.85)+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 12)

ggsave("./fig/s82.png", 
       ggarrange(f3a,f3c,f3c_c,f3c_t, nrow = 4,
                 labels = c("a Non-adoption Baseline", 
                            "b Electric Vehicle Regional", 
                            "c Electric Vehicle Regional + Average", 
                            "d Electric Vehicle Final (Non-adoption + Regional + Average)"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 1.5,
                 font.label = list(size = 14, face = "bold"),
                 heights = c(1, 1, 1, 1) * 0.85)+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 12)


ggsave("./fig/s83.png", 
       ggarrange(f3a,f3d,f3d_c,f3d_t, nrow = 4,
                 labels = c("a Non-adoption Baseline", 
                            "b Heat Pump Regional", 
                            "c Heat Pump Regional + Average", 
                            "d Heat Pump Final (Non-adoption + Regional + Average)"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 1.5,
                 font.label = list(size = 14, face = "bold"),
                 heights = c(1, 1, 1, 1) * 0.85)+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 12)



ggsave("./fig/s8.png", 
       ggarrange(f3a,f3b_t,f3c_t,f3d_t, nrow = 4,
                 labels = c("a Non-adoption", "b Solar PV", "c Electric Vehicle", "d Heat Pump"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 1.5,
                 font.label = list(size = 14, face = "bold"),
                 heights = c(1, 1, 1, 1) * 0.85)+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 12)


ggsave("./fig/s9.png", 
       ggarrange(f3b,f3c,f3d, nrow = 3,
                 labels = c("a Solar PV", "b Electric Vehicle", "c Heat Pump"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 1.5,
                 font.label = list(size = 14, face = "bold"))+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 10)


ggsave("./fig/s10.png", 
       ggarrange(f3b_c,f3c_c,f3d_c, nrow = 3,
                 labels = c("a Solar PV", "b Electric Vehicle", "c Heat Pump"),  # Adds labels to plots
                 label.x = 0,        # Adjust horizontal position of labels
                 label.y = 1,        # Adjust vertical position of labels
                 hjust = 0, vjust = 1.5,
                 font.label = list(size = 14, face = "bold"))+
         theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)), width = 12, height = 10)


# s9 <- cowplot::plot_grid(plotlist = dg_rs %>% 
#                            mutate(IV = recode(IV, !!!name_out),
#                                   IV = factor(IV, levels = otcm)) %>% 
#                            dplyr::rename(value = R_effect) %>% 
#                             mapping(otcm[-1], T, tech = "PV"), labels = otcm1[-1], label_size = 10, 
#                           nrow = 2,
#                           label_x = .2, hjust = 0)
# 
# ggsave(paste0("./fig/", "s9", ".png"),
#        s9, width = 12, height = 5)
# 
# s10 <- cowplot::plot_grid(plotlist = r_combine(dc_rs, ds_rs, F) %>% 
#                             mapping(otcm[-1], T, tech = "EV"), labels = otcm1[-1], label_size = 10, 
#                           nrow = 2,
#                           label_x = .2, hjust = 0)
# 
# ggsave(paste0("./fig/", "s10", ".png"),
#        s10, width = 12, height = 5)
# 
# s11 <- cowplot::plot_grid(plotlist = r_combine(dc_rs, ds_rs, F) %>% 
#                             mapping(otcm[-1], T, tech = "HP"), labels = otcm1[-1], label_size = 10, 
#                           nrow = 2,
#                           label_x = .2, hjust = 0)
# 
# ggsave(paste0("./fig/", "s11", ".png"),
#        s11, width = 12, height = 5)



s11 <- dg_rs %>% 
  
  # mutate(region = ifelse(state %in% northeast, "Northeast",
  #                        ifelse(state %in% midwest, "Midwest",
  #                               ifelse(state %in% west, "West",
  #                                      ifelse(state %in% south, "South",
  #                                             ifelse(state %in% texas, "Texas", "Mtwest"))))),
  #        region = factor(region, levels = c("Northeast","Midwest","Mtwest","West","Texas","South"))) %>% 
  
  mutate(tech = factor(tech, levels = c("PV","EV","HP"))) %>% 
  
  mutate(IV = recode(IV, !!!name_out),
         IV = factor(IV, levels = otcm)) %>% 
  
  # mutate(state = factor(state, levels = ds_rs$state %>% unique())) %>% 
  filter(IV != "Grid\n(MWh/HH)") %>% 
  
  ggplot(aes(x = R_effect, y = reorder(group, R_effect), xmin=R_effect-SE, xmax=R_effect+SE)) +
  geom_vline(xintercept = 0,linetype = "dashed", size = 0.5, color = "gray30") +
  
  geom_pointrangeh(position = position_dodge2v(height = 0.4), fatten = 0.5, size = 0.1) +
  
  facet_grid(tech~IV, scale = "free", switch = "y") + 
  theme_bw() +
  
  labs(x = "", y ="", color = "",
       title = "Technology random effects") +
  
  scale_fill_manual(values=c("red", "gray")) +
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background =element_rect(fill="gray22",color="gray22"),
        strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=9),
        legend.position = "none",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=6),
        axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.title.y = element_text(color = "black",family="Franklin Gothic Book",size=6),
        plot.title=element_text(family="Franklin Gothic Demi", size=16)) 


ggsave(paste0("./fig/", "s11", ".png"),
       s11, width = 12, height = 14)


s12 <- cowplot::plot_grid(plotlist = rate_data %>%
                            mapping(c("E_rate","G_rate","rate")), labels = c("Electricity rate ($/MWh)","Fuel rate ($/MMBtu)","Energy rate ($/MMBtu)"),
                          label_size = 10, nrow = 1,
                          label_x = .2, hjust = 0)



ggsave("./fig/s12.png", 
       s12, width = 12, height = 4)


a <- rate_plot(reffect, "Non-adoption", otcm[-1], T)
b <- rate_plot(reffect, "PV", otcm[-1])
c <- rate_plot(reffect, "EV", otcm[-1])
d <- rate_plot(reffect, "HP", otcm[-1])

ggsave("./fig/s14.png", 
       ggarrange(a,b,c,d, nrow = 4), width = 12, height = 10)


a <- climate_plot(reffect, "Non-adoption", otcm[-1], T)
b <- climate_plot(reffect, "PV", otcm[-1])
c <- climate_plot(reffect, "EV", otcm[-1])
d <- climate_plot(reffect, "HP", otcm[-1])

s15 <- ggarrange(a,b,c,d, nrow = 1, common.legend = TRUE, legend="bottom") %>% 
  annotate_figure(
    bottom = text_grob("Climate zone", size = 14)
  )

ggsave("./fig/s15.png", 
       s15, width = 12, height = 9, dpi = 300)



# ### 
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
# ba <- dat %>%
#   dplyr::select(WH,KWH:TOTALDOL) %>% 
#   gather(IV, value) %>% 
#   mutate(IV = recode(IV, !!!name_out),
#          IV = factor(IV, levels = otcm)) %>% 
#   group_by(IV) %>% 
#   summarise(base = mean(value, na.rm = T)) %>% 
#   mutate(base = ifelse(str_detect(IV, " Elec"), base*3.412, base)) %>%  # MWh to MMBtu) 
#   dplyr::rename(effect = IV) 
# 
# 
# base <- ba %>% 
#   mutate(tech = "Non-adoption") %>% 
#   rbind(
#     f2a_data %>% 
#       filter(var %in% c("PV","EV","HP")) %>% 
#       dplyr::select(var, pe, IV) %>% 
#       dplyr::rename(tech = var, base = pe, effect = IV) %>% 
#       mutate(base = ifelse(str_detect(effect, " Elec"), base*3.412, base)) %>% 
#       left_join(ba, by = "effect") %>% 
#       mutate(base = base.x + base.y) %>% 
#       dplyr::select(-base.x,-base.y)
#   ) %>% 
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   filter(effect %in% otcm[c(1,2,3,4)]) %>% 
#   mutate(effect = as.character(effect),
#          effect = ifelse(str_detect(effect, " Elec"),
#                          str_replace(effect, "MWh", "MMBtu"),
#                          effect),
#          effect = str_replace(effect, "\\n\\(MMBtu/HH\\)", ""),
#          effect = str_replace(effect, "\\n\\(\\k$/HH\\)", ""),
#          effect = factor(effect, levels = c("Grid Elec", "Total Elec","Fossil Fuel","Total Energy")))
# 
# baselines <- data %>%
#   filter(tech == "Non-adoption") %>%
#   dplyr::select(var, effect, baseline_estimate = Estimate)
# 
# plot_data <- data %>%
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   left_join(baselines, by = c("var", "effect")) %>%
#   filter(!(effect == "Grid Elec" & tech != "PV")) %>% 
#   left_join(base, by = c("effect","tech")) %>% 
#   mutate(
#     after_base = base + baseline_estimate,
#     total_val = ifelse(tech == "Non-adoption", after_base, Estimate + after_base),
#     b_color = ifelse(after_base > base, 1, 0)
#   )
# 
# 
# non_adopt_data <- plot_data %>% filter(tech == "Non-adoption")  
# adopt_data <- plot_data %>% filter(tech != "Non-adoption") 

# ggplot() +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
#   
#   # Layer 1: Standalone Non-adoption reference (Far Left)
#   # geom_col(data = non_adopt_data, 
#   #          aes(x = tech, y = base), 
#   #          fill = "gray80", color = "gray80", width = 0.7) +
#   # 
#   # # Layer 2: Baseline "shadow" bars for PV, EV, HP
#   # geom_col(data = adopt_data, 
#   #          aes(x = tech, y = base), 
#   #          fill = "gray80", color = "gray80", width = 0.7) +
#   
#   # Layer 3: Marginal Adoption Effects (Stacked on the baseline shadows)
#   # We use geom_rect for precise positioning on top of the gray bars
#   geom_rect(data = plot_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = base,
#                 ymax = after_base),
#             fill = "gray70", linewidth = 0.3) +
#   
#   
#   geom_rect(data = adopt_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = after_base,
#                 ymax = total_val,
#                 fill = factor(color)),
#             color = NA, alpha = 0.7, linewidth = 0.3) +
#   
#   
#   # Layer 4: Aligned Error Bars (Centered on the top of each stack)
#   geom_errorbar(data = plot_data,
#                 aes(x = tech, ymin = total_val - SE, ymax = total_val + SE),
#                 width = 0.2, color = "black") +
#   
#   # scale_x_continuous(breaks = 1:length(tech_labels), labels = tech_labels) +
#   
#   # Faceting and Theme
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   
#   scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), 
#                      labels = c("Decrease", "Increase"), name = "Climate/rate effect") +
#   
#   scale_fill_manual(values = c("-1" = "#0072B2", "1" = "#D55E00"), 
#                     labels = c("Decrease", "Increase"), name = "Tech response") +
#   labs(y = "Estimate (MMBtu/HH)", x = NULL,
#        title = "Climate and Rate Effect on Energy Consumption") +
#   
#   coord_cartesian(ylim = c(20, 110)) +
#   theme_classic() +
#   theme(
#     # strip.placement = "outside",
#     panel.grid.major.y = element_line(),
#     # strip.text = element_text(face = "bold"),
#     # legend.position = "bottom",
#     
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 9),
#     strip.text = element_text(face = "bold", size = 12),
#     strip.placement = "outside",
#     strip.text.y.left = element_text(angle = 0, hjust = 0.5),
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.36)
#   )

# 
# 
# f6a <- ggplot() +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
#   
#   
#   # We use geom_rect for precise positioning on top of the gray bars
#   geom_rect(data = plot_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = base,
#                 ymax = after_base,
#                 fill = factor(b_color)),
#             color = NA, linewidth = 0.3) +
#   
#   scale_fill_manual(
#     values = c("0" = "#0072B2", "1" = "#D55E00"),
#     labels = c("Decrease", "Increase"),
#     name = "Climate/Rate Effect",
#     guide = guide_legend(order = 3)
#   ) +
#   
#   new_scale_fill() + 
#   
#   geom_rect(data = adopt_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = after_base,
#                 ymax = total_val,
#                 fill = "adoption"),
#             color = NA, alpha = 0.5, linewidth = 0.3) +
#   
#   scale_fill_manual(
#     values = c("adoption" = "gold"),
#     labels = "Adoption specific",
#     name = "Climate/Rate Effect",
#     guide = guide_legend(order = 2)
#   ) +
#   
#   geom_segment(data = plot_data,
#                aes(x = as.numeric(tech) - 0.45,
#                    xend = as.numeric(tech) + 0.45,
#                    y = base,
#                    yend = base,
#                    color = "base"), linewidth = 1) +
#   
#   scale_color_manual(
#     values = c("base" = "black"),
#     labels = "By adoption status",
#     name = "Baseline",
#     guide = guide_legend(order = 4)
#   ) +
#   
#   new_scale_color() + 
#   
#   
#   # # Layer 4: Aligned Error Bars (Centered on the top of each stack)
#   geom_errorbar(data = plot_data,
#                 aes(x = as.numeric(tech), ymin = total_val - SE, ymax = total_val + SE, color = "error"),
#                 width = 0.2) +
#   
#   scale_color_manual(
#     values = c("error" = "black"),
#     labels = "Final outcome",
#     name = "Error Bar",
#     guide = guide_legend(order = 1)
#   ) +
#   
#   scale_x_continuous(
#     breaks = 1:length(levels(plot_data$tech)),
#     labels = levels(plot_data$tech)
#   ) + 
#   
#   
#   # Faceting and Theme
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   
#   labs(y = "Estimate (MMBtu/HH)", x = NULL,
#        title = "Climate and Rate Effect on Energy Consumption") +
#   
#   coord_cartesian(ylim = c(20, 110)) +
#   theme_classic() +
#   theme(
#     # strip.placement = "outside",
#     panel.grid.major.y = element_line(color = "gray70"),
#     # panel.grid.minor.y = element_line(color = "gray70"),
#     
#     # strip.text = element_text(face = "bold"),
#     # legend.position = "bottom",
#     
#     axis.text.y = element_text(size = 12),
#     axis.text.x = element_text(size = 9),
#     strip.text = element_text(face = "bold", size = 12),
#     strip.placement = "outside",
#     strip.text.y.left = element_text(angle = 0, hjust = 0.5),
#     panel.spacing = unit(1, "lines"),
#     plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.34)
#   ) 
#   # guides(
#   #   color = guide_legend(order = 1),  # National Average
#   #   fill  = guide_legend(order = 3),   # Climate/Rate Effect
#   #   fill_new = guide_legend(order = 2),   # Adoption Effect
#   #   color_new = guide_legend(order = 4)  # Error Bar
#   # )
# 
# 
# 
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
# base <- ba %>% 
#   mutate(tech = "Non-adoption") %>% 
#   rbind(
#     f2a_data %>% 
#       filter(var %in% c("PV","EV","HP")) %>% 
#       dplyr::select(var, pe, IV) %>% 
#       dplyr::rename(tech = var, base = pe, effect = IV) %>% 
#       mutate(base = ifelse(str_detect(effect, " Elec"), base*3.412, base)) %>% 
#       left_join(ba, by = "effect") %>% 
#       mutate(base = base.x + base.y) %>% 
#       dplyr::select(-base.x,-base.y)
#   ) %>% 
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   filter(effect %in% otcm[c(5:7)]) %>% 
#   mutate(effect = as.character(effect),
#          effect = ifelse(str_detect(effect, " Elec"),
#                          str_replace(effect, "MWh", "MMBtu"),
#                          effect),
#          effect = str_replace(effect, "\\n\\(MMBtu/HH\\)", ""),
#          effect = str_replace(effect, "\\n\\(\\k$/HH\\)", ""),
#          effect = factor(effect, levels = c("Elec Bill","Fuel Bill","Total Bill")))
# 
# baselines <- data %>%
#   filter(tech == "Non-adoption") %>%
#   dplyr::select(var, effect, baseline_estimate = Estimate)
# 
# plot_data <- data %>%
#   mutate(tech = factor(tech, levels = tech_levels)) %>%
#   left_join(baselines, by = c("var", "effect")) %>%
#   filter(!(effect == "Grid Elec" & tech != "PV")) %>% 
#   left_join(base, by = c("effect","tech")) %>% 
#   mutate(
#     after_base = base + baseline_estimate,
#     total_val = ifelse(tech == "Non-adoption", after_base, Estimate + after_base),
#     b_color = ifelse(after_base > base, 1, 0)
#   )
# 
# 
# non_adopt_data <- plot_data %>% filter(tech == "Non-adoption")  
# adopt_data <- plot_data %>% filter(tech != "Non-adoption") 
# 

# ggplot() +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
#   
#   # Layer 1: Standalone Non-adoption reference (Far Left)
#   geom_col(data = non_adopt_data, 
#            aes(x = tech, y = base), 
#            fill = "gray80", color = "gray80", width = 0.7) +
#   
#   # Layer 2: Baseline "shadow" bars for PV, EV, HP
#   geom_col(data = adopt_data, 
#            aes(x = tech, y = base), 
#            fill = "gray80", color = "gray80", width = 0.7) +
#   
#   # Layer 3: Marginal Adoption Effects (Stacked on the baseline shadows)
#   # We use geom_rect for precise positioning on top of the gray bars
#   geom_rect(data = plot_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = base,
#                 ymax = after_base,
#                 color = factor(b_color)),
#             fill = "gold", alpha = 0.2, linewidth = 0.3) +
#   
#   
#   geom_rect(data = adopt_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = after_base,
#                 ymax = total_val,
#                 fill = factor(color)),
#             color = NA, linewidth = 0.3) +
#   
#   
#   # Layer 4: Aligned Error Bars (Centered on the top of each stack)
#   geom_errorbar(data = plot_data,
#                 aes(x = tech, ymin = total_val - SE, ymax = total_val + SE),
#                 width = 0.2, color = "black") +
#   
#   # Faceting and Theme
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   
#   scale_color_manual(values = c("0" = "#0072B2", "1" = "#D55E00"), 
#                      labels = c("Decrease", "Increase"), name = "Climate/rate effect") +
#   
#   scale_fill_manual(values = c("-1" = "#0072B2", "1" = "#D55E00"), 
#                     labels = c("Decrease", "Increase"), name = "Tech response") +
#   labs(y = "Estimate (k$/HH)", x = NULL,
#        title = "Climate and Rate Effect on Energy Bill") +
#   coord_cartesian(ylim = c(0.35, 2.3)) +
#   theme_classic() +
#   theme(
#     # strip.placement = "outside",
#     panel.grid.major.y = element_line(),
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
#   )
# 
# f6b <- ggplot() +
#   # Zero reference line
#   geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
#   
#   
#   # We use geom_rect for precise positioning on top of the gray bars
#   geom_rect(data = plot_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = base,
#                 ymax = after_base,
#                 fill = factor(b_color)),
#             color = NA, linewidth = 0.3) +
#   
#   scale_fill_manual(
#     values = c("0" = "#0072B2", "1" = "#D55E00"),
#     labels = c("Decrease", "Increase"),
#     name = "Climate/Rate Effect",
#     guide = guide_legend(order = 3)
#   ) +
#   
#   new_scale_fill() + 
#   
#   geom_rect(data = adopt_data,
#             aes(xmin = as.numeric(tech) - 0.35,
#                 xmax = as.numeric(tech) + 0.35,
#                 ymin = after_base,
#                 ymax = total_val,
#                 fill = "adoption"),
#             color = NA, alpha = 0.5, linewidth = 0.3) +
#   
#   scale_fill_manual(
#     values = c("adoption" = "gold"),
#     labels = "Adoption specific",
#     name = "Climate/Rate Effect",
#     guide = guide_legend(order = 2)
#   ) +
#   
#   geom_segment(data = plot_data,
#                aes(x = as.numeric(tech) - 0.45,
#                    xend = as.numeric(tech) + 0.45,
#                    y = base,
#                    yend = base,
#                    color = "base"), linewidth = 1) +
#   
#   scale_color_manual(
#     values = c("base" = "black"),
#     labels = "By adoption status",
#     name = "Baseline",
#     guide = guide_legend(order = 4)
#   ) +
#   
#   new_scale_color() + 
#   
#   
#   # # Layer 4: Aligned Error Bars (Centered on the top of each stack)
#   geom_errorbar(data = plot_data,
#                 aes(x = as.numeric(tech), ymin = total_val - SE, ymax = total_val + SE, color = "error"),
#                 width = 0.2) +
#   
#   scale_color_manual(
#     values = c("error" = "black"),
#     labels = "Final outcome",
#     name = "Error Bar",
#     guide = guide_legend(order = 1)
#   ) +
#   
#   scale_x_continuous(
#     breaks = 1:length(levels(plot_data$tech)),
#     labels = levels(plot_data$tech)
#   ) + 
#   
#   
#   # Faceting and Theme
#   facet_grid(var ~ effect, scales = "free_y", switch = "y") +
#   
#   labs(y = "Estimate (k$/HH)", x = NULL,
#        title = "Climate and Rate Effect on Energy Bill") +
#   
#   coord_cartesian(ylim = c(0.35, 2.3)) +
#   theme_classic() +
#   theme(
#     # strip.placement = "outside",
#     panel.grid.major.y = element_line(color = "gray70"),
#     # panel.grid.minor.y = element_line(color = "gray70"),
#     
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
#   ) 
#   # guides(
#   #   color = guide_legend(order = 1),  # National Average
#   #   fill  = guide_legend(order = 2),   # Climate/Rate Effect
#   #   fill_new = guide_legend(order = 3),   # Adoption Effect
#   #   color_new = guide_legend(order = 4)  # Error Bar
#   # )
# 
# 
# ggsave("./fig/s16.png", 
#        ggarrange(f6a,
#                  f6b, nrow = 2,
#                  common.legend = T,
#                  legend = "right",
#                  heights = c(1,1),
#                  labels = c("A", "B"),  # Adds labels to plots
#                  label.x = 0,        # Adjust horizontal position of labels
#                  label.y = 1,        # Adjust vertical position of labels
#                  hjust = 0, 
#                  # vjust = -0.05,
#                  font.label = list(size = 14, face = "bold")) + 
#          theme(plot.margin = margin(t = 10, r = 5, b = 5, l = 5)),
#        width = 12, height = 12, dpi = 300)
# 
# 
# reffect1 <- create_fuel_rate_data(
#   re_data,
#   rate_data,
#   otcm,
#   tech_name = "Non-adoption"
# ) %>% 
#   rbind(
#     create_fuel_rate_data(
#       rs_data,
#       rate_data,
#       otcm
#     ) %>%
#       mutate(tech = factor(tech, levels = c("PV", "EV", "HP")))
#   ) %>% 
#   dplyr::rename(IV = IV.x)

# a <- rate_fuel_plot(reffect1, "Non-adoption", otcm[c(3,4,6,7)], T)
# b <- rate_fuel_plot(reffect1, "PV", otcm[c(3,4,6,7)])
# c <- rate_fuel_plot(reffect1, "EV", otcm[c(3,4,6,7)])
# d <- rate_fuel_plot(reffect1, "HP", otcm[c(3,4,6,7)])
# 
# ggsave("./fig/s16.png", 
#        ggarrange(a,b,c,d, nrow = 4), width = 12, height = 10)


# a <- re_plot(d_RS, "rate", "Non-adoption", otcm, T)
# b <- re_plot(d_RS, "rate", "PV", otcm, T)
# c <- re_plot(d_RS, "rate", "EV", otcm, T)
# d <- re_plot(d_RS, "rate", "HP", otcm, T)
# 
# ggsave("./fig/s17.png", 
#        ggarrange(a,b,c,d, nrow = 1, common.legend = TRUE, legend="bottom"), width = 12, height = 10)
# 
# 
# a <- re_plot(d_RS, "temp", "Regional Effect", otcm)
# b <- re_plot(d_RS, "temp", "PV", otcm)
# c <- re_plot(d_RS, "temp", "EV", otcm)
# d <- re_plot(d_RS, "temp", "HP", otcm)
# 
# ggsave("./fig/s18.png", 
#        ggarrange(a,b,c,d, nrow = 1, common.legend = TRUE, legend="bottom"), width = 12, height = 10)


# s19 <- syn %>%
#   rbind(data.frame(var = "Combined",
#                    pe = sum(syn$pe),
#                    lwr = sum(syn$lwr),
#                    upr = sum(syn$upr),
#                    IV = otcm[1])) %>%
#   rbind(
#     syn1 %>%
#       rbind(data.frame(var = "Combined",
#                        pe = sum(syn1$pe),
#                        lwr = sum(syn1$lwr),
#                        upr = sum(syn1$upr),
#                        IV = otcm[7]))
#   ) %>%
#   mutate(IV = recode(IV, 
#                      "Grid Elec\n(MWh/HH)" = "Grid Elec (MWh/HH)",
#                      "Total Bill\n(k$/HH)" = "Total Bill (k$/HH)")) %>% 
#   ggplot(aes(x = var, y = pe)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_col(fill = "skyblue") +
#   geom_point(size = 2) +
#   geom_errorbar(aes(ymin = upr, ymax = lwr), width = 0.3, color = "gray20") +
#   facet_wrap(~IV, scale = "free") +
#   labs(x = "", y = "", color = "Zone", title = "PV & EV Coadoption Impact") +
#   theme_bw() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=11, face = "bold"),
#         legend.position = "bottom",
#         axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
#         plot.title=element_text(family="Franklin Gothic Demi", size=14, face = "bold"))
# 
# ggsave("./fig/s19.png", s19, width = 12, height = 6, dpi = 300)
# 
# 
# 
# a_data <- sc_map %>% 
#   left_join(r_combine(dc_rs, ds_rs, F), by = "group") %>%
#   filter(IV == "Total Elec\n(MWh)", tech == "PV") 
# 
# a <- a_data %>%
#   ggplot() +
#   geom_sf(fill = "white", color = "gray0") + # US border
#   geom_sf(aes(fill = value), size = 0.3) +
#   geom_sf(data = a_data %>% 
#             filter(state == "CA") %>% 
#             group_by(state) %>% 
#             summarise(geometry = st_union(geometry)), fill = NA, color = "red", linewidth = 1) +
#   theme_minimal() +
#   scale_fill_distiller(palette = "RdBu") +
#   labs(title = "PV Rebound Effects (MWh)", fill = "") +
#   coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
#            ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 6),
#     legend.key.size = unit(0.3, 'cm')
#   )
# 
# 
# b <- sc_map %>% 
#   left_join(r_combine(dc_rs, ds_rs, F) %>% 
#               filter(IV == "Total Elec\n(MWh)") %>% 
#               filter(tech == "EV") %>% 
#               distinct() %>% 
#               left_join(
#                 r_combine(dc_rs, ds_rs, F) %>% 
#                   filter(IV == "Total Elec\n(MWh)") %>% 
#                   filter(tech == "HP") %>% 
#                   distinct() %>% 
#                   dplyr::select(group, value),
#                 by = "group"
#               ) %>% 
#               mutate(value = value.y - value.x) %>% # HP - EV
#               dplyr::select(-value.x, -value.y), by = "group") %>% 
#   ggplot() +
#   geom_sf(fill = "white", color = "gray0") + # US border
#   geom_sf(aes(fill = value), size = 0.3) +
# 
#   theme_minimal() +
#   scale_fill_distiller(palette = "RdBu") +
#   labs(title = "HP vs. EV Electricity Use Difference (MWh)", fill = "") +
#   coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
#            ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 6),
#     legend.key.size = unit(0.3, 'cm')
#   )
# 
# 
# c_data <- sc_map %>% 
#   left_join(r_combine(dc_rs, ds_rs, F) %>% 
#               mutate(value = ifelse(tech == "HP" & IV == "Total Bill\n(k$)", value - 0.058, value)), by = "group") %>%
#   filter(IV == "Total Bill\n(k$)", tech == "HP") 
# 
# c <- c_data %>%
#   ggplot() +
#   geom_sf(fill = "white", color = "gray0") + # US border
#   geom_sf(aes(fill = value), size = 0.3) +
#   geom_sf(data = c_data %>% 
#             filter(state == "CO") %>% 
#             group_by(state) %>% 
#             summarise(geometry = st_union(geometry)), fill = NA, color = "red", linewidth = 1) +
#   theme_minimal() +
#   scale_fill_distiller(palette = "RdBu") +
#   labs(title = "HP Adopton Bill Change (k$)", fill = "") +
#   coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
#            ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 6),
#     legend.key.size = unit(0.3, 'cm')
#   )
# 
# d <- syn %>%
#   rbind(data.frame(var = "Total",
#                    pe = sum(syn$pe),
#                    lwr = sum(syn$lwr),
#                    upr = sum(syn$upr),
#                    IV = otcm[1])) %>%
#   rbind(
#     syn1 %>%
#       rbind(data.frame(var = "Total",
#                        pe = sum(syn1$pe),
#                        lwr = sum(syn1$lwr),
#                        upr = sum(syn1$upr),
#                        IV = otcm[7]))
#   ) %>%
#   mutate(IV = recode(IV, 
#                      "Grid Elec\n(MWh)" = "Grid Elec (MWh)",
#                      "Total Bill\n(k$)" = "Total Bill (k$)")) %>% 
#   ggplot(aes(x = var, y = pe)) +
#   geom_hline(yintercept = 0, linetype = "dashed") +
#   geom_col(fill = "gray80") +
#   geom_point() +
#   geom_errorbar(aes(ymin = upr, ymax = lwr), width = 0.3, color = "blue") +
#   facet_wrap(~IV, scale = "free") +
#   labs(x = "", y = "", color = "Zone", title = "PV & EV Co-adoption Impact") +
#   theme_bw() +
#   theme(panel.grid.minor = element_blank(),
#         panel.grid.major.x = element_blank(),
#         strip.background =element_rect(fill="gray22",color="gray22"),
#         strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=9),
#         legend.position = "bottom",
#         axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
#         axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
#         axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
#         plot.title=element_text(hjust = -0.35))
# 
# ggsave("./fig/main.png", ggarrange(a,b,c,d, heights = c(1,1,1,0.7), nrow = 4), width = 4, height = 8, dpi = 300)
# 
# 
# f <- cl %>%
#   filter(climate != 8) %>%
#   
#   ggplot() +
#   geom_sf(fill = "white", color = "gray0") +
#   geom_sf(aes(fill = climate), size = 0.3) +
#   theme_minimal() +
#   labs(title = "", fill = "") +
#   coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
#            ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
#   scale_fill_viridis_d(direction = -1, begin = 0.1, end = 1) +
#   labs(title = "Climate Zone") +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 7),
#     legend.key.size = unit(0.3, 'cm'),
#     strip.background = element_rect(fill = "gray22", color = "gray22"),
#     strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 11),
#     axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#     axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#     axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 11),
#     plot.title = element_text(hjust = -0.05)
#   )
#   
# 
# 
# g <- sc_map %>% 
#   left_join(rate_data, by = "group") %>%
#   filter(IV == "E_rate") %>%
#   ggplot() +
#   geom_sf(fill = "white", color = "gray0") + # US border
#   geom_sf(aes(fill = value), size = 0.3) +
#   theme_minimal() +
#   scale_fill_distiller(palette = "RdBu") + 
#   labs(title = "Electricity Rate ($/MWh)", fill = "") +
#   coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
#            ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
#   theme(
#     legend.position = "right",
#     legend.text = element_text(size = 6),
#     legend.key.size = unit(0.3, 'cm')
#   )
# 
# 
# ggsave("./fig/main1.png", f, width = 4, height = 2.1, dpi = 300)
# ggsave("./fig/main2.png", g, width = 4, height = 2.1, dpi = 300)



s16a <- f5a_data1 %>% 
  filter(feature == "Colder Climate") %>%
  mutate(IV = case_when(
    str_detect(IV, "Elec$") ~ "Electricity Use (MWh/HH)",
    str_detect(IV, "Energy$") ~ "Energy Use (MMBtu/HH)",
    str_detect(IV, "Elec Bill$") ~ "Electricity Bill (k$/HH)",
    str_detect(IV, "Bill$") ~ "Energy Bill (k$/HH)"),
    IV = factor(IV, levels = c("Electricity Use (MWh/HH)",
                               "Energy Use (MMBtu/HH)",
                               "Electricity Bill (k$/HH)",
                               "Energy Bill (k$/HH)"))) %>% 
  
  ggplot(aes(x = tech, y = pe, fill = fill_combined)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.15,
    linewidth = 0.6,
    color = "gold3"
  ) +
  facet_wrap(~ IV, scales = "free_y", nrow = 1) +
  labs(
    title = "Marginal effect of colder climate",
    
    x = "",
    y = ""
  ) +
  
  scale_fill_manual(
    values = c(
      "Y_sig"     = "#D55E00",  # significant increase (dark orange)
      "Y_nonsig"  = "#FDD4B3",  # non-significant increase (lighter orange)
      "N_sig"     = "#0072B2",  # significant decrease (dark blue)
      "N_nonsig"  = "#CFE6F3"   # non-significant decrease (lighter blue)
    ),
    labels = c(
      "Y_sig" = "Increase (sig.)",
      "Y_nonsig" = "Increase",
      "N_sig" = "Decrease (sig.)",
      "N_nonsig" = "Decrease"
    ),
    name = "Energy use/bill"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    # axis.text.x = element_blank(), # Hide x-labels as bars are colored
    axis.text.x = element_text(size = 12, hjust = 1), # Hide x-labels as bars are colored
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # strip.placement = "outside",
    # strip.text = element_text(face = "bold"),
    # panel.spacing.x = unit(3, "lines"),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    strip.placement = "outside",
    panel.spacing = unit(3, "lines"),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 14, face = "bold")
  )


s16b <- f5a_data1 %>% 
  filter(feature == "Higher Elec Rate") %>%
  mutate(IV = case_when(
    str_detect(IV, "Elec$") ~ "Electricity Use (MWh/HH)",
    str_detect(IV, "Energy$") ~ "Energy Use (MMBtu/HH)",
    str_detect(IV, "Elec Bill$") ~ "Electricity Bill (k$/HH)",
    str_detect(IV, "Bill$") ~ "Energy Bill (k$/HH)"),
    IV = factor(IV, levels = c("Electricity Use (MWh/HH)",
                               "Energy Use (MMBtu/HH)",
                               "Electricity Bill (k$/HH)",
                               "Energy Bill (k$/HH)"))) %>% 
  
  ggplot(aes(x = tech, y = pe, fill = fill_combined)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.15,
    linewidth = 0.6,
    color = "gold3"
  ) +
  facet_wrap(~ IV, scales = "free_y", nrow = 1) +
  labs(
    title = "Marginal effect of higher electricity rate",
    
    x = "",
    y = ""
  ) +
  
  scale_fill_manual(
    values = c(
      "Y_sig"     = "#D55E00",  # significant increase (dark orange)
      "Y_nonsig"  = "#FDD4B3",  # non-significant increase (lighter orange)
      "N_sig"     = "#0072B2",  # significant decrease (dark blue)
      "N_nonsig"  = "#CFE6F3"   # non-significant decrease (lighter blue)
    ),
    labels = c(
      "Y_sig" = "Increase (sig.)",
      "Y_nonsig" = "Increase",
      "N_sig" = "Decrease (sig.)",
      "N_nonsig" = "Decrease"
    ),
    name = "Energy use/bill"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "none",
    # axis.text.x = element_blank(), # Hide x-labels as bars are colored
    axis.text.x = element_text(size = 12, hjust = 1), # Hide x-labels as bars are colored
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # strip.placement = "outside",
    # strip.text = element_text(face = "bold"),
    # panel.spacing.x = unit(3, "lines"),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    strip.placement = "outside",
    panel.spacing = unit(3, "lines"),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 14, face = "bold")
  )



s16c <- f5a_data1 %>% 
  filter(feature == "Higher Fossil Rate") %>%
  mutate(IV = case_when(
    str_detect(IV, "Elec$") ~ "Electricity Use (MWh/HH)",
    str_detect(IV, "Energy$") ~ "Energy Use (MMBtu/HH)",
    str_detect(IV, "Elec Bill$") ~ "Electricity Bill (k$/HH)",
    str_detect(IV, "Bill$") ~ "Energy Bill (k$/HH)"),
    IV = factor(IV, levels = c("Electricity Use (MWh/HH)",
                               "Energy Use (MMBtu/HH)",
                               "Electricity Bill (k$/HH)",
                               "Energy Bill (k$/HH)"))) %>% 
  
  ggplot(aes(x = tech, y = pe, fill = fill_combined)) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40", linewidth = 0.5) +
  geom_col(width = 0.65) +
  geom_errorbar(
    aes(ymin = lwr, ymax = upr),
    width = 0.15,
    linewidth = 0.6,
    color = "gold3"
  ) +
  facet_wrap(~ IV, scales = "free_y", nrow = 1) +
  labs(
    title = "Marginal effect of higher fossil fuel rate",
    
    x = "",
    y = ""
  ) +
  
  scale_fill_manual(
    values = c(
      "Y_sig"     = "#D55E00",  # significant increase (dark orange)
      "Y_nonsig"  = "#FDD4B3",  # non-significant increase (lighter orange)
      "N_sig"     = "#0072B2",  # significant decrease (dark blue)
      "N_nonsig"  = "#CFE6F3"   # non-significant decrease (lighter blue)
    ),
    labels = c(
      "Y_sig" = "Increase (sig.)",
      "Y_nonsig" = "Increase",
      "N_sig" = "Decrease (sig.)",
      "N_nonsig" = "Decrease"
    ),
    name = "Energy use/bill"
  ) +
  
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    # axis.text.x = element_blank(), # Hide x-labels as bars are colored
    axis.text.x = element_text(size = 12, hjust = 1), # Hide x-labels as bars are colored
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # strip.placement = "outside",
    # strip.text = element_text(face = "bold"),
    # panel.spacing.x = unit(3, "lines"),
    axis.title = element_text(size = 14),
    axis.text.y = element_text(size = 12),
    strip.text = element_text(face = "bold", size = 12),
    legend.text = element_text(size = 12),
    strip.placement = "outside",
    panel.spacing = unit(3, "lines"),
    plot.title = element_text(family = "Franklin Gothic Demi", size = 14, face = "bold")
  )


ggsave("./fig/s16.png",
       ggarrange(s16a, s16b, s16c, nrow = 3, common.legend = T, legend = "bottom",
                 labels = c("a", "b", "c"),  # Avoid duplicate labels
                 label.x = 0, label.y = 1, hjust = 0, vjust = 1,
                 font.label = list(size = 18, face = "bold")),

       width = 12, height = 12, dpi = 300)




# Filter the subset of non-PV households for validation
validation_data <- dat %>% filter(PV == 0) %>% 
  drop_na(KWH, DOLLAREL, climate, state)

# Set seed for reproducibility
set.seed(123)

# Define cross-validation control parameters
cv_control <- trainControl(method = "cv", number = 10)

# Train and validate the model out-of-sample
cv_model <- train(
  KWH ~ DOLLAREL + climate + state, 
  data = validation_data, 
  method = "lm", 
  trControl = cv_control
)

# Extract and print out-of-sample metrics
print(cv_model$results)

# Split indices
train_idx <- createDataPartition(validation_datak$WH, p = 0.8, list = FALSE)
train_set <- validation_data[train_idx, ]
test_set  <- validation_data[-train_idx, ]

# Fit the model on the training set
train_model <- lm(KWH ~ DOLLAREL + climate + state, data = train_set)

# Predict on the holdout test set
test_preds <- predict(train_model, newdata = test_set)

# Compute explicit performance metrics
rmse_val <- RMSE(test_preds, test_setk$WH)
mae_val  <- MAE(test_preds, test_setk$WH)
r2_val   <- R2(test_preds, test_setk$WH)


cv_results <- data.frame(actual = test_setk$WH, 
                         predicted = test_preds)

s17a <- ggplot(cv_results, aes(x = actual, y = predicted)) +
  geom_bin2d(bins = 50) +
  scale_fill_viridis_c(option = "plasma", name = "Household count") +
  geom_abline(intercept = 0, slope = 1, color = "red", linetype = "dashed", linewidth = 1) +
  labs(
    title = "Model cross-validation",
    subtitle = sprintf("Out-of-Sample R² = %.3f | RMSE = %.2f MWh", r2_val, rmse_val),
    x = "Observed annual consumption (MWh/HH)",
    y = "Predicted Out-of-Sample consumption (MWh/HH)"
  ) +
  theme_minimal(base_size = 12) +
  theme(
    plot.title = element_text(face = "bold"),
    legend.position = "bottom"
  )


s17b <- f2a_data %>% 
  filter(domain != "Interaction") %>%
  filter(IV %in% otcm[c(1)]) %>% 
  filter(!str_detect(var, ":temp$|:rate|moist")) %>% 
  filter(domain == "Tech\nadoption") %>% 
  reg_plot("var", "domain") +
  labs(title = "Simulated change by adoption") +
  theme_minimal() +
  theme(plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.2),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        strip.background = element_rect(fill = "white", color = "white"),
        strip.text = element_text(color = "black", family = "Franklin Gothic Book", size = 10, face = "bold"),
        legend.position = "none",
        strip.placement = "outside",
        axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
        axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 10),
        axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9))


s17c <- ggarrange(f5pl(s_data_si[1:5,]),f5pl(s_data_si[6:10,]), 
                  nrow = 1)

ggsave("./fig/s17.png", 
       ggarrange(ggarrange(
         s17a,
         s17b, nrow = 1),
         s17c,
         nrow = 2,

         labels = c("a", "b"),  # Avoid duplicate labels
         label.x = 0, label.y = 1, hjust = 0, vjust = 1,
         font.label = list(size = 16, face = "bold")
         # align = "h"
       ),
       width = 12, height = 10, dpi = 300)




### additional
s19 <- (p_hdd | p_cdd) +
  plot_layout(guides = "collect") &
  theme(legend.position = "bottom")


ggsave("./fig/s19.png", s19, width = 10, height = 7, dpi = 300)

