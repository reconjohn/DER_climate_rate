# Function to aggregate data
aggregate_data <- function(data, value_vars, fun, recode_list = NULL, factor_levels = NULL) {
  data %>%
    unite(col = "group", state, climate, sep = "") %>%
    gather(IV, value, all_of(value_vars)) %>%
    group_by(group, IV) %>%
    summarise(value = fun(value), total = n(), .groups = 'drop') %>%
    # Apply recode if provided
    mutate(
      IV = if (!is.null(recode_list)) recode(IV, !!!recode_list) else IV,
      IV = if (!is.null(factor_levels)) factor(IV, levels = factor_levels) else IV
    )
}

# Function to create maps
create_maps <- function(tp_map, variables, fill_var, scale_labels) {
  map(.x = variables, .f = function(x) {
    tp_map %>%
      filter(IV == x) %>%
      ggplot() +
      geom_sf(fill = "white", color = "gray0") +
      geom_sf(aes_string(fill = fill_var), size = 0.3) +
      labs(fill = "") +
      theme_minimal() +
      scale_fill_viridis_c(labels = scale_labels) +
      coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
      theme(legend.position = "right",
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.3, 'cm'))
  })
}


# Function to prepare data based on climate and RASS flag
prepare_data <- function(data, climate_label, RASS = FALSE, Bill = FALSE) {
  energy_keys <- if (RASS) c("Elec", "Gas", "Energy") else ipt[-1]
  valid_keys <- if (RASS) energy_keys else if (Bill) c("DOLLAREL","DOLLARNG","TOTALDOL") else c("KWH", "BTUNG", "TOTALBTU")
  data %>%
    filter(adopter %in% c("PV", "EV", "HP", "Non")) %>%
    gather(key, value, all_of(energy_keys)) %>%
    filter(key %in% valid_keys) %>%
    mutate(
      key = recode(key, !!!if (RASS) name_out_r else name_out),
      key = factor(key, levels = if (RASS) otcm_r else otcm),
      adopter = factor(adopter, levels = c("Non", "PV", "EV", "HP")),
      climate = climate_label
    ) %>%
    dplyr::select(adopter, value, key, climate)
}

# Function to create the plot
create_plot <- function(data, x_var, y_var, group_var, color_var, facet_var) {
  ggplot(data, aes_string(x = x_var, y = y_var, group = group_var, fill = color_var)) +
    stat_summary(fun = mean, geom = "col", position = position_dodge(width = 0.9), alpha = 0.6) +
    stat_summary(fun = mean, geom = "point", size = 1, position = position_dodge(width = 0.9), color = "gray20") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9), color = "gray30") +
    labs(title = "", x = "", y = "", fill = "") +
    facet_wrap(as.formula(paste0("~", facet_var)), scales = "free") +
    theme_classic() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "gray22", color = "gray22"),
      strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 12, face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 12),
      axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 12),
      plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold")
    )
}

create_plot1 <- function(data, x_var, y_var, group_var, color_var, facet_var) {
  ggplot(data, aes_string(x = x_var, y = y_var, group = group_var, fill = color_var)) +
    stat_summary(fun = mean, geom = "col", position = position_dodge2(width = 0.9, preserve = "single"), alpha = 0.6, width = 0.9) +
    stat_summary(fun = mean, geom = "point", size = 1, position = position_dodge(width = 0.9), color = "gray20") +
    stat_summary(fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, position = position_dodge(width = 0.9), color = "gray30") +
    facet_wrap(as.formula(paste0("~", facet_var)), scales = "free") +
    labs(title = "", x = "", y = "", fill = "") +
    theme_classic() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "gray22", color = "gray22"),
      strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 12, face = "bold"),
      legend.position = "bottom",
      axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 12),
      axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 12),
      plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold")
    )
}

climate_maps <- function(data, fill_var) {
  ggplot(data) +
    geom_sf(fill = "white", color = "gray0") +
    geom_sf(aes(fill = factor(.data[[fill_var]], levels = rev(sort(unique(data[[fill_var]]))))), size = 0.3) +
    # geom_sf(aes(fill = .data[[fill_var]]), size = 0.3) +
    theme_minimal() +
    labs(title = "", fill = "") +
    coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
             ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
    theme(
      legend.position = "right",
      legend.text = element_text(size = 7),
      legend.key.size = unit(0.3, 'cm'),
      strip.background = element_rect(fill = "gray22", color = "gray22"),
      strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 11),
      axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 11),
      plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.05)
    )
}





process_fit_recs <- function(fit, simbetas, IV_name) {
  # Generate matrices
  df1 <- diag(ncol(simbetas))[-1,]
  df2 <- matrix(0, nrow(df1), ncol(df1))
  
  # Calculate xbeta
  xbeta <- (df1 %*% t(simbetas) - df2 %*% t(simbetas))[1:(m-1),]
  
  # Calculate estimates
  d <- data.frame(
    var = factor(rownames(summary(fit)$coefficients)[-1]),
    pe = rowMeans(xbeta),
    lwr = apply(xbeta, 1, quantile, probs = 0.975),
    upr = apply(xbeta, 1, quantile, probs = 0.025)) %>% 
    mutate(color = ifelse(lwr * upr > 0, "Y", "N"),
           IV = IV_name)
  
  # Random effects
  tp <- sqrt(attr(ranef(fit, condVar = TRUE)$state, "postVar")) * 1.96
  d_re <- data.frame(
    state = rownames(ranef(fit)$state),
    R_effect = ranef(fit)$state[, "(Intercept)"],
    SE = tp[1, 1, ],
    lower = ranef(fit)$state[, "(Intercept)"] - tp[1, 1, ],
    upper = ranef(fit)$state[, "(Intercept)"] + tp[1, 1, ],
    IV = IV_name
  )
  
  tp <- sqrt(attr(ranef(fit, condVar = TRUE)$climate, "postVar")) * 1.96
  d_re1 <- data.frame(
    climate = rownames(ranef(fit)$climate),
    R_effect = ranef(fit)$climate[, "(Intercept)"],
    SE = tp[1, 1, ],
    lower = ranef(fit)$climate[, "(Intercept)"] - tp[1, 1, ],
    upper = ranef(fit)$climate[, "(Intercept)"] + tp[1, 1, ],
    IV = IV_name
  )
  
  list(dd = d, ds = d_re, dc = d_re1)
}


process_fit_recs_d <- function(fit, simbetas, IV_name) {
  # Generate matrices
  df1 <- diag(ncol(simbetas))[-1,]
  df2 <- matrix(0, nrow(df1), ncol(df1))
  
  # Calculate xbeta
  xbeta <- (df1 %*% t(simbetas) - df2 %*% t(simbetas))[1:(m-1),]
  
  # Calculate estimates
  d <- data.frame(
    var = factor(rownames(summary(fit)$coefficients)[-1]),
    pe = rowMeans(xbeta),
    lwr = apply(xbeta, 1, quantile, probs = 0.025),
    upr = apply(xbeta, 1, quantile, probs = 0.975)) %>% 
    mutate(color = ifelse(lwr * upr > 0, "Y", "N"),
           IV = IV_name)
  
  # Random effects
  tp <- sqrt(attr(ranef(fit, condVar = TRUE)$group, "postVar")) * 1.96
  d_re <- data.frame(
    group = rownames(ranef(fit)$group),
    R_effect = ranef(fit)$group[, "(Intercept)"],
    SE = tp[1, 1, ],
    lower = ranef(fit)$group[, "(Intercept)"] - tp[1, 1, ],
    upper = ranef(fit)$group[, "(Intercept)"] + tp[1, 1, ],
    IV = IV_name
  )
  
  list(dd = d, dg = d_re)
}


process_fit_recs_e <- function(fit, simbetas, IV_name) {
  # Generate matrices
  df1 <- diag(ncol(simbetas))[-1,]
  df2 <- matrix(0, nrow(df1), ncol(df1))
  
  # Calculate xbeta
  xbeta <- (df1 %*% t(simbetas) - df2 %*% t(simbetas))[1:(m-1),]
  
  # Calculate estimates
  d <- data.frame(
    var = factor(rownames(summary(fit)$coefficients)[-1]),
    pe = rowMeans(xbeta),
    lwr = apply(xbeta, 1, quantile, probs = 0.025),
    upr = apply(xbeta, 1, quantile, probs = 0.975)) %>% 
    mutate(color = ifelse(lwr * upr > 0, "Y", "N"),
           IV = IV_name)
  
  
  return(d)
}

process_fit_rass <- function(fit, simbetas, IV_name) {
  # Generate matrices
  df1 <- diag(ncol(simbetas))[-1,]
  df2 <- matrix(0, nrow(df1), ncol(df1))
  
  # Calculate xbeta
  xbeta <- (df1 %*% t(simbetas) - df2 %*% t(simbetas))[1:(m-1),]
  
  # Calculate estimates
  d <- data.frame(
    var = factor(rownames(summary(fit)$coefficients)[-1]),
    pe = rowMeans(xbeta),
    lwr = apply(xbeta, 1, quantile, probs = 0.975),
    upr = apply(xbeta, 1, quantile, probs = 0.025)) %>% 
    mutate(color = ifelse(lwr * upr > 0, "Y", "N"),
           IV = IV_name)
  
  # Random effects
  tp <- sqrt(attr(ranef(fit, condVar = TRUE)[[1]], "postVar")) * 1.96
  d_re <- data.frame(
    Climate = rownames(ranef(fit)$Climate),
    R_effect = ranef(fit)$Climate[, "(Intercept)"],
    SE = tp[1, 1, ],
    lower = ranef(fit)$Climate[, "(Intercept)"] - tp[1, 1, ],
    upper = ranef(fit)$Climate[, "(Intercept)"] + tp[1, 1, ],
    IV = IV_name
  )
  
  list(dd = d, dc_r = d_re)
}

reg_plot <- function(data, y_var, facet_var){
  data %>%
    ggplot(aes(x = pe, y = reorder(!!sym(y_var), pe), xmin = lwr, xmax = upr)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray30") +
    geom_pointrangeh(aes(fill = color), position = position_dodge2v(height = 0.4), fatten = 4, size = 0.5, pch = 21) +
    facet_grid(.data[[facet_var]] ~ IV, scale = "free", switch = "y") +
    theme_bw() +
    # coord_flip() +
    labs(x = "", y = "", fill = "Significant", title = "") +
    scale_fill_manual(values = c("red", "gray")) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "gray22", color = "gray22"),
      strip.text = element_text(color = "white", family = "Franklin Gothic Book", size = 10, face = "bold"),
      legend.position = "none",
      axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
      axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 10),
      axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.4)
    )
} 


r_combine <- function(dc, ds, re = F) {
  
  if(re){
    cl %>% 
      st_drop_geometry() %>% 
      left_join(dc, by = "climate") %>% 
      mutate(IV = recode(IV, !!!name_out),
             IV = factor(IV, levels = otcm)) %>% 

      inner_join(US %>% 
                   left_join(ds, by = "state") %>% 
                   mutate(IV = recode(IV, !!!name_out),
                          IV = factor(IV, levels = otcm)), by = "IV") %>% 
      unite(col = "group", state, climate, sep = "", remove = F) %>% 
      mutate(value = R_effect.x + R_effect.y)
  }else{
    cl %>% 
      st_drop_geometry() %>% 
      left_join(dc, by = "climate") %>% 
      mutate(IV = recode(IV, !!!name_out),
             IV = factor(IV, levels = otcm)) %>% 

      inner_join(US %>% 
                   left_join(ds, by = "state") %>% 
                   mutate(IV = recode(IV, !!!name_out),
                          IV = factor(IV, levels = otcm))  %>% 
                   dplyr::select(state,IV,tech,R_effect), by = c("IV", "tech")) %>% 
      unite(col = "group", state, climate, sep = "", remove = F) %>% 
      mutate(value = R_effect.x + R_effect.y)
  }

}


r_combine_d <- function(dc, ds, re = F) {
  
  if(re){
    cl %>% 
      st_drop_geometry() %>% 
      left_join(dc, by = "climate") %>% 
      mutate(IV = recode(IV, !!!name_out),
             IV = factor(IV, levels = otcm)) %>% 
      
      inner_join(ds %>% 
                   mutate(IV = recode(IV, !!!name_out),
                          IV = factor(IV, levels = otcm)), by = "IV") %>% 
      unite(col = "group", state, climate, sep = ":", remove = F) %>% 
      mutate(value = R_effect.x + R_effect.y)
  }else{
    cl %>% 
      st_drop_geometry() %>% 
      left_join(dc, by = "climate") %>% 
      mutate(IV = recode(IV, !!!name_out),
             IV = factor(IV, levels = otcm)) %>% 
      
      inner_join(ds %>% 
                   mutate(IV = recode(IV, !!!name_out),
                          IV = factor(IV, levels = otcm))  %>% 
                   dplyr::select(state,IV,tech,R_effect), by = c("IV", "tech")) %>% 
      unite(col = "group", state, climate, sep = ":", remove = F) %>% 
      mutate(value = R_effect.x + R_effect.y)
  }
  
}

mapping_r <- function(data, x_var, color = FALSE, tech = NULL, name = NULL) {
  maps <- map(.x = x_var, .f = function(x) {
    
    us <- sc_map %>% 
      left_join(data, by = "group") 
    
    # Choose color scale
    color_scale <- if (color) {
      # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
      # scale_fill_gradient2(trans = scales::trans_new("signed_log",
      #                                                transform = function(x) sign(x) * log1p(abs(x)),
      #                                                inverse = function(x) sign(x) * (exp(abs(x)) - 1)),
      #                      low = "blue", mid = "white", high = "red", midpoint = 0) 
      # scale_fill_distiller(palette = "RdBu") 
      
      
      rdBu <- RColorBrewer::brewer.pal(5, "RdBu")
      
      scale_fill_gradient2(
        low  = rdBu[5],   # darkest blue
        mid  = rdBu[3],    # white-ish center
        high = rdBu[1],    # darkest red
        midpoint = 0
      )
      
      
    } else {
      scale_fill_viridis_c()
    }
    
    # Check if tech is provided and not NULL
    if (!is.null(tech)) {
      # Loop through tech options
      tech_maps <- map(tech, function(t) {
        us %>%
          filter(IV == x, tech == t) %>%
          ggplot() +
          geom_sf(fill = "white", color = "gray0") + # US border
          geom_sf(aes(fill = value), size = 0.3) +
          geom_sf(
            data = subset(us, group == name),
            fill = NA,
            color = "gold3",
            linewidth = 0.3
          ) +
          
          theme_minimal() +
          color_scale +
          labs(title = "", fill = "") +
          coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                   ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 6),
            legend.key.size = unit(0.3, 'cm'),
            plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
            plot.title = element_text(size = 12, margin = margin(b = 3))
          )
      })
      return(tech_maps) # Return list of tech maps
    } else {
      # Plot without tech filter
      us %>%
        filter(IV == x) %>%
        ggplot() +
        geom_sf(fill = "white", color = "gray0") + # US border
        geom_sf(aes(fill = value), size = 0.3) +
        geom_sf(
          data = subset(us, group == name),
          fill = NA,
          color = "gold",
          linewidth = 0.3
        ) +
        
        theme_minimal() +
        color_scale +
        labs(title = "", fill = "") +
        coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                 ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
        theme(
          legend.position = "right",
          legend.text = element_text(size = 6),
          legend.key.size = unit(0.3, 'cm'),
          plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
          plot.title = element_text(size = 12, margin = margin(b = 3))
          
        )
    }
  })
  
  # Flatten the list if tech is provided
  if (!is.null(tech)) {
    maps <- unlist(maps, recursive = FALSE)
  }
  
  return(maps)
}

mapping <- function(data, x_var, color = FALSE, tech = NULL, name = NULL) {
  maps <- map(.x = x_var, .f = function(x) {
    
    us <- sc_map %>% 
      left_join(data, by = "group") 
    
    # Choose color scale
    color_scale <- if (color) {
      # scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0)
      # scale_fill_gradient2(trans = scales::trans_new("signed_log",
      #                                                transform = function(x) sign(x) * log1p(abs(x)),
      #                                                inverse = function(x) sign(x) * (exp(abs(x)) - 1)),
      #                      low = "blue", mid = "white", high = "red", midpoint = 0) 
      # scale_fill_distiller(palette = "RdBu") 
      
      
      rdBu <- RColorBrewer::brewer.pal(5, "RdBu")
      
      scale_fill_gradient2(
        low  = rdBu[5],   # darkest blue
        mid  = rdBu[3],    # white-ish center
        high = rdBu[1],    # darkest red
        midpoint = 0
      )
      
      
    } else {
      scale_fill_viridis_c()
    }
    
    # Check if tech is provided and not NULL
    if (!is.null(tech)) {
      # Loop through tech options
      tech_maps <- map(tech, function(t) {
        us %>%
          filter(IV == x, tech == t) %>%
          ggplot() +
          geom_sf(fill = "white", color = "gray0") + # US border
          geom_sf(aes(fill = value), size = 0.3) +
          geom_sf(
            data = subset(us, group == name),
            fill = NA,
            color = "gold3",
            linewidth = 0.3
          ) +
          
          theme_minimal() +
          color_scale +
          labs(title = "", fill = "") +
          coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                   ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
          theme(
            legend.position = "bottom",
            legend.text = element_text(size = 8),
            legend.key.size = unit(0.6, 'cm'),
            plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
            plot.title = element_text(size = 12, margin = margin(b = 3))
          )
      })
      return(tech_maps) # Return list of tech maps
    } else {
      # Plot without tech filter
      us %>%
        filter(IV == x) %>%
        ggplot() +
        geom_sf(fill = "white", color = "gray0") + # US border
        geom_sf(aes(fill = value), size = 0.3) +
        geom_sf(
          data = subset(us, group == name),
          fill = NA,
          color = "gold",
          linewidth = 0.3
        ) +
        
        theme_minimal() +
        color_scale +
        labs(title = "", fill = "") +
        coord_sf(crs = st_crs(2163), xlim = c(-2500000, 2500000), 
                 ylim = c(-2300000, 730000), expand = FALSE, datum = NA) +
        theme(
          legend.position = "bottom",
          legend.text = element_text(size = 8),
          legend.key.size = unit(0.6, 'cm'),
          plot.margin = margin(t = 20, r = 5, b = 5, l = 5),
          plot.title = element_text(size = 12, margin = margin(b = 3))
          
        )
    }
  })
  
  # Flatten the list if tech is provided
  if (!is.null(tech)) {
    maps <- unlist(maps, recursive = FALSE)
  }
  
  return(maps)
}

mapping1 <- function(data, x_var, color = FALSE, tech = NULL, name = NULL) {
  
  # 1. Prepare map data with spatial join
  us <- sc_map %>% 
    left_join(data, by = "group") %>%
    filter(IV %in% x_var)
  
  # 2. Filter by tech if supplied
  if (!is.null(tech)) {
    us <- us %>% filter(tech %in% local(tech))
  }
  
  # 3. Highlight geometry layer (if 'name' specified)
  highlight_data <- if (!is.null(name)) {
    us %>% filter(group %in% local(name))
  } else {
    NULL
  }
  
  # 4. Color scale setup
  color_scale <- if (color) {
    rdBu <- RColorBrewer::brewer.pal(5, "RdBu")
    scale_fill_gradient2(
      low      = rdBu[5],  # darkest blue
      mid      = rdBu[3],  # neutral
      high     = rdBu[1],  # darkest red
      midpoint = 0
    )
  } else {
    scale_fill_viridis_c()
  }
  
  # 5. Build base plot
  p <- ggplot(us) +
    geom_sf(fill = "white", color = "gray0") +
    geom_sf(aes(fill = value), size = 0.3)
  
  # Add highlight boundary layer if available
  if (!is.null(highlight_data) && nrow(highlight_data) > 0) {
    p <- p + geom_sf(
      data = highlight_data,
      fill = NA,
      color = "gold3",
      linewidth = 0.3
    )
  }
  
  # 6. Apply themes, coordinate projections, and faceting
  p <- p +
    theme_minimal() +
    color_scale +
    labs(fill = "Value") +
    coord_sf(
      crs = st_crs(2163), 
      xlim = c(-2500000, 2500000), 
      ylim = c(-2300000, 730000), 
      expand = FALSE, 
      datum = NA
    ) +
    facet_wrap(~ IV, nrow = 1) +  # Facets panels side-by-side
    theme(
      legend.position = "bottom",
      legend.text = element_text(size = 8),
      legend.key.size = unit(0.6, 'cm'),
      strip.text = element_text(size = 12, face = "bold"),
      plot.margin = margin(t = 10, r = 5, b = 5, l = 5)
    )
  
  return(p)
}

rate_plot <- function(data, der, output, hline = F){
  # mean_values <- re_average %>% 
  #   filter(var == der) %>% 
  #   dplyr::rename(tech = var) %>% 
  #   dplyr::select(tech, IV, pe) %>% 
  #   filter(IV %in% output)
  # 
  # hline_option <- if (hline) {
  #   geom_hline(yintercept = 0, 
  #              color = "red", linetype = "dashed")
  # } else {
  #   geom_hline(data = mean_values, aes(yintercept = pe), 
  #              color = "red", linetype = "dashed") 
  # }
  
  data %>% 
    filter(tech == der) %>% 
    filter(IV %in% output) %>% 
    
    ggplot()+
    
    geom_hline(yintercept = 0,
               color = "black", linetype = "dashed") +
    
    # hline_option + 
    
    geom_point(aes(x = E_rate, y = R_effect),size = 0.5) +
    geom_smooth(aes(x = E_rate, y = R_effect), method = "lm") +
    labs(x = "Rate ($/ MWh)", y = "") +
    facet_wrap(tech~IV, scales = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
          strip.text.x = element_text(margin = margin(-0.2,0,0,0, "cm")), 
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.14)) 
  
}


rate_plot1 <- function(data, der, output, hline = F){
  mean_values <- re_average %>% 
    filter(var == der) %>% 
    dplyr::rename(tech = var) %>% 
    dplyr::select(tech, IV, pe) %>% 
    filter(IV %in% output) 
  
  hline_option <- if (hline) {
    geom_hline(aes(yintercept = 0, linetype = "Mean Effect"),
               color = "red")
  } else {
    geom_hline(data = mean_values, aes(yintercept = pe, linetype = "Mean Effect"), 
               color = "red") 
  }
  
  data %>% 
    filter(tech == der) %>% 
    filter(IV %in% output) %>% 

    ggplot()+
    
    hline_option + 
    
    geom_point(aes(x = value, y = R_effect),size = 0.5) +
    geom_smooth(aes(x = value, y = R_effect, linetype = "Fitted Slope"), method = "lm") +
    labs(x = "Rate ($/MWh)", y = "",linetype = "", title = "") +
    facet_grid(IV~tech, scales = "free", switch = "y") +
    # Customize the linetype legend
    scale_linetype_manual(values = c("Mean Effect" = "dashed", "Fitted Slope" = "solid")) +
    
    theme_bw() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=11),
          # strip.text.x = element_text(margin = margin(-0.01,0,0,0, "cm")), 
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          plot.title=element_text(family="Franklin Gothic Demi", size=12, hjust = -0.2)) 

}

rate_plot2 <- function(data, output){
  
  tech_colors <- c(
    "Non-adoption" = "gray20", 
    "PV" = "#d95f02",   # orange
    "EV" = "#7570b3",   # purple
    "HP" = "#1b9e77"    # teal
  )
  
  
  data %>%
    filter(IV %in% output) %>%
    mutate(tech = factor(tech, levels = c("Non-adoption", "PV", "EV", "HP"))) %>%
    
    ggplot(aes(x = E_rate, y = R_effect, color = tech, fill = tech)) +
    # geom_point(size = 0.7, alpha = 0.7) +
    geom_smooth(method = "lm", se = T, linewidth = 0.9, alpha = 0.1) +
    
    facet_wrap(~IV, scales = "free", nrow = 1) +
    
    labs(x = "Electricity Rate ($/MWh)", y = "\n", color = "", fill = "", title = "") +
    
    scale_color_manual(values = tech_colors) +
    scale_fill_manual(values = tech_colors) +
    
    theme_bw(base_family = "Franklin Gothic Book") +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "white", color = NA),
      strip.text = element_text(color = "black", size = 16),
      legend.position = "bottom",
      legend.title = element_text(size = 10),
      legend.text = element_text(size = 12),
      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(color = "black", size = 14),
      plot.title = element_text(family = "Franklin Gothic Demi", size=12, hjust = 0)
    )
  
}


rate_fuel_plot <- function(data, der, output, hline = F){
  mean_values <- re_average %>% 
    filter(var == der) %>% 
    dplyr::rename(tech = var) %>% 
    dplyr::select(tech, IV, pe) %>% 
    filter(IV %in% output)
  
  hline_option <- if (hline) {
    geom_hline(yintercept = 0, 
               color = "red", linetype = "dashed")
  } else {
    geom_hline(data = mean_values, aes(yintercept = pe), 
               color = "red", linetype = "dashed") 
  }
  
  data %>% 
    filter(tech == der) %>% 
    filter(IV %in% output) %>% 
    
    ggplot()+
    
    hline_option + 
    
    geom_point(aes(x = IV.y, y = R_effect),size = 0.5) +
    geom_smooth(aes(x = IV.y, y = R_effect), method = "lm") +
    labs(x = "Rate ($/unit)", y = "") +
    facet_wrap(tech~IV, scales = "free", nrow = 1) +
    theme_minimal() +
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=12),
          strip.text.x = element_text(margin = margin(-0.2,0,0,0, "cm")), 
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=9),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          plot.title=element_text(family="Franklin Gothic Demi", size=16, hjust = -0.14)) 
  
}


# Helper function to create filtered and joined datasets
create_rate_data <- function(data, rate_data, iv_list, tech_name = NULL) {
  bind_rows(
    lapply(iv_list, function(iv) {
      data %>%
        filter(IV == iv) %>%
        left_join(rate_data %>% 
                    filter(IV %in% c("E_rate")) %>% 
                    pivot_wider(names_from = "IV", values_from = "value"), by = "group") %>%
        na.omit()
    })
  ) %>%
    {
      if (!is.null(tech_name)) mutate(., tech = tech_name, .before = 1) else .
    }
}

# for individual fuel
create_fuel_rate_data <- function(data, rate_data, iv_list, tech_name = NULL) {
  bind_rows(
    lapply(iv_list, function(iv) {
      data %>%
        filter(IV == iv) %>%
        left_join(rate_data %>% 
                    { if (iv %in% otcm[c(1,2,7)]) filter(., IV == "E_rate") else if (iv %in% otcm[c(3,6)]) filter(., IV == "G_rate") else filter(., IV == "rate")},
                  by = "group") %>%
        na.omit()
    })
  ) %>%
    {
      if (!is.null(tech_name)) mutate(., tech = tech_name, .before = 1) else .
    }
}

# Function to fit random effect model and extract results
fit_random_effect <- function(data, iv, tech_effect) {
  data %>%
    filter(IV == iv & tech == tech_effect) %>%
    mutate(E_rate = scale(E_rate),
           G_rate = scale(G_rate),
           # rate = scale(rate),
           moist = ifelse(temp == 7, "7", moist),
           temp = as.numeric(temp),
           temp = scale(temp)) %>%
    {
      fit <- lm(R_effect ~ (E_rate + G_rate + temp) * moist, data = .)
      
      d <- tidy(fit) %>%
        filter(grepl(":", term)) %>% # Keep only the interaction terms (e.g., E_rate:moistA)
        separate(term, into = c("var", "moist"), sep = ":") %>%
        mutate(
          moist = gsub("moist", "", moist),
          SE_95 = std.error * 1.96, # 95% Margin of Error
          IV = iv,
          TE = tech_effect
        ) %>%
        dplyr::select(moist, var, R_effect = estimate, SE = SE_95, IV, TE)
      # 
      # fit <- lmer(R_effect ~ (E_rate + G_rate + temp | moist), data = .)
      # tp <- sqrt(attr(ranef(fit, condVar = TRUE)[[1]], "postVar")) * 1.96
      # 
      # as.data.frame(ranef(fit)$moist) %>%
      #   tibble::rownames_to_column("moist") %>%
      #   dplyr::select(-`(Intercept)`) %>%
      #   gather(var, R_effect, E_rate:temp) %>%
      #   mutate(SE = c(tp[2, 2, ], tp[3, 3, ], tp[4,4, ]), 
      #          IV = iv,
      #          TE = tech_effect)
    }
}


# Function to fit random effect model and extract results
fit_random_effect_e <- function(data, iv, tech_effect) {
  data %>%
    filter(IV == iv & tech == tech_effect) %>%
    mutate(rate = scale(rate),
           # G_rate = scale(G_rate),
           # E_rate = scale(E_rate),
           moist = ifelse(temp == 7, "7", moist),
           temp = as.numeric(temp),
           temp = scale(temp)) %>%
    {
      fit <- lm(R_effect ~ (rate + temp + moist), data = .)
      
      summary(fit)$coeff %>% 
        as.data.frame() %>% 
        tibble::rownames_to_column("var") %>%
        filter(var %in% c("rate","temp")) %>% 
        dplyr::rename(SE = "Std. Error") %>% 
        dplyr::select(var:SE) %>% 
        mutate(tech = tech_effect,
               effect = iv)
    }
}


# Generate the results
generate_results <- function(data, name_out, eff) {
  bind_rows(
    lapply(1:7, function(i) {
      bind_rows(
        lapply(1:4, function(k) {
          fit_random_effect(data, name_out[[i]], eff[k])
        })
      )
    })
  )
}

generate_results_e <- function(data, name_out, eff) {
  bind_rows(
    lapply(1:7, function(i) {
      bind_rows(
        lapply(1:4, function(k) {
          fit_random_effect_e(data, name_out[[i]], eff[k])
        })
      )
    })
  )
}



# Function to create the plot
re_plot <- function(data, variable, re, output, facet_type = F) {
  
  data %>%
    filter(var == variable) %>%
    mutate(IV = factor(IV, levels = otcm),
           upr = R_effect + SE,
           lwr = R_effect - SE,
           color = ifelse(upr * lwr > 0, "Y", "N"),
           color = factor(color, levels = c("Y", "N")),
           var = if (facet_type == T) "Higher Rate" else "Colder Climate",
           TE = factor(TE, levels = c("Regional Effect", tech)),
           moist = factor(moist, levels = c("A","B","C","7"))) %>%
    filter(TE == re, IV %in% output) %>% 
    
    
    ggplot(aes(x = R_effect, y = moist, xmin = lwr, xmax = upr)) +
    geom_vline(xintercept = 0, linetype = "dashed", size = 0.5, color = "gray30") +
    geom_pointrangeh(aes(fill = color), 
                     position = position_dodge2v(height = 0.4), 
                     fatten = 4, size = 0.5, pch = 21) +
    facet_grid(IV ~ TE + var, switch = "y", scales = "free") +
    
    # # Conditional facet logic
    # {if (facet_type == F)
    #   coord_flip()} +
    
    coord_flip() +
    
    theme_bw() +
    labs(x = "", y = "", fill = "Significant", title = "") +
    scale_fill_manual(values = c("red", "gray")) +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "gray22", color = "gray22"),
      strip.text = element_text(color = 'white', family = "Franklin Gothic Book", size = 12),
      legend.position = "bottom",
      axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 11),
      plot.title = element_text(family = "Franklin Gothic Demi", size = 16, face = "bold", hjust = -0.1)
    )
}


climate_plot <- function(data, der, output, hline = F){
  
  # data <- reffect
  # der <- "Regional Effect"
  # output <- otcm[c(2,3,7)]
  # hline = T
  # 
  
  mean_values <- re_average %>% 
    filter(var == der) %>% 
    filter(IV %in% output) 
  
  hline_option <- if (hline) {
    
    geom_hline(aes(yintercept = 0, linetype = "Mean Effect"), 
               color = "red") 

  } else {
    geom_hline(data = mean_values, aes(yintercept = pe, linetype = "Mean Effect"), 
               color = "red") 
  }
  
  data %>% 
    filter(tech == der) %>% 
    separate(group, into = c('state', 'climate'), sep = 2) %>% 
    separate(climate, into = c('temp', 'moist'), sep = 1) %>% 
    mutate(temp = factor(temp)) %>% 
    # mutate(climate = factor(climate, levels = c("1A","2A","3A","4A","5A","6A",
    #                                             "2B","3B","4B","5B","6B","3C","4C","7")),
    #        zone = ifelse(climate %in% c("2B","3B","4B","5B","6B"), "Dry (B)",
    #                      ifelse(climate %in% c("3C","4C"), "Marine (C)",
    #                             ifelse(climate == "7", "Mixed","Moist (A)"))),
    #        zone = factor(zone, levels = c("Moist (A)","Dry (B)","Marine (C)","Mixed"))) %>% 
    filter(IV %in% output) %>% 

    
    ggplot() +
    # Add mean horizontal lines
    # hline_option + 
    
    # geom_hline(aes(yintercept = 0, linetype = "Zero"), 
    #            color = "black") +

    
    stat_summary(aes(x = temp, y = R_effect, group = 1),
                 fun = mean, geom = "point", color = "blue") +  # Mean trend line
    stat_summary(aes(x = temp, y = R_effect, group = 1), 
                 fun.data = mean_cl_boot, geom = "errorbar", width = 0.2, color = "blue") + # Confidence interval
    geom_smooth(aes(x = temp, y = R_effect, group = 1),
                method = "loess", se = TRUE, linewidth = 1, fill = "gray70", color = "orange") +  # Smooth trend per zone
    
    facet_grid(IV ~ tech, switch = "y", scales = "free")+
    theme_bw() +
    
    labs(x = "", y ="", color = "",
         title = "", linetype = "") +
    
    # Customize the linetype legend
    # scale_linetype_manual(values = c("Zero" = "dashed", "Mean Effect" = "dashed")) +
    
    theme(panel.grid.minor = element_blank(),
          panel.grid.major.x = element_blank(),
          strip.background =element_rect(fill="gray22",color="gray22"),
          strip.text = element_text(color = 'white',family="Franklin Gothic Book",size=11, face = "bold"),
          legend.position = "none",
          axis.text.x = element_text(color = "black",family="Franklin Gothic Book",size=11),
          axis.text.y = element_text(color = "black",family="Franklin Gothic Book",size=11),
          axis.title.x = element_text(color = "black",family="Franklin Gothic Book",size=12),
          plot.title=element_text(family="Franklin Gothic Demi", size=12, hjust = -0.9)) 
  
}

# plot_effects <- function(data, filter_TE = NULL, filter_IV, title, facet_var = "IV", x_var = "TE", 
#                          legend_pos = "none", facet = F) {
#   
#   facet_option <- if (facet) {
#     
#     facet_wrap(as.formula(paste0("~", facet_var)), scales = "free") 
#     
#   } else {
#     facet_wrap(as.formula(paste0("~", facet_var)))
#   }
# 
#   
#   data %>%
#     filter(moist != 7) %>%
#     { if (!is.null(filter_TE)) filter(., TE %in% filter_TE) else . } %>%
#     filter(IV %in% filter_IV) %>%
#     mutate(
#       var = if(facet) case_when(var == "rate" ~ "Higher\nRate", TRUE ~ "Colder\nClimate") else case_when(var == "rate" ~ "Higher Rate", TRUE ~ "Colder Climate"),
#       TE = factor(TE, levels = filter_TE),
#       IV = factor(IV, levels = otcm)
#     ) %>%
#     # ggplot(aes_string(x = x_var, y = "R_effect", fill = "moist")) +
#     ggplot(aes(x = .data[[x_var]], 
#                      y = .data[["R_effect"]], 
#                      fill = .data[["moist"]])) +
#     
#     geom_hline(yintercept = 0, linetype = "dashed") +
#     stat_summary(aes(group = 1), fun.data = mean_cl_boot,
#                  geom = "errorbar", width = 0.4, color = "blue") +
#     geom_point(pch = 21, size = 3) +
#     scale_fill_manual(values = c("#7fc97f", "#fdc086", "#ffff99")) +
#     labs(x = "", y = "", fill = "Zone", title = title) +
#     
#     facet_option + 
# 
#     theme_bw() +
#     theme(
#       panel.grid.minor = element_blank(),
#       panel.grid.major.x = element_blank(),
#       strip.background = element_rect(fill = "gray22", color = "gray22"),
#       strip.text = element_text(color = "white", family = "Franklin Gothic Book", size = 11, face = "bold"),
#       legend.position = legend_pos,
#       axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#       axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#       axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
#       plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "plain")
#     )
# }


plot_effects <- function(data, filter_TE = NULL, filter_IV, filter_RE, title, facet_var = "IV", x_var = "TE", 
                         legend_pos = "none", facet = F) {
  
  facet_option <- if (facet) {
    
    facet_wrap(as.formula(paste0("~", facet_var)), scales = "free", nrow = 1) 
    
  } else {
    facet_wrap(as.formula(paste0("~", facet_var)))
  }
  
  
  data %>%
    filter(moist != 7) %>%
    { if (!is.null(filter_TE)) filter(., TE %in% filter_TE) else . } %>%
    filter(IV %in% filter_IV) %>%
    filter(var == filter_RE) %>% 
    filter(!(IV == "Grid Elec\n(MWh/HH)" & TE != "PV")) %>% 
    mutate(
      var = if(facet) case_when(var == "rate" ~ "Higher\nRate", TRUE ~ "Colder\nClimate") else case_when(var == "rate" ~ "Higher Rate", TRUE ~ "Colder Climate"),
      TE = factor(TE, levels = filter_TE),
      IV = factor(IV, levels = otcm)
    ) %>%
    # ggplot(aes_string(x = x_var, y = "R_effect", fill = "moist")) +
    ggplot(aes(x = .data[[x_var]], 
               y = .data[["R_effect"]], 
               fill = .data[["moist"]])) +
    
    geom_hline(yintercept = 0, linetype = "dashed") +
    stat_summary(aes(group = 1), fun.data = mean_cl_boot,
                 geom = "errorbar", width = 0.4, color = "blue") +
    geom_point(pch = 21, size = 3) +
    scale_fill_manual(values = c("#7fc97f", "#fdc086", "#ffff99")) +
    labs(x = "", y = "", fill = "Climate Zone", title = title) +
    
    facet_option + 
    
    theme_bw() +
    theme(
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_blank(),
      strip.background = element_rect(fill = "gray22", color = "gray22"),
      strip.text = element_text(color = "white", family = "Franklin Gothic Book", size = 11, face = "bold"),
      legend.position = legend_pos,
      axis.text.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      axis.text.y = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      axis.title.x = element_text(color = "black", family = "Franklin Gothic Book", size = 9),
      plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold")
    )
}



waterplot <- function(outcome, te, region, title, yaxis){
  
  ground <- f2a_data %>% 
    filter(IV == outcome & var == te) %>% 
    pull(pe)
  
  # regional effect
  df <- dg_re %>% 
    mutate(IV = recode(IV, !!!name_out),
           IV = factor(IV, levels = otcm)) %>% 
    filter(group == region & IV == outcome) %>% 
    dplyr::select(group, R_effect) %>% 
    dplyr::rename("Non-adoption" = "R_effect") %>% 
    left_join(
      # HP regional effect
      dg_rs %>% 
        mutate(IV = recode(IV, !!!name_out),
               IV = factor(IV, levels = otcm)) %>% 
        filter(group == region & IV == outcome & tech == te) %>% 
        dplyr::select(group, R_effect) %>% 
        dplyr::rename("Tech regional" = "R_effect"), by = "group"
    ) %>% 
    mutate(`Tech average` = ground,
           Final = `Non-adoption` + `Tech regional` + `Tech average`) %>% 
    distinct()
  
  
  
  df_long <- df %>%
    dplyr::select(group, `Non-adoption`, `Tech regional`, `Tech average`) %>%
    pivot_longer(
      cols = c(`Non-adoption`, `Tech regional`, `Tech average`),
      names_to = "component",
      values_to = "value"
    ) %>%
    mutate(
      component = factor(component,
                         levels = c("Non-adoption", "Tech average", "Tech regional"))
    )
  
  # compute start/end cleanly
  df_wf <- df_long %>%
    arrange(component) %>%
    mutate(
      start = lag(cumsum(value), default = 0),
      end   = start + value
    ) %>%
    ungroup()
  
  # add Final bar
  df_final <- df %>%
    transmute(
      group,
      component = factor("Final", levels = c("Non-adoption","Tech average", "Tech regional","Final")),
      value = Final,
      start = 0,
      end = Final
    )

  bind_rows(df_wf, df_final) %>% 
  ggplot(aes(x = component)) +
  
  # waterfall bars
  geom_rect(
    aes(
      xmin = as.numeric(component) - 0.45,
      xmax = as.numeric(component) + 0.45,
      ymin = start,
      ymax = end,
      fill = component
    ),
    color = NA,
    alpha = 0.8
  ) +
  
  # direction arrows
  geom_segment(
    aes(
      x = as.numeric(component),
      xend = as.numeric(component),
      y = start + (end - start) * 0.9,
      yend = end
    ),
    arrow = arrow(length = unit(0.2, "cm")),
    linewidth = 0.6,
    color = "black"
  ) +
  
  # dashed zero line
  geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
  
  scale_fill_manual(values = c(
    "Non-adoption" = "#0072B2",  
    "Tech average"    = "#d95f02",
    "Tech regional"   = "#7570b3",
    "Final"        = "#009E73"
  )) +
  
  labs(
    x = "",
    y = yaxis,
    title = title,
    fill = ""
  ) +
  
  theme_minimal(base_size = 12) +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_blank(),
    legend.position = "none",
    legend.text = element_text(size = 10),
    legend.key.size = unit(0.3, 'cm'),
    plot.title = element_text(size = 12, face = "bold"),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  ) +
  guides(fill = guide_legend(nrow = 2))
}


relation_plot <- function(outcome, name){
  tech_colors <- c(
    "PV" = "#d95f02",   # orange
    "EV" = "#7570b3",   # purple
    "HP" = "#1b9e77"    # teal
  )

  dg_re %>% 
    mutate(IV = recode(IV, !!!name_out),
           IV = factor(IV, levels = otcm)) %>% 
    filter(IV == outcome) %>% 
    dplyr::select(group, R_effect) %>% 
    mutate(tech = "Non-adoption") %>% 
    rbind(  dg_rs %>% 
              mutate(IV = recode(IV, !!!name_out),
                     IV = factor(IV, levels = otcm)) %>% 
              filter(IV == outcome) %>% 
              dplyr::select(group, R_effect, tech)
    ) %>%
    pivot_wider(
      names_from = tech,
      values_from = R_effect
    ) %>%
    pivot_longer(
      cols = c(PV, EV, HP),
      names_to = "tech",
      values_to = "effect"
    ) %>% 
    mutate(tech = factor(tech, levels = c("PV", "EV", "HP"))) %>% 

    ggplot(aes(x = `Non-adoption`, y = effect, color = tech, fill = tech)) +
    geom_smooth(se = TRUE, linewidth = 1.1, alpha = 0.25) +
    geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "gray40") +
    scale_color_manual(values = tech_colors) +
    scale_fill_manual(values = tech_colors) +
    theme_bw(base_size = 12) +
    theme(
      legend.position = "none",
      plot.margin = margin(10,5,5,5),
      plot.title = element_text(hjust = 0.5),

      axis.text = element_text(color = "black", size = 12),
      axis.title = element_text(color = "black", size = 12),
      # legend.box = "vertical"
    ) +
    # guides(color = guide_legend(nrow = 1)) +
    labs(
      x = "",
      y = "",
      color = "",
      fill = "",
      title = name
    )
}


f5plot <- function(data_input, der, outcome, leg = T) {
  
  # data_input <- plot_data
  
  # 1. Prepare the Baseline (Non-adoption) reference
  baselines <- data_input %>%
    filter(tech == "Non-adoption") %>%
    dplyr::select(var, effect, 
                  baseline_val = Estimate, 
                  baseline_SE = SE)
  
  # 2. Build Waterfall Data for each Tech (PV, EV, HP)
  plot_df <- data_input %>%
    filter(tech != "Non-adoption") %>%
    left_join(baselines, by = c("var", "effect")) %>%
    # Create the components for the waterfall
    mutate(Final = baseline_val + Estimate) %>%
    # We pivot to get 'Non-adoption' and 'Tech Effect' as steps
    rename("Adoption" = Estimate) %>%
    pivot_longer(cols = c("baseline_val", "Adoption"), 
                 names_to = "component", values_to = "value") %>%
    mutate(component = ifelse(component == "baseline_val", "Non-adoption", component)) %>%
    mutate(component = factor(component, levels = c("Non-adoption", "Adoption", "Final")))
  
  # 3. Compute Waterfall Coordinates (Start and End)
  df_wf <- plot_df %>%
    group_by(var, effect, tech) %>%
    arrange(component) %>%
    mutate(
      start = lag(cumsum(value), default = 0),
      end   = start + value,
      # Error bars for the steps
      step_se = ifelse(component == "Non-adoption", baseline_SE, SE)
    ) %>%
    ungroup()
  
  # 4. Create the Final Bar
  df_final <- plot_df %>%
    group_by(var, effect, tech) %>%
    slice(1) %>% # Take one row per group
    mutate(component = factor("Final", levels = c("Non-adoption", "Adoption", "Final")),
           start = 0,
           end = Final,
           step_se = SE) %>% # Use the tech SE for the final uncertainty
    ungroup()
  
  # 5. Combine and Plot
  df_full <- bind_rows(df_wf, df_final)
  df_full <- df_wf %>% 
    mutate(color = ifelse(value > 0, "Higher", "Lower")) 
  
  create_panel <- function(df, y_title, show_y_axis = TRUE) {
    ggplot(df, aes(x = component)) +
      geom_hline(yintercept = 0, linetype = "dashed", color = "gray40") +
      
      # Waterfall Bars
      geom_rect(aes(xmin = as.numeric(component) - 0.35, 
                    xmax = as.numeric(component) + 0.35,
                    ymin = start, ymax = end, 
                    fill = color)) +
      
      # Error Bars on each step
      geom_errorbar(aes(ymin = end - step_se, ymax = end + step_se), 
                    width = 0.15, color = "gray30") +
      
      # # Direction Arrows (from your logic)
      geom_segment(aes(x = as.numeric(component)-0.25, xend = as.numeric(component)-0.25,
                       y = start + (end - start) * 0.8, yend = end),
                   arrow = arrow(length = unit(0.3, "cm")), color = "yellow", linewidth = 1.2) +
      
      # scale_fill_manual(values = c(
      #   "Non-adoption" = "#0072B2",  
      #   "Tech Effect"  = "#D55E00",  
      #   "Final"        = "#009E73"
      # )) +
      
      scale_fill_manual(values = c(
        "Lower" = "#0072B2",
        "Higher"  = "#D55E00"
      )) +
      
      
      facet_grid(var ~ effect, scales = "free_y", switch = "y") +
      labs(y = y_title, x = "", fill = "Climate/rate effect") +
      theme_minimal() +
      theme(
        legend.position = "none",
        # axis.text.x = element_blank(), # Hide x-labels as bars are colored
        axis.text.x = element_text(angle = 30, size = 12, hjust = 1), # Hide x-labels as bars are colored
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        # strip.placement = "outside",
        # strip.text = element_text(face = "bold"),
        # panel.spacing.x = unit(3, "lines"),
        strip.text.y = if(show_y_axis) element_text(angle = 0, hjust = 0.5) else element_blank(),
        axis.title = element_text(size = 14),
        axis.text.y = element_text(size = 12),
        strip.text = element_text(face = "bold", size = 12),
        legend.text = element_text(size = 12),
        strip.placement = "outside",
        panel.spacing = unit(3, "lines"),
        plot.title = element_text(family = "Franklin Gothic Demi", size = 12, face = "bold", hjust = -0.25)
      )
  }
  
  p1 <- create_panel(df_full %>% filter(effect %in% outcome &
                                          tech == der), "Effect relative to average climate/rate (MMBtu/HH)", TRUE)
  p2 <- create_panel(df_full %>% filter(effect == "Total Bill" &
                                          tech == der), "Effect relative to average climate/rate (k$/HH)", T)
  
  if(leg) ggarrange(p1, p2, nrow = 1, widths = c(3, 1.1), common.legend = TRUE, legend = "bottom") else ggarrange(p1, p2, nrow = 1, widths = c(3, 1.1))
  
}


# build_waterfall_parallel <- function(df, tech, iv, baseline){
#   
#   df_iv <- df %>% filter(str_detect(IV, iv))
#   
#   adopt_row <- df_iv %>% filter(str_detect(var, paste0("^", tech, "$")))
#   temp_row  <- df_iv %>% filter(str_detect(var, paste0("^", tech, ":temp$")))
#   rate_row  <- df_iv %>% filter(str_detect(var, paste0("^", tech, ":E_rate$")))
#   
#   adopt_eff <- adopt_row$pe
#   temp_eff  <- temp_row$pe
#   rate_eff  <- rate_row$pe
#   
#   tibble(
#     step = c("Non-adoption", "Adoption", "Colder\nClimate", "Higher\nElec Rate"),
#     start = c(
#       0,
#       baseline,
#       baseline + adopt_eff,
#       baseline + adopt_eff
#     ),
#     end = c(
#       baseline,
#       baseline + adopt_eff,
#       baseline + adopt_eff + temp_eff,
#       baseline + adopt_eff + rate_eff
#     ),
#     lwr = c(NA, adopt_row$lwr, temp_row$lwr, rate_row$lwr),
#     upr = c(NA, adopt_row$upr, temp_row$upr, rate_row$upr),
#     color = c("Y", adopt_row$fill_combined, temp_row$fill_combined, rate_row$fill_combined)
#   )
# }


build_waterfall_parallel <- function(df, tech, iv, region, baseline){
  
  df_iv <- df %>% filter(str_detect(IV, iv))
  
  adopt_row <- df_iv %>% filter(str_detect(var, paste0("^", tech, "$")))
  region_row  <- df_iv %>% filter(str_detect(var, paste0("^", region)))
  region_tech_row  <- df_iv %>% filter(str_detect(var, paste0("^", tech, ":", region, "$")))

  adopt_eff <- adopt_row$pe
  region_eff  <- region_row$pe
  region_tech_eff  <- region_tech_row$pe
  
  if(region == "temp") name <- "Colder\nClimate" else name <- "Higher\nElec Rate"

  tibble(
    step = c("Non-adopt", "Adopt", name, paste0(name,"\n(Adopt)"), "Final"),
    start = c(
      0,
      baseline,
      baseline + adopt_eff,
      baseline + adopt_eff + region_eff,
      0
    ),
    end = c(
      baseline,
      baseline + adopt_eff,
      baseline + adopt_eff + region_eff,
      baseline + adopt_eff + region_eff + region_tech_eff,
      baseline + adopt_eff + region_eff + region_tech_eff
    ),
    lwr = c(NA, adopt_row$lwr, region_row$lwr, region_tech_row$lwr, NA),
    upr = c(NA, adopt_row$upr, region_row$upr, region_tech_row$upr, NA),
    color = c("S", adopt_row$fill_combined, region_row$fill_combined, region_tech_row$fill_combined, "E")
  )
}



build_waterfall_total <- function(df, tech, iv, baseline){
  
  df_iv <- df %>% filter(str_detect(IV, iv))
  
  adopt_row <- df_iv %>% filter(str_detect(var, paste0("^", tech, "$")))
  temp_row  <- df_iv %>% filter(str_detect(var, paste0("^", "temp")))
  temp_tech_row  <- df_iv %>% filter(str_detect(var, paste0("^", tech, ":", "temp", "$")))
  rate_row  <- df_iv %>% filter(str_detect(var, paste0("^", "E_rate")))
  rate_tech_row  <- df_iv %>% filter(str_detect(var, paste0("^", tech, ":", "E_rate", "$")))
  
  adopt_eff <- adopt_row$pe
  temp_eff  <- temp_row$pe
  temp_tech_eff  <- temp_tech_row$pe
  rate_eff  <- rate_row$pe
  rate_tech_eff  <- rate_tech_row$pe
  
  tibble(
    step = c("Non-adopt", "Adopt", 
             "Colder\nClimate", paste0("Colder\nClimate","\n(Adopt)"), 
             "Higher\nElec Rate", paste0("Higher\nElec Rate","\n(Adopt)"), 
             "Final"),
    start = c(
      0,
      baseline,
      baseline + adopt_eff,
      baseline + adopt_eff + temp_eff,
      baseline + adopt_eff + temp_eff + temp_tech_eff,
      baseline + adopt_eff + temp_eff + temp_tech_eff + rate_eff,
      0
    ),
    end = c(
      baseline,
      baseline + adopt_eff,
      baseline + adopt_eff + temp_eff,
      baseline + adopt_eff + temp_eff + temp_tech_eff,
      baseline + adopt_eff + temp_eff + temp_tech_eff + rate_eff,
      baseline + adopt_eff + temp_eff + temp_tech_eff + rate_eff + rate_tech_eff,
      baseline + adopt_eff + temp_eff + temp_tech_eff + rate_eff + rate_tech_eff
    ),
    lwr = c(NA, adopt_row$lwr, temp_row$lwr, temp_tech_row$lwr, rate_row$lwr, rate_tech_row$lwr, NA),
    upr = c(NA, adopt_row$upr, temp_row$upr, temp_tech_row$upr, rate_row$upr, rate_tech_row$upr, NA),
    color = c("S", adopt_row$fill_combined, temp_row$fill_combined, temp_tech_row$fill_combined, 
              rate_row$fill_combined, rate_tech_row$fill_combined, "E")
  )
}


f5pl <- function(data){
  

  # Derive baseline from the first step's start
  baseline_val <- data %>%
    dplyr::arrange(step_num) %>%
    dplyr::slice(1) %>%
    dplyr::pull(start)
  
  # Precompute arrows and deltas per facet
  data_arrow <- data %>%
    dplyr::group_by(item) %>%
    dplyr::arrange(step_num) %>%
    dplyr::mutate(
      arrow_x    = step_num + 0.45,
      arrow_xend = step_num + 1 - 0.45,
      arrow_y    = end,
      arrow_yend = dplyr::lead(end),
      delta      = round(dplyr::lead(end) - end, 2),
      is_last    = step_num == max(step_num)
    ) %>%
    dplyr::filter(!is_last) %>%
    dplyr::filter(step_num < max(step_num))
  
  ggplot(data = data) +
    
    # Baseline reference line (constant, not aes)
    geom_hline(
      yintercept = baseline_val,
      color = "gray70",
      linetype = "dashed",
      linewidth = 0.4
    ) +
    
    # Waterfall rectangles
    geom_rect(aes(
      xmin = step_num - 0.45,
      xmax = step_num + 0.45,
      ymin = start,
      ymax = end,
      fill = color
    )) +
    
    # Confidence intervals ONLY on incremental effects
    geom_errorbar(
      data = data %>% dplyr::filter(step_num %in% c(2,3,4)),
      aes(
        x = step_num,
        ymin = ci_lower,
        ymax = ci_upper
      ),
      width = 0.15,
      color = "gold2"
    ) +
    
    # Horizontal reference line at bar top
    geom_segment(
      aes(
        x    = step_num - 0.45,
        xend = step_num + 0.45,
        y    = end,
        yend = end
      ),
      linetype = "dotted",
      color = "black",
      linewidth = 0.4
    ) +
    
    # # Numeric labels on bars
    # geom_text(aes(
    #   x = step_num,
    #   y = end,
    #   label = round(end, 1)
    # ),
    # size = 4,
    # vjust = -0.3
    # ) +
    
    # Arrows between steps (facet-safe)
    geom_segment(
      data = data_arrow,
      aes(
        x    = arrow_x,
        xend = arrow_xend,
        y    = arrow_y,
        yend = arrow_yend
      ),
      arrow = arrow(length = unit(0.07, "inches")),
      color = "gray30",
      linewidth = 0.7
    ) +
    
    # Delta labels above arrows
    # geom_text(
    #   data = data_arrow,
    #   aes(
    #     x = (arrow_x + arrow_xend) / 2,
    #     y = arrow_yend*1.1,
    #     label = paste0(ifelse(delta > 0, "+", ""), delta)
    #   ),
    #   size = 3.5,
    #   color = "gray20",
    #   fontface = "bold"
    # ) +
  
    geom_text(
      data = data_arrow,
      aes(
        x = step_num + 1,
        y = end,
        label = paste0(ifelse(delta > 0, "+", ""), delta)
      ),
      size = 3.5,
      vjust = -0.3,
      color = "black",
      fontface = "bold"
    ) +
    
    facet_wrap(~ item, scales = "free_y") +
    
    scale_x_continuous(breaks = 1:7, labels = data$step[1:7]) +
    
    scale_fill_manual(
      values = c(
        "S"        = "ivory2",
        "Y_sig"    = "#D55E00",
        "Y_nonsig" = "#FDD4B3",
        "N_sig"    = "#0072B2",
        "N_nonsig" = "#CFE6F3",
        "E"        = "ivory3"
      ),
      breaks = c("Y_sig", "Y_nonsig", "N_sig", "N_nonsig"),
      labels = c(
        "Y_sig"    = "Increase (sig.)",
        "Y_nonsig" = "Increase",
        "N_sig"    = "Decrease (sig.)",
        "N_nonsig" = "Decrease"
      ),
      name = "Change"
    ) +
    
    labs(
      title = "",
      y = "",
      x = ""
    ) +
    
    theme_minimal(base_size = 12) +
    theme(
      legend.position = "bottom",
      axis.text.x = element_text(size = 9),
      panel.grid.major.x = element_blank(),
      panel.grid.minor.x = element_blank(),
      axis.title = element_text(size = 14),
      axis.text.y = element_text(size = 12),
      strip.text = element_text(face = "bold", size = 12),
      legend.text = element_text(size = 12),
      strip.placement = "outside",
      panel.spacing = unit(3, "lines")
    )
}


