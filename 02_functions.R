ggplot_lancet <- function(...)
  ggplot2::ggplot(...) +
  scale_fill_brewer(palette = "Blues") +
  scale_colour_brewer(palette = "Blues") +
  scale_y_continuous(expand = c(0, 0)) +
  theme_classic() +
  theme(
    axis.title.x       = element_text(margin = margin(10, 0, 0, 0, "pt")),
    axis.title.y       = element_text(margin = margin(0, 10, 0, 0, "pt")),
    axis.text          = element_text(size = 10),
    axis.ticks.length  = unit(5, "pt"),
    panel.grid.major   = element_blank(),
    panel.grid.minor   = element_blank(),
    strip.background   = element_blank(),
    strip.placement    = "outside",
    strip.text.x       = element_text(hjust = 0,
                                      face  = "bold",
                                      size  = 10),
    strip.text.y       = element_text(hjust = 0,
                                      size  = 10),
    text = element_text(
      family = "Helvetica",
      size = 10,
      face = "plain"
    ),
    legend.position = c(1, 1),
    legend.justification = c(1, 1),
    legend.direction = "horizontal",
    legend.key.size = unit(10, "pt")
  ) +
  guides(fill = guide_legend(title.position = "top"))

# -------------------------------------
# National early warning score  --------------------
news <- function(.data, rr, spo2, o2_rx, hypoxic_target, sbp, hr, temp, alt_conscious, 
                 output = c("vector", "components", "df_vector", "df_components"), na_to_zeros = TRUE, all_na_rm = TRUE){
  # Consider changing these to enquo and taking arguments without ""
  
  .rr = enquo(rr)
  .spo2 = enquo(spo2)
  .o2_rx = enquo(o2_rx)
  .hypoxic_target = enquo(hypoxic_target)
  .sbp = enquo(sbp)
  .hr = enquo(hr)
  .temp = enquo(temp)
  .alt_conscious = enquo(alt_conscious)
  
  out <- .data %>%
    # Ensure correct class / format
    dplyr::mutate_at(vars(!! .rr, !! .spo2, !! .sbp, !! .hr, !! .temp), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .o2_rx, !! .hypoxic_target, !! .alt_conscious), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    # Convert parameters to NEWS scores
    dplyr::mutate(news_spo2 = case_when(
      !! .hypoxic_target == "yes" & 
        !! .o2_rx == "yes"       ~ cut(!! .spo2, right = FALSE,
                                       breaks = c(-Inf, 84, 86, 88, 93, 95, 97, Inf),
                                       labels = c(3, 2, 1, 0, 1, 2, 3)),
      !! .hypoxic_target == "yes" & 
        !! .o2_rx == "no"        ~ cut(!! .spo2, right = FALSE,
                                       breaks = c(-Inf, 84, 86, 88, Inf),
                                       labels = c(3, 2, 1, 0)),
      TRUE                       ~ cut(!! .spo2, right = FALSE,
                                       breaks = c(-Inf, 92, 94, 96, Inf),
                                       labels = c(3, 2, 1, 0))
    ),
    news_alt_conscious = case_when(
      !! .alt_conscious %in% 
        c("1", "yes")                 ~ 3,
      !! .alt_conscious %in%
        c("0", "no")                  ~ 0,
      TRUE                            ~ NA_real_),
    news_o2_rx = case_when(
      !! .o2_rx %in% 
        c("1", "yes")                 ~ 2,
      !! .o2_rx %in%
        c("0", "no")                  ~ 0,
      TRUE                            ~ NA_real_),
    
    news_temp                         = cut(!! .temp, breaks = c(-Inf, 35, 36, 38, 39, Inf),
                                            labels = c(3,1,0,1,2)),
    news_sbp                          = cut(!! .sbp, breaks = c(-Inf, 90, 100, 110, 219, Inf),
                                            labels = c(3,2,1,0,3)),
    news_hr                           = cut(!! .hr, breaks = c(-Inf, 40, 50, 90, 110, 130, Inf),
                                            labels = c(3,1,0,1,2,3)),
    
    # That RR 8 to 11 has very few patients, like 2. 
    # news_rr                           = cut(!! .rr, breaks = c(-Inf, 8, 11, 20, 24, Inf),
    #                                         labels = c(3, 1, 0, 2, 3))) %>% 
    
    news_rr                           = cut(!! .rr, breaks = c(-Inf, 8, 20, 24, Inf),
                                            labels = c(3, 0, 2, 3))) %>% 
    
    dplyr::mutate_at(vars(news_spo2, news_temp, news_sbp, news_hr, news_rr), 
                     ~ as.character(.) %>% as.numeric()) %>% 
    
    # total of all "news" columns
    mutate(news = rowSums(dplyr::select(., dplyr::starts_with("news_")),
                          na.rm = na_to_zeros)) %>% 
    {if(all_na_rm){
      dplyr::mutate(., news = dplyr::if_else(is.na(news_alt_conscious) &
                                               is.na(news_temp) &
                                               is.na(news_sbp) & 
                                               is.na(news_hr) &
                                               is.na(news_rr), NA_real_, news))
    } else {
      .
    }} 
  if(output == "vector"){
    out %>% 
      pull(news)
  } else if(output == "components"){
    out %>% 
      select(starts_with("news"))
  } else if(output == "df_vector"){
    out %>% 
      pull(news) %>% 
      bind_cols(.data, news = .)
  } else if(output == "df_components"){
    out
  }
}

# SOFA --------------------------------------------------
sofa <- function(.data, po2, fio2, invasive_vent, plts, bil, gcs, creat, map, 
                 dopgr15, dop5to15, dopless5, 
                 output = c("vector", "components", "df_vector", "df_components"), na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .po2 = enquo(po2)
  .fio2 = enquo(fio2)
  .invasive_vent = enquo(invasive_vent)
  .plts = enquo(plts)
  .bil = enquo(bil)
  .gcs = enquo(gcs)
  .creat = enquo(creat)
  .map = enquo(map)
  .dopgr15 = enquo(dopgr15)
  .dop5to15 = enquo(dop5to15)
  .dopless5 = enquo(dopless5)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .po2, !! .fio2, !! .plts, !! .bil, !! .gcs, !! .creat, !! .map), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .invasive_vent, !! .dopgr15, !! .dop5to15, !! .dopless5), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # PaO2/FiO2 ratio
      .pf_ratio = (!! .po2*7.5)/!! .fio2,
      
      # Respiration SOFA score
      sofa_respi = case_when(
        .pf_ratio < 100 &
          !! .invasive_vent == "YES" ~ 4, # and ventilated
        .pf_ratio < 200 &
          !! .invasive_vent == "YES" ~ 3, # and ventilated
        .pf_ratio < 300 ~ 2,
        .pf_ratio < 400 ~ 1,
        .pf_ratio >= 400 ~ 0,
        TRUE ~ NA_real_),
      
      # Coagulation  SOFA score
      sofa_coagu = case_when(
        !! .plts < 20 ~ 4,
        !! .plts < 50 ~ 3,
        !! .plts < 100 ~ 2,
        !! .plts < 150 ~ 1,
        !! .plts >= 150 ~ 0,
        TRUE ~ NA_real_),
      
      # Liver  SOFA score
      sofa_liver = case_when(
        !! .bil < 20 ~ 0,
        !! .bil < 33 ~ 1,
        !! .bil < 102 ~ 2,
        !! .bil < 204 ~ 3,
        !! .bil >= 204 ~ 4,
        TRUE ~ NA_real_),
      
      # Central nervous system SOFA score
      sofa_nervo = case_when(
        !! .gcs < 6 ~ 4,
        !! .gcs  < 9 ~ 3,
        !! .gcs  < 12  ~ 2,
        !! .gcs  < 14  ~ 1,
        !! .gcs  >= 14 ~ 0,
        TRUE ~ NA_real_),
      
      # Renal  SOFA score
      sofa_renal = case_when(
        !! .creat < 110 ~ 0,
        !! .creat < 171 ~ 1,
        !! .creat < 300 ~ 2,
        !! .creat < 441 ~ 3,
        !! .creat >= 441 ~ 4,
        TRUE ~ NA_real_),
      
      sofa_cardi = case_when(
        !! .dopgr15 == "yes" ~ 4,      
        !! .dop5to15 == "yes" ~ 3,      
        !! .dopless5 == "yes" ~ 2,
        !! .map < 70 ~ 1,
        !! .map >= 70 ~ 0,
        TRUE ~ NA_real_
      )
    ) %>% 
    
    # total of all "news" columns
    mutate(sofa = rowSums(dplyr::select(., dplyr::starts_with("sofa_")),
                          na.rm = na_to_zeros)) %>% 
    {if(all_na_rm){
      dplyr::mutate(., sofa = dplyr::if_else(
        is.na(sofa_respi) &
          is.na(sofa_coagu) &
          is.na(sofa_liver) &
          is.na(sofa_nervo) &
          is.na(sofa_renal) &
          is.na(sofa_cardi), NA_real_, sofa))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(sofa)
  } else if(output == "components"){
    out %>% 
      select(starts_with("sofa"))
  } else if(output == "df_vector"){
    out %>% 
      pull(sofa) %>% 
      bind_cols(.data, sofa = .)
  } else if(output == "df_components"){
    out
  }
}

# example = dplyr::bind_cols(po2 = sample(c(8:20, c(NA, NA, NA, NA)), 200, replace = T),
#                            fio2 = sample(0.2:0.8, 200, replace = T),
#                            invasive_vent = sample(c("No", "Yes", "No", NA, NA), 200, replace = T),
#                            plts = sample(15:200, 200, replace = T),
#                            bil = sample(10:150, 200, replace = T),
#                            gcs = sample(3:15, 200, replace = T),
#                            creat = sample(60:250, 200, replace = T),
#                            map = sample(40:80, 200, replace = T),
#                            dopgr15 = sample(c("No", "Yes", NA), 200, replace = TRUE),
#                            dop5to15 = sample(c("No", "Yes", NA), 200, replace = TRUE),
#                            dopless5 = sample(c("No", "Yes", NA), 200, replace = TRUE)) %>% 
#   add_row(po2 = NA, fio2 = NA, invasive_vent = NA, plts = NA, 
#           bil= NA, gcs = NA, creat = NA, map = NA, dopgr15 = NA, dop5to15 = NA, dopless5 = NA)
# 
# example %>%                            
#   sofa(po2 = po2, fio2 = fio2, invasive_vent = invasive_vent, plts = plts, 
#        bil= bil, gcs = gcs, creat = creat, map = map, dopgr15 = dopgr15, dop5to15 = dop5to15, dopless5 = dopless5,
#        output = "components", na_to_zeros = FALSE, all_na_rm = FALSE) %>% 
#   tail()

# qsofa -----------------------------------------------------
qsofa <- function(.data, rr, sbp, gcs,
                  output = c("vector", "components", "df_vector", "df_components"), 
                  na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .sbp, !! .gcs), as.numeric) %>% 
    # dplyr::mutate_at(vars(!! .avpu), 
    #                  ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # qSOFA score
      qsofa_rr = case_when(
        !! .rr >=  22 ~ 1, 
        !! .rr < 22 ~ 0,
        TRUE ~ NA_real_),
      
      qsofa_sbp = case_when(
        !! .sbp <=  100 ~ 1, 
        !! .sbp > 100 ~ 0,
        TRUE ~ NA_real_),
      
      qsofa_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      qsofa = rowSums(dplyr::select(., dplyr::starts_with("qsofa_")),
                      na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., qsofa = dplyr::if_else(
        is.na(qsofa_rr) &
          is.na(qsofa_sbp) &
          is.na(qsofa_gcs), NA_real_, qsofa))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(qsofa)
  } else if(output == "components"){
    out %>% 
      select(starts_with("qsofa"))
  } else if(output == "df_vector"){
    out %>% 
      pull(qsofa) %>% 
      bind_cols(.data, qsofa = .)
  } else if(output == "df_components"){
    out
  }
}


# Coronascore-------------------------------------------------------------------
coronascore <- function(.data, rr, spo2, sbp, gcs, card_comrb, resp_cormrb, renal_cormrb, cancer_cormrb,
                        output = c("vector", "components", "df_vector", "df_components"), 
                        na_to_zeros = TRUE, all_na_rm = TRUE){
  
  .rr = enquo(rr)
  .spo2 = enquo(spo2)
  .sbp = enquo(sbp)
  .gcs = enquo(gcs)
  .card_comrb = enquo(card_comrb)
  .resp_comrb = enquo(resp_comrb)
  .renal_comrb = enquo(renal_comrb)
  .cancer_comrb = enquo(cancer_comrb)
  
  out = .data %>% 
    dplyr::mutate_at(vars(!! .rr, !! .spo2, !! .sbp, !! .gcs), as.numeric) %>% 
    dplyr::mutate_at(vars(!! .card_comrb, !! .resp_comrb, !! .renal_comrb, !! .cancer_comrb), 
                     ~ as.character(.) %>% tolower() %>% trimws()) %>% 
    
    mutate(
      # coronascore
      coronascore_rr = case_when(
        !! .rr >=  22 ~ 1, 
        !! .rr < 22 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_spo2 = case_when(
        !! .spo2 >=  92 ~ 1, 
        !! .spo2 < 92 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_sbp = case_when(
        !! .sbp <=  100 ~ 1, 
        !! .sbp > 100 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_gcs = case_when(
        !! .gcs <=  14 ~ 1, 
        !! .gcs > 14 ~ 0,
        TRUE ~ NA_real_),
      
      coronascore_card_comrb = case_when(
        !! .card_comrb == "yes" ~ 1, 
        !! .card_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      coronascore_resp_comrb = case_when(
        !! .resp_comrb == "yes" ~ 1, 
        !! .resp_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      coronascore_renal_comrb = case_when(
        !! .renal_comrb == "yes" ~ 1, 
        !! .renal_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
      coronascore_cancer_comrb = case_when(
        !! .cancer_comrb == "yes" ~ 1, 
        !! .cancer_comrb == "no" ~ 0, 
        TRUE ~ NA_real_),
      
    ) %>% 
    mutate(
      coronascore = rowSums(dplyr::select(., dplyr::starts_with("coronascore_")),
                            na.rm = na_to_zeros)
    ) %>% 
    
    {if(all_na_rm){
      dplyr::mutate(., coronascore = dplyr::if_else(
        is.na(coronascore_rr) &
          is.na(coronascore_spo2) &
          is.na(coronascore_sbp) &
          is.na(coronascore_gcs) &
          is.na(coronascore_coronascore_card_comrb) &
          is.na(coronascore_coronascore_resp_comrb) &
          is.na(coronascore_coronascore_renal_comrb) &
          is.na(coronascore_coronascore_cancer_comrb), NA_real_, coronascore))
    } else {
      .
    }}
  
  if(output == "vector"){
    out %>% 
      pull(coronascore)
  } else if(output == "components"){
    out %>% 
      select(starts_with("coronascore"))
  } else if(output == "df_vector"){
    out %>% 
      pull(coronascore) %>% 
      bind_cols(.data, coronascore = .)
  } else if(output == "df_components"){
    out
  }
}


# Get ggplot axis limits out for labels -------------------------------------------------------
get_ymax <- function(plot) {
  gb = ggplot_build(plot)
  # xmin = gb$layout$panel_params[[1]]$x.range[1]
  # xmax = gb$layout$panel_params[[1]]$x.range[2]
  # ymin = gb$layout$panel_params[[1]]$y.range[1]
  ymax = gb$layout$panel_params[[1]]$y.range[2]
  # list(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax)
  return(ymax)
}
# Table defaults ---------------------------------------------------------------------------
# This makes table resize or continue over multiple pages in all output types
# PDF powered by kableExtra, Word by flextable
mytable = function(x, caption = "", longtable = TRUE, ...){
  
  # if not latex or html then else is Word
  if (is_latex_output() | is_html_output()){
    knitr::kable(x, row.names = FALSE, align = c("l", "l", "r", "r", "r", "r", "r", "r", "r"), 
                 booktabs = TRUE, caption = caption, longtable = longtable,
                 linesep = "", ...) %>%
      kableExtra::kable_styling(latex_options = c("scale_down", "hold_position"))
  }else{
    flextable::flextable(x) %>% 
      flextable::autofit() %>% 
      flextable::width(j = 1, width = 1.5) %>% 
      flextable::height(i = 1, height = 0.5, part = "header")
  }
}