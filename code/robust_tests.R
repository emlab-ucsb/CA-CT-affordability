library(tidyverse)
library(readxl)
library(dplyr)

# Read base data

ca_afford_long <- read_csv("../data/CA_afford_long.csv") #created in raw_data_prep.R

## Assigning values
# Allowance allocations and estimated value
# calculated in raw_data_prep.R
tot_allocations = 70929951
iou_allocations = 48639173
est_alloc_price = 31.34196

# external values
smc = 0.14 # https://calepa.ca.gov/wp-content/uploads/2025/02/2024-ANNUAL-REPORT-OF-THE-IEMAC-final.pdf 

# auction revenues - https://ww2.arb.ca.gov/sites/default/files/cap-and-trade/allowanceallocation/EDU%202023%20Use%20of%20Allowance%20Value%20Report.pdf 
ceee = 51842387          # Clean Energy and Energy Efficiency (CEEE) 
ind_assist = 102957581   # CA Industry Assistance Credit
res_ccc = 1237788821     # Residential CCC
small_bus_cc = 130902978 # Small Business CCC
ggrf = 4013035685.57     # Greenhouse Gas Reduction Fund

# Estimate values of nidustry allowances
# https://ww2.arb.ca.gov/sites/default/files/2022-12/nc-v2023%20Public%20Allocation%20Summary.pdf  
industry_allow = 34614621 * est_alloc_price

## Analysis

# Set up allowance values for the 6-IOU
form_94 <- readxl::read_xlsx("../data/Form94.xlsx", sheet = "Table 9-4", skip = 1)
form_94 <- form_94 %>%
  rename(`utility` = `...1`) %>%
  select(utility, `2023`)%>% 
  filter(utility =="Pacific Gas and Electric Company" |
           utility =="San Diego Gas & Electric Company" |
           utility =="Southern California Edison Company" |
           utility =="Golden State Water Company (Bear Valley Electric Service)" |
           utility =="Liberty Utilities (CalPeco Electric) LLC" |   
           utility =="PacifiCorp"
  )

# Estimating $ value for auction programs and other free allowances
# Total value is divided proportionally for IOUs
iou_prop <- form_94 %>%
  mutate(util_allow_pct = `2023`/iou_allocations) %>%
  mutate(ceee_allocated = util_allow_pct * ceee ) %>%
  mutate(ind_assist_allocated = util_allow_pct * ind_assist ) %>%
  mutate(res_ccc_allocated = util_allow_pct * res_ccc ) %>%
  mutate(small_bus_cc_allocated = util_allow_pct * small_bus_cc ) %>%  
  mutate(industry_allow_allocated = util_allow_pct * industry_allow ) %>%    
  mutate(ggrf_allocated = util_allow_pct * ggrf ) 

# Add in natural gas
ca_aff_prep <- ca_afford_long %>%
  distinct(utility, natgas_tot_proceeds_2023)

iou_prop <- iou_prop %>%
  left_join(ca_aff_prep, by = c("utility"))

# Total corpus of available allocations at utility level
iou_prop <- iou_prop %>%
  mutate(total_corpus = rowSums(across(c(
    ceee_allocated,
    ind_assist_allocated,
    res_ccc_allocated,
    small_bus_cc_allocated,
    industry_allow_allocated,
    ggrf_allocated,
    natgas_tot_proceeds_2023
  )), na.rm = TRUE))

# Final proportion at IOU level
# Dictates the estimated % value the utility would receive from state-wide allocations
iou_prop <- iou_prop %>%
  mutate(total_abs =  sum(total_corpus, na.rm = TRUE)) %>%
  mutate(util_pct = total_corpus / total_abs ) 

#max_exp <- unique(iou_prop$total_abs) # max expenditure = ~$7.2B

# Keep only main vars
iou_prop <- iou_prop %>%
  select(utility, util_pct)

# Combining IOU proportional values with CA_afford for the 3 large IOUs
ca_afford <- iou_prop %>%
  filter(utility =="Pacific Gas and Electric Company" |
           utility =="San Diego Gas & Electric Company" |
           utility =="Southern California Edison Company") %>%
  left_join(ca_afford_long, by = c("utility")) %>%
  select(-natgas_tot_proceeds_2023)

# Retail price impacts
#calculating dq/dp
elasticity = -0.36 # ST elasticity from https://www.haas.berkeley.edu/wp-content/uploads/WP331.pdf 
ca_afford <- ca_afford %>%
  mutate(dq_dp_st = (`elasticity`*cons)/price,
         dq_dp_lt = ((-2.4)*cons)/price
         
         )
#quadratic setup
ca_afford <- ca_afford %>%
  mutate(
    a_st = dq_dp_st*(-1), # a = dQ/dP = elasticity * Q / P
    a_lt = dq_dp_lt*(-1), # a = dQ/dP = elasticity * Q / P
    b = cons,  # b = baseline consumption Q
  )

# Setting quadratic calculations
quad_df <- ca_afford %>%
  mutate(ccc_c = res_ccc*util_pct*(-1)) %>%      # the budget allocation from ccc - $1.2 billion - divided between the 3 IOUs
  select(utility,price,util_pct, group,ccc_c, a_st,a_lt, b)

# setting up budget variations between 0-7.2 billion in 100,000 increments
c_vals <- round(seq(0,7200000000, by = 1000000), 4) 

#quadratic dataframe
curve_df <- quad_df %>%
  rowwise() %>%
  mutate(data = list(tibble(
    c = c_vals, # all possible budget variations; c = -budget
    
    # Solve: a*s^2 + b*s + c = 0 - ST
    s_st = ((-1)*b + sqrt( b^2 - 4*a_st*c*(-1))) / (2*a_st), # solving for s using quadratic solution
    s_st_pct = ((-1)*s_st) / price, 
    
    
    # Solve: a*s^2 + b*s + c = 0 - LT
    s_lt = ((-1)*b + sqrt( b^2 - 4*a_lt*c*(-1))) / (2*a_lt), # solving for s using quadratic solution
    s_lt_pct = ((-1)*s_lt) / price, 
    
    s_ccc_pos =  ((-1)*b + sqrt( b^2 - 4*a_st* (res_ccc*util_pct*(-1)))) / (2*a_st) , # calculating ccc reference
    pct_price_ch_ccc = (-1)*s_ccc_pos/price,
    
    # Solve: a*s^2 + b*s + c = 0 - No Quantity Change
    s_nrdc = c / b ,
    s_nrdc_pct = (-1)*s_nrdc / price,
    
    pct_st_round = round(s_st_pct,4),
    pct_lt_round = round(s_lt_pct,4),
    pct_nrdc_round = round(s_nrdc_pct,4),
    
    smc_s = smc-price,                     # s needed to reach smc
    smc_pct  = smc_s / price,              # percentage change
    smc_s_round = round(smc_s,4),
    smc_pct_round = round(smc_pct,4),
    
    c_round = round(c, 1),
    c_2 = b^2 - 4*a_st*c_round,
    
  ))) %>%
  unnest(cols = c(data)) %>%
  ungroup()

curve_df %>%
  distinct(utility, group,pct_nrdc_round,pct_st_round,pct_lt_round) %>% view()

curve_df %>%
  distinct(utility, group,s_nrdc,s_st,s_lt) %>% view()

# to control for precision issues - finding the nearest value to smc 
curve_df <- curve_df %>%
  group_by(utility, group) %>%
  mutate(
    # ST: Short-Term 
    diff_from_smc_st = abs(pct_st_round - smc_pct_round),
    is_closest_st = if_else(
      diff_from_smc_st == min(diff_from_smc_st, na.rm = TRUE) & diff_from_smc_st <= 0.01,
      TRUE, NA
    ),
    
    # LT: Long-Term 
    diff_from_smc_lt = abs(pct_lt_round - smc_pct_round),
    is_closest_lt = if_else(
      diff_from_smc_lt == min(diff_from_smc_lt, na.rm = TRUE) & diff_from_smc_lt <= 0.01,
      TRUE, NA
    ),
    
    # No Quantity Change
    diff_from_smc_nrdc = abs(pct_nrdc_round - smc_pct_round),
    is_closest_nrdc = if_else(
      diff_from_smc_nrdc == min(diff_from_smc_nrdc, na.rm = TRUE) & diff_from_smc_nrdc <= 0.01,
      TRUE, NA
    )
  ) %>%
  ungroup()


# Convert to long format
curve_long <- curve_df %>%
  pivot_longer(
    cols = c(pct_st_round, pct_lt_round, pct_nrdc_round),
    names_to = "model",
    values_to = "price_change_pct"
  ) %>%
  mutate(
    model = recode(model,
                   "pct_st_round" = "ST",
                   "pct_lt_round" = "LT",
                   "pct_nrdc_round" = "NRDC"
    )
  )



# plot colors
group_colors <- c(
  "all" = "#2b8cbe",      
  "care" = "#fdae61",    
  "summer" = "#a0525c"    
)
# Labels
utility_labels <- c(
  "Pacific Gas and Electric Company" = "PG&E",
  "Southern California Edison Company" = "SCE",
  "San Diego Gas & Electric Company" = "SDG&E"

)

# adjusting place in panel
curve_long <- curve_long %>%
  mutate(utility = factor(
    utility,
    levels = c(
      "Pacific Gas and Electric Company",
      "Southern California Edison Company",
      "San Diego Gas & Electric Company"
    )
  ))


ggplot(curve_long %>% filter(price_change_pct >= -0.7),
       aes(x = c / 1e9, y = price_change_pct * 100, color = group, linetype = model)) +
  
  geom_line(linewidth = 0.8) +
  
  facet_wrap(~ utility, scales = "free_x", ncol = 1,
             labeller = labeller(utility = utility_labels)) +
  
  geom_vline(aes(xintercept = -ccc_c / 1e9),
             linetype = "dashed", color = "grey50", linewidth = 1) +
  
  annotate(
    "text", x = 0.25, y = -60,
    label = "Residential \nClimate Credit",
    hjust = -0, vjust = 0, color = "black",
    fontface = "bold", size = 2.5
  ) +
  
  geom_point(
    data = curve_long %>% filter(is_closest_st == TRUE),
    aes(x = c / 1e9, y = s_st_pct * 100, color = "SMC Point", fill = "SMC Point"),
    shape = 21, size = 2, stroke = 1, inherit.aes = FALSE
  ) +
  geom_point(
    data = curve_long %>% filter(is_closest_lt == TRUE),
    aes(x = c / 1e9, y = s_lt_pct * 100, color = "SMC Point", fill = "SMC Point"),
    shape = 21, size = 2, stroke = 1, inherit.aes = FALSE
  ) +
  geom_point(
    data = curve_long %>% filter(is_closest_nrdc == TRUE),
    aes(x = c / 1e9, y = s_nrdc_pct * 100, color = "SMC Point", fill = "SMC Point"),
    shape = 21, size = 2, stroke = 1, inherit.aes = FALSE
  ) +
  
  
  scale_color_manual(
    values = c(
      "all" = "#2b8cbe",
      "care" = "#fdae61",
      "summer" = "#a0525c",
      "SMC Point" = "#c23517"
    ),
    labels = c(
      "all" = "All Customers",
      "care" = "CARE Customers",
      "summer" = "All Customers (Summer months)",
      "SMC Point" = "Social Marginal Cost"
    )
  ) +
  
  scale_fill_manual(
    values = c("SMC Point" = "#c23517"),
    guide = "none"
  ) +
  
  scale_linetype_manual(
    values = c("ST" = "solid", "LT" = "dotted", "NRDC" = "dashed"),
    labels = c("ST" = "Benchmark", "LT" = "LT Elasticity", "NRDC" = "No Quantity Response")
  )+
  
  labs(
    title = "Affordability Outcomes Under Different Budget Levels",
    x = "Budget Allocation (billion $)",
    y = "Percentage Change in Retail Electricity Prices",
    color = "Group",
    linetype = "Model"
  ) +
  guides(
    color = guide_legend(nrow = 1, order = 1),
    linetype = guide_legend(nrow = 1, order = 2)
  ) +
  theme_minimal() +
  theme_bw(base_size = 14) +
  theme(
    text = element_text(face = "bold"),
    plot.title = element_text(hjust = 0.5),
    axis.title = element_text(face = "bold"),
    axis.text = element_text(face = "bold"),
    strip.text = element_text(face = "bold"),
    legend.text = element_text(face = "bold", size = 9),
    legend.title = element_text(face = "bold", size = 9),
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.background = element_blank(),
    panel.grid = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    axis.line = element_line(color = "black"),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.2),
    panel.grid.minor = element_line(color = "gray85", linewidth = 0.2)
  )

ggsave("plots/affordability_all.png", width = 8, height = 10, dpi = 300)
ggsave("plots/affordability_all.pdf", width = 8, height = 10, dpi = 300)

# Table
# Percentage changes table
quad_df_table <- curve_df %>%
  mutate(
    s_ccc_st =  ((-1)*b + sqrt( b^2 - 4*a_st* (res_ccc*util_pct*(-1)))) / (2*a_st) , # calculating ccc impact on retail price
    s_ccc_lt =  ((-1)*b + sqrt( b^2 - 4*a_lt* (res_ccc*util_pct*(-1)))) / (2*a_lt) , # calculating ccc impact on retail price
    s_ccc_nrdc =  (res_ccc*util_pct) / b, # calculating ccc impact on retail price
    
    pct_st = (-1)*s_ccc_st/price,
    pct_lt = (-1)*s_ccc_lt/price,
    pct_nrdc = (-1)*s_ccc_nrdc/price
    
     ) 

quad_df_table %>% 
  distinct(utility, group,pct_nrdc, pct_st, pct_lt ) %>% view()

