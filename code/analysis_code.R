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
  mutate(dq_dp = (`elasticity`*cons)/price)

#quadratic setup
ca_afford <- ca_afford %>%
  mutate(
    a = dq_dp*(-1), # a = dQ/dP = elasticity * Q / P
    b = cons,  # b = baseline consumption Q
  )

# Setting quadratic calculations
quad_df <- ca_afford %>%
  mutate(ccc_c = res_ccc*util_pct*(-1)) %>%      # the budget allocation from ccc - $1.2 billion - divided between the 3 IOUs
  select(utility,price,util_pct, group,ccc_c, a, b)

# setting up budget variations between 0-7.2 billion in 100,000 increments
c_vals <- round(seq(0,7200000000, by = 1000000), 4) 

#quadratic dataframe
curve_df <- quad_df %>%
  rowwise() %>%
  mutate(data = list(tibble(
    c = c_vals, # all possible budget variations; c = -budget
    
    # Solve: a*s^2 + b*s + c = 0
    s = ((-1)*b + sqrt( b^2 - 4*a*c*(-1))) / (2*a), # solving for s using quadratic solution
    s_pct = ((-1)*s) / price, 
    
    s_ccc_pos =  ((-1)*b + sqrt( b^2 - 4*a* (res_ccc*util_pct*(-1)))) / (2*a) , # calculating ccc impact on retail price
    pct_price_ch_ccc = (-1)*s_ccc_pos/price,
    
    
    smc_s = smc-price,                     # s needed to reach smc
    smc_pct  = smc_s / price,              # percentage change
    smc_s_round = round(smc_s,4),
    smc_pct_round = round(smc_pct,4),
    
    # smc_2 = price - smc,
    # smc_c = a*(smc_2^2) + b*smc_2 ,
    
    
    s2 = round(s,4),                       # rounding to 4 digits
    s2_pct = ((-1)*s2) / price,            # calculating change in price in percentage from subsidy
    s2_pct_round = round(s2_pct,4),
    
    c_round = round(c, 1),
    c_2 = b^2 - 4*a*c_round,
    
  ))) %>%
  unnest(cols = c(data)) %>%
  ungroup()

# to control for precision issues - finding the nearest value to smc 
curve_df <- curve_df %>%
  group_by(utility, group) %>%
  mutate(
    diff_from_smc = abs(s2_pct_round - smc_pct_round),
    is_closest = if_else(diff_from_smc == min(diff_from_smc, na.rm = TRUE) & diff_from_smc <= 0.01, TRUE, NA)
  ) %>%
  ungroup()

# curve_df <- curve_df %>%
#   group_by(utility, group) %>%
#   mutate(
#     diff_from_smc2 = abs(s2_pct_round - smc_pct_round),
#     is_closest2 = if_else(diff_from_smc == min(diff_from_smc, na.rm = TRUE) & diff_from_smc <= 0.001, TRUE, NA)
#   ) %>%
#   ungroup()


## Plotting - IOU level

# plot colors
group_colors <- c(
  "all" = "#2b8cbe",      
  "care" = "#fdae61",    
  "summer" = "#a0525c"    
)
# Labels
utility_labels <- c(
  "Pacific Gas and Electric Company" = "PG&E",
  "San Diego Gas & Electric Company" = "SDG&E",
  "Southern California Edison Company" = "SCE"
)

# adjusting place in panel
curve_df <- curve_df %>%
  mutate(utility = factor(
    utility,
    levels = c(
      "Pacific Gas and Electric Company",
      "Southern California Edison Company",
      "San Diego Gas & Electric Company"
    )
  ))

# Plot - IOU level budget allocation
ggplot(curve_df %>% filter(s2_pct_round>=-0.7),
       aes(x = c/1e9, y = s2_pct_round*100, color = group)) +
  geom_line() +
  #facet_wrap(~ utility, scales = "free") +
  labs(x = " ", y = "", title = "Quadratic Curve by Utility") +
  theme_minimal() +
  theme_bw(base_size = 14) +
  
  #wrap for all utilities 
  facet_wrap(~ utility,scales = "free_x", ncol = 1,labeller = labeller(utility = utility_labels)) +
  labs(
    title = "Affordability Outcomes Under Different Budget Levels",
    x = "Budget Allocation (billion $)",
    y = "Percentage Change in Retiail Electricity Prices",
    color = "Group"
  ) +
  
  geom_vline(aes(xintercept = -ccc_c/1e9),
             linetype = "dashed",
             color = "grey50",
             linewidth = 1) +
  annotate(
    "text",
    x = 0.25,
    y = -60,
    label = "Residential \nClimate Credit",
    hjust = -0,
    vjust = 0,
    color = "black",
    fontface = "bold",
    size = 2.5
  ) +
  
  geom_point(
    data = curve_df %>% filter(is_closest),
    aes(x = c / 1e9, y = s2_pct_round * 100, color = "SMC Point", fill = "SMC Point"),
    shape = 21,
    size = 2,
    stroke = 1
  ) +
  
  scale_color_manual(
    values = c(
      "SMC Point" = "#c23517",
      "all" = "#2b8cbe",
      "care" = "#fdae61",
      "summer" = "#a0525c"
    ),
    labels = c(
      "all" = "All Customers",
      "care" = "CARE Customers",
      "summer" = "All Customers (Summer months)",
      "SMC Point" = "Social Marginal Cost"
    )) +
  
  scale_fill_manual(
    values = c("SMC Point" = "#c23517"),
    guide = "none"  # hide separate fill legend if not needed
  ) +
  
  
  theme(
    text = element_text(face = "bold"),  
    plot.title = element_text(face = "bold", hjust = 0.5),      # title
    axis.title = element_text(face = "bold"),      # x and y axis titles
    axis.text = element_text(face = "bold"),       # tick labels
    strip.text = element_text(face = "bold"),      # facet labels
    legend.text = element_text(face = "bold",size = 9),     # legend labels
    legend.title = element_text(face = "bold", size = 9),    # legend title (if shown)
    panel.grid = element_blank(),
    panel.border = element_blank(),
    strip.background = element_blank(),
    axis.line = element_line(color = "black"),
    legend.position = "bottom",
    legend.key = element_blank(),
    legend.background = element_blank(),
    panel.grid.major = element_line(color = "gray85", linewidth = 0.2),
    panel.grid.minor = element_line(color = "gray85", linewidth = 0.2)
  )

ggsave("plots/affordability_iou.png", width = 8, height = 10, dpi = 300)
ggsave("plots/affordability_iou.pdf", width = 8, height = 10, dpi = 300)

# save df for shiny app
write_csv(curve_df,"data/curve_df.csv")


# Tables:
# C&T Program spending to price reduction
quad_df_table <- ca_afford %>%
  mutate(ccc_c = res_ccc*util_pct*(-1)) %>%      # the budget allocation from ccc - $1.2 billion - divided between the 3 IOUs
  select(utility,price,util_pct, group,ccc_c, a, b) %>%
  left_join(ca_aff_prep, by = c("utility")) 
{
quad_df_table <- quad_df_table %>%
  mutate(
    # calculating auction programs impact on retail price
    # CCC
    
    s_ccc_pos =  ((-1)*b + sqrt( b^2 - 4*a* (res_ccc*util_pct*(-1)))) / (2*a) , 
    pct_price_ch_ccc = (-1)*s_ccc_pos/price,
    
    #CEEE
    s_ceee = ((-1)*b + sqrt( b^2 - 4*a* (ceee*util_pct*(-1)))) / (2*a) , 
    pct_price_ceee = (-1)*s_ceee/price,
  
    #EITE Industrial
    s_ind_assist = ((-1)*b + sqrt( b^2 - 4*a* (ind_assist*util_pct*(-1)))) / (2*a) , 
    pct_price_ind_assist = (-1)*s_ind_assist/price,
    
    #Small Business CCC
    s_small_bus_cc = ((-1)*b + sqrt( b^2 - 4*a* (small_bus_cc*util_pct*(-1)))) / (2*a) , 
    pct_price_small_bus_cc = (-1)*s_small_bus_cc/price,  
    
    #Greenhouse Gas Reduction Fund
    s_ggrf = ((-1)*b + sqrt( b^2 - 4*a* (ggrf*util_pct*(-1)))) / (2*a) , 
    pct_price_ggrf = (-1)*s_ggrf/price,   

    # Estimate Industry allowances
    s_industry_allow = ((-1)*b + sqrt( b^2 - 4*a* (industry_allow*util_pct*(-1)))) / (2*a) , 
    pct_price_industry_allow = (-1)*s_industry_allow/price,   
    
    # Natural Gas Proceed
    s_natgas = ((-1)*b + sqrt( b^2 - 4*a* (natgas_tot_proceeds_2023*util_pct*(-1)))) / (2*a) , 
    pct_price_natgas = (-1)*s_natgas/price,   
    
    # # smc
    # smc_s = price - smc,                     # s needed to reach smc
    # smc_pct  = smc_s / price,    
    )
  }


# Table 1
quad_df_table_summary <- ca_afford %>%
  distinct(utility, group, price, hh, cons) %>% view()

# Table 2
quad_df_table %>%
  select(utility, group,price,pct_price_ch_ccc ) %>%
  view()

# Table 3
# C&T Program allocation per IOU
quad_df_table2 <- ca_afford %>%
  left_join(ca_aff_prep, by = c("utility")) %>%
  select(utility,price,util_pct, group,natgas_tot_proceeds_2023) %>%
  mutate(
    ccc_c = res_ccc*util_pct,
    ceee_c = ceee*util_pct,
    ind_assist_c = ind_assist*util_pct,
    small_bus_cc_c = small_bus_cc*util_pct,
    ggrf_c = ggrf*util_pct,    
    natgas_tot_proceeds_2023_c = natgas_tot_proceeds_2023*util_pct,  
    industry_allowance_c = industry_allow*util_pct
  )

quad_df_table2 %>%
  distinct(utility,ccc_c,natgas_tot_proceeds_2023_c,ceee_c,small_bus_cc_c,ind_assist_c,ggrf_c, industry_allowance_c) %>%
  view()
  



