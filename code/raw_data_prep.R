library(tidyverse)
library(readxl)
library(dplyr)
library(here)

# Residential CCC - https://ww2.arb.ca.gov/sites/default/files/cap-and-trade/allowanceallocation/EDU%202023%20Use%20of%20Allowance%20Value%20Report.pdf 
res_ccc = 1237788821          #Residential CCC
tot_auction_rev =  1524447219 #Total Auction Proceeds Expended

#read form 9-4 - https://ww2.arb.ca.gov/sites/default/files/2022-12/nc-v2023%20Public%20Allocation%20Summary.pdf -> https://www.arb.ca.gov/regact/2016/capandtrade16/attach10.xlsx
form_94 <- readxl::read_xlsx("../data/Form94.xlsx", sheet = "Table 9-4", skip = 1)

form_94 <- form_94 %>%
  rename(`utility` = `...1`) %>%
  select(utility, `2023`) %>%
  mutate(total_alloc = sum(`2023`, na.rm = TRUE))

form_94 <- form_94 %>% 
  filter(utility =="Pacific Gas and Electric Company" |
           utility =="San Diego Gas & Electric Company" |
           utility =="Southern California Edison Company" |
           utility =="Golden State Water Company (Bear Valley Electric Service)" |
           utility =="Liberty Utilities (CalPeco Electric) LLC" |   
           utility =="PacifiCorp"
    ) %>%
  mutate(
    big6_alloc = sum(`2023`, na.rm = TRUE),
    est_ccc_value = res_ccc / big6_alloc,       # estimated dollar value of CCC per permit
    est_ccc_received = est_ccc_value * `2023`   # estimated dollar value of CCC disbursed at IOU level
    )

# IOU estimated allocation price
iou_2023 <- form_94 %>%
  filter(utility =="Pacific Gas and Electric Company" |
           utility =="San Diego Gas & Electric Company" |
           utility =="Southern California Edison Company" |
           utility =="Golden State Water Company (Bear Valley Electric Service)" |
           utility =="Liberty Utilities (CalPeco Electric) LLC" |   
           utility =="PacifiCorp") %>%
  select(utility, big6_alloc,total_alloc) 

#Estimating allocation price for free allocations in 2023 as well as iou and total allowances allocations
est_alloc_price <- unique( tot_auction_rev / iou_2023$big6_alloc) # $31
iou_allocations <- unique(iou_2023$big6_alloc)
tot_allocations <- unique(iou_2023$total_alloc)
util_allow_pct <- unique(iou_2023$big6_alloc/iou_2023$total_alloc)

# CA Climate Credits received by hh in 2023 - https://www.cpuc.ca.gov/climatecredit 
ccc_2023 <- data.frame(
  utility = c(
    "Golden State Water Company (Bear Valley Electric Service)",
    "Liberty Utilities (CalPeco Electric) LLC",
    "Pacific Gas and Electric Company",
    "PacifiCorp",
    "San Diego Gas & Electric Company",
    "Southern California Edison Company"
  ),
  ccc_credit_2023 = c(66, 58, 78, 538, 122, 142),
  stringsAsFactors = FALSE
)

# Estimate hhs at utility level 
est_hh <- form_94 %>%
  left_join(ccc_2023, by = "utility") %>%
  mutate(hh_all = est_ccc_received/ccc_credit_2023) %>%
  select(utility, `2023`, hh_all)

#########
# assumes SoCalGas = SCE
# natgas proceed disbursed in 2023 - table 7.9 - https://www.cpuc.ca.gov/-/media/cpuc-website/divisions/office-of-governmental-affairs-division/reports/2024/2023-ab-67-report.pdf 
nat_gas <- data.frame(
  utility = c(
    "Pacific Gas and Electric Company",
    "San Diego Gas & Electric Company",
    "Southern California Edison Company"
  ),
  #natgas_credit_2023 = c(53, 43,51),
  natgas_tot_proceeds_2023 = c(270504888, 48983757, 325061169),
  stringsAsFactors = FALSE
)

#### CARE Values - Monthly & Annual Reports | LIOB https://liob.cpuc.ca.gov/monthly-annual-reports/ - CARE Table 9
# 2022 Consumption values for SCE as 2023 CARE values were unusually high.
care_df <- data.frame(
  utility = c(
    "Pacific Gas and Electric Company",
    "San Diego Gas & Electric Company",
    "Southern California Edison Company"
  ),
  hh_care = c(1402942, 336819, 1289493),
  avg_monthly_bill = c(86.83, 94.99, 78.6),
  avg_monthly_usage = c(469, 354, 530),
  stringsAsFactors = FALSE
)
#price estimates
care_df <- care_df %>%
  mutate(price_care = avg_monthly_bill/avg_monthly_usage) %>%
  mutate(cons_care = avg_monthly_usage*hh_care*12 )

# All customers prices - 2023 bundled system average rate 
# https://www.cpuc.ca.gov/-/media/cpuc-website/divisions/office-of-governmental-affairs-division/reports/2024/2023-ab-67-report.pdf 
price_df <- data.frame(
  utility = c(
    "Pacific Gas and Electric Company",
    "San Diego Gas & Electric Company",
    "Southern California Edison Company"
  ),
  price_all = c(0.313, 0.385, 0.266),
  stringsAsFactors = FALSE
)

# All customers consumption values
# Monthly Electricity Consumption (GWh) by CEC-defined Planning Area and Sector, 2019-2023
# Data aggregated from https://www.energy.ca.gov/data-reports/energy-almanac/california-electricity-data/california-energy-consumption-dashboards-0 
consumption <- read_csv("../data/iou_monthly_cons.csv")
consumption <- consumption %>%
  filter(Sector == "Residential" & Year == 2023) 

# Total consumption values
cons_all <- consumption %>%
  group_by(Year, Planning_area) %>% 
  mutate(cons_all = sum(GWh)) %>%
  mutate(cons_all = cons_all * 1000000) %>%           # KWh
  distinct(Year, Planning_area, .keep_all = TRUE) %>%
  ungroup() %>%
  select(Planning_area, cons_all)

# For summer
cons_summer <- consumption %>%
  filter(Month==7 | Month==8 | Month==9) %>%
  group_by(Year, Planning_area) %>% 
  mutate(cons_summer = sum(GWh)) %>%
  mutate(cons_summer = cons_summer*1000000) %>%
  distinct(Year, Planning_area, .keep_all = TRUE)  %>%
  ungroup() %>%
  select(Planning_area, cons_summer)

#combine into 1 df
cons <- cons_all %>%
  ungroup() %>%
  left_join(cons_summer %>% ungroup(), by = "Planning_area") %>%
  rename( 'utility' = Planning_area) %>%
  mutate(utility = if_else(utility == "PGE","Pacific Gas and Electric Company", utility),
         utility =if_else( utility == "SDGE","San Diego Gas & Electric Company", utility),
         utility =if_else( utility == "SCE","Southern California Edison Company", utility)
  )

##########
## combine all above into single df
raw_data <- inner_join(est_hh,ccc_2023, by = c("utility")) %>%
  inner_join(nat_gas, by = c("utility")) %>%
  inner_join(care_df, by = c("utility")) %>%
  inner_join(price_df, by = c("utility")) %>%
  inner_join(cons, by = c("utility"))

# Select columns
raw_data <- raw_data %>%
  select(utility,`2023`, price_all, price_care, hh_all, hh_care,natgas_tot_proceeds_2023, cons_all, cons_summer, cons_care)

# generating summer value to prep for long conversions
raw_data <- raw_data %>%
  mutate(
    price_summer = price_all,
    hh_summer = hh_all
  )

# long form data
CA_afford_long <- raw_data %>%
  pivot_longer(
    cols = c(price_all, price_care, price_summer,
             cons_all, cons_care, cons_summer,
             hh_all, hh_care, hh_summer),
    names_to = c("variable", "group"),
    names_pattern = "^(.*)_(all|care|summer)$",
    values_to = "value"
  ) %>%
  pivot_wider(
    names_from = variable,
    values_from = value
  )

write_csv(CA_afford_long, "data/CA_afford_long.csv" )


