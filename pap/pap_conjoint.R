# Author: Kamya Yadav
# Date Created: August 28, 2025 
# Dissertation Pre-Analysis Script

# SET UP ####################################################################################################

# load packages 

library(tidyverse)
library(haven) 
library(readxl)
library(writexl)
library(estimatr)
library(stargazer)
library(modelsummary)
library(readr)
library(labelled)
library(irr)
library(mice)
library(broom)
library(purrr)
library(forcats)
library(xtable)
library(ggh4x)
library(psych)
library(fastDummies)

# set working directory 
setwd(dirname(rstudioapi::getActiveDocumentContext()$path)) 

# load pap data 
pap = read_excel('../pilot/pap_data.xlsx')

# create unique caseid for every individual 
pap$caseid = c(1:nrow(pap))

# subset to caseid and conjoint relevant columns for ward and district leader selection (kept in the main survey)

pap_conjoint = pap %>% 
  select(caseid, starts_with('set1_')) %>% 
  select(caseid, contains('district'), contains('mandal'), contains('ward'), -contains('_count'), -contains('labels')) %>% 
  select(caseid, contains('chosen_candidate'), contains('rating'), contains('role'))

pap_cleaned = pap_conjoint %>% 
  mutate(
    task1_attributes = coalesce(set1_chosen_candidate_district_1, set1_chosen_candidate_mandal_1),
    task2_attributes = coalesce(set1_chosen_candidate_district_2, set1_chosen_candidate_mandal_2),
    task3_attributes = coalesce(set1_chosen_candidate_district_3, set1_chosen_candidate_mandal_3),
    task4_attributes = coalesce(set1_chosen_candidate_district_4, set1_chosen_candidate_mandal_4),
    pol_level = case_when(
      !is.na(set1_chosen_candidate_district_1) | !is.na(set1_chosen_candidate_district_2) | !is.na(set1_chosen_candidate_district_3) | !is.na(set1_chosen_candidate_district_4) ~ "district",
      !is.na(set1_chosen_candidate_mandal_1) | !is.na(set1_chosen_candidate_mandal_2) | !is.na(set1_chosen_candidate_mandal_3) | !is.na(set1_chosen_candidate_mandal_4) ~ "mandal",
      TRUE ~ NA_character_ # Assign NA if no candidate is chosen
    )
  ) %>% 
  mutate(
    task1_chosen = as.numeric(str_extract(task1_attributes, "\\d+$")),
    task2_chosen = as.numeric(str_extract(task2_attributes, "\\d+$")),
    task3_chosen = as.numeric(str_extract(task3_attributes, "\\d+$")),
    task4_chosen = as.numeric(str_extract(task4_attributes, "\\d+$"))
  ) %>% 
  mutate(task1_attributes = str_replace(task1_attributes, "\\|\\d+$", ""),
         task2_attributes = str_replace(task2_attributes, "\\|\\d+$", ""),
         task3_attributes = str_replace(task3_attributes, "\\|\\d+$", ""),
         task4_attributes = str_replace(task4_attributes, "\\|\\d+$", "")) %>% 
  select(-contains('chosen_candidate')) %>% 
  rename_with(~ .x %>%
                str_remove("^set1_") %>%
                str_replace("_1$", "_task1") %>%
                str_replace("_2$", "_task2") %>%
                str_replace("_3$", "_task3") %>%
                str_replace("_4$", "_task4") %>% 
                str_replace("^ward_", "mandal_")
  )

# this code will coalesce the columns that have the same data from the district conjoints and mandal conjoints into one 
pap_coalesced = pap_cleaned

district_cols = names(pap_coalesced) %>% 
  str_subset("district")

for (d_col in district_cols) {
  
  # Initialize variables to hold the mandal and new column names
  m_col = NA
  new_col = NA
  
  # Check which pattern the column name follows and create the corresponding names
  if (str_detect(d_col, "_district")) {
    # Handles cases like 'candidate1_rating_district_task1'
    m_col   = str_replace(d_col, "_district", "_mandal")
    new_col = str_remove(d_col, "_district")
    
  } else if (str_detect(d_col, "district_")) {
    # Handles cases like 'district_role1_task1'
    m_col   = str_replace(d_col, "district_", "mandal_")
    new_col = str_remove(d_col, "district_")
  }
  
  # Check if the corresponding 'mandal' column actually exists
  if (!is.na(m_col) && m_col %in% names(pap_coalesced)) {
    
    # Use mutate with the `:=` operator to create the new coalesced column
    pap_coalesced = pap_coalesced %>%
      mutate(
        "{new_col}" := coalesce(.data[[d_col]], .data[[m_col]])
      )
  }
}
pap_coalesced = pap_coalesced %>%
  select(-contains("district"), -contains("mandal"))

attribute_name_map = c(
  "1" = "gender",
  "2" = "caste",
  "3" = "mob_power", # mobilization power
  "4" = "campaign_con", # electoral campaign contribution
  "5" = "del_const_service", # ability to deliver constituency service
  "6" = "party_mem", # party membership
  "7" = "highest_post", # highest post in the party
  "8" = "networking" # networking ability 
)

# this will pull out all the attributes from the columns that had the task attributes and create separate columns for each attribute
# and create one row for each task, resulting in a dataset that has caseid-task pairs (4 rows per caseid)
pap_task = pap_coalesced %>%
  rename_with(
    ~ str_replace(., "^task(\\d+)_(.*)", "\\2_task\\1"),
    .cols = starts_with("task")
  ) %>%
  pivot_longer(
    cols = matches("_task\\d+$"), 
    names_to = c(".value", "task"), 
    names_pattern = "(.*)_task(\\d+)" 
  ) %>%
  separate_rows(attributes, sep = "\\|") %>%
  filter(attributes != "" & !is.na(attributes)) %>%
  mutate(
    attribute_key = str_extract(attributes, "^\\d+"),
    attribute_name = attribute_name_map[attribute_key],
    cleaned_value = str_remove(attributes, "^\\d+,")
  ) %>%
  pivot_wider(
    names_from = attribute_name,
    values_from = cleaned_value,
    id_cols = -c(attributes, attribute_key)
  ) %>%
  group_by(caseid, task) %>%
  summarise(across(everything(), ~ first(na.omit(.))), .groups = "drop")

# creates binary versions of certain outcomes
pap_candidate = pap_task %>% 
  mutate(candidate1_chosen = ifelse(chosen == 1, 1, 0),
         candidate2_chosen = ifelse(chosen == 2, 1, 0)) %>% 
  rename(voter_reg = role1, 
         election_incharge = role2,
         main_leader = role5,
         morcha_leader = role6,
         elec_candidate = role7) %>% 
  mutate(candidate1_voter_reg = ifelse(voter_reg == 1, 1, 0),
         candidate2_voter_reg = ifelse(voter_reg == 2, 1, 0),
         candidate1_election_incharge = ifelse(election_incharge == 1, 1, 0),
         candidate2_election_incharge = ifelse(election_incharge == 2, 1, 0),
         candidate1_main_leader = ifelse(main_leader == 1, 1, 0),
         candidate2_main_leader = ifelse(main_leader == 2, 1, 0),
         candidate1_morcha_leader = ifelse(morcha_leader == 1, 1, 0),
         candidate2_morcha_leader = ifelse(morcha_leader == 2, 1, 0),
         candidate1_elec_candidate = ifelse(elec_candidate == 1, 1, 0),
         candidate2_elec_candidate = ifelse(elec_candidate == 2, 1, 0)) %>% 
  select(-chosen, -voter_reg, -election_incharge, -main_leader, -morcha_leader, -elec_candidate)

pap_split = pap_candidate
cols_to_split = unname(attribute_name_map)

for (col_name in cols_to_split) {

  if (col_name %in% names(pap_split)) {
    
    new_col_names = c(
      paste0("candidate1_", col_name), 
      paste0("candidate2_", col_name)
    )

    pap_split = pap_split %>%
      separate(
        col = all_of(col_name),     
        into = new_col_names,       
        sep = ",",                  
        remove = TRUE,              
        convert = TRUE              
      )
  }
}

pap_cand_task = pap_split %>% 
  pivot_longer(
    cols = starts_with('candidate'),
    names_to = c('candidate', '.value'),
    names_pattern = "candidate(\\d+)_(.*)"
  )

# replace all the numeric factor values for candidate attributes with strings to convert to dummy variables 

pap_caste_gender = pap %>% 
  select(-SubmissionDate, -starttime, -endtime, -deviceid, -devicephonenum, -device_info, -starts_with('set1'), -contains('conjoint'), -contains('attributes'), -contains('array'), -labels, -instanceID, -formdef_version, -KEY) %>% 
  mutate(caste = case_when(
    caste == 0 ~ 'General',
    caste == 1 ~ 'OBC',
    caste == 2 ~ 'SC',
    caste == 3 ~ 'ST',
    TRUE ~ 'NA' # dont know/prefer not to say
  )) %>% 
  rename(caste_resp = caste,
         gender_resp = gender) %>% 
  select(caseid, duration, enumerator, gender_resp, caste_resp)

pap_attributes = pap_cand_task %>% 
  mutate(gender = ifelse(gender == 1, 'Male', 'Female'),
         gender = factor(gender, levels = c('Male', 'Female')),
    caste = case_when(
    caste == 1 ~ 'General',
    caste == 2 ~ 'SC',
    caste == 3 ~ 'OBC'
  ),
  caste = factor(caste, levels = c('General', 'SC', 'OBC')),
  mob_power = case_when(
   mob_power == 1 ~ 'Low', # 50 person rally
   mob_power == 2 ~ 'Medium', # 250 person rally
   mob_power == 3 ~ 'High' # 400 person rally
  ),
  mob_power = factor(mob_power, levels = c('Low', 'Medium', 'High')),
  campaign_con = case_when(
    campaign_con == 1 ~ 'Low', # < Rs.1 lakh
    campaign_con == 2 ~ 'Medium', # Rs. 1-5 lakh 
    campaign_con == 3 ~ 'High' # Rs. 5-10 lakh
  ),
  campaign_con = factor(campaign_con, levels = c('Low', 'Medium', 'High')),
  const_service = case_when( # pool together the three types of constituency services politician can deliver (fix broken road, collect garbage, and install street light) 
    # and cannot deliver (not fix broken road, not collect garbage, and not install street light)
    del_const_service == 1 | del_const_service == 3 | del_const_service == 5 ~ 'Can_deliver_services',
    del_const_service == 2 | del_const_service == 4 | del_const_service == 6 ~ 'Cannot_deliver_services'
  ),
  const_service = factor(const_service, levels = c('Cannot_deliver_services', 'Can_deliver_services')),
  party_mem = ifelse(party_mem == 1, 'Many_parties', 'One_party'),
  party_mem = factor(party_mem, levels = c('Many_parties', 'One_party')),
  highest_post = case_when(
    highest_post == 1 ~ 'No_post',
    highest_post == 2 ~ 'Main_party_secretary',
    highest_post == 3 ~ 'Morcha_president'
  ),
  highest_post = factor(highest_post, levels = c('No_post', 'Main_party_secretary', 'Morcha_president')),
  networking = case_when(
    networking == 1 ~ 'Low', # invite mandal president
    networking == 2 ~ 'Medium', # invite vidhan sabha legislator 
    networking == 3 ~ 'High' # invite state president 
  ),
  networking = factor(networking, levels = c('Low', 'Medium', 'High'))) %>% 
  left_join(pap_caste_gender, by = 'caseid') %>% 
  mutate(caste_ingroup = ifelse(caste == caste_resp, 1, 0)) %>% 
  dummy_cols(
    select_columns = c(
      "gender", 
      "caste", 
      "mob_power", 
      "campaign_con",
      "const_service",
      "party_mem", 
      "highest_post",
      "networking"
    ),
    remove_first_dummy = T,
    remove_selected_columns = T
  ) %>% 
  select(-del_const_service) %>% 
  mutate(minor_role = case_when(
    (voter_reg == 1 | morcha_leader == 1) & (election_incharge == 0 & main_leader == 0 & elec_candidate == 0) ~ 1,
    voter_reg == 0 & morcha_leader == 0 & election_incharge == 0 & main_leader == 0 & elec_candidate == 0 ~ 1, # no role assigned 
    TRUE ~ 0
  )
  )

# analysis script for conjoint 
## NOTE: add party fixed effects 

conjoint_atts = c("gender_Female", "caste_SC", "caste_OBC", "caste_ingroup", "mob_power_Medium", "mob_power_High", "campaign_con_Medium", "campaign_con_High", "const_service_Can_deliver_services", "party_mem_One_party", "highest_post_Main_party_secretary", "highest_post_Morcha_president", "networking_Medium", "networking_High")
conjoint_rhs = paste(conjoint_atts, collapse = " + ")

# outcome 1: chosen for promotion 

promotion = lm_robust(formula = as.formula(paste('chosen~', conjoint_rhs)),
          data = pap_attributes,
          clusters = caseid,
          se_type = 'CR2',
          fixed_effects = ~ enumerator)
promotion_tidy = tidy(promotion)
promotion_tidy$term = c('Female',
                   'SC', 'OBC', 'In-Group',
                   'MP: Medium', 'MP: High',
                   'CC: Medium', 'CC: High',
                   'Yes',
                   'Lifelong',
                   'Main Party Secretary',
                   'Morcha President',
                   'NA: Medium', 'NA: High')

reference = data.frame(matrix(NA, nrow = 8, ncol = 9))
colnames(reference) = names(promotion_tidy)
reference$term = c('Gender: Male', 
                    'Caste: General',
                    'Mobilization Power: Low',
                    'Campaign Contribution: Low',
                    'Delivers Constituency Services: No',
                    'Party Membership: Multiple',
                    'Highest Post: None',
                    'Networking Ability: Low')
reference$estimate = 0
reference$std.error = 0
reference$statistic = 0
reference$p.value = 0
reference$conf.low = 0
reference$conf.high = 0
reference$df = 0
reference$outcome = rep('chosen', 8)
promotion_tidy = bind_rows(promotion_tidy, reference)
promotion_tidy$term = factor(promotion_tidy$term, levels = c('Gender: Male', 
                                                   'Female',
                                                   'Caste: General', 'SC', 'OBC', 'In-Group',
                                                   'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                                                   'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                                                   'Delivers Constituency Services: No', 'Yes',
                                                   'Party Membership: Multiple', 'Lifelong',
                                                   'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                                                   'Networking Ability: Low', 'NA: Medium', 'NA: High'))

promotion_fig = promotion_tidy %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute')

# ggsave('results/figures/promotion.png', plot = promotion_fig, units = 'in', width = 6, height = 6)

coef_map = c('Female',
             'Caste: SC', 'Caste: OBC', 'Caste: In-Group',
             'Mobilization Power: Medium', 'Mobilization Power: High',
             'Campaign Contribution: Medium', 'Campaign Contribution: High',
             'Delivers Constituency Service: Yes',
             'Party Membership: Lifelong',
             'Highest Post: Main Party Secretary',
             'Highest Post: Morcha President',
             'Networking Ability: Medium', 'Networking Ability: High')

# modelsummary(list('Promotion' = promotion),
#              coef_rename = coef_map,
#              stars = TRUE,
#              gof_omit = 'BIC|AIC',
#             # output = 'results/tables/promotion.tex',
#              align = 'lc')

# outcome 2: rating (perceived quality)

quality = lm_robust(formula = as.formula(paste('rating~', conjoint_rhs)),
                      data = pap_attributes,
                      clusters = caseid,
                      se_type = 'CR2',
                      fixed_effects = ~ enumerator)
quality_tidy = tidy(quality)
quality_tidy$term = c('Female',
                        'SC', 'OBC', 'In-Group',
                        'MP: Medium', 'MP: High',
                        'CC: Medium', 'CC: High',
                        'Yes',
                        'Lifelong',
                        'Main Party Secretary',
                        'Morcha President',
                        'NA: Medium', 'NA: High')

reference_quality = data.frame(matrix(NA, nrow = 8, ncol = 9))
colnames(reference_quality) = names(quality_tidy)
reference_quality$term = c('Gender: Male', 
                   'Caste: General',
                   'Mobilization Power: Low',
                   'Campaign Contribution: Low',
                   'Delivers Constituency Services: No',
                   'Party Membership: Multiple',
                   'Highest Post: None',
                   'Networking Ability: Low')
reference_quality$estimate = 0
reference_quality$std.error = 0
reference_quality$statistic = 0
reference_quality$p.value = 0
reference_quality$conf.low = 0
reference_quality$conf.high = 0
reference_quality$df = 0
reference_quality$outcome = rep('rating', 8)
quality_tidy = bind_rows(quality_tidy, reference_quality)
quality_tidy$term = factor(quality_tidy$term, levels = c('Gender: Male', 
                                                             'Female',
                                                             'Caste: General', 'SC', 'OBC', 'In-Group',
                                                             'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                                                             'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                                                             'Delivers Constituency Services: No', 'Yes',
                                                             'Party Membership: Multiple', 'Lifelong',
                                                             'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                                                             'Networking Ability: Low', 'NA: Medium', 'NA: High'))

quality_fig = quality_tidy %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute')

# ggsave('results/figures/quality.png', plot = quality_fig, units = 'in', width = 6, height = 6)

# modelsummary(list('Perceived Quality' = quality),
#              coef_rename = coef_map,
#              stars = TRUE,
#              gof_omit = 'BIC|AIC',
#              # output = 'results/tables/quality.tex',
#              align = 'lc')

# outcome 3: candidacy 

candidacy = lm_robust(formula = as.formula(paste('elec_candidate~', conjoint_rhs)),
                    data = pap_attributes,
                    clusters = caseid,
                    se_type = 'CR2',
                    fixed_effects = ~ enumerator)
candidacy_tidy = tidy(candidacy)
candidacy_tidy$term = c('Female',
                      'SC', 'OBC', 'In-Group',
                      'MP: Medium', 'MP: High',
                      'CC: Medium', 'CC: High',
                      'Yes',
                      'Lifelong',
                      'Main Party Secretary',
                      'Morcha President',
                      'NA: Medium', 'NA: High')

reference_candidacy = data.frame(matrix(NA, nrow = 8, ncol = 9))
colnames(reference_candidacy) = names(candidacy_tidy)
reference_candidacy$term = c('Gender: Male', 
                           'Caste: General',
                           'Mobilization Power: Low',
                           'Campaign Contribution: Low',
                           'Delivers Constituency Services: No',
                           'Party Membership: Multiple',
                           'Highest Post: None',
                           'Networking Ability: Low')
reference_candidacy$estimate = 0
reference_candidacy$std.error = 0
reference_candidacy$statistic = 0
reference_candidacy$p.value = 0
reference_candidacy$conf.low = 0
reference_candidacy$conf.high = 0
reference_candidacy$df = 0
reference_candidacy$outcome = rep('elec_candidate', 8)
candidacy_tidy = bind_rows(candidacy_tidy, reference_candidacy)
candidacy_tidy$term = factor(candidacy_tidy$term, levels = c('Gender: Male', 
                                                         'Female',
                                                         'Caste: General', 'SC', 'OBC', 'In-Group',
                                                         'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                                                         'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                                                         'Delivers Constituency Services: No', 'Yes',
                                                         'Party Membership: Multiple', 'Lifelong',
                                                         'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                                                         'Networking Ability: Low', 'NA: Medium', 'NA: High'))

candidacy_fig = candidacy_tidy %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute')

# ggsave('results/figures/candidacy.png', plot = candidacy_fig, units = 'in', width = 6, height = 6)

# modelsummary(list('Electoral Candidate' = candidacy),
#              coef_rename = coef_map,
#              stars = TRUE,
#              gof_omit = 'BIC|AIC',
#              # output = 'results/tables/candidacy.tex',
#              align = 'lc')

# outcome 4: minor role 

minor_role = lm_robust(formula = as.formula(paste('minor_role~', conjoint_rhs)),
                      data = pap_attributes,
                      clusters = caseid,
                      se_type = 'CR2',
                      fixed_effects = ~ enumerator)
minor_role_tidy = tidy(minor_role)
minor_role_tidy$term = c('Female',
                        'SC', 'OBC', 'In-Group',
                        'MP: Medium', 'MP: High',
                        'CC: Medium', 'CC: High',
                        'Yes',
                        'Lifelong',
                        'Main Party Secretary',
                        'Morcha President',
                        'NA: Medium', 'NA: High')

reference_minor = data.frame(matrix(NA, nrow = 8, ncol = 9))
colnames(reference_minor) = names(minor_role_tidy)
reference_minor$term = c('Gender: Male', 
                             'Caste: General',
                             'Mobilization Power: Low',
                             'Campaign Contribution: Low',
                             'Delivers Constituency Services: No',
                             'Party Membership: Multiple',
                             'Highest Post: None',
                             'Networking Ability: Low')
reference_minor$estimate = 0
reference_minor$std.error = 0
reference_minor$statistic = 0
reference_minor$p.value = 0
reference_minor$conf.low = 0
reference_minor$conf.high = 0
reference_minor$df = 0
reference_minor$outcome = rep('minor_role', 8)
minor_role_tidy = bind_rows(minor_role_tidy, reference_minor)
minor_role_tidy$term = factor(minor_role_tidy$term, levels = c('Gender: Male', 
                                                             'Female',
                                                             'Caste: General', 'SC', 'OBC', 'In-Group',
                                                             'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                                                             'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                                                             'Delivers Constituency Services: No', 'Yes',
                                                             'Party Membership: Multiple', 'Lifelong',
                                                             'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                                                             'Networking Ability: Low', 'NA: Medium', 'NA: High'))

minor_role_fig = minor_role_tidy %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute')

# ggsave('results/figures/minor_role.png', plot = minor_role_fig, units = 'in', width = 6, height = 6)

# modelsummary(list('Minor Role' = minor_role),
#              coef_rename = coef_map,
#              stars = TRUE,
#              gof_omit = 'BIC|AIC',
#              # output = 'results/tables/minor_role.tex',
#              align = 'lc')

# all main models combined 

modelsummary(list('Promotion' = promotion,
                  'Perceived Quality' = quality,
                  'Electoral Candidacy' = candidacy,
                  'Minor Role' = minor_role),
             coef_rename = coef_map,
             stars = TRUE,
             gof_omit = 'BIC|AIC',
             # output = 'results/tables/conjoint_table.tex',
             align = 'lcccc')

results = bind_rows(promotion_tidy, quality_tidy, candidacy_tidy, minor_role_tidy)
results$outcome = ifelse(results$outcome == 'chosen', 'Promotion', 
                         ifelse(results$outcome == 'rating', 'Perceived Quality',
                                ifelse(results$outcome == 'elec_candidate', 'Electoral Candidate',
                                       ifelse(results$outcome == 'minor_role', 'Minor Role', results$outcome))))

results_fig = results %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term)) + 
  geom_point(position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute') + 
  facet_wrap(~outcome)

# ggsave('results/figures/conjoint_results.png', plot = results_fig, units = 'in', height = 8, width = 10)

# sub group analysis by respondent gender and the candidate profile gender 
gender_resp_results = list()
gender_resp_models = list()
gender_resp = unique(pap_attributes$gender_resp)
for(i in gender_resp){
  pap_gender = pap_attributes[pap_attributes$gender_resp == i, ]
  promotion = lm_robust(formula = as.formula(paste('chosen~', conjoint_rhs)),
                        data = pap_gender,
                        clusters = caseid,
                        se_type = 'CR2',
                        fixed_effects = ~ enumerator)
  quality = lm_robust(formula = as.formula(paste('rating~', conjoint_rhs)),
                      data = pap_gender,
                      clusters = caseid,
                      se_type = 'CR2',
                      fixed_effects = ~ enumerator)
  candidacy = lm_robust(formula = as.formula(paste('elec_candidate~', conjoint_rhs)),
                        data = pap_gender,
                        clusters = caseid,
                        se_type = 'CR2',
                        fixed_effects = ~ enumerator)
  minor_role = lm_robust(formula = as.formula(paste('minor_role~', conjoint_rhs)),
                         data = pap_gender,
                         clusters = caseid,
                         se_type = 'CR2',
                         fixed_effects = ~ enumerator)
  df = bind_rows(tidy(promotion), tidy(quality), tidy(candidacy), tidy(minor_role))
  df$gender_resp = i
  gender_resp_results = bind_rows(gender_resp_results, df)
  gender_resp_models[[paste0('Promotion (', i, ')')]]     = promotion
  gender_resp_models[[paste0('Perceived Quality (', i, ')')]] = quality
  gender_resp_models[[paste0('Electoral Candidacy (', i, ')')]]  = candidacy
  gender_resp_models[[paste0('Minor Role (', i, ')')]]    = minor_role
}

model_order = list(
  'Promotion' = list(
    'Female' = gender_resp_models$`Promotion (1`,
    'Male' = gender_resp_models$`Promotion (0`
  ),
  'Perceived Quality' = list(
    'Female' = gender_resp_models$`Perceived Quality (1`,
    'Male' = gender_resp_models$`Perceived Quality (0`
  ),
  'Electoral Candidate' = list(
    'Female' = gender_resp_models$`Electoral Candidacy (1`,
    'Male' = gender_resp_models$`Electoral Candidacy (0`
  ),
  'Minor Role' = list(
    'Female' = gender_resp_models$`Minor Role (1`,
    'Male' = gender_resp_models$`Minor Role (0`
  )
)
modelsummary(model_order,
  coef_rename = coef_map,
  stars = TRUE,
  gof_omit = 'BIC|AIC',
  shape = 'cbind',
  title = 'Conjoint Analysis Results by Respondent Gender', 
  # output = 'results/tables/conjoint_table_by_gender.tex',
  align = 'lcccccccc'
)
gender_resp_results$term = rep(c('Female',
                                 'SC', 'OBC', 'In-Group',
                                 'MP: Medium', 'MP: High',
                                 'CC: Medium', 'CC: High',
                                 'Yes',
                                 'Lifelong',
                                 'Main Party Secretary',
                                 'Morcha President',
                                 'NA: Medium', 'NA: High'), 8)
gender_resp_results$outcome = ifelse(gender_resp_results$outcome == 'chosen', 'Promotion',
                                     ifelse(gender_resp_results$outcome == 'rating', 'Perceived Quality',
                                            ifelse(gender_resp_results$outcome == 'minor_role', 'Minor Role',
                                                   ifelse(gender_resp_results$outcome == 'elec_candidate', 'Electoral Candidate', gender_resp_results$outcome))))
gender_resp_results$gender_resp = ifelse(gender_resp_results$gender_resp == 1, 'Female', 'Male')

reference_gender_resp = data.frame(matrix(NA, nrow = 64, ncol = 10))
colnames(reference_gender_resp) = names(gender_resp_results)
reference_gender_resp$term = rep(c('Gender: Male', 
                         'Caste: General',
                         'Mobilization Power: Low',
                         'Campaign Contribution: Low',
                         'Delivers Constituency Services: No',
                         'Party Membership: Multiple',
                         'Highest Post: None',
                         'Networking Ability: Low'), 8)
reference_gender_resp$estimate = 0
reference_gender_resp$std.error = 0
reference_gender_resp$statistic = 0
reference_gender_resp$p.value = 0
reference_gender_resp$conf.low = 0
reference_gender_resp$conf.high = 0
reference_gender_resp$df = 0
reference_gender_resp$outcome = rep(c(rep('Promotion', 8), rep('Perceived Quality', 8), rep('Minor Role', 8), rep('Electoral Candidate', 8)), 2)
reference_gender_resp$gender_resp = rep(c(rep('Female', 32), rep('Male', 32)))
gender_resp_results = bind_rows(gender_resp_results, reference_gender_resp)
gender_resp_results = gender_resp_results %>% 
  group_by(outcome, gender_resp) %>% 
  mutate(term = factor(term, levels = c('Gender: Male', 
                                        'Female',
                                        'Caste: General', 'SC', 'OBC', 'In-Group',
                                        'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                                        'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                                        'Delivers Constituency Services: No', 'Yes',
                                        'Party Membership: Multiple', 'Lifelong',
                                        'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                                        'Networking Ability: Low', 'NA: Medium', 'NA: High')))

gender_resp_fig = gender_resp_results %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term), color = as.factor(gender_resp)) + 
  geom_point(aes(shape = as.factor(gender_resp)), position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute',
       color = 'Respondent Gender',
       shape = 'Respondent Gender') + 
  facet_wrap(~outcome)

# ggsave('results/figures/conjoint_by_gender.png', plot = gender_resp_fig, units = 'in', width = 10, height = 10)

# analysis akin to AICE with gender 

conjoint_rhs_cand = c("caste_SC", "caste_OBC", "caste_ingroup", "mob_power_Medium", "mob_power_High", "campaign_con_Medium", "campaign_con_High", "const_service_Can_deliver_services", "party_mem_One_party", "highest_post_Main_party_secretary", "highest_post_Morcha_president", "networking_Medium", "networking_High")
conjoint_rhs_cand = paste(conjoint_rhs_cand, collapse = " + ")
gender_cand_results = list()
gender_cand_models = list()
gender_cand = unique(pap_attributes$gender_Female)
for(i in gender_cand){
  pap_gender = pap_attributes[pap_attributes$gender_Female == i, ]
  promotion = lm_robust(formula = as.formula(paste('chosen~', conjoint_rhs_cand)),
                        data = pap_gender,
                        clusters = caseid,
                        se_type = 'CR2',
                        fixed_effects = ~ enumerator)
  quality = lm_robust(formula = as.formula(paste('rating~', conjoint_rhs_cand)),
                      data = pap_gender,
                      clusters = caseid,
                      se_type = 'CR2',
                      fixed_effects = ~ enumerator)
  candidacy = lm_robust(formula = as.formula(paste('elec_candidate~', conjoint_rhs_cand)),
                        data = pap_gender,
                        clusters = caseid,
                        se_type = 'CR2',
                        fixed_effects = ~ enumerator)
  minor_role = lm_robust(formula = as.formula(paste('minor_role~', conjoint_rhs_cand)),
                         data = pap_gender,
                         clusters = caseid,
                         se_type = 'CR2',
                         fixed_effects = ~ enumerator)
  df = bind_rows(tidy(promotion), tidy(quality), tidy(candidacy), tidy(minor_role))
  df$gender_cand = i
  gender_cand_results = bind_rows(gender_cand_results, df)
  gender_cand_models[[paste0('Promotion (', i, ')')]]     = promotion
  gender_cand_models[[paste0('Perceived Quality (', i, ')')]] = quality
  gender_cand_models[[paste0('Electoral Candidacy (', i, ')')]]  = candidacy
  gender_cand_models[[paste0('Minor Role (', i, ')')]]    = minor_role
}
model_order_cand = list(
  'Promotion' = list(
    'Female' = gender_cand_models$`Promotion (1`,
    'Male' = gender_cand_models$`Promotion (0`
  ),
  'Perceived Quality' = list(
    'Female' = gender_cand_models$`Perceived Quality (1`,
    'Male' = gender_cand_models$`Perceived Quality (0`
  ),
  'Electoral Candidate' = list(
    'Female' = gender_cand_models$`Electoral Candidacy (1`,
    'Male' = gender_cand_models$`Electoral Candidacy (0`
  ),
  'Minor Role' = list(
    'Female' = gender_cand_models$`Minor Role (1`,
    'Male' = gender_cand_models$`Minor Role (0`
  )
)
coef_map_cand = c('Caste: SC', 'Caste: OBC', 'Caste: In-Group',
                  'Mobilization Power: Medium', 'Mobilization Power: High',
                  'Campaign Contribution: Medium', 'Campaign Contribution: High',
                  'Delivers Constituency Service: Yes',
                  'Party Membership: Lifelong',
                  'Highest Post: Main Party Secretary',
                  'Highest Post: Morcha President',
                  'Networking Ability: Medium', 'Networking Ability: High')
modelsummary(model_order_cand,
             coef_rename = coef_map_cand,
             stars = TRUE,
             gof_omit = 'BIC|AIC',
             shape = 'cbind',
             title = 'Conjoint Analysis Results by Candidate Gender', 
             # output = 'results/tables/conjoint_table_aice_gender.tex',
             align = 'lcccccccc'
)
gender_cand_results$term = rep(c('SC', 'OBC', 'In-Group',
                                 'MP: Medium', 'MP: High',
                                 'CC: Medium', 'CC: High',
                                 'Yes',
                                 'Lifelong',
                                 'Main Party Secretary',
                                 'Morcha President',
                                 'NA: Medium', 'NA: High'), 8)
gender_cand_results$outcome = ifelse(gender_cand_results$outcome == 'chosen', 'Promotion',
                                     ifelse(gender_cand_results$outcome == 'rating', 'Perceived Quality',
                                            ifelse(gender_cand_results$outcome == 'minor_role', 'Minor Role',
                                                   ifelse(gender_cand_results$outcome == 'elec_candidate', 'Electoral Candidate', gender_cand_results$outcome))))
gender_cand_results$gender_cand = ifelse(gender_cand_results$gender_cand == 1, 'Female', 'Male')

reference_cand_resp = data.frame(matrix(NA, nrow = 56, ncol = 10))
colnames(reference_cand_resp) = names(gender_cand_results)
reference_cand_resp$term = rep(c('Caste: General',
                                   'Mobilization Power: Low',
                                   'Campaign Contribution: Low',
                                   'Delivers Constituency Services: No',
                                   'Party Membership: Multiple',
                                   'Highest Post: None',
                                   'Networking Ability: Low'), 8)
reference_cand_resp$estimate = 0
reference_cand_resp$std.error = 0
reference_cand_resp$statistic = 0
reference_cand_resp$p.value = 0
reference_cand_resp$conf.low = 0
reference_cand_resp$conf.high = 0
reference_cand_resp$df = 0
reference_cand_resp$outcome = rep(c(rep('Promotion', 7), rep('Perceived Quality', 7), rep('Minor Role', 7), rep('Electoral Candidate', 7)), 2)
reference_cand_resp$gender_cand = rep(c(rep('Female', 28), rep('Male', 28)))
gender_cand_results = bind_rows(gender_cand_results, reference_cand_resp)
gender_cand_results = gender_cand_results %>% 
  group_by(outcome, gender_cand) %>% 
  mutate(term = factor(term, levels = c('Caste: General', 'SC', 'OBC', 'In-Group',
                                        'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                                        'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                                        'Delivers Constituency Services: No', 'Yes',
                                        'Party Membership: Multiple', 'Lifelong',
                                        'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                                        'Networking Ability: Low', 'NA: Medium', 'NA: High')))

gender_cand_fig = gender_cand_results %>% 
  ggplot() + 
  aes(x = estimate, y = fct_rev(term), color = as.factor(gender_cand)) + 
  geom_point(aes(shape = as.factor(gender_cand)), position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate',
       y = 'Attribute',
       color = 'Candidate Gender',
       shape = 'Candidate Gender') + 
  facet_wrap(~outcome)

# ggsave('results/figures/conjoint_aice_gender.png', plot = gender_cand_fig, units = 'in', width = 10, height = 10)
