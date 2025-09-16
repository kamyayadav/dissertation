# Author: Kamya Yadav
# Date Created: August 28, 2025 
# Date Modified: September 15, 2025
# Conjoint Pre-Analysis Script

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

# ggsave('../results/figures/promotion.png', plot = promotion_fig, units = 'in', width = 6, height = 6)

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

# ggsave('../results/figures/quality.png', plot = quality_fig, units = 'in', width = 6, height = 6)

# modelsummary(list('Perceived Quality' = quality),
#              coef_rename = coef_map,
#              stars = TRUE,
#              gof_omit = 'BIC|AIC',
#              # output = 'results/tables/quality.tex',
#              align = 'lc')

# outcome 3: individual roles 

roles = c('voter_reg', 'election_incharge', 'main_leader', 'morcha_leader', 'elec_candidate')
new_term_names = c('Female', 'SC', 'OBC', 'In-Group', 'MP: Medium', 'MP: High',
                    'CC: Medium', 'CC: High', 'Yes', 'Lifelong', 
                    'Main Party Secretary', 'Morcha President', 
                    'NA: Medium', 'NA: High')

reference_terms = data.frame(
  term = c('Gender: Male', 'Caste: General', 'Mobilization Power: Low', 
           'Campaign Contribution: Low', 'Delivers Constituency Services: No',
           'Party Membership: Multiple', 'Highest Post: None', 'Networking Ability: Low'),
  estimate = 0, std.error = 0, statistic = 0, p.value = 0, 
  conf.low = 0, conf.high = 0, df = 0
)

factor_levels = c(
  'Gender: Male', 'Female',
  'Caste: General', 'SC', 'OBC', 'In-Group',
  'Mobilization Power: Low', 'MP: Medium', 'MP: High',
  'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
  'Delivers Constituency Services: No', 'Yes',
  'Party Membership: Multiple', 'Lifelong',
  'Highest Post: None', 'Main Party Secretary', 'Morcha President',
  'Networking Ability: Low', 'NA: Medium', 'NA: High'
)

results_list = list()
model_list = list()

for (role in roles) {
  model = lm_robust(
    formula = as.formula(paste(role, '~', conjoint_rhs)),
    data = pap_attributes,
    clusters = caseid,
    se_type = 'CR2',
    fixed_effects = ~ enumerator
  )
  
  model_list[[role]] = model
  
  tidy_model = tidy(model)
  tidy_model$outcome = role
  tidy_model$term = new_term_names
  
  reference_df = reference_terms
  reference_df$outcome = role
  
  results_list[[role]] = bind_rows(tidy_model, reference_df)
}

all_roles_tidy = bind_rows(results_list)

all_roles_tidy$term = factor(all_roles_tidy$term, levels = factor_levels)
roles_clean = c('Voter Registration', 'Election In-Charge', 'Main Party Leader', 'Party Morcha Leader', 'Electoral Candidate')
for (role in roles_clean) {
  
  plot_data = all_roles_tidy %>%
    mutate(outcome = case_when(
      outcome == 'voter_reg' ~ 'Voter Registration',
      outcome == 'election_incharge' ~ 'Election In-Charge',
      outcome == 'main_leader' ~ 'Main Party Leader',
      outcome == 'morcha_leader' ~ 'Party Morcha Leader',
      outcome == 'elec_candidate' ~ 'Electoral Candidate'
    )) %>% 
    filter(outcome == role) 
  
  role_fig = ggplot(data = plot_data, aes(x = estimate, y = fct_rev(term))) +
    geom_point() +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    theme_bw() +
    labs(
      title = paste("Conjoint Analysis Results for:", role),
      x = 'Estimate (Change in Probability)',
      y = 'Attribute'
    )
  
  #ggsave(filename = paste0("../results/figures/", role, ".png"), plot = role_fig, width = 8, height = 6)
}

names(model_list) = roles_clean

modelsummary(
  model_list,
  coef_rename = coef_map,
  stars = TRUE,
  gof_omit = 'BIC|AIC'
 #title = 'Effect of Political Attributes on Role Selection',
  #notes = 'Estimates are Average Marginal Component Effects (AMCEs). Robust standard errors clustered by respondent.'
  # output = 'results/tables/all_roles.tex'
)

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

# ggsave('../results/figures/minor_role.png', plot = minor_role_fig, units = 'in', width = 6, height = 6)

# modelsummary(list('Minor Role' = minor_role),
#              coef_rename = coef_map,
#              stars = TRUE,
#              gof_omit = 'BIC|AIC',
#              # output = 'results/tables/minor_role.tex',
#              align = 'lc')

# all main models combined 

all_models = c(
  list('Promotion' = promotion, 'Perceived Quality' = quality), 
  model_list, 
  list('Minor Role' = minor_role)
)
modelsummary(all_models,
             coef_rename = coef_map,
             stars = TRUE,
             gof_omit = 'BIC|AIC',
             # output = 'results/tables/conjoint_table.tex',
             align = 'lcccccccc')

all_roles_tidy = all_roles_tidy %>% 
  mutate(outcome = case_when(
    outcome == 'voter_reg' ~ 'Voter Registration',
    outcome == 'election_incharge' ~ 'Election In-Charge',
    outcome == 'main_leader' ~ 'Main Party Leader',
    outcome == 'morcha_leader' ~ 'Party Morcha Leader',
    outcome == 'elec_candidate' ~ 'Electoral Candidate'
  ))
results = bind_rows(promotion_tidy, quality_tidy, minor_role_tidy, all_roles_tidy)
results$outcome = ifelse(results$outcome == 'chosen', 'Promotion', 
                         ifelse(results$outcome == 'rating', 'Perceived Quality',
                                       ifelse(results$outcome == 'minor_role', 'Minor Role', results$outcome)))
results$outcome = factor(results$outcome, levels = c('Promotion', 'Perceived Quality', 'Voter Registration', 'Election In-Charge', 'Main Party Leader', 'Party Morcha Leader', 'Electoral Candidate', 'Minor Role'))
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
  facet_wrap(~outcome, ncol = 2, nrow = 4)

# ggsave('../results/figures/conjoint_results.png', plot = results_fig, units = 'in', height = 8, width = 10)

# sub group analysis by respondent gender and the candidate profile gender 

outcomes = c(
  'chosen' = 'Promotion',
  'rating' = 'Perceived Quality',
  'minor_role' = 'Minor Role',
  'elec_candidate' = 'Electoral Candidate',
  'voter_reg' = 'Voter Registration',
  'election_incharge' = 'Election In-Charge',
  'main_leader' = 'Main Party Leader',
  'morcha_leader' = 'Party Morcha Leader'
)

gender_resp_results = list()
gender_resp_models = list()

gender_resp_values = unique(pap_attributes$gender_resp)

for (g in gender_resp_values) {
  
  pap_gender = pap_attributes[pap_attributes$gender_resp == g, ]
  gender_label = ifelse(g == 1, 'Female', 'Male')
  
  for (outcome_var in names(outcomes)) {
    
    clean_name = outcomes[outcome_var] 
    
    model = lm_robust(
      formula = as.formula(paste(outcome_var, '~', conjoint_rhs)),
      data = pap_gender,
      clusters = caseid,
      se_type = 'CR2',
      fixed_effects = ~ enumerator
    )
    
    tidy_df = tidy(model)
    tidy_df$outcome = clean_name
    tidy_df$gender_resp = gender_label
    
    # Add the tidy data frame to our list of results
    gender_resp_results[[length(gender_resp_results) + 1]] = tidy_df
    
    # Store the raw model object with a unique name for modelsummary
    model_name = paste0(clean_name, ' (', gender_label, ')')
    gender_resp_models[[model_name]] = model
  }
}

# Bind all tidy results into one big data frame
gender_resp_results = bind_rows(gender_resp_results)

model_order = list()
for (clean_name in unique(outcomes)) {
  model_order[[clean_name]] = list(
    'Female' = gender_resp_models[[paste0(clean_name, ' (Female)')]],
    'Male'   = gender_resp_models[[paste0(clean_name, ' (Male)')]]
  )
}

# Generate the summary table
modelsummary(
  model_order,
  coef_rename = coef_map,
  stars = TRUE,
  gof_omit = 'BIC|AIC',
  shape = 'cbind',
  title = 'Conjoint Analysis Results by Respondent Gender'
  # output = 'results/tables/conjoint_table_by_gender.tex',
)

new_term_names_hte = c('Female', 'SC', 'OBC', 'In-Group', 'MP: Medium', 'MP: High', 
                    'CC: Medium', 'CC: High', 'Yes', 'Lifelong', 'Main Party Secretary', 
                    'Morcha President', 'NA: Medium', 'NA: High')

n_models = length(outcomes) * length(gender_resp_values)
gender_resp_results$term = rep(new_term_names_hte, n_models)

n_outcomes = length(outcomes)
n_genders = length(gender_resp_values)
n_ref_cats = 8
total_ref_rows = n_ref_cats * n_outcomes * n_genders

reference_terms_hte = c('Gender: Male', 'Caste: General', 'Mobilization Power: Low', 
                     'Campaign Contribution: Low', 'Delivers Constituency Services: No',
                     'Party Membership: Multiple', 'Highest Post: None', 'Networking Ability: Low')

reference_gender_resp = data.frame(
  term = rep(reference_terms_hte, n_models),
  estimate = 0, std.error = 0, statistic = 0, p.value = 0, conf.low = 0, conf.high = 0, df = 0,
  outcome = rep(unique(outcomes), each = n_ref_cats * n_genders),
  gender_resp = rep(rep(c('Female', 'Male'), each = n_ref_cats), n_outcomes)
)

# Combine results with reference rows
gender_resp_results = bind_rows(gender_resp_results, reference_gender_resp)

# Set factor levels for plotting
factor_levels = c('Gender: Male', 'Female', 'Caste: General', 'SC', 'OBC', 'In-Group',
                   'Mobilization Power: Low', 'MP: Medium', 'MP: High',
                   'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                   'Delivers Constituency Services: No', 'Yes', 'Party Membership: Multiple', 'Lifelong',
                   'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                   'Networking Ability: Low', 'NA: Medium', 'NA: High')

gender_resp_results$term = factor(gender_resp_results$term, levels = factor_levels)
gender_resp_results$outcome = factor(gender_resp_results$outcome, levels = unique(outcomes))

gender_resp_fig = ggplot(gender_resp_results) + 
  aes(x = estimate, y = fct_rev(term), color = gender_resp) + 
  geom_point(aes(shape = gender_resp), position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate', y = 'Attribute', color = 'Respondent Gender', shape = 'Respondent Gender') + 
  facet_wrap(~outcome, ncol = 2, nrow = 4)

# ggsave('../results/figures/conjoint_by_gender.png', plot = gender_resp_fig, units = 'in', width = 10, height = 10)

# analysis akin to AICE with gender 

conjoint_rhs_cand = c("caste_SC", "caste_OBC", "caste_ingroup", "mob_power_Medium", "mob_power_High", 
                       "campaign_con_Medium", "campaign_con_High", "const_service_Can_deliver_services", 
                       "party_mem_One_party", "highest_post_Main_party_secretary", 
                       "highest_post_Morcha_president", "networking_Medium", "networking_High")
conjoint_rhs_cand = paste(conjoint_rhs_cand, collapse = " + ")

gender_cand_results_list = list()
gender_cand_models = list()

gender_cand_values = unique(pap_attributes$gender_Female) 

for (g in gender_cand_values) {
  
  pap_gender = pap_attributes[pap_attributes$gender_Female == g, ]
  gender_label = ifelse(g == 1, 'Female', 'Male')
  
  for (outcome_var in names(outcomes)) {
    
    clean_name = outcomes[outcome_var]
    
    model = lm_robust(
      formula = as.formula(paste(outcome_var, '~', conjoint_rhs_cand)),
      data = pap_gender,
      clusters = caseid,
      se_type = 'CR2',
      fixed_effects = ~ enumerator
    )
    
    tidy_df = tidy(model)
    tidy_df$outcome = clean_name
    tidy_df$gender_cand = gender_label
    
    gender_cand_results_list[[length(gender_cand_results_list) + 1]] = tidy_df
    
    model_name = paste0(clean_name, ' (', gender_label, ')')
    gender_cand_models[[model_name]] = model
  }
}

gender_cand_results = bind_rows(gender_cand_results_list)

model_order_cand = list()
for (clean_name in unique(outcomes)) {
  model_order_cand[[clean_name]] = list(
    'Female' = gender_cand_models[[paste0(clean_name, ' (Female)')]],
    'Male'   = gender_cand_models[[paste0(clean_name, ' (Male)')]]
  )
}

coef_map_cand = c(
  'caste_SC' = 'Caste: SC', 'caste_OBC' = 'Caste: OBC', 'caste_ingroup' = 'Caste: In-Group',
  'mob_power_Medium' = 'Mobilization Power: Medium', 'mob_power_High' = 'Mobilization Power: High',
  'campaign_con_Medium' = 'Campaign Contribution: Medium', 'campaign_con_High' = 'Campaign Contribution: High',
  'const_service_Can_deliver_services' = 'Delivers Constituency Service: Yes',
  'party_mem_One_party' = 'Party Membership: Lifelong',
  'highest_post_Main_party_secretary' = 'Highest Post: Main Party Secretary',
  'highest_post_Morcha_president' = 'Highest Post: Morcha President',
  'networking_Medium' = 'Networking Ability: Medium', 'networking_High' = 'Networking Ability: High'
)

modelsummary(
  model_order_cand,
  coef_rename = coef_map_cand,
  stars = TRUE,
  gof_omit = 'BIC|AIC',
  shape = 'cbind',
  title = 'Conjoint Analysis Results by Candidate Gender',  
  # output = 'results/tables/conjoint_table_aice_gender.tex'
)

new_term_names_cand = c('SC', 'OBC', 'In-Group', 'MP: Medium', 'MP: High', 'CC: Medium', 
                         'CC: High', 'Yes', 'Lifelong', 'Main Party Secretary', 
                         'Morcha President', 'NA: Medium', 'NA: High')

n_models = length(outcomes) * length(gender_cand_values)
gender_cand_results$term = rep(new_term_names_cand, n_models)

n_outcomes = length(outcomes)
n_genders = length(gender_cand_values)
n_ref_cats_cand = 7

reference_terms_cand = c('Caste: General', 'Mobilization Power: Low', 'Campaign Contribution: Low',
                          'Delivers Constituency Services: No', 'Party Membership: Multiple',
                          'Highest Post: None', 'Networking Ability: Low')

reference_cand_resp = data.frame(
  term = rep(reference_terms_cand, n_models),
  estimate = 0, std.error = 0, statistic = 0, p.value = 0, conf.low = 0, conf.high = 0, df = 0,
  outcome = rep(unique(outcomes), each = n_ref_cats_cand * n_genders),
  gender_cand = rep(rep(c('Female', 'Male'), each = n_ref_cats_cand), n_outcomes)
)

gender_cand_results = bind_rows(gender_cand_results, reference_cand_resp)

factor_levels_cand = c('Caste: General', 'SC', 'OBC', 'In-Group', 'Mobilization Power: Low', 
                        'MP: Medium', 'MP: High', 'Campaign Contribution: Low', 'CC: Medium', 'CC: High',
                        'Delivers Constituency Services: No', 'Yes', 'Party Membership: Multiple', 'Lifelong',
                        'Highest Post: None', 'Main Party Secretary', 'Morcha President',
                        'Networking Ability: Low', 'NA: Medium', 'NA: High')

gender_cand_results$term = factor(gender_cand_results$term, levels = factor_levels_cand)
gender_cand_results$outcome = factor(gender_cand_results$outcome, levels = unique(outcomes))

gender_cand_fig = ggplot(gender_cand_results) + 
  aes(x = estimate, y = fct_rev(term), color = gender_cand) + 
  geom_point(aes(shape = gender_cand), position = position_dodge(width = 0.6)) + 
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), position = position_dodge(width = 0.6), height = 0) + 
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  scale_color_brewer(palette = "Set1") + 
  theme_bw() + 
  theme(axis.text.y = element_text(lineheight = 1.1),
        strip.background = element_rect(fill="lightblue")) + 
  labs(x = 'Estimate', y = 'Attribute', color = 'Candidate Gender', shape = 'Candidate Gender') + 
  facet_wrap(~outcome, ncol = 2, nrow = 4)

# print(gender_cand_fig)
# ggsave('../results/figures/conjoint_aice_gender.png', plot = gender_cand_fig, units = 'in', width = 10, height = 10)
