#install.packages("tidyverse")
#install.packages("arrow)
library(tidyverse)
library(arrow)
data = read_parquet("catB_train.parquet")
attach(data)
name = names(data)
name
set.seed(2024)
dim(data)
summary(data)

data$f_purchase_lh[is.na(data$f_purchase_lh)] = 0
data$f_purchase_lh
#encoding+replacing na values race_desc
race <- race_desc
race[is.na(race)] = "Nil"
chinese <- race == "Chinese"
indian <- race == "Indian"
malay <- race == "Malay"
others <- race == "Others"
sc <- sum(chinese)
si <- sum(indian)
sm <- sum(malay)
so <- sum(others)
sampler <- c(rep(c("Chinese"),sc), rep(c("Indian"), si), rep(c("Malay"), sm), rep(c("Others"), so))


insert_race <- rep("q",nrow(data))
for (i in 1:nrow(data)){
  if(is.na(race_desc)[i]){
    insert_race[i] <- sample(sampler, 1, replace = TRUE)
  }else{
    insert_race[i] <- race_desc[i]
  }
}
data$race_desc <- insert_race
data %>% 
  count(race_desc) %>% 
  ggplot(aes(x = race_desc, y = n))+
  geom_bar(stat = "identity")

race_encode <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  if(data$race_desc[i] == "Chinese"){
    race_encode[i] <- 1
  }else if (data$race_desc[i] == "Indian"){
    race_encode[i] <- 2
  }else if (data$race_desc[i] == "Malay"){
    race_encode[i] <- 3
  }else{
    race_encode[i] <- 4
  }
}
data$race_desc <- race_encode
data$race_desc
race_desc
data$race_desc <- as.factor(data$race_desc)
#ctrycode_desc
data %>% 
  count(ctrycode_desc) %>% 
  ggplot(aes(x = ctrycode_desc, y = n))+
  geom_bar(stat = "identity")
data %>% 
  filter(is.na(ctrycode_desc))
data <- data %>% 
  filter(!is.na(ctrycode_desc))
data %>% 
  filter(is.na(ctrycode_desc) | ctrycode_desc != 1 & f_purchase_lh == 1)
#change values to Singapore and Non singapore as only singaporeans will purchase

insert_ctrycode <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  if (data$ctrycode_desc[i] == "Singapore"){
    insert_ctrycode[i] = 1
  }else{
    insert_ctrycode[i] = 0
  }
}
data$ctrycode_desc <- insert_ctrycode
data$ctrycode_desc
ggplot(data, aes(x = ctrycode_desc, y = f_purchase_lh))+
  geom_point()

data$ctrycode_desc <- as.factor(data$ctrycode_desc)
#clttype
data %>% 
  count(clttype) %>% 
  ggplot(aes(x = clttype, y = n))+
  geom_bar(stat = "identity")

data %>% 
  filter(is.na(clttype))
insert_clttype <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  if (data$clttype[i] == "P"){
    insert_clttype[i] = 0
  }else if(data$clttype[i] == "G"){
    insert_clttype[i] = 1
  }else{
    insert_clttype[i] = 2
  }
}
data$clttype <- insert_clttype
data$clttype <- as.factor(data$clttype)
#stat_flag
data %>% 
  count(stat_flag) %>% 
  ggplot(aes(x = stat_flag, y = n))+
  geom_bar(stat = "identity")

insert_stat_flag <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  if (data$stat_flag[i] == "ACTIVE"){
    insert_stat_flag[i] = 0
  }else if(data$stat_flag[i] == "LAPSED"){
    insert_stat_flag[i] = 1
  }else{
    insert_stat_flag[i] = 2
  }
}
data$stat_flag <- insert_stat_flag
data$stat_flag
data %>% 
  filter(stat_flag == 1 & f_purchase_lh == 1) %>% 
  select()

data$stat_flag <- as.factor(data$stat_flag)
#min_occ_date
current_date <- as.Date("2024-01-26","%Y-%m-%d")
insert_min_occ_date <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  date1 <- as.Date(min_occ_date[i], "%Y-%m-%d")
  insert_min_occ_date[i] <- as.numeric(difftime(current_date,date1,units = "days"))
}
data$min_occ_date <-insert_min_occ_date

#cltdob_fix
current_date <- as.Date("2024-01-26","%Y-%m-%d")
insert_cltdob_fix <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  date1 <- as.Date(cltdob_fix[i], "%Y-%m-%d")
  insert_cltdob_fix[i] <- as.numeric(difftime(current_date,date1,units = "days"))
}
data$cltdob_fix <-insert_cltdob_fix
data$cltdob_fix

print(data %>% 
  filter(!is.finite(min_occ_date)| !is.finite(cltdob_fix)) %>% 
  select(f_purchase_lh,cltdob_fix,min_occ_date),n = 32)

data <- data %>% 
  filter(cltdob_fix>=min_occ_date)

#cltsex_fix
data %>% 
  count(cltsex_fix) %>% 
  ggplot(aes(x = cltsex_fix, y = n))+
  geom_bar(stat = "identity")
data %>%
  filter(is.na(cltsex_fix)) %>% 
  select(f_purchase_lh)
data <- data %>% 
  filter(!is.na(cltsex_fix))

insert_cltsex_fix <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  if (data$cltsex_fix[i] == "Female"){
    insert_cltsex_fix[i] = 1
  }else{
    insert_cltsex_fix[i] = 0
  }
}
data$cltsex_fix <- insert_cltsex_fix
data$cltsex_fix <- as.factor(data$cltsex_fix)





client_info <- data[, 1:8]
client_info

#SECTION 2, Client Risk and Status Indicators

#dropping na values
data %>% 
  filter(is.na(flg_substandard)|is.na(flg_is_borderline_standard)|is.na(flg_is_revised_term)) %>% 
  select(flg_substandard,flg_is_borderline_standard,f_purchase_lh)

#flg_substandard
data<- data %>% 
  filter(!is.na(flg_substandard))

data %>% 
  count(flg_substandard) %>% 
  ggplot(aes(x = flg_substandard, y = n))+
  geom_bar(stat = "identity")

data %>% 
  filter(flg_substandard == 1)

data$flg_substandard <- as.factor(data$flg_substandard)
#flg_is_borderline_standard
data %>% 
  count(flg_is_borderline_standard) %>% 
  ggplot(aes(x = flg_is_borderline_standard, y = n))+
  geom_bar(stat = "identity")

data %>% 
  filter(flg_is_borderline_standard == 1 & flg_substandard == 1 & f_purchase_lh == 1) %>% 
  select(flg_substandard, flg_is_borderline_standard, f_purchase_lh)


data$flg_is_borderline_standard <- as.factor(data$flg_is_borderline_standard)
#flg_is_revised_term
data %>% 
  count(flg_is_revised_term) %>% 
  ggplot(aes(x = flg_is_revised_term, y = n))+
  geom_bar(stat = "identity")
data$flg_is_revised_term <- as.factor(data$flg_is_revised_term)
#flg_is_rental_flat
data %>% 
  count(flg_is_rental_flat) %>% 
  ggplot(aes(x = flg_is_rental_flat, y = n))+
  geom_bar(stat = "identity")
data$flg_is_rental_flat <- as.factor(data$flg_is_rental_flat)

#flg_has_health_claim
data %>% filter(is.na(flg_has_health_claim))

data %>% 
  count(flg_has_health_claim) %>% 
  ggplot(aes(x = flg_has_health_claim, y = n))+
  geom_bar(stat = "identity")
data$flg_has_health_claim <- as.factor(data$flg_has_health_claim)
# flg_has_life_claim
data %>% filter(is.na( flg_has_life_claim))
data %>% 
  count(flg_has_life_claim) %>% 
  ggplot(aes(x = flg_has_life_claim, y = n))+
  geom_bar(stat = "identity")
data$flg_has_life_claim <- as.factor(data$flg_has_life_claim)
#flg_gi_claim
data %>% filter(is.na(flg_gi_claim))
data %>% 
  count(flg_gi_claim) %>% 
  ggplot(aes(x = flg_gi_claim, y = n))+
  geom_bar(stat = "identity")
data$flg_gi_claim <- as.factor(data$flg_gi_claim)
#flg_is_proposal
data %>% filter(is.na(flg_is_proposal))
data %>% 
  count(flg_is_rental_flat) %>% 
  ggplot(aes(x = flg_is_rental_flat, y = n))+
  geom_bar(stat = "identity")
data$flg_is_proposal <- as.factor(data$flg_is_proposal)
#flg_with_preauthorisation
data %>% filter(is.na(flg_with_preauthorisation))
data %>% 
  count(flg_with_preauthorisation) %>% 
  ggplot(aes(x = flg_with_preauthorisation, y = n))+
  geom_bar(stat = "identity")
data$flg_with_preauthorisation <- as.factor(data$flg_with_preauthorisation)
#flg_is_returned_mail
data %>% filter(is.na(flg_is_returned_mail))
data %>% 
  count(flg_is_returned_mail) %>% 
  ggplot(aes(x = flg_is_returned_mail, y = n))+
  geom_bar(stat = "identity")
data$flg_is_returned_mail <- as.factor(data$flg_is_returned_mail)
client_risk <- data[, 9:18]
client_risk


#SECTION 3, Client Consent and Communication Preferences

#consent
dim(data)
insert_consent <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  insert_consent[i] = max(data$is_consent_to_mail[i],data$is_consent_to_email[i],data$is_consent_to_call[i],data$is_consent_to_sms[i])
}
insert_consent
data <- data %>% 
  mutate(consent = 0,.before = is_consent_to_mail)
data$consent <- insert_consent
data$consent

data %>% filter(is.na(consent))
data %>% 
  select(consent)
data %>% 
  count(consent) %>% 
  ggplot(aes(x = consent, y = n))+
  geom_bar(stat = "identity")
data$consent <- as.factor(data$consent)
#is_consent_to_mail
data %>% filter(is.na(is_consent_to_mail))
data %>% 
  count(is_consent_to_mail) %>% 
  ggplot(aes(x = is_consent_to_mail, y = n))+
  geom_bar(stat = "identity")

#is_consent_to_email
data %>% filter(is.na(is_consent_to_email))
data %>% 
  count(is_consent_to_email) %>% 
  ggplot(aes(x = is_consent_to_email, y = n))+
  geom_bar(stat = "identity")

#is_consent_to_call
data %>% filter(is.na(is_consent_to_call))
data %>% 
  count(is_consent_to_call) %>% 
  ggplot(aes(x = is_consent_to_call, y = n))+
  geom_bar(stat = "identity")

#is_consent_to_sms
data %>% filter(is.na(is_consent_to_sms))
data %>% 
  count(is_consent_to_sms) %>% 
  ggplot(aes(x = is_consent_to_sms, y = n))+
  geom_bar(stat = "identity")

#is_valid_dm
data %>% filter(is.na(is_valid_dm))
data %>% 
  count(is_valid_dm) %>% 
  ggplot(aes(x = is_valid_dm, y = n))+
  geom_bar(stat = "identity")

#is_valid_email
data %>% filter(is.na(is_valid_email))
data %>% 
  count(is_valid_email) %>% 
  ggplot(aes(x = is_valid_email, y = n))+
  geom_bar(stat = "identity")

insert_valid <- rep(0, nrow(data))
for (i in 1:nrow(data)){
  insert_valid[i] = max(data$is_valid_dm[i], data$is_valid_email[i])
}
insert_valid
data <- data %>% 
  mutate(valid = 0,.before = is_valid_dm)
data$valid <- insert_valid
data$valid

data %>% 
  count(valid) %>% 
  ggplot(aes(x = valid, y = n))+
  geom_bar(stat = "identity")

data$valid <- as.factor(data$valid)
data <- data %>% 
  select(!c(is_consent_to_call, is_consent_to_email, is_consent_to_mail, is_consent_to_sms, is_valid_dm, is_valid_email))


client_consent <- data[, 19:20]
client_consent

#SECTION 4, Demographic and Household Information


#is_housewife_retiree
data %>% filter(is.na(is_housewife_retiree))
data %>% 
  count(is_housewife_retiree) %>% 
  ggplot(aes(x = is_housewife_retiree, y = n))+
  geom_bar(stat = "identity")
data$is_housewife_retiree <- as.factor(data$is_housewife_retiree)
#is_sg_pr
data %>% filter(is.na(is_sg_pr))
data %>% 
  count(is_sg_pr) %>% 
  ggplot(aes(x = is_sg_pr, y = n))+
  geom_bar(stat = "identity")
data$is_sg_pr <- as.factor(data$is_sg_pr)
#is_class_1_2
data %>% filter(is.na(is_class_1_2))
data %>% 
  count(is_class_1_2) %>% 
  ggplot(aes(x = is_class_1_2, y = n))+
  geom_bar(stat = "identity")
data$is_class_1_2 <- as.factor(data$is_class_1_2)
#is_dependent_in_at_least_1_policy
#dropped due to values all being 0
data %>% filter(is.na(is_dependent_in_at_least_1_policy))
data %>% 
  count(is_dependent_in_at_least_1_policy) %>% 
  ggplot(aes(x = is_dependent_in_at_least_1_policy, y = n))+
  geom_bar(stat = "identity")
data %>% 
  distinct(is_dependent_in_at_least_1_policy)
data <- data %>% 
  select(!is_dependent_in_at_least_1_policy)

#group analysis of variables below
data %>% filter(is.na(pop_20) & is.na(hh_20) &is.na(hh_size) &is.na(hh_size_est) &is.na(annual_income_est))


#hh_20
sample_hh_20 <- data %>% 
  filter(!is.na(hh_20)) %>% 
  pull(hh_20)

insert_hh_20 <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  if (is.na(data$hh_20[i])){
    insert_hh_20[i] <- sample(sample_hh_20,1,replace = TRUE)
  }else{
    insert_hh_20[i] <- data$hh_20[i]
  }
}
data$hh_20 <- insert_hh_20
data$hh_20

data <- data %>% 
  mutate(across(c(hh_20),as.numeric))

data %>% 
  count(hh_20) %>% 
  ggplot(aes(x = hh_20, y = n))+
  geom_bar(stat = "identity")

ggplot(data, aes(x = hh_20))+
  geom_boxplot()

#pop_20
sample_pop_20 <- data %>% 
  filter(!is.na(pop_20)) %>% 
  pull(pop_20)

insert_pop_20 <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  if (is.na(data$pop_20[i])){
    insert_pop_20[i] <- sample(sample_pop_20,1,replace = TRUE)
  }else{
    insert_pop_20[i] <- data$pop_20[i]
  }
}
data$pop_20 <- insert_pop_20

data <- data %>% 
  mutate(across(c(pop_20),as.numeric))

data$pop_20

data %>% 
  count(pop_20) %>% 
  ggplot(aes(x = pop_20, y = n))+
  geom_bar(stat = "identity")

ggplot(data, aes(x = pop_20))+
  geom_boxplot()


#hh_size
sample_hh_size <- data %>% 
  filter(!is.na(hh_size)) %>% 
  pull(hh_size)

insert_hh_size <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  if (is.na(data$hh_size[i])){
    insert_hh_size[i] <- sample(sample_hh_size,1,replace = TRUE)
  }else{
    insert_hh_size[i] <- data$hh_size[i]
  }
}
data$hh_size <- insert_hh_size

data <- data %>% 
  mutate(across(c(hh_size),as.numeric))

data$hh_size

data %>% 
  count(hh_size) %>% 
  ggplot(aes(x = hh_size, y = n))+
  geom_bar(stat = "identity")


ggplot(data, aes(x = hh_size))+
  geom_boxplot()



#hh_size_est
sample_hh_size_est <- data %>% 
  filter(!is.na(hh_size_est)) %>% 
  pull(hh_size_est)

insert_hh_size_est <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  if (is.na(data$hh_size_est[i])){
    insert_hh_size_est[i] <- sample(sample_hh_size_est,1,replace = TRUE)
  }else{
    insert_hh_size_est[i] <- data$hh_size_est[i]
  }
}
data$hh_size_est <- insert_hh_size_est

data$hh_size_est[data$hh_size_est == ">4"] <- "4"

data <- data %>% 
  mutate(across(c(hh_size_est),as.numeric))

data$hh_size_est

data %>% 
  count(hh_size_est) %>% 
  ggplot(aes(x = hh_size_est, y = n))+
  geom_bar(stat = "identity")


ggplot(data, aes(x = hh_size_est))+
  geom_boxplot()

#annual_income_est
sample_annual_income_est <- data %>% 
  filter(!is.na(annual_income_est)) %>% 
  pull(annual_income_est)

insert_annual_income_est <- rep(0,nrow(data))
for (i in 1:nrow(data)){
  if (is.na(data$annual_income_est[i])){
    insert_annual_income_est[i] <- sample(sample_annual_income_est,1,replace = TRUE)
  }else{
    insert_annual_income_est[i] <- data$annual_income_est[i]
  }
}
data$annual_income_est <- insert_annual_income_est

for (i in 1:nrow(data)){
  if (data$annual_income_est[i] == "E.BELOW30K"){
    insert_annual_income_est[i] <- 0
  }else if (data$annual_income_est[i] == "D.30K-60K"){
    insert_annual_income_est[i] <- 1
  }else if (data$annual_income_est[i] == "C.60K-100K"){
    insert_annual_income_est[i] <- 2
  }else if (data$annual_income_est[i] == "B.100K-200K"){
    insert_annual_income_est[i] <- 3
  }else{
    insert_annual_income_est[i] <- 4
  }
}
data$annual_income_est <- insert_annual_income_est

data <- data %>% 
  mutate(across(c(annual_income_est),as.numeric))

data$annual_income_est

data %>% 
  distinct(annual_income_est)

data %>% 
  count(annual_income_est) %>% 
  ggplot(aes(x = annual_income_est, y = n))+
  geom_bar(stat = "identity")


ggplot(data, aes(x = annual_income_est))+
  geom_boxplot()





client_demo <- data[, c(21,22,23,25,26,27,28,29)]
client_demo
names(client_demo)


#SECTION 5, Policy and Claim History

#n_months_last_bought_products
data %>% filter(is.na(n_months_last_bought_products))
data %>% 
  count(n_months_last_bought_products) %>% 
  ggplot(aes(x = n_months_last_bought_products, y = n))+
  geom_bar(stat = "identity")

ggplot(data, aes(x = n_months_last_bought_products, y = f_purchase_lh))+
  geom_point()

ggplot(data, aes(x = n_months_last_bought_products))+
  geom_bar()+
  facet_wrap(~f_purchase_lh)

data$f_purchase_lh = as.factor(data$f_purchase_lh)

ggplot(data, aes(x = n_months_last_bought_products, 
                 group = f_purchase_lh, 
                 fill = f_purchase_lh))+
  geom_density(alpha = 0.3)

#flg_latest_being_lapse
data %>% filter(is.na(flg_latest_being_lapse))
data %>% 
  count(flg_latest_being_lapse) %>% 
  ggplot(aes(x = flg_latest_being_lapse, y = n))+
  geom_bar(stat = "identity")

data$flg_latest_being_lapse <- as.factor(data$flg_latest_being_lapse)

ggplot(data, aes(x = f_purchase_lh,
                 fill = flg_latest_being_lapse))+
  geom_bar(alpha = 0.3, position = "dodge")+
  facet_wrap(~flg_latest_being_lapse, scales = "free_y")


#flg_latest_being_cancel
data %>% filter(is.na(flg_latest_being_cancel))
data %>% 
  count(flg_latest_being_cancel) %>% 
  ggplot(aes(x = flg_latest_being_cancel, y = n))+
  geom_bar(stat = "identity")

data$flg_latest_being_cancel <- as.factor(data$flg_latest_being_cancel)

ggplot(data, aes(x = f_purchase_lh,
                 fill = flg_latest_being_cancel))+
  geom_bar(alpha = 0.3, position = "dodge")+
  facet_wrap(~flg_latest_being_cancel, scales = "free_y")
#recency_lapse
#dropped as too many NA values
data %>% filter(is.na(recency_lapse))

data %>%
  filter(is.na(recency_lapse)) %>% 
  select(recency_lapse,f_purchase_lh)

data %>% 
  distinct(recency_lapse)

data <- data %>% 
  select(!recency_lapse)

#recency_cancel
#dropped too many NA values
data %>% 
  filter(!is.na(recency_cancel) & !is.na(tot_cancel_pols)) %>% 
  select(recency_cancel, tot_cancel_pols, f_purchase_lh)

ggplot(data, aes(x = recency_cancel,fill = f_purchase_lh))+
  geom_bar()+
  facet_wrap(~f_purchase_lh)

data %>% 
  filter(!is.na(recency_cancel))

data %>% distinct(recency_cancel)

data <- data %>% 
  select(!recency_cancel)

#tot_inforce_pols
data %>% filter(is.na(tot_inforce_pols))
data %>% 
  count(tot_inforce_pols) %>% 
  ggplot(aes(x = tot_inforce_pols, y = n))+
  geom_bar(stat = "identity")


#tot_cancel_pols
data %>% filter(is.na(tot_cancel_pols))
data %>% 
  count(tot_cancel_pols) %>% 
  ggplot(aes(x = tot_cancel_pols, y = n))+
  geom_bar(stat = "identity")
data %>% distinct(tot_cancel_pols)

data$tot_cancel_pols[is.na(data$tot_cancel_pols)] <- 0

#f_ever_declined_la
data %>% 
  filter(!is.na(f_ever_declined_la)) %>% 
  select(f_ever_declined_la)

data %>% 
  distinct(f_ever_declined_la)

data$f_ever_declined_la[is.na(data$f_ever_declined_la)] <- 0
data %>% 
  count(f_ever_declined_la) %>% 
  ggplot(aes(x = f_ever_declined_la, y = n))+
  geom_bar(stat = "identity")
data$f_ever_declined_la <- as.factor(data$f_ever_declined_la)
client_policy <- data[, c(30,31,32,33,34,24)]
client_policy
names(client_policy)

#SECTION 6, Anonymized Insurance Product Metrics (APE, Sum Insured, Prepaid Premiums)
insurance_metrics <- data[, 35:151]
insurance_metrics
name_insurance_metrics <- names(insurance_metrics)
name_insurance_metrics
attach(data)
names(data)
#ape_gi
data %>% 
  distinct(ape_gi)
data <- data %>% 
  select(!starts_with("ape_gi"))

#ape_ltc
data %>% 
  select(starts_with("ape_ltc"))

data <- data %>% 
  mutate(ape_ltc = ape_ltc, .after = ape_gi)
data$ape_ltc <- rowSums(data %>% select(starts_with("ape_ltc")))
data$ape_ltc

data <- data %>%
  select(!starts_with("ape_ltc_"))



ggplot(data, aes(x = ape_ltc))+
  geom_boxplot()

#ape_grp
data %>% 
  mutate(ape_grp = 0, .after = ape_ltc)

data$ape_grp <- rowSums(data %>%  select(starts_with("ape_grp")))
data$ape_grp

data <- data %>% 
  select(!starts_with("ape_grp_"))

#ape_inv
data %>% 
  select(starts_with("ape_inv")) %>% 
  distinct(ape_inv_e9f316)


data <- data %>% 
  select(!ape_inv_dcd836) %>% 
  rename(ape_inv = ape_inv_e9f316)
data %>% 
  distinct(ape_inv)

ggplot(data, aes(x = ape_inv))+
  geom_boxplot()

#ape_lh
data %>% 
  select(starts_with("ape_lh"))

data %>% 
  mutate(ape_lh = 0, .before = (ape_lh_d0adeb))

data$ape_lh <- rowSums(data %>%  select(starts_with("ape_lh")))
data$ape_lh

ggplot(data, aes(x = ape_lh, y = f_purchase_lh))+
  geom_boxplot()

data <- data %>% 
  select(!starts_with("ape_lh_"))

data <- data %>% 
  filter(!ape_lh>300000)

#ape_XXXX
data %>% 
  select(ape_32c74c,ape_839f8a,ape_e22a6a,ape_d0adeb,ape_c4bda5,ape_507c37) %>% 
  distinct(ape_507c37)

data <- data %>% 
  mutate(ape = 0,.after = ape_lh)
data$ape <- rowSums(data %>%select(ape_32c74c,ape_839f8a,ape_e22a6a,ape_d0adeb,ape_c4bda5,ape_507c37))
data$ape

ggplot(data, aes(x = ape))+
  geom_boxplot()

data <- data %>% 
  select(!c(ape_32c74c,ape_839f8a,ape_e22a6a,ape_d0adeb,ape_c4bda5,ape_507c37))

#cleaned ape
data %>% 
  select(starts_with("ape"))

#sumins
#sumins_gi
data %>% 
  select(starts_with("sumins_gi")) %>% 
  distinct(sumins_gi_a10d1b)

data <- data %>% 
  select(!starts_with("sumins_gi"))

#sumins_ltc
data %>% 
  select(starts_with("sumins_ltc")) %>% 
  mutate(sumin = sumins_ltc == sumins_ltc_43b9d5) %>% 
  distinct(sumin)

data$sumins_ltc <- rowSums(data %>% select(starts_with("sumins_ltc")))
data$sumins_ltc

ggplot(data, aes(x = sumins_ltc))+
  geom_boxplot()

data <- data %>% 
  select(!starts_with("sumins_ltc_"))

#sumins_grp
data %>% 
  select(starts_with("sumins_grp")) %>% 
  distinct(sumins_grp_1581d7)

data <- data %>% 
  mutate(sumins_grp = 0, .before = sumins_grp_6fc3e6)
data$sumins_grp <- rowSums(data %>% select(starts_with("sumins_grp")))
data$sumins_grp

data <- data %>% 
  select(!starts_with("sumins_grp_"))

ggplot(data, aes(x = sumins_grp))+
  geom_boxplot()

#sumins_inv
data %>% 
  select(starts_with("sumins_inv"), f_purchase_lh) %>% 
  filter(sumins_inv_e9f316!=0)

data <- data %>% 
  select(!sumins_inv_dcd836) %>% 
  rename(sumins_inv = sumins_inv_e9f316)
data$sumins_inv

#sumins_lh
data %>% 
  select(starts_with("sumins_lh"))
data <- data %>% 
  mutate(sumins_lh = 0, .before = sumins_lh_d0adeb)
data$sumins_lh <- rowSums(data %>% select(starts_with("sumins_lh")))
data$sumins_lh

data <- data %>% 
  select(!starts_with("sumins_lh_"))
#sumins_XXXX
data %>% 
  select(starts_with("sumins"))

data <- data %>%
  mutate(sumins = 0, .after = sumins_lh)
data$sumins <- rowSums(data %>% select(sumins_32c74c,sumins_839f8a,sumins_e22a6a,sumins_d0adeb,sumins_c4bda5,sumins_507c37))
data$sumins

data <- data %>% 
  select(!c(sumins_32c74c,sumins_839f8a,sumins_e22a6a,sumins_d0adeb,sumins_c4bda5,sumins_507c37))

#sumins
data %>% 
  select(starts_with("sumins"))

#prempaid
#prempaid_gi
data %>% 
  select(starts_with("prempaid_gi")) %>% 
  distinct(prempaid_gi)

data <- data %>% 
  select(!starts_with("prempaid_gi"))

#prempaid_ltc
data %>%
  select(starts_with("prempaid_ltc"))

data$prempaid_ltc <- rowSums(data %>% select(starts_with("prempaid_ltc")))
data$prempaid_ltc

data<- data %>% 
  select(!starts_with("prempaid_ltc_"))
#prempaid_grp
data %>% 
  select(starts_with("prempaid_grp")) %>% 
  distinct(prempaid_grp_1581d7)

data <- data %>% 
  mutate(prempaid_grp = 0, .before = prempaid_grp_6fc3e6)
data$prempaid_grp <- rowSums(data %>% select(starts_with("prempaid_grp")))
data$prempaid_grp

data <- data %>% 
  select(!starts_with("prempaid_grp_"))

#prempaid_inv
data %>% 
  select(starts_with("prempaid_inv")) %>% 
  distinct(prempaid_inv_e9f316)

data <- data %>% 
  select(!prempaid_inv_dcd836) %>% 
  rename(prempaid_inv = prempaid_inv_e9f316)

#prempaid_lh
data %>% 
  select(starts_with("prempaid_lh"))

data <- data %>% 
  mutate(prempaid_lh = 0, .before = prempaid_lh_d0adeb)
data$prempaid_lh <- rowSums(data %>% select(starts_with("prempaid_lh")))
data$prempaid_lh

data <- data %>% 
  select(!starts_with("prempaid_lh_"))


#prempaid_XXXX
data %>% 
  select(starts_with("prempaid"))
data <- data %>% 
  mutate(prempaid = 0, .after = prempaid_lh)

data$prempaid <- rowSums(data %>% select(prempaid_32c74c,prempaid_839f8a, prempaid_e22a6a, prempaid_d0adeb, prempaid_c4bda5, prempaid_507c37))
data$prempaid

data <- data %>% 
  select(!c(prempaid_32c74c,prempaid_839f8a, prempaid_e22a6a, prempaid_d0adeb, prempaid_c4bda5, prempaid_507c37))

#prempaid
data %>% 
  select(starts_with("prempaid"))

#f_hold

data %>% 
  mutate(f_hold = 0)
data$f_hold <- rowSums((data %>% select(starts_with("f_hold"))))
data$f_hold
data %>% 
  distinct(f_hold)
data <- data %>% 
  select(!starts_with("f_hold_"))

#f_elx
data %>% filter(is.na(f_elx))
data %>% 
  count(f_elx) %>% 
  ggplot(aes(x = f_elx, y = n))+
  geom_bar(stat = "identity")
data$f_elx <- as.factor(data$f_elx)
#f_mindef_mha
data %>% filter(is.na(f_mindef_mha))
data %>% 
  count(f_mindef_mha) %>% 
  ggplot(aes(x = f_mindef_mha, y = n))+
  geom_bar(stat = "identity")
data$f_mindef_mha <- as.factor(data$f_mindef_mha)
#f_retail
data %>% filter(is.na(f_retail))
data %>% 
  count(f_retail) %>% 
  ggplot(aes(x = f_retail, y = n))+
  geom_bar(stat = "identity")
data$f_retail <- as.factor(data$f_retail)

#flg_affconnect_*
data$flg_affconnect_lapse_ever[is.na(data$flg_affconnect_lapse_ever)] <- 0
data$flg_affconnect_ready_to_buy_ever[is.na(data$flg_affconnect_ready_to_buy_ever)] <- 0
data$flg_affconnect_show_interest_ever[is.na(data$flg_affconnect_show_interest_ever)] <- 0

data %>% 
  count(flg_affconnect_lapse_ever) %>% 
  ggplot(aes(x = flg_affconnect_lapse_ever, y = n))+
  geom_bar(stat = "identity")

data %>% 
  count(flg_affconnect_show_interest_ever) %>% 
  ggplot(aes(x = flg_affconnect_show_interest_ever, y = n))+
  geom_bar(stat = "identity")

data %>% 
  count(flg_affconnect_ready_to_buy_ever) %>% 
  ggplot(aes(x = flg_affconnect_ready_to_buy_ever, y = n))+
  geom_bar(stat = "identity")

data %>% 
  filter(flg_affconnect_lapse_ever == 1 | flg_affconnect_ready_to_buy_ever == 1 | flg_affconnect_show_interest_ever == 1)

data <- data %>% 
  mutate(flg_affconnect = pmax(data$flg_affconnect_lapse_ever,data$flg_affconnect_ready_to_buy_ever, data$flg_affconnect_show_interest_ever), .before = flg_affconnect_lapse_ever)

data <- data %>% 
  select(!starts_with("flg_affconnect_"))
data %>% 
  count(flg_affconnect) %>% 
  ggplot(aes(x = flg_affconnect, y = n))+
  geom_bar(stat = "identity")

data$flg_affconnect <- as.factor(data$flg_affconnect)
#affcon_visit_days
data %>% filter(is.na(affcon_visit_days))
data %>% 
  count(affcon_visit_days) %>% 
  ggplot(aes(x = affcon_visit_days, y = n))+
  geom_bar(stat = "identity")
print(data %>% 
  distinct(affcon_visit_days), n = 21)
data$affcon_visit_days[is.na(data$affcon_visit_days)] <- 0
data$affcon_visit_days[data$affcon_visit_days>0] <- 1
data %>% 
  count(affcon_visit_days) %>% 
  ggplot(aes(x = affcon_visit_days, y = n))+
  geom_bar(stat = "identity")
data$affcon_visit_days <- as.factor(data$affcon_visit_days)

#n_months_since_visit_affcon
data %>% filter(is.na(n_months_since_visit_affcon))
data %>% 
  count(n_months_since_visit_affcon) %>% 
  ggplot(aes(x = n_months_since_visit_affcon, y = n))+
  geom_bar(stat = "identity")

data$n_months_since_visit_affcon[!is.na(data$n_months_since_visit_affcon)] <- 1
data$n_months_since_visit_affcon[is.na(data$n_months_since_visit_affcon)] <- 0

data %>% 
  mutate(real = data$affcon_visit_days == data$n_months_since_visit_affcon) %>% 
  distinct(real)

data <- data %>% 
  select(!n_months_since_visit_affcon)

#clmcon_visit_days
data %>% filter(is.na(clmcon_visit_days))
data %>% 
  count(clmcon_visit_days) %>% 
  ggplot(aes(x = clmcon_visit_days, y = n))+
  geom_bar(stat = "identity")
data$clmcon_visit_days[!is.na(data$clmcon_visit_days)] <- 1
data$clmcon_visit_days[is.na(data$clmcon_visit_days)] <- 0
data$clmcon_visit_days <- as.factor(data$clmcon_visit_days)

#recency_clmcon
data %>% filter(is.na(recency_clmcon))
data %>% 
  count(recency_clmcon) %>% 
  ggplot(aes(x = recency_clmcon, y = n))+
  geom_bar(stat = "identity")
data$recency_clmcon[!is.na(data$recency_clmcon)] <- 1
data$recency_clmcon[is.na(data$recency_clmcon)] <- 0
data %>% 
  mutate(real = data$clmcon_visit_days == data$recency_clmcon) %>% 
  distinct(real)
data <- data %>% 
  select(!recency_clmcon)
#recency_clmcon_regis
data %>% filter(is.na(recency_clmcon_regis))
data %>% 
  count(recency_clmcon_regis) %>% 
  ggplot(aes(x = recency_clmcon_regis, y = n))+
  geom_bar(stat = "identity")
data$recency_clmcon_regis[!is.na(data$recency_clmcon_regis)] <- 1
data$recency_clmcon_regis[is.na(data$recency_clmcon_regis)] <- 0
data %>% 
  mutate(real = data$clmcon_visit_days == data$recency_clmcon_regis) %>% 
  distinct(real)
data <- data %>% 
  select(!recency_clmcon_regis)
#hlthclaim_amt
data %>% filter(is.na(hlthclaim_amt))
data %>% 
  count(hlthclaim_amt) %>% 
  ggplot(aes(x = hlthclaim_amt, y = n))+
  geom_bar(stat = "identity")

data %>% 
  distinct(hlthclaim_amt)
data %>% 
  filter(!is.na(hlthclaim_amt) & !is.na(hlthclaim_cnt_success) & !is.na(hlthclaim_cnt_unsuccess))

data$hlthclaim_amt[is.na(data$hlthclaim_amt)] <- 0

#giclaim_amt
data %>% filter(is.na(giclaim_amt))
data %>% 
  count(giclaim_amt) %>% 
  ggplot(aes(x = giclaim_amt, y = n))+
  geom_bar(stat = "identity")
data %>% 
  distinct(giclaim_amt)
data$giclaim_amt[is.na(data$giclaim_amt)] <- 0
#recency_hlthclaim
data %>% filter(is.na(recency_hlthclaim))
data %>% 
  count(recency_hlthclaim) %>% 
  ggplot(aes(x = recency_hlthclaim, y = n))+
  geom_bar(stat = "identity")
data %>% 
  distinct(recency_hlthclaim)
data$recency_hlthclaim[!is.na(data$recency_hlthclaim)] <- 1
data$recency_hlthclaim[is.na(data$recency_hlthclaim)] <- 0
data %>% 
  mutate(real = data$hlthclaim_amt >= data$recency_hlthclaim) %>% 
  select(real) %>% 
  filter(real == FALSE)

data <- data %>% 
  select(!c(recency_hlthclaim_839f8a,recency_hlthclaim_14cb37))
data$recency_hlthclaim <- as.factor(data$recency_hlthclaim)
#recency_giclaim
data$recency_giclaim[!is.na(data$recency_giclaim)] <- 1
data$recency_giclaim[is.na(data$recency_giclaim)] <- 0
data %>% 
  count(recency_giclaim) %>% 
  ggplot(aes(x = recency_giclaim, y = n))+
  geom_bar(stat = "identity")
data$recency_giclaim <- as.factor(data$recency_giclaim)
data %>% 
  select(starts_with("recency_giclaim"))
#hlthclaim_cnt_success
data %>% filter(is.na(hlthclaim_cnt_success))
data %>% 
  count(hlthclaim_cnt_success) %>% 
  ggplot(aes(x = hlthclaim_cnt_success, y = n))+
  geom_bar(stat = "identity")
data$hlthclaim_cnt_success[!is.na(data$hlthclaim_cnt_success)] <- 1
data$hlthclaim_cnt_success[is.na(data$hlthclaim_cnt_success)] <- 0
data$hlthclaim_cnt_success <- as.factor(data$hlthclaim_cnt_success)
#hlthclaim_cnt_unsuccess
data <- data %>% 
  select(!hlthclaim_cnt_unsuccess)
#giclaim_cnt_success
data %>% filter(is.na(giclaim_cnt_success))

data <- data %>% 
  select(!giclaim_cnt_success)
#giclaim_cnt_unsuccess
data %>% filter(is.na(giclaim_cnt_unsuccess))

data <- data %>% 
  select(!giclaim_cnt_unsuccess)
#flg_hlthclaim_
data %>%
  select(starts_with("flg_hlthclaim_")) %>% 
  distinct(flg_hlthclaim_839f8a_ever)
data %>%
  select(starts_with("flg_hlthclaim_")) %>% 
  distinct(flg_hlthclaim_14cb37_ever)
data$flg_hlthclaim_839f8a_ever[is.na(data$flg_hlthclaim_839f8a_ever)] <- 0
data$flg_hlthclaim_14cb37_ever[is.na(data$flg_hlthclaim_14cb37_ever)] <- 0
data %>% 
  count(flg_hlthclaim_839f8a_ever) %>% 
  ggplot(aes(x = flg_hlthclaim_839f8a_ever, y = n))+
  geom_bar(stat = "identity")
data %>% 
  count(flg_hlthclaim_14cb37_ever) %>% 
  ggplot(aes(x = flg_hlthclaim_14cb37_ever, y = n))+
  geom_bar(stat = "identity")

data %>% 
  mutate(flg_hlthclaim = pmax(flg_hlthclaim_14cb37_ever,flg_hlthclaim_839f8a_ever)) %>% 
  select(flg_hlthclaim, flg_has_health_claim) %>% 
  mutate(real = flg_hlthclaim == flg_has_health_claim) %>% 
  distinct(real)

data <- data %>% 
  mutate(flg_hlthclaim = pmax(flg_hlthclaim_14cb37_ever,flg_hlthclaim_839f8a_ever))
data <- data %>% 
  select(!c(flg_hlthclaim_14cb37_ever,flg_hlthclaim_839f8a_ever))

data$flg_hlthclaim <- as.factor(data$flg_hlthclaim)
#flg_gi_claim_
data %>%
  select(starts_with("flg_gi_claim"))
data <- data %>% 
  select(!starts_with("flg_gi_claim_"))

data %>% filter(is.na(flg_gi_claim))
data %>% 
  count(flg_gi_claim) %>% 
  ggplot(aes(x = flg_gi_claim, y = n))+
  geom_bar(stat = "identity")

data$flg_gi_claim <- as.factor(data$flg_gi_claim)
#f_ever_bought_
data %>% 
  select(starts_with("f_ever_bought"))

#f_ever_bought_gi
data %>% 
  select(starts_with("f_ever_bought_gi")) %>% 
  distinct(f_ever_bought_gi)
data$f_ever_bought_gi <- as.factor(data$f_ever_bought_gi)
#f_ever_bought_ltc
data %>% 
  select(starts_with("f_ever_bought_ltc"))

data$f_ever_bought_ltc <- rowSums(data %>% select(f_ever_bought_ltc, f_ever_bought_ltc_1280bf, f_ever_bought_ltc_43b9d5))

data$f_ever_bought_ltc <- as.factor(data$f_ever_bought_ltc)

data <- data %>% 
  select(!c(f_ever_bought_ltc_1280bf, f_ever_bought_ltc_43b9d5))
#f_ever_bought_grp
data %>% 
  select(starts_with("f_ever_bought_grp"))
data <- data %>% 
  mutate(f_ever_bought_grp = 0, .before = f_ever_bought_grp_6fc3e6)

data$f_ever_bought_grp <- rowSums(data %>% select(starts_with("f_ever_bought_grp")))
data$f_ever_bought_grp <- as.factor(data$f_ever_bought_grp)
data <- data %>% 
  select(!starts_with("f_ever_bought_grp_"))
#f_ever_bought_inv
data %>% 
  select(starts_with("f_ever_bought_inv"))
data <- data %>% 
  mutate(f_ever_bought_inv = 0, .before = f_ever_bought_inv_dcd836)

data$f_ever_bought_inv <- rowSums(data %>% select(starts_with("f_ever_bought_inv")))
data$f_ever_bought_inv <- as.factor(data$f_ever_bought_inv)
data <- data %>% 
  select(!starts_with("f_ever_bought_inv_"))
data$f_ever_bought_inv <- as.factor(data$f_ever_bought_inv)
#f_ever_bought_lh
data %>%
  select(starts_with("f_ever_bought_lh"))
data <- data %>% 
  mutate(f_ever_bought_lh = 0, .before = f_ever_bought_lh_d0adeb)

data$f_ever_bought_lh <- rowSums(data %>% select(starts_with("f_ever_bought_lh")))
data$f_ever_bought_lh <- as.factor(data$f_ever_bought_lh)
data <- data %>% 
  select(!starts_with("f_ever_bought_lh_"))
data$f_ever_bought_lh <- as.factor(data$f_ever_bought_lh)
#f_ever_bought
data %>%
  select(starts_with("f_ever_bought"))
data <- data %>% 
  mutate(f_ever_bought = 0, .before = f_ever_bought_839f8a)

data$f_ever_bought <- rowSums(data %>% select(f_ever_bought_839f8a,
                                                 f_ever_bought_e22a6a,
                                                 f_ever_bought_d0adeb,
                                                 f_ever_bought_c4bda5,
                                                 f_ever_bought_507c37,
                                                 f_ever_bought_32c74c))
data$f_ever_bought <- as.factor(data$f_ever_bought)
data <- data %>% 
  select(!c(f_ever_bought_839f8a,
            f_ever_bought_e22a6a,
            f_ever_bought_d0adeb,
            f_ever_bought_c4bda5,
            f_ever_bought_507c37,
            f_ever_bought_32c74c))
data$f_ever_bought <- as.factor(data$f_ever_bought)

#f_ever_bought cleaned
data %>%
  select(starts_with("f_ever_bought"))

#n_months_last_bought_
data %>% 
  select(starts_with("n_months_last_bought"))
names(data)

#n_months_last_bought_gi
data %>% 
  select(starts_with("n_months_last_bought_gi")) %>% 
  distinct(n_months_last_bought_gi)
data %>% 
  filter(is.finite(n_months_last_bought_gi)) %>% 
  select(n_months_last_bought_gi)

data %>% 
  filter(is.na(n_months_last_bought_gi)) %>% 
  select(n_months_last_bought_gi)


data$n_months_last_bought_gi[data$n_months_last_bought_gi == 9999] <- 0
data$n_months_last_bought_gi[data$n_months_last_bought_gi > 0] <- 1
data %>% 
  count(n_months_last_bought_gi) %>% 
  ggplot(aes(x = n_months_last_bought_gi, y = n))+
  geom_bar(stat = "identity")


#n_months_last_bought_ltc
data %>% 
  select(starts_with("n_months_last_bought_ltc"))
data$n_months_last_bought_ltc <- as.numeric(data$n_months_last_bought_ltc)
data$n_months_last_bought_ltc_1280bf <- as.numeric(data$n_months_last_bought_ltc_1280bf)
data$n_months_last_bought_ltc_43b9d5 <- as.numeric(data$n_months_last_bought_ltc_43b9d5)

data$n_months_last_bought_ltc[data$n_months_last_bought_ltc == 9999] <- 0
data$n_months_last_bought_ltc[data$n_months_last_bought_ltc > 0] <- 1
data$n_months_last_bought_ltc_1280bf[data$n_months_last_bought_ltc_1280bf == 9999] <- 0
data$n_months_last_bought_ltc_1280bf[data$n_months_last_bought_ltc_1280bf > 0] <- 1
data$n_months_last_bought_ltc_43b9d5[data$n_months_last_bought_ltc_43b9d5 == 9999] <- 0
data$n_months_last_bought_ltc_43b9d5[data$n_months_last_bought_ltc_43b9d5 > 0] <- 1

data$n_months_last_bought_ltc <- rowSums(data %>% select(starts_with("n_months_last_bought_ltc")))


data <- data %>% 
  select(!starts_with("n_months_last_bought_ltc_"))
#n_months_last_bought_grp
data %>% 
  select(starts_with("n_months_last_bought_grp"))


names(data)
data %>% 
  mutate(n_months_last_bought_grp = 0, .before = n_months_last_bought_grp_6fc3e6)
data$n_months_last_bought_grp_6fc3e6[data$n_months_last_bought_grp_6fc3e6 != "9999"] <- 1
data$n_months_last_bought_grp_6fc3e6[data$n_months_last_bought_grp_6fc3e6 == "9999"] <- 0
data$n_months_last_bought_grp_de05ae[data$n_months_last_bought_grp_de05ae != "9999"] <- 1
data$n_months_last_bought_grp_de05ae[data$n_months_last_bought_grp_de05ae == "9999"] <- 0
data$n_months_last_bought_grp_945b5a[data$n_months_last_bought_grp_945b5a != "9999"] <- 1
data$n_months_last_bought_grp_945b5a[data$n_months_last_bought_grp_945b5a == "9999"] <- 0
data$n_months_last_bought_grp_6a5788[data$n_months_last_bought_grp_6a5788 != "9999"] <- 1
data$n_months_last_bought_grp_6a5788[data$n_months_last_bought_grp_6a5788 == "9999"] <- 0
data$n_months_last_bought_grp_9cdedf[data$n_months_last_bought_grp_9cdedf != "9999"] <- 1
data$n_months_last_bought_grp_9cdedf[data$n_months_last_bought_grp_9cdedf == "9999"] <- 0
data$n_months_last_bought_grp_1581d7[data$n_months_last_bought_grp_1581d7 != "9999"] <- 1
data$n_months_last_bought_grp_1581d7[data$n_months_last_bought_grp_1581d7 == "9999"] <- 0
data$n_months_last_bought_grp_22decf[data$n_months_last_bought_grp_22decf != "9999"] <- 1
data$n_months_last_bought_grp_22decf[data$n_months_last_bought_grp_22decf == "9999"] <- 0
data$n_months_last_bought_grp_caa6ff[data$n_months_last_bought_grp_caa6ff != "9999"] <- 1
data$n_months_last_bought_grp_caa6ff[data$n_months_last_bought_grp_caa6ff == "9999"] <- 0
data$n_months_last_bought_grp_fd3bfb[data$n_months_last_bought_grp_fd3bfb != "9999"] <- 1
data$n_months_last_bought_grp_fd3bfb[data$n_months_last_bought_grp_fd3bfb == "9999"] <- 0
data$n_months_last_bought_grp_70e1dd[data$n_months_last_bought_grp_70e1dd != "9999"] <- 1
data$n_months_last_bought_grp_70e1dd[data$n_months_last_bought_grp_70e1dd == "9999"] <- 0
data$n_months_last_bought_grp_e04c3a[data$n_months_last_bought_grp_e04c3a != "9999"] <- 1
data$n_months_last_bought_grp_e04c3a[data$n_months_last_bought_grp_e04c3a == "9999"] <- 0
data$n_months_last_bought_grp_fe5fb8[data$n_months_last_bought_grp_fe5fb8 != "9999"] <- 1
data$n_months_last_bought_grp_fe5fb8[data$n_months_last_bought_grp_fe5fb8 == "9999"] <- 0
data$n_months_last_bought_grp_94baec[data$n_months_last_bought_grp_94baec != "9999"] <- 1
data$n_months_last_bought_grp_94baec[data$n_months_last_bought_grp_94baec == "9999"] <- 0
data$n_months_last_bought_grp_e91421[data$n_months_last_bought_grp_e91421 != "9999"] <- 1
data$n_months_last_bought_grp_e91421[data$n_months_last_bought_grp_e91421 == "9999"] <- 0










data$n_months_last_bought_grp_6fc3e6 <- as.numeric(data$n_months_last_bought_grp_6fc3e6)
data$n_months_last_bought_grp_de05ae <- as.numeric(data$n_months_last_bought_grp_de05ae)
data$n_months_last_bought_grp_945b5a <- as.numeric(data$n_months_last_bought_grp_945b5a)
data$n_months_last_bought_grp_6a5788 <- as.numeric(data$n_months_last_bought_grp_6a5788)
data$n_months_last_bought_grp_9cdedf <- as.numeric(data$n_months_last_bought_grp_9cdedf)
data$n_months_last_bought_grp_1581d7 <- as.numeric(data$n_months_last_bought_grp_1581d7)
data$n_months_last_bought_grp_22decf <- as.numeric(data$n_months_last_bought_grp_22decf)
data$n_months_last_bought_grp_caa6ff <- as.numeric(data$n_months_last_bought_grp_caa6ff)
data$n_months_last_bought_grp_fd3bfb <- as.numeric(data$n_months_last_bought_grp_fd3bfb)
data$n_months_last_bought_grp_70e1dd <- as.numeric(data$n_months_last_bought_grp_70e1dd)
data$n_months_last_bought_grp_e04c3a <- as.numeric(data$n_months_last_bought_grp_e04c3a)
data$n_months_last_bought_grp_fe5fb8 <- as.numeric(data$n_months_last_bought_grp_fe5fb8)
data$n_months_last_bought_grp_94baec <- as.numeric(data$n_months_last_bought_grp_94baec)
data$n_months_last_bought_grp_e91421 <- as.numeric(data$n_months_last_bought_grp_e91421)

data$n_months_last_bought_grp <- rowSums(data %>% select(starts_with("n_months_last_bought_grp")))

data %>% 
  distinct(n_months_last_bought_grp)

data <- data %>% 
  select(!starts_with("n_months_last_bought_grp_"))

#n_months_last_bought_inv
data %>% select(starts_with("n_months_last_bought_inv")) 
data <- data %>% 
  select(!n_months_last_bought_inv_dcd836)

data$n_months_last_bought_inv_e9f316 <- as.numeric(data$n_months_last_bought_inv_e9f316)
data$n_months_last_bought_inv_e9f316[data$n_months_last_bought_inv_e9f316 != 9999] <- 1
data$n_months_last_bought_inv_e9f316[data$n_months_last_bought_inv_e9f316 == 9999] <- 0
data <- data %>% 
  rename(n_months_last_bought = n_months_last_bought_inv_e9f316)
data$n_months_last_bought <- as.factor(data$n_months_last_bought)

#n_months_last_bought_lh
data %>% select(starts_with("n_months_last_bought_lh"))

data <- data %>% 
  select(!n_months_last_bought_lh_d0adeb)
data$n_months_last_bought_lh_507c37[data$n_months_last_bought_lh_507c37 != "9999"] <- 1
data$n_months_last_bought_lh_507c37[data$n_months_last_bought_lh_507c37 == "9999"] <- 0
data$n_months_last_bought_lh_839f8a[data$n_months_last_bought_lh_839f8a != "9999"] <- 1
data$n_months_last_bought_lh_839f8a[data$n_months_last_bought_lh_839f8a == "9999"] <- 0
data$n_months_last_bought_lh_e22a6a[data$n_months_last_bought_lh_e22a6a != "9999"] <- 1
data$n_months_last_bought_lh_e22a6a[data$n_months_last_bought_lh_e22a6a == "9999"] <- 0
data$n_months_last_bought_lh_f852af[data$n_months_last_bought_lh_f852af != "9999"] <- 1
data$n_months_last_bought_lh_f852af[data$n_months_last_bought_lh_f852af == "9999"] <- 0
data$n_months_last_bought_lh_947b15[data$n_months_last_bought_lh_947b15 != "9999"] <- 1
data$n_months_last_bought_lh_947b15[data$n_months_last_bought_lh_947b15 == "9999"] <- 0


data$n_months_last_bought_lh_507c37 <- as.numeric(data$n_months_last_bought_lh_507c37)
data$n_months_last_bought_lh_839f8a <- as.numeric(data$n_months_last_bought_lh_839f8a)
data$n_months_last_bought_lh_e22a6a <- as.numeric(data$n_months_last_bought_lh_e22a6a)
data$n_months_last_bought_lh_f852af <- as.numeric(data$n_months_last_bought_lh_f852af)
data$n_months_last_bought_lh_947b15 <- as.numeric(data$n_months_last_bought_lh_947b15)

data <- data %>% 
  mutate(n_months_last_bought_lh = 0, .before = n_months_last_bought_lh_507c37)

data$n_months_last_bought_lh <- rowSums(data %>% select(starts_with("n_months_last_bought_lh")))

data <- data %>% 
  select(!starts_with("n_months_last_bought_lh"))

#n_months_last_bought
data %>% 
  select(starts_with("n_months_last_bought"))

data$n_months_last_bought_839f8a[data$n_months_last_bought_839f8a != "9999"] <- 1
data$n_months_last_bought_839f8a[data$n_months_last_bought_839f8a == "9999"] <- 0
data$n_months_last_bought_e22a6a[data$n_months_last_bought_e22a6a != "9999"] <- 1
data$n_months_last_bought_e22a6a[data$n_months_last_bought_e22a6a == "9999"] <- 0
data$n_months_last_bought_d0adeb[data$n_months_last_bought_d0adeb != "9999"] <- 1
data$n_months_last_bought_d0adeb[data$n_months_last_bought_d0adeb == "9999"] <- 0
data$n_months_last_bought_c4bda5[data$n_months_last_bought_c4bda5 != "9999"] <- 1
data$n_months_last_bought_c4bda5[data$n_months_last_bought_c4bda5 == "9999"] <- 0
data$n_months_last_bought_507c37[data$n_months_last_bought_507c37 != "9999"] <- 1
data$n_months_last_bought_507c37[data$n_months_last_bought_507c37 == "9999"] <- 0
data$n_months_last_bought_32c74c[data$n_months_last_bought_32c74c != "9999"] <- 1
data$n_months_last_bought_32c74c[data$n_months_last_bought_32c74c == "9999"] <- 0



data$n_months_last_bought_839f8a <- as.numeric(data$n_months_last_bought_839f8a)
data$n_months_last_bought_e22a6a <- as.numeric(data$n_months_last_bought_e22a6a)
data$n_months_last_bought_d0adeb <- as.numeric(data$n_months_last_bought_d0adeb)
data$n_months_last_bought_c4bda5 <- as.numeric(data$n_months_last_bought_c4bda5)
data$n_months_last_bought_507c37 <- as.numeric(data$n_months_last_bought_507c37)
data$n_months_last_bought_32c74c <- as.numeric(data$n_months_last_bought_32c74c)

data <- data %>% 
  mutate(n_months_last_boughtX =0, .before = n_months_last_bought_839f8a)
data$n_months_last_boughtX <- rowSums(data %>% select(n_months_last_bought_839f8a,
                                                      n_months_last_bought_e22a6a,
                                                      n_months_last_bought_d0adeb,
                                                      n_months_last_bought_c4bda5,
                                                      n_months_last_bought_507c37,
                                                      n_months_last_bought_32c74c))
data <- data %>% 
  select(!c(n_months_last_bought_839f8a,
            n_months_last_bought_e22a6a,
            n_months_last_bought_d0adeb,
            n_months_last_bought_c4bda5,
            n_months_last_bought_507c37,
            n_months_last_bought_32c74c))


#cleaned n_months_last_bought
data %>% 
  select(starts_with("n_months_last_bought"))
#lapse_ape_
data %>% 
  select(starts_with("lapse_ape"))

#lapse_ape_ltc
data %>% 
  select(starts_with("lapse_ape_ltc")) %>% 
  filter(is.na(lapse_ape_ltc_43b9d5))

data <- data %>% 
  select(!starts_with("lapse_ape_ltc"))

#lapse_ape_grp
data %>% 
  select(starts_with("lapse_ape_grp"))

data$lapse_ape_grp_6fc3e6[is.na(data$lapse_ape_grp_6fc3e6)] <- 0
data$lapse_ape_grp_de05ae [is.na(data$lapse_ape_grp_de05ae )] <- 0
data$lapse_ape_grp_945b5a [is.na(data$lapse_ape_grp_945b5a )] <- 0
data$lapse_ape_grp_6a5788[is.na(data$lapse_ape_grp_6a5788)] <- 0
data$lapse_ape_grp_9cdedf [is.na(data$lapse_ape_grp_9cdedf )] <- 0
data$lapse_ape_grp_1581d7 [is.na(data$lapse_ape_grp_1581d7 )] <- 0
data$lapse_ape_grp_22decf [is.na(data$lapse_ape_grp_22decf )] <- 0
data$lapse_ape_grp_caa6ff [is.na(data$lapse_ape_grp_caa6ff )] <- 0
data$lapse_ape_grp_fd3bfb [is.na(data$lapse_ape_grp_fd3bfb )] <- 0
data$lapse_ape_grp_70e1dd [is.na(data$lapse_ape_grp_70e1dd )] <- 0
data$lapse_ape_grp_e04c3a [is.na(data$lapse_ape_grp_e04c3a )] <- 0
data$lapse_ape_grp_fe5fb8 [is.na(data$lapse_ape_grp_fe5fb8 )] <- 0
data$lapse_ape_grp_94baec [is.na(data$lapse_ape_grp_94baec )] <- 0
data$lapse_ape_grp_e91421 [is.na(data$lapse_ape_grp_e91421 )] <- 0

data <- data %>% 
  mutate(lapse_ape_grp = pmax(lapse_ape_grp_6fc3e6,
                              lapse_ape_grp_de05ae,
                              lapse_ape_grp_945b5a,
                              lapse_ape_grp_6a5788,
                              lapse_ape_grp_9cdedf,
                              lapse_ape_grp_1581d7,
                              lapse_ape_grp_22decf,
                              lapse_ape_grp_caa6ff,
                              lapse_ape_grp_fd3bfb,
                              lapse_ape_grp_70e1dd,
                              lapse_ape_grp_e04c3a,
                              lapse_ape_grp_fe5fb8,
                              lapse_ape_grp_94baec,
                              lapse_ape_grp_e91421))

data <- data %>% 
  select(!starts_with("lapse_ape_grp_"))

#lapse_ape_inv
data %>% 
  select(starts_with("lapse_ape_inv")) %>% 
  distinct(lapse_ape_inv_e9f316)

data %>% 
  filter(lapse_ape_inv_e9f316 != 0)

data <- data %>% 
  select(!starts_with("lapse_ape_inv"))


#lapse_ape_lh
data %>% 
  select(starts_with("lapse_ape_lh"))

data<- data %>% 
  select(!lapse_ape_lh_d0adeb)

data$lapse_ape_lh_507c37[is.na(data$lapse_ape_lh_507c37)] <- 0
data$lapse_ape_lh_839f8a  [is.na(data$lapse_ape_lh_839f8a  )] <- 0
data$lapse_ape_lh_e22a6a  [is.na(data$lapse_ape_lh_e22a6a  )] <- 0
data$lapse_ape_lh_f852af[is.na(data$lapse_ape_lh_f852af)] <- 0
data$lapse_ape_lh_947b15  [is.na(data$lapse_ape_lh_947b15  )] <- 0

data <- data %>% 
  mutate(lapse_ape_lh = 0, .before = lapse_ape_lh_507c37)

data$lapse_ape_lh <- rowSums(data %>% select(starts_with("lapse_ape_lh")))

data <- data %>% 
  select(!starts_with("lapse_ape_lh_"))

#lapse_ape
data %>% 
  select(starts_with("lapse_ape")) %>% 
  distinct(lapse_ape_32c74c)

data <- data %>% select(!lapse_ape_32c74c)

#cleaned lapse_ape
data %>% 
  select(starts_with("lapse_ape"))

#n_months_since_lapse_
data %>% 
  select(starts_with("n_months_since_lapse"))

#n_months_since_lapse_ltc
data %>% 
  select(starts_with("n_months_since_lapse_ltc")) %>% 
  distinct(n_months_since_lapse_ltc_43b9d5)

data <- data %>% 
  select(!n_months_since_lapse_ltc_1280bf)

data$n_months_since_lapse_ltc_43b9d5[is.na(data$n_months_since_lapse_ltc_43b9d5)] <- "9999"
data$n_months_since_lapse_ltc_43b9d5[data$n_months_since_lapse_ltc_43b9d5 == "9999"] <- "0"
data$n_months_since_lapse_ltc_43b9d5 <- as.numeric(data$n_months_since_lapse_ltc_43b9d5)

data <- data %>% 
  rename(n_months_since_lapse_ltc = n_months_since_lapse_ltc_43b9d5 )

#n_months_since_lapse_grp
#no idea what the values are so dropped
data %>% 
  select(starts_with("n_months_since_lapse_grp"))
data <- data %>% 
  select(!starts_with("n_months_since_lapse_grp"))
#n_months_since_lapse_inv
data %>% 
  select(starts_with("n_months_since_lapse_inv")) %>% 
  distinct(n_months_since_lapse_inv_e9f316)

data %>% 
  filter(n_months_since_lapse_inv_e9f316 != "9999") %>% 
  select(n_months_since_lapse_inv_e9f316, f_purchase_lh)

data <- data %>% 
  select(!starts_with("n_months_since_lapse_inv"))

#n_months_since_lapse_lh
data %>% 
  select(starts_with("n_months_since_lapse_lh")) %>% 
  distinct(n_months_since_lapse_lh_507c37)

data <- data %>% 
  select(!starts_with("n_months_since_lapse_lh"))

#n_months_since_lapse
data %>% 
  select(starts_with("n_months_since_lapse")) %>% 
  distinct(n_months_since_lapse_32c74c)

data <- data %>% 
  select(!n_months_since_lapse_32c74c)

#cleaned n_months_since_lapse
data %>% 
  select(starts_with("n_months_since_lapse")) %>% 
  distinct(n_months_since_lapse_ltc)
#missed data

#recency_hlthclaim_success
data$recency_hlthclaim_success[is.na(data$recency_hlthclaim_success)] <- 0

#recency_hlthclaim_unsuccess
data$recency_hlthclaim_unsuccess[is.na(data$recency_hlthclaim_unsuccess)] <- 0

#recency_giclaim_success
data <- data %>% 
  select(!recency_giclaim_success)

#recency_giclaim_unsuccess
data <- data %>% 
  select(!recency_giclaim_unsuccess)
names(data)



data
for (i in 1:ncol(data)){
  if(sum(is.na(data[,i])) != 0){
    print(data[,i])
  }
}


write_parquet(data,"catB_train_final_clean.parquet" )

