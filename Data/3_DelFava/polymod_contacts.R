library(socialmixr)

data(polymod)
polymod

## Calculate average contacts per person by age group
## These are to form as the baseline number for change by participant age group

cont <- polymod$contacts
part <- polymod$participants

part <- part %>% left_join(
  cont %>% group_by(part_id) %>% summarize(tot_cont =n()),
  by="part_id"
) 



part <- part %>% mutate(tot_cont=tidyr::replace_na(tot_cont,0),
                        age_cat= ifelse(part_age<5,"0-4",
                                  ifelse(part_age <10, "5-9",
                                         ifelse(part_age < 15, "10-14",
                                                ifelse(part_age < 20, "15-19",
                                                       ifelse(part_age < 30, "20-29",
                                                              ifelse(part_age <40, "30-39",
                                                                     ifelse(part_age <50, "40-49",
                                                                            ifelse(part_age <60, "50-59",
                                                                                   ifelse(part_age <70, "60-69", "70+"
                                        ))))))))),
                         age_cat_del = ifelse(part_age>17 & part_age<25, "18-24",
                                          ifelse(part_age>24 & part_age<45, "25-44",
                                             ifelse(part_age>44 & part_age<65, "45-64",
                                                ifelse(part_age>64, "65+", NA)))))


avg_cont_age <- part %>% group_by(country,age_cat_del) %>% 
                        summarize(  n = n(),
                                    mean=mean(tot_cont,na.rm=T),
                                    se = sd(tot_cont,na.rm=T)/sqrt(n)) %>%
                        mutate(lo = mean - 1.96*se,
                               high = mean + 1.96*se)

setwd("C:/Users/cliu369/Box Sync/5.Literature/1.Social contact patterns Lit Review 2020/0_2020rapidreview/Data/3_DelFava")
write.csv(avg_cont_age,"avg_cont_age_base.csv")