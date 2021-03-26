

#Load libraries 
library(ggplot2)
library(tidyverse) # recoding
library(janitor)
library(kableExtra)


#Clean the survey 
# Save the post_stratification survey as a .rData file remember to do this 
Survey <- read.csv("/Users/dans/Documents/Github/Social_media_fake_news/Inputs/Data/113992-V1/election-media-JEP-replication/source/raw/Inputs/Data/PostElectionSurvey/Sheet_1.csv",header = TRUE, sep = ",")
```
```{r, echo=FALSE,message=FALSE,warning=FALSE}
# The first 11 coluns are not related to the survey, therefore I removed them. 
Survey_clean <-
  Survey[, -c(1:11)]


# Rename cols/variables 
# How often do you get news from the following channels. 
Survey_clean <- 
  as_tibble(Survey_clean) %>% 
  janitor::clean_names() %>% 
  rename(social_media = "how_often_do_you",# get news from social media
         web_ap = "x", #Get news from a website or app?
         partyid = "before_the_2016_election_did_you_consider_yourself_a_republican_a_democrat_an_independent_or_what",
         vote_who = "who_did_you_vote_for_in_the_2016_presidential_election",
         vote_when = "when_did_you_make_up_your_mind_about_who_to_vote_for",
         education = "what_is_the_highest_degree_or_level_of_school_you_have_completed",   
         ethnicity = "which_race_ethnicity_best_describes_you_please_choose_only_one",
         zipcode = "in_what_zip_code_is_your_home_located_please_enter_a_5_digit_zip_code_for_example_00544_or_94305_if_you_prefer_not_to_answer_that_s_ok_please_type_00000",
         social_media_use = "in_this_survey_we_ll_ask_you_about_social_media_by_which_we_mean_facebook_twitter_you_tube_reddit_and_the_like_do_you_ever_use_social_media",
         president_preference = "think_about_your_network_of_online_friends_and_other_people_you_follow_or_interact_with_on_social_media_what_share_of_them_do_you_think_preferred_the_same_presidential_candidate_as_you",
         news_time = "think_about_the_month_leading_up_to_the_2016_election_on_average_how_many_minutes_per_day_would_you_say_you_spent_reading_watching_or_listening_to_news_about_the_election",
         socialme_media_time = "of_those_minutes_how_many_were_spent_reading_watching_or_listening_to_election_news_on_social_media",
         news_source = "which_of_those_sources_was_your_strong_most_important_strong_source_of_news_and_information_about_the_2016_election",
         francis_question = "x_pope_francis_endorsed_donald_trump",
         francis_seen = "x_pope_francis_endorsed_donald_trump_1",
         francis_believe = "x_1",
         illegalarms_question = "x_the_clinton_foundation_bought_137_million_in_illegal_arms",
         illegalarms_seen = "x_the_clinton_foundation_bought_137_million_in_illegal_arms_1",
         illegalarms_believe = "x_2",
         asylum_question = "x_in_may_2016_ireland_announced_that_it_was_officially_accepting_americans_requesting_political_asylum_from_a_donald_trump_presidency", 
         asylum_seen = "x_in_may_2016_ireland_announced_that_it_was_officially_accepting_americans_requesting_political_asylum_from_a_donald_trump_presidency_1",
         aslum_believe = "x_3",
         sexring_question = "x_at_the_beginning_of_november_the_fbi_uncovered_evidence_of_a_pedophile_sex_ring_run_under_the_guise_of_the_clinton_foundation" ,
         sexring_seen = "x_at_the_beginning_of_november_the_fbi_uncovered_evidence_of_a_pedophile_sex_ring_run_under_the_guise_of_the_clinton_foundation_1" ,
         sexring_believe ="x_4" ,
         comey_question = "x_fbi_director_james_comey_s_october_28th_letter_about_new_developments_in_the_investigation_of_hillary_clinton_s_emails_went_only_to_republican_members_of_congress_and_not_to_democrats" ,
         comey_seen = "x_fbi_director_james_comey_s_october_28th_letter_about_new_developments_in_the_investigation_of_hillary_clinton_s_emails_went_only_to_republican_members_of_congress_and_not_to_democrats_1",
         comey_believe = "x_5",
         taxes_question = "x_under_donald_trump_s_tax_plan_it_is_projected_that_51_of_single_parents_would_see_their_taxes_go_up",
         taxes_seen = "x_under_donald_trump_s_tax_plan_it_is_projected_that_51_of_single_parents_would_see_their_taxes_go_up_1",
         taxes_beleve = "x_6",
         beyonce_question = "x_the_clinton_campaign_secretly_paid_musicians_beyonce_and_jay_z_62_million_to_appear_at_a_rally_in_support_of_hillary_clinton",
         beyonce_seen = "x_the_clinton_campaign_secretly_paid_musicians_beyonce_and_jay_z_62_million_to_appear_at_a_rally_in_support_of_hillary_clinton_1",
         beyonnce_believe = "x_7",
         puertorico_question = "x_donald_trump_threatened_to_deport_puerto_rican_broadway_star_lin_manuel_miranda_not_realizing_that_puerto_rico_is_a_u_s_territory_and_puerto_ricans_are_u_s_citizens",
         puertorico_seen = "x_donald_trump_threatened_to_deport_puerto_rican_broadway_star_lin_manuel_miranda_not_realizing_that_puerto_rico_is_a_u_s_territory_and_puerto_ricans_are_u_s_citizens_1",
         puertorico_believe = "x_8",
         liar_question = "x_hillary_clinton_s_first_name_was_spelled_with_an_extra_i_hilliary_with_the_word_liar_in_the_middle_on_election_ballots_printed_for_use_in_lonoke_county_arkansas",
         liar_seen = "x_hillary_clinton_s_first_name_was_spelled_with_an_extra_i_hilliary_with_the_word_liar_in_the_middle_on_election_ballots_printed_for_use_in_lonoke_county_arkansas_1",
         liar_believe = "x_9",
         scheme_question = "x_leaked_documents_reveal_that_the_clinton_campaign_planned_a_scheme_to_offer_to_drive_republican_voters_to_the_polls_but_then_take_them_to_the_wrong_place",
         shceme_seen = "x_leaked_documents_reveal_that_the_clinton_campaign_planned_a_scheme_to_offer_to_drive_republican_voters_to_the_polls_but_then_take_them_to_the_wrong_place_1",
         shceme_believe = "x_10",
         investigation_question = "fbi_director_james_comey_was_secretly_communicating_with_hillary_clinton_about_when_to_release_results_of_the_fbi_investigation_into_clinton_s_private_email_server",
         investigation_seen = "fbi_director_james_comey_was_secretly_communicating_with_hillary_clinton_about_when_to_release_results_of_the_fbi_investigation_into_clinton_s_private_email_server_1",
         investigation_believe = "x_11",
         caribbean_question = "x_clinton_foundation_staff_were_found_guilty_of_diverting_funds_to_buy_alcohol_for_expensive_parties_in_the_caribbean",
         caribbean_seen = "x_clinton_foundation_staff_were_found_guilty_of_diverting_funds_to_buy_alcohol_for_expensive_parties_in_the_caribbean_1",
         caribbean_believe = "x_12" ,
         deplorables_question = "x_hillary_clinton_said_that_you_could_put_half_of_trump_s_supporters_into_what_i_call_the_basket_of_deplorables",
         deplorables_seen = "x_hillary_clinton_said_that_you_could_put_half_of_trump_s_supporters_into_what_i_call_the_basket_of_deplorables_1",
         deplorables_believe = "x_13",
         concede_question = "x_at_the_third_presidential_debate_donald_trump_refused_to_say_whether_he_would_concede_the_election_if_he_lost",
         concede_seen = "x_at_the_third_presidential_debate_donald_trump_refused_to_say_whether_he_would_concede_the_election_if_he_lost_1",
         concede_believe = "x_14",
         rally_question = "x_the_musicians_beyonce_and_jay_z_appeared_at_a_rally_in_support_of_hillary_clinton",
         rally_seen = "x_the_musicians_beyonce_and_jay_z_appeared_at_a_rally_in_support_of_hillary_clinton_1",
         rally_believe = "x_15",
         age = "what_is_your_age",
         gender = "what_is_your_gender", 
         income = "how_much_total_combined_money_did_all_members_of_your_household_earn_last_year",
         region = "us_region"
  )



# reproduce the list of conspiracies
conspiracy_list <- read.delim("/Users/dans/Documents/Github/Social_media_fake_news/Inputs/Data/113992-V1/election-media-JEP-replication/source/raw/conspiracy-1/list_of_theories.txt",sep ="|", header = TRUE, dec =".")

#paste two columns by year
conspiracy_list$theory_new = paste(conspiracy_list$year, conspiracy_list$theory, sep=":")
```

conspiracy_list %>% 
  ggplot(aes(factor(theory_new),believe)) +
  geom_col() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60),
                   limits = rev) + # reverse order
  scale_y_continuous(expand = c(0, 0)) +
  #https://stackoverflow.com/questions/61782882/how-to-wrap-an-x-axis-label-when-using-aes-string 
  coord_flip() + 
  ylab("Share of people who believe it is true (%)") +
  #ylim(0, 60) +
  theme_bw() +
  theme(axis.title.y  = element_blank(),
        panel.grid.major = element_blank(),
        #panel.border = element_rect(colour = "black", size=1),
        axis.line = element_line(colour = "black",size = .5)) 
#https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2 

# To save the ggplot as png
# ggsave(path = "/Users/dans/Documents/Github/Social_media_fake_news/Outputs/Plots", filename = "fig1.png")


# Shares on facebook 
fb_share <- 
  read.csv("/Users/dans/Documents/Github/Social_media_fake_news/Inputs/Data/113992-V1/election-media-JEP-replication/source/raw/FakeNews/FB-shares---combined.csv")

fb_share %>% 
  select(Pro,title) %>% 
  group_by(Pro) %>% 
  summarise_each(funs(n_distinct(.))) %>% 
  kbl(caption = "Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")

fb_share[fb_share==""] <- NA

# Notice that Fb_share contains some empty rows at the end of the worksheet.Fill
# them in with NAs

fb_share %>% 
  select(Pro, FB_share) %>% 
  na.omit() %>% 
  group_by(Pro) %>% 
  summarise(total = sum(FB_share)) %>% 
  kbl(caption = "Results") %>%
  kable_classic(full_width = F, html_font = "Cambria")



test <- select(Survey_clean, matches("true"))
test %>% 
  pivot_longer(Francis_true:support_true, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response)) +
  geom_bar() +
  facet_wrap(vars(question), ncol = 3) +
  labs(x = "Response (on a 1 to 3 scale)", y = "Number of respondents")


test %>% 
  pivot_longer(Francis_true:support_true, names_to = "question", values_to = "response") %>%
  ggplot(aes(x = response, fill = question)) +
  facet_wrap(vars(question), ncol = 3) +
  geom_point(stat = "count") +
  geom_line(stat = "count") +
  labs(x = "Response (on a 1 to 5 scale)", y = "Number of respondents")


# reproduce figure 1
conspiracy_list %>% 
  ggplot(aes(factor(theory_new),believe)) +
  geom_col() +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60),
                   limits = rev) + # reverse order
  scale_y_continuous(expand = c(0, 0)) +
  #https://stackoverflow.com/questions/61782882/how-to-wrap-an-x-axis-label-when-using-aes-string 
  coord_flip() + 
  ylab("Share of people who believe it is true (%)") +
  #ylim(0, 60) +
  theme_bw() +
  theme(axis.title.y  = element_blank(),
        panel.grid.major = element_blank(),
        #panel.border = element_rect(colour = "black", size=1),
        axis.line = element_line(colour = "black",size = .5)) 
#https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2 

# To save the ggplot as png
ggsave(path = "/Users/dans/Documents/Github/Social_media_fake_news/Outputs/Plots", filename = "fig1.png")

``` 

# ranked
conspiracy_list %>% 
  ggplot(aes(x=reorder(factor(theory_new), -believe),believe)) +
  # https://stackoverflow.com/questions/25664007/reorder-bars-in-geom-bar-ggplot2-by-value
  geom_bar(stat = "identity") +
  scale_x_discrete(labels = function(x) stringr::str_wrap(x, width = 60),
                   limits = rev) + # reverse order
  scale_y_continuous(expand = c(0, 0)) +
  #https://stackoverflow.com/questions/61782882/how-to-wrap-an-x-axis-label-when-using-aes-string 
  coord_flip() + 
  ylab("Share of people who believe it is true (%)") +
  #ylim(0, 60) +
  theme_bw() +
  theme(axis.title.y  = element_blank(),
        panel.grid.major = element_blank(),
        #panel.border = element_rect(colour = "black", size=1),
        axis.line = element_line(colour = "black",size = .5)) 
#https://stackoverflow.com/questions/10861773/remove-grid-background-color-and-top-and-right-borders-from-ggplot2 

# To save the ggplot as png
ggsave(path = "/Users/dans/Documents/Github/Social_media_fake_news/Outputs/Plots", filename = "fig1.1.png")























#####################################
# EDA 

library(SmartEDA)
# Overview of the data - Type = 1
kbl(ExpData(data=Survey_clean,type=1))

# Structure of the data - Type = 2
ExpData(data=Survey_clean,type=2)


# Summary statistics for numerical variables
ExpNumStat(Survey_clean,by="A",gp=NULL,Qnt=seq(0,1,0.1),MesofShape=2,Outlier=TRUE,round=2,Nlim=10)

# Graphical representation of all numeric features
# Density plot (Univariate)
plot1 <- ExpNumViz(Survey_clean,target=NULL,nlim=10,Page=c(2,2),sample=4)
plot1[[1]]
```

frequency for all categorical independent variables
```{r}
# frequency for all categorical independent variables
ExpCTable(Survey_clean,Target=NULL,margin=1,clim=10,nlim=3,round=2,bin=NULL,per=T)

plot2 <- ExpCatViz(Survey_clean,target=NULL,col ="slateblue4",clim=10,margin=2,Page = c(2,1),sample=40)
plot2[[1]]


# Summary of continuous dependent variable
summary(Survey_clean[,"income"])

# Summary statistics when dependent variable is continuous incomoe
# (Correlation between Target variable vs all independet variables)
ExpNumStat(Survey_clean,by="A",gp="income",Qnt=seq(0,1,0.1),MesofShape=1,Outlier=TRUE,round=2)
```

Graphical representation of all numeric variables
#Note: sample=8 means randomly selected 8 scatter plots
#Note: nlim=4 means included numeric variable with unique value is more than 4
plot3 <- ExpNumViz(Survey_clean,target="income",nlim=4,scatter=FALSE,fname=NULL,col="green",Page=c(2,2),sample=8)
plot3[[1]]


plot31 <- ExpNumViz(Survey_clean,target="education",nlim=4,scatter=TRUE,fname=NULL,Page=c(2,1),sample=4)
plot31[[1]]

# frequency for all categorical independent variables by descretized partisanship
## bin=4, descretized 4 categories based on quantiles
ExpCTable(Survey_clean,Target="partyid",margin=1,clim=10,round=2,bin=4,per=F)


options(width = 150)
qqp <- ExpOutQQ(Survey_clean,nlim=10,fname=NULL,Page=c(2,2),sample=4)
qqp[[1]]

ExpParcoord(Survey_clean,Group=NULL,Stsize=NULL,Nvar=c("age","education","partyid"))