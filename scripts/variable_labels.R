# DEFINE VECTORS OF OUTCOMES AND GENERATE VARIABLE/OUTCOME/OUTCOME GROUP DATAFRAME ----
eth_outcomes <- c("idrank", "patronage", "etrust", 
                  "netrust", "ecconditions_group", 
                  "polinfluence_group", "treatedunfairly_group") 

pol_outcomes <- c("bribe1","knows_MP", "performance", "meeting", "pubaffairs",
                  "demosupp", "trust_opposition", "trust_rulingparty", "voted")  

stat_outcomes <- c("ec_conditions_self", "notenoughfood", "noincome", 
                   "nocleanwater", "crime", "aids",
                   "gov_manage_economy", #from pol_outcomes
                   "ec_conditions_ctry" #from pol_outcomes
                   )

pro_outcomes <- c("hostile", "uncooperative", 
                  "impatient", "suspicious")

youth_outcomes <- c("youth_needs", 
                    "youth_employment", 
                    "youth_pregnancy",
                    "youth_drugabuse", 
                    "youth_alcohol",
                    "youth_delinquency",
                    "youth_smoking")

all_outcomes <- c(eth_outcomes, pol_outcomes, stat_outcomes, pro_outcomes, youth_outcomes)

variable_labels <- 
  data.frame(label = as.factor(
    # STAT OUTCOMES
    c("Does not know someone who died of AIDS",
      "Has not feared crime in own home",
      "Has always had cash income",
      "Has always had clean water",
      "Has always had enough food",
      "Own present living conditions good",
      "Government's handling of the economy", #from pol outcomes
      "Country's economic conditions", # from pol outcomes
      # POL OUTCOMES
      "Exposure to vote buying",
      "Knows MP's name",
      "Approval of president's performance",
      "Attendance at community meetings",
      "Interest in public affairs",
      "Preference for democracy",
      "Distrust in opposition parties", #FLIPPED
      "Trust in ruling party",
      "Voted in last national election",
      # ETH OUTCOMES
      "Ethnic group is treated unfairly",
      "Ethnic group's economic conditions",
      "Ethnic group's political influence",
      "Leaders should help home community",
      "Ethnic vs. national identification", #FLIPPED
      "Trust for coethnics",
      "Distrust for non-coethnics", #FLIPPED
      # PRO OUTCOMES
      "Respondent was uncooperative",
      "Respondent was suspicious",
      "Respondent was hostile",
      "Respondent was impatient", 
      # YOUTH OUTCOMES
      "Good gov't handling of addressing needs of youth", 
      "Good gov't handling of youth employment",
      "Good gov't handling teenage pregnancy",
      "Good gov't handling of drug abuse among youth",
      "Good gov't handling of underage consumption of alcohol",
      "Good gov't handling of youth delinquency",
      "Good gov't handling of smoking among youth")),
    
    var = c(# STAT OUTCOMES
      "aids",
      "crime",
      "noincome",
      "nocleanwater",
      "notenoughfood",
      "ec_conditions_self",
      "gov_manage_economy", # from pol_outcomes
      "ec_conditions_ctry", # from pol_outcomes
      # POL OUTCOMES
      "bribe1",
      "knows_MP",
      "performance",
      "meeting",
      "pubaffairs",
      "demosupp",
      "trust_opposition",
      "trust_rulingparty",
      "voted",
      # ETH OUTCOMES
      "treatedunfairly_group",
      "ecconditions_group",
      "polinfluence_group",
      "patronage",
      "idrank",
      "etrust",
      "netrust",
      # PRO OUTCOMES
      "uncooperative",
      "suspicious",
      "hostile",
      "impatient",
      # YOUTH OUTCOMES
      "youth_needs", 
      "youth_employment", 
      "youth_pregnancy",
      "youth_drugabuse", 
      "youth_alcohol",
      "youth_delinquency",
      "youth_smoking")) %>%
  mutate(group = case_when(var %in% eth_outcomes ~ "eth_outcomes",
                           var %in% pol_outcomes ~ "pol_outcomes",
                           var %in% stat_outcomes ~ "stat_outcomes",
                           var %in% pro_outcomes ~ "pro_outcomes",
                           var %in% youth_outcomes ~ "youth_outcomes"))
  