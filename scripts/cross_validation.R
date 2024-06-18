source("scripts/contrasts.R")
source("scripts/variable_labels.R")

# LOAD NECESSARY PACKAGES ----

library(groundhog)

cv <- c("glue", "lfe", "tidyverse", "lmtest", 
        "sandwich", "modelsummary", "boot", "caret", "rms")
groundhog.library(cv, "2021-11-01")

# To define contrasts, our coarsened age variables need to be factors
# with pre-defined levels. 
if(!is.factor(afpr$coarsened_age_10) |
   !is.factor(afpr$coarsened_age_30) |
   !is.factor(afpr$coarsened_age_35) |
   !is.factor(afpr$coarsened_age_40) ) {
  stop("Make sure that coarsened age variables are factors.")
}

# SETTING UP CONTRASTS ----

# contrast() function takes the inverse of the matrix of
# desired contrast weights. So although there are only
# two contrasts we care about, we need a square matrix,
# so we specify three contrasts, plus a row 1/4s for the constant
not_relevant <- c(0 , 0, 1,  0)
younger_int <- c( -1,   1,  0,    0 )
older_int <- c( 0,   0,   -1,   1 )
# Get inverse of contrast matrix and remove constant
contrasts_matrix <- solve(rbind(constant=1/4, not_relevant, younger_int, older_int))[ , -1]

# Setting up fixed effects as factors ----

afpr$region <- as.factor(afpr$region)
afpr$tribe <- as.factor(afpr$tribe)
afpr$enumeth <- as.factor(afpr$enumeth)
afpr$country <- as.factor(afpr$country)

# afpr <- na.omit(afpr) # doing this throws the error: 
# Error in `contrasts<-`(`*tmp*`, value = contr.funs[1 + isOF[nn]]) : 
# contrasts can be applied only to factors with 2 or more levels

# In message write: "I tried just applying na.omit to afpr but that caused
# an error in glm() because some factor variables only have 1 level aftwerwards,
# so there was a contrasts error


# All the fucking forms ----
form_base  <- paste0("{outcome_variable} ~ {age_variable} + ",
                     "noncoeth +", 
                     "age + gender + edu + ",
                     "urban + minority +",
                     "round + inhomelang + region + tribe + enumeth")

form_base_factor  <- paste0("as.factor({outcome_variable}) ~ {age_variable} + ",
                            "noncoeth +", 
                            "age + gender + edu + ",
                            "urban + minority +",
                            "round + inhomelang + country") # removed  "+ region + tribe + enumeth": just being silly with it

form_base_country  <- paste0("{outcome_variable} ~ {age_variable} + ",
                             "noncoeth +", 
                             "age + gender + edu + ",
                             "urban + minority +",
                             "round + inhomelang + country") # removed + country
# COMPARING ORIGINAL FE TO COUNTRY FE ----

set.seed(2)

cv.error <- list()

cv_fixed_effects <- 
  pmap(outcome_age_combos, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0("z_", outcome) # "z_" is standardized variables
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      print(outcome_variable)
      print(1)
      
      
      model <- glm(as.formula(glue(form_base)), data = 
                     afpr[afpr$round %in% include_round & 
                        complete.cases(afpr[ , c(outcome_variable,age_variable, 
                                                 "noncoeth", "age", "gender", "edu", "urban", 
                                                   "minority", "round", "inhomelang")]), ])
      
      model_cfe <- glm(as.formula(glue(form_base_country)), data = 
                         afpr[afpr$round %in% include_round & 
                          complete.cases(afpr[ , c(outcome_variable,age_variable, 
                                                    "noncoeth", "age", "gender", "edu", "urban", 
                                                   "minority", "round", "inhomelang")]), ])
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      print(outcome_variable)
      print(2)
      
      
      model <- glm(as.formula(glue(form_base)), data =
                     afpr[afpr$round %in% include_round & 
                             complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                      "noncoeth", "age", "gender", "edu", "urban", 
                                                        "minority", "round", "inhomelang")]), ], 
                    contrasts = contrasts_matrix_list)
      
      model_cfe <- glm(as.formula(glue(form_base_country)), data =
                         afpr[afpr$round %in% include_round & 
                                complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                         "minority", "round", "inhomelang")]), ],
                    contrasts = contrasts_matrix_list)
    }
    
    print(3)
    
    cv.error.1 <- cv.glm(afpr[afpr$round %in% include_round & 
                                  complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                           "noncoeth", "age", "gender", "edu", "urban", 
                                                           "minority", "round", "inhomelang")]), ], model, K = 10)$delta[2]
    
    print(cv.error.1)
    
    cv.error.2 <- cv.glm(afpr[afpr$round %in% include_round & 
                                  complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                           "noncoeth", "age", "gender", "edu", "urban", 
                                                           "minority", "round", "inhomelang")]), ], model_cfe, K = 10)$delta[2]
    
    print(cv.error.2)
    
    cv.error <- list(cv.error.1, cv.error.2)
    
    return(cv.error)
  })


# Test function ----

paste0("{outcome_variable} ~ {age_variable} + ",
       "noncoeth +", 
       "age + gender + edu + ",
       "urban + minority +",
       "round + inhomelang")

contrasts_matrix_list <- list(x = contrasts_matrix)
names(contrasts_matrix_list) <- "coarsened_age_30"

afpr_tidy <- afpr[afpr$round %in% 7, ] %>%
  drop_na(., any_of(c("z_youth_employment","coarsened_age_30", "noncoeth", 
                      "age", "gender", "edu", "urban", 
                    "minority", "round", "inhomelang", "country")))

table(afpr_tidy$country)
table(afpr_tidy$z_youth_pregnancy)

afpr_tidy_2 <- afpr[afpr$round %in% 3:4 & complete.cases(afpr[ , c("z_aids","coarsened_age_30", "noncoeth", "age", "gender", "edu", "urban", 
                                                                   "minority", "round", "inhomelang")]), ]
  

model <- glm(as.formula(z_aids ~ coarsened_age_30 + noncoeth + 
                          age + gender + edu + urban + 
                          minority + round + inhomelang),
             data = afpr_tidy 
             # contrasts = contrasts_matrix_list
             )


cv.error.2 <- cv.glm(afpr_tidy, model, K = 10)$delta[2]

cv.error.2

# No. of factor vars with < 10 observations ----

region_t <- table(afpr$region)
region_t <- region_t[region_t < 10]
length(region_t) #19

enumeth_t <- table(afpr$enumeth)
enumeth_t <- enumeth_t[enumeth_t < 10]
length(enumeth_t) #7

tribe_t <- table(afpr$tribe)
tribe_t <- tribe_t[tribe_t < 10]
length(tribe_t) #143

country_t <- table(afpr$country)
country_t <- country_t[country_t < 10]
length(country_t) #0

# No. of factor vars with < 5 observations ----

region_t_5 <- table(afpr$region)
region_t_5 <- region_t_5[region_t_5 < 5]
length(region_t_5) #12

enumeth_t_5 <- table(afpr$enumeth)
enumeth_t_5 <- enumeth_t_5[enumeth_t_5 < 5]
length(enumeth_t_5) #4

tribe_t_5 <- table(afpr$tribe)
tribe_t_5 <- tribe_t_5[tribe_t_5 < 5]
length(tribe_t_5) #80

country_t_5 <- table(afpr$country)
country_t_5 <- country_t_5[country_t_5 < 5]
length(country_t_5) #0

# cv.glm guts ----

function (data, glmfit, cost = function(y, yhat) mean((y - yhat)^2), 
          K = n) 
{
  call <- match.call()
  if (!exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) 
    runif(1)
  seed <- get(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  n <- nrow(data)
  if ((K > n) || (K <= 1)) 
    stop("'K' outside allowable range")
  K.o <- K
  K <- round(K)
  kvals <- unique(round(n/(1L:floor(n/2))))
  temp <- abs(kvals - K)
  if (!any(temp == 0)) 
    K <- kvals[temp == min(temp)][1L]
  if (K != K.o) 
    warning(gettextf("'K' has been set to %f", K), 
            domain = NA)
  f <- ceiling(n/K)
  s <- sample0(rep(1L:K, f), n)
  n.s <- table(s)
  glm.y <- glmfit$y
  cost.0 <- cost(glm.y, fitted(glmfit))
  ms <- max(s)
  CV <- 0
  Call <- glmfit$call
  for (i in seq_len(ms)) {
    j.out <- seq_len(n)[(s == i)]
    j.in <- seq_len(n)[(s != i)]
    Call$data <- data[j.in, , drop = FALSE]
    d.glm <- eval.parent(Call)
    p.alpha <- n.s[i]/n
    cost.i <- cost(glm.y[j.out], predict(d.glm, data[j.out, 
                                                     , drop = FALSE], type = "response"))
    CV <- CV + p.alpha * cost.i
    cost.0 <- cost.0 - p.alpha * cost(glm.y, predict(d.glm, 
                                                     data, type = "response"))
  }
  list(call = call, K = K, delta = as.numeric(c(CV, CV + cost.0)), 
       seed = seed)
}

# Function for stratified sampling ----

stratified <- function(df, column, percent){
  #split dataframe into groups based on column
  listdf<-split(df, df[[column]])
  testsubgroups<-lapply(listdf, function(x){
    #pick the number of samples per group, round up.
    numsamples <- ceiling(percent*nrow(x))
    #selects the rows
    whichones <-sample(1:nrow(x), numsamples, replace = FALSE)
    testsubgroup <-x[whichones,] 
  })  
  #combine the subgroups into one data frame
  testgroup<-do.call(rbind, testsubgroups)
  testgroup
}

testgroup<-stratified(afpr, "region", 0.1)

s <- boot:::sample0(rep(1L:10, ceiling(nrow(afpr)/10)), nrow(afpr))
table(s)
getAnywhere(sample0)

table(afpr$region)
table(afpr$region)



# COMPARING LOGISTIC/ORDERED TO LINEAR ----

cv_lin_log <- 
  pmap(outcome_age_combos, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- paste0(outcome) # "z_" is standardized variables
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.

      print(1)
      
      model_lin <- glm(as.formula(glue(form_base_country)), data = 
                         afpr[afpr$round %in% include_round & 
                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                         "minority", "round", "inhomelang", "country")]), ],
                       family = "gaussian")
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {

        print(2)
        
        model_log <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round %in% include_round & 
                                   complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                            "noncoeth", "age", "gender", "edu", "urban", 
                                                            "minority", "round", "inhomelang", "country")]), ],
                     family = "binomial")
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        print(3)
        
        model_log <- MASS::polr(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round & 
                                          complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                   "noncoeth", "age", "gender", "edu", "urban", 
                                                                   "minority", "round", "inhomelang", "country")]), ],
                            Hess = T
                            ) }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      print(4)
      
      model_lin <- glm(as.formula(glue(form_base_country)), data =
                         afpr[afpr$round %in% include_round & 
                                complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                         "minority", "round", "inhomelang", "country")]), ],
                       contrasts = contrasts_matrix_list)
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        print(5)
        
        model_log <- glm(as.formula(glue(form_base_factor)),
                     data = afpr[afpr$round %in% include_round & 
                                   complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                            "noncoeth", "age", "gender", "edu", "urban", 
                                                            "minority", "round", "inhomelang", "country")]), ],
                     family = "binomial",
                     contrasts = contrasts_matrix_list
        )
      } else {
        
        print(6)
        
        model_log <- MASS::polr(as.formula(glue(form_base_factor)),
                            method = "logistic",
                            data = afpr[afpr$round %in% include_round & 
                                          complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                                   "noncoeth", "age", "gender", "edu", "urban", 
                                                                   "minority", "round", "inhomelang", "country")]), ],
                            Hess = T
                            # contrasts = contrasts_matrix
        ) }
      
      
      
    }
    
    print(7)
    
    cv.error.1 <- cv.glm(afpr[afpr$round %in% include_round & 
                                complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                         "minority", "round", "inhomelang", "country")]), ], 
                         model_lin, K = 10)$delta[2]
    
    print(cv.error.1)
    
    cv.error.2 <- cv.glm(afpr[afpr$round %in% include_round & 
                                complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                         "minority", "round", "inhomelang", "country")]), ], 
                         model_log, K = 10)$delta[2]
    
    print(cv.error.2)
    
    cv.error <- list(cv.error.1, cv.error.2)
    
    return(cv.error)
  })

# Test functions for the second function ----

model_log <- plsRglm::plsRglm(z_aids ~ coarsened_age_10 + noncoeth +
                                age + gender + edu + urban + minority + round + inhomelang + country,
                              modele = "pls-glm-logistic",
                              data = afpr[afpr$round %in% 3:4 & 
                                            complete.cases(afpr[ , c("aids", "coarsened_age_10", 
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              Hess = T)

plsRglm::cv.plsR()

### The most recent function with rms ----

cv_lin_log <- 
  pmap(outcome_age_combos, function(outcome, age_variable, round) {
    
    if(round == "7") include_round <- 7 else include_round <- 3:4
    
    outcome_variable <- outcome # not standardizing variables
    
    print(outcome_variable)
    
    # Run model
    if(age_variable == "coarsened_age_10") {
      # Note: no contrasts for 10-year age difference model
      # because we only have one baseline of interest: same age.
      
      print(1)
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data = 
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,  # need x and y to be TRUE for validate.ols to work
                            y = TRUE
      )
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        
        print(2)
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              data = droplevels(afpr[afpr$round %in% include_round & 
                                                       complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                                "noncoeth", "age", "gender", "edu", "urban", 
                                                                                "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE)
      } else {
        # Note: no contrasts for 10-year age difference model
        # because we only have one baseline of interest: same age.
        
        print(3)
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE ) }
      
    } else {
      
      contrasts_matrix_list <- list(x = contrasts_matrix)
      names(contrasts_matrix_list) <- age_variable
      
      print(4)
      
      model_lin <- rms::ols(as.formula(glue(form_base_country)), data =
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                            x = TRUE,
                            y = TRUE
                            # contrasts = contrasts_matrix_list
      ) 
      
      if(lengths(unique(afpr[,{outcome_variable}]), use.names = FALSE)==3) {
        print(5)
        
        model_log <- rms::lrm(as.formula(glue(form_base_factor)),
                              droplevels(afpr[afpr$round %in% include_round & 
                                                complete.cases(afpr[ , c({outcome_variable},{age_variable},
                                                                         "noncoeth", "age", "gender", "edu", "urban", 
                                                                         "minority", "round", "inhomelang", "country")]), ]),
                              x = TRUE,
                              y = TRUE
                              # contrasts = contrasts_matrix_list
        )
      } else {
        
        print(6)
        
        model_log <- rms::orm(as.formula(glue(form_base_factor)),
                              data = afpr[afpr$round %in% include_round & 
                                            complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
                                                                     "noncoeth", "age", "gender", "edu", "urban", 
                                                                     "minority", "round", "inhomelang", "country")]), ],
                              x = TRUE,
                              y = TRUE
                              # contrasts = contrasts_matrix
        ) }
      
    }
    
    
    print(7)
    
    # cv.error.1 <- cv.glm(afpr[afpr$round %in% include_round & 
    #                             complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
    #                                                      "noncoeth", "age", "gender", "edu", "urban", 
    #                                                      "minority", "round", "inhomelang", "country")]), ], 
    #                      model_lin, K = 10)$delta[2]
    # 
    # print(cv.error.1)
    
    cv.error.1 <- rms::validate(model_lin)
    
    # cv.error.2 <- cv.glm(afpr[afpr$round %in% include_round & 
    #                             complete.cases(afpr[ , c({outcome_variable},{age_variable}, 
    #                                                      "noncoeth", "age", "gender", "edu", "urban", 
    #                                                      "minority", "round", "inhomelang", "country")]), ], 
    #                      model_log, K = 10)$delta[2]
    # 
    # print(cv.error.2)
    
    cv.error.2 <- rms::validate(model_log)
    
    cv.error <- list(cv.error.1, cv.error.2)
    
    return(cv.error)
  })

