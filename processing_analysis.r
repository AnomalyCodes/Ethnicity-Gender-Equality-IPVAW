library(tidyverse) #tidy the data
library(naniar) #observe patterns of missingness
library(mice) #multiple imputation package
library(psych) #tools for psychological analysis including Cronbach's alpha
library(pscl) #compute various pseudo-R2 measures
library(catregs) # compute coefs, SEs, ORs, CIs and % change
library(car) #contains Variance Inflation Factors (VIF) to check multicollinearity
library(interactions) #plot interactions
library(margins) #calculate marginal effects
library(gtsummary) #table
library(broom)  


WVS_data_NG <- read_delim("/Users/adaoraokwo/Desktop/SocialResearch/F00013153-WVS_Wave_7_Nigeria_Csv_v5.0.csv", delim = ";") #read file
View(WVS_data_NG)
info <- WVS_data_NG[, c(299, 301, 314, 318, 328, 338:339, 341, 22, 424, 415:416, 418, 474:476, 481, 372, 225:227)]

# Change all missing values to NA
info[info == -99 | info == -5 | info == -2 |info == -1] <- NA
sum(is.na(info)) #160
sum(apply(info, 1, function(row) any(is.na(row)))) #rows with any NAs - #126

# Transform var levels
#Gender
table(info$Q260)
info$Q260[info$Q260 == 1] <- "Male"
info$Q260[info$Q260 == 2] <- "Female"

#Marital Status
table(info$Q273)
info$Q273[info$Q273 %in% c(1, 2)] <- "Married" #married and cohabiting as married
info$Q273[info$Q273 %in% c(3, 4, 5)] <- "Formerly married" #divorced, separated, widowed
info$Q273[info$Q273 == 6] <- "Single"

#Education
table(info$Q275R)
info$Q275R[info$Q275R == 1] <- "Lower" #no education, primary & lower secondary education
info$Q275R[info$Q275R == 2] <- "Middle" #upper secondary & post-secondary non-tertiary education
info$Q275R[info$Q275R == 3] <- "Higher" #tertiary education including bachelor's, master's & doctorate

#Employment
table(info$Q279) #look at the distribution of values to decide how to reduce levels
info$Q279[info$Q279 %in% c(1, 2, 3)] <- "Working" #Full/Part Time/self-employed
info$Q279[info$Q279 %in% c(4, 5, 6, 7)] <- "Not Working" #Retired/Pensioned, Housewife, Student, Unemployed

#Income
table(info$Q288R)
info$Q288R[info$Q288R == 1] <- "Low"
info$Q288R[info$Q288R == 2] <- "Medium"
info$Q288R[info$Q288R == 3] <- "High"

#Religion 
table(info$Q289) #no denomination has just 2 values; Judaism and Hindu each has one data point, so delete as well
info <- info %>% filter(!(Q289 %in% c(0, 4, 6))) #Remove levels with no data points
info$Q289[info$Q289 == 1] <- "Catholic"
info$Q289[info$Q289 %in% c(2, 3, 8)] <- "Other Christian" #Catholic, Protestant, Orthodox, other christian denomination
info$Q289[info$Q289 == 5] <- "Muslim"

#Ethnicity
table(info$Q290) #other Africans has 1 data point and no telling what ethnic group this is
info <- info %>% filter(!(Q290 == 566998)) #remove "other Africans"
info$Q290[info$Q290 == 566001] <- "Yoruba"
info$Q290[info$Q290 %in% c(566002, 566004)] <- "Hausa/Fulani"
info$Q290[info$Q290 == 566003] <- "Igbo"
info$Q290[info$Q290 %in% c(566005, 566006, 566999)] <- "Others"

#H_URBRURAL
table(info$H_URBRURAL)
info$H_URBRURAL[info$H_URBRURAL == 1] <- "Urban"
info$H_URBRURAL[info$H_URBRURAL == 2] <- "Rural"

#G324 - Men should be outraged if their wife/partner asks them to use a condom, attitudes towards reproductive coercion
table(info$G324)
info$G324[info$G324 %in% c(1, 2)] <- "Agree"
info$G324[info$G324 %in% c(3, 4)] <- "Disagree"

#H340 - Having a son is always better than having a daughter - Son Preference
table(info$H340)
info$H340[info$H340 %in% c(1, 2)] <- "Agree"
info$H340[info$H340 %in% c(3, 4)] <- "Disagree"

# Diagnose missingness
gg_miss_upset(info)
which(colSums(is.na(info)) > 0) #which cols have missing values
vis_miss(info) #Q285 & Q288R have the highest missing values at 9 & 11 respectively
md.pattern(info, plot = TRUE, rotate.names = TRUE) #most helpful in showing patterns of missingness -12 patterns - Diagnose

# Check for MCAR - 30 patterns of missing data
mcar_test(info) #p=0.25 #p > 0.05, we fail to reject the H0 that data is MCAR so we can do listwise deletion 

# Listwise deletion of missing values
info <- info%>% filter(rowSums(is.na(info)) == 0) #delete rows with NAs

# Cronbach's alpha for latent variables
alpha(info[, c(11:13, 16)], check.keys=TRUE) #0.79 - attitudes towards delayed marriage/parenthood
alpha(info[, c(14, 15)], check.keys=TRUE) #0.92 - attitudes towards forced marriage

# Create composites 
info <- info%>%
  mutate(forced_marriage = rowMeans(.[, 14:15]))%>% #attitudes towards forced marriage based on H333 & H334
  mutate(delayed_familyStart = rowMeans(.[, c(11:13,16)]))#attitudes towards delayed marriage/parenthood based on G308, G309, G316, H335
info$forced_marriage <- round(info$forced_marriage)
info$delayed_familyStart <- round(info$delayed_familyStart)

#forced marriage
table(info$forced_marriage)
info$forced_marriage[info$forced_marriage %in% c(1,2)] <- "Agree"
info$forced_marriage[info$forced_marriage %in% c(3,4)] <- "Disagree"

#delayed family start
table(info$delayed_familyStart)
info$delayed_familyStart[info$delayed_familyStart %in% c(1,2)] <- "Agree"
info$delayed_familyStart[info$delayed_familyStart %in% c(3,4)] <- "Disagree"

# Level reductions
table(info$Q189)
info$Q189 <- ifelse(info$Q189 == 1, 0, 1) #originally had 10 levels where 1 is never justifiable and levels 2-10 justifiable at all (always justifiable or justifiable to an extent)
#chose to reduce to binary rather than 3 levels so that each level could be substantially represented, otherwise would have done 3 levels - never justifiable, justifiable to an extent & always justifiable

table(info$Q190) #violence towards children
info$Q190[info$Q190 == 1] <- "Never justifiable"
info$Q190[info$Q190 %in% c(2:10)] <- "Justifiable to a degree"

table(info$Q191) #violence towards others
info$Q191[info$Q191 == 1] <- "Never justifiable"
info$Q191[info$Q191 %in% c(2:10)] <- "Justifiable to a degree"

# Exploratory data analysis
describe(info)
hist(info$Q262)

#recode age as distribution is skewed
table(info$Q262) #18-29, 30-49, 49-100
info$Q262 <- cut(info$Q262, 
                 breaks = c(18, 29, 49, 100), 
                 labels = c("18-29", "30-49", "50-100"), 
                 right = TRUE, 
                 include.lowest = TRUE)

#summary statistics
info %>% #what ethnicity has what level of emancipative values
  group_by(Q290) %>%
  summarize(
    average_equality = mean(EQUALITY, na.rm = TRUE),
  )

# Rearrange data 
info <- info[, c(1:9, 10, 17, 22:23, 20:21, 18:19)]

# Fit logistic regression model for IPVAW justification 
#Redefine reference levels
info <- info %>% mutate_at(c(1, 3:15, 17), factor)
info$Q290 <- relevel(info$Q290, ref = "Yoruba")
info$Q190 <- relevel(info$Q190, ref = "Never justifiable")
info$Q191 <- relevel(info$Q191, ref = "Never justifiable")
info$Q273 <- relevel(info$Q273, ref = "Single")
info$Q279 <- relevel(info$Q279, ref = "Not Working")

# Bivariate analysis - ANOVA test between a categorical and numeric variable
e <- aov(EQUALITY ~ Q290, data = info)
summary(e)
plot(TukeyHSD(e))
w <- aov(EQUALITY ~ Q189, data = info) 
summary(w)

# Create the table, comparing each variable against Q189
table_bivariate <- info %>%
  tbl_summary(
    by = Q189, # Compare each variable against Q189
    statistic = all_categorical() ~ "{n} ({p}%)", # Frequencies and percentages
    missing = "no" # Exclude missing values from analysis
  ) %>%
  add_p() # Add p-values (Chi-square for categorical)

table_bivariate

# Export the table to a Word document
table_bivariate %>%
  as_flex_table() %>%
  flextable::save_as_docx(path = "/Users/adaoraokwo/Desktop/SocialResearch/bivariate_analysis.docx")

# Confirm chi-square tests
t <- table(info$Q189, info$Q289) #test diff chi-squares
chisq.test(t)

# Multivariate Analysis
#simple regression without interaction effects
model1 <- glm(Q189 ~., data = info, family = binomial) 
summary(model1)
vif(model1)
exp(model1$coefficients) #odd ratios
list.coef(model1) #compute coefs, SEs, CIs, ORs & %c
pR2(model1) #compute various pseudo-R2 measures

# Add interaction effects
model2 <- glm(Q189 ~ Q260 + Q262 + Q273 + Q275R + Q279 + Q288R + Q289 + (EQUALITY * Q290) + H_URBRURAL + G324 + H340 + forced_marriage + delayed_familyStart + Q190 + Q191, data = info, family = binomial)
summary(model2)
list.coef(model2)
vif(model2) #check multicollinearity
anova(model2, test = "Chisq")
pR2(model2)

# Plot Interactions
interactions::interact_plot(model2, "EQUALITY", modx = Q290)

# Compare models
anova(model1, model2, test = "LRT") #model2 better
summary(step(model2, direction = "both"))

# Compare Residuals
diags <- diagn(model2)
diags[1:10, c(1,3,6)]
AIC(model1, model2) #improving likelihood - likelihood improves with every added interaction term
BIC(model1, model2) #improving model parsimony - parsimony worsens with every added interaction term

# Split regression by gender
info_male <- info %>% filter(Q260 == "Male")
info_female <- info %>% filter(Q260 == "Female")
info_male <- info_male[, -1]
info_female <- info_female[, -1]

# Logistic regression for males
model_male1 <- glm(Q189 ~., data = info_male, family = binomial)
summary(model_male1)
model_male2 <- glm(Q189 ~ Q262 + Q273 + Q275R + Q279 + Q288R + Q289 + (EQUALITY * Q290) + H_URBRURAL + G324 + H340 + forced_marriage + delayed_familyStart + Q190 + Q191, data = info_male, family = binomial)
summary(model_male2)
vif(model_male2)

# Logistic regression for females
model_female1 <- glm(Q189 ~.,data = info_female, family = binomial)
summary(model_female1)
model_female2 <- glm(Q189 ~ Q262 + Q273 + Q275R + Q279 + Q288R + Q289 + (EQUALITY * Q290) + H_URBRURAL + G324 + H340 + forced_marriage + delayed_familyStart + Q190 + Q191, data = info_female, family = binomial)
summary(model_female2)
vif(model_female2)

# Predicted probabilities and marginal effects
#marginal effects for every model to be included
marg1 <- margins(model_female1)
summary(marg1)
marg2 <- margins(model_female2)
summary(marg2)
summary(margins(model_female2, data = info_female, variables = "EQUALITY", at = list(Q290 = c("Hausa/Fulani", "Yoruba", "Igbo", "Others")),  type = "response"))

marg3 <- margins(model_male1)
summary(marg3)
marg4 <- margins(model_male2)
summary(marg4)
summary(margins(model_male2, data = info_male, variables = "EQUALITY", at = list(Q290 = c("Hausa/Fulani", "Yoruba", "Igbo", "Others")),  type = "response"))

marg5 <- margins(model1)
summary(marg5)
marg6 <- margins(model2)
summary(marg6)
summary(margins(model2, variables = "EQUALITY", at = list(Q290 = c("Hausa/Fulani", "Yoruba", "Igbo", "Others")), type = "response"))

percentiles <- quantile(info$EQUALITY, probs = c(0.25, 0.5, 0.75))
summary(margins(model2, data = info, variables = c("Q260", "Q290"), at = list(EQUALITY= percentiles),  type = "response"))

