setwd("./Election Prediction Contest 2018 Data")
library(tidyverse)
library(lubridate)
library(caret)
temp = list.files(pattern="*.csv")
for (i in 1:length(temp)) assign(temp[i], read.csv(temp[i]))


#change the name

old_election <- `Previous_Elections_2002-2013.csv`

#y is the vote shares

colnames(old_election)[5] <- "y"
#clean y

old_election$y <- as.numeric(as.character(gsub("\\%", "", old_election$y)))
library(tidyverse)

old_election$Position <-
  ifelse(old_election$Position == "Winner", 1, as.numeric(as.character(old_election$Position)))

old_election <- old_election  %>% 
  select(-Assembly)

#sepate the constuency colnames

old_election <- separate(old_election,
                         constituency ,
                         into = c("constituency_id", "constituency_name
                                  "),
                         " ")

#join provinces for province effect

province <- NA_List.csv[, c(2,7)]

colnames(province)[2] <- "constituency_id"

old_election$constituency_id <- as.factor(old_election$constituency_id)

old_election$constituency_name <- as.factor(old_election$constituency_name)
# 
# province <- province[!duplicated(province),]
# 
# old_election <- inner_join(old_election, province, "constituency_id")

# old_election <- old_election %>% group_by(Year, constituency_id) %>% slice(1:4)

basic <- old_election[,c(2,3,4,5,10,13)]


###clean the test

basic_test <- NA_List.csv[, c(2,3,6,7,8)]

basic_test <- basic_test %>% mutate(Party.Affiliation = case_when(
  grepl("Pakistan Muslim League.N.", basic_test$Party.Affiliation) ~ "PML-N",
  grepl("Pakistan Muslim League", basic_test$Party.Affiliation) ~ "PML",
  grepl("Pakistan Peoples Party", basic_test$Party.Affiliation) ~ "PPPP",
  grepl("Independent", basic_test$Party.Affiliation) ~ "IND",
  grepl("Tehreek.e.Insaf", basic_test$Party.Affiliation) ~ "PTI",
  grepl("Mutthida Majlis.e.Amal Pakistan", basic_test$Party.Affiliation) ~ "MMA",
  grepl("Qaumi Watan Party", basic_test$Party.Affiliation) ~ "QWP",
  grepl("Pakistan Muslim League.Z.", basic_test$Party.Affiliation) ~ "PML-Z",
  grepl("PPPP", basic_test$Party.Affiliation) ~ "PPP",
  grepl("Tehreek Labbaik Pakistan", basic_test$Party.Affiliation) ~ "TLP",
  grepl("Pak Sarzameen Party", basic_test$Party.Affiliation) ~ "PSP",
  grepl("All Pakistan Muslim League", basic_test$Party.Affiliation) ~ "APML",
  grepl("Jamiat Ulma.e.lslam Nazryati Pakistan", basic_test$Party.Affiliation) ~ "JUI-Nazryati",
  grepl("Allah-o-Akbar Tehreek", basic_test$Party.Affiliation) ~ "AHTP",
  grepl("Pashtoonkhwa Milli", basic_test$Party.Affiliation) ~ "PKMAP",
  grepl("Awami National Party", basic_test$Party.Affiliation) ~ "ANP",
  grepl("Awami Workers Party", basic_test$Party.Affiliation) ~ "AWP",
  grepl("Majlis.-.Am", basic_test$Party.Affiliation) ~ "MMA",
  grepl("Balochistan National Party", basic_test$Party.Affiliation) ~ "BNP",
  grepl("Majlis-e-Wahdat.e.Muslimeen", basic_test$Party.Affiliation) ~ "MWMP",
  grepl("Pakistan People", basic_test$Party.Affiliation) ~ "PPP",
  grepl("Insaf", basic_test$Party.Affiliation) ~ "PTI",
  
  
  
  TRUE ~ as.character(Party.Affiliation)
))

colnames(basic_test)[2:5] <- c("Candidate", "Party", "constituency_id", "constituency_name")

###Capital the training and test set

basic$Candidate <-
  stringi::stri_trans_totitle(basic$Candidate)

basic_test$Candidate <- stringi::stri_trans_totitle(basic_test$Candidate)


###trim white spaces

basic$Candidate <- trimws(basic$Candidate)

basic_test$Candidate <- trimws(basic_test$Candidate)

name_basic <- as.data.frame(basic[,1])
colnames(name_basic)[1] <- "name_basic"
name_basic_test <- as.data.frame(basic_test[,2])
colnames(name_basic_test)[1] <- "name_basic_test"
name_basic$name_basic_test <-""

for(i in 1:dim(name_basic)[1]) {
  x <-agrep(
    name_basic$name_basic[i],
    name_basic_test$name_basic_test,
    ignore.case = TRUE,
    value = TRUE,
    max.distance = 0.03,
    useBytes = TRUE
  )
  x <- paste0(x, "")
  name_basic$name_basic_test[i] <- x
}

name_basic <- name_basic[name_basic$name_basic_test != "",]

colnames(name_basic)[1] <- "Candidate"
##join test names with the training
join_names <- old_election %>% left_join(name_basic, by = "Candidate")

###remove duplicates

join_names <- join_names[!duplicated(join_names),]

##remove NA

clean_train <- na.omit(join_names)

####clean party names

big_parties <-
  c(
    "IND",
    "PPPP",
    "PML-N",
    "PML",
    "PTI",
    "MQM",
    "JI",
    "MMA",
    "JUI",
    "MDM",
    "TTP",
    "PLM-F",
    "JUP-Noorani"
  )

clean_train <- clean_train %>% 
  filter(Party %in% big_parties)

clean_train_select <- clean_train[, c(14,3,5,6,10,12,13)]
colnames(clean_train_select)[1] <- "Candidate"
colnames(clean_train_select)[4] <- "Votes"

clean_train_select$constituency_id <- as.factor(clean_train_select$constituency_id)
clean_train_select$Candidate <- as.factor(clean_train_select$Candidate)
clean_train_select$Votes <- as.numeric(clean_train_select$Votes)


# model1 <- lm(y~Position+Party+Votes+constituency_id , data = clean_train_select)
# 
# library(nnet)
# set.seed(123)
# nnmdl <- nnet(y~Position+Party+Votes+constituency_id, clean_train_select, size=3, linout=T)
# 
# rmodpla <-
#   glm(y ~ Position + Party + log(Votes) + constituency_id, data = clean_train_select)
# 
# libray(lme4)
# mmod <-lmer(y ~ 1 + (1 |Party) + Candidate + log(Votes) + (1 |constituency_id) ,
#        clean_train_select)

clean_train_select$constituency_name <- gsub("-.*", "", clean_train_select$constituency_name)
set.seed(122)
library(rpart)
model1 <-rpart(y ~ Party + Candidate + log(Votes) + constituency_name ,
               clean_train_select)

library(doParallel)
registerDoParallel(8)
model2 <- train(y ~ Party + Candidate + log(Votes) + constituency_name ,
      data = clean_train_select, 
      method = "rpart",
      tuneGrid = data.frame(cp = c(0.01, 0.05)),
      control = rpart.control(minsplit = 10, minbucket = 50))

###clean Party colnames and include only big party

basic_test <- basic_test %>% 
  filter(Party %in% big_parties)


voters <- Voter_Distribution.csv

voters <-
  voters %>% separate(col = Constituency,
                      into = c("constituency_id", "constituency_name"),
                      " |\\(")


###clean the constituency_ids for test set

voters[!grepl("[0-9]", voters$constituency_id),]


voters[c(84,143), 2] <- c("NA-167", "NA-182")


voters <- voters[,c(2,4,5)]

voters$constituency_id <- as.factor(voters$constituency_id)
clean_test <- basic_test %>% inner_join(voters, by = "constituency_id") %>% 
 rename(Votes = Voters)
clean_test$constituency_name <-  gsub("-.*", "", clean_test$constituency_name)
clean_test <-
  clean_test %>% filter(Candidate %in% clean_train_select$Candidate) %>%
  filter(constituency_name %in% clean_train_select$constituency_name)

clean_test$Votes <- as.numeric(as.character(clean_test$Votes))
clean_test$constituency_id <- as.factor(clean_test$constituency_id)

clean_test$Candidate <- as.factor(clean_test$Candidate)
clean_test$Party <- as.factor(clean_test$Party)
clean_test$Votes <- log(clean_test$Votes)




predict(model1, clean_test)

predict_candidate <- cbind.data.frame(clean_test, prediction = predict(model1, clean_test))

a <- predict_candidate %>% group_by(constituency_id) %>% arrange(desc(prediction)) %>% 
  slice(1) %>% select(2,4)   


join_test_names <- NA_List.csv[,c(1,3,7,8)]

colnames(join_test_names)[2:4] <- c("Candidate", "constituency_id", "constituency_name")

join_test_names$Candidate <- stringi::stri_trans_totitle(join_test_names$Candidate)
join_test_names <- join_test_names[,c(1,2,3)]


b <- join_test_names %>% right_join(a, by = c("Candidate", "constituency_id"))

b <- b[,c(3,1,2)]

library(readxl)
prediction_sheet <- read_excel("Elections_Competition_2018_NA_Predict.xlsx")

colnames(prediction_sheet)[1] <- "constituency_id"

c <- prediction_sheet %>% left_join(b, by = "constituency_id") %>% select(1,4,5) 

colnames(c)[c(1,2,3)] <- c("Constituency", "Predicted Winning Serial Number", "Predicted Winning Name of Candidate")


write.csv(c, "Azam1.csv", sep = ";")
