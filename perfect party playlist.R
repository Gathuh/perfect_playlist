library(readr)
data<- read_csv("datacamp_workspace_export_2023-08-10 07_29_02.csv")
nrow(data)
str(data)

head(data)

library(janitor)
data=clean_names(data)


library(dplyr)
data1<-data %>%
  group_by(danceability )%>%
  summarise(mean_duration=mean(duration_ms))
data1
library(ggplot2)
ggplot(data1,aes(danceability ,mean_duration))+ggtitle("Mean of Duration in Danceability")+geom_col()

data %>%
  group_by(explicit)%>%
  summarise(mean_duration=mean(danceability))%>%
  ungroup()%>%
  ggplot(data,aes())

library(ggplot2)
ggplot(data,aes(danceability,loudness))+geom_point()+geom_smooth(method = "loess")
ggplot(data,aes(explicit,danceability))+geom_point()+geom_smooth(method = "loess")
# Create a histogram of danceability scores
hist(data$danceability, main="Danceability Distribution", xlab="Danceability Score")
library(caret)
confusionMatrix()
library(dplyr)

data_2<-data%>%
  mutate(danceability_numeric=as.numeric(danceability>=0.2))
data_2

table(data_2$danceability_numeric)
sum(is.na(data_2))
str(data_2)
data_2<-data_2[-8]
data_2
model<-glm(danceability_numeric~.,data=data_2,family=binomial(link=logit))
k<-lm(danceability~.,data )
print(summary(k),n=33300)
summary(model)
model_2<-lm(danceability~popularity+explicit,data = data_2)
summary(model_2)
cor(data_2$danceability,data_2$popularity)
model <- glm(danceability_numeric ~ popularity+explicit, family = binomial(link = logit), data = data_2)
summary(model)
data_2
ncol(data_2)
str(data_2)
colnames(data_2)
library(dplyr)
data_2<-data_2%>%
  mutate(track_genre=as.factor(track_genre),time_signature=as.factor(time_signature),mode=as.factor(mode),key=as.factor(key),artists=as.factor(artists))
data_2<-data_2%>%
  select(-1)
data_2
y<-lm(danceability~.,data=data_2)
summary(y)
model_2<-lm(danceability~popularity+explicit+duration_ms+energy+key+loudness+mode+speechiness+acousticness+instrumentalness+liveness+valence+
              tempo+time_signature+track_genre,data = data_2)

colnames(data_2)
summary_table <- summary(model_2)
summary_table

# Extract significant variables
significant_vars <- summary_table$coefficients[summary_table$coefficients[, "Pr(>|t|)"] < 0.05, ]

print(significant_vars)
significant_var_names <- rownames(significant_vars)
significant_var_names
cleaned_data_2_var_names<-gsub("TRUE$|FALSE|^(.*?)[^A-Za-z]+$", "\\1",significant_var_names);cleaned_data_2_var_names

cleaned_data_2_var_names2 <- gsub("track_genreafrobeat", "track_genre",cleaned_data_2_var_names);cleaned_data_2_var_names2
significant_var_names
common<-intersect(colnames(data_2),cleaned_data_2_var_names2)
common
####################################################################
# Create a formula for the model# Create a formula for the model without intercept
formula_str <- paste("danceability ~ ", paste(common, collapse = " + "))
formula_str
formula_obj <- as.formula(formula_str);formula_obj
# Create the model using the formula object
formula_obj
model_significant <- lm(formula_obj, data = data_2)
summary(model_significant)
library(car)
vif(model_significant)


library(tidymodels)
split_data_2<-initial_split(data_2,prop = 0.8)
train_data_2<-training(split_data_2)
test_data_2<-testing(split_data_2)
train_model<-lm(formula_str,data = train_data_2)
summary(train_model)
library(caret)
confusionMatrix()













significant_var_names <- rownames(significant_vars)
significant_var_names <- significant_var_names[significant_var_names != "(Intercept)"]
significant_var_names


original_var_names <- gsub("$", "", significant_var_names);original_var_names
original_var_names <- gsub("FALSE$", "", original_var_names)

# Convert the formula string to an actual formula object
###################################################################################
fruits <- c("apple", "banana", "cherry")
new_fruits <- gsub("banana", "orange", fruits)
new_fruits
grep("$",significant_var_names)
cleaned_data_2_var_names <- gsub("[^A-Za-z]", "", significant_var_names);cleaned_data_2_var_names
common_var_names <- intersect(significant_var_names, colnames(data_2));common_var_names
common_first_chars1 <- union(significant_var_names,colnames(data_2));common_first_chars1

common_first_chars <- unique(significant_var_names,colnames(data_2));common_first_chars
filtered_original_var_names <- original_var_names[grepl(paste(common_first_chars, collapse = "|"), original_var_names)]
filtered_original_var_names
######################################################################################
for (dummy_var_name in significant_var_names[grep("TRUE$", significant_var_names)]) {
  original_var_name <- gsub("$", "", dummy_var_name,significant_var_names)
  significant_var_names <- gsub(dummy_var_name, original_var_name, significant_var_names)
}
original_var_name 
significant_var_names
colnames(data_2)
#######################################################################################################protect this at all cost
cleaned_data_2_var_names<-gsub("TRUE$|FALSE|[^A-Za-z]","",significant_var_names);cleaned_data_2_var_names
cleaned_data_2_var_names2 <- gsub("track_genreafrobeat", "track_genre",cleaned_data_2_var_names);cleaned_data_2_var_names2


########^(.*?)[^A-Za-z]+$", "\\1
cleaned_data_2_var_names1<-gsub("^(.*?)[^A-Za-z]+$", "\\1",significant_var_names);cleaned_data_2_var_names1
cleaned_data_2_var_names2 <- gsub("track_genreafrobeat", "track_genre",cleaned_data_2_var_names1);cleaned_data_2_var_names2

######################################################################

cleaned_data_2_var_names1<-gsub("TRUE$|FALSE$|[^A-Za-z0-9]", "\\1",significant_var_names);cleaned_data_2_var_names1
cleaned_data_2_var_names <- gsub("TRUE$|FALSE$|[^A-Za-z0-9]", "", significant_var_names);cleaned_data_2_var_names

cleaned_data_2_var_names1<-gsub("^(.*?)[^A-Za-z]+$", "\\1",significant_var_names);cleaned_data_2_var_names1
cleaned_data_2_var_names1<-gsub("TRUE$|FALSE$|[^A-Za-z0-9]", "\\1",significant_var_names);cleaned_data_2_var_names1


cleaned_data_2_var_names1<-gsub("TRUE$|FALSE$|[^(.*?)[^A-Za-z]+$]", "\\1",significant_var_names);cleaned_data_2_var_names1

cleaned_data_2_var_names2 <- gsub("track_genreafrobeat", "track_genre", cleaned_data_2_var_names);cleaned_data_2_var_names2
gsub("artists","ar",colnames(data_2))
colnames(data_2)
################################################################################################################

cleaned_data_2_var_names<-gsub("TRUE$|FALSE|[^A-Za-z]","\\1",significant_var_names);cleaned_data_2_var_names

?ls(clr())
significant_var_names
c(significant_var_names,original_var_name)


# Print the summary of the model
summary(model_significant)


data$track_name
k<-as.factor(data$track_genre)
str(k)
str(data_2)
data_2$artists
nrow(data_2$track_name)
v<-data_2$track_name
length(v)
nrow(data)
library(forcats)
data_2$track_name <- as.numeric(data_2$track_name);data_2$track_name
data_2$album_name <- as.numeric(data_2$album_name)
data_2$artist_name <- fct_inorder(data_2$artist_name)
track_name_freq <- table(data$track_name);track_name_freq
data_2$track_name_encoded <- track_name_freq[data_2$track_name];data_2$track_name
