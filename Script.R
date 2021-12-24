# MAVEN ANALYTICS HARRY POTTER SCRIPT

library(ggplot2)
library(tidyverse)
library(ggthemes)
library(tidytext)

setwd("~/R/Harry_Potter_Movies")
characters <- read.csv("Characters.csv")

# cleaning missing values
characters$House <- ifelse (characters$House=="","Unknown",characters$House)
characters$Gender <- ifelse (characters$Gender == "","Unknown", characters$Gender)

characters[which(characters$Character.Name %in% c("Girl","Woman", "Maid", "The Fat Lady", "Waitress", "Witch") ),]$Gender <- "Female"
characters[which(characters$Character.Name %in% c("Waiter", "Man in a painting", "Nearly Headless Nick", "Boy","Old man", "Man", "Wizard", "Centaur","Sorting Hat", "Diary", "Boy 2", "Goblin") ),]$Gender <- "Male"


characters$Patronus <- ifelse (characters$Patronus=="","Unknown",characters$Patronus)
characters$Wand..Wood. <- ifelse (characters$Wand..Wood.=="","Unknown",characters$Wand..Wood.)
characters$Wand..Core. <- ifelse (characters$Wand..Core.=="","Unknown",characters$Wand..Core.)

characters[which(characters$House == "Beauxbatons Academy of Magic"),]$House <- "Other"
characters[which(characters$House == "Durmstrang Institute"),]$House <- "Other"


characters <- data.frame(characters)

characters %>%
  filter (House != "Unknown") %>%
  ggplot () +
  geom_bar (aes(x=fct_infreq(House), fill = Gender)) + 
  labs(x= "House", y = "Totals", title = "Characters split by House and Gender", subtitle = "Now you get all why the rush to get a girl for the prom" ) +
  scale_y_continuous(breaks = seq(0, 100, by = 2)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

characters %>%
  filter (Wand..Core. != "Unknown") %>%
  ggplot () +
  geom_bar (aes(x= fct_infreq(Wand..Core.), fill = Character.Name)) +   #beware of the fct_infreq tidyverse function!
  labs(x= "Wand Core", y = "Totals", title = "Wands split by Core and Wood", subtitle = "Harry and Tom Riddle (aka Voldemort) have both a Phoenix Feather Wand" ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

characters %>%
  filter (Patronus != "Unknown") %>%
  ggplot () +
  geom_bar (aes(x= fct_infreq(Patronus), fill = Species)) +   #beware of the fct_infreq tidyverse function!
  labs(x= "Patronus", y = "Totals", title = "Patronus Form split by Species", subtitle = "Nothing to add..." ) +
  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
  theme_few() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")
###############################  END OF CHARACTERS ############################################################################################

chapters <- read.csv("Chapters.csv")
chapters <- data.frame(chapters)

chapters$Movie.ID <- as.factor (chapters$Movie.ID)

chapters %>%
  ggplot () +
  geom_bar(aes(x= Movie.ID))
#  labs(x= "Patronus", y = "Totals", title = "Patronus Form split by Species", subtitle = "Yom make your own conclusions here..." ) +
#  scale_y_continuous(breaks = seq(0, 10, by = 1)) +
#  theme_few() +
#  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")
###############################  END OF CHAPTERS ############################################################################################
dialogues <- read.csv("Dialogue.csv")
places <- read.csv("Places.csv")
movies <- read.csv("Movies.csv")
movies$Movie.ID <- movies$?..Movie.ID


dialogues_1 <- merge(dialogues, characters[ , c("Character.ID", "Character.Name", "Gender", "House")], by = "Character.ID") #OK
dialogues_2 <- merge(dialogues_1, places[ , c("Place.ID", "Place.Name")], by = "Place.ID") #OK
dialogues_3 <- merge(dialogues_2, chapters[ , c("Chapter.ID", "Chapter.Name", "Movie.ID")], by = "Chapter.ID") #OK
dialogues_4 <- merge(dialogues_3, movies[ , c("Movie.ID", "Movie.Title")], by = "Movie.ID") #OK

good_guys <- c("Harry Potter", "Lily Potter", "Ron Weasley","Hermione Granger", "Albus Dumbledore", "Rubeus Hagrid", "Minerva McGonagall", "Horace Slughorn", "Neville Longbottom", "Remus Lupin", "Alastor Moody", "Fred Weasley", "Arthur Weasley", "Sirius Black", "George Weasley", "Ginny Weasley", "Luna Lovegood", "Molly Weasley", "Dobby", "Seamus Finnigan")
bad_guys <- c("Severus Snape", "Voldemort", "Draco Malfoy", "Dolores Umbridge", "Cornelius Fudge","Vernon Dursley", "Bellatrix Lestrange", "Lucius Malfoy", "Tom Riddle", "Gilderoy Lockhart")

dialogues_4$Character.Type <- ifelse (dialogues_4$Character.Name %in% good_guys, "Good", ifelse(dialogues_4$Character.Name %in% bad_guys, "Bad", "Unknown"))

dialogues_4$Dialogue.Lenght <- str_count(dialogues_4$Dialogue, '\\w+')

dialogues_4 %>%
  filter ((Gender != "Unknown") & (Dialogue.Lenght <75)) %>%
  ggplot() +
  geom_density(aes(x=Dialogue.Lenght, fill = Gender), alpha = 0.5)

dialogues_4 %>%
  # filter (Movie.ID == 1) %>%
  arrange(Movie.ID) %>%
  ggplot () +
  geom_bar(aes(x= Movie.Title, fill = Character.Name)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "")
#facet_grid(rows = ~Movie.ID)


#load of sentiment libraries

word_library_affin <- get_sentiments("afinn") # numeric from -5 to +5
word_library_bing <- get_sentiments("bing") # negative vs positive
word_library_nrc <- get_sentiments("nrc") # feelings (trust, fear)

# OLD CODE#################################################################################
sentence_valuator <- function (sentence) {
  words <- strsplit(sentence, " ")
  val <- vector()
  for (i in unlist(words)) {
    val <- c(val,as.integer(word_library[which(word_library$word== str_replace_all(i,"[^[:alnum:]]", "")),]$value))
    #print (as.integer(word_library[which(word_library$word== str_replace_all(i,"[^[:alnum:]]", "")),]$value))
    #print(val)
  }
  return (prod(val))
}


###### FUNCTION TO SPLIT A SENTENCE INTO SINGLE WORDS#
sentence_split <- function (sentence) {
  words <- strsplit(sentence, " ")
  val <- vector("character")
  for (i in unlist(words)) {
    val <- c(val,str_replace_all(i,"[^[:alnum:]]", ""))
  }
  val
}

dialogues_4$Dialogue.Value <- sapply(dialogues_4$Dialogue,sentence_valuator)
dialogues_4$Dialogue.Class <- as.factor(case_when(
  dialogues_4$Dialogue.Value == 0 ~ "01. NEUTRAL",
  dialogues_4$Dialogue.Value > 0  ~ "02. POSITIVE",
  dialogues_4$Dialogue.Value < 0 ~ "03. NEGATIVE",
  TRUE  ~ "04. OTHER"
))
##########################################################################################



###### FUNCTION TO CREATE DATAFRAME OF EACH SINGLE WORD WITH "VALUE"
dialogues_5 <- data.frame (
  character = character(),
  place = character(),
  house = character(),
  gender = character(),
  chapter_ID = integer(),
  chapter_name = character(),
  movie = character(),
  dialogue_ID = integer(),
  word =character(),
  value_affin = integer(),
  value_bing = character(),
  value_nrc = character())

for (i in c(1:nrow(dialogues_4))) {
  words <- sentence_split (dialogues_4[i,]$Dialogue)
  aux <- length (words)
  
  for (j in c(1:aux)) {
    value_affin <- word_library_affin[which(word_library_affin$word== tolower(str_replace_all(words[j],"[^[:alnum:]]", ""))),]$value
    value_bing <- word_library_bing[which(word_library_bing$word== tolower(str_replace_all(words[j],"[^[:alnum:]]", ""))),]$sentiment
    value_nrc <- word_library_nrc[which(word_library_nrc$word== tolower(str_replace_all(words[j],"[^[:alnum:]]", ""))),]$sentiment
    
    
    dialogues_5[nrow(dialogues_5)+1,] = list(
      dialogues_4[i,]$Character.Name,
      dialogues_4[i,]$Place.Name,
      dialogues_4[i,]$House,
      dialogues_4[i,]$Gender,
      dialogues_4[i,]$Chapter.ID,
      dialogues_4[i,]$Chapter.Name,
      dialogues_4[i,]$Movie.Title,
      dialogues_4[i,]$Dialogue.ID,
      words[j],
      ifelse(length(value_affin) == 0, 0, value_affin),
      ifelse(length(value_bing) == 0, "", value_bing),
      ifelse(length(value_nrc) == 0, "", value_nrc))
  }
}


#as.factor(fct_infreq(place)
# orders the characters by number of words said

dialogues_5 %>%
  filter (value_bing != "") %>%
  ggplot (aes(x= chapter_ID, y = fct_infreq(place), color = value_bing)) +
  geom_point(alpha = 0.4)+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")


dialogues_5 %>%
  filter (value_nrc!="") %>%
  ggplot (aes(x= fct_infreq(value_nrc))) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

dialogues_5 %>%
  filter (value_nrc!="") %>%
  ggplot (aes(x= fct_infreq(house), fill= value_nrc)) +
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")


dialogues_5 %>%
  filter (value_nrc!="") %>%
  ggplot (aes(x= fct_infreq(place), fill= value_nrc)) +
  geom_bar()+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

#geom_boxplot () +
#facet_grid(movie ~.)



# SOME SELECTION OF PARAMETERS JUST TO TEST IN GGPLOT
feelings <- c("anger", "positive", "anticipation", "negative", "fear", "joy")  
place <- c("Great Hall","Headmaster's Office", "Defense Against the Dark Arts Classroom", "	The Burrow", "e	4 Privet Drive", "Griffindor Common Room", "Hagrid's Hut", "Potions Classroom", "	12 Grimmauld Place", "Ministry of Magic", "Pensieve")
titles <- c("Harry Potter and the Philosopher's Stone", "Harry Potter and the Chamber of Secrets", "Harry Potter and the Prisoner of Azkaban", "Harry Potter and the Goblet of Fire", "Harry Potter and the Order of the Phoenix", "Harry Potter and the Half-Blood Prince", "Harry Potter and the Deathly Hallows Part 1", "Harry Potter and the Deathly Hallows Part 2")


dialogues_5 %>%
  filter (house == "Ravenclaw" & value_nrc %in% feelings ) %>%
  ggplot (aes(y = as.factor(fct_infreq(character)), fill = value_nrc)) +
  geom_bar(position = "fill")+
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") 


dialogues_5 %>%
  filter (value_bing != "" & value_nrc %in% feelings & character %in% c("Harry Potter","Rubeus Hagrid", "Severus Snape","Bellatrix Lestrange", "Draco Malfoy", "Voldemort", "Tom Riddle", "Albus Dumbledore", "Hermione Granger", "Ron Weasley")   ) %>%
  ggplot (aes(y = fct_infreq(character), fill = value_nrc)) +
  geom_bar(position = "fill")+
  facet_grid( value_bing ~.)

#geom_text(aes(label = value_nrc), position=position_stack(vjust=0.5))
#theme (axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") 



# DIALOGUES 4 IS OBSOLETE
###################################################################################################################
dialogues_4 %>%
  filter (Dialogue.Value !=0) %>%
  ggplot (aes(x= log10(abs(Dialogue.Value)), y = Movie.ID, color = Dialogue.Class, label = Dialogue)) +
  geom_point(size =2) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") + 
  # geom_label() +
  scale_y_discrete (labels = titles)


dialogues_4 %>%
  filter (Character.Name %in% c("Harry Potter","Rubeus Hagrid", "Severus Snape","Bellatrix Lestrange", "Draco Malfoy", "Voldemort", "Tom Riddle", "Albus Dumbledore", "Hermione Granger", "Ron Weasley")) %>%
  ggplot () +
  geom_bar(aes(y= fct_infreq(Character.Name), fill = as.factor(Dialogue.Class)), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")

dialogues_4 %>%
  filter (House %in% c("Gryffindor", "Slytherin", "Hufflepuff","Ravenclaw")) %>%
  ggplot () +
  geom_bar(aes(y= fct_infreq(House), fill = as.factor(Dialogue.Class)), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom") +
  facet_wrap(Movie.ID ~.)

dialogues_4 %>%
  filter (Character.Name %in% c("Harry Potter", "Draco Malfoy")) %>%
  ggplot () +
  geom_bar(aes(y= Character.Name, fill = as.factor(Dialogue.Class)), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")
#facet_grid(Movie.ID ~.)

dialogues_4 %>%
  filter (Character.Name %in% c("Sirius Black", "Bellatrix Lestrange")) %>%
  ggplot () +
  geom_bar(aes(y= Character.Name, fill = as.factor(Dialogue.Class)), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1), legend.position = "bottom")
#facet_grid(Movie.ID ~.)


dialogues_4 %>%
  filter (Character.Type != "Unknown") %>%
  ggplot () +
  geom_bar(aes(y= Character.Name, fill = as.factor(Dialogue.Class)), position = "fill") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))+
  facet_grid(Character.Type ~.)
