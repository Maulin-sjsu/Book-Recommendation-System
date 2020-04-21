df = read.csv('books.csv')

options(warn=-1)

install.packages(c('stringr', 'data.table', 'ggplot2', 'dplyr', 'caret', 'quanteda', 'rpart', 'janitor', 'partykit'))


#Loading packages
library(stringr)    #Strings operations
library(data.table) #Transposing dataframe
library(ggplot2)    #Visualisation
library(dplyr)      #Dataframe operations
library(caret)      #Models
library(quanteda)   #Tokenizing text into words
library(rpart)      #Decision Tree
library(janitor)    #Validating colnames of dataframe
library(partykit)   #Visualizing Decision Tree


head(df)

df$average_rating <- as.numeric(levels(df$average_rating))[df$average_rating]
colnames(df)[8] <- 'Num_pages'
df$Num_pages <- as.numeric(levels(df$Num_pages))[df$Num_pages]
df$title <- as.character(levels(df$title))[df$title]

#Ensuring that num_page is above 0
colnames(df)[8] <- 'Num_pages'
df <- filter(df, df$Num_pages>0)

# 4 records have a value 'NOT A BOOK' in a feature author, so we'll set it to NA
which(df$authors=='NOT A BOOK')
df$authors <- as.character(df$authors)
df <- dplyr:::filter(df, df$authors!='NOT A BOOK')

#Ensuring that 'average_rating' consist of digits
df<- dplyr:::filter(df, str_detect(df$average_rating, '^[0-9].*[0-9]$'))

#Lastly, let's see how many NA's there are; and delete them
sapply(df, function(x) length(which(is.na(x))))
df <- na.omit(df)


d <- df %>%
  group_by(authors) %>%
  summarize(avg=mean(average_rating), lan = n()) %>%
  arrange(desc(lan)) %>%
  filter(avg > 4.4, lan>5)

head(d)

theme_set(theme_bw())

ggplot(d, aes(x=`authors`, y=lan, label=lan)) + 
  geom_point(stat='identity', fill="black", size=20)  +
  geom_segment(aes(y = 0, 
                   x = `authors`, 
                   yend = lan, 
                   xend = `authors`), 
               color = "black") +
  geom_text(color="white", size=5) +
  labs(title="Authors with Ratings more then 4.4 and with more then 5 Books", 
       subtitle="Normalized Ratings") + 
  ylim(0,15) +
  coord_flip()

m <- df %>%
  group_by(language_code) %>%
  summarize(avg=mean(average_rating), lan=n()) %>%
  arrange(desc(avg)) %>%
  #filter(language_code == 'en-US')
  filter(lan>6,avg>3)

head(m)

m$language_code <- c(  'Chinese',  'Japanese','multiple languages', 'Greece','Eng-Canadian', 'French', 'German','Portuguese','Eng-other','Spanish','Eng-British', 'Eng-American')

options(repr.plot.width = 18 , repr.plot.height = 10)
ggplot(m)+
  theme_classic()+
  geom_bar(aes(x=factor(m$language_code, levels= as.character(m$language_code)),
               y=lan, fill=avg), stat='identity')+
  geom_hline(yintercept=c(10, 100, 1000, 10000), linetype='dashed', alpha=0.1)+
  coord_flip()+
  scale_y_log10()+
  scale_fill_gradient(low='gray', high='black')+
  labs(y='Number of books in a given language', x='Languages', title='Number of books in each language with averaged ratings', 
       subtitle='Only languages with more than 5 books', fill='Averaged rating')+
  theme(axis.text=element_text(size=14), plot.title = element_text(size = 40, face = "bold", hjust=0.5),axis.title=element_text(size=17),
        plot.subtitle = element_text(size = 20, face = "bold", hjust=0.48), legend.title=element_text(size=17), legend.text=element_text(size=13))



options(repr.plot.width = 18 , repr.plot.height = 13)
ggplot(df, aes(x=cut(df$Num_pages,
                     breaks=c(seq(0, 1000, by=100), 2000, 3000, 6600)),
               average_rating))+
  theme_void()+
  geom_boxplot()+
  stat_summary(fun.y=median, geom="line", aes(group=1), colour="#E7B800", lw=5)  + 
  stat_summary(fun.y=median, geom="point", colour="#FC4E07", size=3)+
  coord_cartesian(ylim=c(2, 5))+
  labs(x='Number of words', y='Average rating', title='Average rating', 
       subtitle='depending on the number of words', fill='Averaged rating')+
  theme(plot.title = element_text(hjust = 0.5, size=20),
        plot.subtitle = element_text(hjust = 0.5, size=15),
        axis.text.x = element_text(angle = 90, hjust = 1))+
  scale_x_discrete(labels=c('0-100', '100-200', '200-300','300-400',
                            '400-500', '500-600', '600-700',
                            '700-800', '800-900', '900-1000',
                            '1000-2000', '2000-3000', '3000-6576'))+
  theme(axis.text=element_text(size=14), plot.title = element_text(size = 50, face = "bold", hjust=0.5),axis.title=element_text(size=17),
        plot.subtitle = element_text(size = 40, face = "bold", hjust=0.48))

