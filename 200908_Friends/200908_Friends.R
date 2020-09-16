#Load packages
library(tidyverse)
library(tidytuesdayR)
library(cowplot)
library(beyonce)
theme_set(theme_cowplot())

#Load up data
tt_data <- tt_load("2020-09-08")

#Explore the data
dat.text <- tt_data[[1]]
dat.epi.info <- tt_data[[2]]
dat.emotion <- tt_data[[3]]

#DF of utterances with the emotion, mash in episode info for shits bc we can
dat.text.2 <- dat.text %>%
  left_join(dat.emotion, by = c("season", "episode", "scene", "utterance")) %>%
  left_join(dat.epi.info, by = c("season", "episode"))

#Get a list of the main characters' first names so we can tell when they're talking about each other
main.characters <- dat.text %>%
  count(speaker, sort = TRUE) %>%
  head(6) %>%
  select(speaker) %>%
  separate(speaker, into = c("firstname", "lastname"), remove = FALSE)

#Subset the utterances to only those said by one of the main speakers
dat.text.3 <- dat.text.2 %>% filter(speaker %in% main.characters$speaker)

#Detect utterances where they talked about each other
firstnames <- main.characters$firstname
dat.matched.text <- list()

for (i in 1:length(firstnames)){
dat.matched.text[[i]] <- dat.text.3 %>%
  mutate(subject = str_extract(text, firstnames[i])) %>%
  select(speaker, subject, everything()) %>%
  filter(subject != "")%>%
  filter(!str_detect(speaker, firstnames[i]))
    }
dat.text.matched <- do.call(rbind, dat.matched.text)

#Count how often each character talks about each other
summary.talk.about <- dat.text.matched %>%
  group_by(speaker, subject, season) %>%
  count()

#Who talks about who over the seasons
g <- ggplot(data = summary.talk.about, aes(x= season, y = n, fill = speaker)) +
  geom_bar(stat = "identity") +
  geom_text(x = 1.5, y = 120, aes(label = paste0("\"", subject, "\"")), size = 5, fontface = "italic")+
  scale_fill_manual(values = beyonce_palette(18)) +
  labs(x = "Season", fill = "Speaker", y = "Number of times each character's name is spoken", main = "Most talked about friends over time")+
  facet_wrap(vars(paste0("\"", subject, "\""))) +
  scale_y_continuous(expand = c(0, 0), limits = c(0, NA))+
  scale_x_continuous(expand = c(0, 0), limits = c(0, NA),
                     breaks = c(1,2,3,4,5,6,7,8,9,10))+
  theme(strip.background = element_blank(),
        strip.text = element_blank())

print(g)

#Save out plot
#save_plot("Figures/200908_Friends.pdf", g, base_height = 8, base_width = 14, units="in")

         