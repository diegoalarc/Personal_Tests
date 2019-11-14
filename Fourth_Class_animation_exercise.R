install.packages("tidyverse")
install.packages("gifski")
install.packages("png")

library(gifski)
library(tidyverse)
library(png)
library(RCurl)

df <- read.csv(textConnection(getURL("https://docs.google.com/spreadsheets/d/e/2PACX-1vS0wAfZt28-tqhdxEX4EartljewLmOKpa0XCvDFgqKqibaNYuHFABsiN3NGgOIWj06tfyynEdg3q6bO/pub?gid=1579905509&single=true&output=csv")))
summary(df)

df1 <- data.frame(df[,c(2:6,10,14)], semester=1, courses=11)
df2 <- data.frame(df[,c(2:5,7,11,15)], semester=2, courses=15)
df3 <- data.frame(df[,c(2:5,8,12,16)], semester=3, courses=2)
df4 <- data.frame(df[,c(2:5,9,13,17)], semester=4, courses=3)

df.names <- c("eyecol","haircol","glasses","sex","eoExp","progExp","praesExp","semester", "courses")

names(df1) <- df.names
names(df2) <- df.names
names(df3) <- df.names
names(df4) <- df.names


df <- rbind(df1,df2,df3,df4)

summary(df)

library(ggplot2)
library(gganimate)
# scatterplot EO exp vs. coding exp
p <- ggplot(data=df, aes(y=eoExp, x=progExp, color=eyecol, size=sex))+
  geom_point(alpha=.8)

p

p + transition_time(semester) +
  ease_aes('linear')+
  # labs(title = "semester:{closest_state}")+
  shadow_wake(wake_length = 0.1, alpha = FALSE)+
  enter_fade() +
  exit_fade()

anim_save("eagle_EO_vs_prog_experiences.gif")


# along semester

anim <- p + 
  ggtitle('semester {closest_state}')+
  transition_states(semester,
                    transition_length = 2,
                    state_length = 1, wrap = F)+
  # enter_fade() + 
  # exit_shrink()+
  # exit_fade()+
  enter_grow()+
  exit_disappear()

anim

anim_save("eagle_EO_vs_prog_experiences2.gif")

# xy scatterplot with fitted lines 
library(dplyr)
library(ungeviz)

df %>% ggplot(aes(eoExp, progExp)) +
  geom_point()+
  geom_smooth(
    data=bootstrapper(20),
    aes(group = .draw),
    se =FALSE
  )+
  # stat_smooth_draws(
  #   times = 10, aes(group=stat(.draw))
  # )+
  facet_wrap(~sex)+
  transition_states(.draw,0,1)

# ########

# scatterplot by semester with non ease linear

p <- ggplot(df, aes(x = eoExp, y = progExp)) + 
  # geom_point(aes(size = praesExp), size = 3)+
  geom_jitter(aes(size = praesExp, col=semester),width = 0.5, alpha=0.75) + #col="blue",
  scale_colour_gradient(low = "blue", high = "green")
p
anim <- p + 
  transition_states(semester,
                    transition_length = 2,
                    state_length = 1)+
  ease_aes('cubic-in-out') + # Slow start and end for a smoother look
  enter_fade() + 
  exit_shrink()

anim

anim_save("eagle_EO_vs_prog_experiences_semester_transition.gif")

# ggridges - animation of subsequent ridges

library(ggridges)
p <- ggplot(df, aes(x=eoExp, y=factor(semester), fill=courses)) + 
  geom_density_ridges(alpha = 0.8, color = "white", 
                      scale = 2.5, rel_min_height = 0.01) +
  labs(x = "EO experiences", y = "Semester")+
  # guides(fill = F) + 
  theme_ridges()
p

p+transition_states(semester, transition_length = 3, state_length = 1)+
  ease_aes('linear')+
  enter_fade()+
  exit_fade()

anim_save("eagle_EO_experiences_semester_transition_Ridges.gif")

# boxplot fade by sex

p <- ggplot(data=df, aes(y=eoExp, x=sex, color==glasses))+
  geom_boxplot(alpha=.5, fill="green")+
  geom_jitter(col="blue",width = 0.3, alpha=0.5, size=3)
p

p + transition_time(semester) +
  ease_aes('linear')+
  labs(title = "semester: {round(frame_time,1)}")+
  enter_fade() +
  exit_fade()+
  ease_aes('sine-in-out')

anim_save("eagle_EO_experiences_semester_boxplot_sex.gif")


# ###########
# ###########

anim <- ggplot(df, aes(x=eoExp, y=progExp, color=eyecol, size=praesExp)) +
  geom_point() +
  labs(title = "semester: {closest_state}") +
  facet_wrap(~sex)+
  transition_states(semester, transition_length = 3, state_length = 1)+
  ease_aes('linear')+
  exit_fade()
anim

anim_save("eagle_EO_vs_prog_experiences_facet.gif")


# sex vs. eoExp
p <- ggplot(data=df, aes(y=eoExp, x=sex, color=eyecol, size=glasses))+
  geom_point(alpha=.5)

p


p + transition_time(semester) +
  ease_aes('linear')+
  labs(title = "Year: {frame_time}")+
  enter_fade() +
  exit_fade()





###

# barplot by haircol


p <- ggplot(df, aes(haircol, eoExp, fill=progExp)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
p

p + transition_states(semester, transition_length = 1, state_length = 1, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()

# barplot by semester

p <- ggplot(df, aes(semester, eoExp, fill=courses)) +
  geom_col() +
  scale_fill_distiller(palette = "Reds", direction = 1) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.grid.major.y = element_line(color = "white"),
    panel.ontop = TRUE
  )
p

p + transition_states(semester, transition_length = 1, state_length = 1, wrap = FALSE) +
  shadow_mark() +
  enter_grow() +
  enter_fade()
#examples
anim_save("eagle_barplot_EO_vs_prog_experiences.gif")