
df_yes <- 
  data.frame(
    tv = rep("yes", 1000),
    score = rnorm(n = 1000, mean = 500, sd = 25)
  )

df_no <- 
  data.frame(
    tv = rep("no", 50),
    score = rnorm(n = 50, mean = 520, sd = 25)
  )

df <- rbind(df_yes, df_no)

df_new <- df
df_new$tv <- ordered(df$tv)

summary(lm(score ~ tv, data = df))

summary(lm(score ~ tv, data = df_new))
summary(df_yes)
summary(df_no)

library(maps)
world_map <- map_data("world")
names(world_map)[5] <- "country"

p_country <-

p_country <- ddply(p, "country", summarise, total = length(country))

p_map <- merge(p_country, world_map, by = "country", all = T)
p_map <- p_map[order(p_map$order), ]

ggplot(world_map, aes(long, lat)) +
  geom_polygon(aes(group = group), colour = "grey60", size = .3) +
  ylim(-55, 85) 
  

