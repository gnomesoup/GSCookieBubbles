library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)
## library(extrafont)

## TODO Read in csv data to "IN"

cColors <- c("#ffbe35", "#3AB5E5", "#FF8831", "#862B96", "#FF2D3D", "#45B248", "#653428", "#4FC9BE", "#666666")
cNames <- c("Savannah\nSmiles", "Trefoils\n", "Do-Si-Dos\n", "Samoas\n", "Tagalongs\n", "Thin Mints\n", "S'mores\n", "Toffee-Tastic\n", "Total\n")
names(cColors) <- cNames
cData <- IN
## cData <- cData %>% mutate(sum = rowSums(.[2:9]))
cData <- cData %>% as_tibble
names(cData) <- c("Girl", cNames[1:8])
girls <- c(cData$Girl, " ")
cData <- cData %>% gather(Cookie, Count, -Girl) %>%
  arrange(Girl) %>%
  mutate(Case = Count %/% 12) %>%
  mutate(Box = Count %% 12)
totals <- cData %>% group_by(Girl) %>%
  summarise(Count = sum(Count), Case = sum(Case), Box = sum(Box)) %>%
  ungroup() %>%
  mutate(Cookie = "Total\n") %>%
  select(Girl, Cookie, Count, Case, Box)
blankData <- tibble(Girl = " ", Cookie = cNames, Count = "  ", Case = "  ", Box = "  ")
cData <- rbind(cData, totals, blankData) %>% as_tibble() %>%
  mutate(Case = paste(Case, "c")) %>%
  mutate(Box = paste(Box, "b"))

xUnit <- ( 9 / 6 )
x <- c(xUnit, xUnit*3, xUnit*5)
yUnit <- 11.5 / 6
y <- c(yUnit, yUnit*3, yUnit*5)
totalCircle <- tibble(Cookie = cNames, x = rep_len(xUnit*5, 9),
                      y = rep_len(yUnit, 9),
                      size = seq(75, 60, length.out = 9))

## TODO make the year naming pull from the current year
pdf("GS Cookie Delivery 2019 By Girl.pdf", width = 8.5, height = 11)
for(girl in girls)
{
  gData <- cData %>% filter(Girl == girl) %>%
    mutate(x = c(x[1], x[2], x[3], x[1], x[2], x[3], x[1], x[2], x[3])) %>%
    mutate(y = c(y[3], y[3], y[3], y[2], y[2], y[2], y[1], y[1], y[1]))

  print(
    ggplot(gData, aes(x, y, colour = Cookie)) +
    geom_point(size = 70, show.legend = FALSE) +
    scale_x_continuous(limits = c(0, 9)) +
    scale_y_continuous(limits = c(0, 11)) +
    annotate("point", x = totalCircle$x, y = totalCircle$y,
             size = totalCircle$size,
             color = cColors) +
    geom_text(aes(label = Case), show.legend = FALSE, size = 15,
              color = "white", fontface = "bold",
              nudge_y = 0.37) +
    geom_text(aes(label = Box), show.legend = FALSE, size = 15,
              color = "white",
              nudge_y = -0.37) +
    geom_text(aes(label = Cookie), position = position_dodge(width = 5),
              color = "black", size = 8, vjust = 2.75) +
    scale_color_manual(values = cColors) +
    scale_fill_manual(values = cColors) +
    coord_fixed() +
    ggtitle(paste("\n#23057:", girl)) +
    theme_void() +
    theme(plot.title = element_text(size = 50, face = "bold"))
  )
}

dev.off()
cData %>% mutate(Cookie = str_replace(Cookie, "\n", " "))
