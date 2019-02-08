library(dplyr)
library(ggplot2)
library(tidyr)
library(stringr)

## TODO Read in CSV data

## All the cookie colors
cColors <- c("#ffbe35", "#3AB5E5", "#FF8831", "#862B96", "#FF2D3D", "#45B248", "#653428", "#4FC9BE", "#666666")
## provide a name for ggplot to use
cNames <- c("Savannah\nSmiles", "Trefoils\n", "Do-Si-Dos\n", "Samoas\n", "Tagalongs\n", "Thin Mints\n", "S'mores\n", "Toffee-Tastic\n", "Total Cases\n")
names(cColors) <- cNames

## cleanup the troop data
cData <- cData %>% as_tibble
names(cData) <- c("Troop", cNames[1:8])

## a list of all the troops before we gather the data
troops <- c(cData$Troop, " ")

## turn the wide data into long data for plotting
cData <- cData %>% gather(Cookie, Count, -Troop) %>%
  arrange(Troop)
sumData <- cData %>% group_by(Troop) %>%
  summarise(Count = sum(Count)) %>%
  mutate(Cookie = cNames[9]) %>%
  ungroup() %>% select(Troop, Cookie, Count)
blankData <- tibble(Troop = " ", Cookie = cNames, Count = " ")
cData <- rbind(cData, sumData, blankData)

## setup a series of points to layout the dots
xUnit <- ( 9 / 6 )
x <- c(xUnit, xUnit*3, xUnit*5)
yUnit <- 11.5 / 6
y <- c(yUnit, yUnit*3, yUnit*5)
## create a table to make concentric circles around the total
totalCircle <- tibble(Cookie = cNames, x = rep_len(xUnit*5, 9),
                      y = rep_len(yUnit, 9),
                      size = seq(75, 60, length.out = 9))

## start a pdf file to accept all of the plots
## TODO make the year naming pull from the current year
pdf("GS Cookie Delivery 2019 By Troop.pdf", width = 8.5, height = 11)

## run a loop for each girl
for(troop in troops)
{
  ## Grab a single girl and provide a location for all the dots
  tData <- cData %>% filter(Troop == troop) %>%
    mutate(x = c(x[1], x[2], x[3], x[1], x[2], x[3], x[1], x[2], x[3])) %>%
    mutate(y = c(y[3], y[3], y[3], y[2], y[2], y[2], y[1], y[1], y[1]))

  ## make the actual plot
  print(ggplot(tData, aes(x, y, colour = Cookie)) +
        ## create the dots
        geom_point(size = 70, show.legend = FALSE) +
        ## set the size of grid
        scale_x_continuous(limits = c(0, 9)) +
        scale_y_continuous(limits = c(0, 11)) +
        ## add color around the total
        annotate("point", x = totalCircle$x, y = totalCircle$y,
                 size = totalCircle$size,
                 color = cColors) +
        ## add the case count
        geom_text(aes(label = Count), show.legend = FALSE, size = 26,
                  color = "white", fontface = "bold") +
        ## add cookie names
        geom_text(aes(label = Cookie), position = position_dodge(width = 5),
                  color = "black", size = 8, vjust = 2.75) +
        ## use the colors from the cookies
        scale_color_manual(values = cColors) +
        scale_fill_manual(values = cColors) +
        ## make sure we can't skew the plot
        coord_fixed() +
        ## add a title
        ggtitle(paste0("\nTroop #", troop)) +
        ## get rid of the background
        theme_void() +
        ## format the title
        theme(plot.title = element_text(size = 60, face = "bold")))
}

## Required to stop the pdf() function
## and finish the plot
dev.off()
cData %>% mutate(Cookie = str_replace(Cookie, "\n", " "))
