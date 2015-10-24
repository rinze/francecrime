library(xlsx)
library(reshape2)
library(XML)
library(RCurl)
library(ggplot2)
theme_set(theme_bw(14))

crime <- read.csv("data/Tableaux_4001_TS_1.csv", header = TRUE,
                  stringsAsFactors = FALSE)
crime$Index <- NULL
crime <- melt(crime, id.vars = c("Libellé.index"))
names(crime) <- c("Crime", "y_m", "number")

# Let's build a proper year var
crime$y_m <- gsub(pattern = "X", replacement = "", x = crime$y_m)
crime$year <- sapply(crime$y_m, 
                     function(x) as.numeric(strsplit(x, "_")[[1]][1]))

# Compute the average crime for that label for that year
crime_avg <- aggregate(number ~ year + Crime, crime, mean)

wikidata <- getURL("https://en.wikipedia.org/wiki/Demographics_of_France")
population <- readHTMLTable(wikidata, stringsAsFactors = FALSE)[[4]]
population <- data.frame(year = as.numeric(population[, 1]),
                         pop = as.numeric(gsub(" ", "", population[, 2])))

crime_avg <- merge(crime_avg, population)
crime_avg$norm_crime <- crime_avg$number / crime_avg$pop
crime_first <- crime_avg[crime_avg$year == 1996, c(2, 5)]
crime_last <- crime_avg[crime_avg$year == 2014, c(2, 5)]
names(crime_last) <- c("Crime", "norm_crime_last")
crime_diff <- merge(crime_first, crime_last)
crime_diff$diff <- 100 * crime_diff$norm_crime_last / crime_diff$norm_crime
crime_diff <- crime_diff[complete.cases(crime_diff), ]
crime_diff$Crime <- factor(crime_diff$Crime)
crime_diff$Crime <- reorder(crime_diff$Crime, crime_diff$diff)
crime_diff$col <- crime_diff$diff > 100

pltdiff <- ggplot(crime_diff) +
           geom_bar(aes(x = Crime, y = diff, fill = col), 
                    stat = "identity") + 
           scale_fill_manual(values = c("darkblue", "darkred")) +
           geom_hline(yintercept = 100) +
           coord_flip() + 
           ggtitle("Crime change in France (1996 - 2014)\n") +
           xlab("Crime name (in French)\n") +
           ylab("\nCrime rate in 2014 in relation to 1996 (normalized by population)\n 100 = 1996 levels") +
           annotate("text", x = 1.5, y = 500, 
                    label = "Data sources: data.gouv.fr, Wikipedia", 
                    hjust = 1, color = "lightgray") +
           theme(legend.position="none")
print(pltdiff)
ggsave(pltdiff, filename = "/tmp/france_crime.pdf", height = 20, width = 15)

