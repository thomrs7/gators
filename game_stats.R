library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape)
library(XML)
library(rvest)

tables = readHTMLTable("http://www.gatorzone.com/football/bios.php")
names(tables)
roster <- tables[[1]]

doc <- 'http://www.gatorzone.com/football/boxlist.php?boxfile=20150905193000'
raw_data <- readHTMLTable(doc)
score <- raw_data[[3]]
summary <- raw_data[[4]]
team <- raw_data[[7]]
plays <- raw_data[[46]]

table  <- getNodeSet(htmlParse(doc),"//table") [[28]]  
drives <- readHTMLTable(table,trim = TRUE, stringsAsFactors = FALSE,
                    skip.rows=c(1),
                    header=TRUE
                    )

drives <- data.frame(lapply(drives, function(x) gsub('Â','',x)    ))

names(drives) <- gsub('Â','',names(drives))

drives$TOP <- as.duration(drives$TOP)


ggplot(drives, aes(Team,TOP)) + geom_boxplot( aes(fill=Team)) + 
    geom_jitter( aes(shape=How.Lost), size=4, color='#FF4A00') +
    scale_fill_manual(values = c("#990000","#0021A5")) +
    scale_shape_manual(values = c(0,1,2,3,4,5,11)) + 
    facet_wrap(~ Qtr,  ncol = 2) +
    ggtitle("New Mexico State vs Florida (Sep 05, 2015)") +
    ylab("Time of Position")





