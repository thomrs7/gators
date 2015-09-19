library(data.table)
library(plyr)
library(ggplot2)
library(dplyr)
library(lubridate)
library(reshape)
library(XML)
library(rvest)


get_time <- function( t ){
    return t
    
}

tables = readHTMLTable("http://www.gatorzone.com/football/bios.php")
names(tables)
roster <- tables[[1]]

New_Mexico <- 'http://www.gatorzone.com/football/boxlist.php?boxfile=20150905193000'
East_Carolina <- 'http://www.gatorzone.com/football/boxlist.php?boxfile=20150912190000'

raw_data <- readHTMLTable(East_Carolina)
score <- raw_data[[3]]
summary <- raw_data[[4]]
team <- raw_data[[7]]
plays <- raw_data[[46]]

table  <- getNodeSet(htmlParse(doc),"//table") [[28]]  
drives <- readHTMLTable(table,trim = TRUE, stringsAsFactors = FALSE,
                    skip.rows=c(1),header=TRUE
                    )

drives <- data.frame(lapply(drives, function(x) gsub('Â','',x) ))
names(drives) <- gsub('Â','',names(drives))
drives$TOP <- as.duration(drives$TOP)

ggplot(drives, aes(Team,TOP)) + geom_boxplot( aes(fill=Team)) + 
    geom_jitter( aes(shape=How.Lost), size=4, color='#FF4A00') +
    scale_fill_manual(values = c("#990000","#0021A5")) +
    scale_shape_manual(values = c(0,1,2,3,4,5,11)) + 
    facet_wrap(~ Qtr,  ncol = 2) + ylab("Time of Position")
    ggtitle("New Mexico State vs Florida (Sep 05, 2015)") 

summary <- mutate(summary,
                  V1 = gsub('Â','',V1),
                  V2 = gsub('Â','',V2),
                  V3 = gsub('Â','',V3),
                  V4 = gsub('Â','',V4),
                  V5 = gsub('Â','',V5)
                  )

summary.duration <- filter(summary, V3 == "UF      " | V3 == "NMSU    ")


time_to_score <- ddply(summary.duration, c('V2','V3'), summarise,
                    minutes = as.numeric(strsplit(V2,c(":"))[[1]][1]),
                    seconds = as.numeric(strsplit(V2,c(":"))[[1]][2])
                
)

time_to_score <- ddply(time_to_score , c('V3'), summarise,
                       minutes = sum(minutes),
                       seconds = sum(seconds),
                       count = length(V3)
)

time_to_score <- ddply(time_to_score , c('V3'), summarise,
                       avg = as.duration(minutes(minutes) +seconds(seconds)  ) / count
)

time_to_score




