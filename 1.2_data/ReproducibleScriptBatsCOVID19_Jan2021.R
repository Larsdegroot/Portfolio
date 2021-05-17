################################################################################
#Loading packages
################################################################################
{
  require(dplyr)
  require(tm)
  require(SnowballC)
  require(wordcloud)
  require(gtrendsR)
  require(ggplot2)
  require(reshape2)
  require(reshape)
  require(dplyr)
  require(plyr)
  require(lubridate)
  require(ggwordcloud)
  require(gridExtra)
  require(newsflash)
  require(ggalt)
  require(hrbrthemes)
  require(pageviews)
  require(ISOweek)
  require(xts)
  require(fitdistrplus)
  require(corrplot)
  require(brms)
  require(mgcv)
  require(splines)
  require(INLA)
  require(inlabru)
  require(bayesdfa)
}

################################################################################
#Increase in news about bats on American TVs, from GDELT (Fig. 1)
################################################################################
#Downloading GDELT data
{
  df.news.bats <- as.data.frame.list(query_tv('bats', start_date = "2016-01-01", datacomb="combined",
                                              datanorm = "perc"))
  date.df <- ldply((strsplit(as.character(df.news.bats$date), " ")))
  df.news.bats$date <- date.df$V1
  df.news.bats$hour <- date.df$V2
  head(df.news.bats)
  table(df.news.bats$network)
  df.news.bats$date <- ymd(df.news.bats$date)
  table(df.news.bats$date)
  write.csv(df.news.bats, 
            file="~/Documents/Research_Projects/Ongoing/BatsCovid/NewAnalysis_Dec2020/Data/Weekly/USTelevision/GDELTBatsUS1620.csv")
}

#Fig.1 (2016-2020 US Tv news about bats)
{
tv.dat <- 
  read.csv(file="~/Documents/Research_Projects/Ongoing/BatsCovid/NewAnalysis_Dec2020/Data/Weekly/USTelevision/GDELTBatsUS1620.csv")
tv.dat$date <- ymd(tv.dat$date)
tv.dat[61, "date"] <- "2021-01-01"
tv.dat[61, "value"] <- NA
tv.dat[61, "X"] <- 61
ggplot() + 
  geom_line(data=tv.dat, aes(x=X, y=value), size=0.8, color="steelblue", linetype="solid") + 
  theme_bw() + 
  scale_x_continuous(breaks=c(1, 13, 25, 37, 49, 61), labels=c("2016", "2017", "2018", "2019", "2020", "2021")) +
  theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
        axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
        axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
        plot.title = element_text(size=22, face="bold")) +
  theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
  theme(strip.text = element_text(colour = "black", size=14, face="bold"))
}

################################################################################
# Wordcloud of global Google related topics and US news content (Fig. 2, but the
#complete figure was then adjusted with InkScape. However basic wordclouds are 
#here)
################################################################################
#Google
{
  #Downloading the data (related topic and queries to searches about BATS)
  {
    ############################################################
    #Italian
    ############################################################
    {
      y2016.rt <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("pipistrelli"), geo = c("IT"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      it.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      it.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      it.df.rt$coutry <- "Italy"
      it.df.rq$coutry <- "Italy"
      write.csv(it.df.rt, file="~/Downloads/Google_RelatedTopic_Italy.csv")
      write.csv(it.df.rq, file="~/Downloads/Google_RelatedQueries_Italy.csv")
    }
    
    ############################################################
    ##English (USA, AU, NZ, CA, GB, IE, ZA)
    ############################################################
    {
      #US
      y2016.rt <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("US"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      us.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      us.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      us.df.rt$coutry <- "USA"
      us.df.rq$coutry <- "USA"
      write.csv(us.df.rt, file="~/Downloads/Google_RelatedTopic_USA.csv")
      write.csv(us.df.rq, file="~/Downloads/Google_RelatedQueries_USA.csv")
      
      #CA
      y2016.rt <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("CA"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      ca.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      ca.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      ca.df.rt$coutry <- "Canada"
      ca.df.rq$coutry <- "Canada"
      write.csv(ca.df.rt, file="~/Downloads/Google_RelatedTopic_Canada.csv")
      write.csv(ca.df.rq, file="~/Downloads/Google_RelatedQueries_Canada.csv")
      
      #AU
      y2016.rt <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("AU"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      au.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      au.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      au.df.rt$coutry <- "Australia"
      au.df.rq$coutry <- "Australia"
      write.csv(au.df.rt, file="~/Downloads/Google_RelatedTopic_Australia.csv")
      write.csv(au.df.rq, file="~/Downloads/Google_RelatedQueries_Australia.csv")
      
      #NZ
      y2016.rt <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("NZ"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      nz.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      nz.df.rq <- rbind(y2018.rq, y2019.rq, y2020.rq)
      nz.df.rt$coutry <- "New Zealand"
      nz.df.rq$coutry <- "New Zealand"
      write.csv(nz.df.rt, file="~/Downloads/Google_RelatedTopic_NewZealand.csv")
      write.csv(nz.df.rq, file="~/Downloads/Google_RelatedQueries_NewZealand.csv")
      
      #ZA
      y2016.rt <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("ZA"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      za.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      za.df.rq <- rbind(y2016.rq, y2017.rq, y2020.rq)
      za.df.rt$coutry <- "South Africa"
      za.df.rq$coutry <- "South Africa"
      write.csv(za.df.rt, file="~/Downloads/Google_RelatedTopic_SouthAfrica.csv")
      write.csv(za.df.rq, file="~/Downloads/Google_RelatedQueries_SouthAfrica.csv")
      
      #GB
      y2016.rt <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("GB"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      gb.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      gb.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      gb.df.rt$coutry <- "UK"
      gb.df.rq$coutry <- "UK"
      write.csv(gb.df.rt, file="~/Downloads/Google_RelatedTopic_UK.csv")
      write.csv(gb.df.rq, file="~/Downloads/Google_RelatedQueries_UK.csv")
      
      #IE
      y2016.rt <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("bats"), geo = c("IE"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      ie.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      ie.df.rq <- rbind(y2020.rq)
      ie.df.rt$coutry <- "Ireland"
      ie.df.rq$coutry <- "Ireland"
      write.csv(ie.df.rt, file="~/Downloads/Google_RelatedTopic_Ireland.csv")
      write.csv(ie.df.rq, file="~/Downloads/Google_RelatedQueries_Ireland.csv")
    }
    
    ############################################################
    ##Spanish (ES, AR, CO, MX, CL, UY)
    ############################################################
    {
      #Spain
      y2016.rt <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("murcielagos"), geo = c("ES"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      es.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      es.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      es.df.rt$coutry <- "Spain"
      es.df.rq$coutry <- "Spain"
      write.csv(es.df.rt, file="~/Downloads/Google_RelatedTopic_Spain.csv")
      write.csv(es.df.rq, file="~/Downloads/Google_RelatedQueries_Spain.csv")
      
      #Colombia
      y2016.rt <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("murcielagos"), geo = c("CO"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      co.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      co.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      co.df.rt$coutry <- "Colombia"
      co.df.rq$coutry <- "Colombia"
      write.csv(co.df.rt, file="~/Downloads/Google_RelatedTopic_Colombia.csv")
      write.csv(co.df.rq, file="~/Downloads/Google_RelatedQueries_Colombia.csv")
      
      #Mexico
      y2016.rt <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("murcielagos"), geo = c("MX"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      mx.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      mx.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      mx.df.rt$coutry <- "Mexico"
      mx.df.rq$coutry <- "Mexico"
      write.csv(mx.df.rt, file="~/Downloads/Google_RelatedTopic_Mexico.csv")
      write.csv(mx.df.rq, file="~/Downloads/Google_RelatedQueries_Mexico.csv")
      
      #Argentina
      y2016.rt <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("murcielagos"), geo = c("AR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      ar.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      ar.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      ar.df.rt$coutry <- "Argentina"
      ar.df.rq$coutry <- "Argentina"
      write.csv(ar.df.rt, file="~/Downloads/Google_RelatedTopic_Argentina.csv")
      write.csv(ar.df.rq, file="~/Downloads/Google_RelatedQueries_Argentina.csv")
      
      #Chile
      y2016.rt <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("murcielagos"), geo = c("CL"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      cl.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      cl.df.rq <- rbind(y2018.rq, y2019.rq, y2020.rq)
      cl.df.rt$coutry <- "Chile"
      cl.df.rq$coutry <- "Chile"
      write.csv(cl.df.rt, file="~/Downloads/Google_RelatedTopic_Chile.csv")
      write.csv(cl.df.rq, file="~/Downloads/Google_RelatedQueries_Chile.csv")
      
      #Uruguay
      y2016.rt <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("murcielagos"), geo = c("UY"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      uy.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      uy.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      uy.df.rt$coutry <- "Uruguay"
      uy.df.rq$coutry <- "Uruguay"
      write.csv(uy.df.rt, file="~/Downloads/Google_RelatedTopic_Uruguay.csv")
      write.csv(uy.df.rq, file="~/Downloads/Google_RelatedQueries_Uruguay.csv")
    }
    
    ############################################################
    ##Portuguese (PT, BR)
    ############################################################
    {
      #Portugal
      y2016.rt <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("morcegos"), geo = c("PT"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      pt.df.rt <- rbind(y2016.rt, y2018.rt, y2019.rt, y2020.rt)
      pt.df.rq <- rbind(y2018.rq, y2019.rq, y2020.rq)
      pt.df.rt$coutry <- "Portugal"
      pt.df.rq$coutry <- "Portugal"
      write.csv(pt.df.rt, file="~/Downloads/Google_RelatedTopic_Portugal.csv")
      write.csv(pt.df.rq, file="~/Downloads/Google_RelatedQueries_Portugal.csv")
      
      #Brasil
      y2016.rt <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("morcegos"), geo = c("BR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      br.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      br.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      br.df.rt$coutry <- "Portugal"
      br.df.rq$coutry <- "Portugal"
      write.csv(br.df.rt, file="~/Downloads/Google_RelatedTopic_Brasil.csv")
      write.csv(br.df.rq, file="~/Downloads/Google_RelatedQueries_Brasil.csv")
    }
    
    ############################################################
    ##French (FR)
    ############################################################
    {
      #France
      y2016.rt <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("chauves souris"), geo = c("FR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      fr.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      fr.df.rq <- rbind(y2018.rq, y2020.rq)
      fr.df.rt$coutry <- "France"
      fr.df.rq$coutry <- "France"
      write.csv(fr.df.rt, file="~/Downloads/Google_RelatedTopic_France.csv")
      write.csv(fr.df.rq, file="~/Downloads/Google_RelatedQueries_France.csv")
    }
    
    ############################################################
    ##German (DE, AT)
    ############################################################
    {
      #Germany
      y2016.rt <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("Fledermaus"), geo = c("DE"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      de.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      de.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      de.df.rt$coutry <- "Germany"
      de.df.rq$coutry <- "Germany"
      write.csv(de.df.rt, file="~/Downloads/Google_RelatedTopic_Germany.csv")
      write.csv(de.df.rq, file="~/Downloads/Google_RelatedQueries_Germany.csv")
      
      #Austria
      y2016.rt <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("Fledermaus"), geo = c("AT"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      at.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      at.df.rq <- rbind(y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      at.df.rt$coutry <- "Austria"
      at.df.rq$coutry <- "Austria"
      write.csv(at.df.rt, file="~/Downloads/Google_RelatedTopic_Austria.csv")
      write.csv(at.df.rq, file="~/Downloads/Google_RelatedQueries_Austria.csv")
    }
    
    ############################################################
    #Japan (JP)
    ############################################################
    {
      y2016.rt <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("コウモリ"), geo = c("JP"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      jp.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      jp.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      jp.df.rt$coutry <- "Japan"
      jp.df.rq$coutry <- "Japan"
      write.csv(jp.df.rt, file="~/Downloads/Google_RelatedTopic_Japan.csv")
      write.csv(jp.df.rq, file="~/Downloads/Google_RelatedQueries_Japan.csv")
    }
    
    ############################################################
    #South Korea (KR)
    ############################################################
    {
      y2016.rt <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_topics#"Y-m-d Y-m-d"
      y2016.rq <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2016-01-01 2016-12-31")$related_queries#"Y-m-d Y-m-d"
      y2017.rt <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_topics#"Y-m-d Y-m-d"
      y2017.rq <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2017-01-01 2017-12-31")$related_queries#"Y-m-d Y-m-d"
      y2018.rt <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_topics#"Y-m-d Y-m-d"
      y2018.rq <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2018-01-01 2018-12-31")$related_queries#"Y-m-d Y-m-d"
      y2019.rt <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_topics#"Y-m-d Y-m-d"
      y2019.rq <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2019-01-01 2019-12-31")$related_queries#"Y-m-d Y-m-d"
      y2020.rt <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_topics#"Y-m-d Y-m-d"
      y2020.rq <- gtrends(c("박쥐"), geo = c("KR"), 
                          gprop = "web", time = "2020-01-01 2020-12-08")$related_queries#"Y-m-d Y-m-d"
      
      y2016.rt$year <- 2016
      y2017.rt$year <- 2017
      y2018.rt$year <- 2018
      y2019.rt$year <- 2019
      y2020.rt$year <- 2020
      
      y2016.rq$year <- 2016
      y2017.rq$year <- 2017
      y2018.rq$year <- 2018
      y2019.rq$year <- 2019
      y2020.rq$year <- 2020
      
      kr.df.rt <- rbind(y2016.rt, y2017.rt, y2018.rt, y2019.rt, y2020.rt)
      kr.df.rq <- rbind(y2016.rq, y2017.rq, y2018.rq, y2019.rq, y2020.rq)
      kr.df.rt$coutry <- "Korea"
      kr.df.rq$coutry <- "Korea"
      write.csv(kr.df.rt, file="~/Downloads/Google_RelatedTopic_Korea.csv")
      write.csv(kr.df.rq, file="~/Downloads/Google_RelatedQueries_Korea.csv")
    }
  }
  
  #Analysis of related topics, import the CSV file
  {
    #Loading the dataset
    df <- read.csv(file="~/Documents/Research_Projects/Ongoing/BatsCovid/Data/Related/Topic/Dataset_RelatedTopics.csv")
    
    head(df)
    df <- df[which(df$keep==1), ]
    df <- df[-which(df$value=="Bats"),]
    df <- df[-which(df$value=="Bat"),]
    
    head(df)
    y2016 <- as.data.frame(table(df[which(df$year==2016), "value"]))
    names(y2016) <- c("topic", "freq")
    head(y2016[order(-y2016$freq),], 20)
    y2016$year <- 2016
    
    y2017 <- as.data.frame(table(df[which(df$year==2017), "value"]))
    names(y2017) <- c("topic", "freq")
    head(y2017[order(-y2017$freq),], 20)
    y2017$year <- 2017
    
    y2018 <- as.data.frame(table(df[which(df$year==2018), "value"]))
    names(y2018) <- c("topic", "freq")
    head(y2018[order(-y2018$freq),], 20)
    y2018$year <- 2018
    
    y2019 <- as.data.frame(table(df[which(df$year==2019), "value"]))
    names(y2019) <- c("topic", "freq")
    head(y2019[order(-y2019$freq),], 20)
    y2019$year <- 2019
    
    y2020 <- as.data.frame(table(df[which(df$year==2020), "value"]))
    names(y2020) <- c("topic", "freq")
    head(y2020[order(-y2020$freq),], 20)
    y2020$year <- 2020
    
    y.dat <- rbind(head(y2016, 20), head(y2017, 50), head(y2018, 50), head(y2019, 50),
                   head(y2020, 50))
    
    wordcloud(words = y2016$topic, freq = y2016$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.1, 
              colors=brewer.pal(8, "Dark2"))
    
    wordcloud(words = y2017$topic, freq = y2017$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.1, 
              colors=brewer.pal(8, "Dark2"))
    
    wordcloud(words = y2018$topic, freq = y2018$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.1, 
              colors=brewer.pal(8, "Dark2"))
    
    wordcloud(words = y2019$topic, freq = y2019$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.1, 
              colors=brewer.pal(8, "Dark2"))
    
    wordcloud(words = y2020$topic, freq = y2020$freq, min.freq = 1,
              max.words=200, random.order=FALSE, rot.per=0.1, 
              colors=brewer.pal(8, "Dark2"))
    
    #Wordcloud: topics
    p1 <- ggwordcloud(y2016$topic, y2016$freq) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    p2 <- ggwordcloud(y2017$topic, y2017$freq) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    p3 <- ggwordcloud(y2018$topic, y2018$freq) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    p4 <- ggwordcloud(y2019$topic, y2019$freq) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
    
    p5 <- ggwordcloud(y2020$topic, y2020$freq) +
      theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
  }
}

#Internet Television News Archive (GDELT)
{
#Downloading the data
{
qdf.2016 <- as.data.frame(query_tv('bats"', mode = "WordCloud", start_date = "2016-01-01", "2016-12-31"))
qdf.2016 <- qdf.2016[-which(qdf.2016$label%in%c("Bats", "Bat")),]

qdf.2017 <- as.data.frame(query_tv('bats"', mode = "WordCloud", start_date = "2017-01-01", "2017-12-31"))
qdf.2017 <- qdf.2017[-which(qdf.2017$label%in%c("Bats", "Bat")),]

qdf.2018 <- as.data.frame(query_tv('bats"', mode = "WordCloud", start_date = "2018-01-01", "2018-12-31"))
qdf.2018 <- qdf.2018[-which(qdf.2018$label%in%c("Bats", "Bat")),]

qdf.2019 <- as.data.frame(query_tv('bats"', mode = "WordCloud", start_date = "2019-01-01", "2019-12-31"))
qdf.2019 <- qdf.2019[-which(qdf.2019$label%in%c("Bats", "Bat")),]

qdf.2020 <- as.data.frame(query_tv('bats"', mode = "WordCloud", start_date = "2020-01-01", "2020-12-08"))
qdf.2020 <- qdf.2020[-which(qdf.2020$label%in%c("Bats", "Bat")),]
}

#Wordclouds (they slightly change over multiple trials)
{
ggwordcloud(qdf.2016$label, qdf.2016$count) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggwordcloud(qdf.2017$label, qdf.2017$count) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggwordcloud(qdf.2018$label, qdf.2018$count) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggwordcloud(qdf.2019$label, qdf.2019$count) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
ggwordcloud(qdf.2020$label, qdf.2020$count) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1))
}
}

################################################################################
#Correlation between time series of tv broadcasts, GoogleTrends and Wikipedia
#visits, about bats and coronavirus. Daily data (Fig. 3 and Fig. 4)
################################################################################
#Downloading the data
{
  d1 <- as.data.frame.list(query_tv('coronavirus', start_date = "2020-01-01", end_date = "2020-09-01",
                                    datacomb="combined", datanorm = "perc"))
  date.df <- ldply((strsplit(as.character(d1$date), " ")))
  d1$date <- date.df$V1
  d1$hour <- date.df$V2
  d1$date <- ymd(d1$date)
  d1.scaled <- d1
  max.val <- max(d1.scaled$value)
  d1.scaled$value <- round((d1.scaled$value/max.val)*100, 0)
  
  d2 <- as.data.frame.list(query_tv(c('bats'), start_date = "2020-01-01", end_date = "2020-09-01",
                                    datacomb="combined", datanorm = "perc"))
  date.df <- ldply((strsplit(as.character(d2$date), " ")))
  d2$date <- date.df$V1
  d2$hour <- date.df$V2
  d2$date <- ymd(d2$date)
  d2.scaled <- d2
  max.val <- max(d2.scaled$value)
  d2.scaled$value <- round((d2.scaled$value/max.val)*100, 0)
  
  d34 <- gtrends(c("coronavirus", "bats"), geo = c("US"), 
                 gprop = "web", time = "2020-01-01 2020-09-01")[[1]]
  d34$date <- ymd(d34$date)
  d34[which(d34$hits=="<1"), "hits"] <- "0"
  d34$hits <- as.numeric(as.character(d34$hits))
  d3 <- d34[which(d34$keyword=="coronavirus"), ]
  d4 <- d34[which(d34$keyword=="bats"), ]
  
  d3.scaled <- gtrends(c("coronavirus"), geo = c("US"), 
                       gprop = "web", time = "2020-01-01 2020-09-01")[[1]]
  d3.scaled$date <- ymd(d3.scaled$date)
  d3.scaled[which(d3.scaled$hits=="<1"), "hits"] <- "0"
  d3.scaled$hits <- as.numeric(as.character(d3.scaled$hits))
  
  d4.scaled <- gtrends(c("bats"), geo = c("US"), 
                       gprop = "web", time = "2020-01-01 2020-09-01")[[1]]
  d4.scaled$date <- ymd(d4.scaled$date)
  d4.scaled[which(d4.scaled$hits=="<1"), "hits"] <- "0"
  d4.scaled$hits <- as.numeric(as.character(d4.scaled$hits))
  
  d5 <- article_pageviews(project = "en.wikipedia", article = "Coronavirus", platform = "all", user_type = "all",
                          start = "2020010100", end = "2020090100", granularity = "daily")
  d5$date <- ymd(d5$date)
  d5.scaled <- d5
  max.val <- max(d5.scaled$views)
  d5.scaled$views <- round((d5.scaled$views/max.val)*100, 0)
  
  d6 <- article_pageviews(project = "en.wikipedia", article = "Bat", platform = "all", user_type = "all",
                          start = "2020010100", end = "2020090100", granularity = "daily")
  d6$date <- ymd(d6$date)
  d6.scaled <- d6
  max.val <- max(d6.scaled$views)
  d6.scaled$views <- round((d6.scaled$views/max.val)*100, 0)
}

#Rescaled time series for the whole 2020 (TV, Google and Wikipedia)
#Black lines refer to coronavirus, blue lines to bats.
{
  grid.arrange(
    ggplot() + 
      geom_line(data=d1.scaled, aes(x=date, y=value), size=0.8, color="black", linetype="solid") + 
      geom_line(data=d2.scaled, aes(x=date, y=value), size=0.8, color="steelblue", linetype="solid") + 
      theme_bw() + 
      annotate("text", x = as.Date("2020-07-01"), y = 80, label = "GDELT", hjust=0, size=10, fontface="bold") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold")),
    ggplot() + 
      geom_line(data=d3.scaled, aes(x=date, y=hits), size=0.8, color="black", linetype="solid") + 
      geom_line(data=d4.scaled, aes(x=date, y=hits), size=0.8, color="steelblue", linetype="solid") + 
      theme_bw() + 
      annotate("text", x = as.Date("2020-07-01"), y = 80, label = "Google", hjust=0, size=10, fontface="bold") +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold")),
    ggplot() + 
      geom_line(data=d5.scaled, aes(x=date, y=views), size=0.8, color="black", linetype="solid") + 
      geom_line(data=d6.scaled, aes(x=date, y=views), size=0.8, color="steelblue", linetype="solid") + 
      theme_bw() + 
      annotate("text", x = as.Date("2020-07-01"), y = 80, label = "Wikipedia", hjust=0, size=10, fontface="bold") +
      theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold")), ncol=1)
}

#Non-rescaled time series for the whole 2020 (TV, Google and Wikipedia). You can
#appreciate that television news about bats were much less common than tv news
#about Coronavirus.
{
  grid.arrange(
    ggplot() + 
      geom_line(data=d1, aes(x=date, y=value), size=0.8, color="black", linetype="solid") + 
      geom_line(data=d2, aes(x=date, y=value), size=0.8, color="steelblue", linetype="solid") + 
      theme_bw() + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold")),
    ggplot() + 
      geom_line(data=d3, aes(x=date, y=hits), size=0.8, color="black", linetype="solid") + 
      geom_line(data=d4, aes(x=date, y=hits), size=0.8, color="steelblue", linetype="solid") + 
      theme_bw() + 
      theme(axis.text.x = element_blank(),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_blank(),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold")),
    ggplot() + 
      geom_line(data=d5, aes(x=date, y=views), size=0.8, color="black", linetype="solid") + 
      geom_line(data=d6, aes(x=date, y=views), size=0.8, color="steelblue", linetype="solid") + 
      theme_bw() + 
      theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold")), ncol=1)
}

#Trellis plots (Fig. 3). The peak of media echo is between 2020-02-23 and 2020-05-30
#In the last week of February some cases are recorded in Italy, Iran, Korea.
{
  tv.c <- d1.scaled
  max.val <- max(tv.c$value)
  tv.c$value <- round((tv.c$value/max.val)*100, 0)
  tv.b <- d2.scaled
  max.val <- max(tv.b$value)
  tv.b$value <- round((tv.b$value/max.val)*100, 0)
  
  g.c <- d3.scaled
  max.val <- max(g.c$hits)
  g.c$hits <- round((g.c$hits/max.val)*100, 0)
  g.b <- d4.scaled
  max.val <- max(g.b$hits)
  g.b$hits <- round((g.b$hits/max.val)*100, 0)
  
  w.c <- d5.scaled
  max.val <- max(w.c$views)
  w.c$views <- round((w.c$views/max.val)*100, 0)
  w.b <- d6.scaled
  max.views <- max(w.b$views)
  w.b$views <- round((w.b$views/max.val)*100, 0)
  
  a <- rbind(setNames(tv.c[,which(names(tv.c)%in%c("date", "value"))], c("date", "hits")),
             setNames(tv.b[,which(names(tv.b)%in%c("date", "value"))], c("date", "hits")),
             setNames(g.c[,which(names(g.c)%in%c("date", "hits"))], c("date", "hits")),
             setNames(g.b[,which(names(g.b)%in%c("date", "hits"))], c("date", "hits")),
             setNames(w.c[,which(names(w.c)%in%c("date", "views"))], c("date", "hits")),
             setNames(w.b[,which(names(w.b)%in%c("date", "views"))], c("date", "hits")))
  
  b <- c(rep("tv", nrow(tv.c)), rep("tv", nrow(tv.b)), rep("Google", nrow(g.c)), rep("Google", nrow(g.b)),
         rep("Wikipedia", nrow(w.c)), rep("Wikipedia", nrow(w.b)))
  
  c <- c(rep("Coronavirus", nrow(tv.c)), rep("Bats", nrow(tv.b)), rep("Coronavirus", nrow(g.c)), rep("Bats", nrow(g.b)),
         rep("Coronavirus", nrow(w.c)), rep("Bats", nrow(w.b)))
  
  d <- setNames(data.frame(a, b, c), c("date", "value", "source", "type"))
  d$type <- factor(d$type, levels=c("Coronavirus", "Bats"))
  d$source <- factor(d$source, levels=c("tv", "Google", "Wikipedia"), labels=c("TV", "Google", "Wikipedia"))
  d$date <- ymd(d$date)
  d$one <- "a"
  
  ggplot() + 
    geom_rect(data=d, aes(xmin=as.Date("2020-02-24"), xmax=as.Date("2020-05-31"), ymin=-Inf, ymax=Inf), 
              fill="lightcoral", alpha=0.01) +
    geom_line(data=d, aes(x=date, y=value), size=0.8, color="black", linetype="solid") + 
    facet_grid(source ~ type) + theme_bw() + 
    theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
          plot.title = element_text(size=22, face="bold")) +
    theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
    theme(strip.text = element_text(colour = "black", size=14, face="bold"))

  }

#Correlation matrix between time series at time t (Fig. 4)
{
  #Whole January-September 2020
  m1 <- cor(cbind(tv.c$value, tv.b$value, g.c$hits, g.b$hits, w.c$views, w.b$views))
  rownames(m1) <- c("tv.c", "tv.b", "g.c", "g.b", "w.c", "w.b")
  colnames(m1) <- c("tv.c", "tv.b", "g.c", "g.b", "w.c", "w.b")
  corrplot(m1)
  
  m2 <- cor(cbind(tv.c[which(tv.c$date>="2020-02-24"&tv.c$date<="2020-05-31"),"value"], 
                 tv.b[which(tv.b$date>="2020-02-24"&tv.b$date<="2020-05-31"),"value"], 
                 g.c[which(g.c$date>="2020-02-24"&g.c$date<="2020-05-31"),"hits"], 
                 g.b[which(g.c$date>="2020-02-24"&g.b$date<="2020-05-31"),"hits"],
                 w.c[which(w.c$date>="2020-02-24"&w.c$date<="2020-05-31"),"views"], 
                 w.b[which(w.c$date>="2020-02-24"&w.b$date<="2020-05-31"),"views"]))
  rownames(m2) <- c("tv.c", "tv.b", "g.c", "g.b", "w.c", "w.b")
  colnames(m2) <- c("tv.c", "tv.b", "g.c", "g.b", "w.c", "w.b")
  corrplot(m2)

}

################################################################################
#Time-series analysis
################################################################################

########################################################
#GoogleTrends
########################################################
{
  #Loading the dataset
  g <- read.csv(file="~/Documents/Research_Projects/Ongoing/BatsCovid/NewAnalysis_Dec2020/Data/Weekly/Google1620/GoogleTrends1620_Bats.csv")
  
  #Rescaling the response variable, to fit a Gamma (almost perfect)
  {
    g$hits.scaled <- g$hits+1
    par(mfrow=c(2,2))
    denscomp(fitdist(g$hits.scaled, "gamma"))
    qqcomp(fitdist(g$hits.scaled, "gamma"))
    cdfcomp(fitdist(g$hits.scaled, "gamma"))
    ppcomp(fitdist(g$hits.scaled, "gamma"))
    par(mfrow=c(1,1))
  }
  
  #Decomposing dates
  {
    g$date <- ymd(g$date)
    g$year <- year(g$date)
    g$month <- month(g$date)
    g$week <- week(g$date)
    g$yday <- yday(g$date)
  }
  
  #Assigning languages
  {
    en.idx <- c("AU", "CA", "GB", "IE", "NZ", "US", "ZA")
    g[which(g$geo%in%en.idx), "lang"] <- "en"
    es.idx <- c("AR", "CL", "CO", "ES","MX","UY")
    g[which(g$geo%in%es.idx), "lang"] <- "es"
    fr.idx <- c("FR")
    g[which(g$geo%in%fr.idx), "lang"] <- "fr"
    de.idx <- c("AT", "DE")
    g[which(g$geo%in%de.idx), "lang"] <- "de"
    pt.idx <- c("BR", "PT")
    g[which(g$geo%in%pt.idx), "lang"] <- "pt"
    it.idx <- c("IT")
    g[which(g$geo%in%it.idx), "lang"] <- "it"
    jp.idx <- c("JP")
    g[which(g$geo%in%jp.idx), "lang"] <- "jp"
    ko.idx <- c("KR")
    g[which(g$geo%in%ko.idx), "lang"] <- "kr"
    
    g[which(is.na(g$lang)),]
    
  }
  
  #Assigning the proportion of Internet users in each country
  {
    levels(g$geo)
    g[which(g$geo=="AR"), "internet.users"] <- 0.76
    g[which(g$geo=="AT"), "internet.users"] <- 0.88
    g[which(g$geo=="AU"), "internet.users"] <- 0.86
    g[which(g$geo=="BR"), "internet.users"] <- 0.72
    g[which(g$geo=="CA"), "internet.users"] <- 0.93
    g[which(g$geo=="CL"), "internet.users"] <- 0.82
    g[which(g$geo=="CO"), "internet.users"] <- 0.62
    g[which(g$geo=="DE"), "internet.users"] <- 0.86
    g[which(g$geo=="ES"), "internet.users"] <- 0.84
    g[which(g$geo=="FR"), "internet.users"] <- 0.80
    g[which(g$geo=="GB"), "internet.users"] <- 0.95
    g[which(g$geo=="IE"), "internet.users"] <- 0.84
    g[which(g$geo=="IT"), "internet.users"] <- 0.61
    g[which(g$geo=="JP"), "internet.users"] <- 0.91
    g[which(g$geo=="KR"), "internet.users"] <- 0.95
    g[which(g$geo=="MX"), "internet.users"] <- 0.64
    g[which(g$geo=="NZ"), "internet.users"] <- 0.91
    g[which(g$geo=="PT"), "internet.users"] <- 0.74
    g[which(g$geo=="US"), "internet.users"] <- 0.97
    g[which(g$geo=="UY"), "internet.users"] <- 0.68
    g[which(g$geo=="ZA"), "internet.users"] <- 0.56
    g[which(is.na(g$internet.users)), "geo"]
  }
  
  #Establishing contrasts for factor variables
  {
    contrasts(g$geo) <- contr.sum
    g$year <- factor(g$year)
    g$geo.year <- paste(g$geo, g$year, sep="-")
    g$geo.year <- factor(g$geo.year)
    g$timestep <- rep(c(1:258), 21)
  }
  
  #Data exploration
  {
    boxplot(hits.scaled ~ year, data=g)
    boxplot(hits.scaled ~ week, data=g)
    ggplot(data=g, aes(x=week, y=hits.scaled)) + geom_point() + facet_grid(~year)
    ggplot() + geom_boxplot(data=g, aes(x=geo, y=hits.scaled))
    ggplot() + geom_boxplot(data=g, aes(x=lang, y=hits.scaled))
  }

  #Model fitting with INLA (B-splines and cubic splines)
  {
    #Cubic splines
    knots <- seq(2, 51, by = 20)
    length(knots)
    mod.k3 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                      f(year, model= "iid") + f(geo, model="iid"),
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                    family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                    data=g)
    
    knots <- seq(2, 51, by = 16)
    mod.k4 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                      f(year, model= "iid") + f(geo, model="iid"),
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                    family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                    data=g)
    
    knots <- seq(2, 51, by = 12)
    length(knots)
    mod.k5 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                      f(year, model= "iid") + f(geo, model="iid"),
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                    family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                    data=g)
    
    knots <- seq(2, 51, by = 8)
    length(knots)
    mod.k7 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                      f(year, model= "iid") + f(geo, model="iid"),
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                    family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                    data=g)
    
    knots <- seq(2, 51, by = 6)
    length(knots)
    mod.k9 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                      f(year, model= "iid") + f(geo, model="iid"),
                    control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                    family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                    data=g)
    
    knots <- seq(2, 51, by = 5)
    length(knots)
    mod.k10 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                       f(year, model= "iid") + f(geo, model="iid"),
                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                     data=g)
    
    knots <- seq(2, 51, by = 4)
    length(knots)
    mod.k13 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                       f(year, model= "iid") + f(geo, model="iid"),
                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                     data=g)
    
    knots <- seq(2, 51, by = 3)
    length(knots)
    mod.k17 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                       f(year, model= "iid") + f(geo, model="iid"),
                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                     data=g)
    
    knots <- seq(2, 51, by = 2)
    length(knots)
    mod.k25 <-  inla(hits.scaled ~ 1 + bs(week, knots = knots) +
                       f(year, model= "iid") + f(geo, model="iid"),
                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                     data=g)
    
    c(sum(log(mod.k4$cpo$cpo)), sum(log(mod.k5$cpo$cpo)), sum(log(mod.k7$cpo$cpo)), sum(log(mod.k9$cpo$cpo)),
      sum(log(mod.k10$cpo$cpo)), sum(log(mod.k13$cpo$cpo)), sum(log(mod.k17$cpo$cpo)), sum(log(mod.k25$cpo$cpo)))
    c(mod.k4$waic$waic, mod.k5$waic$waic, mod.k7$waic$waic, mod.k9$waic$waic,
      mod.k10$waic$waic, mod.k13$waic$waic, mod.k17$waic$waic, mod.k25$waic$waic)
    c(mod.k4$dic$dic, mod.k5$dic$dic, mod.k7$dic$dic, mod.k9$dic$dic,
      mod.k10$dic$dic, mod.k13$dic$dic, mod.k17$dic$dic, mod.k25$dic$dic)
    
    #25 knots per year look nice, but maybe a model without year as a RI would be better
    knots <- seq(2, 257, by = 2)
    mod.k128.timestep <-  inla(hits.scaled ~ 1 + bs(timestep, knots = knots) + f(geo, model="iid"),
                               control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                               family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                               data=g)
    c(sum(log(mod.k25$cpo$cpo)), mod.k25$waic$waic, mod.k25$dic$dic)
    c(sum(log(mod.k128.timestep$cpo$cpo)), mod.k128.timestep$waic$waic, mod.k128.timestep$dic$dic)
    
    #A cubic spline looks better
    mod.k128.timestep.cs <-  inla(hits.scaled ~ 1 + ns(timestep, knots = 128) + f(geo, model="iid"),
                                  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                  family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                  data=g)
    
    c(sum(log(mod.k25$cpo$cpo)), mod.k25$waic$waic, mod.k25$dic$dic)
    c(sum(log(mod.k128.timestep$cpo$cpo)), mod.k128.timestep$waic$waic, mod.k128.timestep$dic$dic)
    c(sum(log(mod.k128.timestep.cs$cpo$cpo)), mod.k128.timestep.cs$waic$waic, mod.k128.timestep.cs$dic$dic)
    
    #Model fitting
    knots <- seq(2, 257, by = 2)
    mod <-  inla(hits.scaled ~ 1 + bs(timestep, knots = knots) + f(geo, model="iid"),
                 control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                 family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                 data=g)
    pears.res <- (g$hits.scaled - mod$summary.fitted.values[,1])/sd(g$hits.scaled)
    fitted <- mod$summary.fitted.values[,1]
    ggplot() + geom_point(aes(x=fitted, y=pears.res)) + geom_smooth(aes(x=fitted, y=pears.res)) #No strong pattern in the residuals vs fitted
    ggplot() + geom_point(aes(x=g$timestep, y=pears.res)) + geom_smooth(aes(x=g$timestep, y=pears.res)) #Weekly fluctuations have been addressed
    
    plot(acf(pears.res))
    plot(acf(g$hits.scaled - mod$summary.fitted.values[,1]))
    
    #Marginal effects
    dat <- g[,c("timestep", "geo", "hits.scaled")]
    new.dat <- dat
    new.dat$hits.scaled <- NA
    new.dat$timestep <- dat$timestep 
    new.dat$geo <- NA
    dat.pred <- rbind(dat, new.dat)
    
    knots <- seq(2, 257, by = 2)
    mod <-  inla(hits.scaled ~ 1 + bs(timestep, knots = knots) +
                   f(geo, model="iid"),
                 control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                 family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                 data=dat.pred)
    
    foo <- mod$summary.fitted.values[5419:10836, c(1, 3, 5)]
    names(foo) <- c("fitted", "ci.low", "ci.high")
    foo$timestep <- new.dat$timestep
    
    c(1, 53, 106 ,158, 210)
    c("2016-01-03", "2017-01-01", "2018-01-07", "2019-01-06", "2020-01-05")
    ggplot() + 
      geom_point(data=dat, aes(x=timestep, y=hits.scaled), size=1, color="steelblue", alpha=0.4) + 
      geom_ribbon(data=foo, aes(x=timestep, ymin=ci.low, ymax=ci.high), size=0.8, fill="firebrick", linetype="solid", alpha=0.7) + 
      geom_line(data=foo, aes(x=timestep, y=fitted), size=1, color="black", linetype="solid") + 
      geom_line(data=foo, aes(x=timestep, y=ci.low), size=0.5, color="black", linetype="solid") + 
      geom_line(data=foo, aes(x=timestep,  y=ci.high), size=0.5, color="black", linetype="solid") + 
      theme_bw() +
      scale_x_continuous(breaks=c(1, 53, 106 ,158, 210, 260), labels=c("2016", "2017", "2018", "2019", "2020", "2021")) +
      theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="GoogleTrends") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold"))
  }
  
}

########################################################
#Wikipedia
########################################################
{
  #Loading the dataset
  w <- read.csv(file="~/Documents/Research_Projects/Ongoing/BatsCovid/NewAnalysis_Dec2020/Data/Weekly/Wikipedia1620/WikipediaWeekly1620.csv")
  w <- w[,c(2:18)]
  
  #Adjusting dates
  {
    w$date <- ymd(w$date)
    w$year <- year(w$date)
    w$week <- week(w$date)
  }
  
  #Calculating proportions
  {
    w$bats.en.prop <- w$bats.en/w$tot.en
    w$bats.es.prop <- w$bats.es/w$tot.es
    w$bats.fr.prop <- w$bats.fr/w$tot.fr
    w$bats.de.prop <- w$bats.de/w$tot.de
    w$bats.pt.prop <- w$bats.pt/w$tot.pt
    w$bats.it.prop <- w$bats.it/w$tot.it
    w$bats.ja.prop <- w$bats.ja/w$tot.ja
    w$bats.ko.prop <- w$bats.ko/w$tot.ko
    idx <- c("tot.fr", "tot.de", "tot.en", "tot.it", "tot.es", "tot.pt", "tot.ja", "tot.ko")
    w$tot <- apply(w[,which(names(w)%in%idx)], 1, sum)
    idx <- c("bats.en", "bats.es", "bats.pt", "bats.de", "bats.fr", "bats.it", "bats.ja", "bats.ko")
    w$bat.tot <- apply(w[,which(names(w)%in%idx)], 1, sum)
    w$bat.prop <- w$bat.tot/w$tot
  }
  
  #Plotting the proportion of aggregated searches
  options(scipen=999)
  ggplot() + 
    #geom_line(data=w, aes(x=date, y=bat.prop*1000000), size=0.8, color="steelblue", linetype="solid") + 
    geom_point(data=w, aes(x=date, y=bat.prop*1000000), size=3, color="steelblue", alpha=0.3) + 
    theme_bw() + 
    theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
          axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
          axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
          plot.title = element_text(size=22, face="bold")) +
    theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="") +
    theme(strip.text = element_text(colour = "black", size=14, face="bold")) +
    theme(text=element_text(size=16,  family="serif"))
  table(w$year)
  table(w$week)
  
  #Creating a wide-format dataset
  {
    names(w)
    d <- w[,which(names(w)%in%c("bat.prop", "date", "year", "week"))]
    d$bat.prop.mln <- d$bat.prop*1000000
    d$timestep <- 1:nrow(d)
  }
  
  #Model fitting with INLA (random-walk models)
  {
    
    hist(d$bat.prop.mln)
    par(mfrow=c(2,2))
    denscomp(fitdist(d$bat.prop.mln, "gamma"))
    qqcomp(fitdist(d$bat.prop.mln, "gamma"))
    cdfcomp(fitdist(d$bat.prop.mln, "gamma"))
    ppcomp(fitdist(d$bat.prop.mln, "gamma"))
    par(mfrow=c(1,1))
    
    mod.gam.week.yearfix.rw1 <- inla(bat.prop.mln ~ f(week, year, model="rw1"),
                                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                     data=d)
    
    mod.gam.week.yearfix.rw2 <- inla(bat.prop.mln ~ f(week, year, model="rw2"),
                                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                     data=d)
    
    mod.gamm.week.yearRI.rw1 <- inla(bat.prop.mln ~ f(week, model="rw1") + f(year, model="iid"),
                                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                     data=d)
    
    mod.gamm.week.yearRI.rw2 <- inla(bat.prop.mln ~ f(week, model="rw2") + f(year, model="iid"),
                                     control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                     family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                     data=d)
    
    mod.gam.timestep.rw1 <- inla(bat.prop.mln ~ f(timestep, model="rw1"),
                                 control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                 family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                 data=d)
    
    mod.gam.timestep.rw2 <- inla(bat.prop.mln ~ f(timestep, model="rw2"),
                                 control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                                 family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                                 data=d)
    
    c(sum(log(mod.gam.week.yearfix.rw1$cpo$cpo)), sum(log(mod.gam.week.yearfix.rw2$cpo$cpo)), 
      sum(log(mod.gamm.week.yearRI.rw1$cpo$cpo)), sum(log(mod.gamm.week.yearRI.rw2$cpo$cpo)), 
      sum(log(mod.gam.timestep.rw1$cpo$cpo)),  sum(log(mod.gam.timestep.rw2$cpo$cpo)))
    
    c(mod.gam.week.yearfix.rw1$waic$waic, mod.gam.week.yearfix.rw2$waic$waic, 
      mod.gamm.week.yearRI.rw1$waic$waic, mod.gamm.week.yearRI.rw2$waic$waic, 
      mod.gam.timestep.rw1$waic$waic,  mod.gam.timestep.rw2$waic$waic)
    
    c(mod.gam.week.yearfix.rw1$dic$dic, mod.gam.week.yearfix.rw2$dic$dic, 
      mod.gamm.week.yearRI.rw1$dic$dic, mod.gamm.week.yearRI.rw2$dic$dic, 
      mod.gam.timestep.rw1$dic$dic,  mod.gam.timestep.rw2$dic$dic)
    
    #Using a penalised complexity (PC) prior, playing with U
    idx <- c(seq(from=0.01, to=3, by=0.02))
    a <- c()
    for(i in seq_along(idx)){
      U <- idx[i]
      hyper.prec <- list(theta=list(prior="pc.prec", param=c(U, 0.05)))
      mod <- inla(bat.prop.mln ~ f(timestep, model="rw1", hyper = hyper.prec),
                  control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                  family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                  data=d)
      dic.val <- c(mod$dic$dic)
      waic.val <- c(mod$waic$waic)
      cpo.val <- sum(log(mod$cpo$cpo))
      a[[i]] <- c(dic.val, waic.val, cpo.val)
    }
    
    fit.dat <- ldply(a)
    names(fit.dat) <- c("dic", "waic", "sum(log(cpo))")
    fit.dat$U <- idx
    fit.dat
    fit.dat[which(fit.dat$dic==min(fit.dat$dic)), "U"]
    fit.dat[which(fit.dat$waic==min(fit.dat$waic)), "U"]
    fit.dat[which(fit.dat$`sum(log(cpo))`==max(fit.dat$`sum(log(cpo))`)), "U"]
    
    #Model fitting
    U <- 0.03
    hyper.prec <- list(theta=list(prior="pc.prec", param=c(U, 0.05)))
    mod <- inla(bat.prop.mln ~ f(timestep, model="rw1", hyper = hyper.prec),
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                data=d)
    
    pears.res <- (d$bat.prop.mln - mod$summary.fitted.values[,1])/sd(d$bat.prop.mln)
    fitted <- mod$summary.fitted.values[,1]
    ggplot() + geom_point(aes(x=fitted, y=pears.res)) + geom_smooth(aes(x=fitted, y=pears.res)) #No strong pattern in the residuals vs fitted
    ggplot() + geom_point(aes(x=d$timestep, y=pears.res)) + geom_smooth(aes(x=d$timestep, y=pears.res)) #Weekly fluctuations have been addressed
    plot(acf(pears.res)) #No autocorrelation in Pearson's residuals
    
    #Marginal effects
    dat <- d[,c("timestep", "bat.prop.mln")]
    new.dat <- dat
    new.dat$bat.prop.mln <- NA
    dat.pred <- rbind(dat, new.dat)
    U <- 0.03
    hyper.prec <- list(theta=list(prior="pc.prec", param=c(U, 0.05)))
    mod <- inla(bat.prop.mln ~ f(timestep, model="rw1", hyper = hyper.prec),
                control.compute=list(dic=TRUE, waic=TRUE, cpo=TRUE), 
                family="Gamma", control.family=list(link='log'), control.predictor=list(link=1, compute=TRUE), 
                data=dat.pred)
    
    foo <- mod$summary.fitted.values[258:514, c(1, 3, 5)]
    names(foo) <- c("fitted", "ci.low", "ci.high")
    foo$timestep <- new.dat$timestep
    
    ggplot() + 
      #geom_point(data=d, aes(x=timestep, y=bat.prop.mln), size=2, color="black", alpha=0.9) + 
      geom_ribbon(data=foo, aes(x=timestep, ymin=ci.low, ymax=ci.high), size=0.8, fill="steelblue", linetype="solid", alpha=0.2) + 
      geom_line(data=foo, aes(x=timestep, y=fitted), size=0.8, color="firebrick", linetype="solid") + 
      geom_line(data=foo, aes(x=timestep, y=ci.low), size=0.5, color="black", linetype="solid") + 
      geom_line(data=foo, aes(x=timestep,  y=ci.high), size=0.5, color="black", linetype="solid") + 
      theme_bw() +
      theme(axis.text.x = element_text(colour="black",size=14,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.text.y = element_text(colour="black",size=14,angle=0,hjust=1,vjust=0,face="plain"),  
            axis.title.x = element_text(colour="black",size=12,angle=0,hjust=.5,vjust=.5,face="plain"),
            axis.title.y = element_text(colour="black",size=12,angle=90,hjust=.5,vjust=.5,face="plain"),
            plot.title = element_text(size=22, face="bold")) +
      theme(strip.background =element_rect(fill="wheat")) + labs(x="", y="GoogleTrends") +
      theme(strip.text = element_text(colour = "black", size=14, face="bold"))
  }
}

