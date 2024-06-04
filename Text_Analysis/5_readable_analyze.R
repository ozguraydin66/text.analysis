   
   rm(list = ls())
   options(max.print=100000)  
   closeAllConnections()

   library(plyr)
   library(rstatix)
   library(rstudioapi)
   library(dplyr)

#' Set directory & files
#' ---------------------
   current_path = getActiveDocumentContext()$path 
   rootdir = setwd(dirname(current_path))
   result_file.1 = file.path(rootdir, "results/result_read_descriptive.txt")
   result_file.2 = file.path(rootdir, "results/result_read_analyzing.txt")
   
   FigFile = function(FigN) {
     fig_file <- paste0(rootdir, "/plots/fig_", FigN, ".jpg")
     return(fig_file)}
   

#' Load the data file
#' ------------------  
   set.seed(1)
   df = read.table("data/tidy_format/wordlist_wordform.txt", header = T, sep = "\t")
   df$TextType = as.factor(df$TextType)
   head(df)

#' Load the sentlist data file
#' ---------------------------  
   Sentdat = read.table("data/tidy_format/sentlist_wordform.txt",header = T, sep = "\t")
   Sentdat$TextType = as.factor(Sentdat$TextType)
   head(Sentdat)
   
#' Readable analysis
#' ------------------
   SentLength <- ddply(Sentdat, .(TextType),
                       summarise, 
                       SentLength.m=mean(WordCountinSent),
                       SentLength.t=sum(WordCountinSent),
                       SentCount=mean(SentCount)
                       )
   WordLength <- ddply(df, .(TextType,LineNum),
                       summarise, 
                       syllable.s = sum(syllble),
                       syl.word_2 =sum(syllble==2),
                       syl.word_3 =sum(syllble==3),
                       syl.word_4 =sum(syllble==4),
                       syl.word_5 =sum(syllble==5),
                       syl.word_6 =sum(syllble>=6),
                       Multi.syllable=sum(syllble>=2)
                       )
   WordLength <- ddply(WordLength, .(TextType), summarise, 
                       syllable.t    = sum(syllable.s),
                       syllable.m    = mean(syllable.s),
                       syllable.SD   = sd(syllable.s, na.rm=TRUE), 
                       syl.word_2.m  = mean(syl.word_2),
                       syl.word_2.SD = sd(syl.word_2, na.rm=TRUE),
                       syl.word_3.m  = mean(syl.word_3),
                       syl.word_3.SD = sd(syl.word_3, na.rm=TRUE),
                       syl.word_4.m  = mean(syl.word_4),
                       syl.word_4.SD = sd(syl.word_4, na.rm=TRUE),
                       syl.word_5.m  = mean(syl.word_5),
                       syl.word_5.SD = sd(syl.word_5, na.rm=TRUE),
                       syl.word_6.m  = mean(syl.word_6),
                       syl.word_6.SD = sd(syl.word_6, na.rm=TRUE),
                       syl.Mult.t    = sum(Multi.syllable),
                       syl.Mult.m    = mean(Multi.syllable),
                       syl.Mult.SD   = sd(Multi.syllable, na.rm=TRUE)
                       )
   Descriptive <- left_join(WordLength, SentLength)
   #Descriptive <- merge(WordLength, SentLength)

   Calculate <- Descriptive %>%
     group_by(TextType) %>% 
     mutate(
            Atesman        = 198.825-(40.175*(syllable.t/SentLength.t))-(2.610*(SentLength.t/SentCount)),
            Cetinkaya      = 118.823-(25.987*(syllable.t/SentLength.t))-(0.971*(SentLength.t/SentCount)),
            Bezirci.Yilmaz = sqrt((SentLength.t/SentCount)*((syl.word_3.m*0.84)+
                                                            (syl.word_4.m *1.50)+
                                                            (syl.word_5.m *3.50)+
                                                            (syl.word_6.m *26.25))),
            FRES           = 206.835-1.015*(SentLength.t/SentCount)-84.6*(syllable.t/SentLength.t),
            FKGL           = (0.39*SentLength.m)+(11.8*syllable.m) - 15.59,
            PSK.Edu        = (0.0778*SentLength.m)+(0.455*syllable.t)-2.2029,
            PSK.Age        = (0.778*SentLength.m)+(0.455*syllable.t)+2.7971,
            SMOG           = 3+sqrt(syl.Mult.t),
            SMOG2          = 1.043*sqrt(30*(syl.Mult.t/SentCount))) %>%
   select(c(21:23)
   )
   #' FRES=Flesch Reading Ease (Flesch, 1948)
   #' FKGL= Flesch-Kincaid Grade Level  (Flesch, 1948)
   #' PSK.Edu=Powers-Sumner-Kearl-Education (Powers, Sumner & Kearl, 1958)
   #' PSK.Age=Powers-Sumner-Kearl-Age (Powers, Sumner & Kearl, 1958)
   #' SMOG (McLaughlin, 1969)
   
   write.table(Descriptive, result_file.1, quote = F, row.names = FALSE, sep = "\t")
   write.table(Calculate, result_file.2, quote = F, , row.names = FALSE, sep = "\t")
   
   
   
   
   
#' Clustering
#' ----------   
   set.seed(123)
   Cluster.desc <- Descriptive %>%
     select(TextType, syllable.m,
            #syl.word_2.m, syl.word_3.m,  syl.word_4.m, syl.word_5.m, syl.word_6.m, syl.Mult.m,
            SentLength.t, SentCount)
   Cluster.desc$TextType <- revalue(Cluster.desc$TextType, 
                                    c("inf_T1" = "M1b", "inf_T2" = "M2b","inf_T3" = "M3b", 
                                      "nar_T1" = "M1a", "nar_T2" = "M2a","nar_T3" = "M3a"))
   # View modified levels
   z <- Cluster.desc[,-c(1,1)]
   means <- apply(z,2,mean)
   sds <- apply(z,2,sd)
   nor <- scale(z,center=means,scale=sds)
   distance = dist(nor)
   Data.hclust = hclust(distance)
   jpeg(file=FigFile("cluster"),  units="cm", width=12, height=12, res=300)
      plot(Data.hclust,labels=Cluster.desc$TextType, main="")
   dev.off()
   
   
   library(factoextra)
   library(cluster)
   km <- kmeans(Cluster.desc[,-c(1,1)], centers = 3, nstart = 25)
   fviz_cluster(km, data = Cluster.desc[,-c(1,1)]) 
   