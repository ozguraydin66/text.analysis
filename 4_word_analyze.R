   
   rm(list = ls())
   closeAllConnections()
  
   library(rstudioapi)
   library(plyr)
   library(rstatix)
   library(ggpubr)

#' Select the list type: 
#' ---------------------  
   ListType =  c("lexeme",    # 1
                 "wordform"   # 2
   )[2]

# 'Select the measure type:
#' -----------------------  
   measure.type = c(  "Freq.Norm",   # 1	
                      "OLD20",       # 2
                      "Bigram.Mean", # 3
                      "WordLength",  # 4
                      "syllble",     # 5
                      "morphology"   # 6
                      ) 
   if(ListType ==  "lexeme")  {select.measure = c(1,2,3)}
   if(ListType ==  "wordform"){select.measure = c(2,3,4,5,6)}

#' Set directory & files
#' ---------------------
   current_path = getActiveDocumentContext()$path 
   rootdir = setwd(dirname(current_path))
   result_file.1 = file.path(rootdir, paste0("results/result_descriptive_",ListType, ".txt"))
   result_file.2 = file.path(rootdir, paste0("results/result_ttest_",ListType, ".txt"))
   result_file.3 = file.path(rootdir, paste0("results/result_wilcox_",ListType, ".txt"))
   result_file.4 = file.path(rootdir, paste0("results/result_shapiro_",ListType, ".txt"))
   FigFile = function(FigN) {
     fig_file <- paste0(rootdir, "/plots/fig_", FigN, "_", ListType, ".png")
     return(fig_file)}

#' Load the data file
#' ------------------  
   set.seed(1)
   df = read.table(paste0("data/tidy_format/wordlist_", ListType, ".txt"), 
                   header = T, sep = "\t")
   head(df)
   if(ListType ==  "lexeme") {
     df <- df %>% dplyr::mutate_at(c('word', 'TextType', 'Part.of.Speech'), as.factor)}
   if(ListType ==  "wordform") {
     df <- df %>% dplyr::mutate_at(c('word', 'TextType', 'Text'), as.factor)}
   head(df)
   
   
   dataN <-list() 
   table <- list()
   stat.test.ttest <- list()
   stat.test.wilcox <- list()
   shapiro.test <- list()

   for(z in select.measure) {
     dataN[[z]] = df
     colnames(dataN[[z]])[which(names(dataN[[z]]) == measure.type[[z]])] <- "measuretype"
    
     #'Descriptive results
     table[[z]] = ddply(dataN[[z]], .(TextType),
                        summarise, N=length(measuretype), 
                        M=mean(measuretype, na.rm=TRUE), 
                        SD=sd(measuretype, na.rm=TRUE), 
                        SE=SD/sqrt(N),
                        CI=SE*qt(.975, N-1),
                        CI_min= M-1.96*SD/sqrt(N),
                        CI_max= M+1.96*SD/sqrt(N))
    
     #plot.data = file.path(rootdir, paste0("results/", measure.type[[z]], "_", data.type, ".txt"))
     table[[z]]$olcumturu = measure.type[[z]]
     #write.table(table[[z]], plot.data, quote = F, sep = "\t")
     
     #'Check the normalization
     shapiro.test[[z]] <- dataN[[z]] %>% 
       shapiro_test(measuretype)
     #shapiro.test.group[[z]]=as.data.frame(shapiro.test.group)
     shapiro.test[[z]]$measure.type=measure.type[[z]]
     
     stat.test.ttest[[z]] <- dataN[[z]] %>%
       pairwise_t_test(measuretype ~ TextType, p.adjust.method = "bonferroni") %>%
       mutate(test.type="t-test", measure=measure.type[[z]])   
     stat.test.ttest[[z]]=as.data.frame(stat.test.ttest[[z]])
    
     stat.test.wilcox[[z]] <- dataN[[z]] %>%
       pairwise_wilcox_test(measuretype ~ TextType, p.adjust.method = "bonferroni") %>%
       mutate(test.type="wilcox-test", 
              z.score = qnorm(p/2),
              measure=measure.type[[z]])
     stat.test.wilcox[[z]]=as.data.frame(stat.test.wilcox[[z]])
  }

   (desc.table = dplyr::bind_rows(table))
   (shapiro.table = dplyr::bind_rows(shapiro.test))
   (ttest.table  = dplyr::bind_rows(stat.test.ttest))
   (wilcox.table = dplyr::bind_rows(stat.test.wilcox))

   write.table(desc.table, result_file.1, quote = F, sep = "\t")
   write.table(ttest.table, result_file.2, quote = F, sep = "\t")
   write.table(wilcox.table, result_file.3, quote = F, sep = "\t")
   write.table(shapiro.table, result_file.4, quote = F, sep = "\t")

#' Pilot the results
#' ------------------  
   df$TextTypeN=revalue(df$TextTypeN, c("inf" = "Bilgilendirici", 
                                        "nar" = "Öyküleyici"))
   
   library(ggpubr)
   plot1 <- ggviolin(df,  x = "Text", y = "Freq.Norm", 
                     #color = "Text",  
                     add = c("boxplot"), 
                     add.params=list(color= 'grey',size=0.8),
                     color = "Text", palette = "npg")+
     theme(axis.title.x=element_blank(), legend.title=element_blank())
   plot1 <- facet(plot1, facet.by = "TextTypeN")
     
     
   plot2 <- ggviolin(df,  x = "TextTypeN", y = "OLD20",  
                     add = c("boxplot"), 
                     add.params=list(color= 'grey',size=0.8),
                     color = "TextTypeN", palette = "npg")+
     theme(axis.title.x=element_blank(), legend.title=element_blank())
   plot3 <- ggviolin(df,  x = "TextTypeN", y = "Bigram.Mean",  
                     add = c("boxplot"), 
                     add.params=list(color= 'grey',size=0.8),
                     color = "TextTypeN", palette = "npg")+
     theme(axis.title.x=element_blank(), legend.title=element_blank())
   plot4 <- ggviolin(df,  x = "TextTypeN", y = "WordLength",  
                     add = c("boxplot"), 
                     add.params=list(color= 'grey',size=0.8),
                     color = "TextTypeN", palette = "npg")+
     theme(axis.title.x=element_blank(), legend.title=element_blank())
   plot5 <- ggviolin(df,  x = "TextTypeN", y = "syllble",  
                     add = c("boxplot"), 
                     add.params=list(color= 'grey',size=0.8),
                     color = "TextTypeN", palette = "npg")+
     theme(axis.title.x=element_blank(), legend.title=element_blank())
   plot6 <- ggviolin(df,  x = "TextTypeN", y = "morphology",  
                     add = c("boxplot"), 
                     add.params=list(color= 'grey',size=0.8),
                     color = "TextTypeN", palette = "npg") +
     theme(axis.title.x=element_blank(), legend.title=element_blank())

   if(ListType ==  "lexeme"){
     figure <- ggarrange(plot1, plot2, plot3,
                         ncol = 3, nrow = 1, labels = "", align="v", 
                         common.legend = TRUE, legend="bottom")
     hi=7
   }
   if(ListType ==  "wordform"){
     figure <- ggarrange(plot2, plot3, plot4, plot5, plot6,
                         ncol = 3, nrow = 2, labels = "", align="v", 
                         common.legend = TRUE, legend="bottom")   
     hi=14
   }
   png(file=FigFile("desc_plots"),  units="cm", width=23, height=hi, res=300)
   figure
   dev.off()
