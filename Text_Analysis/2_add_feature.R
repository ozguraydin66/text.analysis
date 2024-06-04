#' https://www.tidytextmining.com/tfidf.html
#' https://github.com/trinker/textshape
#' https://ladal.edu.au/tagging.html

   rm(list = ls())
   closeAllConnections()
   #Sys.setlocale("LC_ALL","Turkish")
   #Sys.getlocale()
   
#' Select the list type: 
#' ---------------------  
   ListType =   c( "wordform",     # 1
                   "lexeme"        # 2
   )[1] # CHANGE THIS NUMBER AND RUN THE CODE

   library(tidyverse)

#' Load the wordlist data & merge: 
#' ----------------------  
   set.seed(1)
   df = read.table(paste0("data/tidy_format/wordlist_", ListType, ".txt"), 
                   header = T, sep = "\t", encoding="UTF-8")
   head(df)
   
#' Load the frequency data and merge it into wordlist data: 
#' --------------------------------------------------------  
   if(ListType=="lexeme"){
     df.freq <- read.table("data/supp_data/data_freq_n.txt", sep="\t", header=TRUE, encoding = "UTF-8")
     head(df.freq)
     Freqdat <- merge(df, df.freq, by="word", all.x =TRUE)
   }
   if(ListType=="wordform"){
     filenames <- list.files("data/supp_data" , pattern="*info.txt", full.names=TRUE)
     df.freq    <- filenames %>% map_dfr(read.delim2)
     #df.freq[,2] <- NULL
     Freqdat    <- merge(df, df.freq, by="word", all.x =TRUE)
   }
   Freqdat    = tidyr::separate(Freqdat, TextType, c('TextTypeN','Text'), sep='_', remove=F)
   sum(duplicated(Freqdat))
   Freqdat =Freqdat[!duplicated(Freqdat),]

#' Write the merge data frame in to a file: 
#' ----------------------------------------  
   write.table(Freqdat, paste0("data/tidy_format/wordlist_", ListType, ".txt"), 
               sep="\t", quote=F, row.names=F)

   