   
   rm(list = ls())
   closeAllConnections()
  
   library(rstudioapi)
   #install.packages("packages/vwr_0.3.0.tar", repos = NULL, type="source")
   library(vwr)
   #install.packages("remotes")
   #remotes::install_github("waltervanheuven/strngrams", force=T)
   library(strngrams)

#' Select the list type: 
#' ---------------------  
   ListType =  c("lexeme",    # 1
                 "wordform"   # 2
   )[1]

#' Set directory & files
#' ---------------------
   current_path = getActiveDocumentContext()$path 
   rootdir = setwd(dirname(current_path))

#' Load the data files
#' ------------------  
   set.seed(1)
   df = read.table(paste0("data/tidy_format/wordlist_", ListType, ".txt"), 
                        header = T, sep = "\t", encoding="UTF-8")
   #df <- df %>% dplyr::mutate_at(c('word', 'n', 'TextType', 'Part.of.Speech'), as.factor)
   lexicon = read.table("data/supp_data/Turkish_lexicon.txt", header = FALSE, encoding = "UTF-8")

#' Calculate the OLD20
#' -------------------      
   df$OLD20 <- old20(df$word, lexicon[,1])
   tail(df)
   
#' Calculate the Summed Bigram values
#' -----------------------------------
   z =get_ngram_frequencies(lexicon$V1, lexicon$V3, type = "bigram", position_specific = TRUE)
   newcol = ncol(df) +1
   
   df[,newcol] <- ngram_frequency( df$word, z, type = "bigram",
                                   position_specific = TRUE,
                                   frequency = "token",
                                   func = "mean", #"summed",
                                   progressbar = TRUE
                                   )
   colnames(df)[ncol(df)] <- "Bigram.Mean"

#' Write the data
#' --------------
   write.table(df, paste0("data/tidy_format/wordlist_", ListType, ".txt"), 
               sep="\t", quote=F, row.names=F)
   
   