
## https://ladal.edu.au/lexsim.html

   rm(list = ls())
   closeAllConnections()
  
   library(rstudioapi)
   library(stringdist)
   library(hashr)
   library(tm)
   library(textTinyR)

#' Select the list type: 
#' ---------------------  
   ListType =  c("lexeme",    # 1
                 "wordform"   # 2
   )[2] # CHANGE THIS NUMBER AND RUN THE CODE
   
#' Set directory & files
#' ---------------------
   current_path <- getActiveDocumentContext()$path 
   rootdir <- setwd(dirname(current_path))
   report <- paste0("results/similarity_",ListType,".txt")

#' Read the text
#' -------------
   text.path <- list.files(path=file.path(rootdir, paste0("texts/",ListType)),
                           pattern = ".txt", full.names = TRUE)
   text.file <-list() 
   for(i in 1:length(text.path)){
     text.file[[i]] <- removePunctuation(readLines(text.path[i]))}
   
#' Jaccard similarity
#' ------------------ 
#' Using the JACCARD_DICE() function to calculate 
#' the Jaccard similarity word-wise 
   JScore.M1b_M2b.inf = JACCARD_DICE(strsplit(text.file[[1]][2], "\\s+"), 
                                  strsplit(text.file[[2]][2], "\\s+"), 
                                  method = 'jaccard', threads = 1)
   JScore.M2b_M3b.inf = JACCARD_DICE(strsplit(text.file[[2]][2], "\\s+"), 
                                  strsplit(text.file[[3]][2], "\\s+"), 
                                  method = 'jaccard', threads = 1)
   JScore.M1b_M3b.inf = JACCARD_DICE(strsplit(text.file[[1]][2], "\\s+"), 
                                  strsplit(text.file[[3]][2], "\\s+"), 
                                  method = 'jaccard', threads = 1)
   JScore.M1a_M2a.nar = JACCARD_DICE(strsplit(text.file[[4]][2], "\\s+"), 
                                  strsplit(text.file[[5]][2], "\\s+"), 
                                  method = 'jaccard', threads = 1)
   JScore.M2a_M3a.nar = JACCARD_DICE(strsplit(text.file[[5]][2], "\\s+"), 
                                  strsplit(text.file[[6]][2], "\\s+"), 
                                  method = 'jaccard', threads = 1)
   JScore.M1a_M3a.nar = JACCARD_DICE(strsplit(text.file[[4]][2], "\\s+"), 
                                  strsplit(text.file[[6]][2], "\\s+"), 
                                  method = 'jaccard', threads = 1)

#' Cousine similarity
#' ------------------ 
#' Using the cosine_distance() function to calculate 
#' the Jaccard similarity word-wise 
   CScore.M1b_M2b.inf  = cosine_distance(text.file[[1]][2],
                                        text.file[[2]][2], split_separator= " ")
   CScore.M2b_M3b.inf  = cosine_distance(text.file[[2]][2],
                                        text.file[[3]][2], split_separator= " ")
   CScore.M1b_M3b.inf  = cosine_distance(text.file[[1]][2],
                                        text.file[[3]][2], split_separator= " ")
   CScore.M1a_M2a.nar = cosine_distance(text.file[[4]][2],
                                       text.file[[5]][2], split_separator= " ")
   CScore.M2a_M3a.nar = cosine_distance(text.file[[5]][2],
                                       text.file[[6]][2], split_separator= " ")
   CScore.M1a_M3a.nar = cosine_distance(text.file[[4]][2],
                                       text.file[[6]][2], split_separator= " ")

   sink(report)
   cat("\n", "Jaccard similarity", "\n")
   cat("\n", "------------------", "\n")
   cat("\n", "Text 1b vs. Text 2b in informative texts:", JScore.M1b_M2b.inf, "\n")
   cat("\n", "Text 2b vs. Text 3b in informative texts:", JScore.M2b_M3b.inf, "\n")
   cat("\n", "Text 1b vs. Text 3b in informative texts:", JScore.M1b_M3b.inf, "\n\n\n")
   
   cat("\n", "Text 1a vs. Text 2a in narrative texts:", JScore.M1a_M2a.nar, "\n")
   cat("\n", "Text 2a vs. Text 3a in narrative texts:", JScore.M2a_M3a.nar, "\n")
   cat("\n", "Text 1a vs. Text 3a in narrative texts:", JScore.M1a_M3a.nar, "\n\n\n")

   cat("\n", "Cousine similarity", "\n")
   cat("\n", "------------------", "\n")
   cat("\n", "Text 1b vs. Text 2b in informative texts:", CScore.M1b_M2b.inf, "\n")
   cat("\n", "Text 2b vs. Text 3b in informative texts:", CScore.M2b_M3b.inf, "\n")
   cat("\n", "Text 1b vs. Text 3b in informative texts:", CScore.M1b_M3b.inf, "\n\n\n")
   
   cat("\n", "Text 1 vs. Text 2 in narrative texts:", CScore.M1a_M2a.nar, "\n")
   cat("\n", "Text 2 vs. Text 3 in narrative texts:", CScore.M2a_M3a.nar, "\n")
   cat("\n", "Text 1 vs. Text 3 in narrative texts:", CScore.M1a_M3a.nar, "\n\n\n")
   sink()
   

