#' https://www.tidytextmining.com/tfidf.html
#' https://github.com/trinker/textshape
#' https://ladal.edu.au/tagging.html

   rm(list = ls())
   closeAllConnections()

   library(textshape)
   library(rstudioapi)
   library(dplyr)
   library(stringi)

#' Select the list type: 
#' ---------------------  
   ListType =   c( "wordform",     # 1
                   "lexeme"        # 2
   )[2] # CHANGE THIS NUMBER AND RUN THE CODE

#' Path organisation
#' -----------------
   current_path <- getActiveDocumentContext()$path 
   rootdir <- setwd(dirname(current_path))

   # Read the texts
   # --------------   
   text.path <- list.files(path=file.path(rootdir, paste0("texts/",ListType)),
                             pattern = ".txt", full.names = TRUE)
   text.file <-list() 
   TextSentence <-list() 
   SentCount <-list()
   WordList <-list()
   WordCount <-list() 
   for(i in 1:length(text.path)){
    
     text.file[[i]] = readLines(text.path[[i]])

     # Split the text to sentences
     # ---------------------------  
     TextSentence[[i]] = split_sentence(text.file[[i]][2])
     TextSentence[[i]] = as.data.frame(TextSentence[[i]])
     TextSentence[[i]]= TextSentence[[i]] %>% purrr::pluck(1)
     (SentCount[[i]] = length(TextSentence[[i]]))
     
     TextSentence[[i]] <- dplyr::tibble(line= 1:SentCount[[i]], 
                                        text = TextSentence[[i]], 
                                        TextType= gsub(".*[/]([^.]+)[_].*", "\\1", text.path[i]))
     TextSentence[[i]]$WordCountinSent = stri_count_words(TextSentence[[i]]$text)
     TextSentence[[i]]$SentCount = SentCount[[i]]
     
     LineNum=TextSentence[[i]]$line 
     
     # Split the sentences to words
     # ---------------------------- 
     library(tidytext)
     
     #  WordList[[i]] = TextSentence[[i]]  %>%
     #   unnest_tokens(word, text) %>%
     #    dplyr::count(word, sort = TRUE)
     #   WordList[[i]]=as.data.frame(WordList[[i]])

     Word <-list()
     Sent = TextSentence[[i]]
     for(z in LineNum){
       Word[[z]] = Sent[z,]  %>%
        unnest_tokens(word, text) %>%
         dplyr::count(word, sort = TRUE)
       Word[[z]]=as.data.frame(Word[[z]])
       Word[[z]]$LineNum = z
     }
     WordList[[i]]=dplyr::bind_rows(Word) 

     #' Add the total word count of the text
     #' ------------------------------------   
     WordCount[[i]]=nrow(WordList[[i]])
     WordList[[i]]$WordCount=NA; WordList[[i]]$WordCount=WordCount[[i]]

     #' Add sentence count of the text
     #' ------------------------------   
     WordList[[i]]$SentCount=NA; WordList[[i]]$SentCount=SentCount[[i]]

     #' Add word lengths
     #' ----------------   
     WordList[[i]]$WordLength=NA; WordList[[i]]$WordLength=nchar(WordList[[i]]$word)   
     WordList[[i]]$TextType=NA
     WordList[[i]]$TextType=gsub(".*[/]([^.]+)[_].*", "\\1", text.path[i])
  }
   WordList=dplyr::bind_rows(WordList) 
   TextSentence=dplyr::bind_rows(TextSentence)

#' Save data
#' ---------   
   write.table(TextSentence, paste0("data/tidy_format/sentlist_", ListType, ".txt"), 
               sep="\t", quote=F, row.names=F)
   write.table(WordList, paste0("data/tidy_format/wordlist_", ListType, ".txt"), 
               sep="\t", quote=F, row.names=F)
   
   