
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

text.file = readLines(text.path[[1]])

base_loop <- function(text, doc_id){
  tokns <- strsplit(text, " ", fixed=TRUE)
  vocab <- sort(unique(unlist(tokns)))
  dtm <- matrix(data = 0L, 
                ncol = length(vocab), nrow = length(tokns),
                dimnames = list(doc_id, vocab) )
  freqs <- lapply(tokns, table)
  for (i in 1:length(freqs) ){
    doc <- freqs[[i]]
    dtm[i, names(doc)] <- as.integer(doc)
  }
  return(dtm)
}


base_lapply <- function(text, doc_id){
  tokns <- strsplit(text, " ") 
  vocab <- sort(unique(unlist(tokns)))
  FUN <- function(x, lev){tabulate(factor(x, levels = lev, 
                                          ordered = TRUE), 
                                   nbins = length(lev))}
  out <- lapply(tokns, FUN, lev = vocab)
  dtm <- as.matrix(t(do.call(cbind, out) ) )
  dimnames(dtm) <- list(doc_id, vocab) 
  return(dtm)
}

dtm1=base_loop(text.file[2], 1)
dtm2=base_lapply(text.file[2], 1)
class(a)

dim(dtm1) == dim(dtm2) 
sum(dtm2) == sum(dtm1)
dimnames(dtm1) %in% dimnames(dtm2)

