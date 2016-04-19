library(tm)
library(stringr)
library(shiny)

wf2 <- read.csv("Bigram table2.csv")
wf3 <- read.csv("Trigram table2.csv")
wf4 <- read.csv("Quadrigram table2.csv")


backoff <- function(x) 
{ 
  
  x <- removePunctuation(x)
  x <- removeNumbers(x) 
  x <- tolower(x)  
  x <- stripWhitespace(x)
  
  if (x == '')
  {paste('')} 
  else if (length(unlist(strsplit(x, " "))) >= 3 & word(x, -3, -1) %in% wf4$first)
  {paste(wf4$last[min(grep(word(x, -3, -1), wf4$first))])}
  else if (length(unlist(strsplit(x, " "))) >= 3 & !(word(x, -3, -1)  %in% wf4$first) & word(x, -2, -1)  %in% wf3$first)
  {paste(wf3$last[min(grep(word(x, -2, -1), wf3$first))])}
  else if (length(unlist(strsplit(x, " "))) >= 3 & !(word(x, -3, -1)  %in% wf4$first) & !(word(x, -2, -1)  %in% wf3$first) & word(x, -1)  %in% wf2$first)
  {paste(wf2$last[min(grep(word(x, -1), wf2$first))])}
  else if (length(unlist(strsplit(x, " "))) == 2 & word(x, -2, -1) %in% wf3$first)
  {paste(wf3$last[min(grep(word(x, -2, -1), wf3$first))])}
  else if (length(unlist(strsplit(x, " "))) == 3 & !(word(x, -2, -1)  %in% wf3$first) & word(x, -1)  %in% wf2$first)
  {paste(wf2$last[min(grep(word(x, -1), wf2$first))])}
  else if (length(unlist(strsplit(x, " "))) == 1 & x %in% wf2$first)
  {paste(wf2$last[min(grep(x, wf2$first))])}
  else paste("the")
}

shinyServer(
  function(input, output) {
    output$prediction <- renderText({backoff(input$phrase)})
    
  }
)