# GISDK crawlR
# 
# Command Line Arguments: todo
#     
# args = commandArgs(trailingOnly=FALSE)

library(dplyr)
library(stringr)
`%!in%` <- Negate(`%in%`)

gisdk <- readLines("./NIRCC.rsc")

# match patterns
regpatterns <- c("Macro \"([^\"]*)\"",
                 "endMacro",
                 "Button \"([^\"]*)\".*do",
                 "endItem",
                 "RunMacro\\(\"([^\"]*)\"[^)]*\\)")

# ignore frequently used helper functions
helpers <- c("G30 File Close All", "G30 new layer default settings", "G30 update map toolbar",
             "TCB Init", "TCB Run Procedure", "TCB Run Operation", "TCB Create View Set", "TCB GetDataVector", "TCB Closing",
             "Set Default RS Style","CheckMatrixCore","CheckMatrixIndex","addfields","LoadConfig","AddLayer", "check inputs", "copy inputs")

# map the main model runner function first
runner <- "runbutton"

foundDefs <- data.frame(name = character(0), startline = numeric(0), endline = numeric(0))
foundCalls <- data.frame(name = character(0), line = numeric(0))

getMacroEnd <- function(start){
  for (i in seq(start,length(gisdk))){
    if(grepl(regpatterns[2],gisdk[i],ignore.case = T)) { return(i) }
  }
}

getButtonEnd <- function(start){
  for (i in seq(start,length(gisdk))){
    if(grepl(regpatterns[4],gisdk[i],ignore.case = T)) { return(i) }
  }
}

for (i in seq(1,length(gisdk))){
  withinMacro <- NULL
  if(grepl("Macro \"",gisdk[i])) {
    macroDef <- stringr::str_extract(gisdk[i], regpatterns[1], group = 1)
    macroEnd <- getMacroEnd(i)
    foundDefs <- rbind(foundDefs, list(name = macroDef, startline = i, endline = macroEnd))
    #cat(i,": DEF ",macroDef,"\n")
  }
  
  if(grepl("Button \"",gisdk[i])) {
    macroDef <- stringr::str_extract(gisdk[i], regpatterns[3], group = 1)
    macroEnd <- getButtonEnd(i)
    foundDefs <- rbind(foundDefs, list(name = macroDef, startline = i, endline = macroEnd))
    #cat(i,": DEF ",macroDef,"\n")
  }
  
  if(grepl("RunMacro",gisdk[i])) {
    runCalls <- stringr::str_extract(gisdk[i], regpatterns[5], group = 1)
    
    for(runmacro in runCalls) {
      foundCalls <- rbind(foundCalls, list(name = runmacro, line = i))
      #cat(i,": CALL ",runmacro,"\n")
    }
  }
}

# create an inequality join where the left side line number is between the right side macro defined between startline and endline
# this gives us a one-to-many join of the called functions (in order of being called) and the calling macro
joinCaller <- join_by(between(line, startline, endline))
callTree <- foundCalls %>% 
  filter(name %!in% helpers) %>%
  left_join(foundDefs, by = joinCaller) %>%
  mutate(caller = name.y) %>%
  select(name = name.x,line,caller)

# build map
mainRunner <- callTree %>%
  filter(caller == runner) #%>% pull(name)

# find the second instance of the first called Macro to ID feedback loop
reloop <- which(mainRunner$name == mainRunner$name[1])[2]

mainRunner <- mainRunner %>% pull(name)
mainRunner <- mainRunner[1:reloop]

# write out mermaid graphs
# define macros so we can use spaces in their pretty names, underscores in IDs
# assume main function call runs macros in line sequence (ignores conditions)
mainDefs <- paste0(gsub("\\s","_",mainRunner),"[",mainRunner,"]")
mainGraph <- paste(gsub("\\s","_",mainRunner), collapse = "-->")
  
writeLines(c(mainDefs,mainGraph), sep = ";\n", con = "out.txt")