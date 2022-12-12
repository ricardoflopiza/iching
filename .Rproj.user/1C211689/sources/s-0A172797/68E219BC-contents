
library(pdftools)
library(quanteda)
library(readtext)
library(stringi)
library(mgsub)
library(stringr)
library(tabulizer)
library(pdfsearch)
library(tidyverse)
library(ggplot2)


 iching <- pdf_text("iching.pdf")

 fe<- pdf_toc("iching.pdf")

# pdf_toc("iching.pdf") -> hexagramas
# 
# saveRDS(hexagramas,"iching.rds")
hexagramas <- readRDS("iching.rds")

hexa <- NA
for (i in 1:72) {
   hexa[i] <- print(hexagramas[2][[1]][i][[1]][[1]])
}

hexa[9:72] -> hexa

for (i in 1:64) {
   #pag[i] <- grep(hexa[i], iching)   
print(grep(hexa[i], iching))
   }
   
ichingl <- tolower(iching)
hexal <- tolower(hexa)

mgsub( hexal, c("/", "\\(", "\\)", "\n") , c("","", ""," ")) -> hexal
mgsub( ichingl, c("/", "\\(", "\\)", "\n") , c("","", ""," ")) -> ichingl

str_squish(ichingl) -> ichingl
str_squish(hexal) -> hexal

paste0(1:64,".-") -> poshex

paste(ichingl, collapse = " ") -> uiching

str_split(uiching," ",simplify = T) -> tok_iching

t(tok_iching) -> tok_iching

rm(list = setdiff(ls(),c("tok_iching","poshex", "hexal")))

tok_iching[grep(paste0("^",poshex[1],"$"),tok_iching):length(tok_iching)] -> tok_iching

hexal

poshex[poshex == "6.-"]  = "6.-sung"
poshex[poshex == "43.-"]   = "43."

tok_ichingDF = data.frame("N" = 1:length(tok_iching),
                          "token" = tok_iching)

contexto <- function(n,cant)
{ 
   p1 <- n-cant
   p2 <- n+cant
   return(tok_ichingDF$token[tok_ichingDF$N > p1 & tok_ichingDF$N < p2])
}

for (i in grep("^2$",tok_iching)) {
print(contexto(i, 0))
print(i)
}

contexto(32728, 23)

32728 + 22

c(tok_iching[1:32727],c("29.-", "k’an","lo","insondable","el","abismo,","el","agua"),tok_iching[32750:72045]) -> tok_iching
      

#### obtenemos posiciones de los títulos
str_remove(string = tok_iching, pattern = "[[:punct:]]") -> tok_iching

mgsub("\\.", "", tok_iching)

pos <- NA
for (i in 1:64) {
   if(length(grep(paste0("^",poshex[i],"$"),tok_ichingCL)) != 0){
   pos[i] <- grep(paste0("^",poshex[i],"$"),tok_ichingCL)
   }else{
      pos[i] <- NA
   }
};pos


contexto(32728, 23)


df <- data.frame("N" = 1:64,
"pos_hex" = poshex,
   "hexa" = hexal,
   "pos"= pos)

df$text <- NA
for (i in 1:64) {
   
   if(i != 64){
   df$text[i] <- list(tok_ichingDF$token[tok_ichingDF$N >= df$pos[i] & tok_ichingDF$N < df$pos[i+1]])   
   }else{
      df$text[i] <- list(tok_ichingDF$token[tok_ichingDF$N >= df$pos[i] & tok_ichingDF$N < length(tok_iching)])
}
   }

tok_ichingDF$token[tok_ichingDF$N  >= df$pos[64]]

tok_ichingDF$token[tok_ichingDF$N >= df$pos[64] & tok_ichingDF$N < df$pos[3+1]][1:10]

stopwors <- stopwords("es")

df %>% 
   unnest(text) -> dfTidy

gsub("\\.","",dfTidy$text) -> dfTidy$text
gsub(",","",dfTidy$text) -> dfTidy$text
gsub('"',"",dfTidy$text) -> dfTidy$text
gsub(':',"",dfTidy$text) -> dfTidy$text

dfTidy %>% 
   filter(!text %in% c(";",":","*","") & !text %in% stopwors)-> dfTidy

dfTidy %>% 
   count(text, sort = T) %>%  select(text) %>%  slice(1:10) -> pgen

for (i in 1:64) {
   print(df$hexa[i])
dfTidy[dfTidy$N == i,] %>% 
   filter(!text %in% c(setdiff(pgen$text,"hombre"), "hexagrama")) %>% 
#   group_by(N) %>% 
count(text, sort = T) %>% 
   print(n= 10)
   
}


elementos <- c("agua", "tierra", "fuego","viento","madera","metal")

table(dfTidy$text == elementos[1])


dfTidy$agua = ifelse(dfTidy$text %in% elementos[1], 1, 0)
dfTidy$tierra = ifelse(dfTidy$text %in% elementos[2], 1, 0)
dfTidy$fuego = ifelse(dfTidy$text %in% elementos[3], 1, 0)
dfTidy$viento = ifelse(dfTidy$text %in% elementos[4], 1, 0)
dfTidy$madera = ifelse(dfTidy$text %in% elementos[5], 1, 0)
dfTidy$metal = ifelse(dfTidy$text %in% elementos[6], 1, 0) 

dfTidy %>% 
   group_by(N) %>% 
   summarise_at(vars(agua, tierra, fuego, viento, madera, metal), sum) %>% 
   print(n = 64)

#tok_iching[ tok_iching == poshex[1]]

cielo <- c(1,1,1)
tierra <- c(0,0,0)

crossing(trigrama1 = c("111","000","101","010","100","011","001","110"),trigrama2 = c("111","000","101","010","100","011","001","110")) %>% 
  mutate(sintesis = paste0(trigrama1,trigrama2))


paste(rep(cielo, each = length(tierra)), tierra, sep = ",")

































