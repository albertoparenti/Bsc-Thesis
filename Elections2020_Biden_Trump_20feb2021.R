setwd("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/DONE/THESIS")
getwd()
library(dplyr)
library(RColorBrewer)
library(ngram)
library(tm)
library(wordcloud)
library(ggplot2)
library(tidyr)
library(syuzhet)
library(udpipe)
library(stringr)
library(rtweet)
library(ngram)
library(ggsci)

source("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS/funzioniTesto.R")
source("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS/sentimentFunctions.R")
source("C:/Users/Alberto Parenti/Downloads/STUDY/UNIBO/TO DO/THESIS/new_funzioni.R")

# per scaricare altri modelli per la lemmatizzazione in inglese si possono usare i comandi
# x <- udpipe_download_model(language = "english-partut")
# x <- udpipe_download_model(language = "english-gum")
# x <- udpipe_download_model(language = "english-lines")
ud_model_EN <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

load("dftwrc.RData") 
load("dftwrrc.RData") 
load("dftwr_b.RData")
load("dftwr3.RData")
load("dftwr4.RData")
load("dftwr5.RData")
load("dftwr6.RData")
load("dftwr7.RData")
load("dftwr8.RData")
load("dftwr9.RData")
load("dftwr10.RData")
load("dftwr11.RData")
load("dftwr12.RData")
load("syuz_sent.RData")
load("txtL_UV2.RData")
load("tmp2.RData")
load("outUV.RData")


# ----- operazioni di recupero e aggregazione dei tweet scaricati ----
# i file .RData con i tweet li ho messi tutti nella sotto-cartella "election2020"
# creo la lista con tutti i file .RData presenti nella cartella election2020b
lstFl <- list.files("./election2020b",pattern = "RData")
lstFl

# carico i file uno alla volta e creo il data.frame complessivo dftw
length(lstFl)
for(i in 1:length(lstFl)){
  print(i)
  temp.space <- new.env()
  bar <- load(paste("./election2020b/",lstFl[i],sep=""), temp.space)
  tmpdat <- get(bar, temp.space)
  tmpdat <- tmpdat %>% 
    select(user_id,status_id,created_at,screen_name,text,is_quote,is_retweet,favorite_count,retweet_count,quote_count,hashtags,mentions_screen_name)
  rm(temp.space)
  if(i == 1) {
    dftw <- tmpdat
  } else {
    dftw <- rbind(dftw,tmpdat)
  }
}

save(dftw,file="dftw_b.RData")

# individuo i tweet univoci
dftw$id <- 1:nrow(dftw)
unici <- dftw %>%
  group_by(status_id) %>%
  summarise(n=n(),id=min(id))

# creo il data.frame dftwr con i tweet univoci
dftwr <- dftw[dftw$id %in% unici$id,]
dim(dftwr)
save(dftwr,file="dftwr_b.RData")

# ---- definizione del tipo di tweet ----
# presenza di biden e/o trump nel testo del tweet
xJB <- grepl("biden",dftwr$text,ignore.case = T)
xDT <- grepl("trump",dftwr$text,ignore.case = T)
xPO <- grepl("potus",dftwr$text,ignore.case = T)
xJBDT <- ifelse(xJB==T & (xDT==T | xPO==T),"BT",ifelse(xJB==T,"JB",ifelse(xDT==T | xPO==T,"DT",NA)))
dftwr$tipo <- xJBDT
table(dftwr$tipo,useNA = "ifany")
# ci sono tweet in cui non è presente nessuno dei due nomi
# il conteggio viene fatto anche tenendo conto delle mention (@Biden)
# per trump vengono considerati i tweet in cui è presente la parola trump o potus

dftwr$text2 <- dftwr$text
dftwr$text2 <- gsub(intToUtf8(8217),intToUtf8(39),dftwr$text2)
dftwr$txt <- cleanTesto(dftwr$text2, punteggiatura = T, numeri = F, minuscolo = F, hashtag = F,mention = F)
dftwr$txt[1:10]
# dftwr$txt <- gsub("amp","",dftwr$txt)

# conteggio di quante volte è presente la parola biden in ciascun tweet
dftwr$nJB <- str_count(tolower(dftwr$text), "biden")
# conteggio di quante volte è presente la parola trump in ciascun tweet
dftwr$nDT <- str_count(tolower(dftwr$text), "trump|potus")

summary(dftwr$nJB)
summary(dftwr$nDT)
table(dftwr$nDT,xJBDT)
table(dftwr$nJB,xJBDT)
dftwr[!is.na(dftwr$tipo) & dftwr$nJB==1 & dftwr$tipo=="JB",c("id","text","nJB","nDT","tipo")]

tab1 <- dftwr %>%
  filter(!is.na(tipo)) %>%
  group_by(tipo,giorno=as.Date(created_at)) %>%
  summarise(n=n(),engagement=sum(favorite_count+retweet_count)) %>%
  mutate(engag_tweet=engagement/n)
tab1

ggplot(tab1,aes(x=giorno,y=n,color=tipo))+
  geom_line()+
  scale_color_manual(values = c("blue","red","green"))+
  theme_light()

ggplot(tab1,aes(x=giorno,y=n,fill()=tipo))+
  geom_bar(stat = "identity", position=position_dodge())+
  scale_fill_manual(values = c("blue","red","black"))

ggplot(tab1,aes(x=giorno,y=engag_tweet,fill=tipo))+
  geom_bar(stat = "identity", position=position_dodge())+
  scale_color_manual(values = c("blue","red","grey"))

# creazione sotto-dataset 
dftwr1 <- dftwr[1:173455,]
dftwr2 <- dftwr[173456:346910,]
dftwr3 <- dftwr[346911:520365,]
dftwr4 <- dftwr[520366:693820,]
dftwr5 <- dftwr[693821:867275,]
dftwr6 <- dftwr[867276:1040730,]
dftwr7 <- dftwr[1040731:1214185,]
dftwr8 <- dftwr[1214186:1387640,]
dftwr9 <- dftwr[1387641:1561095,]
dftwr10 <- dftwr[1561096:1734550,]
dftwr11 <- dftwr[1734551:1908005,]
dftwr12 <- dftwr[1908006:2081460,]

save(dftwr1,file="dftwr1.RData")
save(dftwr2,file="dftwr2.RData")
save(dftwr3,file="dftwr3.RData")
save(dftwr4,file="dftwr4.RData")
save(dftwr5,file="dftwr5.RData")
save(dftwr6,file="dftwr6.RData")
save(dftwr7,file="dftwr7.RData")
save(dftwr8,file="dftwr8.RData")
save(dftwr9,file="dftwr9.RData")
save(dftwr10,file="dftwr10.RData")
save(dftwr11,file="dftwr11.RData")
save(dftwr12,file="dftwr12.RData")

#creo campione di 200k osservazioni
dftwrc <- dftwr[sample(nrow(dftwr), 200000 ),]

save(dftwrc,file="dftwrc_b.RData")
summary(dftwrc$created_at) #periodo di tweet preso dal campione
ts_plot(dftwrc) #grafico per andamento per giorno


dftwrc$text2 <- dftwrc$text
dftwrc$text2 <- gsub(intToUtf8(8217),intToUtf8(39),dftwrc$text2)
dftwrc$txt <- cleanTesto(dftwrc$text2, punteggiatura = T, numeri = F, minuscolo = F, hashtag = F,mention = F)

visNGram(dftwrc$txt, ngrF = 4, nn = 50)

# vettore con correzioni delle forme composte (vedi note alla funzione in new_funzioni.R)
correz <- c("Joe Biden","Biden",
            "Biden and Kamala Harris","Biden_Harris",
            "Joe Biden and Kamala","Biden_Harris",
            "Hunter Biden",NA,
            "Biden Harris",NA,
            "U S","United_States",
            "White House",NA,
            "U S President","Potus",
            "United States",NA,
            "key contacts",NA,
            "Vice President",NA,
            "Biden and Kamala","Biden_Harris",
            "Biden and Harris","Biden_Harris",
            "Trump and Biden","Trump_Biden",
            "American people",NA,
            "Kamala Harris","Harris",
            "Donald Trump","Trump",
            "BIDEN HARRIS","Biden_Harris",
            "BIDEN","Biden",
            "HARRIS","Harris",
            "Democrat party",NA,
            "Chinese firm",NA,
            "President of the United States of America","Potus",
            "President of the United States","Potus",
            "President Elect",NA,
            "President elect",NA,
            'United States of America',"U.S.A.",
            "Biden and Vice President","Biden_Harris",
            "Amy Coney Barrett",NA,
            "President Trump","Trump",
            "President Donald Trump","Trump",
            "President Donald J Trump","Trump",
            "Donald J Trump","Trump",
            "Supreme Court",NA,
            "New York Times",NA,
            "Make America Great Again",NA,
            "GOP","Republican_Party",
            "Republican Party",NA,
            "Democrat Party",NA,
            "America Great Again",NA,
            "trump","Trump",
            "TRUMP","Trump",
            "Department of Homeland Security",NA,
            "Hunter Biden's","Hunter_Biden",
            "Joe Biden's","Biden",
            "Biden's","Biden",
            "Donald Trump's","Trump",
            "Trump's","Trump",
            "Second Amendment",NA,
            "First Lady",NA
)
dftwrc$txt <- corFrmComp(vText = dftwrc$txt, correzioni=correz)
#eliimino tweet con poche parole
# identifico quante parole in ogni testo
dftwrc$id <- 1:nrow(dftwrc)
dftwrc$nparole <- sapply(dftwrc$txt,wordcount)
summary(dftwrc$nparole)

# ad esempio decido di considerare solo i testi con almeno 5 parole
# escludo anche i tweet in cui non è citato nessuno dei due
dftwrrc <- dftwrc[dftwrc$nparole>5 & !is.na(dftwrc$tipo),]
dftwrrc$id <- 1:nrow(dftwrrc) # aggiungo un identificato progressivo ai testi selezionati

dftwrrc %>%
  group_by(tipo) %>%
  summarise(paroletot=sum(nparole),sum(nJB),sum(nDT))

summary(dftwrrc$nparole)

save(dftwrrc,file="dftwrrc.RData")
# analisi degli hashtag
dfHash <- dftwrrc %>%
  select(status_id,tipo,hashtags) %>%
  group_by(status_id) %>%
  mutate(allHash=paste(unlist(hashtags),collapse = " ")) %>%
  select(status_id,tipo,allHash)

dfHashGR <- dfHash %>%
  select(tipo,allHash) %>%
  filter(allHash!="NA") %>%
  group_by(tipo) %>%
  summarise(hash=paste(allHash,collapse = " "))

mystop <- c("biden","trump","Biden","Trump")
# con ponderazione tf
tdmHS <- creaTDM(dfHashGR$hash,lemmatiz = F,mystopwords = mystop)
wordcloud(words = tdmHS$freq.Frm$forme,freq = tdmHS$freq.Frm$freq, max.words = 100,
          random.order = F,colors = brewer.pal(4,"Dark2"))
# con ponderazione tfidf
tdmHS <- creaTDM(dfHashGR$hash,lemmatiz = F,weight = "tfidf",mystopwords = mystop)
wordcloud(words = tdmHS$freq.Frm$forme,freq = tdmHS$freq.Frm$freq, max.words = 100,
          random.order = F,colors = brewer.pal(4,"Dark2"))
#comparazione 
## hashtag per candidato
tdmHS <- creaTDM(dfHashGR$hash,lemmatiz = F,mystopwords = mystop)
colnames(tdmHS$tdm.Frm) <- c("Biden-Trump","Trump","Biden")
par(mar=c(0,0,0,0))
comparison.cloud(
  term.matrix = tdmHS$tdm.Frm, 
  scale = c(3,0.3), 
  max.words = 100, 
  random.order = F, 
  rot.per = 0.2, 
  colors = c("gray20","darkred","blue"), match.colors = T,
  title.size = 2.5, 
  title.bg.colors = "white") 


# ripulisco i testi da tutte le mention e dagli hashtag
dftwrrc$txtp <- cleanTesto(dftwrrc$txt, punteggiatura = T, numeri = F, minuscolo = F, hashtag = T,mention = T)

#---lemmatizzazione---
outUV <- lemmaUDP(dftwrrc$txtp,model = ud_model_EN,doc_id = dftwrrc$id, stopw = tm::stopwords("en"))

head(outUV[,c(1:3,5:9,15)],40) # contenuto dell'output della lemmatizzazione

save(outUV,file="outUV.RData")
load("outUV.RData")

txtL_UV <- outUV %>%             #in questo ciclo  ricreo frasi lemmatizzate
  filter(!is.na(outUV$lemma) & outUV$STOP==FALSE) %>%
  select(doc_id,lemma) %>%
  group_by(doc_id=as.numeric(doc_id)) %>%
  summarise(txtL=paste(lemma,collapse = ""))
txtL_UV
save(txtL_UV,file="txtL_UV.RData")
load("txtL_UV.RData")
# # sistemo i simboli # e @ isolati
txtL_UV$txtL <- gsub(" # "," #",txtL_UV$txtL)
 txtL_UV$txtL <- gsub(" @ "," @",txtL_UV$txtL)


# versione con eliminazione degli xpos e upos "inutili"

outUV %>%
  filter(upos=="AUX") %>%
  group_by(lemma,STOP) %>%
  summarise(n=n()) %>%
  spread(STOP,n)

xpos_del <- c("PRP","DT")
upos_del <- c("SCONJ","CCONJ","NUM","AUX")
txtL_UV2 <- outUV %>%      #ricosrtuuisco i testi dei messaggi lemmattizati eliminando stopwords e congiunzioni,numeri non inclusi nelel stopwords
  filter(!is.na(outUV$lemma) & outUV$STOP==FALSE & !outUV$xpos %in% xpos_del & !outUV$upos %in% upos_del) %>%
  select(doc_id,lemma) %>%
  group_by(doc_id=as.numeric(doc_id)) %>%
  summarise(txtL=paste(lemma,collapse = " "))
txtL_UV2

save(txtL_UV2,file="txtL_UV2.RData")
load("txtL_UV2.RData")

# # sistemo i simboli # e @ isolati
 txtL_UV2$txtL <- gsub(" # "," #",txtL_UV2$txtL)
txtL_UV2$txtL <- gsub(" @ "," @",txtL_UV2$txtL)


mystpw <- c("elections2020","election2020","election","2020","vote")
save(mystpw,file="mystpw.RData")
load("mystpw.RData")

risIDF2 <- creaTDM(tolower(txtL_UV2$txtL), lemmatiz = F, mystopwords = mystpw, output = "all",
                   weight = "tfidf", lang = "english")
names(risIDF2)
head(risIDF2$freq.Frm)
library(ggsci) # libreria con tavolozze di colori
par(mar=c(0,0,0,0))
wordcloud(risIDF2$freq.Frm$forme,risIDF2$freq.Frm$freq,
          max.words = 125,random.order = F, scale = c(2, 0.3),
          colors = rev(pal_lancet("lanonc")(4)))



#------
xxt <- tolower(txtL_UV2$txtL)
txxt <- removeWords(xxt, stopwords("english"))
txxt = gsub('\\s+',' ',txxt)
txxt = gsub("^\\s+|\\s+$", "", txxt)
crp <- Corpus(VectorSource(txxt))
wordLengths=c(3,Inf)
tdm <- TermDocumentMatrix(crp, control=list(wordLengths=wordLengths))
tdm <- weightTfIdf(tdm)
# tdmm <- as.matrix(tdm)
# rimuovo i termini sparsi per ridurre la matrice
tdmr <- removeSparseTerms(tdm,0.999)
save(tdmr,file="tdmr.RData")
load("tdmr.RData")
tdmr <- as.matrix(tdmr)
# dim(tdmr)
dfFrq <- data.frame(forme = rownames(tdmr), freq = rowSums(tdmr))
rownames(dfFrq) <- NULL
dfFrq <- dfFrq[order(dfFrq$freq, decreasing = T),]
library(ggsci) # libreria con tavolozze di colori
par(mar=c(0,0,0,0))
wordcloud(dfFrq$forme,dfFrq$freq,
          max.words = 125,random.order = F, scale = c(2, 0.3),
          colors = rev(pal_lancet("lanonc")(4)))


#####-----
left_join(twr %>%
            select(id,created_at) %>%
            mutate(giorno=as.Date(created_at)),
          scoresUI$overall %>%
          select(id=doc_id,sentiment_polarity) %>%
            mutate(id=as.numeric(id)),by="id")


