
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

# per scaricare altri modelli per la lemmatizzazione in inglese si possono usare i comandi
# x <- udpipe_download_model(language = "english-partut")
# x <- udpipe_download_model(language = "english-gum")
# x <- udpipe_download_model(language = "english-lines")
ud_model_EN <- udpipe_load_model("english-ewt-ud-2.4-190531.udpipe")

load("dftwr.RData") #riprendere linea 65

# ----- operazioni di recupero e aggregazione dei tweet scaricati ----
# i file .RData con i tweet li ho messi tutti nella sotto-cartella "election2020"
# creo la lista con tutti i file .RData presenti nella cartella election2020
lstFl <- list.files("./election2020",pattern = "RData")
lstFl

# carico i file uno alla volta
# carico i file uno alla volta e creo il data.frame complessivo dftw
length(lstFl)
for(i in 1:length(lstFl)){
  print(i)
  temp.space <- new.env()
  bar <- load(paste("./election2020/",lstFl[i],sep=""), temp.space)
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

save(dftw,file="dftw.RData")

# individuo i tweet univoci
dftw$id <- 1:nrow(dftw)
unici <- dftw %>%
  group_by(status_id) %>%
  summarise(n=n(),id=min(id))

# creo il data.frame dftwr con i tweet univoci
dftwr <- dftw[dftw$id %in% unici$id,]
dim(dftwr)
save(dftwr,file="dftwr.RData")

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
  theme_light()

ggplot(tab1,aes(x=giorno,y=n,fill=tipo))+
  geom_bar(stat = "identity", position=position_dodge())

ggplot(tab1,aes(x=giorno,y=engag_tweet,fill=tipo))+
  geom_bar(stat = "identity", position=position_dodge())

# ricerca forme composte
visNGram(dftwr$txt, ngrF = 4, nn = 50)

# vettore con correzioni delle forme composte (vedi note alla funzione in new_funzioni.R)
correz <- c("Joe Biden","Biden",
            "Donald Trump","Trump",
            "Kamala Harris",NA,
            "Vice President",NA,
            "United States",NA,
            "White House",NA,
            "Hunter Biden",NA)
dftwr$txt <- corFrmComp(vText = dftwr$txt, correzioni=correz)

# identifico quante parole in ogni testo
dftwr$id <- 1:nrow(dftwr)
dftwr$nparole <- sapply(dftwr$txt,wordcount)
summary(dftwr$nparole)
# ad esempio decido di considerare solo i testi con almeno 5 parole
# escludo anche i tweet in cui non è citato nessuno dei due
dftwrr <- dftwr[dftwr$nparole>5 & !is.na(dftwr$tipo),]
dftwrr$id <- 1:nrow(dftwrr) # aggiungo un identificato progressivo ai testi selezionati

dftwrr %>%
  group_by(tipo) %>%
  summarise(paroletot=sum(nparole),sum(nJB),sum(nDT))

# analisi degli hashtag
dfHash <- dftwrr %>%
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
dftwrr$txtp <- cleanTesto(dftwrr$txt, punteggiatura = T, numeri = F, minuscolo = F, hashtag = T,mention = T)

#---lemmatizzazione---
outUV <- lemmaUDP(dftwrr$txtp,model = ud_model_EN,doc_id = dftwrr$id, stopw = tm::stopwords("en"))

head(outUV[,c(1:3,5:9,15)],40) # contenuto dell'output della lemmatizzazione

txtL_UV <- outUV %>%             #in questo ciclo  ricreo frasi lemmatizzate
  filter(!is.na(outUV$lemma) & outUV$STOP==FALSE) %>%
  select(doc_id,lemma) %>%
  group_by(doc_id=as.numeric(doc_id)) %>%
  summarise(txtL=paste(lemma,collapse = " "))
txtL_UV
# # sistemo i simboli # e @ isolati
# txtL_UV$txtL <- gsub(" # "," #",txtL_UV$txtL)
# txtL_UV$txtL <- gsub(" @ "," @",txtL_UV$txtL)


# versione con eliminazione degli xpos e upos "inutili"

outUV %>%
  filter(upos=="AUX") %>%
  group_by(lemma,STOP) %>%
  summarise(n=n()) %>%
  spread(STOP,n)

xpos_del <- c("PRP","DT")
upos_del <- c("SCONJ","CCONJ","NUM","AUX")
txtL_UV2 <- outUV %>%
  filter(!is.na(outUV$lemma) & outUV$STOP==FALSE & !outUV$xpos %in% xpos_del & !outUV$upos %in% upos_del) %>%
  select(doc_id,lemma) %>%
  group_by(doc_id=as.numeric(doc_id)) %>%
  summarise(txtL=paste(lemma,collapse = " "))
txtL_UV2
# # sistemo i simboli # e @ isolati
# txtL_UV2$txtL <- gsub(" # "," #",txtL_UV2$txtL)
# txtL_UV2$txtL <- gsub(" @ "," @",txtL_UV2$txtL)


mystpw <- c("elections2020","election2020","election","2020","vote")
risIDF2 <- creaTDM(tolower(txtL_UV2$txtL), lemmatiz = F, mystopwords = mystpw, output = "all",
                   weight = "tfidf", lang = "english")
names(risIDF2)
head(risIDF2$freq.Frm)
library(ggsci) # libreria con tavolozze di colori
par(mar=c(0,0,0,0))
wordcloud(risIDF2$freq.Frm$forme,risIDF2$freq.Frm$freq,
          max.words = 125,random.order = F, scale = c(2, 0.3),
          colors = rev(pal_lancet("lanonc")(4)))

