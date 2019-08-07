### Rejected Sponsor Letter
### Mikaela Miller
# FILEPATH = "Rejected Sponsor Letters/DataToTestFlags-201908071451-WIN1252.csv"
# SEARCHLIST = "Rejected Sponsor Letters/Search_list.csv"

sponsorLetters <- function(FILEPATH, SEARCHLIST){
  if (!require("pacman")) install.packages("pacman")
  library(pacman)
  pacman::p_load(tidytext, dplyr, stringr, textcat, data.table)

  soas = c("CI Bicol", "CI Manila", "CI Colombia", "Lusaka", "Guatemala", "Guayaquil", "Quito","Santo Domingo", 
           "San Pedro Sula", "Jalisco", "Sahay", "Delhi", "Little Rock")
  
  stringsAsFactors = FALSE
  text <- read.csv(FILEPATH)
  text$ChildName = as.character(text$ChildName)

  text$BODY = as.character(tolower(text$CorrespondenceBody))
  text$BODY =  gsub("\r?\n|\r", " ", text$BODY)
  text$language = textcat(text$BODY)
  text$language = ifelse(text$language %in% c("german", "manx", "middle_frisian", "frisian", "scots", "english"), "english", 
                      ifelse(text$language %in% c("spanish", "portuguese", "catalan", "rumantsch"), "spanish", "other"))

 search_list = read.csv(SEARCHLIST)
 
 search_list = split(search_list, f=search_list$flag)
 ls_names = NULL
 for(i in 1:length(search_list)){
   ls_names[[i]] = as.character(search_list[[i]][1,2])
    }
 ls_names=unlist(ls_names)
 names(search_list)=ls_names
 list2env(search_list, envir = .GlobalEnv)
 
 bad_words = tolower(c(as.character(badword_eng$text), as.character(badword_sp$text)))
 bad_phr = tolower(c( as.character(bad_phr_eng$text),  as.character(bad_phr_sp$text)))
 
 contact_words = tolower(c(as.character(contact_words_eng$text), as.character(contact_words_sp$text)))
 contact_phr = tolower(c(as.character(contact_phr_eng$text), as.character(contact_phr_sp$text)))
 
 gift_words = tolower(c(as.character(gift_words_eng$text), as.character(gift_words_sp$text)))
 gift_phr = tolower(c(as.character(gift_phr_eng$text), as.character(gift_phr_sp$text)))
 
 religion_words = (c(as.character(religion_words_eng$text), as.character(religion_words_sp$text)))
 
 education_words = tolower(c(as.character(education_words_eng$text), as.character(education_words_sp$text)))
 education_phrases = tolower(c(as.character(education_phr_eng$text), as.character(education_phr_sp$text)))
  
  tok_letter4 = text %>% unnest_tokens(word, BODY, token="ngrams", n=6)
  tok_letter2 = text %>% unnest_tokens(word, BODY, token="ngrams", n=2)
  tok_letter = text %>% unnest_tokens(word, BODY)
  tok_name = text %>% unnest_tokens(word, ChildName)
  tok_name = tok_name[!(tok_name$word %in% letters),]
  tok_name$name_match = NULL
  for(i in 1:length(rownames(tok_name))){
    tok_name$name_match[i] =  ifelse(tok_name$BODY[i] %like% tok_name$word[i], 1, 0)
  }
  tok_name_0 = tok_name %>% group_by(ChildID, DigitalAssetID) %>% summarize(name_match_max = max(name_match))
  tok_name_0 = tok_name_0[tok_name_0$name_match_max==0, 2]
  ### Remove stop words
  data(stop_words)
  tok_letter <- tok_letter %>%
    anti_join(stop_words)
  tok_name <- tok_name %>%
    anti_join(stop_words)
  
  numextract <- function(string){ 
    str_extract(string, "\\-*\\d+\\.*\\d*")
  } 
  isNumeric <- function(string){
    ## test whether a character vector contains numbers only
    x <- numextract(string)
    notNA <- which(!is.na(x))
    ifelse(length(notNA)==0, FALSE, all(x[notNA]==str_trim(string[notNA])))
  }
  tok_letter$numeric = NULL
  for(i in 1:length(rownames(tok_letter))){
    tok_letter$numeric[i] = isNumeric(tok_letter$word[i])
  }
  tok_letter$potential_contact = ifelse(tok_letter$numeric==TRUE & nchar(tok_letter$word)>=5, 1, 0)
  # phone_num = unique(unlist(stri_extract_all_regex(tok_letter$body, '(\\d[.-])?\\(?\\d{3}\\)?[-. ]?\\d{3}[-. ]?\\d{4}\\b')))
  # phon_num_ID = which(tok_letter$word %in% phone_num)
  
  ### Create Flags
  tok_letter$bad_word_flag = ifelse(tok_letter$word %in% badword_eng$text & tok_letter$language != "spanish", 1, 
                                    ifelse(tok_letter$word %in% badword_sp$text &tok_letter$language=="spanish", 1, 0))
  text$bad_word_fl = ifelse(grepl(paste(bad_phr, collapse = "|"), text$BODY)==TRUE, 1, 0)
  tok_letter$sp_needs_translate = ifelse(tok_letter$language=="spanish" & tok_letter$ChildAgency %in%
                                           c("CI Bicol", "CI Manila", "Lusaka", "Sahay", "Delhi", "Little Rock"), 1, 0)
  tok_letter$contact_flag = ifelse(tok_letter$language != "spanish" & tok_letter$word %in% contact_words_eng$text |
                                     tok_letter$language == "spanish" & tok_letter$word %in% contact_words_sp$text |
                                     tok_letter$potential_contact ==1 ,1, 0)
  text$contact_fl = ifelse(grepl(paste(contact_phr, collapse = "|"), text$BODY)==TRUE, 1, 0)
  tok_letter$gift_flag = ifelse(tok_letter$language != "spanish" & tok_letter$word %in% gift_words_eng$text |
                                  tok_letter$language=="spanish" & tok_letter$word %in% gift_words_sp$text ,1, 0)
  text$gift_fl = ifelse(grepl(paste(gift_phr, collapse = "|"), text$BODY)==TRUE, 1, 0)
  
  tok_letter4 = tok_letter4[substring(tok_letter4$word, 1, 4) %in% c("send",  "sent", "paid", "mand") |
                              substring(tok_letter4$word, 1, 6) %in% c("te env", "paying", "les ma", "les en","mandar","enviar", "te man") ,]
  tok_letter4$ngram_send_flag = ifelse(tok_letter4$word %like% "picture" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "card" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "photo" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "hug" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "kiss" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "love" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "update" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "blessing" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "news" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "attention" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "thought"& tok_letter4$language != "spanish" |
                                         tok_letter4$word %like% "wish"& tok_letter4$language != "spanish" |
                                         tok_letter4$word %like% "regard"& tok_letter4$language != "spanish" |
                                         tok_letter4$word %like% "letter" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "note" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "condolence" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "drawing" & tok_letter4$language != "spanish"|
                                         tok_letter4$word %like% "beso" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "besito" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "fuerte" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "abrazo" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "gran" & tok_letter4$language == "spanish" |
                                         tok_letter4$word %like% "foto" & tok_letter4$language == "spanish" |
                                         tok_letter4$word %like% "fotito" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "amor" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "saludo" & tok_letter4$language == "spanish"|
                                         tok_letter4$word %like% "cariño" & tok_letter4$language == "spanish", 0, 1)
  tok_letter4 = tok_letter4[tok_letter4$ngram_send_flag == 1,]
  tok_letter4 = tok_letter4[!(tok_letter4$word %like% " me" |
                                tok_letter4$word %like% " my" |
                                tok_letter4$word %like% " us"),]
  tok_letter4.1 = tok_letter4[(tok_letter4$word %like% "send you"|
                                 tok_letter4$word %like% "sending you" |
                                 tok_letter4$word %like% "sent you" |
                                 tok_letter4$word %like% "send them" |
                                 tok_letter4$word %like% "send it" |
                                 tok_letter4$word %like% "te env" |
                                 tok_letter4$word %like% "te mand" |
                                 tok_letter4$word %like% "sent to"), ]
  
  tok_letter2 = tok_letter2[tok_letter2$word %in% c("see", "find", "finding")|
                              substring(tok_letter2$word, 1, 4) %in% c("meet") |
                              substring(tok_letter2$word, 1, 5) %in% c("visit", "follo", "found") ,]
  tok_letter2$ngram_send_flag = ifelse(sub("^\\S+\\s+", '', tok_letter2$word) %in% c("us", "you") & tok_letter2$language != "spanish", 1, 0)
  tok_letter2 = tok_letter2[tok_letter2$ngram_send_flag==1,]
  
  tok_letter$religion_flag = ifelse(tok_letter$ChildAgency %in% c("Sahay", "Delhi") & tok_letter$word %in% religion_words,1, 0)
  text$visit_fl = ifelse(grepl(paste(visit_phr$text, collapse = "|"), text$BODY)==TRUE, 1, 0)
  tok_letter$education_fl = ifelse(tok_letter$word %in% education_words, 1, 0)
  text$education_flag  = ifelse(grepl(paste(education_phrases, collapse = "|"), text$BODY)==TRUE, 1, 0)
  
  tok_letter$money_flag = ifelse(tok_letter$CorrespondenceBody %like% "\\$", 1, 0)
  tok_letter$email_flag = ifelse(tok_letter$CorrespondenceBody %like% c("\\@"), 1, 0)
  # tok_letter$picture_mention = ifelse(tok_letter$word %in% picture, 1, 0)
  
  sp_needs_translate = unique(tok_letter[tok_letter$sp_needs_translate == 1, colnames(tok_letter) %in% "DigitalAssetID",])
  gift = unique(tok_letter[tok_letter$gift_flag == 1 | tok_letter$money_flag==1, colnames(tok_letter) %in% "DigitalAssetID",])
  contact = unique(tok_letter[tok_letter$contact_flag == 1 | tok_letter$email_flag==1, colnames(tok_letter) %in% "DigitalAssetID",])
  religion = unique(tok_letter[tok_letter$religion_flag == 1, colnames(tok_letter) %in% "DigitalAssetID",])
  # visit = unique(tok_letter[tok_letter$visit_flag == 1,colnames(tok_letter) %in% "page",])
  education = unique(tok_letter[tok_letter$education_fl == 1 , colnames(tok_letter) %in% "DigitalAssetID",])
  badword = unique(tok_letter[tok_letter$bad_word_flag == 1 , colnames(tok_letter) %in% "DigitalAssetID",])
  # picture_mention =  unique(tok_letter[tok_letter$picture_mention == 1, colnames(tok_letter) %in% "page",])
  
  text$sp_needs_translate = ifelse(text$DigitalAssetID %in% sp_needs_translate, "sp_needs_translate", "")
  text$gift = ifelse(text$DigitalAssetID %in% gift | text$gift_fl==1 | text$DigitalAssetID %in% tok_letter4.1$DigitalAssetID, "gift", "")
  text$contact = ifelse(text$DigitalAssetID %in% contact | text$contact_fl==1 |text$DigitalAssetID %in% tok_letter2$DigitalAssetID, "contact", "")
  text$religion_India = ifelse(text$DigitalAssetID %in% religion, "religion_India", "")
  text$visit = ifelse(text$visit_fl == 1, "visit", "")
  text$education = ifelse(text$DigitalAssetID %in% education | text$education_flag ==1, "education", "")
  text$badword = ifelse(text$DigitalAssetID %in% badword | text$bad_word_fl==1, "bad_word", "")
  text$name_mismatch = ifelse(text$DigitalAssetID %in% tok_name_0$DigitalAssetID, "name_mismatch", "")
  
  inds = which(names(text) %in% c("name_mismatch", "sp_needs_translate", "gift", "contact", "religion_India", "visit", 
                                  "education", "badword"))
  text$anyFlag = apply(text[,inds], 1, function(x) ifelse(sum(x=="Y", na.rm=T)>=1, "Y", ""))
  # which(text$anyflag=="Y")
  # report = text[text$anyFlag =="Y",]
  report=text
  report = dplyr::select(report, c(DigitalAssetID, language, sp_needs_translate:name_mismatch))
  names(report) = c("DigitalAssetID","Language", "Sp Needs Translated", "Gift", "Contact", "Religion (India)", "Visit", "Education", "Bad Word",
                    "Name Discrepency")
  report$Flags = apply(report[,2:10], 1, function(x) paste(x[!x==""], collapse = ", "))
  report = report %>% select(c(DigitalAssetID, Flags))
  return(report)
}