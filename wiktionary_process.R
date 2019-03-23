
#########################################################
####################### WikiNouns #######################
#########################################################


# This script extracts a CSV sheet containing information
# about syllabification, grammatical gender, and pronunciation
# of German nouns from the current Wiktionary dump.

# For the current version, this dump was used:
# https://dumps.wikimedia.org/dewiktionary/20190301/dewiktionary-20190301-pages-articles.xml.bz2

# Code by Peeter Tinits, modified by Stefan Hartmann
# CC-BY-SA-4.0


# TODO: currently this script works with 11 different
# dataframes to ensure that one can always go back to
# a previous one. As soon as the script works reliably,
# this can be dropped and we can work with one single
# dataframe d.



# preliminaries -----------------------------------------------------------

# load packages
library(data.table)
library(stringr)
library(zoo)
library(tidyverse)
library(ngram)
library(stringi)



# read and modify file ----------------------------------------------------

# read file
d <- fread("dewiktionary-20190220-pages-articles-multistream.xml",sep="~",header=F)

# set name of the (currently only) column
names(d) <- "V1"

# get the titles (i.e. lexemes)
d[str_detect(V1,"title"), title:=T]

# get syllabification
d[str_detect(V1,"Worttrennung"),worttrenn2:=T]
d[str_detect(shift(V1,1),"Worttrennung"),worttrenn2:=T]
d[str_detect(shift(V1,2),"Worttrennung"),worttrenn2:=T]


# get pronunciation
d[str_detect(V1,"Aussprache"),aussprache1:=T]
d[str_detect(shift(V1,1),"Aussprache"),aussprache1:=T]
d[str_detect(shift(V1,2),"Aussprache"),aussprache1:=T]
d[str_detect(shift(V1,3),"Aussprache"),aussprache1:=T]
d[str_detect(shift(V1,4),"Aussprache"),aussprache1:=T]

#d[str_detect(V1,"Anmerkung"),worttrenn:=F]

d[str_detect(V1,"Sprache\\|"),sprache:=V1]
d[,sprache2:=str_extract(sprache,"Sprache\\|[a-zA-Z]+")]
d[!is.na(sprache),has_lang:=T]

# gender
d[str_detect(V1, "\\|Genus( 1)?="), genus:=V1]
d[, genus2 := str_extract(genus, "\\|Genus( 1)?=[a-zA-Z]+")]
d[!is.na(genus), has_gender:=T]

d[str_detect(V1, "\\|Genus 2="), genus2:=V1]
d[, genus2 := str_extract(genus2, "\\|Genus 2=[a-zA-Z]+")]
d[!is.na(genus2), has_gender2:=T]

d[str_detect(V1, "\\|Genus 3="), genus3:=V1]
d[, genus3 := str_extract(genus3, "\\|Genus 3=[a-zA-Z]+")]
d[!is.na(genus3), has_gender3:=T]




# find inflected forms (which can be identified
# via "Wortart", where inflected forms are tagged as
# "Deklinierte Form")
d[str_detect(V1, "\\{\\{Wortart\\|"), has_wortart := T]


#d[str_detect(V1, "Deklinierte_Form"), inflected := T]



# new df with relevant data -----------------------------------------------

d2 <- d[worttrenn2==T|aussprache1==T|title==T|has_lang==T|has_gender==T|has_gender2==T|has_gender3==T|has_wortart==T]


# export to save time later on
# saveRDS(d2, "relevant_parts.Rds")

# re-read exported df (uncomment if needed)
# d2 <- readRDS("relevant_parts.Rds")



# get relevant parts ------------------------------------------------------

d2[V1=="",continue:=F]
d2[str_detect(V1, "\\{\\{Wortart\\|"), continue:=T]
d2[str_detect(V1,"Aussprache"),continue:=T]
d2[str_detect(V1,"Worttrennung"),continue:=T]
d2[str_detect(V1,"title"), continue:=T]
d2[str_detect(V1, "\\|Genus"), continue:=T]

# avoid trailing of gender2 and gender3 to subsequent entries
d2[str_detect(V1, "\\|Genus="), has_gender2 := F]
d2[str_detect(V1, "\\|Genus="), has_gender3 := F]
d2[str_detect(V1, "\\|Genus 1="), has_gender3 := F]
d2[str_detect(V1, "\\|Genus 1="), has_gender2 := T]
# d2[is.na(has_gender3), has_gender3 := F]



# replace NAs by the nearest non-NA value
d2 <- na.locf(d2, na.rm = F)


# only use data belonging to the relevant categories
d3 <- d2[continue==T&str_detect(sprache2,"Deutsch")]

# find the relevant categories
d3[str_detect(V1,"Sprache\\|"), title_str:=V1]
d3[str_detect(shift(V1,1),"Worttrennung"), worttrennung_str:=V1]
d3[str_detect(V1,"IPA"), aussprache_IPA:=V1]
d3[str_detect(V1,"Sprache\\|"), aussprache_IPA:="F"]
d3[str_detect(V1,"Sprache\\|"), worttrennung_str:="F"]
d3[str_detect(V1,"title"), aussprache_IPA:="F"]
d3[str_detect(V1,"title"), worttrennung_str:="F"]
d3[str_detect(V1,"\\|Genus( 1)?="), genus := V1]
d3[str_detect(V1,"\\|Genus 2="), genus2 := V1]
d3[str_detect(V1,"\\|Genus 3="), genus3 := V1]
d3[str_detect(V1, "\\{\\{Wortart\\|"), flexion := V1]
#d3[str_detect(V1,"Deklinierte_Form"), flexion := "flektiert"]
#d3[grepl("span class=\"mw-headline\" id=\"(?!Deklinierte_Form)")]


# pull has_gender3 two forward so that it is TRUE
# in the same lines that contain gender1 and gender2
has_g3 <- which(d3$has_gender3==T)
d3[has_g3-2, has_gender3:=T]
d3[has_g3-1, has_gender3:=T]


# where has_gender2 or has_gender3=F, 
# insert non-{mfnx} character in the "gender" columns
d3[has_gender2==FALSE, genus2 := "NONE"]
d3[has_gender3==FALSE, genus3 := "NONE"]

# again, replace NAs by nearest non-NA value
d3 <- na.locf(d3, na.rm = F)

# find relevant instances
d4 <- unique(d3[aussprache_IPA!="F"&worttrennung_str!="F"][,.(title_str,worttrennung_str,aussprache_IPA,genus,genus2,genus3,sprache,sprache2,flexion)])

# remove brackets from title (lexeme) column
d4[,title:=str_remove(title_str,"\\<title\\>")]
d4[,title:=str_remove(title,"\\<\\/title\\>")]


# new df with relevant categories -----------------------------------------

# rename some of the columns
d5 <- d4[,.(title,worttrennung=worttrennung_str,aussprache_IPA,genus,genus2,genus3,subsection=sprache,sprache=sprache2,flexion)]

# get rid of bracket text in different columns
d5[,subsection:=str_remove(title,"\\<text xml\\:space\\=\"preserve\"\\>")]
d5[,title2:=trimws(str_extract(subsection, "[[:alnum:][:space:]\\.\\,\\-]+"))]
d5[,genus := str_remove(genus,"\\|Genus( 1)?=")]
d5[,genus2 := str_remove(genus2, "\\|Genus 2=")]
d5[,genus3 := str_remove(genus3, "\\|Genus 3=")]

d5[,ipa:=str_remove(aussprache_IPA,"\\:\\{\\{IPA\\}\\} \\{\\{Lautschrift\\|")]
d5[,ipa:=str_remove(ipa,"\\}\\}")]
d5 <-d5[!str_detect(ipa,"ref\\&gt")]
d5 <-d5[!str_detect(ipa,"Hilfe\\:IPA")]
d5 <-d5[!str_detect(ipa,"Internationales")]

d6 <- d5[,.(title2,genus,genus2,genus3,worttrennung,sprache,ipa,flexion)]

# set name of "title" column
setnames(d6, "title2", "lexeme")

# remove trailing : from "worttrennung" column
d6[, worttrennung := gsub("^:", "", worttrennung)]

# split up worttrennung into singular and plural
d6[, worttrennung_pl := worttrennung]
d6[, worttrennung_sg := worttrennung]

d6[, worttrennung_sg := gsub(",.*", "", worttrennung_sg)]
d6[, worttrennung_pl := gsub(" *\\{\\{Pl\\.\\}\\}", "", worttrennung_pl)]
d6[!grepl("\\{\\{Pl\\.\\}\\}", d6$worttrennung), worttrennung_pl := NA]

# remove inflections etc. from worttrennung_sg column
d6[, worttrennung_sg := gsub(",.*", "", worttrennung_sg)]


# add column with syllable length -----------------------------------------
d6[, silbenzahl := str_count(worttrennung_sg, "·")]
d6[, silbenzahl := silbenzahl + 1] # because syllable length is always
                                         # one more than there are separators.


# where there is no singular, use plural syllabification instead
d6[worttrennung_sg=="{{kSg.}}", worttrennung_pl := gsub(".*kSg\\.\\}\\}, ", "", worttrennung_pl)]
d6[worttrennung_sg=="{{kSg.}}", silbenzahl := str_count(worttrennung_pl, "·")]
d6[worttrennung_sg=="{{kSg.}}", silbenzahl := silbenzahl + 1] # warning message
                                                              # b/c some are NA



# add column indicating whether form is a noun ----------------------------
d6[, flexion2 := gsub(".*Wortart\\|", "", flexion)]
d6[, flexion2 := gsub("\\|.*", "", flexion2)]
d6[str_detect(flexion2, "Substantiv|onym|name|Name"), nomen := T]

# merge entries with multiple genders
d6[!is.na(genus2), genus := paste(genus, genus2, sep="")]
d6[!is.na(genus3), genus := paste(genus, genus3, sep="")]

# unified column for Worttrennung
d6[worttrennung_sg=="{{kSg.}}", worttrennung := worttrennung_pl]
d6[worttrennung_sg!="{{kSg.}}", worttrennung := worttrennung_sg]


# omit unnecessary columns & duplicates
d7 <- d6[,.(Lexem=lexeme,Genus=genus,Worttrennung=worttrennung,
            IPA=ipa, Wortart=flexion2, 
            Nomen=nomen)]


# only nouns
d8 <- d7[Nomen==T]
d8 <- d8[!duplicated(d8)]

# only appellatives
d9 <- d8[Wortart=="Substantiv"]

# in IPA column, take alternatives into account
d9[grepl("oder \\{\\{Lautschrift", d9$IPA) & !grepl("\\{\\{Pl\\.\\}\\}", d9$IPA), IPA := gsub(" \\'?\\'?oder\\'?\\'? \\{\\{Lautschrift|}}", "", IPA)]


# in IPA column, remove everything after comma
d9[, IPA := gsub(",.*", "", IPA)]

# in IPA column, find instances where there's something before {{IPA

d9[grep("\\{\\{IPA", d9$IPA), IPA := gsub(".*?\\{\\{Lautschrift(\\?| )?\\|", "", IPA)]


# detect words with multiple genders --------------------------------------

# check which ones can occur in multiple genders
duplicate_lexemes <- d9[duplicated(d9$Lexem)]$Lexem

# dataframe without gender
d_no_gender <- d9[,-2,with=F]
d_no_gender <- d_no_gender[!duplicated(d_no_gender)]
duplicate_lexemes_no_gender <- d_no_gender[duplicated(d_no_gender$Lexem)]$Lexem

# vector with gender-varying lexemes
gvl <- setdiff(duplicate_lexemes, duplicate_lexemes_no_gender)

# dataframe with gender-varying lexemes
d9a <- d9[Lexem %in% gvl]
d9b <- d9[!Lexem %in% gvl]

# add multiple genders to Genus column
for(i in 1:length(gvl)) {
  d9a[Lexem==gvl[i], Genus := paste0(d9a[Lexem==gvl[i]]$Genus, collapse="")]
}

# remove duplicates
d9a <- d9a[!duplicated(d9a)]

# reunite the two subdataframes
d10 <- rbind(d9a,d9b)


# remove recurring false hits
d10[, Worttrennung := gsub("\\{\\{kSg\\.\\}\\} \\{\\{Pl.\\}\\} ", "", Worttrennung)]
d10[, Worttrennung := gsub("\\[.*|\\#.*|\\:.*|[0-9].*|[,;]? \\{.*", "", Worttrennung)]
d10[, Worttrennung := gsub("<model>.*?</model>", "", Worttrennung)]
d10[, IPA := gsub("<comment>.*?</comment>", "", IPA)]
d10[, IPA := gsub("^\\:.*", "", IPA)]
d10[, IPA := gsub(" \\(.*", "", IPA)]
d10[, IPA := gsub("\\}\\}", "", IPA)]
d10[, IPA := gsub("\\{\\{.*", "", IPA)]
d10[, IPA := gsub("\\#.*", "", IPA)]


# clean gender column -----------------------------------------------------

# clean genus column
d10[, Genus := gsub("\\{\\{.*?\\}\\}", "", Genus)]
d10[, Genus := gsub("[^[:alpha:]]", "", Genus)]
d10[, Genus := gsub("Schwedisch.*", "", Genus)]
d10[, Genus := gsub("f+", "f", Genus)]
d10[, Genus := gsub("m+", "m", Genus)]
d10[, Genus := gsub("n+", "n", Genus)]
d10[, Genus := gsub("[NOE]", "", Genus)]


# helper function for sorting alphabetically
stri_alphabetic <- function(str) {
  x <- stri_sort(
    unlist(strsplit(str, ""))
  )
  x <- paste0(x, collapse = "")
  return(x)
}

# sort alphabetically
d10[, Genus := sapply(1:nrow(d10), function(i) stri_alphabetic(d10$Genus[i]))]

# remove non-{fmnx} characters
d10[, Genus := gsub("[^fmnx]", "", Genus)]

# remove x if one of the others is present as well
d10[, Genus := gsub(".*(?<=[mfn])x", "x", Genus, perl = T)]

# again, unique values
d10[, Genus := gsub("f+", "f", Genus)]
d10[, Genus := gsub("m+", "m", Genus)]
d10[, Genus := gsub("n+", "n", Genus)]

# remove rows without Lexem
d10 <- d10[Lexem!=""]


# find remaining duplicates
duplicate_lexemes <- d10[duplicated(d10$Lexem)]$Lexem



# remove duplicates automatically -----------------------------------------

d10a <- d10[!Lexem %in% duplicate_lexemes] 
ddup <- d10[Lexem %in% duplicate_lexemes]

# paste duplicate lexemes together
for(i in 1:length(duplicate_lexemes)) {
  x <- ddup[Lexem==duplicate_lexemes[i]]
  ddup[Lexem==duplicate_lexemes[i], Worttrennung := gsub("^\\||\\|$", "", paste(unique(x$Worttrennung), sep="", collapse="|"))]
  ddup[Lexem==duplicate_lexemes[i], IPA := gsub("^\\||\\|$", "", paste(unique(x$IPA), sep="", collapse="|"))]
  print(i)
}

# now find duplicate genders
ddup <- ddup[!duplicated(ddup)]
gvl <- ddup[duplicated(ddup$Lexem),]$Lexem

ddupa <- ddup[Lexem %in% gvl]
ddupb <- ddup[!Lexem %in% gvl]

# add multiple genders to Genus column
for(i in 1:length(gvl)) {
  ddupa[Lexem==gvl[i], Genus := paste0(ddupa[Lexem==gvl[i]]$Genus, collapse="")]
}

# sort alphabetically
ddupa[, Genus := sapply(1:nrow(ddupa), function(i) stri_alphabetic(ddupa$Genus[i]))]
ddupa[, Genus := gsub("f+", "f", Genus)]
ddupa[, Genus := gsub("m+", "m", Genus)]
ddupa[, Genus := gsub("n+", "n", Genus)]

# re-unite tables
ddup <- rbind(ddupa, ddupb)
ddup <- ddup[!duplicated(ddup)]


# re-unite tables with and without duplicates
d11 <- rbind(d10a, ddup)



# LEGACY CODE:
# in an early version, duplicates were corrected
# manually, but this would be way too time-consuming,
# especially when the project is extended to other
# languages and datasets.


# # merge with checked lexemes ----------------------------------------------
# 
# # read in checked lexemes
# d_check <- fread("check_duplicates_checked.csv")
# 
# # dataframe without duplicate lexemes
# d11a <- d10[!Lexem %in% duplicate_lexemes]
# 
# # only checked items
# d_check <- d_check[keep=="y"]
# 
# # remove keep column
# d_check[, keep := NULL]
# 
# # bind
# d11 <- rbind(d11a, d_check)

# replace " oder ..." in Worttrennung and IPA columns
d11[, IPA := trimws(IPA)]
d11[, Worttrennung := trimws(Worttrennung)]
d11[, Worttrennung := gsub(" .*", "", Worttrennung)]
d11[, IPA := gsub(" .*", "", IPA)]

# re-add syllable length column
d11[, Silbenzahl := str_count(Worttrennung, "·|-")]
d11[, Silbenzahl := Silbenzahl + 1]




# remove columns not needed any more
d11[, Nomen := NULL]
d11[, Wortart := NULL]

# replace {{kSg}} in IPA column
d11[, IPA := gsub("\\{\\{.*?\\}\\}", "", IPA)]
d11[, Worttrennung := gsub("\\{\\{.*?\\}\\}", "", Worttrennung)]


# sort alphabetically
setkey(d11, "Lexem")


# set "Silbenzahl" to NA when there is nothing
# in "Worttrennung"
d11[Worttrennung=="", Silbenzahl := NA]

# if word begins with alpha, beta, gamma,
# add 1 to syllable length (because all three
# are two-syllable words)
d11[grep("^[αβγ]", Worttrennung), Silbenzahl := Silbenzahl + 1]




# export
# write_csv(d11, "wikinouns_DE.csv")

