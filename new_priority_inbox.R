library(tm)
library(ggplot2)
library(plyr)

setwd("F:\\ML\\ML_for_Hackers-master\\04-Ranking")
data.path <- "F:\\ML\\ML_for_Hackers-master\\03-Classification\\data\\"
easyham.path <- paste(data.path, "easy_ham", sep = "")

msg.full <- function(path) {
  con <- file(path, open = "rt", encoding = "native.enc")
  msg <- readLines(con)
  close(con)
  msg <- iconv(msg,"WINDOWS-1252","UTF-8")
  return(msg)
}

get.from <- function(msg.vec) {
  from <- msg.vec[grepl("From: ", msg.vec)]
  from <- strsplit(from, '[":<> ]')[[1]]
  from <- from[which(from != "" & from != " ")]
  return(from[grepl("@", from)][1])
}

get.msg <- function(msg.vec) {
  msg <- msg.vec[seq(which(msg.vec == "")[1] + 1, length(msg.vec), 1)]
  return(paste(msg, collapse = "\n"))
}

get.subject <- function(msg.vec) {
  subj <- msg.vec[grepl("Subject: ", msg.vec)]
  if(length(subj) > 0) {
    return(strsplit(subj, "Subject: ")[[1]][2])
  }
  else {
    return("")
  }
}

get.date <- function(msg.vec) {
  date.grep <- grepl("^Date: ", msg.vec)
  date.grepl <- which(date.grep == TRUE)
  date <- msg.vec[date.grepl[1]]
  date <- strsplit(date, "\\+|\\-|: ")[[1]][2]
  date <- gsub("^\\s+|\\s+$", "", date)
  return(strtrim(date, 25))
}

parse.email <- function(path) {
  full.msg <- msg.full(path)
  date <- get.date(full.msg)
  from <- get.from(full.msg)
  subj <- get.subject(full.msg)
  msg <- get.msg(full.msg)
  return(c(date, from, subj, msg, path))
}

easyham.docs <- dir(easyham.path)
easyham.docs <- easyham.docs[which(easyham.docs != "cmds")]
easyham.parse <- lapply(easyham.docs, function(p) 
  parse.email(paste(easyham.path, p, sep = "/")))

epharse.matrix <- do.call(rbind, easyham.parse)
allparse.df <- data.frame(epharse.matrix, stringsAsFactors = FALSE)
names(allparse.df) <- c("Date", "From.EMail", "Subject", "Message", "Path")

date.converter <- function(dates, pattern1, pattern2) {
  pattern1.convert <- strptime(dates, pattern1)
  pattern2.convert <- strptime(dates, pattern2)
  pattern1.convert[is.na(pattern1.convert)] <- pattern2.convert[is.na(pattern1.convert)]
  return(pattern1.convert)
}

pattern1 <- "%a, %d %b %Y %H:%M:%S"
pattern2 <- "%d %b %Y %H:%M:%S"

Sys.setlocale("LC_TIME", "C")

allparse.df$Date <- date.converter(allparse.df$Date, pattern1, pattern2)

allparse.df$Subject <- tolower(allparse.df$Subject)
allparse.df$From.EMail <- tolower(allparse.df$From.EMail)

priority.df <- allparse.df[with(allparse.df, order(Date)), ]

priority.train <- priority.df[1:(round(nrow(priority.df) / 2)), ]

priority.train$Date <- as.POSIXct(priority.train$Date)
from.weight <- ddply(priority.train, .(From.EMail), summarise,  Freq = length(Subject))

from.weight <- transform(from.weight, Weight = log(Freq + 1))

find.threads <- function(email.df) {
  response.threads <- strsplit(email.df$Subject, "re: ")
  is.thread <- sapply(response.threads, function(subj) 
    ifelse(subj[1] == "", TRUE, FALSE))
  threads <- response.threads[is.thread]
  senders <- email.df$From.EMail[is.thread]
  threads <- sapply(threads, function(t) 
    paste(t[2:length(t)], collapse = "re: "))
  return(cbind(senders, threads))
}

threads.matrix <- find.threads(priority.train)

email.thread <- function(threads.matrix) {
  senders <- threads.matrix[, 1]
  senders.freq <- table(senders)
  senders.matrix <- cbind(names(senders.freq), senders.freq, 
                          log(senders.freq + 1))
  senders.df <- data.frame(senders.matrix, stringsAsFactors = FALSE)
  row.names(senders.df) <- 1:nrow(senders.df)
  names(senders.df) <- c("From.EMail", "Freq", "Weight")
  senders.df$Freq <- as.numeric(senders.df$Freq)
  senders.df$Weight <- as.numeric(senders.df$Weight)
  return(senders.df)
}

senders.df <- email.thread(threads.matrix)

get.threads <- function(threads.matrix, email.df) {
  threads <- unique(threads.matrix[, 2])
  threads.counts <- lapply(threads, function(t) 
    thread.counts(t, email.df))
  threads.matrix <- do.call(rbind, threads.counts)
  return(cbind(threads, threads.matrix))
}

thread.counts <- function(thread, email.df) {
  thread.times <- email.df$Date[which(email.df$Subject == thread | 
                                        email.df$Subject == paste(
                                          "re: ", thread, sep = ""))]
  freq <- length(thread.times)
  min.time <- min(thread.times)
  max.time <- max(thread.times)
  time.span <- as.numeric(difftime(max.time, min.time, units = "secs"))
  if(freq < 2) {
    return(c(NA, NA, NA))
  }
  else {
    trans.weight <- freq / time.span
    log.trans.weight <- 10 + log(trans.weight, base = 10)
    return(c(freq, time.span, log.trans.weight))
  }
}

thread.weights <- get.threads(threads.matrix, priority.train)
thread.weights <- data.frame(thread.weights, stringsAsFactors = FALSE)
names(thread.weights) <- c("Thread", "Freq", "Response", "Weight")
thread.weights$Freq <- as.numeric(thread.weights$Freq)
thread.weights <- subset(thread.weights, is.na(thread.weights$Freq) == FALSE)

term.counts <- function(term.vec, control) {
  vec.corpus <- Corpus(VectorSource(term.vec))
  vec.tdm <- TermDocumentMatrix(vec.corpus, control = control)
  return(rowSums(as.matrix(vec.tdm)))
}

thread.terms <- term.counts(thread.weights$Thread, control = 
                              list(stopwords = stopwords()))
thread.terms <- names(thread.terms)

term.weights <- sapply(thread.terms, function(t) 
  mean(as.numeric(thread.weights$Weight[grepl(t, thread.weights$Thread, fixed = TRUE)])))
term.weights <- data.frame(list(Term = names(term.weights), Weight = 
                                 term.weights, stringsAsFactors = FALSE,
                               row.names = 1:length(term.weights)))

msg.terms <- term.counts(priority.train$Message, 
                         control = list(stopwords = stopwords(), 
                                        removePunctuation = TRUE, 
                                        removeNumbers = TRUE))

msg.weights <- data.frame(list(Term = names(msg.terms), 
                               Weight = log(msg.terms, base = 10)), 
                          stringsAsFactors = FALSE, row.names = 1:length(msg.terms))

msg.weights <- subset(msg.weights, Weight > 0)

get.weights <- function(serch.term, weight.df, term = TRUE) {
  if(length(serch.term) > 0) {
    if(term) {
      term.match <- match(names(serch.term), weight.df$Term)
    }
    else {
      term.match <- match(serch.term, weight.df$Thread)
    }
    match.weights <- weight.df$Weight[which(!is.na(term.match))]
    if(length(match.weights) > 1) {
      return(1)
    }
    else {
      return(mean(as.numeric(match.weights)))
    }
  }
  else {
    return(1)
  }
}

rank.message <- function(path) {
  msg <- parse.email(path)
  
  from <- ifelse(length(which(from.weight$From.EMail == msg[2])) > 0, 
                 from.weight$Weight[which(from.weight$From.EMail == msg[2])], 1)
  
  thread.from <- ifelse(length(which(senders.df$From.EMail == msg[2])) > 0, 
                        senders.df$Weight[which(senders.df$From.EMail == msg[2])], 1)
  
  subj <- strsplit(tolower(msg[3]), "re: ")
  is.thread <- ifelse(subj[[1]][1] == "", TRUE, FALSE)
  if(is.thread) {
    activity <- get.weights(subj[[1]][2], thread.weights, term = FALSE)
  }
  else {
    activity <- 1
  }
  
  thread.terms <- term.counts(msg[3], control = list(stopwords = stopwords()))
  thread.terms.weights <- get.weights(thread.terms, term.weights)
  
  msg.terms <- term.counts(msg[4], control = list(stopwords = stopwords(), 
                                                  removePunctuation = TRUE, 
                                                  removeNumbers = TRUE))
  
  msg.weights <- get.weights(msg.terms, msg.weights)
  
  rank <- prod(from, thread.from, activity, thread.terms.weights, 
               msg.weights, na.rm = TRUE)
  
  return(c(msg[1], msg[2], msg[3], rank))
}

train.paths <- priority.df$Path[1:(round(nrow(priority.df) / 2))]

test.paths <- priority.df$Path[((round(nrow(priority.df) / 2)) + 1):
                                 nrow(priority.df)]

train.ranks <- lapply(train.paths, rank.message)

train.ranks.matrix <- do.call(rbind, train.ranks)

train.ranks.matrix <- cbind(train.paths, train.ranks.matrix, "RRAINING")

train.ranks.df <- data.frame(train.ranks.matrix, stringsAsFactors = FALSE)

names(train.ranks.df) <- c("Message", "Date", "From", "Subj", "Rank", "Type")

train.ranks.df$Rank <- as.numeric(train.ranks.df$Rank)

priority.threshold <- median(train.ranks.df$Rank)

train.ranks.df$Priority <- ifelse(train.ranks.df$Rank >= priority.threshold, 1, 0)
