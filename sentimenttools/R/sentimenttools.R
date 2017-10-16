# just put this in for server hash test

#Collection of Shiny Tools for Sentiment Analysis

YearMatcher <- function(year_string) {
  #' YearMatcher
  #' 
  #' YearMatcher is just a convenience function to parse Shiny string input to a format appropriate for comparison
  #' @param year_string, a string indicating a year e.g. "2016"
  #' @return an integer 
  #' @export
  #' @details compare this to `as.POSIXlt[["year"]]` numbers. The result is (year_string - 1900) cause that's how POSIXlt rolls. 
  #' see also here \url{https://stat.ethz.ch/R-manual/R-devel/library/base/html/DateTimeClasses.html}
  year = as.integer(year_string) - 1900
  as.integer(year)
}


#
## Serverside
#
# Module file_in

csvFile <- function(input, output, session, stringsAsFactors, file_name="file") {
  #' csvFile
  #' 
  #' csvFile processes the input from csvFileInput and reads it into a \code{data.frame}. Adapted from \link{https://shiny.rstudio.com/articles/modules.html}
  #' @param stringAsFactors is input for \code{read.csv}
  #' @param file_name is the name of the file; needs to match \code{file_name} fed into \code{csvFileInput}, the UI function.
  #' @export
  #' @return dataframe with contents of the CSV file
  # The selected file, if any
  userFile <- reactive({
    # If no file is selected, don't do anything: https://shiny.rstudio.com/reference/shiny/latest/validate.html
    validate(need(input[[file_name]], message = FALSE))
    input[[file_name]]
  })
  # The user's data, parsed into a data frame
  dataframe <- reactive({
    read.csv(userFile()[['datapath']], 
             # note that POSIXct wants a space where our time has a "T". However, this works here as it still extracts the day
             colClasses = c("created_at" = "POSIXct", "rel_deliver" = 'logical',"rel_hebtoyou" = 'logical',"rel_shipt" = 'logical',"rel_instacart"= "logical"),
             header = TRUE)
  })
  # We can run observers in here if we want to
  # Docs for observer are here: \link{https://shiny.rstudio.com/reference/shiny/latest/observe.html}
  # Says: "observe(x): x	An expression (quoted or unquoted). Any return value will be ignored".
  # This will only execute once we have a \code{userFile} which is a reactive depending on an input being present.
  observe({
    msg <- sprintf("File %s was uploaded from path %s, size is %s", userFile()[['name']], userFile()[['datapath']], userFile()[['size']])
    cat(msg, "\n")
  })
  # Return the reactive that yields the data frame
  #cat("Return of csvFile is ", class(dataframe))
  return(dataframe)
}

wordfinder <- function(input_text, search_term, lower_case = TRUE)
  {
  #' Wordfinder
  #' 
  #' Wordfinder takes a string as input_text and returns TRUE if search_term is contained. 
  #' @param input_text: string
  #' @param search_term: string that will be checked for existence in input_text
  #' @param lower_case: as of now, argument is not used; all text is lowercased
  #' @return TRUE / FALSE
  #' @export
  #' @examples wordfinder("This is a sentence", "sentence")
  #' NB THIS WILL MESS WITH EMOTICONS, NO?
  print ("THIS IS AN OUTDATED VERSION YOU SURE YOU WANT TO USE THIS?")
  tokens <- strsplit(tolower(input_text), "\\W")
  result <- is.element(search_term, unlist(tokens))
  return (result)
  }

  MeanSentimentPerDay <- function(data_frame, sentiment_column, date_column, days_data_frame, origin_date)
   {
    #' MeanSentimentPerDay 
    #'
    #' MeanSentimentPerDay computes mean sentiment score by day.
    #' @param data_frame: the data.frame to be processed, where
    #' @param sentiment_column: a column of data_frame that contains sentiment scores
    #' @param date_column: a column of data_frame that contains the date of the object
    #' @param days_data_frame: a data.frame of days to be matched to the days in date_column
    #' @param origin_date: beginning date, CHECK THE FORMAT ON THIS ONE
    #' @export
    #' @return A time series  (\code{"xts" "zoo"}, created by \code{as.xts}) of mean sentiment scores, format: index=date, mean_sentiment. Note that it will return an empty \code{xts} object if the input has zero rows. 
    #' 
    print ("Run mean Sent per Day 1xy")    
    # here we need to control for empty df ("no rows to aggregate")
    if (nrow(data_frame) == 0) 
      {return (as.xts(data.frame()))}
    else {
      print (nrow(data_frame))
      sentimentbyday <- aggregate(
        list("mean_sentiment"=data_frame[[sentiment_column]]), list("date"=as.numeric(data_frame[[date_column]])), FUN=mean)
      #convert days to POSIX; we could make this an additional col and save us the naming business above
      print ("Run mean Sent per Day 2")
      sentimentbyday['date'] <- 
        as.numeric(sentimentbyday[['date']]) %>% as.POSIXct(origin = origin_date) %>% as.Date()
      daysdataframe <- setNames(days_data_frame, 'date')
      #merge the two dfs after renaming so 'by' works
      print ("Run mean Sent per Day 3")
      sentimentbyday <- merge(sentimentbyday, daysdataframe, by.y = 'date', all=T)
      #we fill the NA days with the overall mean, set rownames to date, and kill the date column, convert to TS
      sentimentbyday[is.na(sentimentbyday[['mean_sentiment']])==T,][['mean_sentiment']] <- mean(data_frame[[sentiment_column]])
      row.names(sentimentbyday) <- sentimentbyday[[1]]
      sentimentbyday[1] <- NULL
      sentimentbyday.xts <- as.xts(sentimentbyday)
      print ("Run mean Sent per Day 4")
      sentimentbyday.xts <- sentimentbyday.xts[1:nrow(days_data_frame)]
      print ("Finish mean Sent per Day")
      print (class(sentimentbyday.xts))
      return (sentimentbyday.xts )
    }
    }

  
  deliveryConverter <- function(input_data_frame) {
    #' deliveryConverter
    #' 
    #' deliveryConverter takes a data_frame with sentiment scores and converts it to a time series object (\code{xts / zoo}) to feed into a dygraph.
    #' It's based on Lars' initial process chain. 
    #' 
    #' processes the input from csvFileInput and reads it into a \code{data.frame}. Adapted from \link{https://shiny.rstudio.com/articles/modules.html}
    #' @param input_data_frame is a \code{data.frame} that includes at least sentiment scores and relevance columns for individual delivery themes. E.g. read in with \code{csvFile()} in this package. 
    #' @details the exception is modeled after \link{https://shiny.rstudio.com/gallery/file-upload.html} FIX THE CAPS IN THE TITLE
    #' @export
    #' @return time series object \code{xts} formatted, containing sentiment scores by week. 
    print ("running delivery_converter")
    #if (is.null(input_data_frame)) {return(NULL)}
    print ("Runing delivery_converter")
    delivery_temp <- isolate(input_data_frame)
    #note that we added the POSIX read to CSV input
    #note that round returns an POSIXlt object, \link{https://stat.ethz.ch/R-manual/R-devel/library/base/html/round.POSIXt.html}
    delivery_temp[['date1']] <- round(delivery_temp[['created_at']], "days")
    #the code above is equivalent but we are keeping it as this is flimsy things
    #delivery_temp$date1 <- delivery_temp$created_at  %>% as.POSIXct() %>% round(units="days")
    delivery_temp$created_at <- NULL
    delivery_temp$id_str = as.character(delivery_temp$id_str)
    # replace retrieve id with first part of id_str for FB posts; revert to factor
    delivery_temp['retrieve_id'] = as.character(delivery_temp[['retrieve_id']])
    delivery_temp[delivery_temp[['source']] == 'f', ]['retrieve_id']  = make.retrieve.id(delivery_temp[delivery_temp$source == 'f', "id_str"])
    delivery_temp['retrieve_id'] = as.factor(delivery_temp[['retrieve_id']])
    #make relevance columns boolish (is this necessary?)
    rel_columns <- c("rel_deliver", "rel_hebtoyou", "rel_shipt", "rel_instacart")
    delivery_temp[ ,rel_columns] <- apply(delivery_temp[ ,rel_columns], MARGIN = 2, FUN = function(x) {as.logical(x)})
    #find curbside, as not in original data set (added '\\W' in revision)
    delivery_temp$rel_curbside <- grepl("\\Wcurbside", delivery_temp$text, ignore.case = T)
    #bool for any field
    delivery_temp$rel_any <- delivery_temp$rel_deliver | delivery_temp$rel_curbside | delivery_temp$rel_hebtoyou | delivery_temp$rel_shipt | delivery_temp$rel_instacart
    #inverse
    delivery_temp$rel_none <- ifelse(delivery_temp$rel_any, FALSE, TRUE)
    #make final dataframe called df
    # note that this has html tags in its text, other than initial file. Why that
    # we took out this one: ,"text.b"
    cols_to_keep = c("text","word_count","date1","source","retrieve_id","rel_deliver",
                     "rel_curbside","rel_hebtoyou", "rel_shipt","rel_instacart","rel_any",
                     "rel_none", "sentiment_score","pos_words","neg_words")
    delivery_temp = delivery_temp[ , cols_to_keep] 
    # set some working variables fo date
    mindate <- min(delivery_temp$date1) %>% as.Date() # %>% round(units = c("days"))
    maxdate <- max(delivery_temp$date1) %>% as.Date() # %>% round(units = c("days"))
    day.seq <-  seq(mindate, maxdate, by='day') %>% as.data.frame()
    colnames(day.seq) <- 'date1'
    origin = '1970-01-01'
    # subset dat to keyword-relevant narrow sets
    all <- delivery_temp[, c('date1', 'sentiment_score')]
    #tweets not relevant to any key word, text only
    els <- delivery_temp[delivery_temp$rel_any==FALSE, 'text'] %>% as.data.frame
    names(els) <- 'text'
    # data frames relevant to each keyword
    del <- delivery_temp[delivery_temp$rel_deliver==TRUE, c('date1', 'sentiment_score', 'text')]
    cur <- delivery_temp[delivery_temp$rel_curbside==TRUE, c('date1', 'sentiment_score', 'text')]
    hty <- delivery_temp[delivery_temp$rel_hebtoyou==TRUE, c('date1', 'sentiment_score', 'text')]
    sht <- delivery_temp[delivery_temp$rel_shipt==TRUE, c('date1', 'sentiment_score', 'text')]
    ins <- delivery_temp[delivery_temp$rel_instacart==TRUE, c('date1', 'sentiment_score', 'text')]
    # convert for the filter thing to work
    delivery_temp['date1'] = as.POSIXct(delivery_temp[['date1']])
    delivery_temp <- delivery_temp %>% filter(date1 > '2014-04-01')
    #delivery_temp['date1'] = as.POSIXlt(delivery_temp[['date1']])
    #mean sentiment score by day, complete dataset
    print ("Running mean sent per day")
    all.day <- MeanSentimentPerDay(all, "sentiment_score", "date1", day.seq, origin)
    names(all.day) <- 'all'
    #same game, but only delivery relevant 
    del.day <- MeanSentimentPerDay(del, "sentiment_score", "date1", day.seq, origin)
    names(del.day) <- 'deliver-X'
    #same game, hebtoyou
    hty.day <- MeanSentimentPerDay(hty, "sentiment_score", "date1", day.seq, origin)
    names(hty.day) <- 'hebtoyou'
    cur.day <- MeanSentimentPerDay(cur, "sentiment_score", "date1", day.seq, origin)
    names(cur.day) <- 'curbside'
    #same game, shipt
    sht.day <- MeanSentimentPerDay(sht, "sentiment_score", "date1", day.seq, origin)
    names(sht.day) <- 'shipt'
    #same game, instacart
    ins.day <- MeanSentimentPerDay(ins, "sentiment_score", "date1", day.seq, origin)
    names(ins.day) <- 'instacart'
    # now, aggregate weekly and monthly alternatives
    all.weekly <- all.day %>% to.weekly(OHLC=F)
    #print(all.weekly)
    del.weekly <- del.day %>% to.weekly(OHLC=F)
    hty.weekly <- hty.day %>% to.weekly(OHLC=F)
    cur.weekly <- cur.day %>% to.weekly(OHLC=F)
    sht.weekly <- sht.day %>% to.weekly(OHLC=F)
    ins.weekly <- ins.day %>% to.weekly(OHLC=F)
    # Bind all xts objects of a type into one and name the series accordingly.
    # this is what were working with for the plot
    sentiment.weekly <- merge.xts(
      all.weekly,
      del.weekly,
      hty.weekly,
      cur.weekly,
      sht.weekly,
      ins.weekly
    )
    names(sentiment.weekly) <- c('all', 'deliverx', 'hebtoyou', 'curbside', 'shipt', 'instacart')
    print ("done gettting delivery data, writing to sentiment.weekly.csv")
    write.zoo(sentiment.weekly, "sentiment.weekly.csv")
    sentiment.weekly
  }  
 
  
  TimeseriesConverter <- function(input_data_frame, col_name) {
    #' The TimeseriesConverter takes an input_data_frame and converts it into a \code{xts} frame to be plotted in a dygraph. 
    #' 
    #' It is a generalized version of the deliveryConverter, but e.g. controls for zero length input. 
    #' 
    #' @param input_data_frame is a \code{data.frame} that includes at least sentiment scores and relevance columns for individual delivery themes. E.g. read in with \code{csvFile()} in this package. 
    #' @param col_name is a string giving a title to the result-column of the output time series. This will e.g. show up as label in a dygraph. 
    #' @export
    #' @return time series object \code{xts} formatted, containing sentiment scores by week. 
    print ("running TimeseriesConverter")
    if (is.null(input_data_frame)) {print ("Warning: input to TimeseriesConverter is length 0") 
      return(NULL)}
    input_data_frame[['date1']] <- as.POSIXct(input_data_frame[['created_at']])
    input_data_frame[['date1']] <- round(input_data_frame[["date1"]], "days")  
    input_data_frame$created_at <- NULL
    input_data_frame[['id_str']] = as.character(input_data_frame[['id_str']])
    # replace retrieve id with first part of id_str for FB posts; revert to factor
    input_data_frame['retrieve_id'] = as.character(input_data_frame[['retrieve_id']])
    input_data_frame[input_data_frame[['source']] == 'f', ]['retrieve_id']  = make.retrieve.id(input_data_frame[input_data_frame$source == 'f', "id_str"])
    input_data_frame['retrieve_id'] = as.factor(input_data_frame[['retrieve_id']])
    #make relevance columns boolish - we don't really need this most times but let's keep it
    rel_columns <- c("rel_deliver", "rel_hebtoyou", "rel_shipt", "rel_instacart")
    input_data_frame[ ,rel_columns] <- apply(input_data_frame[ ,rel_columns], MARGIN = 2, FUN = function(x) {as.logical(x)})
    #find curbside, as not in original data set (added '\\W' in revision)
    input_data_frame$rel_curbside <- grepl("\\Wcurbside", input_data_frame$text, ignore.case = T)
    #bool for any field
    input_data_frame$rel_any <- input_data_frame$rel_deliver | input_data_frame$rel_curbside | input_data_frame$rel_hebtoyou | input_data_frame$rel_shipt | input_data_frame$rel_instacart
    #inverse
    input_data_frame$rel_none <- ifelse(input_data_frame$rel_any, FALSE, TRUE)
    #make final dataframe called df
    # note that this has html tags in its text, other than initial file. Why that
    # we took out this one: ,"text.b"
    cols_to_keep = c("text","word_count","date1","source","retrieve_id","rel_deliver",
                     "rel_curbside","rel_hebtoyou", "rel_shipt","rel_instacart","rel_any",
                     "rel_none", "sentiment_score","pos_words","neg_words")
    input_data_frame = input_data_frame[ , cols_to_keep] 
    # set some working variables fo date
    mindate <- min(input_data_frame$date1) %>% as.Date() # %>% round(units = c("days"))
    maxdate <- max(input_data_frame$date1) %>% as.Date() # %>% round(units = c("days"))
    day.seq <-  seq(mindate, maxdate, by='day') %>% as.data.frame()
    colnames(day.seq) <- 'date1'
    origin = '1970-01-01'
    # subset dat to keyword-relevant narrow sets
    all <- input_data_frame[, c('date1', 'sentiment_score')]
    #tweets not relevant to any key word, text only
    # what do we need this for?
    els <- input_data_frame[input_data_frame$rel_any==FALSE, 'text'] %>% as.data.frame
    names(els) <- 'text'
    # data frames relevant to each keyword
    # convert for the filter thing to work
    input_data_frame['date1'] = as.POSIXct(input_data_frame[['date1']])
    #do we want this all the time and everywhere???
    input_data_frame <- input_data_frame %>% filter(date1 > '2014-04-01')
    #mean sentiment score by day, complete dataset
    print ("Running mean sent per day")
    all.day <- MeanSentimentPerDay(all, "sentiment_score", "date1", day.seq, origin)
    names(all.day) <- col_name
    print ("here be all day")
    print(all.day[1,])
    # now, aggregate weekly and monthly alternatives
    sentiment.weekly <- all.day %>% to.weekly(OHLC=F)
    print ("here be all wewek")
    print(sentiment.weekly[1,])
    print ("done getting timeseries data, writing to sentiment_weekly_custom.csv")
    write.zoo(sentiment.weekly, "sentiment_weekly_custom.csv")
    sentiment.weekly
  }  
  
  
  GlobToRegex = function(input_string)
  {
    #' GlobToRegex
    #' 
    #' GlobToRegex takes a string as input_text and converts it to a regex to be used by \code{grep}.
    #' @param input_string: string, optionally containing wildcards such as `*` or `?`
    #' @return string to be used as input into \code{grep}
    #' @export
    #' @examples `run*` --> `run.*`; `r?n` --> `r.{1}n`
    #' @details functionality follows \code{glob} as outlined here: \url{https://en.wikipedia.org/wiki/Glob_%28programming%29}; equivalence pairs are stored in the list \code{replacements}.
    
    replacements = list("\\*" = ".*", "\\?" = "\\\\w{1}")
    for (rep in names(replacements)){
      input_string = gsub(rep, replacements[rep], input_string)
    }
    #cat("globto returns", paste("(^|\\W)", input_string, "($|\\W)", sep = ""), "\n")
    regex = paste("(^|\\W)", input_string, "($|\\W)", sep = "")
    }
  
  WordFinder_regex <- function(input_text, search_term, regex_search = FALSE, lower_case = TRUE)
  {
    #' WordFinder_regex
    #' 
    #' WordFinder_regex is built on top of Wordfinder. It takes a string as input_text and returns TRUE if search_term is contained. 
    #' @param input_text: string
    #' @param search_term: string that will be checked for existence in input_text
    #' @param lower_case: if `TRUE`, all words in input_text are lower cased before the search
    #' @param regex_search: if `TRUE`, uses a limited version regular expression pattern matching: the `*` wildcard is used to match 0 to infinite characters; `?` matches a single character.  
    #' @return TRUE / FALSE
    #' @export
    #' @examples wordfinder("This is a sentence", "sentence")
    #' @details see GlobToRegex for details on regex_search
    #' NB THIS WILL MESS WITH EMOTICONS, NO?
    #' NB this get real messed up if user inputs whitespace; we could strip it but do we want to?
    if (lower_case == TRUE) {
      input_text <- tolower(input_text)}
    # check here for wildcard etc that can be used: https://stat.ethz.ch/R-manual/R-devel/library/utils/html/glob2rx.html
    if (regex_search == TRUE) {
      search_term = GlobToRegex(search_term)
      result = grepl(search_term, input_text)
    }
    else {
      tokens <- strsplit(input_text, "\\W")
      result <- is.element(search_term, unlist(tokens))}
    return (result)
  }
  
     
#
## UI side
#

#Module file_in
  
  # Module UI function
  csvFileInput <- function(name_space, file_name="file", label = "Input file") {
    #' csvFileInput
    #'
    #' UI function that lets user input a CSV to be read into \code{data.frame} by server function csvFile. Adapted from \link{https://shiny.rstudio.com/articles/modules.html}
    #' @param name_space The name space within this is located
    #' @param file_name Variable, to be called as \code{input[[file_name]]}
    #' @param label Is the label displayed to user
    #' @export
    #' @return a \code{input$file_name}: from doc: [Returned] dataframe contains one row for each selected file, and the following columns [...] datapath, name, size
    # Create a namespace function using the provided id
    ns <- NS(name_space)
    # doc here: https://shiny.rstudio.com/reference/shiny/latest/fileInput.html
    fileInput(ns(file_name), label)
  }

  
  
## MODULE SEARCH
  
  CheckSearchInput <- function(input, output, session){
    #' CheckSearchInput
    #' 
    #' CheckSearchInput figures out if there is any input in `search_input` at the moment. 
    #' Don't forget to call from appropriate namespace
    #' @return TRUE / FALSE
    #' @export
    cat ("CheckSearch: search_term in module", session$ns("name"), "is == ''", identical(input$search_term, ""))
    identical(input$search_term, "")
  }
  
  
  SearchTermInput <- function(name_space, 
                              label = "Search box", button_label = "Search", help_text = NULL) {
    #' SearchTermInput
    #' 
    #' SearchTermInput is the UI-side function of the SearchTerm module, cf \link{SearchTerm}
    #' It creates a tagList to be fed into any \code{sidebarPanel} 
    #' Taglist contains a box for text input and an action button. 
    #' @param name_space the associated namespace, what the docs call \code{id}
    #' @param label a string to be supplied to \code{label = } of \code{textInput}
    #' @param button_label a string to be supplied to \code{label = } of \code{ActionButton}
    #' @param help_text a string to appear as help text (mouse over) on \code{textInput}. Defaults to \code{NULL}
    #' @export
    #' @return \code{input[['search_term']]} from \code{textInput} and \code{input[['execute_search']]} from \code{ActionButton}
    #' @details The \code{div} part is from SO, \url{https://stackoverflow.com/questions/16449252/tooltip-on-shiny-r#16451601}
    ns <- NS(name_space)
    tagList(
      # the `div` part is from SO, thank you very much
      tags$div(title = help_text, 
               textInput(ns("search_term"), label = label)),
      actionButton(ns("execute_search"), label = button_label)
    )
  }
  
  SearchTerm <- function(input, output, session, remove_whitespace = FALSE) {
    #' SearchTerm
    #' 
    #' SearchTerm is the server-side function of the SearchTerm module, cf \link{SearchTermInput}
    #' It takes the input from a search box after "Search" action button has been clicked. 
    #' @param input string from a \code{textInput} search box as \code{input[['search_term']]}
    #' @param input event from an \code{ActionButton} button as \code{execute_search} 
    #' @param remove_whitespace if set to \code{TRUE}, remove leading and trailing whitespace
    #' @export
    #' @return reactive expression object created by \code{eventReactive}
    #' @details \code{eventReactive} docs: \url{http://shiny.rstudio.com/reference/shiny/latest/observeEvent.html}
    searchterm <- 
      eventReactive(input$execute_search, 
                    {
                      cat ("\nreceived execute_search\n")
                      search_term <- isolate(input$search_term)
                      if (isTRUE(remove_whitespace)) {
                        search_term <- trimws(search_term)
                      }
                      cat ("returning search_term:", search_term, "in module", session$ns("name"), "\n")
                      return (search_term)
                    }
      )
    return(searchterm)
  }

  
  TextParser <- function (input_text, 
                          lower_case = TRUE, remove_stopwords = FALSE, remove_numbers = FALSE){
    #'TextParser
    #'
    #'The TextParser takes a string of words and tokenizes it into a list
    #'@param input_text a string
    #'@param lower_case if set to \code{TRUE}, will lowercase the \code{input_text}
    #'@param remove_stopwords if set to \code{TRUE}, will remove all stopwords (currently the ones from pckg tokenizers)
    #'@param remove_numbers CURRENTLY NOT IMPLEMENTED
    #'@return a list with one entry, a vector of tokens
    #'@details note that this deletes all punctuation by default
    if (isTRUE(remove_stopwords)) {remove_stopwords <- c(tokenizers::stopwords('en'))}
    text_tokenized <- tokenizers::tokenize_words(input_text, lowercase = lower_case, stopwords = remove_stopwords)
    if (!identical(length(text_tokenized), as.integer(1))) {print ("TextParser returned more/less than 1 item")}
    text_tokenized
  }
  
  WordCounter <- function(list_of_tokens){
    #' WordCounter
    #' 
    #' The WordCounter counts how often each token in list_of_tokens occurs.
    #' @param list_of_tokens a list with one entry containing a tokenized string (vector)
    #' @return a vector mapping tokens to counts
    #' @export
    #' @details as of now, the counts are only accurate if there is only one item in the \code{list_of_tokens}. 
    #' It will work well with output from \link{TextParser}.
    for (tokenized_string in list_of_tokens){
      counts = sapply(unique(tokenized_string), function(x) {sum(tokenized_string == x)})
    }
    # this returns a vector looking like this:  Named int [1:3] 1 2 2
    #- attr(*, "names")= chr [1:3] "I" "am" "dumb"
    return(counts)
  }
  
  DictMaker <- function(text_vector, 
                        threshold = 10, to_lower = FALSE, remove_punctuation = FALSE,  
                        remove_numbers = FALSE, remove_stopwords = FALSE, stemming = FALSE){
  #' DictMaker
  #' 
  #' The DictMaker calls on tm's \code{termFreq()} to create a matrix of word counts in text_vector.
  #' @param text_vector a flat vector of string, not tokenized; if directly out of \code{data.frame}, \code{paste} and \code{collapse} first.   
  #' @param threshold items with a token count lower than threshold will not be returned
  #' @param to_lower input to termFreq's \code{tolower} parameter; TRUE or FALSE for lower casing
  #' @param remove_punctuation TRUE or FALSE for removing punctuation
  #' @param remove_numbers TRUE or FALSE for removing numbers
  #' @param remove_stopword FALSE for none; TRUE for standard tm set; input vector for custom stopword set
  #' @param stemming  TRUE or FALSE; can supply external stemming function
  #' @export
  #' @return a \code{matrix} of word,count
  #' @example \code{countmatrix <- DictMaker(paste(df[["text"]], collapse = " "), threshold = 100)}
  #' @details this uses the tm library's \url{https://cran.r-project.org/web/packages/tm/index.html} \code{termFreq} \url{https://www.rdocumentation.org/packages/tm/versions/0.7-1/topics/termFreq} function. 
  #' Note that stemming increases processing time like nobody's business. 
  dict = tm::termFreq(text_vector,
                      control = list(
                        tolower = to_lower,
                        removePunctuation = remove_punctuation,
                        removeNumbers = remove_numbers,
                        # stopwords is TRUE, FALSE, or character vector
                        stopwords = remove_stopwords,
                        # stemming is super slow, if needed we should bring in external function
                        stemming = stemming,
                        tokenize = "words"))
  return(as.matrix(dict[dict > threshold]))
  }
  

  

  ResultChecker = function(in_data, min_rows = 1) {
    #' ResultChecker
    #' 
    #' ResultChecker runs sanity tests on data.frames, e.g. produced by search
    #' @param in_data a \code{data.frame}, to be checked
    #' @param min_rows minumum number of rows a in_data needs to have to pass
    #' @return boolean: Passed TRUE or FALSE
    #' @export
    #' 
    if (nrow(in_data) > min_rows) {print ("Resultchecker returns T"); TRUE}
    else {print ("Resultchecker returns F"); FALSE}
    #print(summary(in_data))
  }
  