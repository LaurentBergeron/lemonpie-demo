library(tidyverse)
library(shiny)
library(jsonlite)

MAX_AMOUNT = 500 # Min and Max amounts in filter range.
ACCOUNT_SEP = ' / '
PLOT_HEIGHT = '600px'

DATABASE_FOLDER = 'database_demo'
possib_csvformats = c('demo_bank')
TITLE = "Lemonpie (demo)"


read_csv <- function(filepath) {
  df <- read.csv(filepath, header=FALSE, stringsAsFactors=FALSE)
  df['Date'] <- as.Date(df$V1, tryFormats=c("%m/%d/%Y", "%m-%d-%Y"))
  df['Amount'] <- df$V2
  df['Note'] <- df$V3
  df['Payee'] <- df$V4
  
  # Delete unused columns
  df <- df[,-c(1:4)]
  
  df
}



import_csv_statement <- function(input, output) {
  filepath = input$uploadedFile$datapath
  print(filepath)

  # Read csv file
  df <- read_csv(filepath)
  
  # Go through a series of test to verify validity of table to import
  success_bool <- tryCatch({  
    
    # Check that the shape of table is correct
    conditions <- (names(df2import) != c('Date', 'Amount', 'Note', 'Payee'))
    if (sum(conditions) > 0) {
      stop("Columns names don't match format. Contact app developer.")
    }
    
    # Check that types of columns are valid
    conditions <- (sapply(df2import, class) != c("Date", "numeric", "character", "character"))
    if (sum(conditions) > 0) {
      stop("Columns types don't match format. Contact app developer.")
    }
    
    # Check that the columns are not NA
    conditions <- (is.na(df2import))
    if (sum(conditions) > 0) {
      stop("Import function released N/A value(s). Contact app developer.")
    }
    },
    warning=function(w) {
      print(w)
      output$importNotificationText <- renderText({paste("Import went wrong: ", w, sep="")})
      return(FALSE)
    },
    error=function(e) {
      print(e)
      output$importNotificationText <- renderText({paste("Import went wrong: ", e, sep="")})
      return(FALSE)
    })
  
  
  df['Ownership'] <- 'missing'
  df['Account'] <- 'missing'
  df['Heading'] <- 'missing'
  df['Category'] <- 'missing'
  # Add index column
  df <- tibble::rowid_to_column(df, "index")
  return(df)
}


read_database <- function() {
  df <- data.frame(Date=character(),
                   Amount=double(),
                   Note=character(),
                   Payee=character(),
                   Account=character(),
                   Ownership=character(),
                   Heading=character(),
                   Category=character(), 
                   stringsAsFactors=FALSE) 
  for (file in Sys.glob(file.path(DATABASE_FOLDER,"*","*","db_*.csv"))) {
    df_temp <- read.csv(file, fileEncoding="UTF-8-BOM")
    print(names(df_temp))
    df <- rbind(df, df_temp)
  }
  df$Date <- as.character(df$Date)
  df
}

reload_database <- function(rv) {
  rv$database_df_full <- read_database()
}

apply_filters <- function(input, df_full) {
  
  # Copy dataframe
  df_show <- data.frame(df_full)
  # print(input$filterAccount)
  
  # Account + owner filter
  df_show <- filter(df_show, (paste(Ownership, Account, sep=ACCOUNT_SEP) %in% input$filterAccount))
  if (nrow(df_show)==0) {return(df_show)}
  
  # Heading filter
  valid_headings = input$filterCateg
  df_show <- filter(df_show, Heading %in% valid_headings)
  if (nrow(df_show)==0) {return(df_show)}
  
  # Amount filter
  filterAmountMin <- input$filterAmount[1]
  if (filterAmountMin == -MAX_AMOUNT) {filterAmountMin <- -Inf}
  filterAmountMax <- input$filterAmount[2]
  if (filterAmountMax == +MAX_AMOUNT) {filterAmountMax <- Inf}
  df_show <- filter(df_show, (Amount > filterAmountMin))
  df_show <- filter(df_show, (Amount < filterAmountMax))
  if (nrow(df_show)==0) {return(df_show)}
  
  # Date filter
  dateRangeMin <- input$filterDateRange[1]
  dateRangeMax <- input$filterDateRange[2]
  df_show <- filter(df_show, (as.Date(Date) >= dateRangeMin))
  df_show <- filter(df_show, (as.Date(Date) <= dateRangeMax))
  if (nrow(df_show)==0) {return(df_show)}

  df_show
}

apply_category <- function(rv, heading, category) {
  i <- rv$current_import_row
  rv$statement_df[i, 'Heading'] <- heading
  rv$statement_df[i, 'Category'] <- category
}

increment_current_row <- function(rv) {
  new_val <- rv$current_import_row + 1
  if (new_val < nrow(rv$statement_df) + 1) {
    rv$current_import_row <- new_val
  }
}

reload_statement_df <- function(output, rv) {
  if (nrow(rv$statement_df) > 0){
    i <- rv$current_import_row
    j <- rv$current_import_row + 4
    df <- data.frame(rv$statement_df)
    
    # Add two empty rows at the start and end of displayed statement
    two_null_rows <- data.frame(df[1:2,])
    two_null_rows[1:2,] <- NA
    df_show <- rbind(two_null_rows, df, two_null_rows)
    
    rv$statement_df_show <- df_show[i:j,] %>%
                            datatable(options=list(searching=FALSE,  paging=FALSE,
                                                   info=FALSE, ordering=FALSE, processing=FALSE),
                                      rownames= FALSE) %>%
                            formatDate('Date') %>%
                            formatCurrency('Amount') %>%
                            formatStyle('index', target='row', 
                                        backgroundColor=styleEqual(rv$current_import_row, '#FDFF00')
                            )
  }
}

increment_current_row <- function(rv) {
  new_val <- rv$current_import_row + 1
  if (new_val < nrow(rv$statement_df) + 1) {
    rv$current_import_row <- new_val
  }
}

decrement_current_row <- function(rv) {
  new_val <- rv$current_import_row - 1
  if (new_val > 0) {
    rv$current_import_row <- new_val
  }
}


finish_import <- function(output, df2import) {
  
  df2import$index <- NULL
  
  # Go through a series of test to verify validity of table to import
  success_bool <- tryCatch({  
    
    # Check that the shape of table is correct
    conditions <- (names(df2import) != get_valid_colnames(use_index=FALSE))
    if (sum(conditions) > 0) {
      stop("Columns names don't match format. Contact app developer.")
    }
    
    # Check that types of columns are valid
    print(sapply(df2import, class))
    conditions <- (sapply(df2import, class) != get_valid_coltypes(use_index=FALSE))
    if (sum(conditions) > 0) {
      stop("Columns types don't match format. Contact app developer.")
    }
    
    # Check that there is a single Ownership
    ownership = df2import[1, 'Ownership']
    
    # Check that there is a single account
    account = df2import[1, 'Account']
    
    # Check that headings and categories are valid
    for (i in seq_len(nrow(df2import))){
      head <- df2import[i,'Heading']
      categ <- df2import[i,'Category']
      if (!is.element(head, get_valid_headings())) {
        stop('Heading"', head, '" is invalid. Edit table and try again! <3\n')
      } else if (!is.element(categ, get_valid_categories(head))) {
        stop('Category"', categ, '" is invalid. Edit table and try again! <3\n')
      }
    }
    
    # Create file name
    current_time <- str_replace_all(Sys.time(), " ", "_")
    current_time <- str_replace_all(current_time, ":", "'")
    filename = paste('db', current_time, ownership, account, ".csv", sep="_")
    
    # Write csv file
    write.csv(x=df2import, file=file.path(DATABASE_FOLDER, ownership, account, filename), row.names=FALSE)
    
    output$importNotificationText <- renderText({"Import successful!"})
    TRUE
  },
  warning=function(w) {
    print(w)
    output$importNotificationText <- renderText({paste("Import went wrong: ", w, sep="")})
    return(FALSE)
  },
  error=function(e) {
    print(e)
    output$importNotificationText <- renderText({paste("Import went wrong: ", e, sep="")})
    return(FALSE)
  })
  return(success_bool)
}

get_valid_categories <- function(heading) {
  fromJSON("categories.json")[[heading]]
}

get_valid_headings <- function() {
  names(fromJSON("categories.json"))
}


get_valid_colnames <- function(use_index=TRUE) {
  col_names <- c("index", "Date", "Amount", "Note", "Payee", "Ownership", "Account", "Heading", "Category")
  if (use_index) {
    return(col_names)
  } else {
    return(col_names[-1]) # all but the first element
  }
}

get_valid_coltypes <- function(use_index=TRUE) {
  col_types <- c("integer", "Date", "numeric", "character", "character", "character", "character", "character", "character")
  if (use_index) {
    return(col_types)
  } else {
    return(col_types[-1]) # all but the first element
  }
}

get_valid_owners <- function() {
  list.dirs(path = file.path(DATABASE_FOLDER), full.names = FALSE, recursive = FALSE)
}

get_valid_accounts <- function(ownership) {
  list.dirs(path = file.path(DATABASE_FOLDER, ownership), full.names = FALSE, recursive = FALSE)
}

get_all_accounts <- function() {
  all_accounts <- c()
  for (owner in get_valid_owners()){
    for (account in get_valid_accounts(owner)) {
      all_accounts <- c(all_accounts, paste(owner, account, sep=ACCOUNT_SEP))
    } 
  }
  return(all_accounts)
}
