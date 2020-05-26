
apply_theme <- function (plot){
  plot <- plot + scale_x_date()
  plot <- plot + theme(axis.title = element_text(size=20), 
                       axis.text = element_text(size=16))
  return(plot)
}


plot_time_series <- function(input, df) {
  if (nrow(df)==0) {return()}
  # Pivot table on Amount by Date
  agg <- aggregate(df$Amount, by=list(df$Date), FUN=sum)
  names(agg)<-c("Date","TotalAmount")
  agg$Date <- as.Date(agg$Date)
  
  # Add point at previous date
  t <- data.frame(input$filterDateRange[1]-1, as.numeric(0))
  names(t)<-c("Date","TotalAmount")
  agg <- rbind(t, agg)
  
  # Order result by Date
  agg <- agg[order(agg$Date),]
  
  # Calculate cummulative sum
  agg$TotalAmount <- cumsum(agg$TotalAmount)
  
  plot <- ggplot(agg, aes(Date, TotalAmount))
  plot <- plot + geom_point()
  plot <- plot + geom_line(aes(group=1))  
  plot <- plot + labs(x="", y="Amount ($)") 
  plot <- apply_theme(plot)
  plot
}


plot_time_series_by_month <- function(input, df) {
  if (nrow(df)==0) {return()}
  
  df_pos <- df[df$Amount > 0, ]
  df_neg <- df[df$Amount < 0, ]
  
  
  # Pivot table on Amount by Date
  aggregate_df <- function(df, type) {
    months <- format(as.Date(df$Date), "%Y-%m")
    agg <- aggregate(df$Amount, by=list(months), FUN=sum)
    agg$Type <- type
    names(agg)<-c("Date","Amount", "Type")
    agg
  }
  agg_tot <- aggregate_df(df, "Total")
  agg_pos <- aggregate_df(df_pos, "Income")
  agg_neg <- aggregate_df(df_neg, "Expenses")
  
  agg <- rbind(agg_tot, agg_pos, agg_neg)

  agg$Date <- as.Date(paste(agg$Date,"-01",sep=''), format="%Y-%m-%d")
  
  
  # Order result by Date
  agg <- agg[order(agg$Date),]
  
  
  plot <- ggplot(agg, aes(Date, Amount, fill=Type))
  plot <- plot + geom_bar(position="dodge", stat="identity")
  plot <- plot + labs(x="", y="Amount ($)") 
  plot <- apply_theme(plot)
  plot
}


xy_click_str <- function(e) {
  if(is.null(e)) return(" \n ")
  paste0(as.Date(e$x, origin="1970-01-01"), "\n", round(e$y, 1), "$")
}


plot_time_series_by_COL <- function(input, df, COL) {
  if (nrow(df)==0) {return()}
  
  if (COL=="OwnerAccount") {
    df$COL <- paste(df$Ownership, df$Account, sep=ACCOUNT_SEP)
  } else {
    df$COL <- df[[COL]]
  }
  
  # Pivot table on Amount by Date and Heading
  agg <- aggregate(df$Amount, by=c(list(df$Date), list(df$COL)), FUN=sum)
  names(agg) <- c("Date", "COL", "TotalAmount")
  
  # Get valid elements for COL
  if (COL=="OwnerAccount") {
    valid_elements <- get_all_accounts()
  } else if (COL=="Heading") {
    valid_elements <- get_valid_headings()
  } else {
    stop("Invalid COL input.\n")
  }
  
  # Add point at previous date for each COL
  for (elem in valid_elements) {
    t <- data.frame(input$filterDateRange[1]-1, elem, as.numeric(0))
    names(t) <- c("Date", "COL", "TotalAmount")
    agg <- rbind(t, agg)
  }
  
  # Order result by Date
  agg <- agg[order(as.Date(agg$Date)),]
  
  
  # Calculate cummulative sum per COL
  for (elem in valid_elements) {
    agg[agg$COL == elem,]$TotalAmount <- cumsum(agg[agg$COL == elem,]$TotalAmount) 
  }
  
  plot <- ggplot(agg, aes(x=as.Date(Date))) 
  plot <- plot + geom_point(aes(y=agg$TotalAmount, col=agg$COL))
  plot <- plot + geom_line(aes(y=agg$TotalAmount, col=agg$COL))
  plot <- plot + labs(x="", y="Amount ($)", col=COL)
  plot <- apply_theme(plot)
  plot
}

plot_time_series_by_COL_by_month <- function(input, df, COL) {
  if (nrow(df)==0) {return()}
  
  if (COL=="OwnerAccount") {
    df$COL <- paste(df$Ownership, df$Account, sep=ACCOUNT_SEP)
  } else {
    df$COL <- df[[COL]]
  }

  # Pivot table on Amount by Date and COL
  d <- format(as.Date(df$Date), "%Y-%m")
  agg <- aggregate(df$Amount, by=c(list(d), list(df$COL)), FUN=sum)
  names(agg) <- c("Date", "COL", "TotalAmount")
  agg$Date <- as.Date(paste(agg$Date,"-01",sep=''), format="%Y-%m-%d")
  
  # Order result by Date
  agg <- agg[order(as.Date(agg$Date)),]
  
  plot <- ggplot(agg, aes(x=as.Date(Date), y=agg$TotalAmount, fill=agg$COL)) 
  plot <- plot + geom_bar(position="dodge", stat='identity')
  plot <- plot + labs(x="", y="Amount ($)", fill=COL)
  plot <- apply_theme(plot)
  plot
}

plot_categ_bars <- function(input, df) {
  if (nrow(df)==0) {return()}
  
  agg <- aggregate(df$Amount, by=c(list(df$Heading), list(df$Category)), FUN=sum)
  names(agg) <- c("Heading", "Category", "TotalAmount")
  plot <- ggplot(agg, aes(Category, TotalAmount))
  plot <- plot + geom_col() + coord_flip()
  plot <- plot + facet_grid(rows=vars(Heading), scales="free", space="free")
  plot <- plot + labs(x="", y="Amount ($)")
  plot
}

plot_total_distrib <- function(input, df) {
  plot <- ggplot(df, aes(Amount))
  plot <- plot + geom_histogram(bins=30)
  plot
}