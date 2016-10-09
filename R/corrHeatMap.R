corrHeatMap <- function(df,roundto=2,na.rm=FALSE,plotText=TRUE){

  if("character" %in% sapply(df,class)){stop("character columns not supported")}

  library(dplyr,quietly=T);library(ggplot2);library(tidyr,quietly = T);library(reshape2,quietly=T)

  facVarIndex <- which(sapply(df, is.factor))
  numFacVars <- length(facVarIndex)

  if(sum(is.na(df)) > 0 & na.rm == TRUE){ #if number of missing variables is greater than 0 AND na.rm arg set to TRUE
    obsBefore <- dim(df)[1]
    df <- na.rm(df) #remove all observations with missing variables
    obsAfter <- dim(df)[1]
    print(paste("removed ",obsBefore-obsAfter," rows"))}#and report how many observations were removed

  if(is.matrix(df)==FALSE){ #if df is not in matrix form

    if(numFacVars==1){ #if data has only one factor variable
      nonDummyVars <- df[,-facVarIndex] #isolate non factor variables
      dummyVars <- contrasts(df[,facVarIndex],contrasts = FALSE) #create dummy variables
      sapply(df[,facVarIndex],function(x) dummyVars[x == rownames(dummyVars),]) %>% #create dummy matrix
        t() %>% #transpose matrix
        cbind(nonDummyVars,.) -> df #combine new dummy variables with non dummy variables
    } else {
      df <- model.matrix(~., data=df, #convert into a matrix
                   contrasts.arg = lapply(df[, sapply(df, is.factor)], contrasts, contrasts = FALSE))[,-1] #no dummy vars dropped
    }
  }

  df %>% #remove intercept column created by model.matrix()
    cor() %>% #create correlation matrix
    round(., roundto) -> cormat #round to level as set by roundto and store result

  # Get lower triangle of the correlation matrix
  get_lower_tri<-function(cormat){
    cormat[upper.tri(cormat)] <- NA
    return(cormat)
  }
  # Get upper triangle of the correlation matrix
  get_upper_tri <- function(cormat){
    cormat[lower.tri(cormat)]<- NA
    return(cormat)
  }

  #reorder the correlation matrix
  reorder_cormat <- function(cormat){
    if(numFacVars == 1) return(cormat)
    # Use correlation between variables as distance
    dd <- as.dist((1-cormat)/2)
    hc <- hclust(dd)
    cormat <-cormat[hc$order, hc$order]
    return(cormat)
  }

  cormat %>%
    reorder_cormat %>%
    get_upper_tri() %>%
    ifelse(. == 1, NA, .) %>%
    as.matrix() %>%
    melt(., na.rm = TRUE) %>%
    #filter(abs(value) > minCorr) %>%
    top_n(10,abs(value)) %>%
    ggplot(aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white",
                         midpoint = 0, limit = c(-1,1), space = "Lab",
                         name="Pearson\nCorrelation") +
    theme_minimal() + # minimal theme
    theme(axis.text.x = element_text(angle = 45, vjust = 1,
                                     size = 12, hjust = 1))+
    coord_fixed() +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = "top",
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5)) -> p

    if(plotText == TRUE) p <- p + geom_text(aes(Var2, Var1, label = value*100), color = "black", size = 4)

  print(p)

  cormat %>%
    as.table %>%
    as.data.frame %>%
    filter(Var1 != "(Intercept)",
           Var2 != "(Intercept)",
           Var1 != Var2) %>%
    arrange(-abs(Freq)) %>%
    tbl_df %>%
  return(list("cormat" = cormat,"cormatOrdered" = .))
}

