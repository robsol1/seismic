library(tidyverse)
library(readr)
library(factoextra)
library(DT)
library(shiny)
library(corrplot)

descnums <- function(df) {
  nums <-
    df[, sapply(df, is.numeric)] # new df containing just the numeric variables
  rangenums <- nums %>%
    mutate(sample = row_number()) %>% # Create an identifier column for each sample
    pivot_longer(cols = -sample) %>% # creates a long data set with an observation for each sample and variable
    group_by(name) %>% # Groups the data by the variable name.
    summarise(
      #create the summary statistics for each variable
      min = min(value),
      max = max(value),
      median = median(value),
      sd = sd(value),
      # strandard deviation
      p01 = quantile(value, prob = 0.01),
      # first prcentile
      p99 = quantile(value, prob = 0.99) # 99th percentile
    ) %>%
    mutate(range = (max - min) / sd) %>%  # range in terms of z statistic
    #(number of standard deviations between 1st and 99th percentile)
    #- if normally distributed this should be max of ~6)
    arrange(-range) # arrange such that the biggest variance variables are at the top
}





trim_outlier <- function(df, max_Z) {
  mean <- mean(df$value)
  sd <- sd(df$value)
  df <- df %>%
    mutate(mean = mean,
           sd = sd,
           z = abs(value - mean) / sd) %>%
    arrange(-z)
  if (df$z[1] > max_Z) {
    df <- df[2:nrow(df),]
    meann <- mean(df$value)
    sdn <- sd(df$value)
    df <- df %>%
      mutate(mean = meann,
             sd = sdn,
             z = abs(value - meann) / sdn) %>%
      arrange(-z)
  }
  df
}




################################

clean_all_nums <- function(dframe,
                           max_Z = 4,
                           keep_vars = NULL,
                           data_cols) {
  n_outliers = 3
  count = 1
  dframe1 <- dframe
  while (n_outliers > 0) {
    count = count + 1
    print(count)
    ranges <- descnums(dframe1)
    if (!is.null(keep_vars)) {
      ranges <- anti_join(ranges, data.frame(name = keep_vars))
    }
    outliers <- ranges %>%
      filter(range > 2 * max_Z)
    n_outliers <- nrow(outliers)
    print(paste0("outliers :", n_outliers))
    if (n_outliers > 0) {
      long <- dframe %>%
        #pivot_longer(cols = 3:ncol(dframe), names_to = "Var")
        pivot_longer(cols = all_of(data_cols), names_to = "Var")
      i = 1
      varname <- outliers$name[i]
      print(varname)
      thisvar <- long %>%
        filter(Var == varname)
      obs <- nrow(thisvar) + 1
      while (nrow(thisvar) < obs) {
        obs <- nrow(thisvar)
        thisvar <- trim_outlier(df = thisvar, max_Z = max_Z)
      }
      thisvar <- thisvar %>% select(1:2)
      dframe1 <- inner_join(thisvar, dframe1)
    }
  }
  dframe1
}


n_clusters <- function(df,colnames,max_clusters=10,seed=123){
  data <- df %>% 
    select(all_of(colnames))
  data <-  data.frame(scale(data))
  # Initialize total within sum of squares error: wss
  wss <- numeric(max_clusters)
  set.seed(seed)
  # Look over 1 to n possible clusters
  for (i in 1:max_clusters) {
    # Fit the model: km.out
    km.out <- kmeans(data, centers = i, nstart = 20)
    # Save the within cluster sum of squares
    wss[i] <- km.out$tot.withinss
  }
  
  # Produce a scree plot
  wss_df <- tibble(clusters = 1:max_clusters, wss = wss) %>% 
    mutate(improvement=lag(wss)-wss)
  
  wss_plot <- ggplot(wss_df, aes(x = clusters, y = wss, group = 1)) +
    geom_point(size = 4) +
    geom_line() +
    xlab('Number of clusters')
  
  improve.plot <-
    ggplot(wss_df, aes(x = clusters, y = improvement, group = 1)) +
    geom_point(size = 4) +
    geom_line() +
    xlab('Number of clusters')

  res <- list(scaled_data=data,table=wss_df,wss_plot=wss_plot,improve.plot=improve.plot)
}

summarise_cluster <- function(df,colnames){
  data <- df %>% 
    select(colnames)
  summary <- df %>% 
    pivot_longer(cols=colnames,names_to = "Element") %>% 
    group_by(Element,cluster) %>% 
    summarise(mean=mean(value)) %>% 
    mutate(mean=signif(mean,4)) %>% 
    pivot_wider(names_from = cluster,names_prefix ="Cluster_",values_from = mean)
  rows <- df %>% 
    group_by(cluster) %>% 
    summarise(Count=n()) %>% 
    pivot_wider(names_from = cluster,names_prefix ="Cluster_",values_from = Count) %>% 
    mutate(Element="Count") %>% 
    select(Element,1:(ncol(.)-1))
  summary <- rbind(rows,summary)
}
kmeans_cluster <- function(df,clusters,cluster_col_names,cluster_name="cluster",seed=NULL){
  if(!is.null(seed)){
    set.seed(seed)
  }
  scaled <- df %>% 
    select(all_of(cluster_col_names))
  scaled <- scale(scaled)
  km.out <-
    kmeans(scaled,
           centers = clusters, nstart = 50)
  
  ## Finally bring the data sets together
  
  clustered_data <-
    cbind(df %>% select(!(all_of(cluster_col_names))),
          data.frame(cluster =  as.factor(km.out$cluster)))
  names(clustered_data)[ncol(clustered_data)] <- cluster_name
  list(cluster=km.out,cluster_table=clustered_data,scaled=scaled)
  
}
