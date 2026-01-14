# this function is "invisible" to the user
.write_file_tree = function(home_dir = getwd(), main_dir = NULL){
  # 1. Checks if main_dir name is appropriate, if not, changes
  # 2. Checks to see if directory exists, if it does, throws error
  # 3. If given home_dir exists, creates main_dir
  # 4. If given home_dir does not exist, creates home_dir and main_dir
  # 5. Creates sub-folders
  # 6. Sets wd in analysis subfolder to place .Rmd file
  today = Sys.Date()
  if(is.null(main_dir)){
    main_dir = paste0("Report_",today)}
  if(typeof(main_dir) != "character"){ #1.
    main_dir = paste0("Report_",main_dir,"/")} #1.
  banned = c("#", "%", "&", "<", ">", "*", ".",
             "?","$", "!", ":", ";", "@", "=") #1.
  for(i in banned){ #1.
    if(grepl(i, main_dir, fixed = TRUE) == TRUE){#1.
      main_dir = paste0("Report_", Sys.Date(),"/")#1.
      message("Sorry, your main_dir contained a banned character")#1.
      message(paste0("The new name is: ", main_dir))#1.
      break}}#1.
  if(file.exists(file.path(home_dir, main_dir))){ #2.
    return("Location already exists, please provide a different location")} #2.
  if(file.exists(home_dir)){ #3.
    output = file.path(home_dir, main_dir) #3.
    dir.create(path = output) #3.
  }else{
    output = file.path(home_dir, main_dir) #4.
    dir.create(path = home_dir)#4.
    dir.create(path = output)}#4.
  dir.create(paste(output,"analysis", sep = "/")) #5.
  dir.create(paste(output,"analysis", "old", sep = "/"))
  dir.create(paste(output,"docs", sep = "/")) #5.
  dir.create(paste(output,"primaryData", sep = "/")) #5.
  dir.create(paste(output,"processedData", sep = "/")) #5.
  setwd(file.path(output,"analysis")) # 6.
  message("Your file path is: ", output, "/analysis")  #7. explains location
  return(T)} #2

# This is the function the user calls to create the directory structure
mk_dir = function(location = NULL, title = NULL){
  default_rmd = paste0("Report_",Sys.Date())
  if(is.null(location) & is.null(title)){
    temp = .write_file_tree()
    if(temp != T){return(message(temp))}
  } else if(is.null(location) & !is.null(title)){
    temp = .write_file_tree(main_dir = title)
    if(temp != T){return(message(temp))}
  } else if(!is.null(location) & is.null(title)){
    temp = .write_file_tree(home_dir = location)
    if(temp != T){return(message(temp))}
  } else {
    temp = .write_file_tree(home_dir = location, main_dir = title)
    if(temp != T){return(message(temp))}}
}

# Finds the dataframe
.find_data = function(file_path,  file_name = NULL, time){
  if(!is.null(file_name) & file_path == T){
    file_load = paste0("../processedData/", file_name)
    message("Loading: ", file_load)
    d_name = paste0("data_", unlist(strsplit(file_name, ".", fixed = T))[1],
                    "_", time,".RData")
    message(paste0("Saving as: ", d_name))
    assign('d', get(load(file_load)), envir = .GlobalEnv)
    save(d, file = d_name)}
  else{
    d_names = dir(pattern = ".RData")
    message("Loading: ", d_names[length(d_names)])
    assign('d', get(load(d_names[length(d_names)])))}
}
# Calls .find_data and makes the info file from that df
mk_info = function(file_name = NULL){
  today = gsub(" ", ".",Sys.time())
  today = gsub(":", "-",today)
  if(is.null(file_name)){
    .find_data(time = today, file_path == F)
  } else{
    .find_data(file_name = file_name, time = today, file_path = T)}
  info = data.frame(Variables = colnames(d), Labels = colnames(d),
                    Class = NA, Analysis_Types = NA, Ordered = 0,
                    Number_Unique_Values = NA, Number_Levels = NA,
                    Factor_Levels = NA, Ref_Levels = NA,
                    Univariate_Condition = 0, SLR = 0,  MLR = 0, Include = 0,
                    Censor = 0, Time = 0)
  message("Classes are:")
  for(i in 1:nrow(info)){
    info$Class[i] = class(d[,i])[1]
    message(paste0(colnames(d)[i],"->", info$Class[i]))
    info$Number_Unique_Values[i] = length(unique(d[,i]))
    if(info$Class[i] == "factor" | info$Class[i] == "ordered"){
      info$Number_Levels[i] = length(levels(d[,i]))
      if(length(levels(d[,i])) == 2){
        info$Analysis_Types[i] = "binary"
      }else{
        info$Analysis_Types[i] = "factor"
      }
      info$Factor_Levels[i] = paste0(levels(d[,i]), collapse = ", ")
      info$Ref_Levels[i] = levels(d[,i])[1]
      if(is.ordered(d[,i])){
        info$Ordered[i] = 1}
    }
    else if(info$Class[i] == "character"){
      info$Analysis_Types[i] = "character"}
    else{
      info$Analysis_Types[i] = "continuous"}
  }
  if(is.null(file_name)){
    i_name = paste0("new_info_", today, ".csv")
  } else{
    i_name = paste0("info_", unlist(strsplit(file_name, ".", fixed = T))[1],
                    "_", today,".csv")
  }
  message(paste0("Saving info as: ", i_name))
  write.csv(info, file = i_name, row.names = F)
}
# Determines the YAML header
.set_head = function(titleF, authorF, today = Sys.time()){
  title  = paste0("title: ", "'",titleF,"'")# creates title line based on title input
  author = paste0("author: ", authorF)      # creates author line from input
  date = paste0("date: ", today)            # sets date of creation from input
  # document type for when we want to fold code

  doc = paste("output:",
              "output:",
              "  html_document:",
              "  toc: true # table of content true",
              "toc_float:",
              "  collapsed: true",
              "smooth_scroll: true",
              "depth: 3  # up to three depths of headings (specified by #, ## and ###)",
              "number_sections: TRUE  ## if you want number sections at each table header",
              "theme: default # one of 'default', 'cerulean', 'journal', 'flatly',",
              "# 'readable', 'spacelab', 'united', 'cosmo', 'lumen',",
              "# 'paper', 'sandstone', 'simplex', 'yeti'",
              "self_contained: true",
              "params:",
              "  docStatus: 'DRAFT VERSION'",
              "exeTime: !r date()",
              "sessionDetails: FALSE # Controls display of R session details",
              sep = "\n")

  return(paste("---", title, author, date, doc, "---", sep = "\n"))
}
# this function writes the log chunk
.create_log = function(authorF, fname = NULL){
  if(is.null(fname)){
    fname = "*file name here*"
  }
  log = paste(
    # the title of the log chunk
    "# Change Log",
    # Generates the date/time when file is generate: records
    paste("1.", fname, "generated by", authorF, "on", Sys.time(), sep = " "),
    # Provides places for you to log modifications to boilerplate code
    "2. Enter modifications to boiler plate code below:",
    "    -","    -", "    -","    -",sep = "\n")
  return(log)
}
# This function writes the global options
.set_global_options = function(){
  # Sets title of R Chunk, as well as its parameters
  title1 = paste0("```{r"," Global-Options, include=FALSE", "}")
  # Sets the global parameters for all chunks to only show output
  opt1 = "knitr::opts_chunk$set(echo=TRUE, warning=FALSE, message=FALSE)"
  opt2 = "options(width = 800)"
  return(paste(title1, opt1, opt2,"```",
               "```{css, echo=FALSE}",
               "pre, code {white-space:pre !important; overflow-x:auto}",
               "```",
               sep = "\n"))
}
# this function writes a chunk to load all the necessary libraries
.library_loader = function(call){
  # This will become significantly more complicated and probably exist at
  # the end of the file b/c will only load necessary libraries
  inst = "# call mk_import() to install below packages"
  l0 = "library(bigBERD) # Provides access to helper functions"
  l1 = "library(ggplot2) # used for plotting"
  l2 = "library(gtsummary) # creates html formatted tables"
  l3 = "library(ggsci) # calls color palettes for plots"
  l4 = "library(corrplot) # plots correlation plot"
  l5 = "library(MASS) # ordinal logistic modelling"
  l6 = "library(forestmodel) # Creates Detailed Forest plots"
  l7 = "library(jtools); library(broom.mixed) # Creates Simple Forest plots"
  l8 = "library(pROC) # Creates ROC"
  l9 = "library(survival) # Creates survival models"
  l10 = "library(survminer) # Creates KM Plots"
  l11 = "library(nnet) # creates multinomial models"
  if(call == 1){ # mk_exp
    return(paste(
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l0, l1, l2, l3, l4, sep = "\n"),
      "```", sep = "\n"))
  }
  else if(call == 2){ # mk_univ, SLR = F
    return(paste(
      "# Libraries",
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l0, l1, l2, l3, l4, sep = "\n"),
      "```", sep = "\n"))
  }
  else if(call == 3){ # mk_univ, SLR = T
    return(paste(
      "# Libraries",
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l0, l5, l1, l2, l3, l4, l5, l8, l11,
            sep = "\n"), "```", sep = "\n"))
  }
  else if(call == 4){ # mk_MLR, SLR = F
    return(paste(
      "# Libraries",
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l0, l1, l2, l3, l6, l7, l8, sep = "\n"),
      "```", sep = "\n"))
  }
  else if(call == 5){ # MK_MLR, SLR = T
    return(paste(
      "# Libraries",
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l0, l5, l1, l2, l3, l5, l6, l7, l8, l11,
            sep = "\n"), "```", sep = "\n"))
  }
  else if(call == 6){ # mk_TTE, SLR = F
    return(paste(
      "# Libraries",
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l0, l1, l2, l3, l4, l6, l9, l10,
            sep = "\n"), "```", sep = "\n"))
  }
  else if(call == 7){ # mk_TTE, SLR = T
    return(paste(
      "# Libraries",
      "```{r Libraries, echo = FALSE, message = FALSE}",
      paste(inst, l1, l2, l3, l4, l5, l6, l8, l9, l10, l11,
            sep = "\n"), "```", sep = "\n"))
  }
}
# This writes a baseline summary statistics table for mk_exp()
create_baseline_table = function(info, order = F){
  tbl_lbl = rep(NA, length.out = nrow(info))
  include = rep(NA, length.out = nrow(info))
  dgts = rep(NA, length.out = nrow(info))
  for(i in 1:nrow(info)){
    if(info$Analysis_Types[i] != "character"){
      include[i] = paste0("'",info$Variables[i],"'")
      tbl_lbl[i] = paste0(info$Variables[i], " ~ ", "'",
                          info$Labels[i], "'")
      if(info$Analysis_Types[i] == "factor" | info$Analysis_Types[i] == "binary"){
        dgts[i] = paste0(info$Variables[i], " ~ ","c(0,0)")
      } else{dgts[i] = paste0(info$Variables[i], " ~ ","c(2,2,2,2)")}
    }
  }
  if(order == F){
    include = include[c(which(info$Analysis_Types == "continuous"),
                        which(info$Analysis_Types == "binary"),
                        which(info$Analysis_Types == "factor"))]}
  include = include[!is.na(include)]
  tbl_lbl = tbl_lbl[!is.na(tbl_lbl)]
  dgts = dgts[!is.na(dgts)]
  lbl = paste0("list(",paste0(tbl_lbl, collapse = ","),")")
  dgts = paste0("list(",paste0(dgts, collapse = ","),")")
  tb = paste( "# Baseline Table",
              "```{r Baseline-Table}",
              "# Set a compact theme",
              "theme_gtsummary_compact()",
              "# Undo theme: reset_gtsummary_theme()",
              "baseline_table = tbl_summary(d,",
              paste0("                  label = ", lbl,","),
              paste0("                  statistic = list(","\n",
                     "                    all_continuous() ~","'",
                     "{mean} ({sd}) [{min},{max}]", "'", "),"),
              paste0("                  missing_text = 'Missing', include = c("),
              paste0(paste0(include, collapse = ","),"),"),
              paste0("                   digits =", dgts,")"),
              "",
              paste0("baseline_table = modify_caption(baseline_table,",
                     "'**Summary Table**')"),
              "", "baseline_table",
              "```","", "",
              sep = "\n")
  return(tb)
}
# Creates a correlation matrix plot for all continuous variables in the dataset
create_corr_plot = function(info){
  corr = paste(
    "```{r Correlation-Plot}",
    "con_vars = which(info$Analysis_Types == 'continuous')",
    "rho.out = matrix(ncol = length(con_vars), nrow = length(con_vars))",
    "p.out = matrix(ncol = length(con_vars), nrow = length(con_vars))",
    "row.names(rho.out) = info$Labels[con_vars]",
    "colnames(rho.out) = info$Labels[con_vars]",
    "row.names(p.out) = info$Labels[con_vars]",
    "colnames(p.out) = info$Labels[con_vars]",
    "for(i in 1:length(con_vars)){ # loop through columns twice",
    "  for(j in 1:length(con_vars)){",
    "    x = d[,c(con_vars[i],con_vars[j])] # subset data frame",
    "    x = x[complete.cases(x),]     # remove NA",
    "    rho.out[i,j] = unname(unlist( # calculate rho",
    "      cor.test(x = x[,1], y = x[,2], method = 'pearson', exact = F)[4]))",
    "    p.out[i,j] = unname(unlist( # calculate pvalue",
    "      cor.test(x = x[,1], y = x[,2], alternative = 'two.sided',",
    "               method = 'pearson', exact = F)[3])) # pvalue",
    "  }}",
    "corrplot(rho.out, type = 'lower', addCoef.col = 'black', p.mat = p.out)",
    "```",
    sep = "\n")
  return(corr)
}
# Writes bar plots for categorical variables
.write_hist_cat = function(vari, Label){
  title = paste0("title = ","'",Label,"'")
  g = paste(
    title,
    paste0("p_",vari, " = ", "ggplot(data = d, aes(x = ",vari,
           ", fill = ", vari, ")) +"),
    "  geom_bar(width = 1, stat = 'count', na.rm = T) +",
    "  stat_count(aes(label = scales::percent(..count../sum(..count..))),",
    "             geom = 'text', color = 'white', size = 5,",
    "             position = position_stack(vjust = 0.5), na.rm = T) +",
    "  theme_classic() + ggtitle(title) + scale_y_continuous(expand = c(0,0)) +",
    paste0("  scale_fill_npg(name = ", "'",Label, "'",
           ", labels = levels(d$", vari, ")) +"),
    "  scale_x_discrete(na.translate = F) +",
    paste0("  xlab(", "'" ,Label, "'", ") + ylab('Count')"),
    sep = "\n")
  return(g)
}
# writes histograms with density lines for continuous variables
.write_hist_cont = function(vari, Label){
  title = paste0("title = ","paste0(","'",Label,", '",
                 ",'Median = ', round(m_", vari, "),2)")
  g = paste(
    paste0("m_",vari, "= median(d","$",vari,", na.rm = T)"),
    "bin_number = 30",
    title,
    paste0("p_",vari, " = ", "ggplot(data = d, aes(x = ",vari,")) +"),
    paste0("    geom_histogram(aes(y = ..density.., fill = colr), alpha = 0.8,
                   bins = bin_number, na.rm = T) +"),
    paste0("    geom_density(color = colr, size = 1, na.rm = T) +"),
    paste0("    theme_classic() +", " xlab(","'" ,Label,"'",") +"),
    paste0("    ggtitle(title) + ylab('Density') +"),
    paste0("    theme(legend.position = 'none') +"),
    paste0("    geom_vline(xintercept = ", "m_", vari,
           ", color = col_vline, size = 1) +"),
    "    scale_y_continuous(expand = c(0,0))",
    sep = "\n")
  return(g)
}
# Calls the above two functions and decides which to use based on data
create_hists_single = function(info){
  hist_chunks = rep(NA, length.out = nrow(info))
  p_hist = rep(NA, length.out = nrow(info))
  for(i in 1:nrow(info)){
    if(info$Analysis_Types[i] == "continuous"){
      hist_chunks[i] =  paste(
        .write_hist_cont(vari = info$Variables[i],
                        Label = info$Labels[i]),
        "",
        sep = "\n")
      p_hist[i] = paste0("p_", info$Variables[i])
    }
    else if(info$Analysis_Types[i] == "factor" |
            info$Analysis_Types[i] == "binary"){
      hist_chunks[i] =  paste(
        .write_hist_cat(vari = info$Variables[i],
                       Label = info$Labels[i]),
        "",
        sep = "\n")
      p_hist[i] = paste0("p_", info$Variables[i])
    }
  }
  hist_chunks = hist_chunks[!is.na(hist_chunks)]
  p_hist = p_hist[!is.na(p_hist)]
  hist_out = paste( "# Univariate Plots",
                    "```{r Univariate-Plots}",
                    "colr = '#E64B35FF'",
                    "col_vline = '#3C5488FF'",
                    paste0(hist_chunks, collapse = "\n"),
                    paste0(p_hist, collapse = ";"),
                    "```","","",
                    sep = "\n")
  return(hist_out)
}
# Creates a stratified bar plot between two categorical variables
.write_by_cat = function(x_var, fill_var, Label_x, Label_fill){
  title = paste0("title = ","'",Label_x, " by ", Label_fill, "'")
  g = paste(
    title,
    paste0("b_",x_var,"_", fill_var," = ", "ggplot(data = d, aes(x = ",x_var,
           ", fill = ", fill_var, "), na.rm = T) +"),
    "  geom_bar(stat = 'count', position = position_dodge(), na.rm = T) +",
    "  theme_classic() + ggtitle(title) + scale_y_continuous(expand = c(0,0)) +",
    paste0("  scale_fill_npg(name = ", "'",Label_fill, "'",
           ", labels = levels(d$", fill_var, "), na.translate = F) +"),
    "  scale_x_discrete(na.translate = F) +",
    paste0("  xlab(", "'" ,Label_x, "'", ") + ylab('Count')"),
    sep = "\n")
  return(g)
}
# Creates a scatter plot between two continuous variables
.write_scatter = function(x, y, Label_x, Label_y){
  title = paste0("title = ","'",Label_x," vs ",Label_y,"'")
  g = paste(
    "colr = '#3C5488FF'",
    title,
    paste0("sp_", x, "_", y, " = ",
           "ggplot(data = d, aes(y = ", y, ", x= ", x,")) +"),
    "  geom_point(color = colr, size = 2, shape = 16, alpha = 0.75, na.rm = T) +",
    "  theme_classic() + ggtitle(title) + geom_rug(color = colr) +",
    paste0("  xlab(", "'" ,Label_x,"') + ", "ylab(", "'" ,Label_y,"')"),
    sep = "\n")
  return(g)
}
# Creates a boxplot between a continuous and categorical variable
.write_boxplot = function(cont, cat, Label_cont, Label_cat){
  title = paste0("title = ","'",Label_cat," vs ",Label_cont,"'")
  g = paste(
    title,
    paste0("bw_",cat,"_",cont," = ",
           "ggplot(data = d, aes(y = ", cont, ", x= ", cat,
           ", fill = ", cat,")) +"),
    paste0("  geom_jitter(aes(color = ", cat ,"), width = 0.2 ,na.rm = T) +"),
    "  geom_boxplot(alpha = 0.1, outlier.alpha = 0, na.rm = T) +",
    paste0("  scale_fill_npg(name = ","'", Label_cat, "'",
           ", labels = levels(d$", cat, "), na.translate = F) +"),
    paste0("  scale_color_npg(name = ","'", Label_cat, "'",
           ", labels = levels(d$", cat, "), na.translate = F) +"),
    "  scale_x_discrete(na.translate = F) +",
    "  theme_classic() + ggtitle(title) +",
    paste0("  xlab(", "'" ,Label_cat,"') + ", "ylab(", "'" ,Label_cont,"')"),
    sep = "\n")
  return(g)
}
# Reads through by variable list and decides which of the above to call
# Ensures no duplicate plots
create_by_plots = function(info, by_list){
  by_chunks = rep(NA, length.out = nrow(info))
  p_by =  rep(NA, length.out = nrow(info))
  no_go_cont = which(info$Analysis_Types[which(
    info$Variables %in% by_list)] == "continuous")
  no_go_fac = which(info$Analysis_Types[which(
    info$Variables %in% by_list)] == "factor")
  no_go_bin = which(info$Analysis_Types[which(
    info$Variables %in% by_list)] == "binary")
  for(i in 1:nrow(info)){
    if(info$Analysis_Types[i] == "continuous"){
      temp_by = rep(NA, length.out = length(by_list))
      temp_p = rep(NA, length.out = length(by_list))
      for(b in 1:length(by_list)){
        b_element = which(info$Variables == by_list[b])
        if(info$Analysis_Types[b_element]=="factor"&i!=b_element&
           i!= any(no_go_cont)|info$Analysis_Types[b_element] == "binary"&
           i!=b_element&i!= any(no_go_cont)){
          # the below if statement removes duplicate plots
          # removed when i separated output by "by" variable
          #if(info$Variables[i] != by_list[b]){
          temp_by[b] = paste(
            .write_boxplot(cont = info$Variables[i], cat = by_list[b],
                           Label_cont = info$Labels[i],
                           Label_cat = info$Labels[b_element]),
            "","",sep = "\n")
          temp_p[b] = paste0("bw_",by_list[b],"_",info$Variables[i])
          #}
        }
        else if(info$Analysis_Types[b_element] == "continuous" & i!=b_element){
          #if(info$Variables[i] != by_list[b]){
          temp_by[b] = paste(
            .write_scatter(x = by_list[b],
                           y = info$Variables[i],
                           Label_x = info$Labels[b_element],
                           Label_y = info$Labels[i]),
            "","",sep = "\n")
          temp_p[b] = paste0("sp_",by_list[b],"_",info$Variables[i])
          #}

        }
      }
      temp_by = temp_by[!is.na(temp_by)]
      temp_p = temp_p[!is.na(temp_p)]
      if(length(temp_by)!=0 & length(temp_p)!=0){ # this should not be necessary
        by_out[i] = paste(
          paste0("\n# ",info$Labels[i], " Bivariate Plots"),
          paste0("```{r ",info$Variables[i],"}"),
          paste0(temp_by, collapse = ""),
          paste0(temp_p, collapse = ";"),
          "```",
          sep = "\n")
        rm(temp_by); rm(temp_p)
      }
    }
    else if(info$Analysis_Types[i] == "factor"|info$Analysis_Types[i] == "binary"){
      temp_by = rep(NA, length.out = length(by_list))
      temp_p = rep(NA, length.out = length(by_list))
      for(b in 1:length(by_list)){
        b_element = which(info$Variables == by_list[b])
        if(info$Analysis_Types[b_element]=="factor"&i!=b_element&
           i!=any(no_go_bin)&i!=any(no_go_fac)
           |info$Analysis_Types[b_element]=="binary"&i!=b_element&
           i!=any(no_go_fac)&i!= any(no_go_bin)){
          x_in = min(info$Number_Unique_Values[i],
                     info$Number_Unique_Values[b_element])
          if(info$Number_Unique_Values[i] == x_in){
            x_in = i
            fill_in = b_element}
          else{
            x_in = b_element
            fill_in = i}
          temp_by[b] = paste(.write_by_cat(
            x_var = info$Variables[x_in],
            fill_var = info$Variables[fill_in],
            Label_x = info$Labels[x_in],
            Label_fill = info$Labels[fill_in]),
            "","",sep = "\n")
          temp_p[b] = paste0("b_",info$Variables[x_in],"_",
                             info$Variables[fill_in])}
        else if(info$Analysis_Types[b_element] == "continuous" & i!=b_element
                & i!= any(no_go_fac) & i!= any(no_go_bin)){
          if(info$Variables[i] != by_list[b]){
            temp_by[b] = paste(
              .write_boxplot(cat = info$Variables[i],
                             cont = info$Variables[b_element],
                             Label_cat = info$Labels[i],
                             Label_cont = info$Labels[b_element]),
              "","",sep = "\n")
            temp_p[b] = paste0("bw_",info$Variables[i],"_",by_list[b])
          }
        }
      }
      temp_by = temp_by[!is.na(temp_by)]
      temp_p = temp_p[!is.na(temp_p)]
      if(length(temp_by)!=0 & length(temp_p)!=0){ # this should not be necessary
        by_out[i] = paste(
          paste0("\n# ",info$Labels[i], " Bivariate Plots"),
          paste0("```{r ",info$Variables[i],"}"),
          paste0(temp_by, collapse = ""),
          paste0(temp_p, collapse = ";"),
          "```",
          sep = "\n")
        rm(temp_by); rm(temp_p)
      }
    }
  }
  return(by_out)
}
# Creates the explore document
mk_exp = function(author, by = F){
  info_names= (dir(pattern="info_"))
  info_names = info_names[[length(info_names)]]
  d_names = dir(pattern = ".RData")
  d_names = d_names[length(d_names)]
  message("Using info version: ", info_names)
  message("Using Data version: ", info_names)
  info = read.csv(info_names, header = T)
  sp = ""
  today = gsub(" ", ".",Sys.time())
  today = gsub(":", "-",today)
  outfile = paste0("Exploratory_Analysis_", today, ".Rmd")


  load = paste(
    "```{r Load-Data}",
    paste0("load(file = ", "'", d_names, "'", ")"), sp,
    paste0("info = read.csv(", "'", info_names, "',", "header = T)"),
    "```",
    sep = "\n")

  baseline = create_baseline_table(info)
  corr = create_corr_plot(info)
  plots1 = create_hists_single(info)
  if(by[1] == F){
    t = paste0("Report_",Sys.time(), ": Univariate Exploratory Anlysis")
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 2), sp,
      load,
      sep = "\n")
    content = paste(
      head, sp, baseline, sp, corr, sp, plots1, sep = "\n")
    write(content, file = outfile)
  }else{
    t = paste0("Report_",Sys.time(), ": Bivariate Exploratory Anlysis")
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 2), sp,
      load,
      sep = "\n")
    plots2 = create_by_plots(info, by_list = by)
    content = paste(
      head, sp, baseline, sp, corr, sp, plots1, plots2, sep = "\n")
    write(content, file = outfile)}
}
# Creates univariate analysis tables
create_univariate_table = function(info, by_list, order = F){
  tbl_lbl = rep(NA, length.out = nrow(info))
  tp_lbl = rep(NA, length.out = nrow(info))
  include = rep(NA, length.out = nrow(info))
  dgts = rep(NA, length.out = nrow(info))
  tb =  rep(NA, length.out = length(by_list))
  for(i in 1:nrow(info)){
    if(info$Analysis_Types[i] != "character"){
      include[i] = paste0("'",info$Variables[i],"'")
      tbl_lbl[i] = paste0(info$Variables[i], " ~ ", "'",
                          info$Labels[i], "'")
      if(info$Analysis_Types[i] == "factor" |
         info$Analysis_Types[i] == "binary"){
        dgts[i] = paste0(info$Variables[i], " ~ ","c(0,0)")
      } else{
        dgts[i] = paste0(info$Variables[i], " ~ ","c(2,2,2,2)")
        tp_lbl[i] = paste0(info$Variables[i], " ~ ","'continuous'")}
    }
  }
  if(order == F){include = include[c(which(
    info$Analysis_Types == "continuous"),
    which(info$Analysis_Types == "binary"),
    which(info$Analysis_Types == "factor"))]}
  tbl_lbl = tbl_lbl[!is.na(tbl_lbl)]
  dgts = dgts[!is.na(dgts)]
  tp_lbl = tp_lbl[!is.na(tp_lbl)]
  lbl = paste0("list(",paste0(tbl_lbl, collapse = ",\n    "),")")
  dgts = paste0("list(",paste0(dgts, collapse = ",\n               "),")")
  tps =  paste0("list(",paste0(tp_lbl, collapse = ",\n    "),")")
  for(b in 1:length(by_list)){
    b_element = which(info$Variables == by_list[b])
    include = include[!is.na(include)]
    by_label = info$Variables[b_element]
    if(info$Analysis_Types[b_element] == "binary"){
      tb[b] = paste(
        paste(
          paste0("\n# ",info$Labels[b_element], " Univariate Table" ),
          paste0("```{r By-",by_label,"}"),
          "# Set a compact theme",
          "theme_gtsummary_compact()",
          "# Undo theme:reset_gtsummary_theme()",
          "# The include list controls the order of the variables in the table",
          "# You only need to change the variable order in that list",
          "# To remove a variable from the table, remove it from ALL lists",
          "# t in bold_p stands for threshold of bolding -> default = 0.05",
          paste0("tb_",by_label, " = tbl_summary(d,"),
          paste0("  by = ", info$Variables[b_element],","),
          paste0("  label = ", lbl,","),
          paste0("  type = ", tps,","),
          paste0("  statistic = list(","all_continuous() ~","'",
                 "{mean} ({sd}), [{min},{max}]", "'", "),"),
          paste0("  missing_text = 'Missing', include = c("),
          paste0(paste0(include, collapse = ",\n    "),"),"),
          paste0("  digits =", dgts,")"),
          "",
          paste0("tb_",by_label,
                 " = bold_labels(","tb_",by_label,")"),
          "", sep = "\n"),
        paste0("param_",by_label, " = add_p(","tb_",by_label, ","),
        "      test = list(all_continuous() ~ 't.test',",
        "                  all_categorical() ~ 'chisq.test'),",
        "      test.args = list(all_tests('t.test') ~ list(",
        "        var.equal = F, alternative = 'two.sided', paired = F),",
        "        all_tests('chisq.test') ~ list(simulate.p.value = T)),",
        "      pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>% bold_p(t = 0.05)",
        "",
        paste(
          paste0("nparam_",by_label, " = add_p(","tb_",by_label, ","),
          "      test = list(all_continuous() ~ 'wilcox.test',",
          "                  all_categorical() ~ 'fisher.test'),",
          "      test.args = list(all_tests('wilcox.test') ~ list(",
          "        alternative = 'two.sided', paired = F, exact = F)),",
          "        #all_tests('fisher.test') ~ list(simulate.pvalue = T)),",
          "      pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>% bold_p(t = 0.05)",
          sep = "\n"),
        "",
        paste0("param_",by_label, " = modify_caption(param_", by_label,", '",
               "**Parametric Summary by ", by_label,"**')"),
        paste0("nparam_",by_label, " = modify_caption(nparam_", by_label,", '",
               "**Non-Parametric Summary by ", by_label,"**')"),
        paste0("param_",by_label),
        paste0("nparam_",by_label),
        "```\n", sep = "\n")
    }
    else if(info$Analysis_Types[b_element] == "factor"){
      tb[b] = paste(
        paste(
          paste0("\n# ",by_label, " Univariate Table" ),
          paste0("```{r By-",by_label,"}"),
          "# Set a compact theme",
          "theme_gtsummary_compact()",
          "# Undo theme:reset_gtsummary_theme()",
          "# The include list controls the order of the variables in the table",
          "# To change order, change order in that list only",
          "# To remove a variable from the table, remove it from ALL lists",
          "# t in bold_p stands for threshold of bolding -> default = 0.05",
          paste0("tb_",by_label, " = tbl_summary(d,"),
          paste0("  by = ", info$Variables[b_element],","),
          paste0("  label = ", lbl,","),
          paste0("  type = ", tps,","),
          paste0("  statistic = list(","all_continuous() ~","'",
                 "{mean} ({sd}), [{min},{max}]", "'", "),"),
          paste0("  missing_text = 'Missing', include = c("),
          paste0(paste0(include, collapse = ",\n    "),"),"),
          paste0("  digits =", dgts,")"),
          "",
          paste0("tb_",by_label,
                 " = bold_labels(","tb_",by_label,")"),
          "", sep = "\n"),
        paste0("param_",by_label, " = add_p(","tb_",by_label, ","),
        "      test = list(all_continuous() ~ 'aov', # no real options",
        "      all_categorical() ~ 'chisq.test'),",
        "      test.args = list(all_tests('chisq.test') ~ list(simulate.p.value = T)),",
        "      pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>% bold_p(t = 0.05)","",
        paste(
          paste0("nparam_",by_label, " = add_p(","tb_",by_label, ","),
          "      test = list(all_continuous() ~ 'kruskal.test', # no real options",
          "                  all_categorical() ~ 'fisher.test'),",
          "      #test.args = list(all_tests('fisher.test') ~ list(",
          "      #simulate.pvalue = T, or = T, alternative = 'two.sided'))",
          "      pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>% bold_p(t = 0.05)",
          sep = "\n"),
        paste0("param_",by_label, " = modify_caption(param_", by_label,", '",
               "**Parametric Summary by ", by_label,"**')"),
        paste0("nparam_",by_label, " = modify_caption(nparam_", by_label,", '",
               "**Non-Parametric Summary by ", by_label,"**')"),
        paste0("param_",by_label),
        paste0("nparam_",by_label),
        "```\n", sep = "\n")
    }
  }
  tb = tb[!is.na(tb)]
  return(paste0(tb,collapse = "\n"))
}
# Determines simple regression based on outcome and generates tables
create_simple_reg = function(info){
  ocm = which(info$SLR == 1)
  regs = vector(mode = "character", length = length(ocm))
  for(i in ocm){
    y = info$Variables[i]
    if(info$Analysis_Types[i] == "continuous"){
      tmp.m = rep(NA, length.out = nrow(info))
      tmp.t = rep(NA, length.out = nrow(info))
      tmp.t2 = rep(NA, length.out = nrow(info))
      tmp.s = rep(NA, length.out = nrow(info))
      tmp.s2 = rep(NA, length.out = nrow(info))
      tmp.d = rep(NA, length.out = nrow(info))
      tmp.d2 = rep(NA, length.out = nrow(info))
      for(v in 1:nrow(info)){
        if(v!=i & info$Analysis_Types[v] != "character"){
          m = paste0(y,"_m", v)
          x = info$Variables[v]
          tmp.m[v] = paste0(m, "= lm(data = d[complete.cases(d),], ",y, "~",
                            x, ")")
          tmp.t[v] = paste(
            paste0(m, "_tbl = tbl_regression(", m,
                   ", label = list(", x," ~ ","'", info$Labels[v],
                   "'"  ,"),"),
            "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
            paste0("  modify_header(update = list(label ~ ","'**",
                   info$Labels[i], "**'", ")) %>% "),
            "  italicize_levels() %>% add_global_p() %>% bold_p(t = 0.05)",
            sep = "\n")
          tmp.t2[v] = paste0(m, "_tbl")
          tmp.d[v] = .write_fit_resid_single(model = m, y = y, x = x,
                                            Label_y = info$Labels[i],
                                            Label_x = info$Labels[v])
          tmp.d2[v] = paste0("fr_",m)
          if(info$Analysis_Types[v] == "continuous"){
            tmp.s[v] =paste(
              .write_smooth(x = x,y = y,
                           Label_x = info$Labels[v],Label_y = info$Labels[i]),
              sep = "\n")
            tmp.s2[v] = paste0("sp_", x, "_", y)}
        }
      }
      tmp.m = tmp.m[!is.na(tmp.m)]
      tmp.t = tmp.t[!is.na(tmp.t)]
      tmp.t2 = tmp.t2[!is.na(tmp.t2)]
      tmp.s = tmp.s[!is.na(tmp.s)]
      tmp.s2 = tmp.s2[!is.na(tmp.s2)]
      tmp.d = tmp.d[!is.na(tmp.d)]
      tmp.d2 =tmp.d2[!is.na(tmp.d2)]
      tmp.s = paste0(tmp.s, collapse = "\n\n")
      tmp.t2 = paste0(tmp.t2, collapse = ",\n  ")
      tmp.s2 = paste0(tmp.s2, collapse = ";")
      tmp.d = paste0(tmp.d, collapse = "\n\n")
      tmp.d2 = paste0(tmp.d2, collapse = ";")
      regs[which(ocm == i)] = paste(
        paste0("\n# ",info$Labels[i]," Simple Linear Regression Table"),
        paste0("```{r ", info$Labels[i],"-Simple-Linear-Regression}"),
        "# Set a compact theme",
        "theme_gtsummary_compact()",
        "# Undo theme:reset_gtsummary_theme()",
        "q.method ='bonferroni' # 'fdr', 'holm','hochberg','hommell','BH','BY'",
        paste0(tmp.m, collapse = "\n"),
        paste0(tmp.t, collapse = "\n"),
        paste0(y, "_tb = tbl_stack(list(",tmp.t2,
               ")) %>% add_q(method = q.method) %>%"),
        paste0("  modify_caption(", "'**", info$Labels[i],
               " Simple Linear Regressions on:**') %>%"),
        "modify_footnote(update = list(p.value ~ 'Global p.value')) %>%",
        "  bold_p(t = 0.05, q = T)",
        paste0(y,"_tb"), "```",
        paste0("\n## ",info$Labels[i]," Regression Graphs"),
        paste0("```{r ", info$Labels[i],"-Regression-Graphs}"),
        tmp.s, "\n", tmp.d, "\n",
        tmp.s2, tmp.d2, "```", "",
        sep = "\n")
    }
    if(info$Analysis_Types[i] == "binary"){
      tmp.m = rep(NA, length.out = nrow(info))
      tmp.t = rep(NA, length.out = nrow(info))
      tmp.t2 = rep(NA, length.out = nrow(info))
      tmp.d = rep(NA, length.out = nrow(info))
      tmp.d2 = rep(NA, length.out = nrow(info))
      for(v in 1:nrow(info)){
        if(v!=i & info$Analysis_Types[v] != "character"){
          m = paste0(y,"_m", v)
          x = info$Variables[v]
          tmp.m[v] = paste0(m, "= glm(data = d[complete.cases(d),], ",y, "~",
                            x, ", family = binomial)")
          tmp.t[v] = paste(
            paste0(m, "_tbl = tbl_regression(", m,
                   ", label = list(", x," ~ ","'", info$Labels[v],
                   "'"  ,"), exponentiate = T,"),
            "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
            paste0("  modify_header(update = list(label ~ ","'**",
                   info$Labels[i], "**'", ")) %>% "),
            "  italicize_levels() %>% add_global_p() %>% bold_p(t = 0.05)",
            sep = "\n")
          tmp.t2[v] = paste0(m, "_tbl")
          tmp.d[v] = .write_roc_single(model = m, Label_y = info$Labels[i],
                                      Label_x = info$Labels[v], ocm = i)
          tmp.d2[v] = paste0("roc_",m)
        }
      }
      tmp.m = tmp.m[!is.na(tmp.m)]
      tmp.t = tmp.t[!is.na(tmp.t)]
      tmp.t2 = tmp.t2[!is.na(tmp.t2)]
      tmp.d = tmp.d[!is.na(tmp.d)]
      tmp.d2 =tmp.d2[!is.na(tmp.d2)]
      tmp.t2 = paste0(tmp.t2, collapse = ",\n  ")
      tmp.d = paste0(tmp.d, collapse = "\n\n")
      tmp.d2 = paste0(tmp.d2, collapse = ";")
      regs[which(ocm == i)] = paste(
        paste0("\n# ",info$Labels[i]," Simple Logistic Regression Table"),
        paste0("```{r ", info$Labels[i],"-Simple-Logistic-Regression}"),
        "# Set a compact theme",
        "theme_gtsummary_compact()",
        "# Undo theme:reset_gtsummary_theme()",
        "q.method ='bonferroni' # 'fdr', 'holm','hochberg','hommell','BH','BY'",
        paste0(tmp.m, collapse = "\n"),
        paste0(tmp.t, collapse = "\n"),
        paste0(y, "_tb = tbl_stack(list(",tmp.t2,
               ")) %>% add_q(method = q.method) %>%"),
        paste0("  modify_caption(", "'**", info$Labels[i],
               " Simple Logistic Regressions on:**') %>%"),
        "modify_footnote(update = list(p.value ~ 'Global p.value')) %>%",
        "  bold_p(t = 0.05, q = T)",
        paste0(y,"_tb"), "```",
        paste0("\n## ",info$Labels[i]," Simple Diagnostic Graphs"),
        paste0("```{r ", info$Labels[i],"-Simple-Diagnostic-Graphs}"),
        tmp.d, "\n",
        tmp.d2, "```", "",
        sep = "\n")
    }
    else if(info$Analysis_Types[i] == "factor" & info$Ordered[i] == 0){
      print(y)
      tmp.m = rep(NA, length.out = nrow(info))
      tmp.t = rep(NA, length.out = nrow(info))
      tmp.t2 = rep(NA, length.out = nrow(info))
      for(v in 1:nrow(info)){
        if(v!=i & info$Analysis_Types[v] != "character"){
          m = paste0(y,"_m", v)
          x = info$Variables[v]
          tmp.m[v] = paste0(m, "= multinom(data = d[complete.cases(d),], ",
                            y, "~",x,")")
          tmp.t[v] = paste(
            paste0(m, "_tbl = tbl_regression(", m,
                   ", label = list(", x," ~ ","'", info$Labels[v],
                   "'"  ,"), exponentiate = T,"),
            "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
            paste0("  modify_header(update = list(label ~ ","'**",
                   info$Labels[i], "**'", ")) %>% bold_p(t = 0.05)"),
            sep = "\n")
          tmp.t2[v] = paste0(m, "_tbl")
        }
      }
      tmp.m = tmp.m[!is.na(tmp.m)]
      tmp.t = tmp.t[!is.na(tmp.t)]
      tmp.t2 = tmp.t2[!is.na(tmp.t2)]
      tmp.t2 = paste0(tmp.t2, collapse = ",\n  ")
      regs[which(ocm == i)] = paste(
        paste0("\n# ",info$Labels[i]," Simple Multinomial Regression Table"),
        paste0("```{r ", info$Labels[i],"-Simple-Multinomial-Regression}"),
        "# Set a compact theme",
        "theme_gtsummary_compact()",
        "# Undo theme:reset_gtsummary_theme()",
        "# WARNING: These are NOT global pvalues",
        paste0(tmp.m, collapse = "\n"),
        paste0(tmp.t, collapse = "\n"),
        paste0(y, "_tb = tbl_stack(list(",tmp.t2,
               ")) %>%"),
        paste0("  modify_caption(", "'**", info$Labels[i],
               " Simple Multinomial Regressions on:**')"),
        paste0(y,"_tb"), "```",
        sep = "\n")
    }
    else if(info$Analysis_Types[i] == "factor" & info$Ordered[i] == 1){
      tmp.m = rep(NA, length.out = nrow(info))
      tmp.t = rep(NA, length.out = nrow(info))
      tmp.t2 = rep(NA, length.out = nrow(info))
      for(v in 1:nrow(info)){
        if(v!=i & info$Analysis_Types[v] != "character"){
          m = paste0(y,"_m", v)
          x = info$Variables[v]
          tmp.m[v] = paste0(m, "= polr(data = d[complete.cases(d),], ",
                            y, "~",x,", method = 'logistic')")
          tmp.t[v] = paste(
            paste0(m, "_tbl = tbl_regression(", m,
                   ", label = list(", x," ~ ","'", info$Labels[v],
                   "'"  ,"), exponentiate = T,"),
            "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
            paste0("  modify_header(update = list(label ~ ","'**",
                   info$Labels[i], "**'", ")) %>% italicize_levels()",
                   "%>% add_global_p() %>% bold_p(t = 0.05)"),
            sep = "\n")
          tmp.t2[v] = paste0(m, "_tbl")
        }
      }
      tmp.m = tmp.m[!is.na(tmp.m)]
      tmp.t = tmp.t[!is.na(tmp.t)]
      tmp.t2 = tmp.t2[!is.na(tmp.t2)]
      tmp.t2 = paste0(tmp.t2, collapse = ",\n  ")
      regs[which(ocm == i)] = paste(
        paste0("\n# ",info$Labels[i]," Simple Ordinal Regression Table"),
        paste0("```{r ", info$Labels[i],"-Simple-Ordinal-Regression}"),
        "# Set a compact theme",
        "theme_gtsummary_compact()",
        "# Undo theme:reset_gtsummary_theme()",
        "q.method ='bonferroni' # 'fdr', 'holm','hochberg','hommell','BH','BY'",
        paste0(tmp.m, collapse = "\n"),
        paste0(tmp.t, collapse = "\n"),
        paste0(y, "_tb = tbl_stack(list(",tmp.t2,
               ")) %>%"),
        paste0("  modify_caption(", "'**", info$Labels[i],
               " Simple Ordinal Regressions on:**') %>%"),
        paste0("  add_q(method = q.method) %>%",
               "modify_footnote(update = list(p.value ~ 'Global p.value')) %>%"),
        "  bold_p(t = 0.05)",
        paste0(y,"_tb"), "```",
        sep = "\n")
    }
  }
  return(paste0(regs, collapse = "\n"))
}
# Writes a scatter plot with regression line
.write_smooth = function(x, y, Label_x, Label_y){
  title = paste0("title = ","'",Label_x," vs ",Label_y,"'")
  g = paste(
    "col_point = '#3C5488FF'",
    "col_line = '#E64B35FF'",
    title,
    paste0("sp_", x, "_", y, " = ",
           "ggplot(data = d, aes(y = ", y, ", x= ", x,")) +"),
    "  geom_point(color = col_point, size = 2, shape = 16,alpha = 0.75,na.rm = T) +",
    "  geom_smooth(method = 'lm', formula = 'y~x', color = col_line,na.rm = T) +",
    "  theme_classic() + ggtitle(title) + geom_rug(color = col_point) +",
    paste0("  xlab(", "'" ,Label_x,"') + ", "ylab(", "'" ,Label_y,"')"),
    sep = "\n")
  return(g)
}
# Writes a fitted vs residual plot for the full models
.write_fit_resid = function(model, Label){
  x = unname(unlist(strsplit(model, "_", fixed = T)))
  m = paste(
    paste0(x[1], "_p = data.frame(y = ",model,"$residuals,"),
    paste0("                x = ", model,"$fitted.values)"),
    sep = "\n")
  if(x[2] == "sat"){
    title = paste0("title =", "'",Label," Main Effects: Fitted vs Residuals", "'")
  }else{
    title = paste0("title =", "'",Label," Reduced: Fitted vs Residuals", "'")
  }
  g = paste(
    paste0("# ", model),
    m,
    "colr = '#3C5488FF'",
    title,
    paste0("fr_", x[1]," = ",
           "ggplot(data =", x[1], "_p, aes(y = y, x= x)) +"),
    "  geom_point(color = colr, size = 2, shape = 16, alpha = 0.75, na.rm = T) +",
    "  theme_classic() + ggtitle(title) +",
    "  geom_hline(yintercept = 0, color = colr, linetype = 'dashed', alpha = 0.5) +",
    "  xlab('Fitted') + ylab('Residuals')",
    paste0("fr_", x[1]),
    sep = "\n")
  return(g)
}
# Writes 2 ROC curves on one plot for the full models
.write_roc = function(models, Label, ocm){
  x = unname(unlist(strsplit(models[1], "_", fixed = T)))[1]
  g = paste(
    paste0("title =", "'", Label, ": Main Effects and Reduced ROC", "'"),
    paste0(x, "_rc_sat = roc(d[complete.cases(d),", ocm,
           "] ~ predict(", models[1], ",",
           " type = 'response'), plot = F, print.auc = F)"),
    paste0(x, "_rc_red = roc(d[complete.cases(d),", ocm,
           "] ~ predict(", models[2], ",",
           " type = 'response'), plot = F, print.auc = F)"),
    paste0(x, "_perc_sat = ",
           "paste0('AUC: ' ,round(unlist(unname(", x,
           "_rc_sat[9]))*100, 1), '%')"),
    paste0(x, "_perc_red = ",
           "paste0('AUC: ' ,round(unlist(unname(", x,
           "_rc_red[9]))*100, 1),'%')"),
    "colr1 = '#3C5488FF' # blue",
    "colr2 = '#E64B35FF' # red",
    paste0(x,"_roc = ggroc(data = list(",
           x,"_rc_sat, ", x, "_rc_red),
           legacy.axes = T, size = 1.2) +"),
    "  theme_classic() + ggtitle(title) +",
    "  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),",
    "                     color='grey', linetype='dashed') +",
    "  scale_y_continuous(expand = c(0,0)) +",
    "  scale_color_manual(values = c(colr1, colr2),",
    "                     labels = c('Main Effects', 'Reduced Model'),",
    "                     name = 'Model') +",
    "  annotate('text', x= 0.85, y = 0.2,",
    paste0("           label =",x ,"_perc_sat, color = colr1, size = 5) +"),
    "  annotate('text', x= 0.85, y = 0.15,,",
    paste0("           label =",x ,"_perc_red, color = colr2, size = 5)"),
    paste0(x,"_roc"),
    sep = "\n")
  return(g)
}
# Writes a fitted vs residual plot for the simple models
.write_fit_resid_single = function(model, y, x, Label_y, Label_x){
  m = paste(
    paste0(model, "_p = data.frame(y = ",model,"$residuals,"),
    paste0("                x = ", model,"$fitted.values)"),
    sep = "\n")
  title = paste0("title =", "'",Label_y, " on ", Label_x, ": Fitted vs Residuals", "'")
  g = paste(
    paste0("# ", model),
    m,
    "colr = '#3C5488FF'",
    title,
    paste0("fr_", model," = ",
           "ggplot(data =", model, "_p, aes(y = y, x= x)) +"),
    "  geom_point(color = colr, size = 2, shape = 16, alpha = 0.75, na.rm = T) +",
    "  theme_classic() + ggtitle(title) +",
    "  geom_hline(yintercept = 0, color = colr, linetype = 'dashed', alpha = 0.5) +",
    "  xlab('Fitted') + ylab('Residuals')",
    sep = "\n")
  return(g)
}
# Writes ROC plot for the simple models
.write_roc_single = function(model, Label_x, Label_y, ocm){
  g = paste(
    paste0("title =", "'", Label_y, " on ", Label_x, ": ROC", "'"),
    paste0(model, "_rc = roc(d[complete.cases(d),", ocm,
           "] ~ predict(", model, ",",
           " type = 'response'), plot = F, print.auc = F)"),
    paste0(model, "_perc = ",
           "paste0('AUC: ' ,round(unlist(unname(", model,
           "_rc[9]))*100, 1))"),
    "colr1 = '#3C5488FF'",
    paste0("roc_", model ,"= ggroc(data = ", model,"_rc, ",
           "legacy.axes = T, size = 1.2, color = colr1) +"),
    "  theme_classic() + ggtitle(title) +",
    "  geom_segment(aes(x = 0, xend = 1, y = 0, yend = 1),",
    "                     color='grey', linetype='dashed') +",
    "  scale_y_continuous(expand = c(0,0)) +",
    "  annotate('text', x= 0.85, y = 0.2,",
    paste0("           label =", model,"_perc, color = colr1, size = 5)"),
    sep = "\n")
  return(g)
}
# Creates relevel statements for non-ordered factors
create_relevel = function(info){
  lvls = rep(NA, length.out = nrow(info))
  for(i in 1:nrow(info)){
    if(!is.na(info$Ref_Levels[i])){
      if(info$Ordered[i] == 0){
        lvls[i] = paste0("d$", info$Variables[i], " = relevel(x = ",
                         "d$", info$Variables[i], ", ref = ",
                         "'", info$Ref_Levels[i],"')")
      }
      #      else{
      #        lvls[i] = paste0("d$", info$Variables[i], " = ordered(x = ",
      #                         "d$", info$Variables[i], ", levels = ",
      #                         "'", info$Factor_Levels[i],"')")
      #      }
    }}
  lvls = lvls[!is.na(lvls)]
  out = paste(
    "# Relevel",
    "```{r Relevel-Statements}",
    paste0(lvls, collapse = "\n"),
    "```", "", "",
    sep = "\n" )
  return(out)
}
# Creates a main effects cox proportional hazards model
# Creates KM plots with log-rank test
create_surv = function(info){
  tme = which(info$Time == 1)
  cns = which(info$Censor == 1)
  m.list = rep(NA, length.out = nrow(info))
  p.list = rep(NA, length.out = nrow(info))
  p.list2 = rep(NA, length.out = nrow(info))
  srv_object = paste0("Surv(as.numeric(",info$Variables[tme],
                      ") , as.numeric(", info$Variables[cns],"))")
  for(v in 1:nrow(info)){
    if(v!=tme & v!=cns & info$Analysis_Types[v] != "character"
       & info$Analysis_Types[v] != "continuous"){
      x = info$Variables[v]
      m = paste0("surv_", x)
      m.list[v] = paste(
        paste0(m, "= survfit(data = d, ",srv_object, "~",x, ")"),
        paste0("p_", m, " = ggsurvplot(fit = ", m,
               ", palette = 'npg', pval = T, pval.method = T,"),
        paste0("           title = ", "'", info$Labels[v], "'", ", data = d,"),
        paste0("           legend.labs = c(levels(d$",x,")))"),
        sep = "\n")
      p.list[v] = paste0("p_", m)
    }
  }
  m.list = m.list[!is.na(m.list)]
  p.list = p.list[!is.na(p.list)]
  m_out = paste0(m.list, collapse = "\n")
  p_out = paste0(p.list, collapse = ";")

  lbl1 = info$Variables[-c(which(info$Analysis_Types == "character"),
                           tme, cns)]
  lbl2 = paste("'",
               info$Labels[-c(which(info$Analysis_Types == "character"),
                              tme, cns)], "'", sep = "")
  lbl = paste0(paste(lbl1,lbl2,sep = "~"), collapse = ",\n  ")
  pred = paste0(lbl1, collapse = " + ")
  sat = paste0("surv_model = coxph(", srv_object, " ~ ", pred,", data =d)")
  sat_tbl = paste(
    paste0("surv_tbl = tbl_regression(surv_model,"),
    paste0("  label = list(",lbl,")) %>% "),
    "bold_labels() %>% bold_p(t = 0.05) %>% ",
    paste0("  modify_caption(","'**Cox Proportional Hazards**') %>%"),
    "  modify_header(estimate ~ '**Beta Values**')",
    sep = "\n")
  out = paste(
    "\n# KM Plots","```{r KM Plots}",m_out,p_out,"```", "",
    "\n# Cox Proportional Hazards Model","```{r CoxPH-Model}",
    "# Set a compact theme",
    "theme_gtsummary_compact()",
    "# Undo theme:reset_gtsummary_theme()",
    sat, sat_tbl, "surv_tbl", "```","",sep = "\n")
}
# generates the Univariate Analysis .Rmd document
mk_univ = function(author, by = NULL, order = F, SLR = T,
                   plt_univ = F){
  message("Dir: ",getwd())
  info_names= (dir(pattern="info_"))
  info_names = info_names[[length(info_names)]]
  d_names = dir(pattern = ".RData")
  d_names = d_names[length(d_names)]
  message("Using info version: ", info_names)
  message("Using Data version: ", info_names)
  info = read.csv(info_names, header = T)

  if(all(info$Univariate_Condition == 0) & all(info$SLR == 0)){
    return(message("Quitting: No Univariate tests or regressions specified"))
  }
  else{
    unv = info$Variables[which(info$Univariate_Condition == 1)]
  }
  if(is.null(by)){
    by_plts = unv
  }
  else{
    by_plts = unique(c(unv,by))
  }
  sp = ""
  today = gsub(" ", ".",Sys.time())
  today = gsub(":", "-",today)

  load = paste(
    "```{r Load-Data}",
    paste0("load(file = ", "'", d_names, "'", ")"), sp,
    paste0("info = read.csv(", "'", info_names, "',", "header = T)"),
    "head(d)",
    "```",sep = "\n")

  if(SLR == T & all(info$SLR == 0)){
    message("No Simple Regressions Specified")
    SLR = F}

  if(SLR == T){
    t = paste0("Report_",Sys.time(), ": Univariate Analysis + Simple Regression")
    message(paste0("SLR Outcome: ", paste0(
      info$Labels[which(info$SLR == 1)], collapse = "; ")))
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 3), sp,
      load,
      sep = "\n")
    if(plt_univ == F & is.null(by)){
      plts = ""
    }
    else{
      plts = create_by_plots(info = info, by_list = by_plts)
    }
    tbls = create_univariate_table(info, by_list = unv, order = order)
    lvls = create_relevel(info)
    crr = create_corr_plot(info)
    slr = create_simple_reg(info)
    content = paste(head, plts, tbls, lvls, sp, crr, slr, sep = "\n")
    outfile = paste0("Univariate_Analysis_SLR_", today, ".Rmd")
    write(content, file = outfile)
  }
  else{
    t = paste0("Report_",Sys.time(), ": Univariate Analysis")
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 2), sp,
      load,
      sep = "\n")
    if(plt_univ == F & is.null(by)){
      plts = ""
    }
    else{
      plts = create_by_plots(info = info, by_list = by_plts)
    }
    tbls = create_univariate_table(info, by_list = unv, order = order)
    crr = create_corr_plot(info)
    content = paste(head, plts, sp, crr, tbls, sep = "\n")
    outfile = paste0("Univariate_Analysis_", today, ".Rmd")
    write(content, file = outfile)
  }
}
# Creates models, tables, and calls plots for multiple regressions
create_multi_reg = function(info){
  ocm = which(info$MLR == 1)
  incl = which(info$Include == 1)
  out = vector(mode = "character", length = length(ocm))
  for(i in ocm){
    if(info$Analysis_Types[i] == "continuous"){
      y = info$Variables[i]
      lbl1 = info$Variables[
        -c(which(info$Analysis_Types == "character"),
           which(info$Variables == y))]
      lbl2 = paste("'",info$Labels[
        -c(which(info$Analysis_Types == "character"),
           which(info$Variables == y))], "'", sep = "")
      lbl = paste0(paste(lbl1,lbl2,sep = "~"), collapse = ",\n")
      pred = paste0(lbl1, collapse = " + ")
      sat = paste0(y,"_sat = lm(", y, " ~ ", pred,
                   ", data =d[complete.cases(d),])")
      sat_tbl = paste(
        paste0(y, "_sat_tbl = tbl_regression(",y, "_sat,"),
        paste0("label = list(",lbl,"),"),
        "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
        "bold_labels() %>% bold_p(t = 0.05) %>% ",
        paste0("  modify_caption(","'**",info$Labels[i],
               " Multiple Regression on:**') %>%"),
        "  modify_header(estimate ~ '**Beta Values**')",
        sep = "\n")
      if(length(incl) > 0){
        incl2 = incl[-c(which(incl == i))]
        if(length(incl2) == 1){
          incl_in = info$Variables[incl2]
          red = paste(
            paste0(y, "_red = stepAIC(object =",y,"_sat,",
                   " direction = 'backward',"),
            paste0("          scope = list(lower = as.formula(",
                   y, " ~ ", incl_in,"), upper =", y, "_sat))"),
            sep = "\n")}
        else if(length(incl2) > 1){
          incl_in = paste0(info$Variables[c(incl2)], collapse = " + ")
          red = paste(
            paste0(y, "_red = stepAIC(object =",y,"_sat,",
                   " direction = 'backward',"),
            paste0("          scope = list(lower = as.formula(",
                   y, " ~ ", incl_in,"), upper =", y, "_sat))"),
            sep = "\n")}
        else{
          red = paste0(y,"_red = stepAIC(object =",
                       y,"_sat, direction = 'backward')")}
      }else{
        red = paste0(y, "_red = stepAIC(object =",
                     y,"_sat, direction = 'backward')")}
      red_tbl = paste(
        "# Add labels from the saturated table to the label = list() param here",
        paste0(y,"_red_tbl = tbl_regression(",
               y, "_red, #, label = list()) %>%"),
        "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
        "  bold_labels()  %>% bold_p(t = 0.05) %>%",
        paste0("modify_caption(","'**",info$Labels[i],
               " StepAIC Regression on:**') %>%"),
        "  modify_header(estimate ~ '**Beta Values**')",
        sep = "\n")
      out[i] = paste(
        paste(
          paste0("\n# ", info$Labels[i], " Multiple Regression"),
          paste0("```{r ", info$Labels[i],"-Multiple-Regression}"),
          sep = "\n"),
        paste(
          "# Set a compact theme",
          "theme_gtsummary_compact()",
          "# Undo theme:reset_gtsummary_theme()",
          sep = "\n"),
        sat, sat_tbl, red, red_tbl,
        paste0(y, "_red_tbl; ", y,"_sat_tbl"),
        paste0("plot_summs("
               ,y, "_red, ", y,"_sat, ","model.names = c('Reduced', 'Main Effects')",
               ") # package from jtools"),
        paste0("forest_model(",y, "_sat)"),
        paste0("forest_model(",y, "_red)"), # package from "forestmodel"
        "",
        .write_fit_resid(model = paste0(y,"_sat"), Label = info$Labels[i]),
        "",
        .write_fit_resid(model = paste0(y,"_red"), Label = info$Labels[i]),
        "```", sep = "\n" )
    }
    else if(info$Analysis_Types[i] == "binary"){
      y = info$Variables[i]
      lbl1 = info$Variables[
        -c(which(info$Analysis_Types == "character"),
           which(info$Variables == y))]
      lbl2 = paste("'",info$Labels[
        -c(which(info$Analysis_Types == "character"),
           which(info$Variables == y))], "'", sep = "")
      lbl = paste0(paste(lbl1,lbl2,sep = "~"), collapse = ",\n")
      pred = paste0(lbl1, collapse = " + ")
      sat = paste(
        paste0(y,"_sat = glm(", y, " ~ ", pred, ","),
        "           family = 'binomial', data =d[complete.cases(d),])",
        sep = "\n")
      sat_tbl = paste(
        paste0(y, "_sat_tbl = tbl_regression(",y, "_sat,"),
        paste0("label = list(",lbl,"),"),
        "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
        "  bold_labels() %>% bold_p(t = 0.05) %>% ",
        paste0("  modify_caption(","'**",info$Labels[i],
               " Multiple Regression on:**') %>%"),
        "  modify_header(estimate ~ '**Beta Values**')",
        sep = "\n")
      if(length(incl) > 0){
        incl2 = incl[-c(which(incl == i))]
        if(length(incl2) == 1){
          incl_in = info$Variables[incl2]
          red = paste(
            paste0(y, "_red = stepAIC(object =",y,"_sat,",
                   " direction = 'backward',"),
            paste0("          scope = list(lower = as.formula(",
                   y, " ~ ", incl_in,"), upper =", y, "_sat))"),
            sep = "\n")}
        else if(length(incl2) > 1){
          incl_in = paste0(info$Variables[c(incl2)], collapse = " + ")
          red = paste(
            paste0(y, "_red = stepAIC(object =",y,"_sat,",
                   " direction = 'backward',"),
            paste0("          scope = list(lower = as.formula(",
                   y, " ~ ", incl_in,"), upper =", y, "_sat))"),
            sep = "\n")}
        else{
          red = paste0(y,"_red = stepAIC(object =",
                       y,"_sat, direction = 'backward')")}
      }else{
        red = paste0(y, "_red = stepAIC(object =",
                     y,"_sat, direction = 'backward')")}
      red_tbl = paste(
        "# Add labels from the saturated table to the label = list() param here",
        paste0(y,"_red_tbl = tbl_regression(",
               y, "_red, #, label = list()) %>%"),
        "  pvalue_fun = function(x)style_pvalue(x, digits = 3)) %>%",
        "  bold_labels()  %>% bold_p(t = 0.05) %>%",
        paste0("  modify_caption(","'**",info$Labels[i],
               "  StepAIC Regression on:**') %>%"),
        "modify_header(estimate ~ '**Beta Values**')",
        sep = "\n")
      out[i] = paste(
        paste(
          paste0("\n# ", info$Labels[i], " Multiple Regression"),
          paste0("```{r ", info$Labels[i],"-Multiple-Regression}"),
          sep = "\n"),
        paste(
          "# Set a compact theme",
          "theme_gtsummary_compact()",
          "# Undo theme:reset_gtsummary_theme()",
          sep = "\n"),
        sat, sat_tbl, red, red_tbl,
        paste0(y, "_red_tbl; ", y,"_sat_tbl" ),
        paste0("plot_summs("
               ,y, "_red, ", y,"_sat",
               ", model.names = c('Reduced', 'Main Effects')",
               ") # package from jtools"),
        paste0("forest_model(",y, "_sat)"),
        paste0("forest_model(",y, "_red)"), # package from forestmodel
        .write_roc(models = c(paste0(y, "_sat"), paste0(y, "_red")),
                  Label = info$Labels[i], ocm = i),
        "```",sep = "\n" )
    }}
  return(paste0(out, collapse = "\n"))
}
# Generates the multiple regression .Rmd document
mk_MLR = function(author, SLR = F){
  message("Dir: ",getwd())
  info_names= (dir(pattern="info_"))
  info_names = info_names[[length(info_names)]]
  d_names = dir(pattern = ".RData")
  d_names = d_names[length(d_names)]
  message("Using info version: ", info_names)
  message("Using Data version: ", d_names)
  info = read.csv(info_names, header = T)
  sp = ""
  today = gsub(" ", ".",Sys.time())
  today = gsub(":", "-",today)
  outfile = paste0("MLR_Analysis_", today, ".Rmd")
  if(all(info$MLR == 0)){
    return(message("Quitting: No Multiple Regressions specified"))
  }
  else{
    message(paste0("MLR Outcome: ", paste0(
      info$Labels[which(info$MLR == 1)], collapse = "; ")))
  }

  if(any(info$Include == 1)){
    inc = info$Labels[which(info$Include == 1)]
    if(length(inc) > 1){
      message(paste("Include: ", paste0(inc, collapse = "; ")))}
    else{message(paste0("Include: ", inc))}}

  load = paste(
    "```{r Load-Data}",
    paste0("load(file = ", "'", d_names, "'", ")"), sp,
    paste0("info = read.csv(", "'", info_names, "',", "header = T)"),
    "head(d); info",
    "```",sep = "\n")

  if(SLR == T & all(info$SLR == 0)){
    message("No Simple Regressions Specified")
    SLR = F}

  if(SLR == F){
    t = paste0("Report_",Sys.time(), ": Multiple Regression")
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 4), sp,
      load,
      sep = "\n")
    crr = create_corr_plot(info)
    lvls = create_relevel(info)
    mlr = paste0(create_multi_reg(info), collapse = "\n")
    content = paste(head, sp, crr, sp, lvls, mlr, sep = "\n")
    write(content, file = outfile)
  }else{
    t = paste0("Report_",Sys.time(), ": Multiple Regression + Simple Regression")
    message(paste0("SLR Outcome: ", paste0(
      info$Labels[which(info$SLR == 1)], collapse = "; ")))
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 5), sp,
      load,
      sep = "\n")
    crr = create_corr_plot(info)
    lvls = create_relevel(info)
    mlr = paste0(create_multi_reg(info), collapse = "\n")
    SLR = create_simple_reg(info)
    content = paste(head, sp, crr, sp, lvls, SLR, mlr, sep = "\n")
    write(content, file = outfile)}
}
# Generate a Time-to-Event analysis document
mk_TTE = function(author, SLR = F){
  message("Dir: ",getwd())
  info_names= (dir(pattern="info_"))
  info_names = info_names[[length(info_names)]]
  d_names = dir(pattern = ".RData")
  d_names = d_names[length(d_names)]
  message("Using info version: ", info_names)
  message("Using Data version: ", d_names)
  info = read.csv(info_names, header = T)
  sp = ""
  today = gsub(" ", ".",Sys.time())
  today = gsub(":", "-",today)
  outfile = paste0("TTE_Analysis_", today, ".Rmd")
  if(all(info$Time == 0)){
    return(message("Quitting: No Time variable specified"))}
  else if(all(info$Censor == 0)){
    return(message("Quitting: No Censor variable specified"))}
  else{
    message(paste0("Time ~ Censor= ",
                   info$Labels[which(info$Time == 1)], " ~ ",
                   info$Labels[which(info$Censor == 1)]))}

  load = paste(
    "```{r Load-Data}",
    paste0("load(file = ", "'", d_names, "'", ")"), sp,
    paste0("info = read.csv(", "'", info_names, "',", "header = T)"),
    "head(d); info",
    "```",sep = "\n")

  if(SLR == T & all(info$SLR == 0)){
    message("No Simple Regressions Specified")
    SLR = F}

  if(SLR == F){
    t = paste0("Report_",Sys.time(), ": Time to Event Analysis")
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 6), sp,
      load,
      sep = "\n")
    crr = create_corr_plot(info)
    lvls = create_relevel(info)
    srv = create_surv(info)
    content = paste(head, sp, crr, sp, lvls, srv, sep = "\n")
    write(content, file = outfile)
  }else{
    t = paste0("Report_",Sys.time(), ": Time to Event Analysis + Simple Regression")
    head = paste(
      .set_head(authorF = author, titleF = t), sp,
      .create_log(authorF = author, fname = t), sp,
      "# Setup",
      .set_global_options(), sp,
      .library_loader(call = 7), sp,
      load,
      sep = "\n")
    crr = create_corr_plot(info)
    lvls = create_relevel(info)
    srv = create_surv(info)
    SLR = create_simple_reg(info)
    content = paste(head, sp, crr, sp, lvls, SLR, srv, sep = "\n")
    write(content, file = outfile)}
}
# Installs below packages
mk_import = function(){
  pkg_list = c("bigBERD", "ggplot2","gtsummary","ggsci","corrplot","MASS",
               "forestmodel","jtools","pROC","survival","survminer","nnet")
  for(i in pkg_list){
    if(require(i)){
      message(paste0(i," already installed"))
    }
    else{
      install.packages(i, dependencies = T)
    }
  }
  message("All packages installed, re-run me after updating")
}
