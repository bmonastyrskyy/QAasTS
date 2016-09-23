#!/usr/bin/Rscript

# Author : Bohdan Monastyrskyy
# Date : 2016-10-13
# Description : 
#   The project performs analysis of the QA groups
#   if they submitted TS predictions based on their MQA score.

# load CRAN libraries
libs <- c("RPostgreSQL", "dplyr", "ggplot2", "manipulate", "ggthemes");
for (l in libs){
  if (!require(l, character.only = TRUE)){
    tryCatch({
      install.packages(l)
      }, warning = function(w){
        print(paste0("There were problems with installing library", l, ". Check."));
      },
      error = function(e){
        stop(paste("The library", l, " couldn't be loaded"));  
      }, 
      finally = function(){
        # do nothing 
      });
  };
  tryCatch({require(l, character.only = TRUE)}, 
            warning = function(w){
              stop(paste0("There were problems with installing library", l, ". Check."));
            },
            error=function(e){
              stop(paste0("There were problems with installing library", l, ". Check."));
            },
            finally = function(){
              # do nothing
            });
}

# load config.R
# the variables related to database: HOST_IP, DB_USER, DB_NAME, CASP
source("config.R")

# load user-defined libraries
source("Functions.R");

# load utils
source("Utils.R")

# -------------------------------------------------------
# retrieve data from database
# -------------------------------------------------------
# set up database driver
drv <- dbDriver("PostgreSQL")

# set db connector
con <- dbConnect(drv, host=HOST_IP, user=DB_USER, 
                 dbname=DB_NAME)

qa.models <- getQAModels(con, CASP)
ts.models <- NULL
for (qa_pr_id in qa.models$qa_pr_id){
  if (is.null(ts.models)){
    ts.models <- getTopTSmodel(con, qa_pr_id);
  } else {
    ts.models <- rbind(ts.models, getTopTSmodel(con, qa_pr_id));
    #break;
  }
}

# get all TS results
ts.data <- getAllResults(con)
head(ts.data)

# join tables results and qa.models
qa.data <- inner_join(inner_join(ts.models, ts.data, by=("ts_pr_id")), qa.models, by=("qa_pr_id"))




# write 4 files for different sets of parameters#
#flag <- 'TBM_FM_TBMFM' # dom.class <- c(2,3,4)
#dom.class_ <- c(2,3,4)
 flag <- 'FM' # dom.class<-c(3)
 dom.class_ <- c(3)
# flag <- 'FM_FMTBM' # dom.class<-c(3,4)
# dom.class_ <- c(3,4)
# flag <- 'TBM' # dom.class<-c(2)
# dom.class_ <- c(2)
# flag <- 'TBM_FMTBM' # dom.class<-c(3,4)
# dom.class_ <- c(2,4)

# generate data for the best models
tmp.b <- genSummary.best(ts.data, qa.data, server_only=1, dom.class = dom.class_, gr_type=1)

write.table(tmp.b, paste0("./output/_", flag, ".QAasTS.csv"), quote = FALSE, row.names = FALSE, sep=",")
# generate data for the first models
tmp.f <- genSummary.first(ts.data, qa.data, server_only=1, dom.class = dom.class_, gr_type=1)

manipulate(my.plot2(ts.data, qa.data, gr_type_= groupMode, model_mode_ = modelMode), 
           modelMode=picker("first", 'best'), 
           groupMode = picker("all" = 0, "server_only" = 1))



