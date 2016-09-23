
# The file contains functions related to the retrieving data from database at predictioncenter.org
# arguments:
# dbCon - data base connector, should be connected to database at predictioncenter.org
# casp - casp's round 

getQAModels <- function(dbCon, casp="casp12"){
  query <- paste0("
SELECT pr.id as qa_pr_id, pr.target || pr.pfrmat || 
  CASE WHEN gr.code < 10 THEN '00' || gr.code 
	  ELSE 
	    CASE WHEN gr.code < 100 THEN '0' || gr.code 
	    ELSE 
	     '' || gr.code 
	END 
	    END
	    || '_' || pr.model as model_name, 
   'QA'||
CASE WHEN gr.code < 10 THEN '00' || gr.code 
    ELSE 
	    CASE WHEN gr.code < 100 THEN '0' || gr.code 
	    ELSE 
	     '' || gr.code 
	END 
	    END  as qacode
	FROM ", casp, ".predictions pr
	JOIN ", casp, ".groups gr ON pr.groups_id=gr.id
	JOIN ", casp, ".targets t ON t.name=pr.target where pfrmat='QA' and model=2 and t.cancellation_qa=0
ORDER BY model_name",
                  sep=''
  );
  res <- dbSendQuery(con, query);
  #transform in data.frame
  as.data.frame(fetch(res, n=-1))
}

getTopTSmodel <- function(dbCon, QApr_id, casp="casp12"){
  query <- paste0("
    SELECT pr.target || pr.pfrmat || 
      CASE WHEN gr.code < 10 THEN '00' || gr.code 
        ELSE 
	        CASE WHEN gr.code < 100 THEN '0' || gr.code 
	          ELSE 
	          '' || gr.code 
	        END 
	    END
	    || '_' || pr.model as ts_model, pr.id as ts_pr_id, 
      qaa.predictions_id as qa_pr_id, mqas_score as mqas
      FROM ", casp,".qa_results qre 
      JOIN ", casp,".qa_analysis qaa ON qaa.id=qre.qa_analysis_id 
      JOIN ", casp, ".results re ON qre.results_id=re.id
      JOIN ", casp, ".predictions pr ON pr.id=re.predictions_id
      JOIN ", casp, ".groups gr ON pr.groups_id=gr.id
      WHERE qaa.predictions_id IN (", QApr_id, ") ORDER BY mqas_score DESC LIMIT 5 
  ");
  res <- dbSendQuery(dbCon, query);
  #transform in data.frame
  as.data.frame(fetch(res, n=-1));
}

getAllResults <- function(dbCon, casp="casp12"){
  query <- paste0("
          SELECT pr.id AS ts_pr_id, pr.target, 
          'TS' || CASE WHEN gr.code < 10 THEN '00' || gr.code 
           ELSE 
             CASE WHEN gr.code < 100 THEN '0' || gr.code 
	           ELSE 
	            '' || gr.code 
	           END 
	        END AS code, gr.type AS gr_type, 
          pr.model, re.domain, d.domain_classifications_id AS dci, 
          CASE WHEN re.gdt_4_z_score_all>-2.0 THEN re.gdt_4_z_score_all ELSE -2.0 END AS zscore, 
          re.gdt_ts_4 AS gdt_ts,
          t.is_server_only,
          CASE WHEN gr.type = 0 THEN 'TS(h)' ELSE 'TS(s)' END AS grType
          FROM casp12.results re
          JOIN casp12.predictions pr ON re.predictions_id=pr.id
          JOIN casp12.groups gr ON gr.id=pr.groups_id
          JOIN casp12.targets t ON t.name=pr.target
          JOIN casp12.domains d ON t.id=d.targets_id AND d.index=re.domain
          WHERE re.domain<7 AND re.n1_4>19 AND t.cancellation_status=0 AND pr.target SIMILAR TO 'T0%'
          ORDER BY pr.target, re.domain, gr.code, pr.model
                  "
    );
  res <- dbSendQuery(dbCon, query);
  #transform in data.frame
  as.data.frame(fetch(res, n=-1));
}

# generate summary based on best model
genSummary.best <- function(ts.data, qa.data, server_only=0, dom.class = c(3), gr_type_ = 1){
  ts.summary <- ts.data %>% filter(is_server_only <= server_only, dci %in% dom.class, gr_type >= gr_type_) %>% 
    group_by(target, code, domain, dci, grtype) %>% 
    summarise(zscore=max(zscore), gdt_ts=max(gdt_ts) ) %>% 
    group_by(code, grtype) %>% 
    summarise(sum_z = sum(zscore), mean_z = mean(zscore), sum_gdt_ts = sum(gdt_ts), mean_gdt_ts=mean(gdt_ts), no_doms=n())
  qa.summary <- qa.data %>% filter(is_server_only <= server_only, dci %in% dom.class, gr_type >= gr_type_) %>% 
    group_by(target, qacode, domain, dci, grtype) %>% 
    summarise(zscore=max(zscore), gdt_ts=max(gdt_ts)) %>% 
    group_by(qacode) %>% 
    summarise(sum_z = sum(zscore), mean_z = mean(zscore), sum_gdt_ts = sum(gdt_ts), mean_gdt_ts=mean(gdt_ts), no_doms=n())
  qa.summary$grtype <- 'QA'
  # rename the column name to be able to combine two tables
  # the headers of both tables have to be the same
  colnames(qa.summary)[1] <- "code"
  # combine the summary tables and sort by mean_z
  mix.summary <- arrange(union(qa.summary, ts.summary), desc(mean_z))
  dom_class <- ""
  if (2 %in% dom.class){
    dom_class <- paste0(dom_class,"+","TBM")
  }
  if (3 %in% dom.class){
    dom_class <- paste0(dom_class,"+","FM")
  }
  if (4 %in% dom.class){
    dom_class <- paste0(dom_class,"+","FM/TBM")
  }
  dom_class <- substr(dom_class, 2, nchar(dom_class))
  mix.summary$domClass <- dom_class
  mix.summary$modelMode <- 'best'
  mix.summary <- mix.summary[mix.summary$no_doms>0.5*max(mix.summary$no_doms), ]
  mix.summary
}

# generate summary based on first model
# in case of the ties for QA predictions the average values are calculated
genSummary.first <- function(ts.data, qa.data, server_only=0, dom.class = c(3), gr_type_ = 1){
  ts.summary <- ts.data %>% filter(is_server_only <= server_only, dci %in% dom.class, gr_type >= gr_type_, model==1) %>% 
    group_by(target, code, domain, dci, grtype) %>% 
    summarise(zscore=max(zscore), gdt_ts=max(gdt_ts) ) %>% 
    group_by(code, grtype) %>% 
    summarise(sum_z = sum(zscore), mean_z = mean(zscore), sum_gdt_ts = sum(gdt_ts), mean_gdt_ts=mean(gdt_ts), no_doms=n())
  qa.summary <- qa.data %>% filter(is_server_only <= server_only, dci %in% dom.class, gr_type >= gr_type_) %>% 
    group_by(target, qacode, domain, dci, grtype) %>% filter(mqas == max(mqas)) %>%
    summarise(zscore=mean(zscore), gdt_ts=mean(gdt_ts)) %>% 
    group_by(qacode) %>% 
    summarise(sum_z = sum(zscore), mean_z = mean(zscore), sum_gdt_ts = sum(gdt_ts), mean_gdt_ts=mean(gdt_ts), no_doms=n())
  qa.summary$grtype <- 'QA'
  # rename the column name to be able to combine two tables
  # the headers of both tables have to be the same
  colnames(qa.summary)[1] <- "code"
  # combine the summary tables and sort by mean_z
  mix.summary <- arrange(union(qa.summary, ts.summary), desc(mean_z))
  dom_class <- ""
  if (2 %in% dom.class){
    dom_class <- paste0(dom_class,"+","TBM")
  }
  if (3 %in% dom.class){
    dom_class <- paste0(dom_class,"+","FM")
  }
  if (4 %in% dom.class){
    dom_class <- paste0(dom_class,"+","FM/TBM")
  }
  dom_class <- substr(dom_class, 2, nchar(dom_class))
  mix.summary$domClass <- dom_class
  mix.summary$modelMode <- 'first'
  mix.summary <- mix.summary[mix.summary$no_doms>0.5*max(mix.summary$no_doms), ]
  mix.summary
}

my.plot<-function(ts.data, qa.data, gr_type_=1, model_mode_ = 'first'){
  if (gr_type_ >=1 ){
    server_only = 1;
  } else if (gr_type_ == 0){
    server_only = 0;
  }
  tmp <- NULL
  list.dom.classes <- list(c(3), c(3,4), c(2), c(2,4), c(2,3,4))
  for (dom.class in list.dom.classes){
    if (model_mode_ == 'first'){
      df <- genSummary.first(ts.data, qa.data, server_only, dom.class, gr_type_)
    } else {
      df <- genSummary.best(ts.data, qa.data, server_only, dom.class, gr_type_)
    }
    if (is.null(tmp)){
      #tmp$code <- factor(df$code, levels=df$code[order(df$mean_z)])
      tmp <- df[1:20,]
    } else {
      tmp <- rbind(tmp, df[1:20,])
    }
    tmp$code <- factor(tmp$code, levels = tmp$code[order(tmp$domClass, tmp$mean_z)])    
  }
  p <- ggplot(tmp, aes(x=code, y=mean_z, fill=grtype)) + 
    geom_bar(stat="identity") + coord_flip() + 
    facet_grid(. ~ domClass, scales="free")
  print(p)
}

my.plot2<-function(ts.data, qa.data, gr_type_=1, model_mode_ = 'first'){
  if (gr_type_ >=1 ){
    server_only = 1;
  } else if (gr_type_ == 0){
    server_only = 0;
  }
  color_vals <- c('QA'="#F8766D", 'TS(h)'="#00BA38", 'TS(s)'="#00BFC4")
  list.dom.classes <- list(c(3), c(3,4), c(2), c(2,4), c(2,3,4))
  pp <- rep(NULL, length(list.dom.classes))
  for (i in 1:length(list.dom.classes)){
    dom.class <- list.dom.classes[[i]]
    if (model_mode_ == 'first'){
      df <- genSummary.first(ts.data, qa.data, server_only, dom.class, gr_type_)
    } else {
      df <- genSummary.best(ts.data, qa.data, server_only, dom.class, gr_type_)
    }
    df$code <- factor(df$code, levels=df$code[order(df$mean_z)])
    df <- df[1:20,]
    p <- ggplot(df, aes(x=code, y=mean_z, fill=grtype)) + 
      geom_bar(stat="identity") + coord_flip() + 
      facet_grid(. ~ domClass, scales="free")  
    if (i == 1){
      p1 <- p +  ylab('Z(gdt_ts)') + theme_fivethirtyeight() +
        theme(legend.position="none", axis.title = element_blank()) + 
        scale_fill_manual(values=color_vals)
    } else if (i == 2){
      p2 <- p + ylab('Z(gdt_ts)')+ theme_fivethirtyeight() + 
        theme(legend.position="none" , axis.title = element_blank())+ 
        scale_fill_manual(values=color_vals)
    } else if (i == 3){
      p3 <- p + ylab('Z(gdt_ts)')+ theme_fivethirtyeight() + 
        theme(legend.position="none" , axis.title = element_blank())+ 
        scale_fill_manual(values=color_vals)
    } else if (i == 5 ){
      p5 <- p + ylab('Z(gdt_ts)')+ theme_fivethirtyeight() + 
        theme(legend.justification=c(1,0), legend.position=c(1,0), 
              legend.title=element_blank(), 
              legend.background = element_rect(fill="white", size=1, linetype="solid"),
              axis.title = element_blank(),
              legend.direction='vertical'
              )+ 
        scale_fill_manual(values=color_vals)
    }
  }
  print(multiplot(p1, p2, p3, p5, cols=4))
}

