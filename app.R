library(shiny)
library(DT)
library(ggplot2)
options(shiny.maxRequestSize=30*1024^2)
root <- normalizePath(getwd(), winslash='/')
readcsv <- function(p) read.csv(p, check.names=FALSE, stringsAsFactors=FALSE, colClasses=c(po_id='character',item_id='character'), na.strings=c('', 'NA'), fileEncoding='UTF-8-BOM')
readplain <- function(p) read.csv(p,check.names=FALSE,stringsAsFactors=FALSE,fileEncoding='UTF-8-BOM')
evidence <- function(area,name) { p <- file.path(root,'evidence',area,'tables',paste0(name,'.csv')); validate(need(file.exists(p),paste('Supporting evidence missing:',basename(p),'. Extract the complete application folder.'))); readplain(p) }
python <- Sys.getenv('SUPPLY_PYTHON', unset=if (.Platform$OS.type=='windows') 'python' else 'python3')
fmt <- function(x,d=2) if(length(x)==0 || !is.finite(x)) '—' else formatC(x,digits=d,format='f',big.mark=',')
kpi <- function(label,value) div(class='kpi',div(class='kpi-label',label),div(class='kpi-value',value))
card <- function(...) div(class='panel-card',...)
static_table <- function(id) DTOutput(id)
ui <- navbarPage(title='Supply Chain Analytics | WORKFLOW',id='nav',inverse=TRUE,
 header=tagList(tags$head(tags$style(HTML('
 body{background:#f3f6f9;color:#183247;font-family:Arial,sans-serif} .navbar-inverse{background:#17374f;border:0}.navbar-inverse .navbar-brand,.navbar-inverse .navbar-nav>li>a{color:#ecf5fb}.panel-card{background:white;border:1px solid #dce4eb;border-radius:8px;padding:18px;margin-bottom:16px}.kpi{display:inline-block;vertical-align:top;min-width:170px;background:white;padding:15px 23px;margin:5px 10px 10px 0;border-radius:7px;border-top:4px solid #138579;box-shadow:0 2px 5px #dce4eb}.kpi-label{font-size:12px;color:#53697b}.kpi-value{font-size:27px;font-weight:bold;color:#17374f}.note{background:#e8f2f6;border-left:4px solid #23858b;padding:12px;margin-bottom:12px}h2,h3{font-weight:600} .btn-primary{background:#137b75;border-color:#137b75} .dataTables_wrapper{font-size:12px}
 '))),div(class='note',strong('CAPSTONE RESEARCH • SUPPLY CHAIN INTELLIGENCE • SYNTHETIC DATA'), ' | Frozen baseline plus optional, session-only adaptive model refresh; no live SAP connection.')),
 tabPanel('1 · Overview',fluidRow(column(12,h2('Analytics workflow'),p('Predictive and prescriptive lead-time analytics for resilient German manufacturing.'),uiOutput('overview'),card(h3('Source and evidence'),p('Start with Scoring.csv for forecasts. Use Test.csv to compare observed and predicted outcomes. Use Data intake to change the active batch. The historical evidence in the model tabs stays fixed; dynamic charts and order predictions use the current batch only.'),p('Workflow: validate the input → explore data → supplier intelligence → frozen model predictions → uncertainty → optional decision simulation → export.'))))),
 tabPanel('2 · Data intake',fluidRow(column(4,card(h3('Select dataset'),selectInput('source','Bundled batch',c('Scoring','Upload_Example','Train','Validation','Test','All_Orders')),radioButtons('input_kind','Data source',c('Bundled dataset'='bundled','My CSV file'='upload')),fileInput('upload','Browse: purchase order CSV (e.g. Scoring.csv)',accept=c('.csv','text/csv')),downloadButton('upload_example','Download sample order CSV'),p('Use Upload_Example.csv to try 12 open orders quickly. Use Scoring.csv for new orders; Test.csv for observed outcomes; Train.csv or Validation.csv for historical exploration; All_Orders.csv for full exploration. Do not upload supplier master, external signals, dictionary or exported predictions. Select My CSV file to use Browse.'),checkboxInput('decision','Run prescriptive simulation (max. 1,000 rows)',FALSE),actionButton('run','Validate and score',class='btn-primary'),hr(),verbatimTextOutput('status'),p('Browse selects the upload option automatically. To return to built-in data, select Bundled dataset. A successful run loads the new batch; a rejected run clears previous results.'))),column(8,card(h3('Batch preview'),DTOutput('preview')),card(h3('Data dictionary'),DTOutput('dictionary'))))),
 tabPanel('3 · Quality & EDA',fluidRow(column(12,h2('Quality and exploratory analysis'),p('Choose a feature to compare its distribution and transport groups. Missing outcomes in open orders are expected.'),uiOutput('qualitykpi'),card(h3('Current batch missingness and cardinality'),DTOutput('quality')),fluidRow(column(6,card(h3('Planned and observed lead times'),plotOutput('hist'))),column(6,card(h3('Orders by transport mode'),plotOutput('transport')))),card(h3('Explore a numerical feature'),selectInput('eda_feature','Feature',c('planned_lead_days','actual_lead_days','order_qty','daily_demand','inventory_on_hand','port_congestion','geopolitical_risk','weather_severity','predicted_lead_days')),fluidRow(column(6,plotOutput('featurehist')),column(6,plotOutput('featurebox')))),card(h3('Historical data quality checks'),DTOutput('q1'))))),
 tabPanel('4 · Unsupervised models',fluidRow(column(12,h2('Unsupervised learning'),p('Clusters are frozen supplier assignments, not quality ratings. Unknown suppliers are not assigned a cluster. Anomalies indicate unusual inputs, not delay probabilities.'),uiOutput('unsupkpi'),fluidRow(column(6,card(h3('Current batch anomalies'),plotOutput('anomaly'))),column(6,card(h3('Historical supplier PCA'),plotOutput('pca',hover=hoverOpts('pca_hover',delay=150)),textOutput('pca_hover_text')))),card(h3('PCA variance and cluster diagnostics'),p('Historical training evidence: choose a component or inspect the silhouette curve. Higher silhouette suggests better separation; it does not establish supplier quality.'),fluidRow(column(6,plotOutput('scree')),column(6,plotOutput('silhouette'))),selectInput('pc','PCA component',paste0('PC',1:8)),plotOutput('loadings'),plotOutput('clusterbars')),card(h3('Hierarchical clustering (Ward linkage)'),p('Historical supplier dendrogram; nearby branches indicate similar standardised profiles.'),tags$img(src='ward_dendrogram.png',alt='Ward hierarchical clustering of suppliers',style='width:100%;max-width:1000px;height:auto;')),card(h3('Historical cluster profiles'),DTOutput('clusters')),card(h3('Historical cluster selection'),DTOutput('clusterselect')),card(h3('Current anomalous orders'),DTOutput('anomalytable'))))),
 tabPanel('5 · Supervised models',fluidRow(column(12,h2('Regression and classification'),p('Historical model comparisons; prediction outputs reflect the current batch. Ridge and logistic regression are deployed.'),fluidRow(column(6,card(h3('Regression validation and test evidence'),DTOutput('regmetrics'))),column(6,card(h3('Classification validation and test evidence'),DTOutput('classmetrics')))),card(h3('Feature importance'),p('Historical permutation importance: greater increase in loss after shuffling means greater predictive contribution, not causality.'),sliderInput('top_features','Number of features',5,23,10),selectInput('importance_kind','Outcome',c('Lead time'='regression_importance','Late delivery'='classification_importance')),plotOutput('importance'),DTOutput('importance_table')),card(h3('Validation model comparisons'),fluidRow(column(6,plotOutput('regbar')),column(6,plotOutput('classbar')))),card(h3('Current batch descriptive performance (only labelled rows)'),p('Select Test.csv or another labelled order dataset in Data intake to enable these metrics. The threshold below changes binary classifications, not probability estimates.'),sliderInput('eval_threshold','Evaluation probability threshold',0,1,.5,.05),p('Training or previously used rows are not independent evaluation. New labelled batches may be evaluated after checking their provenance.'),DTOutput('currentmetrics'),fluidRow(column(6,plotOutput('confusion')),column(6,plotOutput('residuals')))),card(h3('Historical classification discrimination'),p('ROC curve from the original held-out test records. It remains fixed for new uploads.'),plotOutput('roc_test')),fluidRow(column(6,card(h3('Observed vs predicted'),plotOutput('scatter',hover=hoverOpts('scatter_hover',delay=150)),textOutput('scatter_hover_text'))),column(6,card(h3('Delay risk distribution'),plotOutput('risthist'))))))),
 tabPanel('6 · Uncertainty',fluidRow(column(12,h2('Probabilistic forecasting'),p('Choose an 80% or 90% nominal interval and an alert threshold. Higher interval coverage generally makes intervals wider. Calibrated probabilities are an alternative, not a proven improvement.'),fluidRow(column(4,selectInput('level','Nominal interval',c('80','90'),selected='90')),column(4,selectInput('prob','Probability version',c('Original'='raw_late_probability','Calibrated'='calibrated_late_probability'))),column(4,sliderInput('threshold','Alert threshold',0,1,.75,.05))),uiOutput('riskkpi'),card(h3('Prediction intervals • sample up to 50 orders'),plotOutput('intervals')),card(h3('Historical coverage and probability reliability'),fluidRow(column(6,plotOutput('coverage')),column(6,plotOutput('reliability')))),card(h3('Historical interval evaluation'),DTOutput('intmetrics')),card(h3('Historical calibration comparison'),DTOutput('calmetrics'))))),
 tabPanel('7 · Predictions',fluidRow(column(12,h2('Order-level predictions'),p('Filter by transport or show only orders above the alert threshold selected in Uncertainty. Search and sort the table, then export the filtered or complete results.'),fluidRow(column(4,selectInput('mode','Transport filter',c('All','Air','Road','Rail','Sea'))),column(4,checkboxInput('alerts','Only orders above alert threshold',FALSE))),card(DTOutput('predictions')),downloadButton('download','Download full scored batch'),downloadButton('download_filtered','Download filtered predictions')))),
 tabPanel('8 · Decision evidence',fluidRow(column(12,h2('Prescriptive analytics'),p('Historical simulation results are fixed evidence. Run the optional simulation in Data intake for current-batch recommendations; it assumes independent orders and simulated costs, not actual savings.'),card(h3('Procurement policy comparison'),p('Historical simulations, separate from uploaded orders. Choose a cohort and stress scenario.'),selectInput('policy_split','Evidence cohort',c('New orders'='scoring','Historical test (base only)'='test')),selectInput('policy_stress','Disruption scenario',c('Base'='0','Moderate'='1','Severe'='2')),fluidRow(column(6,plotOutput('policycost')),column(6,plotOutput('policyrisk'))),DTOutput('policy')),card(h3('Current batch recommendations'),p('In Data intake choose Scoring.csv and tick Run prescriptive simulation before scoring.'),uiOutput('decisionkpi'),plotOutput('decisionplot'),DTOutput('decisions')),downloadButton('decisiondownload','Download current decisions')))),

 tabPanel('10 · Adaptive model refresh',fluidRow(column(12,
  h2('One-file adaptive training and risk prioritisation'),
  p('Upload ONE complete order-level CSV, including completed orders with actual outcomes. Choose how to split the labelled records; open orders are scored but never used for training or evaluation.'),
  fluidRow(column(4,card(
   fileInput('adaptive_file','Upload consolidated order dataset (.csv)',accept='.csv'),
   downloadButton('adaptive_example','Download example complete dataset'),
   sliderInput('adaptive_train','Training percentage',min=10,max=80,value=70,step=5),
   sliderInput('adaptive_validation','Validation percentage',min=10,max=80,value=15,step=5),
   uiOutput('adaptive_test_pct'),
   selectInput('adaptive_method','Split method',c('Chronological (recommended for evolving data)'='chronological','Random (exploratory)'='random')),
   actionButton('adaptive_go','Validate, split, retrain and refresh',class='btn-primary'),
   verbatimTextOutput('adaptive_status'))),
  column(8,card(h3('How results are updated'),p('The candidate Ridge and logistic models are fitted using only training data from this upload. Validation estimates prediction-interval width; the test set is reserved for evaluation. The same uploaded file supplies any open orders for prediction.'),p('The historical model comparisons and procurement optimisation remain unchanged. The refreshed risk queue below is advisory, not a reoptimised procurement policy.'),DTOutput('adaptive_metrics'),uiOutput('adaptive_summary')))),
  card(h3('Updated order-level risk and suggested human review'),DTOutput('adaptive_predictions'),downloadButton('adaptive_download','Download updated predictions and priorities')))))
, tabPanel('9 · Exports & guidance',fluidRow(column(12,h2('Export and reproducibility'),card(p('Download the complete scored batch or quality report. The exported scored data preserve order keys and include point forecasts, uncertainty, anomalies, and optional recommendations.'),downloadButton('download2','Download full scored batch'),downloadButton('qualitydownload','Download quality report'),downloadButton('dictdownload','Download schema dictionary')),card(h3('Governance'),p('Use only authorised synthetic or approved enterprise data. Do not upload confidential procurement records to a public server. The application does not update SAP IBP or place orders. Frozen models do not learn from uploads; retraining requires a separate validated workflow.'),p('Required columns and date formats are defined in the dictionary; outcomes may be absent for open orders. Unseen categories are flagged.')))))
)
server <- function(input,output,session){

 # -------------------------------------------------------------------------
 # Adaptive model refresh: single upload, configurable non-overlapping splits.
 # This deliberately does not overwrite approved frozen models or policies.
 # -------------------------------------------------------------------------
 adaptive_state <- reactiveVal(NULL)
 adaptive_message <- reactiveVal('No refreshed model yet. Upload a labelled order CSV.')
 output$adaptive_status <- renderText(adaptive_message())
 output$adaptive_test_pct <- renderUI({
   remaining <- 100 - input$adaptive_train - input$adaptive_validation
   div(style='margin-top:26px;',strong(sprintf('Testing: %s%%',remaining)),
       if(remaining < 10 || remaining > 80) span(style='color:#b04532;',
         ' Adjust training/validation so testing is 10–80%.'))
 })
 observeEvent(input$adaptive_go, {
   adaptive_state(NULL)
   if(is.null(input$adaptive_file)) {
     adaptive_message('Select ONE complete order CSV first.');return()
   }
   tr <- input$adaptive_train
   va <- input$adaptive_validation
   te <- 100 - tr - va
   if(te < 10 || te > 80) {
     adaptive_message('Invalid split: testing must be between 10% and 80%.');return()
   }
   if(!nzchar(Sys.which(python)) && !file.exists(python)) {
     adaptive_message('Python not found. Set SUPPLY_PYTHON to the working Python executable.');return()
   }
   folder <- tempfile('adaptive_session_')
   dir.create(folder)
   # Keep output files until the next refresh/session end; Shiny holds their paths.
   tryCatch({
     withProgress(message='Validating data and retraining candidate models',value=.1,{
       args <- c(shQuote(file.path(root,'backend','adaptive_training.py')),
                 shQuote(input$adaptive_file$datapath),shQuote(folder),
                 '--train',as.character(tr),'--validation',as.character(va),
                 '--method',input$adaptive_method)
       msg <- suppressWarnings(system2(python,args,stdout=TRUE,stderr=TRUE))
       code <- attr(msg,'status')
       if(!is.null(code) && code!=0) stop(paste(msg,collapse='\n'))
       incProgress(.8)
       met <- read.csv(file.path(folder,'adaptive_metrics.csv'),check.names=FALSE)
       pr <- readcsv(file.path(folder,'adaptive_predictions.csv'))
       adaptive_state(list(metrics=met,predictions=pr,folder=folder))
       adaptive_message(paste('Model updated in this session.',paste(msg,collapse='\n'),
                              'No GitHub files or historic policy simulations were changed.',sep='\n'))
     })
   },error=function(e){
       adaptive_message(paste('Refresh not completed:',conditionMessage(e)))
       showNotification('Adaptive model refresh failed; inspect the status.',type='error')
   })
 })
 output$adaptive_metrics <- renderDT({
   req(adaptive_state())
   DT::datatable(adaptive_state()$metrics,rownames=FALSE,
                 options=list(pageLength=5,scrollX=TRUE))
 })
 output$adaptive_summary <- renderUI({
   req(adaptive_state())
   p(sprintf('Forecasts refreshed for %s records; %s were open orders.',
      nrow(adaptive_state()$predictions),
      sum(adaptive_state()$predictions$partition=='Open / unlabelled')))
 })
 output$adaptive_predictions <- renderDT({
   req(adaptive_state())
   x <- adaptive_state()$predictions
   x <- x[order(-x$adaptive_late_probability),]
   show <- intersect(c('po_id','item_id','partition','supplier_id','transport_mode',
     'adaptive_predicted_lead_days','adaptive_late_probability','adaptive_lower_90',
     'adaptive_upper_90','risk_priority','available_inventory_days',
     'indicative_next_step'),names(x))
   DT::datatable(x[,show,drop=FALSE],rownames=FALSE,
     options=list(pageLength=10,scrollX=TRUE))
 })
 output$adaptive_download <- downloadHandler(
   filename=function() paste0('adaptive_priorities_',Sys.Date(),'.csv'),
   content=function(file){req(adaptive_state());
     file.copy(file.path(adaptive_state()$folder,'adaptive_predictions.csv'),
               file,overwrite=TRUE)}
 )
 output$adaptive_example <- downloadHandler(
   filename=function() 'All_Orders_example.csv',
   content=function(file) file.copy(file.path(root,'data','All_Orders.csv'),
                                    file,overwrite=TRUE)
 )

 result <- reactiveVal(NULL); quality <- reactiveVal(NULL); current <- reactiveVal('None'); status <- reactiveVal('Select a batch and click Validate and score.');
 perform <- function(path,label,dec=FALSE){
  result(NULL);quality(NULL);current('None');status('Running validation and frozen-model scoring…')
  td <- tempfile('supply_batch_');dir.create(td);on.exit(unlink(td,recursive=TRUE),add=TRUE)
  tryCatch({
   if(!file.exists(path)) stop('Input file missing. Extract the complete application folder.'); if(!nzchar(Sys.which(python)) && !file.exists(python)) stop('Python interpreter not found. Configure SUPPLY_PYTHON using the installation guide.'); withProgress(message='Validating and scoring orders',value=.2,{
    args <- c(shQuote(file.path(root,'backend','run_batch.py')),shQuote(path),shQuote(td),if(dec) '--decisions')
    msg <- suppressWarnings(system2(python,args,stdout=TRUE,stderr=TRUE)); code <- attr(msg,'status');
    if(!is.null(code) && code!=0) stop(paste(msg,collapse='\n'))
    incProgress(.7); z <- readcsv(file.path(td,'results.csv')); q <- readplain(file.path(td,'quality.csv'))
    result(z);quality(q);current(label);status(paste(msg,collapse='\n'))
   })
  },error=function(e){status(paste('Batch rejected:',conditionMessage(e)));showNotification('Batch not loaded. See status in Data intake.',type='error',duration=8)})
 }
 observeEvent(input$upload,{updateRadioButtons(session,'input_kind',selected='upload')},ignoreNULL=TRUE)
 observeEvent(input$run,{ if(input$input_kind=='bundled') perform(file.path(root,'data',paste0(input$source,'.csv')),input$source,input$decision) else { if(is.null(input$upload)){result(NULL);quality(NULL);current('None');status('Choose a CSV using Browse before scoring.');return()};perform(input$upload$datapath,input$upload$name,input$decision) } })
 observeEvent(TRUE,{perform(file.path(root,'data','Scoring.csv'),'Scoring',FALSE)},once=TRUE)
 d <- reactive({validate(need(!is.null(result()),'No successfully scored batch.'));result()})
 output$status <- renderText(status())
 output$overview <- renderUI({x<-d();tagList(kpi('Active batch',current()),kpi('Orders',nrow(x)),kpi('Suppliers',length(unique(x$supplier_id))),kpi('Mean forecast / days',fmt(mean(x$predicted_lead_days))),kpi('Mean delay probability',paste0(fmt(100*mean(x$raw_late_probability),1),'%')))})
 dt <- function(x) datatable(x,rownames=FALSE,options=list(pageLength=10,scrollX=TRUE),filter='top')
 output$preview <- renderDT({x<-d();dt(head(x[,intersect(readplain(file.path(root,'data','Data_Dictionary.csv'))$Variable,names(x)),drop=FALSE],100))})
 output$dictionary <- renderDT(dt(readplain(file.path(root,'data','Data_Dictionary.csv'))))
 output$quality <- renderDT({req(quality());dt(quality())})
 output$qualitykpi <- renderUI({x<-d();tagList(kpi('Validated orders',nrow(x)),kpi('Labelled orders',sum(!is.na(x$actual_lead_days))),kpi('Unseen categories',sum(as.logical(x$unseen_category),na.rm=TRUE)),kpi('Unknown supplier cluster',sum(is.na(x$supplier_cluster))))})
 output$hist <- renderPlot({x<-d();g<-ggplot(x,aes(planned_lead_days))+geom_histogram(bins=30,fill='#3979a8',alpha=.7);if(any(!is.na(x$actual_lead_days)))g<-g+geom_histogram(aes(actual_lead_days),bins=30,fill='#cc914d',alpha=.5,na.rm=TRUE);g+labs(x='Lead time (days)',y='Orders',subtitle='Blue: planned; amber: actual when available')+theme_minimal()})
 output$transport <- renderPlot({ggplot(d(),aes(transport_mode))+geom_bar(fill='#147f79')+labs(x=NULL,y='Orders')+theme_minimal()})
 output$unsupkpi <- renderUI({x<-d();tagList(kpi('Flagged anomalies',sum(x$anomaly_flag)),kpi('Flagged rate',paste0(fmt(100*mean(x$anomaly_flag),1),'%')))})
 output$anomaly <- renderPlot({ggplot(d(),aes(anomaly_score))+geom_histogram(bins=30,fill='#3979a8')+geom_vline(xintercept=0,linetype=2)+theme_minimal()})
 output$pca <- renderPlot({ggplot(evidence('Supplier_Intelligence','supplier_profiles'),aes(PC1,PC2,colour=factor(cluster)))+geom_point(size=2)+labs(colour='Cluster')+theme_minimal()})
 output$anomalytable <- renderDT({x<-d();z<-x[x$anomaly_flag==1,c('po_id','item_id','supplier_id','transport_mode','anomaly_score','supplier_cluster')];dt(z[order(-z$anomaly_score),])})
 for(spec in list(c('q1','Exploratory_Analysis','quality_checks'),c('clusters','Supplier_Intelligence','cluster_profiles'),c('clusterselect','Supplier_Intelligence','cluster_selection'),c('regmetrics','Predictive_Modelling','regression_metrics'),c('classmetrics','Predictive_Modelling','classification_metrics'),c('intmetrics','Uncertainty_Quantification','interval_metrics'),c('calmetrics','Uncertainty_Quantification','calibration_metrics'))){local({s<-spec;output[[s[1]]]<-renderDT(dt(evidence(s[2],s[3])))})}
 output$currentmetrics <- renderDT({x<-d();x<-x[!is.na(x$actual_lead_days)&!is.na(x$late_flag),];validate(need(nrow(x)>0,'No outcomes available for this batch.'));dt(data.frame(n=nrow(x),MAE=mean(abs(x$actual_lead_days-x$predicted_lead_days)),RMSE=sqrt(mean((x$actual_lead_days-x$predicted_lead_days)^2)),Brier=mean((x$late_flag-x$raw_late_probability)^2),Accuracy=mean((x$raw_late_probability>=input$eval_threshold)==x$late_flag)))})
 output$scatter <- renderPlot({x<-d();x<-x[!is.na(x$actual_lead_days),];validate(need(nrow(x)>0,'No observed outcomes in this batch.'));ggplot(x,aes(actual_lead_days,predicted_lead_days))+geom_point(alpha=.3,colour='#147f79')+geom_abline(linetype=2)+labs(x='Observed days',y='Forecast days')+theme_minimal()})
 output$risthist <- renderPlot({ggplot(d(),aes(raw_late_probability))+geom_histogram(bins=25,fill='#3979a8')+theme_minimal()+labs(x='Original late-delivery probability',y='Orders')})
 output$riskkpi <- renderUI({x<-d();p<-x[[input$prob]];lo<-x[[paste0('group_lower_',input$level)]];hi<-x[[paste0('group_upper_',input$level)]];tagList(kpi('Alerts',sum(p>=input$threshold)),kpi('Mean interval width',paste(fmt(mean(hi-lo)),'days')),kpi('Mean delay risk',paste0(fmt(mean(p)*100,1),'%')))})
 output$intervals <- renderPlot({x<-d();x<-x[order(x$predicted_lead_days),];x<-x[unique(round(seq(1,nrow(x),length.out=min(50,nrow(x))))),];x$rank<-seq_len(nrow(x));x$lower<-x[[paste0('group_lower_',input$level)]];x$upper<-x[[paste0('group_upper_',input$level)]];ggplot(x,aes(rank,predicted_lead_days))+geom_linerange(aes(ymin=lower,ymax=upper),colour='#87a7c2')+geom_point(colour='#147f79')+labs(x='Selected orders (sorted by forecast)',y='Days')+theme_minimal()})
 filtered <- reactive({x<-d();if(input$mode!='All')x<-x[x$transport_mode==input$mode,];if(isTRUE(input$alerts))x<-x[x[[input$prob]]>=input$threshold,];x[order(-x[[input$prob]]),]})
 output$predictions <- renderDT({x<-filtered();dt(x[,c('po_id','item_id','supplier_id','transport_mode','planned_lead_days','predicted_lead_days',input$prob,paste0('group_lower_',input$level),paste0('group_upper_',input$level),'anomaly_flag','unseen_category')])})
 decdata <- reactive({x<-d();validate(need('action'%in%names(x),'Run optional decision simulation in Data intake.'));x[,c('po_id','item_id','supplier_id','action','buffer_units','cost','baseline_cost','estimated_savings','stockout_probability','baseline_stockout_probability')]})
 output$decisionkpi<-renderUI({x<-decdata();tagList(kpi('Expected cost / order',paste0('EUR ',fmt(mean(x$cost)))),kpi('Estimated savings / order',paste0('EUR ',fmt(mean(x$estimated_savings)))),kpi('Stockout exposure',paste0(fmt(100*mean(x$stockout_probability),1),'%')))})
 output$decisions<-renderDT(dt(decdata()))
 downloader<-function(name,fun)downloadHandler(filename=function()paste0(name,'_',Sys.Date(),'.csv'),content=function(file)write.csv(fun(),file,row.names=FALSE,na=''))
 output$download<-downloader('scored_orders',d);output$download2<-downloader('scored_orders',d);output$download_filtered<-downloader('filtered_orders',filtered);output$qualitydownload<-downloader('quality',function(){req(quality());quality()});output$upload_example<-downloadHandler(filename=function() 'Upload_Example.csv',content=function(file)file.copy(file.path(root,'data','Upload_Example.csv'),file,overwrite=TRUE));output$dictdownload<-downloader('data_dictionary',function()readplain(file.path(root,'data','Data_Dictionary.csv')));output$decisiondownload<-downloader('decisions',decdata)
 output$scree <- renderPlot({z<-evidence('Supplier_Intelligence','pca_variance');ggplot(z,aes(factor(component),100*variance_ratio))+geom_col(fill='#147f79')+labs(x='Principal component',y='Variance explained (%)')+theme_minimal()})
 output$silhouette <- renderPlot({z<-evidence('Supplier_Intelligence','cluster_selection');ggplot(z,aes(k,silhouette))+geom_line(colour='#147f79')+geom_point(size=3)+labs(x='Number of clusters',y='Silhouette score')+theme_minimal()})
 output$loadings <- renderPlot({z<-evidence('Supplier_Intelligence','pca_loadings');z$value<-z[[input$pc]];ggplot(z,aes(reorder(feature,value),value,fill=value>0))+geom_col()+coord_flip()+labs(x=NULL,y='Signed loading',title=input$pc)+theme_minimal()+theme(legend.position='none')})
 output$clusterbars <- renderPlot({z<-d();z$segment<-ifelse(is.na(z$supplier_cluster),'Unassigned',as.character(z$supplier_cluster));ggplot(z,aes(segment))+geom_bar(fill='#147f79')+labs(x='Supplier segment',y='Current orders')+theme_minimal()})
 imp<-reactive({z<-evidence('Predictive_Modelling',input$importance_kind);head(z[order(-z$importance),],input$top_features)})
 output$importance<-renderPlot({z<-imp();ggplot(z,aes(reorder(feature,importance),importance))+geom_col(fill='#147f79')+geom_errorbar(aes(ymin=importance-sd,ymax=importance+sd),width=.2)+coord_flip()+labs(x=NULL,y=if(input$importance_kind=='regression_importance') 'Increase in MAE (days)' else 'Increase in Brier score')+theme_minimal()})
 output$importance_table<-renderDT(dt(imp()))
 output$regbar<-renderPlot({z<-evidence('Predictive_Modelling','regression_metrics');z<-z[z$split=='Validation',];ggplot(z,aes(reorder(model,MAE),MAE))+geom_col(fill='#3979a8')+coord_flip()+labs(x=NULL,y='Validation MAE (lower is better)')+theme_minimal()})
 output$classbar<-renderPlot({z<-evidence('Predictive_Modelling','classification_metrics');z<-z[z$split=='Validation',];ggplot(z,aes(reorder(model,Brier),Brier))+geom_col(fill='#147f79')+coord_flip()+labs(x=NULL,y='Validation Brier score (lower is better)')+theme_minimal()})
 output$coverage<-renderPlot({z<-evidence('Uncertainty_Quantification','interval_metrics');z<-z[z$split=='Test' & z$transport=='All',];ggplot(z,aes(factor(nominal),100*coverage,fill=method))+geom_col(position='dodge')+labs(x='Nominal coverage',y='Observed coverage (%)')+theme_minimal()})
 output$reliability<-renderPlot({z<-evidence('Uncertainty_Quantification','reliability_bins');z<-z[z$split=='Test',];ggplot(z,aes(mean_probability,observed,colour=method))+geom_abline(linetype=2)+geom_line()+geom_point()+labs(x='Predicted probability',y='Observed late fraction')+theme_minimal()})
 pol<-reactive({z<-evidence('Prescriptive_Analytics','policy_comparison');z<-z[z$split==input$policy_split & z$stress==(if(input$policy_split=='test') 0L else as.integer(input$policy_stress)),];validate(need(nrow(z)>0,'No historical evidence for this cohort and scenario.'));z})
 observeEvent(input$policy_split,{choices<-if(input$policy_split=='test') c('Base'='0') else c('Base'='0','Moderate'='1','Severe'='2');updateSelectInput(session,'policy_stress',choices=choices,selected='0')})
 output$policy<-renderDT(dt(pol()))
 output$policycost<-renderPlot({ggplot(pol(),aes(policy,cost))+geom_col(fill='#147f79')+coord_flip()+labs(x=NULL,y='Expected EUR/order')+theme_minimal()})
 output$policyrisk<-renderPlot({ggplot(pol(),aes(policy,100*stockout_probability))+geom_col(fill='#c38a42')+coord_flip()+labs(x=NULL,y='Potential stockout (%)')+theme_minimal()})
 output$decisionplot<-renderPlot({ggplot(decdata(),aes(action))+geom_bar(fill='#147f79')+coord_flip()+labs(x=NULL,y='Current orders')+theme_minimal()})
 fe<-reactive({z<-d();z$value<-z[[input$eda_feature]];z<-z[is.finite(z$value),];validate(need(nrow(z)>0,'No observed values. Choose another feature or a labelled batch.'));z})
 output$featurehist<-renderPlot({ggplot(fe(),aes(value))+geom_histogram(bins=30,fill='#3979a8')+labs(x=input$eda_feature,y='Orders')+theme_minimal()})
 output$featurebox<-renderPlot({ggplot(fe(),aes(transport_mode,value))+geom_boxplot(fill='#b3d9d5')+labs(x='Transport',y=input$eda_feature)+theme_minimal()})

 labelled<-reactive({x<-d();x<-x[is.finite(x$actual_lead_days)&!is.na(x$late_flag),];validate(need(nrow(x)>0,'Observed outcomes unavailable. Load Test.csv or another labelled dataset.'));x})
 output$confusion<-renderPlot({x<-labelled();z<-as.data.frame(table(Observed=factor(x$late_flag,levels=0:1,labels=c('On time','Late')),Predicted=factor(as.integer(x$raw_late_probability>=input$eval_threshold),levels=0:1,labels=c('On time','Late'))));ggplot(z,aes(Predicted,Observed,fill=Freq))+geom_tile(colour='white')+geom_text(aes(label=Freq),size=5)+scale_fill_gradient(low='#e6f3f2',high='#147f79')+labs(title='Confusion matrix',fill='Orders')+theme_minimal()})
 output$residuals<-renderPlot({x<-labelled();x$residual<-x$actual_lead_days-x$predicted_lead_days;ggplot(x,aes(predicted_lead_days,residual))+geom_point(alpha=.25,colour='#3979a8')+geom_hline(yintercept=0,linetype=2)+labs(x='Predicted days',y='Observed minus predicted (days)',title='Regression residuals')+theme_minimal()})
 output$roc_test<-renderPlot({z<-evidence('Predictive_Modelling','test_predictions');z<-z[order(-z$late_probability),];ends<-c(which(diff(z$late_probability)!=0),nrow(z));r<-data.frame(FPR=c(0,cumsum(z$late_flag==0)[ends]/sum(z$late_flag==0)),TPR=c(0,cumsum(z$late_flag==1)[ends]/sum(z$late_flag==1)));ggplot(r,aes(FPR,TPR))+geom_path(colour='#147f79',linewidth=1)+geom_abline(linetype=2)+coord_equal()+labs(x='False positive rate',y='True positive rate')+theme_minimal()})
 output$pca_hover_text<-renderText({if(is.null(input$pca_hover))return('Hover over a point to identify the supplier.');z<-evidence('Supplier_Intelligence','supplier_profiles');q<-nearPoints(z,input$pca_hover,xvar='PC1',yvar='PC2',threshold=12,maxpoints=1);if(!nrow(q))return('Move closer to a point.');paste(q$supplier_id,'| Cluster',q$cluster,'|',q$default_transport_mode)})
 output$scatter_hover_text<-renderText({if(is.null(input$scatter_hover))return('Hover over a point to identify a purchase order.');z<-labelled();q<-nearPoints(z,input$scatter_hover,xvar='actual_lead_days',yvar='predicted_lead_days',threshold=12,maxpoints=1);if(!nrow(q))return('Move closer to a point.');paste(q$po_id,'/',q$item_id,'| observed',q$actual_lead_days,'days; forecast',fmt(q$predicted_lead_days),'days')})
}
shinyApp(ui,server)
