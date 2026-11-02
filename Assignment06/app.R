# BDA400 Assignment 6 - Portfolio Visualization Dashboard
# Student: Alia GhahremaniTabrizi
# Run: install.packages(c('shiny','ggplot2','quantmod','TTR')); shiny::runApp('.')
library(shiny)
library(ggplot2)
library(quantmod)
library(TTR)

ui <- fluidPage(
  titlePanel('Portfolio Technical Analysis Dashboard'),
  sidebarLayout(
    sidebarPanel(
      textInput('symbol','Yahoo Finance ticker','AAPL'),
      dateRangeInput('dates','Date range',start=Sys.Date()-730,end=Sys.Date()),
      selectInput('frequency','Time frame',c('Daily','Weekly','Monthly')),
      selectInput('chart','Price chart',c('Line','Area','Candlestick')),
      checkboxGroupInput('indicators','Technical indicators',c('Moving Averages','RSI','MACD'),selected=c('Moving Averages','RSI','MACD')),
      numericInput('short','Short moving average',20,min=2,max=150),
      numericInput('long','Long moving average',50,min=3,max=250),
      checkboxInput('annotations','Show crossover signals',TRUE),
      actionButton('refresh','Refresh Yahoo Finance data'),
      helpText('Educational signals only; not financial advice.')
    ),
    mainPanel(
      textOutput('status'),
      plotOutput('price',height='440px'),
      conditionalPanel("input.indicators.indexOf('RSI') >= 0",plotOutput('rsi',height='190px')),
      conditionalPanel("input.indicators.indexOf('MACD') >= 0",plotOutput('macd',height='190px')),
      h4('Recent observations'),tableOutput('recent'),
      h4('Trading signal definitions'),
      p('BUY: short MA crosses above long MA; SELL: short MA crosses below long MA; otherwise HOLD. Signals are computed using prior bars and marked only at crossover events.')
    )
  )
)
server <- function(input,output,session) {
  raw <- eventReactive(list(input$symbol,input$dates,input$refresh),{
    validate(need(nzchar(trimws(input$symbol)),'Enter a ticker symbol.'),
             need(input$dates[1] < input$dates[2],'Select a valid date range.'))
    tryCatch(getSymbols(toupper(trimws(input$symbol)),src='yahoo',
                        from=as.Date(input$dates[1])-400,
                        to=as.Date(input$dates[2])+1,auto.assign=FALSE),
             error=function(e) NULL)
  })
  observations <- reactive({
    x <- raw()
    validate(need(!is.null(x),'Unable to retrieve Yahoo Finance data. Check ticker, internet, and dates.'))
    x <- na.omit(x)
    validate(need(NROW(x)>2,'Not enough stock observations.'))
    if(input$frequency=='Weekly') x <- to.weekly(x,indexAt='endof',drop.time=TRUE)
    if(input$frequency=='Monthly') x <- to.monthly(x,indexAt='endof',drop.time=TRUE)
    # Indicators calculated before display filtering to preserve lookback history.
    close <- as.numeric(Cl(x))
    validate(need(input$short<input$long,'Short MA must be less than long MA.'),
             need(NROW(x)>input$long+2,'Insufficient history for long MA; reduce window or choose Daily.'))
    ma_s <- as.numeric(SMA(close,n=input$short))
    ma_l <- as.numeric(SMA(close,n=input$long))
    rsi <- as.numeric(RSI(close,n=14))
    macd <- MACD(close,nFast=12,nSlow=26,nSig=9,maType='EMA')
    diff <- ma_s-ma_l
    prev <- c(NA_real_,head(diff,-1))
    signal <- ifelse(!is.na(prev)&prev<=0&diff>0,'Buy',
                     ifelse(!is.na(prev)&prev>=0&diff<0,'Sell','Hold'))
    d <- data.frame(Date=as.Date(index(x)),Open=as.numeric(Op(x)),
                    High=as.numeric(Hi(x)),Low=as.numeric(Lo(x)),Close=close,
                    ShortMA=ma_s,LongMA=ma_l,RSI=rsi,
                    MACD=as.numeric(macd[,1]),MACDSignal=as.numeric(macd[,2]),Signal=signal)
    d <- d[d$Date>=as.Date(input$dates[1]) & d$Date<=as.Date(input$dates[2]),]
    validate(need(nrow(d)>0,'No observations in selected date range.'))
    d
  })
  output$status <- renderText({d<-observations();paste('Ticker:',toupper(input$symbol),'|',nrow(d),'bars | Latest:',format(tail(d$Date,1)))})
  output$price <- renderPlot({
    d<-observations()
    p<-ggplot(d,aes(x=Date))
    if(input$chart=='Line') p<-p+geom_line(aes(y=Close),colour='#2563eb',linewidth=.65)
    if(input$chart=='Area') p<-p+geom_area(aes(y=Close),fill='#60a5fa',alpha=.35)+geom_line(aes(y=Close),colour='#2563eb')
    if(input$chart=='Candlestick') {
      up<-d$Close>=d$Open
      p<-p+geom_segment(aes(y=Low,yend=High,xend=Date),colour='#64748b')+
        geom_segment(data=d[up,,drop=FALSE],aes(y=Open,yend=Close,xend=Date),colour='#16a34a',linewidth=3)+
        geom_segment(data=d[!up,,drop=FALSE],aes(y=Open,yend=Close,xend=Date),colour='#dc2626',linewidth=3)
    }
    if('Moving Averages'%in%input$indicators) p<-p+
      geom_line(aes(y=ShortMA,colour=paste0('SMA ',input$short)),linewidth=.7,na.rm=TRUE)+
      geom_line(aes(y=LongMA,colour=paste0('SMA ',input$long)),linewidth=.7,na.rm=TRUE)+
      scale_colour_manual(values=setNames(c('#eab308','#7c3aed'),c(paste0('SMA ',input$short),paste0('SMA ',input$long))))
    if(input$annotations) {
      sig<-d[d$Signal!='Hold',,drop=FALSE]
      if(nrow(sig)>0) p<-p+geom_point(data=sig,aes(y=Close,shape=Signal,colour=Signal),size=2.8)+
        geom_text(data=sig,aes(y=Close,label=Signal),vjust=-1,size=3,check_overlap=TRUE)
    }
    p+labs(title=paste(toupper(input$symbol),'price and trading signals'),x=NULL,y='Price (USD)',colour='Overlay')+
      theme_minimal(base_size=12)
  })
  output$rsi<-renderPlot({
    d<-observations()
    ggplot(d,aes(Date,RSI))+geom_line(colour='#7c3aed',na.rm=TRUE)+
      geom_hline(yintercept=c(30,70),linetype='dashed',colour='#dc2626')+
      coord_cartesian(ylim=c(0,100))+labs(title='RSI (14)',x=NULL,y='RSI')+theme_minimal()
  })
  output$macd<-renderPlot({
    d<-observations()
    ggplot(d,aes(Date))+geom_col(aes(y=MACD-MACDSignal),fill='#94a3b8',na.rm=TRUE)+
      geom_line(aes(y=MACD,colour='MACD'),na.rm=TRUE)+
      geom_line(aes(y=MACDSignal,colour='Signal'),na.rm=TRUE)+
      labs(title='MACD (12,26,9)',x=NULL,y='Value',colour=NULL)+theme_minimal()
  })
  output$recent<-renderTable({
    d<-tail(observations()[,c('Date','Close','ShortMA','LongMA','Signal')],10)
    d$Close<-round(d$Close,2);d$ShortMA<-round(d$ShortMA,2);d$LongMA<-round(d$LongMA,2)
    d
  })
}
shinyApp(ui,server)
