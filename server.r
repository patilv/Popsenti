# Installing package if not already installed (Stanton 2013)
EnsurePackage<-function(x)
{x <- as.character(x)
 if (!require(x,character.only=TRUE))
 {
   install.packages(pkgs=x,repos="http://cran.r-project.org")
   require(x,character.only=TRUE)
 }
}

#Identifying packages required  (Stanton 2013)
PrepareTwitter<-function()
{
  EnsurePackage("twitteR")
  EnsurePackage("stringr")
  EnsurePackage("ROAuth")
  EnsurePackage("RCurl")
  EnsurePackage("ggplot2")
  EnsurePackage("reshape")
  EnsurePackage("tm")
  EnsurePackage("wordcloud")
  EnsurePackage("gridExtra")
  #EnsurePackage("gplots") Not required... ggplot2 is used
  EnsurePackage("plyr")
}

PrepareTwitter()

load("credential") # A credential obtained from twitter permitting access to their data - A user will need this to proceed
# Please see http://cran.r-project.org/web/packages/twitteR/vignettes/twitteR.pdf for more info on this.

registerTwitterOAuth(credential)

# Function to create a data frame from tweets, Stanton 2013

shinyServer(function(input, output) {
  
  # Function to clean tweets, Stanton 2013
  CleanTweets<-function(tweets)
  {
    # Remove redundant spaces
    tweets <- str_replace_all(tweets," "," ")
    # Get rid of URLs
    tweets <- str_replace_all(tweets, "http://t.co/[a-z,A-Z,0-9]*{8}","")
    # Take out retweet header, there is only one
    tweets <- str_replace(tweets,"RT @[a-z,A-Z]*: ","")
    # Get rid of hashtags
    tweets <- str_replace_all(tweets,"#[a-z,A-Z]*","")
    # Get rid of references to other screennames
    tweets <- str_replace_all(tweets,"@[a-z,A-Z]*","")
    return(tweets)
    
  }
  
  #Search tweets and create a data frame -Stanton (2013)
  TweetFrame<-function(searchTerm, maxTweets)
  {
    twtList<-searchTwitter(searchTerm,n=maxTweets,cainfo="cacert.pem",lang="en")
    return(do.call("rbind",lapply(twtList,as.data.frame)))
    
  }
  
  # function to calculate number of tweets (input is text column, if the entire data frame was submitted, 
  #could've used nrow(), as done at a different place below)
  
  numoftweets<-function(entity1,entity2,entity1entry,entity2entry){
    ent1numtweets<-nrow(entity1)
    ent2numtweets<-nrow(entity2)
    notweets<-c(ent1numtweets,ent2numtweets)
    names(notweets)<-c(entity1entry,entity2entry)
    notweets
  } 
  
  #Determining porbability of a new tweet arriving within a particular time, t
  
  ArrivalProbability<-function(times, increment, max) # function from Jeffrey Stanton's book (2013)
  {
    # Initialize an empty vector
    plist <- NULL
    # Probability is defined over the size of this sample
    # of arrival times
    timeLen <- length(times)
    # May not be necessary, but checks for input mistake
    if (increment>max) {return(NULL)}
    for (i in seq(increment, max, by=increment))
    {
      
      plist<-c(plist,(sum(as.integer(diff(times))<i))/timeLen)
    }
    return(plist)
  }      
  
  #function calling the above arrival probability function and creating a data frame or arrival probabilities and time 
  #- Tweaking Stanton's algorithm --- The first few commands of this function are repeated in a few other functions below... 
  #Clearly, this can be cleaned up further.
  
  
  arrivalprob<-function(entity1,entity2,entity1entry,entity2entry){
    
    ent1sort<-entity1[order(as.integer(entity1$created)),]  #Ordering tweets based on arrival time
    ent2sort<-entity2[order(as.integer(entity2$created)),]  #Ordering tweets based on arrival time
    
    ent1diffcreated<-as.integer(diff(ent1sort$created)) #calculate the difference in seconds between each pair of neighboring values. 
    ent2diffcreated<-as.integer(diff(ent2sort$created))
    
    #creating a data frame with arrival probabilities for both univs
    arriveprobent1<-ArrivalProbability(ent1diffcreated,10,1000)
    arriveprobent2<-ArrivalProbability(ent2diffcreated,10,1000)
    
    bothdatatimes<-data.frame(entity1=arriveprobent1, entity2=arriveprobent2)
    names(bothdatatimes)<-c(entity1entry,entity2entry)# renaming columns to entries from user
    
    bothdatatimes$time<-c(1:nrow(bothdatatimes)) # creating an index variable
    arrivaldata<-melt(bothdatatimes,id.vars="time") #manipulating data frame to have it ready for ggplot2
    }
  
  # A function to create a data frame of delay between tweets, ordered based on arrival time ---Tweak of Stanton's algorithm  
  DelayTweets<-function(entity1,entity2,entity1entry,entity2entry){
  ###### Few repeated commands below
    ent1sort<-entity1[order(as.integer(entity1$created)),]  #Ordering tweets based on arrival time
    ent2sort<-entity2[order(as.integer(entity2$created)),]  #Ordering tweets based on arrival time
    
    ent1diffcreated<-as.integer(diff(ent1sort$created)) #calculate the difference in seconds between each pair of neighboring values 
    ent2diffcreated<-as.integer(diff(ent2sort$created))
    
    entity1diff<-data.frame(delay=ent1diffcreated)
    entity1diff$entity<-c(entity1entry) #creating a factor variable for Entity
    
    entity2diff<-data.frame(delay=ent2diffcreated)
    entity2diff$entity<-c(entity2entry)#creating a factor variable for Entity
    
    bothentitiesdelay<-rbind(entity1diff,entity2diff) #our data frame
    
  }
  
  # function to calculate number of tweets expected to arrive within a specified time frame - "time" - Tweak of Stanton's code
  
  numtweetsintime<-function(entity1,entity2,time,entity1entry,entity2entry){
    ###### Few repeated commands below
    ent1sort<-entity1[order(as.integer(entity1()$created)),]  #Ordering tweets based on arrival time
    ent2sort<-entity2[order(as.integer(entity2()$created)),]  #Ordering tweets based on arrival time
    
    ent1diffcreated<-as.integer(diff(ent1sort$created)) #calculate the difference in seconds between each pair of neighboring values. 
    ent2diffcreated<-as.integer(diff(ent2sort$created))
    
    # Number of tweets occuring before specified time -time # This would also be done in the next function
    nument1tweetsbeftime<-sum(as.integer(ent1diffcreated)<=time)
    nument2tweetsbeftime<-sum(as.integer(ent2diffcreated)<=time)
    
    # Proportion of tweets occuring before specified time -time
    propent1tweetsbeftime<-nument1tweetsbeftime/(nrow(entity1))
    propent2tweetsbeftime<-nument2tweetsbeftime/(nrow(entity2))
    
    #Poisson tests for confidence intervals of the two proportions
    ent1poiss<-poisson.test (nument1tweetsbeftime,nrow(entity1))
    ent2poiss<-poisson.test (nument2tweetsbeftime,nrow(entity2))
    
    ent1poissconfint<-ent1poiss$conf.int
    ent2poissconfint<-ent2poiss$conf.int
  
    #Determining lower and upper values of confidence intervals and storing them for subsequent use
    lowent1<-ent1poiss$conf.int[1]
    uppent1<-ent1poiss$conf.int[2]
    lowent2<-ent2poiss$conf.int[1]
    uppent2<-ent2poiss$conf.int[2]
    
    # gplots not used below...--- I love ggplot2 ----bar graphs of proportion of tweets occuring before 
    #specified time and confidence intervals.
    
    #propplot<-barplot2(c(propent1tweetsbeftime,propent2tweetsbeftime),ci.l=c(lowent1,lowent2),ci.u=c(uppent1,uppent2), plot.ci=TRUE,names.arg=c("Entity 1","Entity 2"))#ci=TRUE places confidence int whiskers
    
    # Creating data frames with all required information to create bar graphs of proportion of tweets arriving within specified time---
    #with error bars (confidence intervals) - two separate data frames and then combining the two together
    dataprop1<-data.frame(Entity=entity1entry,NumberTweetsBeforeTime=nument1tweetsbeftime,
                          TotalTweetsRetrieved=nrow(entity1),TimeInSeconds=time,ProportionEstimated=propent1tweetsbeftime,
                          ymin=lowent1,ymax=uppent1)
    dataprop2<-data.frame(Entity=entity2entry,NumberTweetsBeforeTime=nument2tweetsbeftime,
                          TotalTweetsRetrieved=nrow(entity2),TimeInSeconds=time,ProportionEstimated=propent2tweetsbeftime,
                          ymin=lowent2,ymax=uppent2)
    dataprop<-rbind(dataprop1,dataprop2)
    
  }  
  
  # Conducting an overall poisson test of proportions for both entities --- a rate ratio of 1 or in its vicinity 
  #would suggest that the rates are the same (or similar?)
  
  poisstweetsintime<-function(entity1,entity2,time){
    ent1sort<-entity1[order(as.integer(entity1()$created)),]  #Ordering tweets based on arrival time
    ent2sort<-entity2[order(as.integer(entity2()$created)),]  #Ordering tweets based on arrival time
    
    ent1diffcreated<-as.integer(diff(ent1sort$created)) #calculate the difference in seconds between each pair of neighboring values. 
    ent2diffcreated<-as.integer(diff(ent2sort$created))
    
    nument1tweetsbeftime<-sum(as.integer(ent1diffcreated)<=time)
    nument2tweetsbeftime<-sum(as.integer(ent2diffcreated)<=time)
    
    poiss<-poisson.test(c(nument1tweetsbeftime,nument2tweetsbeftime),c(nrow(entity1),nrow(entity2)))
    return(poiss)
  }
  
  
  # function for word cloud (wordcloud package and discussed in a previous post on tweetanalytics)
  
  wordcloudentity<-function(entitycleantext)
  {
    tweetCorpus<-Corpus(VectorSource(CleanTweets(entitycleantext)))
    tweetTDM<-TermDocumentMatrix(tweetCorpus,control=list(removePunctuation=TRUE,
                                                          stopwords=c(stopwords('english')),
                                                          removeNumbers=TRUE,tolower=TRUE))
    tdMatrix <- as.matrix(tweetTDM) # creating a data matrix
    sortedMatrix<-sort(rowSums(tdMatrix),decreasing=TRUE) # calculate row sum of each term and sort in descending order (high freq to low)
    cloudFrame<-data.frame(word=names(sortedMatrix),freq=sortedMatrix)#extracting names from named list in prev command and binding together into a dataframe with frequencies - called cloudFrame, names in separate columns
    
    wcloudentity<-wordcloud(cloudFrame$word,cloudFrame$freq,max.words=100, colors=brewer.pal(8,"Dark2"),scale=c(8,1), random.order=TRUE)
    print(wcloudentity)
  }
  
  # To assess the valence of tweets, we use Jeffrey Breen's approach, which was also relied on by 
  #Gaston Sanchez's work on his twitter project
  #Jeffrey Breen: http://jeffreybreen.wordpress.com/2011/07/04/twitter-text-mining-r-slides/ 
  #via Gaston Sanchez's twitter mining project: https://sites.google.com/site/miningtwitter/questions/sentiment/analysis 

  # Scoring sentiment expressed - Breen's algorithm, almost verbatim
  
  score.sentiment = function(sentences, pos.words, neg.words)
  {
       
    # we got a vector of sentences. plyr will handle a list
    # or a vector as an "l" for us
    # we want a simple array ("a") of scores back, so we use 
    # "l" + "a" + "ply" = "laply":
    scores = laply(sentences, function(sentence, pos.words, neg.words) {
      
      # clean up sentences with R's regex-driven global substitute, gsub():
      sentence = gsub('[[:punct:]]', '', sentence)
      sentence = gsub('[[:cntrl:]]', '', sentence)
      sentence = gsub('\\d+', '', sentence)
      # and convert to lower case:
      sentence = tolower(sentence)
      
      # split into words. str_split is in the stringr package
      word.list = str_split(sentence, '\\s+')
      # sometimes a list() is one level of hierarchy too much
      words = unlist(word.list)
      
      # compare our words to the dictionaries of positive & negative terms
      pos.matches = match(words, pos.words)
      neg.matches = match(words, neg.words)
      
      # match() returns the position of the matched term or NA
      # we just want a TRUE/FALSE:
      pos.matches = !is.na(pos.matches)
      neg.matches = !is.na(neg.matches)
      
      # and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
      score = sum(pos.matches) - sum(neg.matches)
      
      return(score)
    }, pos.words, neg.words)
    
    scores.df = data.frame(score=scores, text=sentences)
    return(scores.df)
  }
  
  #calling the above sentiment scoring function using this function below... the text of tweets serve as inputs
  
  sentimentalanalysis<-function(entity1text,entity2text,entity1entry,entity2entry){

    # A compiled list of words expressing positive and negative sentiments ----
    #http://www.cs.uic.edu/~liub/FBS/sentiment-analysis.html
    # List of words and additional information on the original source from Jeffrey Breen's github site at:
    #https://github.com/jeffreybreen/twitter-sentiment-analysis-tutorial-201107/tree/master/data/opinion-lexicon-English
  
    positivewords=readLines("positive_words.txt")
    negativewords=readLines("negative_words.txt")
    
    #Applying score.sentiment algorithm to cleaned tweets and getting data frames of tweets, net sentiment score for a tweet 
    #(number of positive sentiments minus negative sentiments)
    
    entity1score = score.sentiment(CleanTweets(entity1text),positivewords,negativewords)
    entity2score = score.sentiment(CleanTweets(entity2text),positivewords,negativewords)
    
    # Adding a dummy variable useful for a ggplot
    entity1score$entity = entity1entry
    entity2score$entity = entity2entry

    #combine all of this
    entityscores<-rbind(entity1score,entity2score)
  
    }   
  
  # Time for execution
  
  # Reading in values for the two entities
  entity1<-reactive({entity1<-TweetFrame(input$entity1, input$maxTweets)})
  entity2<-reactive({entity2<-TweetFrame(input$entity2, input$maxTweets)})
  
  # creating dataframe of arrival probabilities by calling the arrivalprob function
  arrivaldata<-reactive({arrivaldata<-arrivalprob(entity1(),entity2(),input$entity1,input$entity2)})
  
  #creating dataframe of delays by calling DelayTweets function
  
  bothentities<-reactive({bothentities<-DelayTweets(entity1(),entity2(),input$entity1,input$entity2)})
  
  #creating dataframe of proportion of tweets occuring before time- t
  
  dataprop<-reactive({dataprop<-numtweetsintime(entity1(),entity2(),input$tweettime,input$entity1,input$entity2)})
  
  # Overall Poisson Test to compare Ratio of Proportion of TWeets arriving within the specified time
  
  poiss<-reactive({poisstweetsintime(entity1(),entity2(),input$tweettime)})
  
  #Creating sentiment scores
  entityscores<-reactive({entityscores<-sentimentalanalysis(entity1()$text,entity2()$text,input$entity1,input$entity2)})
  
  #Preparing the output in a series of tabs
  
 #tab 1  - number of tweets for the two entities and also plotting the probability of arrival of a new tweet 
  #within a particular time t
  
  #number of tweets
  output$notweets<-renderPrint({numoftweets(entity1(),entity2(),input$entity1,input$entity2)})
    
  #See ggtitle below
  output$arrivalprob<-renderPlot({cptplot<-ggplot(arrivaldata(),aes(x=time,y=value,color=variable))+geom_point()+ geom_line()+
                                    labs(y= "Probability", x="Time (t)------>")+ theme(axis.text.y = element_text(color="black"))+
                                    theme(axis.text.x = element_text(color="black")) + ggtitle("Probability of a new tweet arriving within a particular time, t")
                                  print(cptplot)})
    
  #tab 2 --- Three plots to understand the distribution of delay time between tweets - A box plot, histogram, and 
  #a kernel density function ---- All plots combined using grid.arrange and some adjustments to axes performed
  
  output$bothentitiesdelayhist<-renderPlot({
    
    cptboxplot<-ggplot(bothentities(),aes(x=entity,y=delay))+
      geom_boxplot(aes(color=entity))+geom_jitter(aes(x=entity,y=delay,color=entity,alpha=.3))+
      stat_summary(fun.y=mean, geom="point",color="blue", size=5)+coord_flip()+ 
      scale_y_continuous(breaks = NULL) + labs(x= "")+
      theme(legend.position="none",
            axis.text.x=element_blank(),
            axis.ticks.x=element_blank(),
            plot.margin=unit(c(0,0,-0.25,0), "cm"))+theme(axis.text.y = element_text(color="black"))+
      theme(axis.text.x = element_text(color="black"))
    
    cpthist<-ggplot(bothentities(),aes(x=delay, fill=entity))+geom_histogram(binwidth=1)+scale_x_continuous(breaks = NULL) + 
     
      theme(legend.position="none",axis.text.x=element_blank(),
      plot.margin=unit(c(-.25,0,-0.25,0), "cm"))+theme(axis.text.y = element_text(color="black"))+
      theme(axis.text.x = element_text(color="black"))
    
    cptdensity<-ggplot(bothentities(),aes(x=delay, fill=entity))+geom_density(alpha=.5)+
      scale_y_continuous(breaks = NULL) + guides(fill=guide_legend(title=NULL))+
      theme(plot.margin=unit(c(-.25,0,0,0), "cm"))+
      theme(legend.position=c(1,1),legend.justification=c(1,1)) +
      theme(legend.background=element_blank()) + 
      theme(legend.key=element_blank())+theme(axis.text.y = element_text(color="black"))+
      theme(axis.text.x = element_text(color="black"))
    print(grid.arrange(cptboxplot,cpthist,cptdensity))})
  

  #tab 3 Three elements here - One, a table to show the number of tweets arriving within a user specified time for both entities, 
  # and proportion, based on how many tweets were retrieved for each entity. Confidence intervals computed as well - 
  #poisson distribution assumed, this table is visually depicted using error bars; thanks ggplot2 documentation 
  #(http://docs.ggplot2.org/0.9.3.1/geom_errorbar.html). Lastly, an overall poisson test testing if the ratio of the two proportions is 1.
  
  #table
  output$proptable<-renderTable({tab<-dataprop()})
  
  #bar graph with error bars to show confidence interval
  output$propplot<-renderPlot({propplot<-ggplot(dataprop(),aes(x=Entity,y=ProportionEstimated,fill=Entity))+
                                 geom_bar(stat="identity",position="dodge")+
                                 geom_errorbar(aes(ymax=ymax,ymin=ymin,position="dodge",width=.25))+theme(axis.text.y = element_text(color="black"))+
                                 theme(axis.text.x = element_text(color="black"))+ theme(legend.position="none")+labs(x= "")
                               print(propplot)})
  #overall poisson test
  output$poisstest<-renderPrint({poiss()})
  
  #tab 4: Not all chatter may be good. So a box plot to see the distribution of scores of sentiments 
  
  output$sentiboxplot<-renderPlot({sentiboxplot<-ggplot(entityscores(),aes(x=entity,y=score,fill=entity))+geom_boxplot()+geom_jitter(alpha=.3)+
                                     theme(axis.text.y = element_text(color="black"))+
                                     theme(axis.text.x = element_text(color="black"))+ theme(legend.position="none")+labs(x="")+
                                     stat_summary(fun.y=mean, geom="point",color="blue", size=5)
                                   print(sentiboxplot)})
    
  # getting a feel for how sentiments were scored by scanning 4 tweets per entity and sentiment scores - data frame entity scores shown
  output$sentiheadtable<-renderTable({tab<-head(entityscores(),4)})
  output$sentitailtable<-renderTable({tab<-tail(entityscores(),4)})
  
  #tab 5 - Word Clouds to highlight terms used in tweets associated with the two entities
  output$entity1wc<-renderText({input$entity1})
  output$entity1wcplot<-renderPlot({wordcloudentity(entity1()$text)})
                                    
  output$entity2wc<-renderText({input$entity2})
  output$entity2wcplot<-renderPlot({wordcloudentity(entity2()$text)})
  
  
  #tab  6: Raw tweets of entity 1
  output$tableentity1 <- renderTable({tab<-entity1()[1]})
  
  #tab 7: Raw tweets of entity 2
  
  output$tableentity2<-renderTable({tab<-entity2()[1]})
  
    
})

