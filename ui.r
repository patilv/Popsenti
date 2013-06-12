library(shiny)

shinyUI(pageWithSidebar(
  
  headerPanel("Twitter-Twitter on the Web - Who is the Most Popular of All?"),
  
  # Getting User Inputs
  
  sidebarPanel(
    
    textInput("entity1", "Entity 1:First Entity's Name or First Term Here; Feel free to replace 'Michael' with your name 
              and/or feel free to replace the '#' with '@'", "#Michael"),
    textInput ("entity2","Entity 2:Please Enter Second Entity's Name or Second Term Here; Feel free to replace 'Mary' 
               with any other name and/or replace the '#' with '@'", "#Mary"),
    sliderInput("maxTweets","Number of recent tweets to use for analysis:",min=5,max=50,value=5), # The max can, of course, be increased
    sliderInput("tweettime","Of the number of tweets retrieved above, let's calculate the proportion of these that arrive within 
                a certain time.  Please specify Time (in seconds)?:",min=30,max=3600,value=30),
    submitButton(text="Twitter, who/what is more popular?")
  ),
  
  mainPanel(
    tabsetPanel(
      
      #Output from tab 1 ----number of tweets for the two entities and also plotting the probability of arrival of a new tweet 
      #within a particular time t
      
      tabPanel("Probability of New Tweet Arrival",HTML("<div>Number of tweets</div>"),verbatimTextOutput("notweets"),plotOutput("arrivalprob"),
               HTML
               ("<div> The probability of a new tweet arriving within a particular time t (horizontal axis) is given by the vertical axis. So, the entity 
with colored dots (and lines) higher than the other has a higher probability of a tweet arriving within time t. Of course, this based 
on the most recent tweets retrieved and this probability can change depending on how many tweets are retrieved 
and when they are retrieved. Higher probability, perhaps, suggests higher frequency of chatter.
                </div>")),
      
      # Output from tab 2 ---Three plots to understand the distribution of delay time between tweets - A box plot, histogram, and 
      #a kernel density function
      
      tabPanel("Distribution of delay times between Tweets",plotOutput("bothentitiesdelayhist"),
               HTML
               ("<div> At the very top are the box plots,which give information on the minimum, maximum, and 25th, 50th (median), 
and 75th percentile of the distribution. Blue dots are mean delay between tweets for the two entities. Below them are the histogram 
and the kernel density function, both colored based on the entity being referred to. Overall, lower the delay time between tweets, 
higher the frequency of tweets, suggesting a higher popularity.</div>")),
      
      # Output from tab 3--- Three elements here - One, a table to show the number of tweets arriving within a user 
      #specified time for both entities,and proportion, based on how many tweets were retrieved for each entity. 
      #Confidence intervals computed as well - poisson distribution assumed, this table is visually depicted using error bars; thanks ggplot2 documentation 
      #(http://docs.ggplot2.org/0.9.3.1/geom_errorbar.html). Lastly, an overall poisson test testing if the ratio of the two proportions is 1.
      
      tabPanel("Number of Tweets arriving Within Time Specified",plotOutput("propplot"),tableOutput("proptable"),HTML
               ("<div> The graph shows the proportion of Entity 1 and Entity 2 tweets (among all retrieved 
                tweets) that arrived within the user specified time. The vertical lines on each bar (known as error bars) 
                denote the range of error (given by ymin and ymax in the table above) possible in the computation of these 
                proportions. So, if these two vertical lines (error bars) appear to overlap at any point in the proportion 
                estimated (vertical axis), then the evidence that one entity's proportion is statistically different from the 
                other is weak (at the 95% confidence interval). This should also be corraborated by the <b>comparison of poisson rates 
                test performed below.</b> This tests whether the ratio of the two proportions is = 1
                (i.e., the two proportions are equal). If the 95% confidence interval includes the value of 1, 
                then evidence suggests that the proportions are pretty much the same. 
                The p-value will tend to be greater than .05 in such instances.</div>"),
               
               verbatimTextOutput("poisstest")),
      
      #Output from tab 4 ----So a box plot to see the distribution of scores of sentiments 
      tabPanel("Is the chatter positive or negative?",plotOutput("sentiboxplot"),HTML
               ("<div> This plot shows the distribution of positive/negative sentiments about each entity.(Note that tweets were 
cleaned before this analysis was performed.) For each tweet, a 
net score of positive and negative sentiments is computed and this plot shows the distribution of scores. 
Boxplots give information on the minimum, maximum, and 25th, 50th (median), and 75th percentile of the distribution. Blue dots are mean sentiment scores for the 
two entities. A higher sentiment score suggests more positive (or a less negative) discussion of that entity than the other. A sampling of how these were coded is given below.</div>"),
               tableOutput("sentiheadtable"),tableOutput("sentitailtable")),
      
      #Output from tab 5 - Word clouds - with some html tags
      
      tabPanel("Entity 1 and Entity 2 Word Clouds",HTML
               ("<div> <b><u><center>Entity 1</b></u></center></div>"),plotOutput("entity1wcplot"),HTML
               ("<div> <b><u><center>Entity 2</b></u></center></div>"),plotOutput("entity2wcplot")),
      
      #Output from tabs 6 and 7, the raw tweets
      tabPanel("Entity 1 raw tweets",tableOutput("tableentity1")),
      tabPanel("Entity 2 raw tweets",tableOutput("tableentity2"))
    )
  )
  
))
