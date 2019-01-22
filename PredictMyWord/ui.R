# Course 10 - Data Science Capstone
# Author: Sanjay Lonkar
# Date: 20-Jan-2018

library(shiny)

# Define UI for application that predicts next word (s)
shinyUI(fluidPage(
  
  # Application title/Title in browser
  titlePanel("Predicting Next Word for Me"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      h4("Instructions: "),
      helpText ("Please input a", strong ("word,"), " a", strong ("sentenance"), "or a", strong ("phrase"), " in English and click", strong("Predict"), " to get next suggested word."),
      helpText (strong("Settings"), " allow you to configure whether common words like is, my, some, etc. are to be ignored for prediction. You can also configure number of words to predict (Min 1 - Max - 5)."),
      
      #textInput("txtInputString", "", "anytime soon love"), - has output word from both
      textInput("txtInputString", "", "Your Input Here"),
      actionButton ("btnPredict", "Predict!", align = "center", style="color: #fff; background-color: blue;"),
      
      tags$hr(style="border-color: lightblue;"),
      h4("Settings: "),
      tags$br (),
      radioButtons ("rdoStopWords", "1. Common English Words", list ("Consider common words such as is, my, some, etc. for prediction" = 0,"Do not consider common words for prediction" = 1), "0"),
      numericInput ("numWordsToPredict","2. Maximum Number of Words to Predict", value = 1, min=1, max=5),

      helpText("Note: "),
      helpText("a. Predicted results are displayed in ", strong("Prediction"), " tab."),
      helpText("b. ", strong ("For reference, Appendix tab lists tested phrases from sampled dataset."),  "It also displays list of common words that can be configured for omission.")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      h3("Prediced Results"),
      
      tabsetPanel (
        tabPanel("Prediction", 
                 tags$br (),
                 h3 ("Predicted word for given text is "),         
                 h3 (code (htmlOutput("tabPrediction"))) 
        ),
        
        tabPanel("Appendix", 
                 tags$br (),
                 h3 ("Test phrases from sampled dataset"),
                 helpText ("This product uses sampled datasets due to limited computer resources. It is tested for following phrases from English HC Corpora (5 phrases each from Twitter, News and Blogs)."),
                 helpText ("Reviewer is requested to use any of these phrases if predictions are not working for random user entered phrases."),
                 helpText (strong ("Twitter:"), "1. I think Josh Willingham missed tonight's game due to a Herpes outbreak, 2. I'm giddy with anticipation"),
                 helpText ("3. If Romney wants to attract young voters, he'll get rid of old has beens like Ted Nugent 4. You are sweet to mention my story. Thank you"),
                 
                 helpText (strong ("News:"), "1. They were as alien as foreigners 2. About a decade ago, a once-obscure Austrian wine became the darling of American oenophiles"),
                 helpText ("3. At the bottom of this story, you will find a form to add your own., 4. Karl Tallman, the chief ranger for several parks in the Santa Cruz Mountains"),
                 
                 helpText (strong ("Blogs:"), "1. Even if I had assessed the signs to reflect what turned out to be reality 2. In this passage I believe it is safe to say that hailstones"),
                 helpText ("3. Glamour is a double-edged sword, 4. Many Governor's Mansions throughout the US have had the same problem"),                 
                 
                 
                 tags$hr(),
                 
                 h3 ("List of common words is as follows:"),    
                 helpText ("i, me, my, myself, we, our, ours, ourselves, you, your, yours, yourself, yourselves, he, him, his, himself, she, her, hers, herself, as"),
                 helpText ("their, theirs, themselves,what, which, who, whom, this, that, these, those, am, is, are, was, were, be, been, being, have, it, its"),
                 helpText ("doing, would, should, could, ought, i'm, you're, he's, she's, it's, we're, they're, i've, has, had, having, do, does, did, very, here's, there's"),
                 helpText ("you've, we've, they've, i'd, you'd, he'd, she'd, we'd, they'd, i'll, you'll, he'll, she'll, we'll, they'll, isn't, aren't, wasn't, weren't,"),
                 helpText ("hasn't, haven't, hadn't, doesn't, don't, didn't, won't, wouldn't, shan't, shouldn't, can't, cannot, couldn't, mustn't, let's, that's, who's, what's"),
                 helpText ("when's, where's, why's, how's, a, an, until, while, of, at, by, for, with, about, against, between, into, through, during,  the, and, but, if, or"),
                 helpText ("before, after, above, below, to, from, up, down, in, out, on, off, over, too, under, again, further, then, once, here, there, because"),
                 helpText ("both, each, few, more, most, other, some, such, no, nor, not, only, own, same, so, than, when, where, why, how, all, any, itself, they, them")
        )
      )
    )
  )
))
