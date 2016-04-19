shinyUI(pageWithSidebar(
  headerPanel("Guess Word"),
  sidebarPanel(
    textInput("phrase", label = h3("Your phrase:"), 
              value = "" ),
    submitButton("Try it"),
	br(),
	h5('Enter any phrase you please and check our suggestion for the following word on the right. We hope it helps!', style = "color:darkgrey")
  ),
  mainPanel(
  br(),
  br(),
  br(),
  br(),
  verbatimTextOutput("prediction"), width=2
  )
))