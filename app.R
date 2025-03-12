library(shiny)
library(tidyr)
library(dbplyr)

##### Powerball #####
# To win Division 1 you need to select all 7 winning numbers plus the Powerball in a single game panel. 
# You can win a prize with as little as 2 winning numbers and the Powerball number.

play_powerball <- function(user_numbers, times = 1) {
  # Validate input
  if(!is.vector(user_numbers))
    stop("Input must be a vector")
  
  if(length(user_numbers) != 8)
    stop("Input must contain exactly 8 numbers")
  
  if(sum(user_numbers %% 1) > 0)
    stop("Input must contain only natural numbers")
  
  if(any(user_numbers[1:7] < 1 | user_numbers[1:7] > 35))
    stop("First seven numbers must be selected from 1 to 35")
  
  if(any(user_numbers[8] < 1 | user_numbers[8] > 20))
    stop("Last number must be selected from 1 to 20")
  
  if(any(duplicated(user_numbers[1:7])))
    stop("First seven numbers cannot contain repetition")
  
  # Set up counters
  jackpots <- 0
  losses <- 0
  division2_wins <- 0
  division3_wins <- 0
  division4_wins <- 0
  division5_wins <- 0
  division6_wins <- 0
  division7_wins <- 0
  division8_wins <- 0
  division9_wins <- 0
  
  # Set up frequency tracking
  main_number_freq <- numeric(35)
  powerball_freq <- numeric(20)
  
  # Initialize a data frame to store all winning numbers
  all_winning_numbers <- data.frame(
    draw_number = integer(),
    n1 = integer(), 
    n2 = integer(), 
    n3 = integer(), 
    n4 = integer(), 
    n5 = integer(), 
    n6 = integer(), 
    n7 = integer(),
    powerball = integer(),
    outcome = character()
  )
  
  # Run the simulation the specified number of times
  for(i in 1:times) {
    # Generate winning numbers
    winning_numbers <- c(sample(1:35, size = 7, replace = FALSE),
                         sample(1:20, size = 1, replace = FALSE))
    
    # Update frequency counts
    for (j in 1:7) {
      main_number_freq[winning_numbers[j]] <- main_number_freq[winning_numbers[j]] + 1
    }
    powerball_freq[winning_numbers[8]] <- powerball_freq[winning_numbers[8]] + 1
    
    # Determine outcome
    outcome <- "LOSS"
    
    # Check if user won division 1
    if(all(sort(user_numbers[1:7]) == sort(winning_numbers[1:7])) &&
       user_numbers[8] == winning_numbers[8]) {
      jackpots <- jackpots + 1
      cat("JACKPOT! Draw #", i, "\n")
      outcome <- "DIVISION1"
      
    } else if(all(sort(user_numbers[1:7]) == sort(winning_numbers[1:7]))) {
      # Check division 2 winner
      division2_wins <- division2_wins + 1
      cat("Division 2 winner! Draw #", i, "\n")
      outcome <- "DIVISION2"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 6 &&
              user_numbers[8] == winning_numbers[8]) {
      # Division 3 winner
      division3_wins <- division3_wins + 1
      cat("Division 3 winner! Draw #", i, "\n")
      outcome <- "DIVISION3"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 6) {
      # Division 4 winner
      division4_wins <- division4_wins + 1
      cat("Division 4 winner! Draw #", i, "\n")
      outcome <- "DIVISION4"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 5 &&
              user_numbers[8] == winning_numbers[8]) {
      # Division 5 winner
      division5_wins <- division5_wins + 1
      cat("Division 5 winner! Draw #", i, "\n")
      outcome <- "DIVISION5"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 4 &&
              user_numbers[8] == winning_numbers[8]) {
      # Division 6 winner
      division6_wins <- division6_wins + 1
      cat("Division 6 winner! Draw #", i, "\n")
      outcome <- "DIVISION6"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 5) {
      # Division 7 winner
      division7_wins <- division7_wins + 1
      cat("Division 7 winner! Draw #", i, "\n")
      outcome <- "DIVISION7"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 3 &&
              user_numbers[8] == winning_numbers[8]) {
      # Division 8 winner
      division8_wins <- division8_wins + 1
      cat("Division 8 winner! Draw #", i, "\n")
      outcome <- "DIVISION8"
      
    } else if(sum(user_numbers[1:7] %in% winning_numbers[1:7]) == 2 &&
              user_numbers[8] == winning_numbers[8]) {
      # Division 9 winner
      division9_wins <- division9_wins + 1
      cat("Division 9 winner! Draw #", i, "\n")
      outcome <- "DIVISION9"
    } else {
      losses <- losses + 1
      cat("LOSER! Draw #", i, "\n")
      outcome <- "LOSS"
    }
    
    # Store winning numbers and outcome for this draw
    draw_data <- data.frame(
      draw_number = i,
      n1 = winning_numbers[1],
      n2 = winning_numbers[2],
      n3 = winning_numbers[3],
      n4 = winning_numbers[4],
      n5 = winning_numbers[5],
      n6 = winning_numbers[6],
      n7 = winning_numbers[7],
      powerball = winning_numbers[8],
      outcome = outcome
    )
    
    all_winning_numbers <- rbind(all_winning_numbers, draw_data)
  }
  
  # Create stats dataframe
  stats_df <- data.frame(
    division = c("Division 1", "Division 2", "Division 3", "Division 4", "Division 5", 
                 "Division 6", "Division 7", "Division 8", "Division 9", "Losses"),
    wins = c(jackpots, division2_wins, division3_wins, division4_wins, division5_wins,
             division6_wins, division7_wins, division8_wins, division9_wins, losses),
    percentage = c(jackpots, division2_wins, division3_wins, division4_wins, division5_wins,
                   division6_wins, division7_wins, division8_wins, division9_wins, losses) / times * 100
  )
  
  # Create number frequency dataframes
  main_numbers_freq_df <- data.frame(
    number = 1:35,
    frequency = main_number_freq,
    percentage = main_number_freq / (times * 7) * 100
  )
  
  powerball_freq_df <- data.frame(
    number = 1:20,
    frequency = powerball_freq,
    percentage = powerball_freq / times * 100
  )
  
  # Return summary statistics
  cat("\nSimulation Summary:\n")
  cat("Total Drawings:", times, "\n")
  cat("Jackpots:", jackpots, "(",format(jackpots/times*100, digits=4),"%)\n")
  cat("division 2:", division2_wins, "(",format(division2_wins/times*100, digits=4),"%)\n")
  cat("division 3:", division3_wins, "(",format(division3_wins/times*100, digits=4),"%)\n")
  cat("division 4:", division4_wins, "(",format(division4_wins/times*100, digits=4),"%)\n")
  cat("division 5:", division5_wins, "(",format(division5_wins/times*100, digits=4),"%)\n")
  cat("division 6:", division6_wins, "(",format(division6_wins/times*100, digits=4),"%)\n")
  cat("division 7:", division7_wins, "(",format(division7_wins/times*100, digits=4),"%)\n")
  cat("division 8:", division8_wins, "(",format(division8_wins/times*100, digits=4),"%)\n")
  cat("division 9:", division9_wins, "(",format(division9_wins/times*100, digits=4),"%)\n")
  cat("Losses:", losses, "(",format(losses/times*100, digits=4),"%)\n")
  
  # Return results as a list
  results <- list(
    stats = stats_df,
    main_numbers_frequency = main_numbers_freq_df,
    powerball_frequency = powerball_freq_df,
    draws = all_winning_numbers
  )
  
  return(results)
}

# Run the simulation and store the results
results <- play_powerball(c(1, 2, 23, 13, 15, 33, 27, 12), times = 1000)

# Access the different dataframes
stats <- results$stats
main_numbers_freq <- results$main_numbers_frequency
powerball_freq <- results$powerball_frequency
all_draws <- results$draws































# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("bins",
                        "Number of bins:",
                        min = 1,
                        max = 50,
                        value = 30)
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
