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
user_numbers <- c(1,2,3,4,5,6,7,8)
results <- play_powerball(user_numbers, times = 100)

# Access the different dataframes
stats <- results$stats
main_numbers_freq <- results$main_numbers_frequency
powerball_freq <- results$powerball_frequency
all_draws <- results$draws

# Plot of frequent main numbers
main_num_plot <- barplot(
  main_numbers_freq$freq,
  names.arg = main_numbers_freq$number,
  main = "Frequency of Main Numbers",
  xlab = "Numbers",
  ylab = "Frequency",
  col = "darkmagenta",
  border = "white"
)

# Plot of frequent powerball
powerball_plot <- barplot(
  powerball_freq$freq,
  names.arg = powerball_freq$number,
  main = "Frequency of Powerballs",
  xlab = "Numbers",
  ylab = "Frequency",
  col = "darkmagenta",
  border = "white"
)

# Plot of wins
wins_plot <- stats <- stats[stats$percentage != "losses", ]
barplot(
  stats$percentage, 
  names.arg = stats$division, 
  main = "Winning Statistics", 
  xlab = "Division", 
  ylab = "Winning Percentage", 
  col = "darkmagenta", 
  border = "white"
)

# Making of the app

# Load libraries
library(shiny)


# Define UI
ui <- fluidPage(
  titlePanel("Powerball Simulator: Good Luck!"),
  
  sidebarLayout(
    sidebarPanel(
      h4("Enter Your Numbers:"),
      
      # Input for 7 main numbers
      numericInput("num1", "Number 1", value = 1, min = 1, max = 35),
      numericInput("num2", "Number 2", value = 2, min = 1, max = 35),
      numericInput("num3", "Number 3", value = 3, min = 1, max = 35),
      numericInput("num4", "Number 4", value = 4, min = 1, max = 35),
      numericInput("num5", "Number 5", value = 5, min = 1, max = 35),
      numericInput("num6", "Number 6", value = 6, min = 1, max = 35),
      numericInput("num7", "Number 7", value = 7, min = 1, max = 35),
      
      # Input for Powerball number
      numericInput("num8", "Powerball", value = 1, min = 1, max = 20),
      
      # Number of simulations to rin
      numericInput("simulations", "Number of Simulations", value = 100, min = 1, max = 1000000),
      
      actionButton("runSim", "Run Simulation")
    ),
    
    mainPanel(
      h3("Results"),
      textOutput("userNumbersOutput"),
      verbatimTextOutput("simulationSummary"),
      
      tabsetPanel(
        tabPanel("Number Frequency", plotOutput("mainNumberPlot")),
        tabPanel("Powerball Frequency", plotOutput("powerballPlot")),
        tabPanel("Winning Frequency", plotOutput("winsPlot"))
      )
    )
  )
)

# Define Server
server <- function(input, output, session) {
  
  # Reactive value to store the winning numbers
  simulationResults <- reactiveVal(NULL)
  
  observeEvent(input$runSim, {
    user_numbers <- c(input$num1, input$num2, input$num3, input$num4,
                      input$num5, input$num6, input$num7, input$num8)
    
    # Check for duplicates
    if(any(duplicated(user_numbers[1:7]))) {
      showNotification("First Seven Numbers Must Be Unique!", type = 'error')
      return()
    }
    
    # Run the Powerball simulation
    results <- play_powerball(user_numbers, times = input$simulations)
    
    # Store the simulation results
    simulationResults(results)
  })
  
   
  # Display User Numbers
  output$userNumbersOutput <- renderText({
    req(input$runSim)
    user_numbers <- c(input$num1, input$num2, input$num3, input$num4,
                      input$num5, input$num6, input$num7, input$num8)
    paste("Your Numbers:", paste(user_numbers[1:7], collapse = ", "),
          "| Powerball:", user_numbers[8])
  })
   

  # Display simulation summary
    output$simulationSummary <- renderPrint({
    req(simulationResults())
    results <- simulationResults()
      results_summary <- paste(
        "Simulation Summary:",
        paste("Total Drawings:", input$simulations),
        paste("Jackpots:", results$stats$wins[1], "(", format(results$stats$percentage[1], digits=4), "%)"),
        paste("Division 2:", results$stats$wins[2], "(", format(results$stats$percentage[2], digits=4), "%)"),
        paste("Division 3:", results$stats$wins[3], "(", format(results$stats$percentage[3], digits=4), "%)"),
        paste("Division 4:", results$stats$wins[4], "(", format(results$stats$percentage[4], digits=4), "%)"),
        paste("Division 5:", results$stats$wins[5], "(", format(results$stats$percentage[5], digits=4), "%)"),
        paste("Division 6:", results$stats$wins[6], "(", format(results$stats$percentage[6], digits=4), "%)"),
        paste("Division 7:", results$stats$wins[7], "(", format(results$stats$percentage[7], digits=4), "%)"),
        paste("Division 8:", results$stats$wins[8], "(", format(results$stats$percentage[8], digits=4), "%)"),
        paste("Division 9:", results$stats$wins[9], "(", format(results$stats$percentage[9], digits=4), "%)"),
        paste("Losses:", results$stats$wins[10], "(", format(results$stats$percentage[10], digits=4), "%)"),
        sep = "\n"
      )
      cat(results_summary)
    })
    
    # Generate frequency table for main numbers
    output$mainNumberPlot <- renderPlot({
      req(simulationResults())
      
      main_numbers_freq <- simulationResults()$main_numbers_frequency
      
      barplot(
        main_numbers_freq$frequency,
        names.arg = main_numbers_freq$number,
        main = "Frequency of Main Numbers",
        xlab = "Numbers",
        ylab = "Frequency",
        col = "darkmagenta",
        border = "white"
      )
    })
    
    # Powerball frequency plot
    output$powerballPlot <- renderPlot({
      req(simulationResults())
      
      powerball_freq <- simulationResults()$powerball_frequency
      
      barplot(
        powerball_freq$frequency,
        names.arg = powerball_freq$number,
        main = "Frequency of Powerballs",
        xlab = "Numbers",
        ylab = "Frequency",
        col = "darkmagenta",
        border = "white"
      )
    })
    
    # Wins plot - excluding losses for better visualization
    output$winsPlot <- renderPlot({
      req(simulationResults())
      
      stats <- simulationResults()$stats
      # Exclude losses for better visualization
      stats_filtered <- stats[1:9, ]
      
      barplot(
        stats_filtered$percentage,
        names.arg = stats_filtered$division,
        main = "Winning Statistics",
        xlab = "Division",
        ylab = "Winning Percentage (%)",
        col = "darkmagenta",
        border = "white",
        las = 2
      )
    })
  }
  
# Run the app
shinyApp(ui = ui, server = server)

