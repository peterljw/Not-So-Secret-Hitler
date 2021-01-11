library(shiny)
library(shinyjs)

# ---------------- Helper Functions ----------------

add <- function(x) Reduce("+", x)

calculate_remaining_policies <- function(df_history){
  claim_mapping = list("3L"=c(3,0),
                       "2L+1F"=c(2,1),
                       "1L+2F"=c(1,2),
                       "3F"=c(0,3),
                       "L"=c(1,0),
                       "F"=c(0,1))
  # before reshuffling
  if(nrow(df_history) == 0){
    return(c(6,11))
  }
  else if(nrow(df_history) < 5){
    # calculate based on player1_claim history
    history_policies = sapply(df_history$`Claim 1`, function(x) claim_mapping[x])
    return(c(6,11) - add(history_policies))
  }
  # after reshuffling
  else if(nrow(df_history) == 5){
    # calculate based on outcome history
    history_outcomes = sapply(df_history$`Outcome`, function(x) claim_mapping[x])
    return(c(6,11) - add(history_outcomes))
  }
  else{
    # calculate based on outcome history up to row 5 and player1_claim starting at round 6
    history_outcomes = sapply(df_history[1:5,]$`Outcome`, function(x) claim_mapping[x])
    history_policies = sapply(df_history[5:nrow(df_history),]$`Claim 1`, function(x) claim_mapping[x])
    history_combined = c(history_outcomes, history_policies)
    return(c(6,11) - add(history_combined))
  }
}

generate_prob_table <- function(remaining_policies){
  # get remaining liberal policies, and remaining fascist policies, and total policies
  l = remaining_policies[1]
  f = remaining_policies[2]
  n = l + f
  # calculate probabilities
  prob_3l = choose(l,3)/choose(n,3)
  prob_2l1f = choose(l,2)*choose(f,1)/choose(n,3)
  prob_1l2f = choose(l,1)*choose(f,2)/choose(n,3)
  prob_3f = choose(f,3)/choose(n,3)
  prob_1fplus = 1 - prob_3l
  prob_2fplus = prob_1l2f + prob_3f
  prob_1lplus = 1 - prob_3f
  prob_2lplus = prob_2l1f + prob_3l
  # create data table
  Event = c("3L","2L+1F","1L+2F","3F","At least 1F","At least 2F","At least 1L","At least 2L")
  Probability = c(prob_3l,prob_2l1f,prob_1l2f,prob_3f,prob_1fplus,prob_2fplus,prob_1lplus,prob_2lplus)
  Percentage = sapply(Probability, function (x) paste(toString(round(x*100,2)),'%',sep=''))
  df = data.frame(Event, Percentage)
  return(df)
}

undo_history <- function(df){
  df_history_new = head(df,-1)
  return(df_history_new)
}

calculate_projected_prob <- function(remaining_policies,claim){
  # get remaining liberal policies, and remaining fascist policies, and total policies
  l = remaining_policies[1]
  f = remaining_policies[2]
  n = l + f
  # return the corresponding probability based on the claim
  if(claim == "3L"){
    p = choose(l,3)/choose(n,3)
    percent = paste(toString(round(p*100,2)),'%',sep='')
    return(percent)
  }
  else if(claim == "2L+1F"){
    p = choose(l,2)*choose(f,1)/choose(n,3)
    percent = paste(toString(round(p*100,2)),'%',sep='')
    return(percent)
  }
  else if(claim == "1L+2F"){
    p = choose(l,1)*choose(f,2)/choose(n,3)
    percent = paste(toString(round(p*100,2)),'%',sep='')
    return(percent)
  }
  else{
    p = choose(f,3)/choose(n,3)
    percent = paste(toString(round(p*100,2)),'%',sep='')
    return(percent)
  }
}

update_history <- function(df_history, player1, player1_claim, player2, player2_claim, outcome){
  remaining_policies = calculate_remaining_policies(df_history)
  projected_prob = calculate_projected_prob(remaining_policies,player1_claim)
  new_row = c(player1, player1_claim, player2, player2_claim, outcome, projected_prob)
  df_history_new = rbind(df_history,new_row)
  df_history_new <- data.frame(lapply(df_history_new, as.character), stringsAsFactors=FALSE)
  colnames(df_history_new) = c("Player 1", "Claim 1", "Player 2", "Claim 2" , "Outcome", "Claim 1 Projected Probability")
  return(df_history_new)
}


# intialize dataframe to store match history
df_history = data.frame(matrix(ncol = 6, nrow = 0))
colnames(df_history) = c("Player 1", "Claim 1", "Player 2", "Claim 2" , "Outcome", "Claim 1 Projected Probability")

# intialize dataframe to store projected probabilities
df_prob = generate_prob_table(c(6,11))

# ---------------- Server ----------------

shinyServer(function(input, output) {
  
  output$probability_table <- DT::renderDataTable(df_prob)
  output$history_table <- DT::renderDataTable(df_history)
  
  observeEvent(input$update, {
    df_history <<- update_history(df_history,input$player1,input$player1_claim,input$player2,input$player2_claim,input$outcome)
    df_prob <<- generate_prob_table(calculate_remaining_policies(df_history))
    reset("player1")
    reset("player1_claim")
    reset("player2")
    reset("player2_claim")
    
    output$probability_table <- DT::renderDataTable(df_prob)
    output$history_table <- DT::renderDataTable(df_history)
  })
  
  observeEvent(input$undo, {
    df_history <<- undo_history(df_history)
    df_prob <<- generate_prob_table(calculate_remaining_policies(df_history))
    reset("player1")
    reset("player1_claim")
    reset("player2")
    reset("player2_claim")
    
    output$probability_table <- DT::renderDataTable(df_prob)
    output$history_table <- DT::renderDataTable(df_history)
  })
  
  observeEvent(input$reset, {
    df_history <<- data.frame(matrix(ncol = 6, nrow = 0))
    colnames(df_history) = c("Player 1", "Claim 1", "Player 2", "Claim 2" , "Outcome", "Claim 1 Projected Probability")
    df_prob <<- generate_prob_table(c(6,11))
    reset("player1")
    reset("player1_claim")
    reset("player2")
    reset("player2_claim")
    
    output$probability_table <- DT::renderDataTable(df_prob)
    output$history_table <- DT::renderDataTable(df_history)
  })
  
})
