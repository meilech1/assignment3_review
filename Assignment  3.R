library(magrittr)

# Starting screen, prompting user to enter if they are ready to play
# Using a while() loop, we can control user input such that we require user to just hit the enter key on their keyboard in order to start
# Any other input by user on this screen is negated and forces user back to starting screen until they just hit enter (blank input)
# using (TRUE) as the condition creates an infinite loop until break is executed

while (TRUE) {
  starting_input <- readline("Welcome to Ramiz's Hangman game. Press Enter to start the game")
  if (nchar(starting_input) == 0) {
    cat("The rules are simple, guess individual letters or the entire word. For every incorrect guess, you lose a life")
    break
  }
  
  print("Invalid input, please press Enter to start the game")
}

# Choose number of lives 
# Maxed it at 99 for those who are really bad at hangman

while (TRUE) {
  lives <- readline("Please choose your desired # of lives (you may choose any natural number between 1 and 99): ")
  
  if (!grepl("^[1-9][0-9]{0,1}$|^10$", lives)) {
    print("Invalid input, please enter a natural number between 1 and 99")
  } else {
    lives <- as.integer(lives)
    break
  }
}

# Reading word list for hangman game using external .txt file
# I chose read.csv so I can indicate parameters such as header and colClasses
# header = false makes sure it does not read our first column as a header and treats is as an actual word for the game
# colClasses sets our words to be identified as characters for the purpose of this game

dictionary <- read.csv("Assignment 3 dictionary.txt", header = FALSE, colClasses = "character")

# Assigning variable to one element from the dictionary using sample()
# we sample using the dictionary variable which stores all the words from the txt file and we choose the parameter 1 to sample one word only
# word_length is stored as a variable to paste later when providing user with the word information

guessable_word <- sample(dictionary, 1)
word_length <- nchar(guessable_word)

# Informing user about word chosen using print(paste()) to combine strings and variables

cat("The word you must guess is", word_length, "letters long!")

# Optional hint feature that users can select if they want
# Using while loop again which remains true until break
# if user inputs Y or y, then an additional hint is provided based on the guessable_word variable in said session, then breaks
# if user inputs N or n, then game proceeds without additional hint
# Any other input is negated and looped back to readline()

while (TRUE) {
  optional_hint <- readline("Would you like to receive an additional hint? (Y/N): ")
  if (optional_hint %in% c("Y", "y")) {
    if (guessable_word == "SPIDERMAN") {
      print("Hint: Superhero")
    } else if (guessable_word %in% c("PASTA", "WATER", "PIZZA")) {
      print("Hint: Food / drink!")
    } else if (guessable_word %in% c("AIRPLANE", "MEXICO", "PYRAMID")) {
      print("Hint: Travel and Geography!")
    } else if (guessable_word %in% c("ELEPHANT", "CROCODILE")) {
      print("Hint: Animals!")
    } else {
      print("Hint: Nature!")
    }
    break
  } else if (optional_hint %in% c("N", "n")) {
    break
  } else {
    print("Invalid input. Please enter 'Y' or 'N'.")
  }
}

# Main part of the game variables 

blank_word <- strrep("_ ", word_length)
word_solved <- FALSE
previous_guesses <- list()


# Main while() loop for game iteration 
# condition of the while loop is when lives are greater than 0 and the word is NOT solved

while (lives > 0 && !word_solved) {
  
  # print all previous guesses made by user at the start of each turn, collapse = ", " for better formatting
  print(paste("Previous guesses:", paste(previous_guesses, collapse = ", ")))
  
  # take user input for guess 
  user_input <- readline(paste("Here is your current status:\n", blank_word, "\nPlease guess a letter (or the entire word!): "))
  
  # checks if the users input was previously guessed
  # if it was then they are returned to input without losing a life
  # if not, loop proceeds 
  if (user_input %in% previous_guesses) {
    print("You have already guessed that. Please enter a new guess.")
    next
  }
  
  # checks if user input is exactly 1 letter character
  # had to copy paste the grepl() function from stack overflow
  if (is.character(user_input) && nchar(user_input) == 1 && grepl("^[A-Za-z]$", user_input)) {
    
    # checks if user input is inside the mystery word by splitting the string and checking input against it
    if (toupper(user_input) %in% strsplit(toupper(guessable_word), "")[[1]]) {
      print("The letter you inputted is part of the word!")
      
      # Update the blank word with the correctly guessed letter(s)
      indices <- which(strsplit(toupper(guessable_word), "")[[1]] == toupper(user_input))
      for (i in indices) {
        blank_word <- substring(blank_word, 1, (i - 1) * 2) %>%
          paste0(toupper(user_input), " ") %>%
          paste0(substring(blank_word, (i - 1) * 2 + 3, nchar(blank_word)))
      }
      # If letter was an incorrect guess, lives are dropped by 1
    } else {
      print("Sorry, the letter you inputted is not part of the word.")
      lives <- lives - 1
    }
    # if user correctly guesses the word
  } else if (is.character(user_input) && toupper(user_input) == toupper(guessable_word)) {
    print("Congrats, you solved the word!")
    word_solved <- TRUE
    # if user inputs anything but a letter or word guess
  } else {
    if (!is.character(user_input) || !grepl("^[A-Za-z]+$", user_input)) {
      print("Invalid input. Please enter a single letter or the entire word.")
      # if the word guessed was incorrect
    } else {
      print(paste("Sorry, the word you guessed was incorrect."))
      lives <- lives - 1
    }
  }
  
  previous_guesses <- c(previous_guesses, user_input)
  
  # Check if all letters have been guessed correctly
  if (gsub(" ", "", blank_word) == guessable_word) {
    print("Congrats, you solved the word!")
    word_solved <- TRUE
  }
  
  # Provide update on number of lives remaining
  print(paste("Lives remaining:", lives))
}

# Ending game
if (!word_solved) {
  print(paste("Sorry, you ran out of lives. The word was:", guessable_word))
}
