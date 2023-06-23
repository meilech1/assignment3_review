library(magrittr) #Comment: Perhaps add a quick comment title or something "Hangman" related for the user. 

# Starting screen, prompting user to enter if they are ready to play
# Using a while() loop, we can control user input such that we require user to just hit the enter key on their keyboard in order to start
# Any other input by user on this screen is negated and forces user back to starting screen until they just hit enter (blank input)
# using (TRUE) as the condition creates an infinite loop until break is executed

while (TRUE) {
  starting_input <- readline("Welcome to Ramiz's Hangman game. Press Enter to start the game")
  
  #' Comment: I like how you added an "Press Enter to Start" prompt. I think it's a great way to
  #' prompt the user to engage with the game, prior to executing the core logic of the game. Nicely done!
  
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
  
  #' Comment: Again, I like how you prompted the user and asked them how many lives they would like. This is essentially
  #' a way for the player to choose their desired difficulty. What could be an interesting extension to this is
  #' to create 'levels' of difficulty. So the user can choose from "easy", "medium" or "hard" which might be easier
  #' for them to pick, rather than an exact # of lives. That being said, this feature is really neat and goes above
  #' the requirements for the assignment, so I think it's also great as-is. 
  
  if (!grepl("^[1-9][0-9]{0,1}$|^10$", lives)) {
    
    #' Comment: Nice use of REGEX here (even before we covered it in class). Taking in what we learned from
    #' our lesson on June 23, I think the first argument could be shorted to:^([1-9]|[1-9][0-9])$. This would
    #' make the code even more sleek. I know this wasn't covered yet, just using it as an exercise for myself lol.
    
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

#' Comment: Great comments here from Lines 47 - 59. It was really clear what you did and WHY you did it.
#' I think that's really important especially when it comes to reading external files and setting up data reading.
#' So I just want to highlight that because I think it's a great practice. 


# Informing user about word chosen using print(paste()) to combine strings and variables

cat("The word you must guess is", word_length, "letters long!")

# Optional hint feature that users can select if they want
# Using while loop again which remains true until break
# if user inputs Y or y, then an additional hint is provided based on the guessable_word variable in said session, then breaks
# if user inputs N or n, then game proceeds without additional hint
# Any other input is negated and looped back to readline()

while (TRUE) {
  optional_hint <- readline("Would you like to receive an additional hint? (Y/N): ")
  
  #' Comment: I like how you added this extra on top of what was required for the assignment. I think
  #' it adds a nice extra dimension to the game. I used this when I was playing your game because sometimes
  #' I got stuck, so it enhanced the end user experience. Great job!
  
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

#' Comment: One thing I would state for the hint section from lines 83 to 101 is
#' that the words are encoded statically. So what the means is if you changed your 
#' dictionary text file (added words, removed words, changed words), you would have to
#' update the strings that are encoded in the guess check block above. Again, since this was
#' an extra feature it's not a problem at all! More 'food for thought' but I think an
#' interesting follow-up could be to create a dictionary 2.0 which consists of two columns.
#' One column could be the word and the other the hint or hint category. That way the logic
#' above could be adjusted to pull in the hint data dynamically from the file and print it without
#' having to adjust the code itself. Again, this is many many levels beyond what was required. Nice job!

# Main part of the game variables 

blank_word <- strrep("_ ", word_length)
word_solved <- FALSE
previous_guesses <- list()




# Main while() loop for game iteration 
# condition of the while loop is when lives are greater than 0 and the word is NOT solved

while (lives > 0 && !word_solved) {
  
  # print all previous guesses made by user at the start of each turn, collapse = ", " for better formatting
  print(paste("Previous guesses:", paste(previous_guesses, collapse = ", ")))
  
  # Comment: I like how you print the previous guesses for each round. It's easy for the player to keep track
  # this way and avoid duplicate guesses. Nice job!
  
  # take user input for guess 
  user_input <- readline(paste("Here is your current status:\n", blank_word, "\nPlease guess a letter (or the entire word!): "))
  
  # checks if the users input was previously guessed
  # if it was then they are returned to input without losing a life
  # if not, loop proceeds 
  if (user_input %in% previous_guesses) {
    print("You have already guessed that. Please enter a new guess.")
    next
    
    # Comment: The use of next on line 141 is really good. If the user's guess appears in previous_guesses
    # then the next block will bring the code to the next iteration of while(). This makes the code more
    # efficient as the rest of the code below in the while() block won't excute. This is good for memory!
    
  }
  
  # checks if user input is exactly 1 letter character
  # had to copy paste the grepl() function from stack overflow
  if (is.character(user_input) && nchar(user_input) == 1 && grepl("^[A-Za-z]$", user_input)) {
    
    # Comment: Great use of REGEX/grepl on Line 151 again! It makes the code very efficient and covers the defensive
    # programming requirements. Awesome!
    
    # checks if user input is inside the mystery word by splitting the string and checking input against it
    if (toupper(user_input) %in% strsplit(toupper(guessable_word), "")[[1]]) {
      print("The letter you inputted is part of the word!")
      
      # Update the blank word with the correctly guessed letter(s)
      indices <- which(strsplit(toupper(guessable_word), "")[[1]] == toupper(user_input))
      
      #' Comment: In the lines above you use strsplit(toupper(guessable_word)) a couple of times.
      #' I think where you set up your game variables on line 115 you may want to set up a variable
      #' for the split word, something like split.word that calls this function. This prevents you
      #' from having to make these nested function calls repeatedly below. 
      
      for (i in indices) {
        blank_word <- substring(blank_word, 1, (i - 1) * 2) %>%
          paste0(toupper(user_input), " ") %>%
          paste0(substring(blank_word, (i - 1) * 2 + 3, nchar(blank_word)))
      }
      #' Comment: for the for() loop above on line 168, I believe you are replacing characters in blank_word
      #' at position: indices followed by a space (first paste0 call). As such, blank_word is updated at
      #' each iteration with the appropriate index in indices. It took some time to understand the logic
      #' especially with the (i-2)*2 position argument, so I would maybe add some comments here to explain
      #' the logic in case a user has to make adjustments / to better understand.
      
      # If letter was an incorrect guess, lives are dropped by 1
    } else {
      print("Sorry, the letter you inputted is not part of the word.")
      lives <- lives - 1
    }
    # if user correctly guesses the word
  } else if (is.character(user_input) && toupper(user_input) == toupper(guessable_word)) {
    print("Congrats, you solved the word!")
    word_solved <- TRUE
    
    # Comment: Great use of the word_solved boolean. This is one of the conditions for the while() loo
    # that keeps the guess rounds running, so a smart way to break that condition and end it. 
    
    # if user inputs anything but a letter or word guess
  } else {
    if (!is.character(user_input) || !grepl("^[A-Za-z]+$", user_input)) {
      print("Invalid input. Please enter a single letter or the entire word.")
      # if the word guessed was incorrect
    } else {
      print(paste("Sorry, the word you guessed was incorrect."))
      lives <- lives - 1
      
      #' Comment: I see that you allow users to guess words of different lengths than the target mystery word.
      #' Since that's by design, it could be a nice idea to add one more defensive programming if statement in 
      #' the section above (e.g. between if one 194 and the else on 197) to check if nchar(user_input) > 1 & !=
      #' guessable_word. The program could then prompt the user with something like "the word you guessed ____ " is 
      #' incorrect. Hint: it is not the correct length which is _ characters long." 
    }
  }
  
  previous_guesses <- c(previous_guesses, user_input)
  
  # Check if all letters have been guessed correctly
  if (gsub(" ", "", blank_word) == guessable_word) {
    print("Congrats, you solved the word!")
    word_solved <- TRUE
  }
  
  # Comment: The block above on line 212 is really good because you are running a check to see if the
  # main guessing while() loop should be broken. That is the case when all letters have been gussed. This
  # is an important aspect of the functionality of this game. 
  
  # Provide update on number of lives remaining
  print(paste("Lives remaining:", lives))
}

# Ending game
if (!word_solved) {
  print(paste("Sorry, you ran out of lives. The word was:", guessable_word))
}


#' Comments Overall:
#' Firstly, great job! This game met all the required criteria for the Hangman game and substantially exceeded these requirements.
#' The use of hints, setting desired lives and pressing enter to start were wonderful additions that enhanced the user experience. 
#' I think it shows a real appreciation for user flows and programming, so really really well done!! Stylistically, the code is
#' very clean and syntax rules are followed well. One slight improvement could be some extended comments on more challenging code blocks
#' such as on Line 168. On lines 47-59 you added great comments for loading data, so something similar for Line 168 would be great for
#' someone reading your code to understand the design decisions. The other suggestions I've added as in-line comments are really
#' just extensions to your existing extensions. For example, making the hint code more dynamic or adding pre-set levels of difficulty.
#' None of these conversations are related to what was required for the code, but just cool extensions I think would make this game worth paying for ;)
#' I also tried various bad user inputs and your defensive programming was set up really well. 
#' The core functionality of the code is great and it was a pleasure to review this code. Thank you!



