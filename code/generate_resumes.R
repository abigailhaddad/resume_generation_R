library(openai)
library(readxl)
library(jsonlite)
library(dplyr)
library(purrr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

load_api_key_to_env <- function(path) {
  key_file <- file(path, "r")
  api_key <- readLines(key_file, n = 1)
  close(key_file)
  
  Sys.setenv(OPENAI_API_KEY = api_key)
}

compose_prompt <- function(row) {
  overall_prompt <- "Please compose a sample resume using real years and institutions for the following person. Please go into a lot of detail and make this longer than you would think to, particularly the descriptions of what they're doing at their jobs."
  prompt <- paste0(overall_prompt, row[['Name']], " currently has the job title of ", row[['Current.Job']], ". They have an undergraduate degree in ", row[['Academic.Background']], ". This is a description of them: ", row[['Descriptions']])
  print(prompt)
  return(prompt)
}

get_response <- function(prompt, engine) {
  MAX_RETRIES <- 5
  attempts <- 0
  
  while (attempts < MAX_RETRIES) {
    tryCatch({
      messages <- list(list("role" = "user", "content" = prompt))
      response <- openai::create_chat_completion(model = engine, messages = messages)
      print(str(response))
      
      if(!is.null(response$choices) && nrow(response$choices) > 0 && !is.null(response$choices$message.role[1]) && !is.null(response$choices$message.content[1])){
        return(response$choices$message.content[1])
      } else {
        print("Unexpected Error")
        return('')
      }
      
    }, error = function(e) {
      attempts <- attempts + 1
      print(paste0("Error processing prompt. Engine: ", engine, ", Prompt: ", prompt, ", Error: ", substr(e$message, 1, 100)))
      Sys.sleep(10)
    })
  }
}


process_responses <- function(df, engine) {
  df <- df %>% mutate(response_gpt = map_chr(seq_len(n()), ~get_response(compose_prompt(df[.x,]), engine)))
  return(df)
}

main <- function(input_file, output_file, n = NULL) {
  load_api_key_to_env("../../key/key.txt")
  api_key <- Sys.getenv("OPENAI_API_KEY")

  
  df <- read.csv(paste0("../data/", input_file, ".csv"))
  df <- process_responses(df, 'gpt-4')
  
  write.csv(df, file = paste0("../data/", output_file, ".csv"), row.names = FALSE)
  return(df)
}

df= main("fake resumes", "generated_in_r", n=1)

