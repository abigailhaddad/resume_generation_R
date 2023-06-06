# Libraries
library(openai)
library(jsonlite)
library(dplyr)
library(purrr)

setwd(dirname(rstudioapi::getSourceEditorContext()$path))

#' Load OpenAI API Key to Environment Variable
#' 
#' @param path: file path to the API Key
#' 
#' This function reads the API Key from a file and sets it as an environment variable.
load_api_key_to_env <- function(path) {
  key_file <- file(path, "r")
  api_key <- readLines(key_file, n = 1)
  close(key_file)
  
  Sys.setenv(OPENAI_API_KEY = api_key)
}

#' Compose Prompt for OpenAI GPT-4 model
#' 
#' @param row: A row from the dataframe containing details for the prompt
#' 
#' This function creates a custom prompt for the GPT-4 model based on the details provided in the input row.
compose_prompt <- function(row) {
  overall_prompt <- "Please compose a sample resume using real years and institutions for the following person. Please go into a lot of detail and make this longer than you would think to, particularly the descriptions of what they're doing at their jobs."
  prompt <- paste0(overall_prompt, row[['Name']], " currently has the job title of ", row[['Current.Job']], ". They have an undergraduate degree in ", row[['Academic.Background']], ". This is a description of them: ", row[['Descriptions']])
  return(prompt)
}

#' Get Response from OpenAI GPT-4 model
#' 
#' @param prompt: The prompt for the GPT-4 model
#' @param engine: The OpenAI engine to use (GPT-4 in this case)
#' 
#' This function sends the prompt to the GPT-4 model and retrieves the response.
get_response <- function(prompt, engine) {
  MAX_RETRIES <- 5
  attempts <- 0
  
  while (attempts < MAX_RETRIES) {
    tryCatch({
      messages <- list(list("role" = "user", "content" = prompt))
      response <- openai::create_chat_completion(model = engine, messages = messages)
      
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

#' Process Responses from OpenAI GPT-4 model
#' 
#' @param df: The dataframe containing details for the prompts
#' @param engine: The OpenAI engine to use (GPT-4 in this case)
#' @param n: Number of samples to process
#' 
#' This function creates a random sample from the dataframe, sends prompts to the GPT-4 model, and saves the responses in the dataframe.
process_responses <- function(df, engine, n = nrow(df)) {
  sampled_df <- df %>% sample_n(n)
  sampled_df <- sampled_df %>% mutate(response_gpt = map_chr(1:n, ~get_response(compose_prompt(sampled_df[.x,]), engine)))
  return(sampled_df)
}

#' Main Function to Generate Resumes using OpenAI GPT-4 model
#' 
#' @param input_file: The input CSV file name (without extension) containing details for the prompts
#' @param output_file: The output CSV file name (without extension) where the generated resumes will be saved
#' @param n: Number of samples to process
#' 
#' This function reads the input CSV, generates resumes using the GPT-4 model, and writes the results to the output CSV.
main <- function(input_file, output_file, n = NULL) {
  load_api_key_to_env("../../key/key.txt")
  api_key <- Sys.getenv("OPENAI_API_KEY")
  
  df <- read.csv(paste0("../data/", input_file, ".csv"))
  
  if (is.null(n)) {
    # Process all rows if n is NULL
    df <- process_responses(df, 'gpt-4', n = nrow(df))
  } else {
    # Process n rows if n is not NULL
    df <- process_responses(df, 'gpt-4', n = n)
  }
  
  write.csv(df, file = paste0("../data/", output_file, ".csv"), row.names = FALSE)
  return(df)
}


df= main("fake resumes", "generated_in_r")
