
#' String Matching Function with Negation Handling
#' 
#' This function performs string matching using both fuzzy and exact matching,
#' with support for excluding terms and handling negations within a specified character range.
#' It is designed for case-insensitive text processing and allows the user to specify 
#' how matched and unmatched strings are replaced.
#' 
#' @param data A data frame containing the text data to be processed.
#' @param column A string representing the column name in the data frame where text matching will be applied.
#' @param include A vector of terms or phrases to include in the matching process (case-insensitive).
#' @param exclude_list A vector of terms or phrases to explicitly exclude from matches (case-insensitive).
#' @param replace_match_with A string used to replace the text if a match is found. Defaults to retaining the original text if not provided.
#' @param replace_unmatched_with A string used to replace the text if no match is found. Defaults to retaining the original text if not provided.
#' @param max_dist A numeric value specifying the maximum allowable distance for fuzzy matching, where 0 represents exact matches only and larger values allow greater flexibility. Default is 0.2.
#' @param negation_range An integer specifying the number of characters before and after the inclusion term to search for negation words. Spaces are counted as characters. Default is 20.
#' @param negation_list A vector of custom negation words to look for around inclusion terms. If not provided, no negation handling is applied.
#' @param replace_by_include_term Enable replacement by the matched include term
#' @param NA_list Values treated as NA, e.g., ".", "--", "", " ". Default: c(".", "--", "", " ").
#' 
#' @return A data frame with the processed text column modified according to the specified matching, exclusion, and negation rules.
#' 
#' @details
#' The function performs the following steps:
#' 1. Converts the text in the specified column to uppercase to handle case-insensitive matching.
#' 2. Applies fuzzy and exact matching to identify rows that include specified terms.
#' 3. Excludes rows that match terms in the exclusion list.
#' 4. Searches for negation terms within the specified character range around inclusion terms.
#' 5. Combines these conditions to determine the final matches and applies the specified replacements to the column.
#' 
#' @examples
#' data <- data.frame(text = c("ABDOMINAL INFECTION with PAIN",
#'                             "NO ABDOMINAL INFECTION IS SUSPECTED", 
#'                             "ABDOMINAL infection CONFIRMED", 
#'                             "DENIES ABDOMINAL PAIN", 
#'                             "without ABDOMINAL INFECTION DETECTED", 
#'                             "ABDOMINAL PAIN IS NOT SUSPECTED", 
#'                             "ABDOMINAL MASS DETECTED"))
#' 
#' result <- string_match_HM(
#'   data = data,
#'   column = "text",
#'   include = c("MASS", "TUMOr", "ABDOMINAL INFECTION WITH NO PAIN", "ABDOMINAL"),
#'   exclude_list = c("ABDOMINAL PAIN"),
#'   replace_match_with = "Wanted",  
#'   replace_unmatched_with = "Not wanted", 
#'   max_dist = 0.2,
#'   negation_range = 20,
#'   negation_list = c("NO", "Not,"suspected", "unlikely")
#' )
#' print(result)
#' 
#' @author Harun Mazumder


# Load required packages
if(!require(stringdist)) install.packages("stringdist", dependencies = TRUE)
if(!require(stringr)) install.packages("stringr", dependencies = TRUE)

#***********Function to do string matching using fuzzy algorithm and exact matching**********

string_match_HM <- function(data, column, include, exclude_list = NULL, replace_match_with = NULL, 
                            replace_unmatched_with = NULL, max_dist = 0.2, 
                            negation_range = 20, negation_list = NULL, replace_by_include_term = FALSE,
                            NA_list = NULL) {
  
  #function to escape special characters for regular expression matching
  escape_regex <- function(term) {
    gsub("([\\^\\$\\.\\|\\?\\*\\+\\(\\)\\[\\]\\\\])", "\\\\\\1", term)
  }
  
  #convert column text to uppercase; also deal with NA
  data[[column]] <- ifelse(
    data[[column]] %in% NA_list | is.na(data[[column]]), NA, toupper(as.character(data[[column]]))
  )
  
  if (is.null(include)) stop("Include terms must be provided for matching.") else include = toupper(trimws(include))
  if (!is.null(exclude_list)) exclude_list = toupper(exclude_list)

  #handle negation if negation list is provided
  neg_patterns <- if (!is.null(negation_list)) {
    paste(paste0("\\b", toupper(negation_list), "\\b"), collapse = "|")
  } else {
    NULL
  }
  
  #fuzzy match for inclusion terms (including exact match)
  
  include_matches <- lapply(data[[column]], function(text) {
    if (is.na(text)) return(list(closest_match = NA, refined_match = NA, match_found = FALSE))
    
    # Calculate distances using stringdist
    distances <- sapply(paste0("\\b", include, "\\b"), function(inc) {
      stringdist::stringdist(text, inc, method = "jw") 
    })
    
    #identify the closest match
    min_dist <- min(distances)
    closest_match <- include[which.min(distances)]
    match_found <- min_dist < max_dist || grepl(paste0("\\b", closest_match, "\\b"), text, ignore.case = TRUE)

    if (match_found) {
      match_terms <- unlist(strsplit(closest_match, " ")) # Split closest match into words
      
      #filter words that exist in the text
      # partial_matches <- match_terms[sapply(match_terms, function(term) {
      #   grepl(paste0("\\b", term, "\\b"), text, ignore.case = TRUE)
      # })]

      # Function to escape special characters in the term for regex matching
      escape_special_chars <- function(term) {
        #escape all special characters for regular expressions except for the parentheses
        return(gsub("([.*+?^=!:${}()|\\[\\]\\\\/-])", "\\\\\\1", term))
      }
      
      match_terms <- unlist(strsplit(closest_match, " "))  
      escaped_match_terms <- sapply(match_terms, escape_special_chars)
      partial_matches <- escaped_match_terms[sapply(escaped_match_terms, function(term) {
        # Use grepl with fixed = TRUE for literal matching (case-insensitive is managed manually)
        grepl(term, text, fixed = TRUE)
      })]
      
      
      refined_match <- if (length(partial_matches) > 0) {
        paste(partial_matches, collapse = " ")
      } else {
        closest_match
      }
    } else {
      refined_match <- NA
    }
    
    return(list(closest_match = closest_match, refined_match = refined_match, match_found = match_found))
  })
  
  
  #check for exact matches (full inclusion term match)
  exact_include_matches <- sapply(data[[column]], function(text) {
    if (is.na(text)) return(FALSE)
    any(sapply(include, function(inc) {
      grepl(paste0("^", paste0("\\b", inc, "\\b"), "$"), text)
    }))
  })
  
  #exclude exact or partial matches (based on user provided exclude_list)
  exclude_matches <- if (!is.null(exclude_list)) {
    sapply(data[[column]], function(text) {
      if (is.na(text)) return(FALSE)
      any(sapply(paste0("\\b", exclude_list, "\\b"), function(exc) grepl(exc, text, ignore.case = TRUE)))
    })
  } else rep(FALSE, nrow(data))

  # #negation handling (checking for negation within a range before/after inclusion term)
  #since closest match was already computed in the include_matches step
  #Now, leverage include_matches outputs for negation handling
  negation_matches <- if (!is.null(neg_patterns)) {
    sapply(1:nrow(data), function(i) {
      text <- data[[column]][i]
      if (is.na(text)) return(FALSE)
      
      #extract the closest match from the include_matches result
      closest_match_info <- include_matches[[i]]
      closest_match <- closest_match_info$refined_match #use refined match (matched part in "include" and text)
      match_found <- closest_match_info$match_found
      
      #if no match is found, return FALSE for negation match
      if (!match_found) return(FALSE)
      
      #locate positions of the closest match
      include_positions <- str_locate_all(text, closest_match)[[1]][, 1]
      
      if (length(include_positions) == 0) return(FALSE)
      
      for (pos in include_positions) {
        before_range <- substr(text, max(1, pos - negation_range), pos - 1)
        after_range <- substr(text, pos + nchar(closest_match), pos + nchar(closest_match) + negation_range - 1)
        
        #check for negation patterns in the before and after ranges
        negation_before <- str_detect(before_range, neg_patterns)
        negation_after <- str_detect(after_range, neg_patterns)
        
        #if any negation word is found in the range, return TRUE for negation match
        if (negation_before || negation_after) {
          return(TRUE)
        }
      }
      return(FALSE)
    })
  } else rep(FALSE, nrow(data))
  
  
  #combine all match conditions: inclusion term found, no exclusion term, and no negation
  #Note: if inclusion term fully matched with the text data, then do not apply exclusion
  final_matches <- mapply(function(match_info, exact_match, exclusion, negation) {
    (match_info$match_found & !exclusion & !negation) | exact_match
  }, include_matches, exact_include_matches, exclude_matches, negation_matches)
  
  #handling replacement:
  
  data$new_column <- ifelse(
    is.na(data[[column]]), "NA",
    ifelse(
      final_matches,  #if it's a match
      if (!is.null(replace_match_with)) {
        replace_match_with
      } else if (replace_by_include_term) {
        #apply the replacement logic for the matched terms
        sapply(1:nrow(data), function(i) {
          if (is.na(data[[column]][i])) return(NA)
          
          term <- data[[column]][i]  
          closest_match_info <- include_matches[[i]]  #get closest match info for this row
          closest_match <- closest_match_info$closest_match  #Get the closest match. 
          #(note: we used refined closest match in negation only to calculate the positions; not to replace)
          match_found <- closest_match_info$match_found  
          
          if (match_found) {
            return(closest_match)
          } else {
            #if no match found, keep the original term or apply replace_unmatched_with
            if (!is.null(replace_unmatched_with)) {
              return(replace_unmatched_with)
            } else {
              return(term)
            }
          }
        })
      } else {
        data[[column]]
      },
      if (!is.null(replace_unmatched_with)) replace_unmatched_with else data[[column]]
    )
  )
  
  
  return(data)
}


data <- data.frame(text = c( "PHY-SURGICAL DENIES ABDOMINAL PAIN","PHY-SURGICAL", 
                             "PHY-NON-SURGICAL", "PHY Surgical", 
                            "ABDOMINAL INFECTION WITH NO PAIN",
                            "NO ABDOMINAL INFECTION IS SUSPECTED",
                            "ABDOMINAL infection CONFIRMED",
                            "DENIES ABDOMINAL PAIN", NA, "", "*",
                            "without ABDOMINAL INFECTION DETECTION",
                            "ABDOMINAL PAIN IS NOT SUSPECTED",
                            "ABDOMINAL MASS DETECTED",
                           "ABDOMINAL INFECTION But denies pain","EMPIRIC (UNKNOWN SOURCE)"))


#call function
#make sure to include terms in the order of relevance.
#use this version if you want exact/partial and jw implemented in both matching and replacement.

result <- string_match_HM(
  data = data,
  column = "text",
  include = c("PHY-SURGICAL DENIES ABDOMINAL PAIN ","Phy surgical", "ABDOMINAL INFECTION denied abd pain",
              "ABDOMINAL INFECTION with No pain", "ABDOMINAL Infection", "ABDOMiNal MASS identified but no infection",
              "EMPIRIC (UNKNOWN SOURCE)"), 
  exclude_list = c("ABDOMINAL PAIN"),
  replace_match_with = NULL,  
  replace_unmatched_with = "not matched",
  max_dist = 0.2,
  negation_range = 20,
  negation_list = c("No", "no abdominal infection", "NOT", "unlikely", "Denies", "DENIED", "without", "doesn't", "does not"),
  replace_by_include_term = TRUE,
  NA_list = c(".", "-","--", "", " ")
)

#result
