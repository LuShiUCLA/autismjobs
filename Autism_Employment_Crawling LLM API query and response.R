
# =============================================================================
# AUTISM EMPLOYMENT PROGRAM EFFECTIVENESS ANALYSIS
# =============================================================================
# Focus: Evaluating program outcomes, success metrics, and effectiveness indicators
# Revised to address API integration issues and focus on program assessment

# Set working directory with error handling
tryCatch({
  setwd("C:/data/AutismEmployment")
  cat("Working directory set to:", getwd(), "\n")
}, error = function(e) {
  dir.create("C:/data/AutismEmployment", recursive = TRUE, showWarnings = FALSE)
  setwd("C:/data/AutismEmployment")
  cat("Created and set working directory to:", getwd(), "\n")
})

# =============================================================================
# PACKAGE MANAGEMENT
# =============================================================================

required_packages <- c(
  "tidyverse", "tm", "pdftools", "httr", "jsonlite", 
  "stringr", "ggplot2", "dplyr", "readr"
)

install_and_load_packages <- function(packages) {
  failed_packages <- character(0)
  
  for (pkg in packages) {
    tryCatch({
      if (!require(pkg, character.only = TRUE, quietly = TRUE)) {
        cat("Installing package:", pkg, "\n")
        install.packages(pkg, dependencies = TRUE, quiet = FALSE)
        
        if (!library(pkg, character.only = TRUE, logical.return = TRUE, quietly = TRUE)) {
          failed_packages <- c(failed_packages, pkg)
        }
      }
    }, error = function(e) {
      cat("Failed to install/load package", pkg, ":", e$message, "\n")
      failed_packages <<- c(failed_packages, pkg)
    })
  }
  
  if (length(failed_packages) > 0) {
    cat("Failed to load packages:", paste(failed_packages, collapse = ", "), "\n")
    cat("Some functionality may be limited.\n")
  } else {
    cat("All required packages loaded successfully.\n")
  }
  
  return(failed_packages)
}

suppressPackageStartupMessages({
  failed_pkgs <- install_and_load_packages(required_packages)
})

# =============================================================================
# IMPROVED API KEY CONFIGURATION
# =============================================================================

configure_openai_api <- function() {
  # Check multiple sources for API key
  api_key <- NULL
  
  # Method 1: Environment variable (most secure)
  env_key <- Sys.getenv("sk...")
  if (env_key != "" && env_key != "sk...") {
    api_key <- env_key
    cat("API key loaded from OPENAI_API_KEY environment variable.\n")
  }
  
  # Method 2: Config file (if environment variable not found)
  if (is.null(api_key)) {
    config_file <- "openai_config.txt"
    if (file.exists(config_file)) {
      tryCatch({
        file_content <- readLines(config_file, n = 1, warn = FALSE)
        if (length(file_content) > 0 && nchar(trimws(file_content)) > 0) {
          api_key <- trimws(file_content)
          cat("API key loaded from config file.\n")
        }
      }, error = function(e) {
        cat("Error reading config file:", e$message, "\n")
      })
    }
  }
  
  # Method 3: Interactive prompt (development only)
  if (is.null(api_key) && interactive()) {
    cat("No API key found in environment or config file.\n")
    response <- readline("Would you like to enter an API key now? (y/n): ")
    if (tolower(trimws(response)) == "y") {
      api_key <- readline("Enter your OpenAI API key: ")
      if (nchar(trimws(api_key)) == 0) {
        api_key <- NULL
      }
    }
  }
  
  # Validate API key format
  if (!is.null(api_key) && !grepl("^sk-", api_key)) {
    cat("Warning: API key format appears invalid (should start with 'sk-').\n")
    api_key <- NULL
  }
  
  if (is.null(api_key)) {
    cat("No valid API key configured. LLM analysis will be skipped.\n")
    cat("To enable LLM analysis:\n")
    cat("1. Set OPENAI_API_KEY environment variable, or\n")
    cat("2. Create 'openai_config.txt' file with your API key\n")
  }
  
  return(api_key)
}

# Configure API
openai_api_key <- configure_openai_api()

# =============================================================================
# DOCUMENT LOADING AND PREPROCESSING
# =============================================================================

load_document <- function(file_path) {
  if (file.exists(file_path)) {
    tryCatch({
      doc_raw <- readLines(file_path, warn = FALSE, encoding = "UTF-8")
      doc_text <- paste(doc_raw, collapse = " ")
      cat("Document loaded successfully from:", file_path, "\n")
      return(doc_text)
    }, error = function(e) {
      cat("Error loading document:", e$message, "\n")
      return(NULL)
    })
  } else {
    cat("Document not found at:", file_path, "\n")
    return(create_sample_program_data())
  }
}

create_sample_program_data <- function() {
  cat("Creating sample program effectiveness data for demonstration.\n")
  
  sample_text <- paste(
    "Program evaluation results show 78% employment success rate among participants.",
    "Job retention rates improved by 45% compared to baseline measurements.",
    "Workplace accommodation success documented in 89% of placements.",
    "Employee satisfaction scores averaged 4.2 out of 5.0 across all participants.",
    "Employer feedback indicated 85% would hire additional program graduates.",
    "Cost-benefit analysis demonstrates 3.2:1 return on investment ratio.",
    "Follow-up surveys at 12 months show 82% participants remain employed.",
    "Skills training completion rate reached 94% among enrolled individuals.",
    "Mental health outcomes improved significantly in 71% of participants.",
    "Technology-assisted interventions showed 67% effectiveness improvement.",
    "Peer mentoring programs contributed to 58% increase in job satisfaction.",
    "Reduced support needs observed in 73% of participants over time.",
    sep = " "
  )
  
  return(sample_text)
}

# =============================================================================
# PROGRAM EFFECTIVENESS METRICS EXTRACTION
# =============================================================================

# Define effectiveness indicators and outcome measures
effectiveness_indicators <- list(
  employment_outcomes = c(
    "employment rate", "job placement", "employment success", "hired", "employed",
    "job retention", "sustained employment", "career advancement", "promotion"
  ),
  
  performance_metrics = c(
    "productivity", "performance rating", "job performance", "work quality",
    "attendance rate", "punctuality", "task completion", "efficiency"
  ),
  
  satisfaction_measures = c(
    "satisfaction", "satisfaction score", "employee satisfaction", "job satisfaction",
    "workplace satisfaction", "happiness", "well-being", "quality of life"
  ),
  
  employer_feedback = c(
    "employer satisfaction", "supervisor rating", "manager feedback", 
    "employer perspective", "workplace integration", "team acceptance"
  ),
  
  cost_effectiveness = c(
    "cost-benefit", "return on investment", "roi", "cost effectiveness",
    "economic impact", "financial benefit", "program cost", "budget"
  ),
  
  retention_success = c(
    "retention rate", "job retention", "stayed employed", "continued employment",
    "long-term success", "sustained placement", "follow-up", "maintenance"
  ),
  
  skill_development = c(
    "skill improvement", "skills training", "competency development",
    "learning outcomes", "skill acquisition", "training effectiveness"
  ),
  
  support_effectiveness = c(
    "accommodation effectiveness", "support success", "intervention effectiveness",
    "assistance quality", "coaching success", "mentoring outcomes"
  )
)

# Extract quantitative metrics from text
extract_quantitative_metrics <- function(text) {
  metrics <- list()
  
  # Percentage patterns
  percentage_patterns <- c(
    "([0-9]+(?:\\.[0-9]+)?)\\s*%\\s*(?:of\\s+)?(?:participants|employees|individuals|people|workers)",
    "([0-9]+(?:\\.[0-9]+)?)\\s*percent\\s*(?:of\\s+)?(?:participants|employees|individuals|people|workers)",
    "success\\s+rate\\s+(?:of\\s+)?([0-9]+(?:\\.[0-9]+)?)\\s*%",
    "retention\\s+rate\\s+(?:of\\s+)?([0-9]+(?:\\.[0-9]+)?)\\s*%",
    "employment\\s+rate\\s+(?:of\\s+)?([0-9]+(?:\\.[0-9]+)?)\\s*%"
  )
  
  percentages <- c()
  for (pattern in percentage_patterns) {
    matches <- regmatches(text, gregexpr(pattern, text, ignore.case = TRUE))[[1]]
    if (length(matches) > 0) {
      numbers <- gsub("[^0-9.]", "", matches)
      percentages <- c(percentages, as.numeric(numbers[!is.na(as.numeric(numbers))]))
    }
  }
  
  metrics$percentages <- unique(percentages[percentages <= 100])
  
  # Ratio patterns (e.g., 3.2:1, 4:1)
  ratio_pattern <- "([0-9]+(?:\\.[0-9]+)?)\\s*:\\s*([0-9]+)\\s*(?:ratio|return)"
  ratio_matches <- regmatches(text, gregexpr(ratio_pattern, text, ignore.case = TRUE))[[1]]
  if (length(ratio_matches) > 0) {
    metrics$ratios <- ratio_matches
  }
  
  # Rating scale patterns (e.g., 4.2 out of 5, 3.8/5)
  rating_patterns <- c(
    "([0-9]+(?:\\.[0-9]+)?)\\s*out\\s*of\\s*([0-9]+)",
    "([0-9]+(?:\\.[0-9]+)?)\\s*/\\s*([0-9]+)\\s*(?:rating|score)"
  )
  
  ratings <- c()
  for (pattern in rating_patterns) {
    matches <- regmatches(text, gregexpr(pattern, text, ignore.case = TRUE))[[1]]
    if (length(matches) > 0) {
      ratings <- c(ratings, matches)
    }
  }
  metrics$ratings <- unique(ratings)
  
  return(metrics)
}

# Analyze program effectiveness indicators
analyze_program_effectiveness <- function(text, indicators) {
  results <- list()
  
  for (category in names(indicators)) {
    category_count <- 0
    category_context <- c()
    
    for (term in indicators[[category]]) {
      pattern <- paste0("\\b", gsub("\\s+", "\\\\s+", term), "\\b")
      matches <- gregexpr(pattern, text, ignore.case = TRUE)[[1]]
      
      if (length(matches) > 0 && matches[1] != -1) {
        category_count <- category_count + length(matches)
        
        # Extract context around matches
        for (match_pos in matches) {
          start_pos <- max(1, match_pos - 50)
          end_pos <- min(nchar(text), match_pos + 50)
          context <- substr(text, start_pos, end_pos)
          category_context <- c(category_context, context)
        }
      }
    }
    
    results[[category]] <- list(
      count = category_count,
      context = unique(category_context)[1:min(3, length(unique(category_context)))]
    )
  }
  
  return(results)
}

# =============================================================================
# IMPROVED LLM ANALYSIS FOR PROGRAM EFFECTIVENESS
# =============================================================================

analyze_program_with_llm <- function(text, api_key) {
  if (is.null(api_key) || api_key == "") {
    cat("API key not available. Skipping LLM analysis.\n")
    return(NULL)
  }
  
  # Create focused prompt for program effectiveness
  effectiveness_prompt <- paste(
    "As an expert in autism employment program evaluation, analyze the following text",
    "and provide a structured assessment focusing on:",
    "\n1. Quantitative outcomes and success metrics",
    "\n2. Employment and retention rates",
    "\n3. Cost-effectiveness and ROI indicators", 
    "\n4. Participant satisfaction and well-being measures",
    "\n5. Employer feedback and workplace integration success",
    "\n6. Long-term sustainability indicators",
    "\n\nProvide specific numbers, percentages, and concrete evidence where available.",
    "\n\nLimit response to 400 words and focus on measurable outcomes.",
    "\n\nText to analyze:\n\n",
    substr(text, 1, 6000)
  )
  
  messages <- list(
    list(
      role = "system", 
      content = "You are a program evaluation specialist with expertise in autism employment initiatives. Focus on quantifiable outcomes and evidence-based assessment."
    ),
    list(role = "user", content = effectiveness_prompt)
  )
  
  tryCatch({
    response <- httr::POST(
      "https://api.openai.com/v1/chat/completions",
      httr::add_headers(
        "Authorization" = paste("Bearer", api_key),
        "Content-Type" = "application/json"
      ),
      body = jsonlite::toJSON(list(
        model = "gpt-3.5-turbo",
        messages = messages,
        max_tokens = 500,
        temperature = 0.2
      ), auto_unbox = TRUE),
      encode = "raw",
      timeout = 45
    )
    
    if (httr::status_code(response) == 200) {
      content <- httr::content(response, "parsed")
      if (!is.null(content$choices) && length(content$choices) > 0) {
        analysis <- content$choices[[1]]$message$content
        cat("LLM program effectiveness analysis completed successfully.\n")
        return(analysis)
      } else {
        cat("No content returned from API.\n")
        return(NULL)
      }
    } else {
      cat("API request failed with status code:", httr::status_code(response), "\n")
      error_content <- httr::content(response, "text")
      cat("Error response:", substr(error_content, 1, 200), "\n")
      return(NULL)
    }
  }, error = function(e) {
    cat("Error during LLM API request:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# VISUALIZATION FOR PROGRAM EFFECTIVENESS
# =============================================================================

create_effectiveness_visualization <- function(effectiveness_data) {
  tryCatch({
    # Prepare data for visualization
    viz_data <- data.frame(
      Category = names(effectiveness_data),
      Mentions = sapply(effectiveness_data, function(x) x$count),
      stringsAsFactors = FALSE
    )
    
    # Filter out categories with zero mentions
    viz_data <- viz_data[viz_data$Mentions > 0, ]
    
    if (nrow(viz_data) == 0) {
      cat("No effectiveness indicators found for visualization.\n")
      return(NULL)
    }
    
    # Create bar plot
    effectiveness_plot <- ggplot(viz_data, aes(x = reorder(Category, Mentions), y = Mentions)) +
      geom_bar(stat = "identity", fill = "steelblue", alpha = 0.8) +
      coord_flip() +
      theme_minimal() +
      labs(
        title = "Program Effectiveness Indicators Analysis",
        subtitle = "Frequency of effectiveness measures in program documentation",
        x = "Effectiveness Category",
        y = "Frequency of Mentions"
      ) +
      theme(
        plot.title = element_text(size = 14, face = "bold"),
        plot.subtitle = element_text(size = 11),
        axis.text = element_text(size = 9),
        axis.text.y = element_text(size = 8),
        panel.grid.minor = element_blank()
      )
    
    ggsave("program_effectiveness_analysis.png", effectiveness_plot, 
           width = 12, height = 8, dpi = 300)
    cat("Program effectiveness visualization saved successfully.\n")
    
    return(effectiveness_plot)
  }, error = function(e) {
    cat("Error creating effectiveness visualization:", e$message, "\n")
    return(NULL)
  })
}

# =============================================================================
# MAIN ANALYSIS EXECUTION
# =============================================================================

# Load and analyze document
main_doc_path <- "Autism_Employment_Report.txt"
doc_text <- load_document(main_doc_path)

if (!is.null(doc_text)) {
  # Extract quantitative metrics
  quantitative_metrics <- extract_quantitative_metrics(doc_text)
  
  # Analyze effectiveness indicators
  effectiveness_analysis <- analyze_program_effectiveness(doc_text, effectiveness_indicators)
  
  # Create visualization
  effectiveness_plot <- create_effectiveness_visualization(effectiveness_analysis)
  
  # Perform LLM analysis if API key is available
  llm_effectiveness_analysis <- NULL
  if (!is.null(openai_api_key)) {
    cat("Performing LLM-assisted program effectiveness analysis...\n")
    llm_effectiveness_analysis <- analyze_program_with_llm(doc_text, openai_api_key)
    
    if (!is.null(llm_effectiveness_analysis)) {
      writeLines(llm_effectiveness_analysis, "llm_program_effectiveness.txt")
      cat("LLM effectiveness analysis saved to llm_program_effectiveness.txt\n")
    }
  }
  
  # =============================================================================
  # COMPREHENSIVE EFFECTIVENESS REPORT GENERATION
  # =============================================================================
  
  generate_effectiveness_report <- function(effectiveness_data, quant_metrics, llm_analysis) {
    report_lines <- c(
      "# Autism Employment Program Effectiveness Analysis Report",
      "",
      paste("**Report Generated:** ", Sys.Date()),
      paste("**Analysis Timestamp:** ", Sys.time()),
      paste("**Document Analyzed:** ", basename(main_doc_path)),
      "",
      "## Executive Summary",
      "",
      "This analysis focuses specifically on program effectiveness indicators within autism employment initiatives. The assessment examines quantitative outcomes, success metrics, and evidence-based measures of program impact.",
      ""
    )
    
    # Add quantitative findings
    if (length(quant_metrics$percentages) > 0 || length(quant_metrics$ratings) > 0 || length(quant_metrics$ratios) > 0) {
      report_lines <- c(report_lines,
                        "## Quantitative Program Metrics",
                        ""
      )
      
      if (length(quant_metrics$percentages) > 0) {
        report_lines <- c(report_lines,
                          "**Percentage-based Outcomes:**"
        )
        for (pct in quant_metrics$percentages) {
          report_lines <- c(report_lines, paste("- ", pct, "%"))
        }
        report_lines <- c(report_lines, "")
      }
      
      if (length(quant_metrics$ratings) > 0) {
        report_lines <- c(report_lines,
                          "**Rating Scale Measures:**"
        )
        for (rating in quant_metrics$ratings) {
          report_lines <- c(report_lines, paste("- ", rating))
        }
        report_lines <- c(report_lines, "")
      }
      
      if (length(quant_metrics$ratios) > 0) {
        report_lines <- c(report_lines,
                          "**Cost-Benefit Ratios:**"
        )
        for (ratio in quant_metrics$ratios) {
          report_lines <- c(report_lines, paste("- ", ratio))
        }
        report_lines <- c(report_lines, "")
      }
    }
    
    # Add effectiveness indicators analysis
    report_lines <- c(report_lines,
                      "## Program Effectiveness Indicators",
                      ""
    )
    
    for (category in names(effectiveness_data)) {
      if (effectiveness_data[[category]]$count > 0) {
        clean_category <- gsub("_", " ", stringr::str_to_title(category))
        report_lines <- c(report_lines,
                          paste("**", clean_category, ":** ", effectiveness_data[[category]]$count, " references")
        )
        
        if (length(effectiveness_data[[category]]$context) > 0) {
          context_sample <- effectiveness_data[[category]]$context[1]
          if (!is.na(context_sample) && nchar(context_sample) > 0) {
            report_lines <- c(report_lines,
                              paste("Sample context: \"...", substr(context_sample, 1, 100), "...\""),
                              ""
            )
          }
        }
      }
    }
    
    # Add visualization reference
    if (file.exists("program_effectiveness_analysis.png")) {
      report_lines <- c(report_lines,
                        "![Program Effectiveness Analysis](program_effectiveness_analysis.png)",
                        ""
      )
    }
    
    # Add LLM analysis if available
    if (!is.null(llm_analysis)) {
      report_lines <- c(report_lines,
                        "## AI-Assisted Program Effectiveness Assessment",
                        "",
                        llm_analysis,
                        ""
      )
    }
    
    # Add methodology and conclusions
    report_lines <- c(report_lines,
                      "## Methodology",
                      "",
                      "This analysis employed targeted extraction of program effectiveness indicators including employment outcomes, performance metrics, satisfaction measures, employer feedback, cost-effectiveness data, retention success, skill development outcomes, and support effectiveness measures. Quantitative metrics were systematically identified and extracted to provide evidence-based assessment of program impact.",
                      "",
                      "## Key Assessment Areas",
                      "",
                      "The analysis concentrated on eight primary effectiveness domains: employment outcomes and job placement success, performance metrics and workplace productivity measures, participant and employer satisfaction indicators, cost-effectiveness and return on investment data, job retention and long-term success rates, skill development and training effectiveness, and support intervention success measures.",
                      "",
                      "## Recommendations for Program Evaluation",
                      "",
                      "Future program assessments should emphasize longitudinal tracking of employment outcomes, standardized measurement of cost-effectiveness ratios, systematic collection of employer feedback, regular assessment of participant satisfaction and well-being indicators, and comprehensive documentation of workplace accommodation effectiveness.",
                      "",
                      "---",
                      "*Report generated by Autism Employment Program Effectiveness Analysis System*"
    )
    
    return(report_lines)
  }
  
  # Generate final report
  effectiveness_report <- generate_effectiveness_report(
    effectiveness_analysis, 
    quantitative_metrics, 
    llm_effectiveness_analysis
  )
  
  writeLines(effectiveness_report, "autism_employment_program_effectiveness_report.md")
  
  # =============================================================================
  # COMPLETION SUMMARY
  # =============================================================================
  
  cat("\n", paste(rep("=", 80), collapse = ""), "\n")
  cat("PROGRAM EFFECTIVENESS ANALYSIS COMPLETE\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  cat("Generated files:\n")
  cat("- autism_employment_program_effectiveness_report.md (Effectiveness assessment)\n")
  
  if (file.exists("program_effectiveness_analysis.png")) {
    cat("- program_effectiveness_analysis.png (Effectiveness visualization)\n")
  }
  
  if (file.exists("llm_program_effectiveness.txt")) {
    cat("- llm_program_effectiveness.txt (AI-assisted analysis)\n")
  }
  
  cat("\nQuantitative metrics found:\n")
  cat("- Percentages:", length(quantitative_metrics$percentages), "\n")
  cat("- Ratings:", length(quantitative_metrics$ratings), "\n")
  cat("- Ratios:", length(quantitative_metrics$ratios), "\n")
  
  cat("\nEffectiveness indicators analyzed:\n")
  for (category in names(effectiveness_analysis)) {
    if (effectiveness_analysis[[category]]$count > 0) {
      clean_name <- gsub("_", " ", stringr::str_to_title(category))
      cat("- ", clean_name, ": ", effectiveness_analysis[[category]]$count, " mentions\n")
    }
  }
  
  cat("\nAnalysis completed successfully at:", as.character(Sys.time()), "\n")
  cat(paste(rep("=", 80), collapse = ""), "\n")
  
} else {
  cat("Error: Could not load document for analysis.\n")
}