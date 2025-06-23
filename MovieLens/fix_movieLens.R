# Fix script for MovieLens.Rmd Chrome error

# Option 1: Quick fix - Add to your R Markdown setup chunk
fix_setup <- function() {
  knitr::opts_chunk$set(
    echo = TRUE,
    screenshot.force = FALSE,
    warning = FALSE,
    message = FALSE
  )
  options(
    chromote.timeout = 120,
    chromote.launch_echo = FALSE
  )
}

# Option 2: Render with specific settings
render_with_fix <- function() {
  # Close any Chrome sessions
  try(chromote::default_chromote_object()$close(), silent = TRUE)
  gc()
  
  # Set options
  options(chromote.timeout = 120)
  
  # Render
  rmarkdown::render(
    "MovieLens.Rmd",
    output_format = html_document(
      self_contained = TRUE,
      pandoc_args = c("--self-contained")
    ),
    envir = new.env()
  )
}

# Option 3: Fix the workflow chunk specifically
# Add this to the workflow chunk header:
# {r workflow, screenshot.force = FALSE}

# Option 4: If using HTML widgets, save them instead of printing
# Instead of: print(my_plotly_plot)
# Use: htmlwidgets::saveWidget(my_plotly_plot, "plot.html")

# Run the fix
cat("Running render with fixes...\n")
render_with_fix()

