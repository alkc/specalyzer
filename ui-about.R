get_about_ui <- function() {
  fluidPage(
    column(8,
           includeMarkdown("content/about.md")
           #htmlOutput("demo_explore_text")
    ))
}
