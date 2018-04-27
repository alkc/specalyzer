# Specalyzer Shiny App

This is the main repository for the web application Specalyzer, currently hosted at http://specalyzer.org

## Installation

1. **Install the Specalyzer R package.** To do this, follow the instruction in the README file in the [specalyzer-pkg subdirectory](https://github.com/alkc/specalyzer/tree/master/specalyzer-pkg)

2. **Install the remaining dependencies:**

```{R}
install.packages("DT")
install.packages("readr")
install.packages("shiny")
install.packages("shinyjs")
install.packages("V8")
```
3. **Configure where Specalyzer should save user-uploaded data.**

This is achieved by setting the `user_data_base` variable in the top of the `server.R` file to a desired path.

The Shiny app is ready to run now!

## Contact

Send feedback to alexander.koc@slu.se and aakash.chawade@slu.se
