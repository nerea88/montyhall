% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/monty-hall-problem.R
\name{play_n_games}
\alias{play_n_games}
\title{Run and display 100 iterations.}
\usage{
play_n_games(n = 100)
}
\arguments{
\item{Arguments}{used are combination of previous functions.}
}
\value{
Returns a table of either strategy and the
corresponding outcomes of those choices.
}
\description{
\code{play_n_games()} combines previous functions and runs
multiple iterations of the game and displays proportions.
}
\details{
The game setup replicates the game on the TV show "Let's
Make a Deal" where there are three doors for a contestant
to choose from, one of which has a car behind it and two
have goats. The contestant selects a door, then the host
opens a door to reveal a goat, and then the contestant is
given an opportunity to stay with their original selection
or switch to the other unopened door. There was a famous
debate about whether it was optimal to stay or switch when
given the option to switch, so this simulation was created
to test both strategies.
}
\examples{
  play_n_games()
  
}
