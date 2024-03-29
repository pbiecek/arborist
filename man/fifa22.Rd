% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/data_fifa22.R
\docType{data}
\name{fifa22}
\alias{fifa22}
\title{FIFA 22 preprocessed data}
\format{
a data frame with 19165 rows, 62 columns and rownames
}
\source{
The \code{players_22.csv} dataset was downloaded from the Kaggle site and went through few transformations.
The complete dataset was obtained from
\url{https://www.kaggle.com/stefanoleone992/fifa-22-complete-player-dataset} on August 31, 2022.
}
\usage{
data(fifa22)
}
\description{
The \code{fifa} dataset is a preprocessed \code{players_22.csv} dataset which comes as
a part of "FIFA 22 complete player dataset" at Kaggle.
}
\details{
It contains 19 165 players and 62 variables. Among all you will find there:
\itemize{
\item short_name (rownames)
\item nationality of the player (not used in modeling)
\item overall, potential, value_eur, wage_eur (4 potential target variables)
\item age, height, weight, attacking skills, defending skills, goalkeeping skills (37 variables)
}
It is advised to leave only one target variable for modeling.


Source: \url{https://www.kaggle.com/datasets/stefanoleone992/fifa-22-complete-player-dataset}
}
\keyword{fifa22}
