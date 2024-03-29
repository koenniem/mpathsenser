% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/database.R
\name{vacuum_db}
\alias{vacuum_db}
\title{Vacuum a database}
\usage{
vacuum_db(db)
}
\arguments{
\item{db}{A database connection to an m-Path Sense database.}
}
\value{
a scalar numeric that specifies the number of rows affected by the vacuum.
}
\description{
\ifelse{html}{\href{https://lifecycle.r-lib.org/articles/stages.html#stable}{\figure{lifecycle-stable.svg}{options: alt='[Stable]'}}}{\strong{[Stable]}}

This is a convenience function that calls the \code{VACUUM} command on a database. This command will
rebuild the database file, repacking it into a minimal amount of disk space.
}
\examples{
# Create a database in a temporary directory
db <- create_db(tempdir(), "mydb.db")

# Assuming that we have imported some data into the database, we can vacuum it
vacuum_db(db)

# Cleanup
close_db(db)
file.remove(file.path(tempdir(), "mydb.db"))
}
