% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{has_credentials}
\alias{has_credentials}
\title{Check for 'API' Credentials}
\usage{
has_credentials()
}
\value{
A logical value: `TRUE` if both `API_USER` and `API_PASS` environment variables are set (i.e., not `NA`); `FALSE` otherwise
}
\description{
Check if necessary 'API' credentials are available in the environment.
}
\examples{
# Example 1: When environment variables are set
Sys.setenv(API_USER = "my_username", API_PASS = "my_password")
has_credentials()
# Expected output: TRUE

# Example 2: When one or both environment variables are not set
Sys.unsetenv("API_USER")
Sys.unsetenv("API_PASS")
has_credentials()
# Expected output: FALSE

# Clean up by removing environment variables
Sys.unsetenv("API_USER")
Sys.unsetenv("API_PASS")

}
\keyword{internal}
