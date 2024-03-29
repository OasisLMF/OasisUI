% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/buildCustom_module.R
\name{buildCustom}
\alias{buildCustom}
\alias{buildCustomUI}
\title{Custom Model Module}
\usage{
buildCustomUI(id)

buildCustom(
  input,
  output,
  session,
  portfolioID,
  modelID,
  supplierID,
  nameID,
  analysisID,
  counter,
  active = reactive(TRUE)
)
}
\arguments{
\item{id}{Namespace ID for the module.}

\item{input, output, session}{Shiny input, output, and session objects.}

\item{portfolioID}{Selected portfolio ID.}

\item{modelID}{Selected model ID.}

\item{supplierID}{Selected supplier model ID.}

\item{nameID}{Selected model ID version.}

\item{analysisID}{Selected analysis ID.}
}
\description{
Shiny Module for showing customized details.
}
\section{Functions}{
\itemize{
\item \code{buildCustomUI}: Returns the UI elements of the module.

\item \code{buildCustom}: Allows user to build their own mode on the fly.
}}

