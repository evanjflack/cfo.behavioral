% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/acute_event_outcomes.R
\name{unpack_ip}
\alias{unpack_ip}
\title{Unpack IP Claims (Acute Outcomes)}
\usage{
unpack_ip(ip, num_dgns, num_prcdr)
}
\arguments{
\item{ip}{data.table of inpatient claims with the columns bene_id, clm_id,
from_dt, and dgnscd}

\item{num_dgns}{integer (1-6), number of diagnosis codes to use}

\item{num_prcdr}{integer (1-6), number of procedure codes to use}
}
\value{
list with 2 elements
\item{ip_diag}{data.table of ip diagnosis}
\item{ip_prcdr}{data.table of ip prcedures}
}
\description{
Reformats ip claims diagnosis and procedure codes from wide to long. Used for
generating acute outcome variables.
}
