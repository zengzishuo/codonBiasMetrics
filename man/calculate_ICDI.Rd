\name{calculate_ICDI}
\alias{calculate_ICDI}
\title{calculate_ICDI}
\description{
Given a DNA coding sequence, calculate ICDI (Intrinsic Codon Deviation Index) of the sequence.

Freire-Picos, M. A., et al. (1994). "Codon usage in Kluyveromyces lactis and in yeast cytochrome c-encoding genes." Gene 139(1): 43-49.
}
\usage{
calculate_ICDI(input_sequence)
}
\arguments{
  \item{input_sequence}{a DNA coding sequence}
}
\examples{
my_sequence = generate_a_sequence(20)
calculate_ICDI(input_sequence = my_sequence)

}
