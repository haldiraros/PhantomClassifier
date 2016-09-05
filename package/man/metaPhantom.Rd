\name{metaPhantom}
\alias{metaPhantom}
\title{
  Generate phantom objects based on provided dataset and errors
}
\description{
  Generates phantom (synthetic) objects based on provided data and classification errors
}
\usage{
  metaPhantom(form, data, errors,
                     k=3,
                     alpha=0.5)
}
\arguments{
  \item{form}{
    A formula identifying the class variable. For example: \code{Class ~ .}
  }
  \item{data}{
    A dataframe containign original dataset.
  }
  \item{errors}{
    A dataframe containing objects that were wrongly classified -  must be a subset of dataframe provided in data!
  }
  \item{k}{
    How many phantoms should be created for each error object.
    Default value is 3.
  }
  \item{alpha}{
    Where in space between an error object and it's neighbour of correct class
    should the phantom object be generated. Only relevant for numeric atributes of an object.
    Default value is 0.5. Can take values in the range of \code{(0,1)}.
  }
}
\details{
  Improving performance of classification models is a popular field of study in the realm of machine learning. There are many methods intended to do just that, for example bagging or boosting.

  The experimental method introduced here is called Phantom Metaclassifier (???) and is another attempt at that. The general idea is to perform initial classification on a dataset and then generate new synthetic objects - phantoms, based on resulting classification errors and the original dataset.

  For each provided classification error a phantom object is created between it and a set of it's nearest neighbours that are examples of the same class. The number of phantoms generated for each provided classification error is controled by parameter \code{k}. The position of a newly created object in a numeric attribute space is regulated by paramter \code{alpha}. By default phantoms are generated halfway between a classification error and its neigbour. Any nominal attribute values are chosen randomly from corresponding attribute value of given classification error and its nearest neighbours.
}
\value{
  Returns a dataframe with newly generated phantom objects.
}
\references{
TODO!!! put link/name of masters thesis
}
\author{
  Marcin Błaszyk
}
\examples{
## A small usage example based on a subset of IRIS dataset
data(iris)
data <- iris[which(iris$Species!="setosa"),]
## After performing classification certain observations are
## wrongly classified. We manualy choose a subset here for presentation
errors <- data[c(1,14,32,50,73,88,92),]

## Generate 'phantoms' based on original dataset and observations that give errors
phantoms<-metaPhantom(Species ~ .,data, errors)

## We generated 3 new objects for each classification error provided.
phantoms
}
\keyword{datagen}
\keyword{classif}
