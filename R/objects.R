##%######################################################%##
#                                                          #
####                      Objects                       ####
#                                                          #
##%######################################################%##

# Can manage:
#  - 3D volumes
#  - Matrices/vectors
#  - File paths
#  - A generic R object (e.g. a linear regression model)

# For each one, define functions for:
#  - Import
#  - Export
#  - Print
#  - Plot (where appropriate)
#  - Check object type
#  - Check compatibility between object types.

# When the object is a volume:
#  - Import/Export from NIfTI format.

# Placeholders for object types to be used in workflow steps:
#  - element_volume()
#  - element_matrix()
#  - element_file()
#  - element_object()

#' @export
element_volume <- function() "volume"
#' @export
element_matrix <- function() "matrix"
#' @export
element_file <- function() "file"
#' @export
element_object <- function() "object"

