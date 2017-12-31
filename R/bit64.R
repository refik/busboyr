#' Converts integer64 to list before pmap
#' 
#' @export
pre_pmap_int64 <- function(.data) {
    int64_vec_to_list <- function(int64) purrr::map(int64, ~.)
    dplyr::mutate_if(.data, bit64::is.integer64, int64_vec_to_list)
}