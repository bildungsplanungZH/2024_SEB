#' Quarto-Vorlage in diesem Repository verwenden
#'
#' @param file_name Dateiname (ohne Erweiterung) der neuen Unterlage, default ist report
#' @param ext_name gewünschte Vorlage (biplaR-html, biplaR-revealjs, biplaR-pdf, biplaR-docx oder biplaR-typst)
#'
#' @returns NULL
#' @export
#'
#' @examples \dontrun{use_quarto("bericht", "biplaR-html")}
use_quarto <- function(file_name = "report", ext_name = "biplaR-html") {

    if (is.null(file_name)) {
        stop("You must provide a valid file_name")
    }

    # check for available extensions
    stopifnot("Extension not in package" = ext_name %in%
                  get_names())

    # check for existing _extensions directory
    if(!file.exists("_extensions")) dir.create("_extensions")
    message("Created '_extensions' folder")

    # create folder
    if(!file.exists(paste0("_extensions/", ext_name))) dir.create(paste0("_extensions/", ext_name))

    # copy from internals
    file.copy(
        from = system.file(paste0("extdata/_extensions/", ext_name), package = "biplaRquarto"),
        to = paste0("_extensions/"),
        overwrite = TRUE,
        recursive = TRUE,
        copy.mode = TRUE
    )

    # logic check to make sure extension files were moved
    n_files <- length(dir(paste0("_extensions/", ext_name)))

    if(n_files >= 2){
        message(paste(ext_name, "was installed to _extensions folder in current working directory."))
    } else {
        message("Extension appears not to have been created")
    }

    # create new qmd report based on skeleton
    file.copy(file.path("_extensions", ext_name, "template.qmd"),
              paste0(file_name, ".qmd", collapse = ""))

    # open the new file in the editor
    utils::file.edit(paste0(file_name, ".qmd", collapse = ""))

}
