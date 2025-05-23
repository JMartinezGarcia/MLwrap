.onAttach <- function(libname, pkgname) {
  version <- read.dcf(
    file = system.file("DESCRIPTION", package = pkgname),
    fields = "Version"
  )
  packageStartupMessage("
  **************************************************
  *                                                *
  *   TTTTTT        dd         MM     MM  LL       *
  *     TT   ii     dd  yy  yy MM M M MM  LL       *
  *     TT   ii  dd dd   y y   MM  M  MM  LL       *
  *     TT   ii d   dd   yy    MM     MM  LL       *
  *     TT   ii  dd dd  yy     MM     MM  LLLLLL   *
  *                                                *
  **************************************************

  TidyML v", version, ": **Start simple, scale smart**
  ")
}
