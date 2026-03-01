# R/check_paths.R
# Check that GRAVE-D export is available from grave_d_data2026
# via a relative path and print the resolved absolute path.

check_grave_d_path <- function(
                                            filename = "GRAVE_D_Master_with_Leaders.csv"
) {
  # Expected relative path from autocracy_conflict_signaling/
  rel_path <- file.path("..", "grave_d_data2026", "ready_data", filename)

  # Resolve to absolute path (without assuming a specific root like C:/Users/...)
  abs_path <- normalizePath(rel_path, winslash = "\\", mustWork = FALSE)

  if (!file.exists(abs_path)) {
    stop(
      paste0(
        "GRAVE-D data file not found.\n",
        "Expected relative path (from autocracy_conflict_signaling):\n",
        "  ", rel_path, "\n",
        "Resolved absolute path:\n",
        "  ", abs_path, "\n\n",
        "Make sure both repositories are cloned as siblings, e.g.:\n",
        "  C:\\Users\\tomha\\R_Projects\\grave_d_data2026\\\n",
        "  C:\\Users\\tomha\\R_Projects\\autocracy_conflict_signaling\\\n",
        "and that ", filename, " has been produced by grave_d_data2026."
      ),
      call. = FALSE
    )
  }

  message("Found GRAVE-D file at: ", abs_path)
  return(abs_path)
}
