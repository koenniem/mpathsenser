# empty-file registration keeps mixed input rows aligned

    Code
      skipped <- .read_register_empty_files(db, empty_meta)
    Condition
      Warning:
      Skipped 1 empty file with an unrecognised file name.
      i Empty files are registered as processed when their name follows the m-Path Sense convention.

# empty-file registration skips a non-numeric id between valid files

    Code
      skipped <- .read_register_empty_files(db, empty_meta)
    Condition
      Warning:
      Skipped 1 empty file with a non-numeric participant id.
      i Participant ids are stored as unsigned integers.

# empty-file registration enforces the UINTEGER participant range

    Code
      skipped <- .read_register_empty_files(db, empty_meta)
    Condition
      Warning:
      Skipped 2 empty files with a participant id that cannot be stored as an unsigned integer.
      i Participant ids must be whole numbers between 0 and 4294967295.

# read_mpath_sense returns all unrecognized empty paths

    Code
      skipped <- read_mpath_sense(path = dir, db = db, recursive = FALSE, .progress = FALSE)
    Condition
      Warning:
      Skipped 2 empty files with an unrecognised file name.
      i Empty files are registered as processed when their name follows the m-Path Sense convention.
    Message
      No new files to process.

