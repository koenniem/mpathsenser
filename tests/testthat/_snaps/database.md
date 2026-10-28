# copy_db rejects in-memory targets without disconnecting them

    Code
      copy_db(source_db, target_db)
    Condition
      Error in `copy_db()`:
      ! The target database must have a file-backed path.

# copy_db detaches its target after a copy error

    Code
      copy_db(source_db, target_db)
    Condition
      Error in `dbExecute()`:
      ! forced copy failure

