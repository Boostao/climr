test_that("test ts tables", {
  testInit("data.table")
  
  dbCon <- climr::data_con()
  
  t <- expand.grid(
    tbl = climr:::dbnames_ts$dbname,
    v = c("PPT", "Tmin", "Tmax")
  )
  t$vtbl <- stringi::stri_replace_all_fixed(t$tbl, "VAR", tolower(t$v), vectorize_all = TRUE)
  q <- paste0(collapse = " UNION ALL ", 
              "SELECT '%s' AS tbl, '%s' AS vtbl, MAX(ST_NUMBANDS(rast)) maxnbands, MIN(ST_NUMBANDS(rast)) minnbands FROM \"%s\"" |> sprintf(t$tbl, t$vtbl, t$vtbl)
  )
  res <- DBI::dbGetQuery(dbCon, q) |> data.table::setDT()
  res[order(tbl, vtbl)]
  ts_table_with_differings_num_bands_across_VAR <- res[,min(maxnbands) != max(minnbands), by = "tbl"][which(V1), tbl]
  
  testthat::expect_length(ts_table_with_differings_num_bands_across_VAR, 0)
  
  if (length(ts_table_with_differings_num_bands_across_VAR)) {
    cat("\n")
    print(res[tbl %in% ts_table_with_differings_num_bands_across_VAR])
  }
  
})

test_that("test that all constraints have been set on raster tables from data_processing/climr_raster_table_constraints.R", {
  testInit("data.table")
  
  dbCon <- climr::data_con()
  
  # Identify all raster tables (mirroring your script's logic)
  target_tb <- DBI::dbGetQuery(dbCon, "
    SELECT table_schema, table_name
    FROM information_schema.tables t
    WHERE table_type = 'BASE TABLE'
    AND table_schema NOT IN ('pg_catalog', 'information_schema')
    AND (
        SELECT COUNT(*)
        FROM information_schema.columns c
        WHERE c.table_schema = t.table_schema
        AND c.table_name = t.table_name
    ) = 2
    AND EXISTS (
        SELECT 1
        FROM information_schema.columns c
        WHERE c.table_schema = t.table_schema
        AND c.table_name = t.table_name
        AND c.column_name = 'rid'
        AND c.data_type = 'integer'
    )
    AND EXISTS (
        SELECT 1
        FROM information_schema.columns c
        WHERE c.table_schema = t.table_schema
        AND c.table_name = t.table_name
        AND c.column_name = 'rast'
        AND c.data_type = 'USER-DEFINED'
        AND c.udt_name = 'raster'
    )
    ORDER BY table_schema, table_name;
  ") |> data.table::setDT()
  
  # Query raster_columns to check constraints
  raster_constraints <- DBI::dbGetQuery(dbCon, "
    SELECT r_table_schema AS table_schema,
           r_table_name AS table_name,
           srid,
           scale_x,
           scale_y,
           blocksize_x,
           blocksize_y,
           same_alignment,
           regular_blocking,
           num_bands
    FROM raster_columns
    WHERE r_table_catalog = 'climr'
  ") |> data.table::setDT()
  
  # Merge to find raster tables missing constraints
  target_tb[, full_name := paste(table_schema, table_name, sep = ".")]
  raster_constraints[, full_name := paste(table_schema, table_name, sep = ".")]
  missing_constraints <- setdiff(target_tb$full_name, raster_constraints$full_name)
  
  # Test that all raster tables have constraints
  testthat::expect_length(missing_constraints, 0)
  
  # Additional check: Ensure key constraints are not null
  if (nrow(raster_constraints) > 0) {
    null_constraints <- raster_constraints[
      is.na(srid) | 
        is.na(scale_x) | 
        is.na(scale_y) | 
        is.na(blocksize_x) | 
        is.na(blocksize_y) | 
        is.na(num_bands),
      .(table_schema, table_name)
    ]
    testthat::expect_equal(nrow(null_constraints), 0)
  }
  
  # Diagnostic output if tests fail
  if (length(missing_constraints) > 0) {
    cat("\n")
    cat("Raster tables missing constraints: %s\n" |> sprintf(paste(missing_constraints, collapse = ", ")))
    cat("Run `DBI::dbExecute(climr::data_con(), \"SELECT AddRasterConstraints('%s'::name, '%s'::name, 'rast'::name);\", schema, table)` for each missing table.\n" |> 
          sprintf(target_tb[full_name %in% missing_constraints, table_schema], 
                  target_tb[full_name %in% missing_constraints, table_name]))
  }
  
  if (nrow(null_constraints) > 0) {
    cat("\n")
    cat("Raster tables with incomplete constraints:\n")
    print(null_constraints)
  }
  
})

test_that("test that all tables have last modified trigger set from data_processing/climr_lastmodified_tracking_table.R", {
  testInit("data.table")
  
  dbCon <- climr::data_con()
  # Tables that needs triggers
  tb <- DBI::dbGetQuery(dbCon, "
     SELECT table_name 
     FROM information_schema.tables 
     WHERE table_catalog = 'climr' 
       AND table_schema = 'public' 
       AND table_type = 'BASE TABLE'
       AND table_name != 'table_modification_log'
  ") |> data.table::setDT()
  
  mod_triggers <- DBI::dbGetQuery(con, "
    SELECT event_object_table AS table_name,
           trigger_name,
           event_manipulation AS event,
           action_timing AS timing
    FROM information_schema.triggers
    WHERE trigger_catalog = 'climr'
      AND trigger_name like 'track_modification_%'
    ORDER BY table_name, trigger_name;
  ") |> data.table::setDT()
  
  no_triggers_tb <- setdiff(tb$table_name, mod_triggers$table_name)
  
  testthat::expect_length(no_triggers_tb, 0)
  
  if (length(no_triggers_tb)) {
    cat("\n")
    cat("Missing tracking triggers for tables: %s" |> sprintf(paste(no_triggers_tb, collapse = ", ")))
    cat("\n")
    cat("Run `new_table_trigger(\"%s\")` from data_processing/climr_lastmodified_tracking_table.R" |> sprintf(no_triggers_tb), sep = "\n")
  }
  
})