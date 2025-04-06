# Find all raster tables (two columns table rid + rast)

library(data.table)

climr:::.globals[["sessprof"]]$set("adm", list(
  host = '146.190.244.244',
  user = 'postgres',
  password = 'climr2022'
))
con<-climr::data_con("adm")

target_tb <- DBI::dbGetQuery(con, "
SELECT table_schema, table_name
FROM information_schema.tables t
WHERE table_type = 'BASE TABLE'
AND table_schema NOT IN ('pg_catalog', 'information_schema') -- Exclude system schemas
AND (
    SELECT COUNT(*)
    FROM information_schema.columns c
    WHERE c.table_schema = t.table_schema
    AND c.table_name = t.table_name
) = 2 -- Exactly two columns
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
    AND c.udt_name = 'raster' -- Specific to PostGIS raster type
)
ORDER BY table_schema, table_name;") |> data.table::setDT()

target_tb[, climr:::with_retries(DBI::dbExecute(climr::data_con(), "SELECT AddRasterConstraints('%s'::name,'%s'::name,'rast'::name);" |> sprintf(table_schema, table_name))), by = .I]

