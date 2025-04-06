
# Use for new table
new_table_trigger <- function(table_name) {
  climr:::.globals[["sessprof"]]$set("adm", list(
    host = '146.190.244.244',
    user = 'postgres',
    password = 'climr2022'
  ))
  con <- climr::data_con("adm")
  on.exit(climr::connections_clear())
  DBI::dbExecute(con, paste0("
    DO $$
    DECLARE
        r RECORD;
    BEGIN
        -- Loop through all user tables in the climr database (public schema)
        FOR r IN 
            SELECT table_name 
            FROM information_schema.tables 
            WHERE table_catalog = 'climr' 
              AND table_schema = 'public' 
              AND table_type = 'BASE TABLE'
              AND table_name = '", table_name, "'
        LOOP
            -- Insert initial log entry if not already present
            INSERT INTO table_modification_log (table_name, last_modified)
            VALUES (r.table_name, CURRENT_TIMESTAMP)
            ON CONFLICT (table_name) DO NOTHING;
    
            -- Create the trigger for each table
            EXECUTE format('
                CREATE TRIGGER \"track_modification_%s\"
                AFTER INSERT OR UPDATE OR DELETE
                ON %I
                FOR EACH STATEMENT
                EXECUTE FUNCTION update_last_modified();
            ', r.table_name, r.table_name);
        END LOOP;
    END;
    $$;
  "))
}
new_table_trigger("new_table_name")


### Initialization

# Attach a lastmodified trigger to all tables in climr database

climr:::.globals[["sessprof"]]$set("adm", list(
  host = '146.190.244.244',
  user = 'postgres',
  password = 'climr2022'
))
con <- climr::data_con("adm")

DBI::dbExecute(con, "
CREATE TABLE table_modification_log (
    table_name TEXT PRIMARY KEY,
    last_modified TIMESTAMP
);")

DBI::dbExecute(con, "
CREATE OR REPLACE FUNCTION update_last_modified()
RETURNS TRIGGER AS $$
BEGIN
    UPDATE table_modification_log
    SET last_modified = CURRENT_TIMESTAMP
    WHERE table_name = TG_TABLE_NAME;
    RETURN NULL;
END;
$$ LANGUAGE plpgsql;              
")

# Initial values for all tables except `table_modification_log`
DBI::dbExecute(con, "
DO $$
DECLARE
    r RECORD;
BEGIN
    -- Loop through all user tables in the climr database (public schema)
    FOR r IN 
        SELECT table_name 
        FROM information_schema.tables 
        WHERE table_catalog = 'climr' 
          AND table_schema = 'public' 
          AND table_type = 'BASE TABLE'
          AND table_name != 'table_modification_log'
    LOOP
        -- Insert initial log entry if not already present
        INSERT INTO table_modification_log (table_name, last_modified)
        VALUES (r.table_name, CURRENT_TIMESTAMP)
        ON CONFLICT (table_name) DO NOTHING;

        -- Create the trigger for each table
        EXECUTE format('
            CREATE TRIGGER \"track_modification_%s\"
            AFTER INSERT OR UPDATE OR DELETE
            ON %I
            FOR EACH STATEMENT
            EXECUTE FUNCTION update_last_modified();
        ', r.table_name, r.table_name);
    END LOOP;
END;
$$;
")

