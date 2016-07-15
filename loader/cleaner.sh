#!/usr/bin/env bash
# cleaner.sh [DATABAES NAME] [POSTGRES USERNAME]

DB_NAME="$1"
USERNAME="$2"

function run_sql() {
    SQL="$1"
    psql -h localhost -U "${USERNAME}" "${DB_NAME}" -qtAc "${SQL}"
}

# Add the deltacs column
run_sql "ALTER TABLE formdata ADD COLUMN deltacs interval"
run_sql "CREATE INDEX formdata_deltacs on formdata (deltacs)"
run_sql "UPDATE formdata SET deltacs = received_on - time_end"

# Clean the data
run_sql "DELETE FROM formdata where time_end < '2011-01-01'"
run_sql "DELETE from formdata where user_id = '' or user_id is NULL"
run_sql "DELETE from formdata where user_id in ('demo_user', 'commtrack-system', 'system', 'admin', 'bihar-system')"
run_sql "DELETE from formdata where char_length(user_id) < 10"
run_sql "DELETE from formdata where deltacs <= '0 days'::interval"

# Create to_hours function
run_sql "CREATE OR REPLACE FUNCTION to_hours(i interval) RETURNS DOUBLE PRECISION AS \$\$
    SELECT EXTRACT(EPOCH FROM i) / (60*60);
    \$\$ LANGUAGE sql;"
