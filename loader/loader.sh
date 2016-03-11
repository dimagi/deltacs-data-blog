#!/usr/bin/env bash
# loader.sh [/path/to/dump.csv] [DATABAES NAME] [POSTGRES USERNAME]

DUMP_PATH="$1"
DB_NAME="$2"
USERNAME="$3"

TABLE_NAME="formdata"
CHECK_DB_SQL="SELECT 1 from pg_database WHERE datname='${DB_NAME}'"
CHECK_TABLE_SQL="
    SELECT 1
    FROM   information_schema.tables
    WHERE  table_schema = 'public'
    AND    table_name = '${TABLE_NAME}';
"
CREATE_TABLE_SQL="
    CREATE TABLE ${TABLE_NAME}
    (
        form_id VARCHAR(255) PRIMARY KEY NOT NULL,
        domain VARCHAR(255) NOT NULL,
        user_id VARCHAR(255),
        time_end TIMESTAMP WITH TIME ZONE NOT NULL,
        received_on TIMESTAMP WITH TIME ZONE NOT NULL
    );
    CREATE INDEX formdata_ad5f82e8 ON ${TABLE_NAME} (domain);
    CREATE INDEX formdata_811bd339 ON ${TABLE_NAME} (received_on);
    CREATE INDEX formdata_f1f6dc1a ON ${TABLE_NAME} (time_end);
    CREATE INDEX formdata_e8701ad4 ON ${TABLE_NAME} (user_id);
"
CHECK_DATA_SQL="
SELECT form_id FROM ${TABLE_NAME} LIMIT 1
"
TRUNCATE_TABLE_SQL="
TRUNCATE TABLE ${TABLE_NAME} RESTART IDENTITY
"
LOAD_DATA_SQL="\copy formdata(domain,form_id,user_id,time_end,received_on) FROM '${DUMP_PATH}' DELIMITER ',' CSV HEADER"

function run_sql() {
    if [ "$#" -eq 2 ]; then
        DB="$1"
        SQL="$2"
    else
        DB="${DB_NAME}"
        SQL="$1"
    fi
    psql -h localhost -U "${USERNAME}" "${DB}" -qtAc "${SQL}"
}

confirm () {
    # call with a prompt string or use a default
    read -r -p "${1:-Are you sure? [y/N]} " response
    case $response in
        [yY][eE][sS]|[yY])
            true
            ;;
        *)
            false
            ;;
    esac
}

# ensure file is readable by postgres
chmod a+r "${DUMP_PATH}"

DB_EXISTS=$(run_sql template1 "${CHECK_DB_SQL}")
if [ -z "${DB_EXISTS}" ]; then
    echo "Creating database ${DB_NAME}"
    createdb -h localhost -U "${USERNAME}" "${DB_NAME}"
fi

TABLE_EXISTS=$(run_sql "${CHECK_TABLE_SQL}")
if [ -z "${TABLE_EXISTS}" ]; then
    echo "Creating table"
    run_sql "${CREATE_TABLE_SQL}"
fi

DATA_EXISTS=$(run_sql "${CHECK_DATA_SQL}")
if [ -z "${DATA_EXISTS}" ]; then
    run_sql "${LOAD_DATA_SQL}"
else
    confirm "Table already has data. Do you want to delete existing data before continuing (Ctrl-C to abort)? [y/N]" && run_sql "${TRUNCATE_TABLE_SQL}"
    run_sql "${LOAD_DATA_SQL}"
fi
