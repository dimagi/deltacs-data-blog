import json
import os
import sys

from dumper.es_utils import iter_data, get_es
from dumper.parser import get_csv_writer, parse_doc, write_domains


def get_query(query_path):
    if not os.path.isfile(query_path):
        raise Exception("Query file not found", query_path)

    with open(query_path, 'r') as f:
        return json.load(f)


def init_writer():
    writer = get_csv_writer('data_dump.csv')
    writer.writerow(['domain','form_id','user_id','time_completed','time_received','delta_cs_hours'])
    return writer


if __name__ == '__main__':
    if len(sys.argv) < 4:
        print "Usage: python dumper.main [ES URL] [ES INDEX] [QUERY FILE PATH]"

    es_url = sys.argv[1]
    es_index = sys.argv[2]
    query_path = sys.argv[3]

    query = get_query(query_path)
    writer = init_writer()

    es = get_es(es_url)
    counter = 0
    unique_domains = set()
    for doc in iter_data(es, es_index, query):
        counter += 1
        row = parse_doc(doc)
        row.write(writer)
        unique_domains.add(row.domain)

    write_domains(unique_domains, "all_domains.csv")
