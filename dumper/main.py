import argparse
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


def init_writer(filename):
    writer = get_csv_writer(filename)
    writer.writerow(['domain','form_id','user_id','time_completed','time_received','delta_cs_hours'])
    return writer


def dot():
    sys.stdout.write('.')
    sys.stdout.flush()


if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("url", help="ES URL")
    parser.add_argument("index", help="ES index")
    parser.add_argument("query", help="Path to query file")
    parser.add_argument("-o", "--output", default="data_dump.csv", help="Path to output file for data")
    parser.add_argument("--domain-output", default="domains.csv", help="Path to output file for domains")
    args = parser.parse_args()

    query = get_query(args.query)
    writer = init_writer(args.output)

    es = get_es(args.url)
    counter = 0
    unique_domains = set()
    for doc in iter_data(es, args.index, query):
        counter += 1
        if counter % 100 == 0:
            dot()
        row = parse_doc(doc)
        row.write(writer)
        unique_domains.add(row.domain)

    write_domains(unique_domains, args.domain_output)
