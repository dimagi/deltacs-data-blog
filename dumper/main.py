import argparse
import json
import os
import sys

from dumper.es_utils import iter_forms_data, query_users_data, get_es
from dumper.parser import get_csv_writer, parse_user, parse_form_doc, write_domains


def get_query(query_path):
    if not os.path.isfile(query_path):
        raise Exception("Query file not found", query_path)

    with open(query_path, 'r') as f:
        return json.load(f)


def init_writer(query_choice):
    writer = get_csv_writer(output_file)
    if 'f' == query_choice:
        writer.writerow(['domain','form_id','user_id','time_completed','time_received','delta_cs_hours'])
    elif 'u' == query_choice:
        writer.writerow(['user_id','avg_delta_cs'])
    return writer


def dot():
    sys.stdout.write('.')
    sys.stdout.flush()


def dump_forms(query, writer):
    counter = 0
    unique_domains = set()
    for doc in iter_forms_data(es, index, query):
        counter += 1
        if counter % 100 == 0:
            dot()
        form_obj = parse_form_doc(doc)
        form_obj.write(writer)
        unique_domains.add(form_obj.domain)

    write_domains(unique_domains, args.domain_output)


def dump_users(query, writer):
    counter = 0
    for hit in query_users_data(es, index, query):
        counter += 1
        if counter % 100 == 0:
            dot()
        user_obj = parse_user(hit)
        user_obj.write(writer)


def execute_query_choice(query_choice):
    writer = init_writer(query_choice)
    if 'f' == query_choice:
        query = get_query('form_dump_query.json')
        dump_forms(query, writer)
    elif 'u' == query_choice:
        query = get_query('user_dump_query.json')
        dump_users(query, writer)
    else:
        raise Exception('Invalid parameter entered for query-type')



if __name__ == '__main__':
    parser = argparse.ArgumentParser()
    parser.add_argument("url", help="ES URL")
    parser.add_argument("index", help="ES index")
    parser.add_argument("query-type", help="'f' to run the forms dump, 'u' to run the aggregation-by-users dump")
    parser.add_argument("-o", "--output", default="data_dump.csv", help="Path to output file for data")
    parser.add_argument("--domain-output", default="domains.csv", help="Path to output file for domains, if doing forms dump")
    args = parser.parse_args()

    es = get_es(args.url)
    index = args.index
    output_file = args.output
    execute_query_choice(args.query_type)
