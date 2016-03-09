import json
import os
import sys

from dumper.es_utils import iter_data, get_es


def get_query(query_path):
    if not os.path.isfile(query_path):
        raise Exception("Query file not found", query_path)

    with open(query_path, 'r') as f:
        return json.load(f)


if __name__ == '__main__':
    if len(sys.argv) < 4:
        print "Usage: python dumper.main [ES URL] [ES INDEX] [QUERY FILE PATH]"

    es_url = sys.argv[1]
    es_index = sys.argv[2]
    query_path = sys.argv[3]

    query = get_query(query_path)

    es = get_es(es_url)
    counter = 0
    for doc in iter_data(es, es_index, query):
        counter += 1
        print doc

    print counter
