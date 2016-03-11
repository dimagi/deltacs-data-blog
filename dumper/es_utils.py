from elasticsearch import Elasticsearch

NUM_RESULTS_DESIRED = 1000000
# Integer.MAX_VALUE in Java = 2147483647


def iter_forms_data(es_client, index, query):
    page = es_client.search(
        index=index,
        scroll='2m',
        search_type='scan',
        size=2500,
        body=query
    )

    sid = page['_scroll_id']
    scroll_size = page['hits']['total']

    while scroll_size > 0:
        page = es_client.scroll(scroll_id=sid, scroll='2m')
        sid = page['_scroll_id']
        scroll_size = len(page['hits']['hits'])
        for doc in page['hits']['hits']:
            yield doc


def query_users_data(es_client, index, query):
    page = es_client.search(
        index=index,
        size=NUM_RESULTS_DESIRED,
        body=query
    )

    for hit in page['aggregations']['users']['buckets']:
            yield hit


def get_es(url):
    return Elasticsearch([url])
