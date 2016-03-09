from elasticsearch import Elasticsearch


def iter_data(es_client, index, query):
    page = es_client.search(
        index=index,
        scroll='2m',
        search_type='scan',
        size=10,
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


def get_es(url):
    return Elasticsearch([url])
