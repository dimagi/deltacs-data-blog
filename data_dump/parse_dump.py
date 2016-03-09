import json
import FormSubmission as FS
import csv

def parse():
    csv_file = open("form_dump.csv", 'w')
    writer = csv.writer(csv_file, delimiter=',')
    writer.writerow(['domain','user_id','time_completed','time_received','delta_cs_hours'])

    filename = 'sample_dump_data.json'  # update this
    with open(filename) as data_file:
        hits = json.load(data_file)['hits']['hits']
        unique_domains = set()
        for hit in hits:
            source_obj = hit['_source']
            meta_obj = source_obj['form']['meta']

            user_id = meta_obj['userID']
            time_completed = meta_obj['timeEnd']
            time_received = source_obj['received_on']
            domain_name = source_obj['domain']
            unique_domains.add(domain_name)

            form = FS.FormSubmission(domain_name, user_id, time_completed, time_received)
            form.write_to_csv(writer)
            
    write_domains_to_file(unique_domains)


def write_domains_to_file(domains):
    domains_file = open('all_domains.txt', 'w')
    for domain in domains:
        domains_file.write(domain + '\n')

parse()





