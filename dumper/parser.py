import csv

import iso8601

class FormSubmission:

    def __init__(self, domain, form_id, user_id, time_completed, time_received):
        self.domain = domain
        self.form_id = form_id
        self.submitted_by = user_id
        self.time_completed = time_completed
        self.time_received = time_received

        delta_seconds = (iso8601.parse_date(time_received) - iso8601.parse_date(time_completed)).total_seconds()
        self.delta_cs_in_hours = delta_seconds / 3600

    def write(self, writer):
        writer.writerow([
            self.domain,
            self.form_id,
            self.submitted_by,
            self.time_completed,
            self.time_received,
            "{0:.1f}".format(self.delta_cs_in_hours)
        ])


def get_csv_writer(output_filename):
    csv_file = open(output_filename, 'w')
    writer = csv.writer(csv_file, delimiter=',')
    return writer


def parse_doc(doc):
    doc_id = doc['_id']
    source_obj = doc['_source']
    meta_obj = source_obj['form']['meta']

    user_id = meta_obj['userID']
    time_completed = meta_obj['timeEnd']
    time_received = source_obj['received_on']
    domain_name = source_obj['domain']

    if not user_id:
        print doc
    return FormSubmission(domain_name, doc_id, user_id, time_completed, time_received)


def write_domains(domains, output_filename):
    with open(output_filename, 'w') as f:
        f.write('\n'.join(domains))
