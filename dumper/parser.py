import csv
import iso8601


class FormSubmission:

    def __init__(self, domain, form_id, user_id, time_completed, time_received):
        self.domain = domain
        self.form_id = form_id
        self.submitted_by = user_id
        self.time_completed = time_completed
        self.time_received = time_received

        try:
            delta_seconds = (iso8601.parse_date(time_received) - iso8601.parse_date(time_completed)).total_seconds()
            self.delta_cs_in_hours = delta_seconds / 3600
        except iso8601.ParseError:
            self.delta_cs_in_hours = None

    def write(self, writer):
        writer.writerow([
            self.domain,
            self.form_id,
            self.submitted_by,
            self.time_completed,
            self.time_received,
            "{0:.1f}".format(self.delta_cs_in_hours) if self.delta_cs_in_hours else ''
        ])


class UserData:

    def __init__(self, user_id, avg_delta_cs):
        self.user_id = user_id
        self.avg_delta_cs = avg_delta_cs

    def write(self, writer):
        writer.writerow([self.user_id, self.avg_delta_cs])


def get_csv_writer(output_filename):
    csv_file = open(output_filename, 'w')
    writer = csv.writer(csv_file, delimiter=',')
    return writer


def parse_form_doc(doc):
    doc_id = doc['_id']
    source_obj = doc['_source']
    meta_obj = source_obj['form']['meta']

    user_id = meta_obj['userID']
    time_completed = meta_obj['timeEnd']
    time_received = source_obj['received_on']
    domain_name = source_obj['domain']

    return FormSubmission(domain_name, doc_id, user_id, time_completed, time_received)


def parse_user(hit):
    return UserData(hit['key'], hit['average_deltacs']['value'])


def write_domains(domains, output_filename):
    with open(output_filename, 'w') as f:
        f.write('\n'.join(domains))
