import iso8601
from datetime import timedelta

class FormSubmission:

    def __init__(self, domain, user_id, time_completed, time_received):
        self.domain = domain
        self.submitted_by = user_id
        self.time_completed = iso8601.parse_date(time_completed)
        self.time_received = iso8601.parse_date(time_received)
        self.delta_cs_in_hours = (self.time_received - self.time_completed).total_seconds() / 3600

    def write_to_csv(self, writer):
        writer.writerow([self.domain, self.submitted_by, self.time_completed, self.time_received, self.delta_cs_in_hours])
