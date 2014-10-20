import re, os, datetime

file_shell_rex = "data/200[0-9]_[0-9][0-9]_[a-z]*/*.[0-9][0-9][0-9]"
site_rex = re.compile("^\*SITE NUMBER: ([0-9]+)$")
date_rex = re.compile("^DATE\s+([0-9]?[0-9])/([0-1]?[0-9])/([0-3]?[0-9])\s[0-9\s]+AVERAGE$")
count_rex = re.compile("[12]?[0-9]\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+")
totals_rex = re.compile("^TOTALS$")

ifilenames = [s.strip() for s in os.popen("ls %s" % file_shell_rex).readlines()]

def ilines(ifilenames):
    for ifilename in ifilenames:
        ifile = open(ifilename) # pypy requires explicit close
        for line in ifile:
            if len(line)>2 and line[-1]=="\n": yield ifilename, line.strip()
        ifile.close()
        yield None, 'eof'        
                            
def events(lines):
    for ifilename, line in lines:
        if line=="eof":
            yield 'eof', None
        else:
            m = re.match(site_rex, line)
            if m:
                yield 'site', (int(m.groups()[0]), ifilename)
            else:
                m = re.match(date_rex, line)
                if m:
                    yield 'date', m.groups()
                else:
                    m = re.match(count_rex, line)
                    if m:
                        try:
                            fields = [int(i) if i != "-" else None for i in line.split()]
                            yield 'hourly', (fields[0], fields[1:8])
                        except ValueError:
                            yield 'error', line
                    else:
                        m = re.match(totals_rex, line)
                        if m:
                            yield 'eod', None
                        
def data_lines(evs):
    errors = []
    for evt, info in evs:
        if evt=="error":
            errors.append("format")
            continue
        elif evt=="eof":
            date, site, hdata = None, None, {}
        elif evt=="site":
            site, filename = info
        elif evt=="date":
            date = info
            hours, hdata = set(xrange(24)), {}
        elif evt=="hourly":
            if date==None or site==None:
                continue
            else:
                hour, counts = info
                hours.remove(hour)
                hdata[hour] = counts
        elif evt=="eod":
            if len(hours)==0:
                for hour, counts in hdata.iteritems():
                    for iday, count in enumerate(counts):
                        yield errors, (site, date, iday, hour, count)
            else:
                errors.append("incomplete")
            hdata = {}

def date_vars(dtuple, i):
    year, month, day = [int(i) for i in dtuple]
    date = datetime.datetime(year+2000, month, day, 0, 0) + datetime.timedelta(int(i))
    days = (date - datetime.datetime(2004, 1, 1, 0, 0)).days
    return date.strftime("%Y-%m-%d"), days, date.strftime("%A"), date.timetuple().tm_yday


print "site date day weekday yday hour count"
for errors, (site, dtuple, iday, hour, count) in data_lines(events(ilines(ifilenames))):
    print "%s %s %s %s %s %s %s" % ((site, ) + date_vars(dtuple, iday) + (hour, "NA" if count==None else count))
