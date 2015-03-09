import re, os, datetime

file_shell_rex = "data_orig/200[0-9]_[0-9][0-9]_[a-z]*/*.2[0-9][0-9]"
site_rex = re.compile("^\*SITE NUMB\s*ER: ([0-9\s]+)$")
channel_rex = re.compile("^\*CHANNEL\s*:\s+([0-9]+)\s+OF")
date_rex = re.compile("^DATE\s+([0-9]?[0-9])\s*/([0-1]?[0-9])/([0-3]?[0-9])\s[0-9\s]+AVERAGE$")
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
                yield 'site', (int(re.sub("\s*", "", m.groups()[0])), ifilename)
	    else:
                m = re.match(channel_rex, line)
                if m:
                    yield 'channel', (int(m.groups()[0]), ifilename)
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
            errors.append("%s %s %s format %s"%(filename, site, date, info))
            continue
        elif evt=="eof":
            date, site, hdata = None, None, {}
        elif evt=="site":
            site, filename = info
            channel = '0'
        elif evt=="channel":
            channel = info[0]
        elif evt=="date":
            date = info
            hours, hdata = set(xrange(24)), {}
        elif evt=="hourly":
            if date==None or site==None:
                continue
            else:
                hour, counts = info
                if hour in hours:
                  hours.remove(hour)
                  hdata[hour] = counts
                else:
                  errors.append("%s %s %s %s duplicate hour"%(filename, site, date, hour))
        elif evt=="eod":
            if len(hours)==0:
                for hour, counts in hdata.iteritems():
                    for iday, count in enumerate(counts):
                        yield errors, (filename, site, channel, date, iday, hour, count)
            else:
                errors.append("%s %s %s incomplete"%(filename, site, date))
            hdata = {}

def date_vars(dtuple, iday):
    year, month, day = [int(i) for i in dtuple]
    date = datetime.datetime(year+2000, month, day, 0, 0) + datetime.timedelta(int(iday))
    days = (date - datetime.datetime(2003, 12, 1, 0, 0)).days
    return date.strftime("%Y-%m-%d"), days, date.strftime("%A"), date.timetuple().tm_yday


print "file site channel date day weekday yday hour count"
for errors, (filename, site, channel, dtuple, iday, hour, count) in data_lines(events(ilines(ifilenames))):
    print "%s %s %s %s %s %s %s %s %s" % (
      (filename, site, channel) + date_vars(dtuple, iday) + (hour, "NA" if count==None else count))

logfile = open("model_data/errors.log", "w")
for error in errors:
  print >>logfile, error
logfile.close()
