import re, os

ifilenames = [s.strip() for s in os.popen("ls data/200[0-9]_[0-9][0-9]_[a-z]*/*.[0-9][0-9][0-9]").readlines()]

def ilines(ifilenames):
    for ifilename in ifilenames:
        for line in open(ifilename):
            if len(line)>2 and line[-1]=="\n": yield ifilename, line.strip()
        yield None, 'eof'
                            
def events(lines):
    for ifilename, line in lines:
        if line=="eof":
            yield 'eof', None
        else:
            m = re.match("^\*SITE NUMBER: ([0-9]+)$", line)
            if m:
                yield 'site', (int(m.groups()[0]), ifilename)
            else:
                m = re.match("^DATE\s+ ([0-9][0-9]/[0-9][0-9]/[0-9][0-9])\s[0-9\s]+AVERAGE", line)
                if m:
                    yield 'date', m.groups()[0]
                else:
                    m = re.match("[12]?[0-9]\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+\s+[0-9-]+", line)
                    if m:
                        try:
                            fields = [int(i) if i != "-" else None for i in line.split()]
                            yield 'hourly', (fields[0], fields[1:8])
                        except ValueError:
                            yield 'error', line

def data_lines(evs):
    errc = 0
    for evt, info in evs:
        if evt=="error":
            errc += 1
            continue
        elif evt=="eof":
            date, site, hcount = None, None, 0
        elif evt=="date":
            date = info
            hcount = 0
        elif evt=="site":
            site, filename = info
            yield site
        elif evt=="hourly":
            if date==None or site==None: continue
            else:
                hcount += 1
                if hcount>24:
                    raise ValueError, "Oops, hcount goes over 24."
                hour, counts = info
                for iday, count in enumerate(counts):
                    yield errc, site, date, iday, hour, count
                
            
for line in data_lines(events(ilines(ifilenames))):
    print line
    
