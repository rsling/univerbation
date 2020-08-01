# -*- coding: utf-8 -*-

from ManaCOW import cow_query

INFILE = '/home/schaefer/Projects/Zusammenschreibung/ohneFE_gefiltert.csv'
OUTFILE = '/home/schaefer/Projects/Zusammenschreibung/ohneFE_gefiltert_forms.csv'

# INFILE = '/home/schaefer/Projects/Zusammenschreibung/mitFE_gefiltert.csv'
# OUTFILE = '/home/schaefer/Projects/Zusammenschreibung/mitFE_gefiltert_forms.csv'

# CORPUS = 'decow16a'
CORPUS = 'decow16a-nano'

Q_VVPP  = u'[ lemma = "%s" & tag = "VVPP" ]'
Q_VVZU  = u'[ lemma = "%s" & tag = "VVIZU" ]'
Q_VVFIN = u'[ lemma = "%s" & tag = "VVFIN" & word = "[a-zäöüß]+" ]'
Q_NNPL  = u'[ lemma = "%s" & tag = "NN" & morph = "pl" ]'


def do_query(query_string):
  query = cow_query(query_string, corpus = CORPUS, references = [],
    attributes = ['word'], structures = [], deduping = False, max_hits = 1)
  if query['hits'] < 1:
    return 'NOT_FOUND'
  else:
    return query['concordance'][0]['region'][0].split(' ')[int(query['concordance'][0]['match_offset'])]

def do_forms_query(query_string):
  query = cow_query(query_string, corpus = CORPUS, references = [],
    attributes = ['word'], structures = [], deduping = False, max_hits = 1000)
  if query['hits'] < 1:
    return 'NOT_FOUND'
  else:
    forms = '|'.join(list(set([ x['region'][0].split(' ')[int(x['match_offset'])] for x in query['concordance'] ])))
    return forms

infile = open(INFILE, 'r')
outfile = open(OUTFILE, 'w')

# Dump header line.
header = infile.readline().strip().decode('utf-8').split('\t') + ['Nounpl', 'Verbpp', 'Verbzu', 'Verbfin']
outfile.write('\t'.join(header).encode('utf-8') + '\n')

counter = 0
while True:
  line = infile.readline()
  if not line:
    break
  line = line.decode('utf-8').strip()
  if not line:
    continue
  counter += 1
  line = line.split('\t')

  query_string = (Q_NNPL % line[1]).encode('utf-8')
  nnpl = do_query(query_string).decode('utf-8')
  query_string = (Q_VVPP % line[2]).encode('utf-8')
  vvpp = do_query(query_string).decode('utf-8')
  query_string = (Q_VVZU % line[2]).encode('utf-8')
  vvzu = do_query(query_string).decode('utf-8')
  query_string = (Q_VVFIN % line[2]).encode('utf-8')
  vvfin = do_forms_query(query_string).decode('utf-8')
  
  print vvfin.encode('utf-8')

  outfile.write(('\t'.join(line + [nnpl, vvpp, vvzu, vvfin])).encode('utf-8') + '\n')

infile.close()
outfile.close()
