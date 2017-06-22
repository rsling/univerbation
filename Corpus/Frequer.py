# -*- coding: utf-8 -*-

from ManaCOW import cow_query, cow_print_query

# INFILE  = '/home/schaefer/Projects/Zusammenschreibung/ohneFE_gefiltert_forms.csv'
# OUTFILE = '/home/schaefer/Projects/Zusammenschreibung/ohneFE_gefiltert_forms_freqs.csv'

INFILE  = '/home/schaefer/Projects/Zusammenschreibung/mitFE_gefiltert_forms.csv'
OUTFILE = '/home/schaefer/Projects/Zusammenschreibung/mitFE_gefiltert_forms_freqs.csv'

OUTDIR  = '/home/schaefer/Projects/Zusammenschreibung/Queries/'

REFERENCES = [ 'doc.url', 'doc.id', 's.idx',
  'doc.forum', 'doc.corex_clitindef', 'doc.corex_short', 'doc.corex_gen' ]

# TODO In final run, comment out second line:
# CORPUS = 'decow16a'
CORPUS = 'decow16a-nano'

# Overall compound frequency.
Q_COMPOUND_ZUSAMMEN  = u'[] [ word = "%s" ] within <s/>'

# NP with normal determiner.
Q_NP_DET_ZUSAMMEN  = u'[ tag = "PDAT|PIAT|POSAT|ART" ] [ word = "%s" ] within <s/>'
Q_NP_DET_GETRENNT  = u'[ tag = "PDAT|PIAT|POSAT|ART" ] [ word = "%s" ] [ word = "%s" ] within <s/>'

# NP with preposition + cliticised determiner.
Q_NP_APPRART_ZUSAMMEN = u'[ tag = "APPRART" & word != "[Aa]m" ] [ word = "%s" ] within <s/>'
Q_NP_APPRART_GETRENNT = u'[ tag = "APPRART" & word != "[Aa]m" ] [ word = "%s" ] [ word = "%s" ] within <s/>'

# NP without determiner.
Q_NP_NODET_ZUSAMMEN = u'[ tag != "PDAT|PIAT|POSAT|ART|PRELAT|APPRART|ADJA" ]{2,} [ word = "%s" ] within <s/>'
Q_NP_NODET_GETRENNT = u'[ tag != "PDAT|PIAT|POSAT|ART|PRELAT|APPRART|ADJA" ]{2,} [ word = "%s" ] [ word = "%s" ] within <s/>'

# Infinitive with 'zu'.
Q_INF_ZU_ZUSAMMEN = u'[ word = "%s" ]'
Q_INF_ZU_GETRENNT = u'[ word = "%s" ] [ word = "zu" ] [ word = "%s"]  within <s/>'

# Progressive with 'am'.
Q_PROG_ZUSAMMEN  = u'[ word = "[Aa]m" ] [ word = "%s" ] within <s/>'
Q_PROG_GETRENNT  = u'[ word = "[Aa]m" ] [ word = "%s" ] [ word = "%s" ] within <s/>'

# Infinitive with 'zu'.
Q_INF_ZU_ZUSAMMEN = u'[ word = "%s" ]'
Q_INF_ZU_GETRENNT = u'[ tag != "PDAT|PIAT|POSAT|ART|PRELAT|APPRART|ADJA" ] [ word = "%s" ] [ word = "zu" ] [ word = "%s"]  within <s/>'

# Participle.
Q_PARTICIP_ZUSAMMEN = u'[ word = "%s" ]'
Q_PARTICIP_GETRENNT = u'[ tag != "PDAT|PIAT|POSAT|ART|PRELAT|APPRART|ADJA" ] [ word = "%s" ] [ word = "%s"]  within <s/>'

# Normal finite verb.
Q_FINITE_ZUSAMMEN = u'[ word = "%s(%s)" ] within <s/>'
Q_FINITE_GETRENNT = u'[tag != "PDAT|PIAT|POSAT|ART|PRELAT|APPRART|ADJA" ] [ word = "%s" ] [ word = "(%s)" ] within <s/>'

#######################################################################

def do_query(query_string, filename):
  query = cow_query(query_string, corpus = CORPUS, references = REFERENCES,
    attributes = ['word'], structures = [], deduping = True)
  cow_print_query(query, filename)
  return int(query['hits'])

infile = open(INFILE, 'r')
outfile = open(OUTFILE, 'w')

qstrings = [
  'q_compound_joint_cap', 'q_compound_joint_nocap', 
  'q_np_det_joint_cap', 'q_np_det_joint_nocap', 'q_np_det_sep_cap', 'q_np_det_sep_nocap', 'q_np_det_sep_capcap', 
  'q_np_apprart_joint_cap', 'q_np_apprart_joint_nocap', 'q_np_apprart_sep_cap', 'q_np_apprart_sep_nocap', 'q_np_apprart_sep_capcap', 
  'q_np_nodet_joint_cap', 'q_np_nodet_joint_nocap', 'q_np_nodet_sep_cap', 'q_np_nodet_sep_nocap', 'q_np_nodet_sep_capcap', 
  'q_infzu_joint_cap', 'q_infzu_joint_nocap', 'q_infzu_sep_cap', 'q_infzu_sep_nocap', 
  'q_prog_joint_cap', 'q_prog_joint_nocap', 'q_prog_sep_cap', 'q_prog_sep_nocap', 'q_prog_sep_capcap', 
  'q_particip_joint_cap', 'q_particip_joint_nocap', 'q_particip_sep_cap', 'q_particip_sep_nocap', 'q_particip_sep_capcap', 
  'q_finite_joint_cap', 'q_finite_joint_nocap', 'q_finite_sep_cap', 'q_finite_sep_nocap'
]

# Dump header line.
header = infile.readline().strip().decode('utf-8').split('\t')
header = header + qstrings
outfile.write('\t'.join(header) + '\n')

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

  # Generate different forms for w/ LE and w/o LE.
  verb     = line[2]
  v_part   = line[8]
  v_fin    = '|'.join(filter(lambda x: not x.replace(u'ß', u'ss') == verb.replace(u'ß', u'ss'), line[10].split('|')))

  if not line[3] == '0':
    if line[3] == u's':
      noun     = line[1] + u's'
    else:
      noun     = line[7]
  else:
    noun     = line[1]

  compound = noun + verb
  particip = noun + v_part

  infzu = (noun + u'zu' + verb) if line[9] == 'X' else (noun + line[9])

  q_compound_joint_cap     = (Q_COMPOUND_ZUSAMMEN % compound).encode('utf-8')
  q_compound_joint_nocap   = (Q_COMPOUND_ZUSAMMEN % compound.lower()).encode('utf-8')
  q_np_det_joint_cap       = (Q_NP_DET_ZUSAMMEN % compound).encode('utf-8')
  q_np_det_joint_nocap     = (Q_NP_DET_ZUSAMMEN % compound.lower()).encode('utf-8')
  q_np_det_sep_cap         = (Q_NP_DET_GETRENNT % (noun, verb)  ).encode('utf-8')
  q_np_det_sep_nocap       = (Q_NP_DET_GETRENNT % (noun.lower(), verb)  ).encode('utf-8')
  q_np_det_sep_capcap      = (Q_NP_DET_GETRENNT % (noun, verb.title())  ).encode('utf-8')
  q_np_apprart_joint_cap   = (Q_NP_APPRART_ZUSAMMEN % compound).encode('utf-8')
  q_np_apprart_joint_nocap = (Q_NP_APPRART_ZUSAMMEN % compound.lower()).encode('utf-8')
  q_np_apprart_sep_cap     = (Q_NP_APPRART_GETRENNT % (noun, verb)  ).encode('utf-8')
  q_np_apprart_sep_nocap   = (Q_NP_APPRART_GETRENNT % (noun.lower(), verb)  ).encode('utf-8')
  q_np_apprart_sep_capcap  = (Q_NP_APPRART_GETRENNT % (noun, verb.title())  ).encode('utf-8')
  q_np_nodet_joint_cap     = (Q_NP_NODET_ZUSAMMEN % compound).encode('utf-8')
  q_np_nodet_joint_nocap   = (Q_NP_NODET_ZUSAMMEN % compound.lower()).encode('utf-8')
  q_np_nodet_sep_cap       = (Q_NP_NODET_GETRENNT % (noun, verb)  ).encode('utf-8')
  q_np_nodet_sep_nocap     = (Q_NP_NODET_GETRENNT % (noun.lower(), verb)  ).encode('utf-8')
  q_np_nodet_sep_capcap    = (Q_NP_NODET_GETRENNT % (noun, verb.title())  ).encode('utf-8')
  q_infzu_joint_cap        = (Q_INF_ZU_ZUSAMMEN % infzu).encode('utf-8')
  q_infzu_joint_nocap      = (Q_INF_ZU_ZUSAMMEN % infzu.lower()).encode('utf-8')
  q_infzu_sep_cap          = (Q_INF_ZU_GETRENNT % (noun, verb) ).encode('utf-8')
  q_infzu_sep_nocap        = (Q_INF_ZU_GETRENNT % (noun.lower(), verb) ).encode('utf-8')
  q_prog_joint_cap         = (Q_PROG_ZUSAMMEN % compound).encode('utf-8')
  q_prog_joint_nocap       = (Q_PROG_ZUSAMMEN % compound.lower()).encode('utf-8')
  q_prog_sep_cap           = (Q_PROG_GETRENNT % (noun, verb)  ).encode('utf-8')
  q_prog_sep_nocap         = (Q_PROG_GETRENNT % (noun.lower(), verb)  ).encode('utf-8')
  q_prog_sep_capcap        = (Q_PROG_GETRENNT % (noun, verb.title())  ).encode('utf-8')
  q_particip_joint_cap     = (Q_PARTICIP_ZUSAMMEN % particip).encode('utf-8')
  q_particip_joint_nocap   = (Q_PARTICIP_ZUSAMMEN % particip.lower()).encode('utf-8')
  q_particip_sep_cap       = (Q_PARTICIP_GETRENNT % (noun, v_part ) ).encode('utf-8')
  q_particip_sep_nocap     = (Q_PARTICIP_GETRENNT % (noun.lower(), v_part) ).encode('utf-8')
  q_particip_sep_capcap    = (Q_PARTICIP_GETRENNT % (noun, v_part.title()) ).encode('utf-8')
  q_finite_joint_cap       = (Q_FINITE_ZUSAMMEN % (noun, v_fin)).encode('utf-8')
  q_finite_joint_nocap     = (Q_FINITE_ZUSAMMEN % (noun.lower(), v_fin)).encode('utf-8')
  q_finite_sep_cap         = (Q_FINITE_GETRENNT % (noun, v_fin)).encode('utf-8')
  q_finite_sep_nocap       = (Q_FINITE_GETRENNT % (noun.lower(), v_fin)).encode('utf-8')

  qs = [
    q_compound_joint_cap, q_compound_joint_nocap, 
    q_np_det_joint_cap, q_np_det_joint_nocap, q_np_det_sep_cap, q_np_det_sep_nocap, q_np_det_sep_capcap, 
    q_np_apprart_joint_cap, q_np_apprart_joint_nocap, q_np_apprart_sep_cap, q_np_apprart_sep_nocap, q_np_apprart_sep_capcap, 
    q_np_nodet_joint_cap, q_np_nodet_joint_nocap, q_np_nodet_sep_cap, q_np_nodet_sep_nocap, q_np_nodet_sep_capcap, 
    q_infzu_joint_cap, q_infzu_joint_nocap, q_infzu_sep_cap, q_infzu_sep_nocap, 
    q_prog_joint_cap, q_prog_joint_nocap, q_prog_sep_cap, q_prog_sep_nocap, q_prog_sep_capcap, 
    q_particip_joint_cap, q_particip_joint_nocap, q_particip_sep_cap, q_particip_sep_nocap, q_particip_sep_capcap, 
    q_finite_joint_cap, q_finite_joint_nocap, q_finite_sep_cap, q_finite_sep_nocap
  ]

  print ('=== %s ===' % compound).encode('utf-8')
  counts = [0] * len(qs)
  max    = len(qs) if len(v_fin) > 0 else len(qs)-4
  for i in range(0, max):
    print ('    %s: ' % qstrings[i]).encode('utf-8'),
    filename = ('%s%s_%s.csv' % (OUTDIR, compound, qstrings[i])).encode('utf-8')
    f = do_query(qs[i], filename)
    print f
    counts[i] = f
  
  outfile.write(('\t'.join(line + [str(x) for x in counts]) + '\n').encode('utf-8'))


infile.close()
outfile.close()
