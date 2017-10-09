import os
from os import listdir, walk
from os.path import isfile, join, abspath, dirname, isfile
from glob import glob
import sys
from pprint import pprint
from string import Template

def fixReal(r) : return float(r)*100.

basedir = abspath(dirname(__file__))
dirs = [d for d in os.listdir(basedir) if not isfile(d)]

test = {}
for d in dirs:
  dir = join(basedir,d)
  files = [f for f in os.listdir(dir) if f.endswith(".tsv")]
  for file in files:
    # data = file[:-4].split("-")
    # database = data[0]
    # algo = data[1]
    # missingRate = data[2]
    # n = data[3]

    absfile = join(dir, file)
    with open(absfile, 'r') as sys.stdin:

      aname = raw_input()
      dname = raw_input()
      mrate = float(raw_input())
      n     = int(raw_input())

      results = map(fixReal, raw_input().split())
      rmin, rmax    = map(fixReal,raw_input().split())
      rmean, rstand = map(fixReal,raw_input().split())
      interval = map(fixReal,raw_input().split())

      test[mrate] = test.get(mrate,{})
      test[mrate][dname] =  test[mrate].get(dname,{})
      test[mrate][dname][aname] = test[mrate][dname].get(aname,{})
      test[mrate][dname][aname][n] = test[mrate][dname][aname].get(n,{})


      test[mrate][dname][aname][n] = \
        {
          'file'     : absfile
        , 'results'  : results
        , 'min'      : rmin
        , 'max'      : rmax
        , 'mean'     : rmean
        , 'stand'    : rstand
        , 'interval' : interval
        }


colors = \
  {
    'ARSI'     : "arsiColor"
  , 'ROUSTIDA' : "roustidaColor"
  , 'VTRIDA'   : "vtridaColor"
  }

lines = \
  {
    'ARSI'     : "solid"
  , 'ROUSTIDA' : "dashed"
  , 'VTRIDA'   : "dotted"
  }

mark = \
  {
    'ARSI'     : "square*"
  , 'ROUSTIDA' : "pentagon*"
  , 'VTRIDA'   : "oplus*"
  }


sep = "% " + "-"*78 + "\n\n"

with open(join(basedir, "main.tex"), "w") as sys.stdout:

  sections = []
  for mrate in sorted(test.keys()):
    figures = []
    print "% Mrate\tWin\tLost\tDataset"
    for dname in test[mrate].keys():
      algos = sorted([a for a in test[mrate][dname].keys()])
      addplots = []
      for algo in algos:
        template = open( join(basedir, "addplot-template.tex"))
        src = Template(template.read()) \
            .safe_substitute(\
                { "color" : colors[algo]
                , "name"  : algo
                , "mark"  : mark[algo]
                , "lines" : lines[algo]
                })
        addplots.append(src)

      # comment = ["% ", "-"*77, '\n'] + \
      #   ["%\t", str(mrate),"\t", str(dname), "\t",str(algos), '\n'] + \
      #   ["%", "-"*77, '\n']
      # sComment = ''.join(comment)

      header =  "Sample,\t" + ',\t'.join(algos) +  "\\\\"

      # ------------------------------------------------------------------
      # Check how many times ARSI wins
      # ------------------------------------------------------------------

      arsiwins = 0
      arsilost = 0

      for i in range(50):
        carsiwins = 0
        carsilost = 0

        if "ROUSTIDA" in algos:
          if test[mrate][dname]['ARSI'][50]['results'][i]\
           > test[mrate][dname]['ROUSTIDA'][50]['results'][i]:
            carsiwins += 1
        if "VTRIDA" in algos:
          if test[mrate][dname]['ARSI'][50]['results'][i]\
           > test[mrate][dname]['VTRIDA'][50]['results'][i]:
            carsiwins += 1
        if carsiwins + 1 == len(algos):
          arsiwins += 1

        if "ROUSTIDA" in algos:
          if test[mrate][dname]['ARSI'][50]['results'][i]\
           < test[mrate][dname]['ROUSTIDA'][50]['results'][i]:
            carsilost += 1
        if "VTRIDA" in algos:
          if test[mrate][dname]['ARSI'][50]['results'][i]\
           < test[mrate][dname]['VTRIDA'][50]['results'][i]:
            carsilost += 1
        if carsilost + 1 == len(algos):
          arsilost += 1

      print "% {:1.2f}\t{:2d}/50 \t{:2d}/50\t{}".format(mrate, arsiwins, arsilost, dname)
      # ------------------------------------------------------------------

      table  = [ header]
      for i in range(50):
        row =  [test[mrate][dname][aname][50]['results'][i] for aname in algos]
        fixedRow = map(lambda r : "%.1f"%r,row)
        srow = ',\t'.join(["{:2d}".format(i+1)] + fixedRow) + "\\\\"
        table.append(srow)
      sTable = '\n'.join(table)

      values = \
        {
          'missingrate' : "%.0f"% fixReal(mrate)
        , 'dataset'     : str(dname).capitalize()
        , 'table'       : sTable
        , 'addplots'    : '\n\n'.join(addplots)
        , 'algorithms'  : ', '.join(algos)
        , 'nalgos'      : len(algos)
        , 'arsiwins'    : arsiwins
        }
      template = open( join(basedir, "figure-template.tex"))
      src = Template( template.read() ).safe_substitute(values)
      figures.append(src)
    print "%", "-"*78


    sFigures = sep.join(figures)

    template = open( join(basedir, "section-template.tex"))
    section = Template( template.read() )\
      .safe_substitute({"figures" : sFigures, "missingrate" : "%.0f"% fixReal(mrate)})
    sections.append(section)

  sSections = sep.join(sections)
  template = open( join(basedir, "main-template.tex"))
  src = Template( template.read() ).safe_substitute({"sections" :sSections})
  print src
