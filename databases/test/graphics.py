from __future__ import print_function

import os
from os import listdir, walk
from os.path import isfile, join, abspath, dirname, isfile
from glob import glob
import sys
from pprint import pprint
from string import Template

def fixReal(r) :
    return float(r)*100.

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
      mrate = fixReal(raw_input())
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
        , 'Min'      : "%2.1f"%rmin
        , 'Max'      : "%2.1f"%rmax
        , 'Mean'     : "%2.1f"%rmean
        , 'Stand.'   : "%2.1f"%rstand
        , 'Int.'     : "[%2.1f %2.1f]"%(interval[0], interval[1])
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


sep = "% " + "-"*77 + "\n\n"

with open(join(basedir, "main.tex"), 'w') as mainLatex:

  sections = []
  for mrate in sorted(test.keys()):
    figures = []
    print("% Mrate\tWin\tLost\tDataset")
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

      print("% {:1.2f}\t{:2d}/50 \t{:2d}/50\t{}".format(mrate, arsiwins, arsilost, dname))
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
          'missingrate' : str(int(mrate))
        , 'dataset'     : str(dname).capitalize()
        , 'table'       : sTable
        , 'addplots'    : '\n\n'.join(addplots)
        , 'algorithms'  : ', '.join(algos)
        , 'nalgos'      : len(algos)
        , 'arsiwins'    : arsiwins
        , 'arsilost'    : arsilost
        }
      template = open( join(basedir, "figure-template.tex"))
      src = Template( template.read() ).safe_substitute(values)
      figures.append(src)
    print("%", "-"*78)

    sFigures = sep.join(figures)

    template = open( join(basedir, "section-template.tex"))
    section = Template( template.read() )\
      .safe_substitute({"figures" : sFigures, "missingrate" : str(int(mrate))})
    sections.append(section)
  sFigures = sep.join(sections)

  # ----------------------------------------------------------------------------

  fields = ["Min", "Max", "Mean", "Int."]
  datasets = [
    "breast-cancer"
  , "spect"
  , "lymphography"
  , "soybean"
  , "zoo"
  ]
  algos = ["ARSI", "ROUSTIDA", "VTRIDA"]


  nameDataset = \
   {
    "breast-cancer" : "B. Cancer"
  , "spect"         : "Spect"
  , "lymphography"  : "Lymph."
  , "soybean"       : "Soybean"
  , "zoo"           : "Zoo"
   }

  ListRates  = [
             map(fixReal, ["0.05", "0.10", "0.15", "0.20"])
           , map(fixReal, ["0.25", "0.30", "0.35", "0.40"])
           ]

  tables = []
  for rates in ListRates:
    latexTabular = ""

    calgos = 'c'*len(algos)
    numCols = str(2 + (len(algos)*len(rates)))

    latexTabular += "\\begin{tabular}{|c|c|" + '|'.join([calgos]*len(rates)) + "|}\n"
    latexTabular += "\\hline\n"

    header = "Dataset& & " + '& '.join(["\\multicolumn{3}{c|}{$%s"%mrate + "\%$}" for mrate in rates])
    header += "\\\\ \\cline{3-"+numCols+"}\n\n"

    latexTabular += header

    titles = '& ' + ''.join(["&\\textsc{%s} "%aname for aname in algos]*len(rates))
    titles += "\\\\ \\cline{1-"+numCols+"}\n"
    latexTabular += titles

    row = ""
    for dname in datasets:
      row += "\\multirow{4}{1.6cm}{%s}"%nameDataset[dname] + "\n"
      for field in fields:
        row += "&" +  field + "\n"
        for mrate in rates:
          if field ==  "Int.":
            for aname in algos:
              row += " &" + test[mrate][dname][aname][50][field]
          else:
            arsiwins = False
            carsiwins = 0
            if "ROUSTIDA" in algos:
              if float(test[mrate][dname]['ARSI'][50][field])\
               > float(test[mrate][dname]['ROUSTIDA'][50][field]):
                carsiwins += 1
            if "VTRIDA" in algos:
              if float(test[mrate][dname]['ARSI'][50][field])\
               > float(test[mrate][dname]['VTRIDA'][50][field]):
                carsiwins += 1
            if carsiwins + 1 == len(algos):
              arsiwins = True

            for aname in algos:
              row += " &"
              if arsiwins and aname == "ARSI":
                row += "{\\bfseries "
              row += test[mrate][dname][aname][50][field]
              if arsiwins and aname == "ARSI":
                row += "}"

          row +="  % -- " +  str(mrate) + "\n"
        row += "\\\\\n"
      row += "\\cline{1-"+numCols+"}" + "\n"
    row += "\\hline" + "\n"

    latexTabular += row
    latexTabular += "\\end{tabular}"

    template = open( join(basedir, "table-template.tex"))
    sTable = Template(template.read())\
          .safe_substitute({"tabular" : latexTabular
                           , "rates"  : '\%, '.join(map(lambda x :"%2.1f"%x, rates)) + '\%'
                           })

    tables.append(sTable)

  # ----------------------------------------------------------------------------

  sTables = sep.join(tables)

  template = open( join(basedir, "main-template.tex"))
  src = Template( template.read() )\
        .safe_substitute({ "figures" : sFigures
                         , "tables"  : sTables})

  print(src, file=mainLatex)



