#!/usr/bin/gnuplot

#dir = 'log-100-10'
#dir = 'log-100-4'
dir = 'log-400-4'

t = "wxt"
#t = "png"
#t = "pdf"

file = sprintf('%s/log', dir)

# <no> | <lnc> <rnc> <ec> | <mc> <t> <rss> | <mc> <t> <rss> | ok
#   1      3     4    5      7    8    9      11  12   13

set key top left box opaque

set terminal t
if (t eq "png") set terminal t size 1024,768



set title "time[s]/edges"
set output sprintf("%s/time_edges.%s", dir, t)

plot file using 5:12 title "fgl", file using 5:8 title "mine"

if (t eq "wxt") pause -1  "Hit return to continue"



set title "time[s]/nodes"
set output sprintf("%s/time_nodes.%s", dir, t)

plot file using ($3 + $4):12 title "fgl", file using ($3 + $4):8 title "mine"

if (t eq "wxt") pause -1  "Hit return to continue"



set title "time[s]/matches"
set output sprintf("%s/time_matches.%s", dir, t)

plot file using 11:12 title "fgl", file using 7:8 title "mine"

if (t eq "wxt") pause -1  "Hit return to continue"



set title "max RSS[kB]/edges"
set output sprintf("%s/mem_edges.%s", dir, t)

plot file using 5:13 title "fgl", file using 5:9 title "mine"

if (t eq "wxt") pause -1  "Hit return to continue"



set title "max RSS[kB]/nodes"
set output sprintf("%s/mem_nodes.%s", dir, t)

plot file using ($3 + $4):13 title "fgl", file using ($3 + $4):9 title "mine"

if (t eq "wxt") pause -1  "Hit return to continue"



set title "max RSS[kB]/matches"
set output sprintf("%s/mem_matches.%s", dir, t)

plot file using 11:13 title "fgl", file using 7:9 title "mine"

