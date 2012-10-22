#!/usr/bin/gnuplot

#dir = 'log-10-100-3'
dir = 'log-10-500-1'


#t = "wxt"
t = "png"


set key top left box opaque

set terminal t
if (t eq "png") set terminal t size 1200,900



file = sprintf('%s/log', dir)

# Graph               | Mine                 | FGL             |
# no  lnc  rnc     ec |   mc       t     rss |       t     rss | ok
#  1    2    3      4 5    6       7     8   9      10      11 12 13

lnc=2; rnc=3; ec=4; mc=6;   t1=7; m1=8;  t2=10; m2=11;



set title "time[s]/edges"
if (t ne "wxt") set output sprintf("%s/time_edges.%s", dir, t)

plot file using ec:t2 title "fgl", file using ec:t1 title "mine"

if (t eq "wxt") pause mouse close "Close window to continue\n"



set title "time[s]/nodes"
if (t ne "wxt") set output sprintf("%s/time_nodes.%s", dir, t)

plot file using (column(lnc) + column(rnc)):t2 title "fgl", file using (column(lnc) + column(rnc)):t1 title "mine"

if (t eq "wxt") pause mouse close "Close window to continue\n"



set title "time[s]/matches"
if (t ne "wxt") set output sprintf("%s/time_matches.%s", dir, t)

plot file using mc:t2 title "fgl", file using mc:t1 title "mine"

if (t eq "wxt") pause mouse close "Close window to continue\n"



set title "max RSS[kB]/edges"
if (t ne "wxt") set output sprintf("%s/mem_edges.%s", dir, t)

plot file using ec:m2 title "fgl", file using ec:m1 title "mine"

if (t eq "wxt") pause mouse close "Close window to continue\n"



set title "max RSS[kB]/nodes"
if (t ne "wxt") set output sprintf("%s/mem_nodes.%s", dir, t)

plot file using (column(lnc) + column(rnc)):m2 title "fgl", file using (column(lnc) + column(rnc)):m1 title "mine"

if (t eq "wxt") pause mouse close "Close window to continue\n"



set title "max RSS[kB]/matches"
if (t ne "wxt") set output sprintf("%s/mem_matches.%s", dir, t)

plot file using mc:m2 title "fgl", file using mc:m1 title "mine"


