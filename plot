#!/usr/bin/gnuplot



term = "wxt"
#term = "png"


set key top left box opaque

set terminal term
if (term eq "png") set terminal term size 1200,900



# n+m  n    m    v    [s]    [kB]
  s=1; n=2; m=3; v=4; cpu=5; rss=6;


set title "time[s]/nodes+edges"
if (term ne "wxt") set output sprintf("%s/time_edges.%s", dir, term)

plot "tmp/timings.fglMatcher"    using s:cpu  title 'fglMatcher', \
     "tmp/timings.matcher"       using s:cpu  title 'matcher'

if (term eq "wxt") pause mouse close "Close window to continue\n"



#set title "time[s]/nodes"
#if (term ne "wxt") set output sprintf("%s/time_nodes.%s", dir, term)
#
#plot f("allstrict",data)     using (column(lc) + column(rc)):t title "allstrict" \
#   , f("foldstrict",data)    using (column(lc) + column(rc)):t title "foldstrict" \
#   , f("insertstrict",data)  using (column(lc) + column(rc)):t title "insertstrict" \
##   , f("nonstrict",data)     using (column(lc) + column(rc)):t title "nonstrict"
#
#if (term eq "wxt") pause mouse close "Close window to continue\n"
#
#
#
#set title "time[s]/matches"
#if (term ne "wxt") set output sprintf("%s/time_matches.%s", dir, term)
#
#plot f("allstrict",data)     using mc:t title "allstrict" \
#   , f("foldstrict",data)    using mc:t title "foldstrict" \
#   , f("insertstrict",data)  using mc:t title "insertstrict" \
##   , f("nonstrict",data)     using mc:t title "nonstrict"
#
#if (term eq "wxt") pause mouse close "Close window to continue\n"
#
#
#
#set title "max RSS[kB]/edges"
#if (term ne "wxt") set output sprintf("%s/mem_edges.%s", dir, term)
#
#plot f("allstrict",data)     using ec:m title "allstrict" \
#   , f("foldstrict",data)    using ec:m title "foldstrict" \
#   , f("insertstrict",data)  using ec:m title "insertstrict" \
##   , f("nonstrict",data)     using ec:m title "nonstrict"
#
#if (term eq "wxt") pause mouse close "Close window to continue\n"
#
#
#
#set title "max RSS[kB]/nodes"
#if (term ne "wxt") set output sprintf("%s/mem_nodes.%s", dir, term)
#
#plot f("allstrict",data)     using (column(lc) + column(rc)):m title "allstrict" \
#   , f("foldstrict",data)    using (column(lc) + column(rc)):m title "foldstrict" \
#   , f("insertstrict",data)  using (column(lc) + column(rc)):m title "insertstrict" \
##   , f("nonstrict",data)     using (column(lc) + column(rc)):m title "nonstrict"
#
#if (term eq "wxt") pause mouse close "Close window to continue\n"
#
#
#
#set title "max RSS[kB]/matches"
#if (term ne "wxt") set output sprintf("%s/mem_matches.%s", dir, term)
#
#plot f("allstrict",data)     using mc:m title "allstrict" \
#   , f("foldstrict",data)    using mc:m title "foldstrict" \
#   , f("insertstrict",data)  using mc:m title "insertstrict" \
##   , f("nonstrict",data)     using mc:m title "nonstrict"

