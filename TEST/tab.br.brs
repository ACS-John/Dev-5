library 's:\Core\Library': fnTab
library 's:\Core\Library': fnAcs2
library 's:\Core\Library': fnToS
library 's:\Core\Library': fnToP
library 's:\Core\Library': fnCmdSet
library 's:\Core\Library': fnTxt
library 's:\Core\Library': fnLbl
fnToP(program$)
fnToS
dim cap$(2)
dim resp$(2)*128
cap$(1)='one'
cap$(2)='two'
fnTab(1,1,20,60,mat cap$)
fnLbl(1,1,'one:', 5,1,0,0,1,'label tooltip 1')
fnTxt(1,10,0,0,0,'',0,'tooltip 1',0,1)
resp$(1)='one'

fnCmdSet(1)
fnAcs2(mat resp$,ckey)
pr 'ckey=';ckey
pr mat resp$