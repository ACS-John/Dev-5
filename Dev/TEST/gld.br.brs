autoLibrary

dim resp$(10)*80

fnTos
lc=0
fnComboF('gld-alpha',lc+=1,1,0,"[Q]\CLmstr\GLmstr.h[cno]",13,20,1,12,"[Q]\CLmstr\GLIndx2.h[cno]",1)
resp$(1)=fngld$('0   700  0')
fnCmdSet(1)
ckey=fnAcs(mat resp$)
pr resp$(1)
pr fngld$('0   375  0')
pr 'first 0 should be in pos 24 - first blank in 22'
stop
