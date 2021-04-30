! Replace S:\Core\Programs\PrintTest
autoLibrary
on error goto Ertn

dim prg$*256,cap$*128,ln$*900

cancel=5
fnTop(prg$='S:\Core\Programs\PrintTest',cap$='Print Test')
fnTos
rc=0
fnLbl(1,1,'Characters Per Line:',40,2)
fnTxt(1,42,3,0,0,'30')
resp$(rc+=1)='80'
fnLbl(2,1,'Lines to print:',40,2)
fnTxt(2,42,3,0,0,'30')
resp$(rc+=1)='54'
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=cancel then goto Xit
fnopenprn
ln$=rpt$("X",val(resp$(1)))
for j=1 to val(resp$(2)) : pr #255: ln$ : next j
fncloseprn
goto Xit
 
Xit: fnXit
 
include: ertn
 
