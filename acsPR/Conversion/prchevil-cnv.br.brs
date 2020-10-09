! Replace acsPR\Conversion\prCHevil-Cnv
! CONVERT second field from PD 6 to N 6
! this is not a program to convert towards standard !!!!!

autoLibrary
on error goto Ertn


open #4: "Name=[Q]\PRmstr\PRCkHist.h[cno],RecL=150,USE",internal,outIn
L220: read #4,using L230: d1 eof L350,conv L320
L230: form pos 9,pd 6
if d1<1000000 then goto L270
d1=val(str$(d1)(5:6)&str$(d1)(7:8)&str$(d1)(3:4))
! pr D1
L270: !
rewrite #4,using L290: d1
L290: form pos 9,n 6
goto L220

L320: read #4,using L290: d1 eof L350
goto L220

L350: close #4:
execute "Index [Q]\PRmstr\PRCkHist.h[cno]"&' '&"[Q]\PRmstr\PRCKINDX.h[cno] 1 14 Replace DupKeys -n"
Xit: stop  ! Chain "PRMENU/acsPR"
include: ertn
