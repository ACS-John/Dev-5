autoLibrary
on error goto Ertn
fnTop(program$)

open #paymstr:=fngethandle: "Name=[Q]\GLmstr\PayMstr.h[cno],Version=1,KFName=[Q]\GLmstr\PayIdx1.h[cno],RecL=276,kln=8,kps=1,replace",internal,outIn,keyed 
close #paymstr: 
fnindex_it("[Q]\GLmstr\paymstr.H[cno]","[Q]\GLmstr\Payidx1.H[cno]",'1 8') 
fnindex_it("[Q]\GLmstr\paymstr.H[cno]","[Q]\GLmstr\Payidx2.H[cno]",'9 38')

open #6: "Name=[Q]\GLmstr\bankrec.H[cno],KFName=[Q]\GLmstr\bankrec-idx.H[cno],Version=1,Shr",internal,outIn,keyed 
close #6: 
fnindex_it("[Q]\GLmstr\bankrec.H[cno]","[Q]\GLmstr\bankrec-idx.h[cno]",'79/3/4 12/1/8')

open #2: "Name=[Q]\GLmstr\GLTR1099.H[cno],RecL=64,Use",internal,outIn 
close #2: 
fnindex_it("[Q]\GLmstr\gltr1099.H[cno]","[Q]\GLmstr\gltridx1.H[cno]",'1 8')

goto Xit

Xit: fnXit

include: Ertn
