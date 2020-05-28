! Replace S:\acsUB\ubColDel
! -- Remove Cash Receipt Records
! if the collections ever exceed 100,000 then sort wont work for the monthly receipts journal  ( one clue is the address is -2020202 instead of a real address
autoLibrary
fnTop(program$,"Remove Cash Receipt Records")
on error goto Ertn

dim alloc(10)
dim _o(2)

open #6: "Name=[Q]\UBmstr\Collect.h[cno]",internal,input
open #7: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=72",internal,output

gosub ASKDATE
READ_COLLECT: !
	read #6,using L200: x$,_m,_n,mat _o,adrnxt,rcpt$,mat alloc eof END1
	L200: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pd 3,c 9,10*pd 4.2
	n2=fn_cd(_n)
	if int(n2*.0001)<50 then n2=n2+20000000 else n2=n2+19000000
	if n2<ld1 then goto READ_COLLECT
	write #7,using L200: x$,_m,_n,mat _o,adrnxt,rcpt$,mat alloc
goto READ_COLLECT

ASKDATE: !
	sn$="ubcoldel"
	fnTos(sn$)
	mylen=37
	mypos=mylen+2
	respc=0
	fnLbl(1,1,"Lowest Date to Retained (mm\dd\ccyy):",mylen,1)
	fnTxt(1,mypos,10,10,0,"2")
	resp$(respc+=1)=""
	fnCmdSet(2)
	fnAcs2(mat resp$,ckey)
	if ckey=5 then goto Xit
	ld1=val(resp$(1)) conv ASKDATE
	ld1=fn_cd2(ld1)
	cutoff$=str$(ld1)
return
END1: !
	close #7:
	close #6,free:
	execute "Rename "&env$('temp')&"\Work."&session$&' '&"[Q]\UBmstr\Collect.h[cno] -n"
Xit: fnXit
def fn_cd(x)=(x-int(x*.01)*100)*10000+int(x*.01) ! /r
def fn_cd2(x)=(x-int(x*.0001)*10000)*10000+int(x*.0001) ! /r
include: Ertn