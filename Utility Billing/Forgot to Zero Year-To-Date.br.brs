autoLibrary
on error goto Ertn
fnTop(program$)
dim srv$(3)*1
dim tg(11)
MAIN: !
fnTos
fnLbl(1,1,"First Billing Date of New Year (MMDDYY):",42,1)
fnTxt(1,44,8,0,0,"1")
resp$(1)=str$(fbd)
fnCmdSet(2)
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
fbd$=lpad$(str$(val(resp$(1))),6)
fbd=val(resp$(1)) conv MAIN
x=fndate_mmddyy_to_ccyymmdd(fbd)
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
open #2: "Name=[Q]\UBmstr\ubtransvb.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr",internal,input,keyed
do
	read #1,using 'form pos 1,c 10': z$ eof Xit
	watuse=eleuse=gasuse=0
	restore #2,key>=z$&"         ": nokey Eo2
	do
		read #2,using 'form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof Eo2
		if p$=z$ and tcode=1 and tdate>=x then ! only charge transactions
			watuse+=wu
			eleuse+=eu
			gasuse+=gu
		end if
	loop while p$=z$
	Eo2: !
	rewrite #1,using 'form pos 232,pd 5,pos 252,pd 5,pos 272,pd 5': watuse,elecuse,gasuse
loop
 
Xit: fnXit
include: ertn
 
