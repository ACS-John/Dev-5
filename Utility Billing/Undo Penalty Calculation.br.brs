! formerly S:\acsUB\ubUnPen
autoLibrary
on error goto Ertn
fnTop(program$)
dim z$*10
dim g(12)
dim e$(4)*30
dim tg(11)
dim penatly$(10)*1,gb(10)
dim extra(23),extra$(11)*30
dim a(7)
dim serviceName$(10)*20,service$(10),tax_code$(10)*1,penalty$(10)
fnGetServices(mat serviceName$,mat service$,mat tax_code$,mat penalty$)
 
fnTos
fnLbl(1,1,"Penalty Date:",26,1)
fnTxt(1,28,10,0,0,"3")
resp$(1)=""
fnCmdSet(2)
ckey=fnAcs(mat resp$)
ubpendat=val(srep$(resp$(1),'/',''))
if ckey=5 then goto Xit
fnAutomatedSavePoint('before')
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,outIn,k
open #hTrans=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr",i,outIn,k
open #hTrans2=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",i,outIn,k
 
do
	L280: !
	read #hTrans,using 'form pos 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof Xit
	if tdate=ubpendat and tcode=2 then
		read #1,using 'form pos 1,C 10,4*C 30,pos 143,7*PD 2,pos 292,PD 4.2,PD 4,12*PD 4.2,pos 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb nokey L280
		bal=bal-tamount
		for j=1 to 10
			if uprc$(penalty$(j))="Y" then gb(j)-=tg(j) ! subtract penalty breakdown from balance breakdown
		next j
		rewrite #1,using 'form pos 1,C 10,4*C 30,pos 143,7*PD 2,pos 292,PD 4.2,PD 4,12*PD 4.2,pos 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb
		delete #hTrans:
	end if
loop
Xit: fnXit
include: ertn no
