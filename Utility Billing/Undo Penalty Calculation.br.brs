! formerly S:\acsUB\ubUnPen
library 'S:\Core\Library': fnAcs2
library 'S:\Core\Library': fnLbl,fnxit,fnTxt,fnTos,fnCmdSet,fntop
library 'S:\Core\Library': fnAutomatedSavePoint,fnget_services
on error goto ERTN
fntop(program$)
dim z$*10
dim g(12)
dim e$(4)*30
dim tg(11)
dim penatly$(10)*1,gb(10)
dim extra(23),extra$(11)*30
dim a(7)
dim serviceName$(10)*20,service$(10),tax_code$(10)*1,penalty$(10)
fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$)

fnTos(sn$="ubUnPen") 
fnLbl(1,1,"Penalty Date:",26,1)
fnTxt(1,28,10,0,0,"3") 
resp$(1)=""
fnCmdSet(2) 
fnAcs2(mat resp$,ckey)
ubpendat=val(srep$(resp$(1),'/',''))
if ckey=5 then goto XIT
fnAutomatedSavePoint('before')
open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
open #2: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubtrindx.h[cno],Shr",internal,outIn,keyed 

do
	L280: !
	read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof XIT
	if tdate=ubpendat and tcode=2 then 
		read #1,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb nokey L280
		bal=bal-tamount
		for j=1 to 10
			if uprc$(penalty$(j))="Y" then gb(j)=gb(j)-tg(j) ! subtract penalty breakdown from balance breakdown
		next j
		rewrite #1,using 'Form POS 1,C 10,4*C 30,POS 143,7*PD 2,POS 292,PD 4.2,PD 4,12*PD 4.2,POS 388,10*PD 5.2',key=z$: z$,mat e$,mat a,bal,f,mat g,mat gb
		delete #2: 
	end if
loop 
XIT: fnxit
include: ertn no
