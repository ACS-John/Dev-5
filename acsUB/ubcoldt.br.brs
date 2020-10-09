! Replace S:\acsUB\UBColPrn
! -- Cash Receipts Journal

autoLibrary

dim dat$*20,scr1$(10)*30,alloc(10),nam$*30,o(2),route(200)
dim r(20,4),hd1$*255,serviceName$(10)*20,tg(11),resp$(7)*40
dim ml$(3)*90

fnTop(program$,"Cash Receipts Journal")

fndat(dat$,1)
fnGetServices(mat serviceName$)

gosub SCREEN1

	hd1$="{\ul  Account  }  {\ul    Total}    {\ul    Date   }"
	for j=1 to 10
		x2=pos(trim$(serviceName$(j))," ",1) : _
		if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&"-"&serviceName$(j)(x2+1:len(serviceName$(j))) ! if service name two words long, use part of both
		if trim$(serviceName$(j))<>"" then : _
			scr1$(sz1+=1)=serviceName$(j) : _
			hd1$=hd1$&"  {\ul "&lpad$(rtrm$(serviceName$(j)(1:7)),7)&"}"
	next j
	hd1$=hd1$&"  {\ul Customer Name               }"
	mat scr1$(sz1)
	mat alloc(sz1)
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\ubtrdt.h[cno],Shr",internal,input,keyed

	on fkey 5 goto Xit
	fnopenprn
	gosub HDR
goto L430



L430: read #2,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof PRTOTALS
	if ld1<>0 and tdate<ld1 then goto L430
	if hd1<>0 and tdate>hd1 then goto L430
	if tamount=0 then goto L430
	if tcode<3 or tcode>5 then goto L430 ! don't pr charges or penalties
	if tcode=3 then ti2=1 ! REG.COLLECTION
	if tcode=4 then ti2=2 ! CREDIT MEMO
	if tcode=5 then ti2=3 ! DEBIT MEMO
	if ti2=3 then r(1,1)-=tamount else r(1,1)+=tamount
	r(1,ti2+1)+=tamount
	x=0
	for j=1 to 10
		if trim$(serviceName$(j))<>"" then
			alloc(x+=1)=tg(j)
			if ti2=3 then r(x+3,1)-=tg(j) else r(x+3,1)+=tg(j)
			r(x+3,ti2+1)+=tg(j)
		end if
	next j
	c$=" "
	if tcode=4 then c$="CM" else if tcode=5 then c$="DM"
	if ti1$="True" then
		pr #255,using 'Form POS 1,C 10,N 10.2,C 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 9.2': p$,tamount,c$,tdate,mat alloc pageoflow PGOF
	end if
	if sum(alloc)<>tamount then goto L642 else goto L655
L642: !
mat ml$(3)
	ml$(1)="The breakdown on a collection transation dated "&str$(tdate)& " for customer # "&p$
	ml$(2)="does not balance.  Your totals will be off by "& trim$(cnvrt$("pic($$$,$$$.## cr)",tamount-sum(alloc)))&"." : _
	fnmsgbox(mat ml$,resp$,'',49)
L655: !
if resp$="Cancel" then goto Xit
goto L430

PGOF: pr #255: newpage
	gosub HDR
continue

HDR: !
! need date$,time$
	pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
	if ld1<>0 and hd1<>0 then : _
		pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",ld1)& "  To "&cnvrt$("pic(zzzz/zz/zz)",hd1)&"}"
	pr #255: ""
	pr #255: "\ql "&hd1$
return

PRTOTALS: !
	pr #255: ""
	pr #255: "    ************ Totals ************"
	pr #255: tab(34);"{\ul       Total}  {\ul    Reg.Col}  {\ul   Cr.Memos}  {\ul   Db.Memos}"
	for j=1 to sz1
		pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': scr1$(j),r(j+3,1),r(j+3,2),r(j+3,3),r(j+3,4) pageoflow PGOF
	next j
	pr #255: ""
	pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': "Total      ",r(1,1),r(1,2),r(1,3),r(1,4)
	fncloseprn
Xit: fnXit

SCREEN1: !
	fnTos
	mylen=33 : mypos=mylen+2
	fnLbl(1,1,"Report Heading Date:",mylen,1)
	fnTxt(1,mypos,20)
	resp$(1)=dat$
	fnLbl(2,1,"Starting Date (blank for all):",mylen,1)
	fnTxt(2,mypos,10,0,1,"3",0,"First day of the period to be printed. (ccyymmdd format)")
	resp$(2)=str$(ld1)
	fnLbl(3,1,"Ending Date (blank for all):",mylen,1)
	fnTxt(3,mypos,10,0,1,"3",0,"Last day of the period to be printed. (ccyymmdd format)")
	resp$(3)=str$(hd1)
	fnChk(4,mypos,"Include Details:",1)
	resp$(4)="True"
	fnCmdSet(3)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dat$=resp$(1)
	ld1=val(resp$(2))
	hd1=val(resp$(3))
	ti1$=resp$(4)
	fndat(dat$,2)
return

include: ertn

