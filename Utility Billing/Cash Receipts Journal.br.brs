! formerly S:\acsUB\UBColPrn
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit, fnLbl,fnTxt,fnTos
	library 'S:\Core\Library': fnopenprn,fncloseprn
	library 'S:\Core\Library': fnget_services
	library 'S:\Core\Library': fnChk,fnAcs2,fnCmdSet,fnmsgbox,fngethandle
! ______________________________________________________________________
	dim scr1$(10)*30
	dim alloc(10)
	dim nam$*30
	dim route(200)
	dim r(20,4)
	dim hd1$*255
	dim serviceName$(10)*20
	dim tg(11)
	dim resp$(7)*40
	dim ml$(3)*90
! ______________________________________________________________________
	fntop(program$)
! skip_header=1 ! <--  this is really a developer only option.
	fnget_services(mat serviceName$)
	gosub SCREEN1
	hd1$="{\ul  Account  }  {\ul    Total}    {\ul    Date   }"
	for j=1 to 10
		x2=pos(trim$(serviceName$(j))," ",1)
		if x2>0 then serviceName$(j)=serviceName$(j)(1:2)&"-"&serviceName$(j)(x2+1:len(serviceName$(j))) ! if service name two words long, use part of both
		if trim$(serviceName$(j))<>"" then
			scr1$(sz1+=1)=serviceName$(j)
			hd1$=hd1$&"  {\ul "&lpad$(rtrm$(serviceName$(j)(1:7)),7)&"}"
		end if
	next j
	hd1$=hd1$&"  {\ul Customer Name               }"
	mat scr1$(sz1)
	mat alloc(sz1)
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	open #h_trans:=2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed
! ______________________________________________________________________
	fnopenprn
	gosub HDR
!
MAIN_LOOP_TOP: !
	read #h_customer,using 'Form POS 1,C 10,POS 41,C 28,pos 1741,n 2',release: z$,nam$,extra1 eof PRTOTALS
	restore #h_trans,key>=z$&"         ": nokey MAIN_LOOP_TOP
READ_TRANS: !
	read #h_trans,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof MAIN_LOOP_TOP
	if p$<>z$ then goto MAIN_LOOP_TOP
	if ld1<>0 and tdate<ld1 then goto READ_TRANS
	if hd1<>0 and tdate>hd1 then goto READ_TRANS
	if tamount=0 then goto READ_TRANS
	if tcode<3 or tcode>5 then goto READ_TRANS ! don't pr charges or penalties
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
	if tcode=4 then
		c$="CM"
	else if tcode=5 then
		c$="DM"
	end if
	if ti1$="True" then
		pr #255,using 'Form POS 1,C 10,N 10.2,C 4,PIC(ZZZZ/ZZ/ZZ),SZ1*N 9.2,X 3,C 30': z$,tamount,c$,tdate,mat alloc,nam$(1:25) pageoflow PGOF
	end if
	if extra1<0 or extra1>200 then extra1=200
	if sum(alloc)<>tamount then
		mat ml$(3)
		ml$(1)="The breakdown on a collection transaction dated "&str$(tdate)& " for customer "&z$
		ml$(2)="does not balance.  Your totals will be off by "& trim$(cnvrt$("pic($$$,$$$.## cr)",tamount-sum(alloc)))&"."
		ml$(3)="(transaction record number: "&str$(rec(h_trans))&')'
		fnmsgbox(mat ml$,resp$,'',49)
	end if
	route(extra1)+=tamount
	if resp$="Cancel" then goto XIT
	goto READ_TRANS
! ______________________________________________________________________
PGOF: ! r:
	pr #255: newpage
	gosub HDR
	continue  ! /r
PRTOTALS: ! r:
	pr #255: ""
	pr #255: "    ************ Totals ************"
	pr #255: tab(34);"{\ul       Total}  {\ul    Reg.Col}  {\ul   Cr.Memos}  {\ul   Db.Memos}"
	for j=1 to sz1
		pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': scr1$(j),r(j+3,1),r(j+3,2),r(j+3,3),r(j+3,4) pageoflow PGOF
	next j
	pr #255: ""
	pr #255,using 'Form POS 4,C 30,N 11.2,3*N 12.2': "Total      ",r(1,1),r(1,2),r(1,3),r(1,4)
	if routetotals<>0 then
		pr #255,using "form skip 2,c 20": "Route Totals"
		for j=1 to 200
			if route(j)<>0 then
				pr #255,using "form pos 1,c 10,pic(zzz,zzz,zzz.##)": "Route "&cnvrt$("pic(zzz)",j),route(j) pageoflow PGOF
			end if
		next j
	end if
	fncloseprn
	goto XIT ! /r
XIT: fnxit
SCREEN1: ! r:
	fnTos
	mylen=33 : mypos=mylen+2
	fnLbl(2,1,"Starting Date (blank for all):",mylen,1)
	fnTxt(2,mypos,10,0,1,"3",0,"First day of the period to be printed. (ccyymmdd format)")
	resp$(1)=str$(ld1)
	fnLbl(3,1,"Ending Date (blank for all):",mylen,1)
	fnTxt(3,mypos,10,0,1,"3",0,"Last day of the period to be printed. (ccyymmdd format)")
	resp$(2)=str$(hd1)
	fnChk(4,mypos,"Include Details:",1)
	resp$(3)="True"
	fnChk(5,mypos,"Show Totals by Route:",1)
	resp$(4)="False"
	fnCmdSet(3)
	fnAcs2(mat resp$,ck)
	if ck=5 then goto XIT
	ld1=val(resp$(1))
	hd1=val(resp$(2))
	ti1$=resp$(3)
	if resp$(4)="True" then routetotals=1
return  ! /r
HDR: ! r:
	if ~skip_header then
		pr #255: "\qc  {\f181 \fs20 \b "&env$('cnam')&"}"
		pr #255: "\qc  {\f181 \fs28 \b "&env$('program_caption')&"}"
		! need date$,time$
		if ld1<>0 and hd1<>0 then
			pr #255: "\qc  {\f181 \fs18 \b From "&cnvrt$("pic(zzzz/zz/zz)",ld1)& "  To "&cnvrt$("pic(zzzz/zz/zz)",hd1)&"}"
		end if
		pr #255: ""
		pr #255: "\ql "&hd1$
	end if
return  ! /r
include: ertn
