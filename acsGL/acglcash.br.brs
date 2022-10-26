! Replace S:\acsGL\acglCash
! Cash Flow Statement
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,actpd$*6,pedat$*20,cch$*20 ,in3$(4)
	dim pedat$*20,actpd$*6,bm(13),bp(13),by(13)
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,7),cap$*128
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
 
	fnTop(program$,cap$="Cash Flow Statement")
	report$=cap$
	fncno(cno,cnam$)
	actpd$=fnactpd$
	pedat=val(actpd$)
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",i,i,r  
	read #20,using 'form pos 384,n 2',rec=1: nap : close #20:
	fscode=fnfscode
	fnfscode
	fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit 		! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fscode=fnfscode
	priorcd=fnpriorcd
 
	pors=1
	in3$(1)="8,25,N 12.2,UT,N" : in3$(2)="8,45,N 12.2,UT,N"
	mp1=75
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno]," : _
		fl1$=fl1$&"KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
	open #1: fl1$,i,i,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L360
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L360: if fnps=2 then goto L390 ! secondary
	close #3: ioerr L370
L370: execute "Index [Q]\GLmstr\GLmstr.h[cno] [Temp]\fsindex.h[cno] 75 3 Replace DupKeys -N"
	goto L400
L390: execute "Index [Q]\GLmstr\GLmstr.h[cno] [Temp]\fsindex.h[cno] 78 3 Replace DupKeys -N"
L400: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr",i,i,k
	fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
L420: read #1,using L460: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1820
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L420
	if costcntr=0 then goto L460
	if costcntr><fc then goto L420
L460: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L490
	if heading=0 and te$><"R" then gosub HDR
L490: on pos ("RFHDTSBC",te$,1) goto L1190,L1240,L500,L560,L1080,L1190,L560,L1880 none L420
L500: pr #255,using L510: d$(1:40)
L510: form pos sp,c 40
	gosub L1390
	gosub L1320
	goto L420
 
L560: if te$="B" and ap>0 then accum1=-accum1: accum2=-accum2: goto L1080 ! ENDING BANK BALANCE
	if notrans=1 then goto L870
	if ir>=val(r$) and val(r$)><0 then goto L700
L590: ! read amounts from gl master
L600: read #3,using L690: ir,bb,cb,mat by,mat bp,mat bm eof L860
	if ir=0 then goto L600
	if fscode=0 or (fscode=pedat and priorcd=1) then goto L690
	if fscode<1 or fscode>13 then fscode=1
	if fnpriorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if fnpriorcd=2 then goto L680
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L690
L680: if fscode>1 then bb=bp(fscode-1) else bb=0
L690: form pos mp1,pd 3,pos 81,41*pd 6.2
L700: if ir=val(r$) then total+=(cb-bb) else goto L840
	if te$="B" then : _
		total-=(cb-bb): total-=bb: total2-=bp(nap) : goto L730
	total2+=cb
L730: for z=1 to 13 : annualb+=bm(z) : next z
	if fscode=0 then monthb=monthb+bm(fnactpd) else : _
		monthb=monthb+bm(fscode)
	if fscode=0 then goto L760 else goto L800
L760: for j=1 to fnactpd
		ytdb=ytdb+bm(j)
	next j
	goto L590
L800: for j=1 to fscode
		ytdb=ytdb+bm(j)
	next j
	goto L590
L840: if ir<val(r$) then goto L590
	if ir>val(r$) then goto L870
L860: notrans=1
L870: for j=1 to 9
		if ac(j)<>9 then : _
			accum(j,1)+=total : accum(j,2)+=total2
	next j
	if rs=1 then total=-total else goto L930
	total2=-total2
	ytdb=-ytdb
L930: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 or total2><0 then goto L970
	if total<>0 then goto L970
	if ls+ds+ul+ic>0 then goto L970 else goto L420
L970: sp2=30-sp-1
	if te$="B" then total=-total: total2=-total2 : _
		! Reverse sign on beginning bank balance
	if ul=1 then pr #255,using L1001: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow L1550 : goto L1000
	pr #255,using L1000: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow L1550
L1000: form pos sp,c sp2,pos 52,c 1,pic(--,---,---.##),x 1,c 1,pic(--,---,---.##),skip redir
L1001: form pos sp,c sp2,pos 52,c 1,c 5,pic(--,---,---.##),c 1,x 1,c 1,c 5,pic(--,---,---.##),c 1,skip redir
	total=0
	total2=0
	gosub L1320
	if ul=1 then goto L1050
	gosub L1560
L1050: gosub L1390
	goto L420
 
L1080: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=30-sp-1
	if te$="B" then accum2=0
	if ul=1 then pr #255,using L1001: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow L1550 : goto L1140
	pr #255,using L1000: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow L1550
L1140: gosub L1320
	if ul=1 then goto L1160
	gosub L1560
L1160: gosub L1390
	goto L420
 
L1190: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1390
	goto L420
 
L1240: if foot1=1 then goto L1290
	tabnote=sp
	foot1=1
	foot$=d$
	goto L420
L1290: foot$=rtrm$(foot$)&d$
	goto L420
 
L1320: for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1360
		accum(j,1)=0
		accum(j,2)=0
L1360: next j
return
 
L1390: if ls=0 then goto L1530
	if ls=99 then goto L1440
	pr #255,using L1420: " "
L1420: form pos 1,c 1,skip ls
	goto L1530
L1440: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1490: rtrm$(foot$),"Page "&str$(pt1)
L1490: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1530
	pr #255: newpage
	gosub HDR
L1530: return
 
L1550: gosub L1440: continue
L1560: if ul=0 then goto L1650
	if ul=1 then goto L1620
	underlin$="=============="
	goto L1630
	goto L1650
L1620: underlin$="______________"
L1630: pr #255,using L1640: underlin$,underlin$
L1640: form skip redir,pos 52,2*c 15,skip redir
L1650: if redir=0 then pr #255,using L1660: " "
L1660: form c 1,skip 1
return
 
HDR: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255: ""
	pr #255: tab(50);lpad$(trim$(fncch$),15);tab(74);"Year To"
	pr #255: tab(56);"       ";tab(75);"Date"
	pr #255: ""
return
 
L1820: eofcode=1
	gosub L1440
	fnfscode(pedat)
	fnpriorcd(1)
	fncloseprn
	goto Xit
 
L1880: fnTos
	mylen=25: mypos=mylen+3 : right=1
	fnLbl(1,1,"Total Current Month:",mylen,right)
	fnTxt(1,mypos,12,0,right,'10',0,"Enter the total for the current month.",0 ) : _
	resp$(1)=str$(total)
	fnLbl(2,1,"Total Year to Date:",mylen,right)
	fnTxt(2,mypos,12,0,right,'10',0,"Enter the total for the year.",0 ) : _
	resp$(2)=str$(total2)
	fnCmdKey("&Next",1,1,0,"Accept the answer.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	total=val(resp$(1))
	total2=val(resp$(2))
	if ckey=5 then goto Xit
Xit: fnXit
 
include: ertn
