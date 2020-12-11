! formerly S:\acsGL\acglinc
! -- pr Income Statement : _
	! FOR 8 1/2 * 11 PAPER WITHOUT PERCENTAGES
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,cch$*20,by(13),bp(13),cap$*128,form$*200
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),sc1$(2)*20
 
	fnTop(program$)
	actpd$=fnactpd$
	actpd=fnactpd
	! fscode=fnfscode
	! priorcd=fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnps,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	fscode=fnfscode
	priorcd=fnpriorcd
	pors=1
	mp1=69
	if fnps=2 then
		mp1=mp1+3
		mp2=78
		fl1$="Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr"
	else
		mp2=75
		fl1$="Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr"
	end if
	open #h_acglfnx:=1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L320
	fnTos(sn$="GLInput")
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 )
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L320: !
	fnopenprn
	if fnps=2 then ! secondary
		execute "Index [Q]\GLmstr\GLmstr.h[cno] [Temp]\fsindex.h[cno] 72 3 Replace DupKeys -N"
	else
		execute "Index [Q]\GLmstr\GLmstr.h[cno] [Temp]\fsindex.h[cno] 69 3 Replace DupKeys -N"
	end if
	open #h_glmstr:=3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr",internal,input,keyed
F_GLMSTR_A: form pos mp1,pd 3,pos 81,2*pd 6.2
F_GLMSTR_B: form pos 1,c 12,pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
	report$="Statement of Income and Expenses"
! GOSUB BLDPCT1 ! BUILD % BASED ON REF # IN PRIMARY FUND # IN G/L ACCOUNT
READ_ACGLFNS: !
	read #h_acglfnx,using 'form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3': r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1650
 ! if trim$(d$(1:sp2))='INCOME TAX' then debug_this=1 else debug_this=0 ! pr #255: 'just read the d$' : pause
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto READ_ACGLFNS
	if costcntr=0 then goto L450
	if fc=0 and te$="F" then goto L460 ! 5/8/89
	if costcntr><fc then goto READ_ACGLFNS
L450: !
L460: if te$="S" or te$="F" then goto L480
	if heading=0 and te$><"R" then gosub HDR_REPORT
L480: on pos ("RFHDTS",te$,1) goto L1070,L1110,L490,L540,L970,L1070 none READ_ACGLFNS
L490: pr #255,using L500: d$(1:40)
L500: form pos sp,c 40,skip 1
	gosub HDR_COLUMN_A
	gosub RESET_ACCUM_ARRAY
	goto READ_ACGLFNS
L540: if notrans=1 then goto L750
	if ir=val(r$) and val(r$)><0 then goto L680
	if ir>val(r$) then goto L680
L570: ! read gl master file for amounts
L580: read #h_glmstr,using F_GLMSTR_B: gl_number$,ir,pcr,bb,cb,mat by,mat bp eof L740
	if ir=0 then goto L580 ! skip any gl accounts not pointed to ic
	if fscode=0 or (fscode=actpd and priorcd=1) then goto L680
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L670
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L680
L670: !
	if fscode>1 then bb=bp(fscode-1) else bb=0
L680: !
	if ir=val(r$) then
!  if debug_this then pr gl_number$;'  total(';total;')+=(cb(';cb;')-bb(';bb;'))'
		total=total+(cb-bb)
!  if debug_this then pr gl_number$;'  so now total=';total: pause
	else
		goto L720
	end if
	total2=total2+cb
	k$=cnvrt$("N 5",pcr)
	goto L570
L720: if ir<val(r$) then goto L570
	if ir>val(r$) then goto L750
L740: notrans=1
L750: !
	for j=1 to 9
		if ac(j)=9 then goto L790 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
L790: !
	next j
	if rs=1 then
		total=-total
	else
		goto L820
	end if
	total2=-total2
L820: !
	if ds=1 then
		dollar$="$"
	else
		dollar$=" "
	end if
	if total><0 or total2><0 then goto L850
	if ls+ul+ds+ic>0 then goto L850 else goto READ_ACGLFNS
L850: sp2=49-sp-1
! If DS=1 Then dOLLAR$="": foRM$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC($$$$,$$$,$$$.##),C 1,POS 74,C 1,C 5,PIC($$$$$$,$$$,$$$.##),C 1,skip 1" Else foRM$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC(----,---,---.##),C 1,POS 74,C 1,C 5,PIC(------,---,---.##),C 1,skip 1"
! if debug_this then pr #255: '***'
	if ul=1 then
		pr #255,using L856: d$(1:sp2),dollar$,"{\ul ",total,"}",dollar$,"{\ul ",total2,"}" pageoflow PGOF
L856: form pos sp,c sp2,pos 50,c 1,c 5,pic(----,---,---.##),c 1,pos 74,c 1,c 5,pic(------,---,---.##),c 1,skip 1
	else
		pr #255,using L870: d$(1:sp2),dollar$,total,dollar$,total2 pageoflow PGOF
L870: form pos sp,c sp2,pos 49,c 1,pic(-----,---,---.##),pos 67,c 1,pic(-------,---,---.##),skip 1
	end if
! if debug_this then pr #255: '***'
	if pc0=1 then gosub BLDPCT2
	if pc3>0 or pc4>0 then pr #255,using L900: pc3,pc4
L900: form pos 63,n 4,pos 82,n 4,skip 1
	total=0
	total2=0
	gosub RESET_ACCUM_ARRAY
	if ul=1 then goto L950
	gosub L1410
L950: !
	gosub HDR_COLUMN_A
	goto READ_ACGLFNS
L970: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$="$" else dollar$=" "
	sp2=49-sp-1
	if ds=1 then
		dollar$=""
		form$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC($---,---,---.##),C 1,POS 74,C 1,C 5,PIC($-----,---,---.##),C 1,skip 1"
	else
		form$="Form POS SP,C SP2,POS 50,C 1,C 5,PIC(----,---,---.##),C 1,POS 74,C 1,C 5,PIC(------,---,---.##),C 1,skip 1"
	end if
! pr some sub total like thingies
	if ul=1 then
		pr #255,using L856: d$(1:sp2),dollar$,"{\ul ",accum1,"}",dollar$,"{\ul ",accum2,"}" pageoflow PGOF
	else
		pr #255,using L870: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow PGOF
	end if
	gosub RESET_ACCUM_ARRAY
	if ul=1 then goto L1050
	gosub L1410
L1050: !
	gosub HDR_COLUMN_A
	goto READ_ACGLFNS
L1070: ! r:
	if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub HDR_COLUMN_A
	goto READ_ACGLFNS ! /r
L1110: !
	if foot1=1 then goto L1160
	tabnote=sp
	foot1=1
	foot$=d$
	goto READ_ACGLFNS
L1160: !
	foot$=rtrm$(foot$)&d$
	goto READ_ACGLFNS
RESET_ACCUM_ARRAY: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then goto L1220 ! 10/14/87
		accum(j,1)=0
		accum(j,2)=0
L1220: !
	next j
return  ! /r
HDR_COLUMN_A: ! r:
	if ls=0 then goto HDR_COLUMN_XIT
	if ls=99 then goto HDR_COLUMN_B
	pr #255,using L1270: " "
L1270: form pos 1,c 1,skip ls
	goto HDR_COLUMN_XIT
HDR_COLUMN_B: !
	fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1340: rtrm$(foot$),"Page "&str$(pt1)
L1340: form skip sk,pos tabnote,c fl,pos 80,c 8,skip 1
	if eofcode=1 then goto HDR_COLUMN_XIT
	pr #255: newpage
	gosub HDR_REPORT
HDR_COLUMN_XIT: !
return  ! /r
PGOF: ! r:
	gosub HDR_COLUMN_B
	continue  ! /r
L1410: ! r:
	if ul=0 then goto L1500
	if ul=1 then goto L1470
	pr #255,using 'form pos 49,c 17,pos 67,c 19': "=================","==================="
	goto L1500
L1470: !
	pr #255: ''
	pr #255,using 'form pos 49,c 18,pos 67,c 19': "_________________","___________________"
L1500: !
return  ! /r
HDR_REPORT: ! r:
	heading=1
	if pt1=0 then pt1=1 else pt1=pt1+1
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs24 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(actpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	pr #255,using L1620: lpad$(rtrm$(fncch$),20),"Year To Date"
L1620: form pos 45,c 20,pos 73,c 12,skip 2
return  ! /r
 
L1650: ! r:
	eofcode=1
	gosub HDR_COLUMN_B
	fnfscode(actpd)
	fnpriorcd(1)
	fncloseprn
	goto Xit ! /r
 
BLDPCT1: ! r:
	open #10: "Name=[Temp]\Work."&session$&",KFName=[Temp]\Addr."&session$&",Replace,RecL=17,KPS=1,KLN=5",internal,outIn,keyed
	for j=1 to lrec(3)
		read #h_glmstr,using F_GLMSTR_A,rec=j: pc1,bb,cb noRec L1830
		k$=cnvrt$("N 5",pc1)
		read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1820
L1770: form pos 1,g 5,2*pd 6.2
		pc2=pc2+cb-bb
		yt2=yt2+cb
		rewrite #10,using L1770: pc1,pc2,yt2
		goto L1830
L1820: write #10,using L1770: pc1,cb-bb,cb
L1830: next j
	pc0=1
return  ! /r
 
BLDPCT2: ! r:
	pc3=pc4=0
	if val(k$)=0 then goto L1970
	read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1970
	if total=0 then goto L1940
	pc3=round(((total-pc2)/total)*100,0)
	if pc3<-999 or pc3>9999 then pc3=0
L1940: if total2=0 then goto L1970
	pc4=round(((total2-yt2)/total2)*100,0)
	if pc4<-999 or pc4>9999 then pc4=0
L1970: !
return  ! /r
 
Xit: fnXit
 
include: ertn
