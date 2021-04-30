! Replace S:\acsGL\acglChgS
! STATEMENT OF CHANGES IN FINANCIAL POSITION WITH COMPARRISON : _
	! FOR 8 1/2 * 11 PAPER
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,cogl$(3)*12,cap$*128,udf$*256
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*14
	dim cnam$*40,b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2)
	dim acct$*12,bp(13),fli$(2),flo$(2),by(13)
	dim p$(20)*50
 
	fnTop(program$,cap$="Change Amount")
	fncno(cno,cnam$)
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit
	udf$=env$('temp')&'\'
	open #20: "Name=[Q]\GLmstr\Company.h[cno],Shr",internal,input,relative: read #20,using 'Form Pos 152,3*C 12',rec=1: mat cogl$ : close #20:
	fscode=fnfscode
	pors=1
	mp1=75
	if fnps=2 then mp1=mp1+3
	fl1$="Name=[Q]\GLmstr\ACGLFNSF.h[cno],KFName=[Q]\GLmstr\agfsidx5.h[cno],Shr"
	if fnps=2 then fl1$="Name=[Q]\GLmstr\ACGLFNSG.h[cno],KFName=[Q]\GLmstr\agfsidx6.h[cno],Shr"
L240: form pos 1,c 12,pos 87,27*pd 6.2
	flo$(1)="8,5,C 50,N"
	flo$(2)="8,58,N 10.2,N"
	fli$(1)="8,5,C 50,UT,N"
	fli$(2)="8,58,N 10.2,UT,N"
	open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed
L300: read #1,using L240: acct$,cb,mat by,mat bp eof L360
	if acct$>cogl$(3) then goto L360
	if fscode=0 then income=income-cb else goto L340
	goto L350
L340: if fnpriorcd=2 then income-=bp(fscode) else income-=by(fscode)
L350: goto L300
L360: close #1:
	open #1: fl1$,internal,input,keyed
	if fnprocess=1 or fnUseDeptNo=0 then goto L480
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Cost Center or Department #:",mylen,right)
	fnTxt(1,mypos,3,0,right,"30",0,"Enter the cost center or department number if you wish to pr only one department, else leave blank for all.",0 ) : _
	resp$(1)=""
	fnLbl(2,1,"(Blank for all Departments)",mylen,right)
	fnCmdKey("&Next",1,1,0,"Prints the financial statement.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L480: on fkey 5 goto L1970 : _
	fnopenprn : _
	if file$(255)(1:4)<>"PRN:" then redir=1 else redir=0
 
	report$="Statement of Changes in Financial Position"
	if fnps=2 then goto L540 ! secondary
	execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 75 3 Replace DupKeys -N"
	goto L550
L540: execute "Index [Q]\GLmstr\GLmstr.h[cno] "&udf$&"fsindex.h[cno] 78 3 Replace DupKeys -N"
L550: open #3: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName="&udf$&"fsindex.h[cno],Shr",internal,input,keyed
L560: read #1,using L600: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof L1970
	if ltrm$(r$)="" or ltrm$(r$)="0" then goto L560
	if costcntr=0 then goto L600
	if costcntr><fc then goto L560
L600: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3
	if te$="S" or te$="F" then goto L630
	if heading=0 and te$><"R" then gosub L1760
L630: on pos ("RFHDTSP",te$,1) goto L1240,L1290,L640,L700,L1140,L1240,L2040 none L560
L640: pr #255,using L650: d$
L650: form pos sp,c 50
	gosub L1430
	gosub L1370
	goto L560
 
L700: if notrans=1 then goto L860
	if fr=val(r$) and val(r$)><0 then goto L800
	if fr>val(r$) then goto L800
L730: ! read amounts from gl master file
L740: read #3,using L760: fr,bb,cb,mat by,mat bp,pbp eof L850
	if fr=0 then goto L740
L760: form pos mp1,pd 3,pos 81,28*pd 6.2,pos 327,pd 6.2
	if fscode=0 then goto L800
	if fscode<1 or fscode>12 then fscode=1
	if fnpriorcd=2 then cb=bp(fscode) else cb=by(fscode)
L800: if fr=val(r$) then goto L810 else goto L830
L810: if fnpriorcd=2 then total+=(cb-pbp) else total+=(cb-bp(12))
	goto L730
L830: if fr<val(r$) then goto L730
	if fr>val(r$) then goto L860
L850: notrans=1
L860: fnTos(sn$="ACglchgs2") : _
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,"Description:",mylen,right)
	fnTxt(1,mypos,50,0,right,"",0,"Enter the description if not accurate.",0 ) : _
	resp$(1)=d$
	fnLbl(2,1,"Total Year to Date:",mylen,right)
	fnTxt(2,mypos,12,0,right,"10",0,"Enter the total for the year.",0 ) : _
	resp$(2)=str$(total)
	fnCmdKey("&Next",1,1,0,"Accept the answer.")
	fnCmdKey("&Cancel",5,0,1,"Returns to menu without posting.")
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	d$=resp$(1)
	total=val(resp$(2))
 
L980: for j=1 to 9
		accum(j,1)=accum(j,1)+total
	next j
	if rs=1 then total=-total else goto L1020
L1020: if ds=1 then dollar$="$" else dollar$=" "
	if total><0 then goto L1050
	if ls+ds+ul+ic>0 then goto L1050 else goto L560
L1050: sp2=67-sp-1
	pr #255,using L1070: d$(1:sp2),dollar$,total pageoflow L1600
L1070: form pos sp,c sp2,pos 67,c 1,pic(--,---,---.##),skip redir
	total=0
	gosub L1370
	gosub L1620
	gosub L1430
	goto L560
 
L1140: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	sp2=67-sp-1
	if ds=1 then dollar$="$" else dollar$=" "
	pr #255,using L1070: d$(1:sp2),dollar$,accum1 pageoflow L1600
	gosub L1370
	gosub L1620
	gosub L1430
	goto L560
 
L1240: if te$="R" then report$=d$
	if te$="S" then secondr$=d$
	gosub L1430
	goto L560
 
L1290: if foot1=1 then goto L1340
	tabnote=sp
	foot1=1
	foot$=d$
	goto L560
L1340: foot$=rtrm$(foot$)&d$
	goto L560
 
L1370: for j=1 to 9
		if ac(j)=0 then goto L1400
		accum(j,1)=0
L1400: next j
return
 
L1430: if ls=0 then goto L1580
	if ls=99 then goto L1490
	pr #255,using L1460: " "
L1460: form pos 1,c 1,skip ls
	goto L1580
 
L1490: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK=SK+1
	pr #255,using L1540: rtrm$(foot$),"Page "&str$(pt1)
L1540: form skip sk,pos tabnote,c fl,pos 75,c 8,skip 1
	if eofcode=1 then goto L1580
	pr #255: newpage
	gosub L1760
L1580: return
 
L1600: gosub L1490: continue
 
L1620: if ul=0 then goto L1720
	if ul=1 then goto L1690
	underlin$="=============="
	pr #255,using L1660: underlin$
L1660: form skip 1,pos 67,c 14,skip redir
	goto L1720
 
L1690: underlin$="______________"
	pr #255,using L1710: underlin$
L1710: form pos 67,c 14,skip redir
L1720: if redir=0 then pr #255,using L1730: " "
L1730: form skip 1,c 1,skip 0
return
 
L1760: heading=1
	pt1+=1
	pr #255: "\qc  {\f181 \fs24 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&trim$(report$)&"}"
	if trim$(secondr$)<>"" then pr #255: "\qc  {\f181 \fs18 \b "&trim$(secondr$)&"}"
	pr #255: "\qc  {\f181 \fs16 \b For the "&rtrm$(fnactpd$)&" month period ended "&rtrm$(fnpedat$)&"}"
	pr #255: "\ql "
	pr #255:
	on error goto L1920
	a=len(rtrm$(fnpedat$))
	b=val(rtrm$(fnpedat$(a-4:a)))
	pr #255,using L1880: b
L1880: form pos 72,pic(zzzz),skip 2
	on error system
	goto L1950
 
L1920: pr #255: tab(68);"CURRENT YEAR"
	on error system
	pr #255:
L1950: return
 
L1970: eofcode=1
	gosub L1490
 
 
	fncloseprn
	goto Xit
 
L2040: total=income
	goto L980
Xit: fnXit
 
include: ertn
