! formerly S:\acsGL\ACGLIncP
! -- INCOME STATEMENT FOR 8 1/2 * 11 PAPER WITH PERCENTAGES
 
	autoLibrary
	on error goto Ertn
 
	dim fl1$*256,tp1(4),by(13)
	dim b$*3,a$(8)*30,oldtrans$*16,g(8),accum(9,2),bp(13)
	dim r$*5,d$*50,te$*1,ac(9),report$*50,secondr$*50,foot$*132,underlin$*25
 
	fnTop(program$)
	actpd=fnactpd
	actpd$=fnactpd$
	fscode=fnfscode
	priorcd=fnpriorcd
	if fnGlAskFormatPriorCdPeriod=5 then goto Xit 	! sets fnps,priorcd,fnfscode (primary/secondary,current year/Prior,period to print)
	priorcd=fnpriorcd
	fscode=fnfscode
	if fnps=2 then mp1=72 else mp1=69
	if fnps=2 then fl1$='Name=[Q]\GLmstr\ACGLFNSJ.h[cno],KFName=[Q]\GLmstr\agfsidx2.h[cno],Shr' else : _
		fl1$='Name=[Q]\GLmstr\ACGLFNSI.h[cno],KFName=[Q]\GLmstr\agfsidx3.h[cno],Shr'
L190: form pos mp1,pd 3,pos 81,41*pd 6.2
	pas=1 : open #hwork:=4: 'Name=[Temp]\Work.[Session],KFName=IDX.'&wsid$&',Replace,RecL=33,KPS=1,KLN=5',i,outIn,k
L210: acglfnsi=1 : _
	open #acglfnsi: fl1$,i,outIn,k
	if fnprocess=1 or fnUseDeptNo=0 then goto L330
	if percent=1 then goto L330
	fnTos
	mylen=30: mypos=mylen+3 : right=1
	fnLbl(1,1,'Cost Center or Department #:',mylen,right)
	fnTxt(1,mypos,3,0,right,'30',0,'Enter the cost center or department number if you wish to pr only one department, else leave blank for all.',0 ) : _
	resp$(1)=''
	fnLbl(2,1,'(Blank for all Departments)',mylen,right)
	fnCmdKey('&Next',1,1,0,'Prints the financial statement.')
	fnCmdKey('&Cancel',5,0,1,'Returns to menu without posting.')
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	costcntr=val(resp$(1))
L330: on fkey 5 goto L1910
	fnopenprn : _
	if file$(255)(1:4)<>'PRN:' then redir=1 else redir=0
	if fnps=2 then goto L380 ! secondary
	execute 'Index [Q]\GLmstr\GLmstr.h[cno] [temp]\fsindex.h[cno] 69 3 Replace DupKeys -N'
	goto L390
L380: execute 'Index [Q]\GLmstr\GLmstr.h[cno] [temp]\fsindex.h[cno] 72 3 Replace DupKeys -N'
L390: open #3: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[temp]\fsindex.h[cno],Shr',i,i,k
	report$='Statement of Income and Expenses'
READ_ACGLFNSI: !
L420: read #acglfnsi,using L470: r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc,rnp eof L1880,conv L2060
	if ltrm$(r$)='' or ltrm$(r$)='0' then goto READ_ACGLFNSI
	if costcntr=0 then goto L470
	if fc=0 and te$='F' then goto L480 ! 5/08/89
	if costcntr><fc then goto READ_ACGLFNSI
L470: form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3,n 5
L480: if te$='S' or te$='F' then goto L500
	if heading=0 and te$><'R' then gosub L1730
L500: on pos ('RFHDTS',te$,1) goto L1250,L1300,L510,L570,L1040,L1250 none READ_ACGLFNSI
L510: if percent=0 then goto L550
	pr #255,using L530: d$(1:40)
L530: form pos sp,c 40,skip 1
	gosub L1420
L550: gosub L1370
	goto READ_ACGLFNSI
L570: if notrans=1 then goto L750
	if ir>=val(r$) and val(r$)><0 then goto L690
L590: ! read balances from gl master file
L600: read #3,using L190: ir,bb,cb,mat by,mat bp eof L740
	if ir=0 then goto L600
	if fscode=0 or fscode=actpd then goto L690
	if fscode<1 or fscode>13 then fscode=1
	if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
	if priorcd=2 then goto L680
	if fscode>1 then bb=by(fscode-1) else bb=0
	goto L690
L680: if fscode>1 then bb=bp(fscode-1) else bb=0
L690: if ir=val(r$) then total=total+(cb-bb) else goto L720
	total2=total2+cb
	goto L590
L720: if ir<val(r$) then goto L590
	if ir>val(r$) then goto L750
L740: notrans=1
L750: for j=1 to 9
		if ac(j)=9 then goto L790 ! 10/14/87
		accum(j,1)=accum(j,1)+total
		accum(j,2)=accum(j,2)+total2
L790: next j
	if rs=1 then total=-total else goto L820
	total2=-total2
L820: if ds=1 then dollar$='$' else dollar$=' '
	if ds=1 then percent$='%' else percent$=' '
	if total><0 or total2><0 then goto L860
	if ls+ds+ul>0 then goto L860 else goto L420
L860: if percent=0 then goto L970
	if pas=2 then gosub PAS2
	sp2=31-sp-1
	if percent1><0 then monthpct=total/percent1*100 else monthpct=0
	if percent2><0 then ytdpct=total2/percent2*100 else ytdpct=0
	if monthpct<-999.99 then monthpct=-999.99
	if monthpct>999.99 then monthpct=999.99
	if ytdpct<-999.99 then ytdpct=-999.99
	if ytdpct>999.99 then ytdpct=999.99
	if ul=1 then pr #255,using L961: d$(1:sp2),dollar$,'{\UL ',total,'}',monthpct,percent$,dollar$,'{\UL ',total2,'}',ytdpct,percent$ pageoflow L1590 : goto L960
	pr #255,using L960: d$(1:sp2),dollar$,total,monthpct,percent$,dollar$,total2,ytdpct,percent$ pageoflow L1590
L960: form pos sp,c sp2,pos 31,c 1,pic(-----,---,---.##),pic(-----.##),c 2,x 3,c 1,pic(-----,---,---.##),pic(-----.##),c 1,skip redir
L961: form pos sp,c sp2,pos 31,c 1,c 5,pic(-----,---,---.##),c 1,pic(-----.##),c 2,x 3,c 1,c 5,pic(-----,---,---.##),c 1,pic(-----.##),c 1,skip redir
L970: if pas=1 then tp1=total : tp2=total2 : gosub PAS1
	total=0
	total2=0
	gosub L1370
	if ul=1 then goto L1020
	gosub L1600
L1020: gosub L1420
	goto READ_ACGLFNSI
L1040: if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$='$' else dollar$=' '
	if ds=1 then percent$='%' else percent$=' '
	if percent=0 then goto L1180
	if percent1><0 then monthpct=accum1/percent1*100 else monthpct=0
	if percent2><0 then ytdpct=accum2/percent2*100 else ytdpct=0
	if monthpct<-999.99 then monthpct=-999.99
	if monthpct>999.99 then monthpct=999.99
	if ytdpct<-999.99 then ytdpct=-999.99
	if ytdpct>999.99 then ytdpct=999.99
	sp2=31-sp-1
	if ul=1 then pr #255,using L961: d$(1:sp2),dollar$,'{\UL ',accum1,'}',monthpct,percent$,dollar$,'{\UL ',accum2,'}',ytdpct,percent$ pageoflow L1590 : goto L1180
	pr #255,using L960: d$(1:sp2),dollar$,accum1,monthpct,percent$,dollar$,accum2,ytdpct,percent$ pageoflow L1590
L1180: if pas=1 then tp1=accum1 : tp2=accum2 : gosub PAS1
	gosub L1370
	if pas=2 then gosub PAS2
	if ul=1 then goto L1220
	gosub L1600
L1220: gosub L1420
	goto READ_ACGLFNSI
 
L1250: if te$='R' then report$=d$
	if te$='S' then secondr$=d$
	gosub L1420
	goto READ_ACGLFNSI
 
L1300: if foot1=1 then goto L1340
	tabnote=sp : foot1=1 : foot$=d$
	goto READ_ACGLFNSI
 
L1340: foot$=rtrm$(foot$)&d$
	goto READ_ACGLFNSI
 
L1370: ! r:
	for j=1 to 9
		if ac(j)=0 or ac(j)=9 then
			goto L1390
		else
			accum(j,1)=0 : accum(j,2)=0
		end if
L1390: !
		next j
return ! /r
 
L1420: !
	if percent=0 then goto L1570
	if ls=0 then goto L1570
	if ls=99 then goto L1480
	pr #255,using L1460: ' '
L1460: form pos 1,c 1,skip ls
	goto L1570
L1480: fnpglen(pglen)
! If PGLEN<>42 Then pGLEN=58
	sk=pglen-krec(255): fl=len(rtrm$(foot$))
! If PGLEN=42 Then sK+=1
	pr #255,using L1530: rtrm$(foot$),'Page '&str$(pt1)
L1530: form skip sk,pos tabnote,c fl,pos 80,c 8
	if eofcode=1 then goto L1570
	pr #255: newpage
	gosub L1730
L1570: !
return
 
L1590: gosub L1480: continue
L1600: if percent=0 then goto L1710
	if ul=0 then goto L1700
	if ul=1 then goto L1670
	underlin$='================= ======='
	pr #255,using L1650: underlin$,underlin$
L1650: form pos 31,c 25,pos 61,c 25,skip redir
	goto L1700
L1670: underlin$='_________________ _______'
	pr #255,using L1690: underlin$,underlin$
L1690: form skip redir,pos 31,c 25,pos 61,c 25,skip redir
L1700: if redir=0 then pr #255: ''
L1710: return
 
L1730: heading=1
	pt1+=1
	pr #255: '\qc  {\f181 \fs24 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f181 \fs24 \b '&trim$(report$)&'}'
	if trim$(secondr$)<>'' then pr #255: '\qc  {\f181 \fs18 \b '&trim$(secondr$)&'}'
	pr #255: '\qc  {\f181 \fs16 \b For the '&rtrm$(fnactpd$)&' month period ended '&rtrm$(fnpedat$)&'}'
	pr #255: '\ql '
	pr #255:
	pr #255,using L1840: fncch$,'       Year To Date'
	pr #255,using L1830: '_________________________','_________________________'
L1830: form pos 31,c 25,pos 61,c 25,skip redir
L1840: form pos 38,c 20,pos 61,c 25,skip redir
	pr #255:
return
 
L1880: if pas=2 then goto L1910
	pas=2 : percent=1 : percent1=percent2=0
	goto L1970
L1910: eofcode=1
	gosub L1480
 
	fncloseprn
	fnfscode(actpd)
	fnpriorcd(1)
	goto Xit
 
L1970: close #acglfnsi:
	close #3:
	total=total2=0
	mat accum=(0)
	foot1=0
	foot$=' '
	ir=notrans=0
	goto L210
 
L2060: read #acglfnsi,using 'form pos 1,C 5': r$
	delete #acglfnsi:
	goto L420
 
PAS1: if rnp=0 then goto L2170
	read #hwork,using L2120,key=r$: k4$,mat tp1 nokey L2160
L2120: form pos 1,g 5,4*pd 7.2
	tp1(1)=tp1 : tp1(2)=tp2
	rewrite #hwork,using L2120: k4$,mat tp1
	goto L2170
L2160: write #hwork,using L2120: r$,tp1,tp2,0,0
L2170: return
 
PAS2: mat tp1=(0)
	if rnp=0 then goto L2250
	k4$=lpad$(str$(rnp),5)
	read #hwork,using L2120,key=k4$: k4$,mat tp1 nokey L2230
L2230: percent1=tp1(1)
	percent2=tp1(2)
L2250: return
 
Xit: fnXit
 
include: ertn
