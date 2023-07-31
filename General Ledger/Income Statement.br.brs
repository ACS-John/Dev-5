! formerly S:\acsGL\acglinc
! -- pr Income Statement  for 8 1/2 * 11 paper without percentages
autoLibrary
on error goto Ertn

fnTop(program$)
actpd$=fnactpd$
actpd=fnactpd
! fscode=fnfscode
! priorcd=fnpriorcd
if fnGlAskFormatPriorCdPeriod=5 then goto Xit ! sets fnPs,fnpriorcd,fnfscode (primary/secondary,current year/Prior,period to print)
fscode=fnfscode
priorcd=fnpriorcd
hFinStmtDesign=fnOpenFsdAcglfnsIJ(mp1,mp2)
if fnProcess=1 or fnUseDeptNo=0 then goto DoReport
fnTos
mylen=33: mypos=mylen+3 : right=1
fnLbl(1,1,'Cost Center or Department Number:',mylen,right)
fnTxt(1,mypos,3,0,right,'30',0,'Enter the cost center or department number if you wish to print only one department, else leave blank for all.',0 )
resp$(1)=''
fnLbl(2,1,'(Blank for all)',mylen,right)
fnCmdKey('&Next',1,1,0,'Prints the financial statement.')
fnCmdKey('&Cancel',5,0,1,'Returns to menu without posting.')
ckey=fnAcs(mat resp$)
if ckey=5 then goto Xit
costcntr=val(resp$(1))

DoReport: ! r:
fnOpenPrn
fnFsIndexIncStmt
open #hAccount=fnH: 'Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Temp]\fsindex.h[cno],Shr',i,i,k

dim report$*50
report$='Statement of Income and Expenses'
! gosub BLDPCT1 ! build % based on ref # in primary fund # in g/l account
ReadFinStmtDesign: !
	dim r$*5  ! Financial Statement Number
	dim d$*50 ! Description
	dim te$*1 ! Type of Entry
	dim ac(9) ! clear accumulator flags
	read #hFinStmtDesign,using 'form pos 1,c 5,c 50,c 1,2*n 2,5*n 1,9*n 1,n 1,n 3': r$,d$,te$,sp,ls,ds,ul,rs,bc,ap,mat ac,ic,fc eof EoFinStmtDesign
	! if trim$(d$(1:sp2))='INCOME TAX' then debug_this=1 else debug_this=0 ! pr #255: 'just read the d$' : pause
	if ltrm$(r$)='' or ltrm$(r$)='0' then goto ReadFinStmtDesign
	if ~fc and te$='F' then goto L460
	if costcntr and costcntr><fc then goto ReadFinStmtDesign
	L460: !
	if ~heading and te$><'R' and te$<>'S' and te$<>'F' then heading=fn_prHeader(pt1,report$,secondr$,actpd$)
	if te$='R' or te$='S' then 
		gosub TypeRS
	else if te$='D' then
		gosub TypeD
	else if te$='F' then
		gosub TypeF
	else if te$='H' then
		gosub TypeH
	else if te$='T' then
		gosub TypeT
	end if
goto ReadFinStmtDesign

! /r
EoFinStmtDesign: ! r:
	eofcode=1
	fn_prFooter(pt1,heading,eofcode,report$,secondr$,actpd$)
	fnfscode(actpd)
	fnpriorcd(1)
	close #hFinStmtDesign:
	fnClosePrn
goto Xit ! /r

TypeH: ! r: Header (d$,sp,...)
	pr #255,using L500: d$(1:40)
	L500: form pos sp,c 40,skip 1
	fn_prBlankLines(ls,pt1,heading,eofcode,report$,secondr$,actpd$)
	fn_resetAccumArray(mat ac,mat accum)
return ! /r
TypeD: ! r: Detail (hAccount,mp1,mp2,...
	! pr '_________________ top of TypeD _________________';ir,trim$(r$)
	if notrans=1 then goto TdAfterTotals
	if ir=val(r$) and val(r$)><0 then goto TdAddIntoTotalsEtc
	if ir>val(r$) then goto TdAddIntoTotalsEtc
	do
		do ! read gl master file for amounts
			dim by(13)
			dim bp(13)
			read #hAccount,using FaccountB: gl_number$,ir,pcr,bb,cb,mat by,mat bp eof EoAccount
			FaccountB: form pos 1,c 12,pos mp1,pd 3,pos mp2,pd 3,pos 81,41*pd 6.2
			! if trim$(d$)='TRANSFERS IN' then debug_this=1 else debug_this=0
			! if debug_this then pr 'read '&gl_number$
		loop while ~ir ! skip any gl accounts not pointed to ic
		if ~fscode or (fscode=actpd and priorcd=1) then 
			goto TdAddIntoTotalsEtc
		else
			if fscode<1 or fscode>13 then fscode=1
			if priorcd=1 then cb=by(fscode) else cb=bp(fscode)
			if priorcd=2 then 
				if fscode>1 then bb=bp(fscode-1) else bb=0
			else
				if fscode>1 then bb=by(fscode-1) else bb=0
			end if
		end if
		
		TdAddIntoTotalsEtc: !
		if ir=val(r$) then
			!  if debug_this then pr gl_number$;'  total1(';total1;')+=(cb(';cb;')-bb(';bb;'))'
			total1+=(cb-bb)
			!  if debug_this then pr gl_number$;'  so now total1=';total1: pause
			total2+=cb
			! if debug_this then pr 'total1 now '&str$(total1)&'  total2 now '&str$(total2) : pause
			! not used   7/28/2023 removed    	k$=cnvrt$('N 5',pcr)
		else if ir>val(r$) then 
			goto TdAfterTotals
		end if
	loop

	EoAccount: !
		notrans=1
	goto TdAfterTotals

	TdAfterTotals: !
	for j=1 to 9
		if ac(j)<>9 then
			dim accum(9,2)
			accum(j,1)+=total1
			accum(j,2)+=total2
		end if
	next j
	if rs=1 then
		total1=-total1
		total2=-total2
	end if
	if ds=1 then 	dollar$='$' else dollar$=' '
	if total1 or total2 or ls+ul+ds+ic>0 then 
		sp2=49-sp-1
		! If DS=1 Then dOLLAR$='': form$='form pos SP,C SP2,pos 50,C 1,C 5,PIC($$$$,$$$,$$$.##),C 1,pos 74,C 1,C 5,PIC($$$$$$,$$$,$$$.##),C 1,skip 1' Else form$='form pos SP,C SP2,pos 50,C 1,C 5,PIC(----,---,---.##),C 1,pos 74,C 1,C 5,PIC(------,---,---.##),C 1,skip 1'
		! if debug_this then pr #255: '***'
		
		! if debug_this then pr rtrm$(d$),gl_number$,total1,total2 : pause
		
		if ul=1 then
			pr #255,using L856: d$(1:sp2),dollar$,'{\ul ',total1,'}',dollar$,'{\ul ',total2,'}' pageoflow PgOf
			L856: form pos sp,c sp2,pos 50,c 1,c 5,pic(----,---,---.##),c 1,pos 74,c 1,c 5,pic(------,---,---.##),c 1,skip 1
		else
			pr #255,using L870: d$(1:sp2),dollar$,total1,dollar$,total2 pageoflow PgOf
			L870: form pos sp,c sp2,pos 49,c 1,pic(-----,---,---.##),pos 67,c 1,pic(-------,---,---.##),skip 1
		end if
		! if debug_this then pr #255: '***'
		! not used     removed 7/28/2023       if pc0=1 then gosub BldPct2
		! not used                             if pc3>0 or pc4>0 then 
		! not used                             	pr #255,using Fpc34: pc3,pc4
		! not used                             	Fpc34: form pos 63,n 4,pos 82,n 4,skip 1
		! not used                             end if
		total1=total2=0
		fn_resetAccumArray(mat ac,mat accum)
		if ul<>1 then fn_prUnderline(ul)
		fn_prBlankLines(ls,pt1,heading,eofcode,report$,secondr$,actpd$)
	end if
	! pr '________________________________________________'
return ! /r

TypeT: ! r: Total
	if ap=0 then ap=1
	if rs=1 then accum1=-accum(ap,1) else accum1=accum(ap,1)
	if rs=1 then accum2=-accum(ap,2) else accum2=accum(ap,2)
	if ds=1 then dollar$='$' else dollar$=' '
	sp2=49-sp-1
	if ds=1 then
		dollar$=''
		dim form$*200
		form$='form pos SP,C SP2,pos 50,C 1,C 5,PIC($---,---,---.##),C 1,pos 74,C 1,C 5,PIC($-----,---,---.##),C 1,skip 1'
	else
		form$='form pos SP,C SP2,pos 50,C 1,C 5,PIC(----,---,---.##),C 1,pos 74,C 1,C 5,PIC(------,---,---.##),C 1,skip 1'
	end if
	! pr some sub total like thingies
	if ul=1 then
		pr #255,using L856: d$(1:sp2),dollar$,'{\ul ',accum1,'}',dollar$,'{\ul ',accum2,'}' pageoflow PgOf
	else
		pr #255,using L870: d$(1:sp2),dollar$,accum1,dollar$,accum2 pageoflow PgOf
	end if
	fn_resetAccumArray(mat ac,mat accum)
	if ul=1 then goto L1050
	fn_prUnderline(ul)
	L1050: !
	fn_prBlankLines(ls,pt1,heading,eofcode,report$,secondr$,actpd$)
return ! /r
TypeRS: ! r: Report Heading and SubHeading  (te$,&report$,&secondr$*50,ls,&pt1,eofcode,actpd$)
	if te$='R' then report$=d$
	dim secondr$*50
	if te$='S' then secondr$=d$
	fn_prBlankLines(ls,pt1,heading,eofcode,report$,secondr$,actpd$)
return ! /r
TypeF: ! r: Footnote
	if foot1=1 then 
		dim foot$*132
		foot$=rtrm$(foot$)&d$
	else
		tabnote=sp
		foot1=1
		foot$=d$
	end if
return ! /r

def fn_resetAccumArray(mat ac,mat accum; ___,j)
	for j=1 to 9
		if ac(j)<>0 and ac(j)<>9 then
			accum(j,1)=0
			accum(j,2)=0
		end if
	next j
fnend
def fn_prBlankLines(ls,&pt1,&heading,eofcode,report$*50,secondr$*50,actpd$)
	if ls=99 then 
		fn_prFooter(pt1,heading,eofcode,report$,secondr$,actpd$)
	else if ls then
		pr #255,using FskipLs: ' '
		FskipLs: form pos 1,c 1,skip ls
	end if
fnend
def fn_prFooter(&pt1,&heading,eofcode,report$*50,secondr$*50,actpd$; ___,pglen,fl,sk)
	fnPgLen(pglen)
	sk=pglen-krec(255)
	! If PGLEN<>42 Then pGLEN=58
	! If PGLEN=42 Then sK=SK+1
	fl=len(rtrm$(foot$))
	pr #255,using L1340: rtrm$(foot$),'Page '&str$(pt1)
	L1340: form skip sk,pos tabnote,c fl,pos 80,c 8,skip 1
	if eofcode<>1 then
		pr #255: newpage
		heading=fn_prHeader(pt1,report$,secondr$,actpd$)
	end if
fnend
PgOf: ! r:
	fn_prFooter(pt1,heading,eofcode,report$,secondr$,actpd$)
continue  ! /r
def fn_prUnderline(ul)
	if ul=1 then
		pr #255: ''
		pr #255,using 'form pos 49,c 18,pos 67,c 19': '_________________','___________________'
	else if ul then
		pr #255,using 'form pos 49,c 17,pos 67,c 19': '=================','==================='
	end if
fnend

def fn_prHeader(&pageNumber,report$*50,secondr$*50,actpd$)
	if pageNumber=0 then pageNumber=1 else pageNumber+=1
	pr #255: '\qc  {\f181 \fs18 \b '&env$('cnam')&'}'
	pr #255: '\qc  {\f181 \fs24 \b '&trim$(report$)&'}'
	if trim$(secondr$)<>'' then 
		pr #255: '\qc  {\f181 \fs24 \b '&trim$(secondr$)&'}'
	end if
	pr #255: '\qc  {\f181 \fs16 \b For the '&rtrm$(actpd$)&' month period ended '&rtrm$(fnpedat$)&'}'
	pr #255: '\ql '
	pr #255:
	pr #255,using L1620: lpad$(rtrm$(fncch$),20),'Year To Date'
	L1620: form pos 45,c 20,pos 73,c 12,skip 2
	fn_prHeader=1 ! heading=1
fnend ! return 

! not used   7/13/2023 removed    BLDPCT1: ! r:
! not used   7/13/2023 removed    	open #10: 'Name=[Temp]\Work.[Session],KFName=[Temp]\Addr.[Session],Replace,RecL=17,KPS=1,KLN=5',i,outIn,k
! not used   7/13/2023 removed    	L1770: form pos 1,g 5,2*pd 6.2
! not used   7/13/2023 removed    	for j=1 to lrec(hAccount)
! not used   7/13/2023 removed    		read #hAccount,using 'form pos mp1,pd 3,pos 81,2*pd 6.2',rec=j: pc1,bb,cb noRec L1830
! not used   7/13/2023 removed    		k$=cnvrt$('N 5',pc1)
! not used   7/13/2023 removed    		read #10,using L1770,key=k$: pc1,pc2,yt2 nokey L1820
! not used   7/13/2023 removed    		pc2=pc2+cb-bb
! not used   7/13/2023 removed    		yt2=yt2+cb
! not used   7/13/2023 removed    		rewrite #10,using L1770: pc1,pc2,yt2
! not used   7/13/2023 removed    		goto L1830
! not used   7/13/2023 removed    		L1820: !
! not used   7/13/2023 removed    			write #10,using L1770: pc1,cb-bb,cb
! not used   7/13/2023 removed    		L1830: !
! not used   7/13/2023 removed    	next j
! not used   7/13/2023 removed    	pc0=1
! not used   7/13/2023 removed    return  ! /r
! not used   7/28/2023 removed    BldPct2: ! r: (total1,total2,#10)
! not used   7/28/2023 removed    	pc3=pc4=0
! not used   7/28/2023 removed    	if val(k$)=0 then goto L1970
! not used   7/28/2023 removed    	read #10,using 'form pos 1,g 5,2*pd 6.2',key=k$: pc1,pc2,yt2 nokey L1970
! not used   7/28/2023 removed    	if total1=0 then goto L1940
! not used   7/28/2023 removed    	pc3=round(((total1-pc2)/total1)*100,0)
! not used   7/28/2023 removed    	if pc3<-999 or pc3>9999 then pc3=0
! not used   7/28/2023 removed    	L1940: !
! not used   7/28/2023 removed    	if total2=0 then goto L1970
! not used   7/28/2023 removed    	pc4=round(((total2-yt2)/total2)*100,0)
! not used   7/28/2023 removed    	if pc4<-999 or pc4>9999 then pc4=0
! not used   7/28/2023 removed    	L1970: !
! not used   7/28/2023 removed    return  ! /r

Xit: fnXit

include: ertn
