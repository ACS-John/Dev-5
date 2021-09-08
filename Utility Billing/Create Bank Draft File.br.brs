! formerly S:\acsUB\BkDraft
fn_setup
fnTop(program$)
 
 
	if ~fnclient_has('UB-EFT') then
		mat ml$(2)
		ml$(1)="You must purchase the ACS Utility Billing EFT"
		ml$(2)="module to access these features"
		fnmsgbox(mat ml$, response$, '',64)
		goto Xit
	end if
 
! r: Screen 1
	dim pth$*128
	fnureg_read('Bank Draft File',pth$,env$('Desktop')&'\bkdraft.dat')
	fnLastBillingDate(d1)
	
	fnTos(sn$="BKDraft")
	mylen=13
	mypos=mylen+2
	respc=0
	fnLbl(1,1,"Billing Date:",mylen,1)
	fnTxt(1,mypos,8,0,1,"1")
	resp$(respc+=1)=str$(d1)
	fnLbl(2,1,"Payment Date:",mylen,1)
	fnTxt(2,mypos,8,0,1,"1")
	resp$(respc+=1)=""
	fnLbl(3,1,"File:",mylen,1)
	fnTxt(3,mypos,30,128,0,"70",0,"Destination and file name.",0)
	resp$(respc_bankDraftFile:=respc+=1)=pth$
	fnChk(4,mypos,"Post Collections:",1)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	
	if ckey=5 then goto Xit
	d1=val(resp$(1))
	d2=val(resp$(2))
	pth$=trim$(resp$(respc_bankDraftFile))
	if resp$(4)="True" then postub=1
	
	fnureg_write('Bank Draft File',pth$)
	
goto initialization ! /r
initialization: ! r: initialization
	! open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],Shr",i,outi,r
	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed
	close #22: ioerr ignore
	open #hOut=fnH: "Name=[Temp]\BkDraft_Tmp_22.[Session],RecL=94,Replace",d,o
	if postub=1 then
		open #6: "Name=[Q]\UBmstr\Collections-"&env$('acsUserId')&".h[cno],RecL=91,Replace", internal,outIn,relative
	end if
	open #7: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\ubTrIndx.h[cno],Shr",internal,outIn,keyed
	open #hTrans2=fnH: "Name=[Q]\UBmstr\ubTransVB.h[cno],KFName=[Q]\UBmstr\UBTrdt.h[cno],Shr",internal,outIn,keyed
	fnopenprn
	gosub HdrP1
	gosub HDR1
goto READ_CUSTOMER ! /r
READ_CUSTOMER: ! r: main loop
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),alp$*7
	read #hCustomer,using Fcustomer: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,alp$,f$(2),f$(3),mat gb,df$,dr$,bc,da$ eof Finis
	Fcustomer: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 354,c 7,2*c 12,pos 388,10*pd 5.2,pos 1712,c 1,c 9,n 2,c 17
	if d1><f then goto READ_CUSTOMER
	if bal<=0 then goto READ_CUSTOMER
	if uprc$(df$)="Y" then gosub DETAIL1
goto READ_CUSTOMER ! /r
HDR1: ! r: ! FILE HEADER RECORD
	pcde=01 ! PRIORITY CODE
	! will need simular lines for anyone using this option; actually need to be moved to company information
	imd$=fnEftData$('Immediate Destination')
	imo$=fnEftData$('Immediate Origin') ! IMMEDIATE ORIGIN (routing number of the bank  which the city uses)
	fcd$=date$(1:2)&date$(4:5)&date$(7:8) ! FILE CREATION DATE
	if env$('client')="Depoe Bay" then fcd$=str$(d2)
	if len(fcd$)=5 then fcd$="0"&fcd$
	fct$=time$(1:2)&time$(4:5) ! FILE CREATION TIME
	fidm$="A" ! FILE ID MODIFIER
	rsz$="094" ! RECORD SIZE
	bf$="10" ! BLOCKING FACTOR
	dim ion$*23
	ion$=fnEftData$('Immediate Origin Name')  ! (23) IMMEDIATE ORIGIN NAME  (name of bank the city uses)
	fc$="1" ! FORMAT CODE
	dim idn$*23
	idn$="                       " ! (23) IMMEDIATE DESTINATION NAME
	rc$="0000000" ! REFERENCE CODE
	pr #hOut,using L690: 1,pcde,imd$,imo$,fcd$,fct$,fidm$,rsz$,bf$,fc$,idn$,ion$,rc$,"0"
	L690: form pos 1,g 1,pic(##),c 10,c 10,g 6,g 4,c 1,c 3,c 2,c 1,c 23,c 23,c 7,c 1
	! COMPANY/BATCH HEADER RECORD
	scc=225 ! SERVICE CLASS CODE
	cdd$="" ! COMPANY DISCRETIONARY DATA
	cid$=fnEftData$('Company Identification')
	ecc$=fnEftData$('standard entry class code')
	ced$="UTILITIES" ! COMPANY ENTRY DESCRIPTIVE
	eed$=date$(1:2)&date$(4:5)&date$(7:8) ! EFFEC)TIVE ENTRY DATE
	osc$="1" ! ORIGINATOR STATUS CODE
	odi$=fnEftData$('originating dfi identification') ! ORIGINATING DFI IDENTIFICATION  (bank's account)
	bn=1 !  BN=BATCH NUMBER
	if env$('client')="Depoe Bay" then
		pr #hOut,using L850: 5,scc,env$('cnam')(1:16),cdd$,cid$,ecc$,ced$,fcd$    ,fcd$,"",osc$,odi$,bn
	else
		pr #hOut,using L850: 5,scc,env$('cnam')(1:16),cdd$,cid$,ecc$,ced$,fncd(d2),eed$,"",osc$,odi$,bn
	end if
	L850: form pos 1,g 1,pic(###),c 16,c 20,c 10,c 3,c 10,pic(######),g 6,g 3,g 1,c 8,pic(#######)
return  ! /r
DETAIL1: ! r:
	pr #255,using L900: z$,e$(2),d2,bal pageoflow NEWPGE
	L900: form pos 1,c 12,c 32,pic(zz/zz/zz),n 13.2,skip 1
	t1=t1+bal
	tc=bc ! Transaction Type
	! l1=LEN(RTRM$(BA$))
	! cD=VAL(BA$(L1:L1)) ! CHECK DIGIT
	ari=0 ! ADDENDA RECORD INDICATOR
	tn1=tn1+1
	tn$=br$&cnvrt$("PIC(#######)",tn1) ! TRACE NUMBER
	pr #hOut,using L1000: 6,tc,dr$,da$,bal*100,z$,e$(2)(1:22),"",ari,odi$&tn$
	if postub=1 then gosub WritePostingEntry
	L1000: form pos 1,g 1,g 2,c 9,c 17,pic(##########),c 15,c 22,g 2,n 1,c 15
	td1=td1+bal
	if env$('client')<>"Billings" then tc1=tc1+bal
	eh=eh+val(dr$(1:8)) ! ENTRY HASH
return  ! /r
WritePostingEntry: ! r:
	mat alloc(sz1)
	x=0
	for j=1 to 10
		if order(j)=1 then alloc(x+=1)=gb(j)
	next j
	m=bal : n=d2 : o(1)=3
	write #6,using "Form POS 1,C 10,PD 4.2,PD 4,2*N 1,POS 24,C 9,SZ1*PD 4.2,5*PD 3,PD 4.2": z$,m,n,mat o,rcpt$,mat alloc,mat bd2
return ! /r
 
CTRL1: ! r: COMPANY/BATCH CONTROL RECORD
	scc=225 ! SERVICE CLASS CODE
	eac=tn1 ! ENTRY ADDENDA COUNT
	n=d2: m=bal: o(1)=3: rcpt$= "BkDraft"
	eh$=str$(eh): x=len(eh$): eh=val(eh$(max(1,x-9):x))
	! TD1=TOTAL DEBIT AMOUNT
	! TC1=TOTAL CREDIT AMOUNT
	! CID$=COMPANY IDENTIFICATION
	tn1=tn1+1 : tn$=br$&cnvrt$("PIC(#######)",tn1) ! TRACE NUMBER
	if env$('client')="Billings" then
		tn1-=1
	else
		pr #hOut,using L1126: 6,22,imo$(2:9),imo$(10:10),trim$(odi$),td1*100,"",env$('cnam')(1:22),"",0,odi$&tn$ ! putting total deposit in city's bank account ! Billings does not offset the withdrawls with a deposit.  The banker does that manually.
		L1126: form pos 1,g 1,g 2,c 8,c 1,c 17,pic(##########),c 15,c 22,g 2,n 1,c 15
	end if
	pr #hOut,using L1140: 8,scc,eac,eh,td1*100,tc1*100,cid$,mac$,"",odi$,bn
	L1140: form pos 1,g 1,pic(###),pic(######),pic(##########),2*pic(############),c 10,c 19,c 6,c 8,pic(#######)
	! FILE CONTROL RECORD
	bactr=1 ! BATCH COUNT
	tn2=tn1+4 ! total # records (all 6 records plus the 1&5 plus 8&9)
	if fp(tn2/10)>0 then blctr=int(tn2/10)+1: bkfactor=blctr*10-tn2 ! block counter and block factor
	if fp(tn2/10)=0 then blctr=int(tn2/10): bkfactor=0
	eac=tn1 ! entry/adgenda count (number of 6 records)
	! EH=ENTRY HASH
	pr #hOut,using L1230: 9,bactr,blctr,eac,eh,td1*100,tc1*100,"                                      "," "
	L1230: form pos 1,g 1,2*pic(######),pic(########),pic(##########),2*pic(############),c 38,c 1
	if bkfactor=0 then goto L1280
	for j=1 to bkfactor
		pr #hOut,using "Form POS 1,C 94": "9999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999999"
	next j
	L1280: !
return  ! /r
Finis: ! r:
	gosub CTRL1
	pr #255: "                                                    {\ul            }"
	pr #255,using L900: "  Total","",0,t1
	fncloseprn
	close #hOut:
	
	! r:
	open #hTmpIn=fnH: "Name=[Temp]\BkDraft_Tmp_22.[Session],RecL=94",display,input
	open #hTmpOut=fnH: "Name=[Temp]\BkDraft_Tmp_24.[Session],RecL=96,EOL=None,Replace",external,output
	dim a$*94
	do
		linput #hTmpIn: a$ eof L1590
		if a$(94:94)="X" then a$(94:94)=""
		write #hTmpOut,using "Form POS 1,C 94,C 1,c 1": rpad$(a$,94),chr$(13),chr$(10)
	loop
	L1590: !
	close #hTmpOut:
	close #hTmpIn:
	COPY_TO_DESTINATION: !
	if ~fnRename(env$('temp')&"\BkDraft_Tmp_24."&session$,pth$) then
		mat ml$(3)
		ml$(1)="Unable to create file: "
		ml$(2)=pth$
		ml$(3)='Select OK to retry.'
		fnmsgbox(mat ml$,resp$,'',65)
		if resp$="OK" then goto COPY_TO_DESTINATION
		goto Xit
	end if
	! /r
	
	
if postub=1 then
	mat ml$(3)
	ml$(1)="You have indicated you want to post the drafts"
	ml$(2)="as collections to each customers account."
	ml$(3)="Click OK to continue else Cancel to skip posting."
	fnmsgbox(mat ml$,resp$,'',1)
	if resp$="OK" then gosub Merge
end if
goto Xit ! /r
Merge: ! r:
	r6=0
	do
		L1860: !
		r6+=1
		if r6>lrec(6) then goto L2120 ! prevent stopping to deleted record and quit when finished
		dim bd2(5)
		dim o(2)
		read #6,using L1890,rec=r6: z$,m,n,mat o,rcpt$,mat alloc,mat bd2 noRec L1860
		L1890: form pos 1,c 10,pd 4.2,pd 4,2*n 1,pos 24,c 9,sz1*pd 4.2,5*pd 3,pd 4.2
		if p$(1:2)="  " and m=0 then goto L1860
		read #hCustomer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=z$: bal,mat gb nokey L1860
		if o(1)=3 then tcode=3 ! collection
		if o(1)=4 then tcode=4 ! credit memo
		if o(1)=5 then tcode=5 ! debit memo
		if o(1)=5 then bal+=m else bal-=m
		tmp=fndate_mmddyy_to_ccyymmdd(n)
		mat tg=(0): x=0
		for j=1 to 10
			if trim$(srvname$(j))<>"" then tg(j)=alloc(x+=1)
		next j
		write #7,using 'Form POS 1,C 10,N 8,N 1,12*PD 4.2,6*PD 5,PD 4.2,N 1': z$,tmp,tcode,m,mat tg,0,0,0,0,0,0,bal,pcode
		j2=0
		for j=1 to 10
			if trim$(srvname$(j))<>'' then
				j2=j2+1
				if o(1)=5 then
					gb(j)=gb(j)+alloc(j2)
				else
					gb(j)=gb(j)-alloc(j2)
				end if
			end if
		next j
		rewrite #hCustomer,using 'Form POS 292,PD 4.2,POS 388,10*PD 5.2,pos 1859,pd 5.2',key=z$: bal,mat gb
		o(2)=9
		rewrite #6,using "Form POS 19,2*N 1",rec=r6: mat o
	loop
	L2120: !
	close #6,free:
return  ! /r
Xit: fnXit
NEWPGE: pr #255: newpage : gosub HdrP1: continue
HdrP1: ! r:
	pr #255: "\qc  {\f181 \fs18 \b "&env$('cnam')&"}"
	pr #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
	pr #255: "\qc  {\f181 \fs22 \b "&date$("Month DD, CCYY")&"}"
	pr #255: "{\ul Account No}  {\ul Customer Name                 }  {\ul Pay Date}  {\ul     Amount}"
return  ! /r
 
 
def fn_setup
	if ~setup then
		setup=1
		autoLibrary
		dim ml$(0)*128
		dim alloc(10)
		dim resp$(5)*128
		
		dim srvname$(10)*20
		fnGetServices(mat srvname$)
		for j=1 to 10
			if trim$(srvname$(j))>"" then order(j)=1 : sz1+=1
		next j
		on error goto Ertn
	end if
fnend
 
include: ertn
