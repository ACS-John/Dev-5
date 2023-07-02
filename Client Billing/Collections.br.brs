autoLibrary
fnTop(program$)
on error goto Ertn
! r: setup

		! from Current COLLECTIONS program
		transType_invoice    	=1 ! 1 Invoices
		transType_debit      	=5 ! 2 Debit Memos
		transType_collection	=4 ! 3 Collections
		transType_credit     	=6 ! 4 Credit Memos
		transType_credit     	=6 ! 4 Credit Memos

		! TO CO TransactionCode
		! 1 Invoice
		! 2 Finance Charge
		! 3 Standard Charge
		! 4 Collection
		! 5 Debit Memo
		! 6 Credit Memo

		open #h_company=fnH: 'Name=S:\Core\Data\acsllc\Company.h[cno],Shr',i,i
		dim gln1(3)
		dim gln2(3)
		read #h_company,using 'form pos 161,3*n 1,pos 178,n 3,n 6,n 3,n 3,n 6,n 3': i3,i4,i5,mat gln1,mat gln2
		! i3=1 ! ENTER G/L #'S
		close #h_company:

		sz=5
		gx=2
		dim gl(10,4)
		mat gl(10,2)=(0)
		dim pgl(3)
		mat pgl(1)=(0)
		gpx=1

		dim ot1$(49)
		ot1$(1 )='3,30,C 5,ut,n'
		ot1$(2 )='4,30,c 12,ut,n'
		ot1$(3 )='5,30,n 6,ut,n'
		ot1$(4 )='6,30,n 11.2,ut,n'
		ot1$(5 )='7,30,c 20,ut,n'
		ot1$(6 )='8,30,n 11.2,ut,n'
		ot1$(7 )='9,34,n 6,ut,n'
		ot1$(8 )='12,24,n 6,ut,n'
		ot1$(9 )='12,40,n 11.2,ut,n'
		ot1$(10)='13,24,n 6,ut,n'
		ot1$(11)='13,40,n 11.2,ut,n'
		ot1$(12)='14,24,n 6,ut,n'
		ot1$(13)='14,40,n 11.2,ut,n'
		ot1$(14)='15,24,n 6,ut,n'
		ot1$(15)='15,40,n 11.2,ut,n'
		ot1$(16)='16,24,n 6,ut,n'
		ot1$(17)='16,40,n 11.2,ut,n'
		ot1$(18)='17,24,n 6,ut,n'
		ot1$(19)='17,40,n 11.2,ut,n'
		ot1$(20)='18,24,n 6,ut,n'
		ot1$(21)='18,40,n 11.2,ut,n'
		ot1$(22)='19,24,n 6,ut,n'
		ot1$(23)='19,40,n 11.2,ut,n'
		ot1$(24)='20,24,n 6,ut,n'
		ot1$(25)='20,40,n 11.2,ut,n'
		ot1$(26)='21,24,n 6,ut,n'
		ot1$(27)='21,40,n 11.2,ut,n'

! /r
gosub OpenFiles
goto ScreenBatch
OpenFiles: ! r:
! open #hTransBatch:=fnH: 'Name=[Temp]\transBatch[acsUserId].h[cno],Replace,RecL=239',i,outi,r
open #hTransBatch:=fnH: 'Name=[Temp]\transBatch[acsUserId].h[cno],Use,RecL=239',i,outi,r
FtransBatch: form pos 1,C 5,C 12,N 6,2*PD 5.2,PD 2,2*N 1,C 20,x 3,n 6,x 3,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2,x 3,n 6,x 3,pd 5.2
       FtransBatchSimple: form pos 1,c 5,c 12,n 6,2*pd 5.2,pd 2,2*n 1,c 20
open #hCl1=fnH: 'Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\Client-Idx.h[cno],Shr',i,i,k
FclientName: form pos 6,c 30
open #hCl2=fnH: 'Name=S:\Core\Data\acsllc\Client.h[cno],KFName=S:\Core\Data\acsllc\CLIndx2.h[cno],Shr',i,i,k  ! alpha index on clients
return ! /r

ScreenBatch: ! r:
	dim proofTotal(10)
do
	fntos : rc=0 : lc=0

	fnButton(13, 1,'Add Invoice'     	,21) ! , 0,0)
	fnButton(13, 16,'Add Debit Memo' 	,22) ! , 0,0)
	fnButton(13, 32,'Add Collection' 	,23) ! , 1,0)
	fnButton(13, 48,'Add Credit Memo'	,24) ! , 0,0)
	fn_gridDraw(15,1,hTransBatch,hCl1,totalAccount,tdt,totalCashSales,mat proofTotal)
	fnLbl( 2, 5,'Total Account #s:',20,1)     	: fnTxt( 2,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(totalAccount)
	fnLbl( 3, 5,'Total Invoices:',20,1)        	: fnTxt( 3,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(proofTotal(transType_invoice))
	fnLbl( 4, 5,'Total Debit Memos:',20,1)    	: fnTxt( 4,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(proofTotal(transType_debit))
	fnLbl( 5, 5,'Total Collections:',20,1)    	: fnTxt( 5,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(proofTotal(transType_collection))
	fnLbl( 6, 5,'Total Credit Memos:',20,1)   	: fnTxt( 6,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(proofTotal(transType_credit))
	fnLbl( 7, 5,'Total Cash Sales:',20,1)     	: fnTxt( 7,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(totalCashSales)
	fnLbl( 8, 5,'Total Discounts Taken:',20,1)	: fnTxt( 8,26,0,11,0,'pointtwo',1) : resp$(rc+=1)=str$(tdt)
	resp_recSelected=1 ! rc+=1
	fnCmdKey('Post Batch'            	,8, 0,0)
	fnCmdKey('Edit'            	,2, 1,0)
	fnCmdKey('Print Listing'  	,3, 0,0)
	fnCmdKey('Finish'          	,5, 0,1)
	ckey=fnacs(mat resp$)
	if ckey=5 then
		transType=0
		vf=1
		goto Xit ! ScreenTotals
	end if
	if ckey=8 then
		! goto ChainArMerge
		close #hTransBatch:
		close #hCl1:
		close #hCl2:
		fnClientBillingMergeTrans
		! exe 'dir "[Temp]\transBatch[acsUserId].h[cno]"'
		fnFree('[Temp]\transBatch[acsUserId].h[cno]')
		gosub OpenFiles
		
	else if ckey=3 then
		fn_printListing(hTransBatch)
	else if ckey=2 then
	
	pause
		r1=val(resp$(resp_recSelected))
		fn_edit(r1)
	else if ckey=>21 and ckey<=24 then ! add one
		! transType=0
		if ckey=21 then transType=transType_invoice    
		if ckey=22 then transType=transType_debit      
		if ckey=23 then transType=transType_collection 
		if ckey=24 then transType=transType_credit     
		goto ScreenAddEdit
	end if
loop ! /r

ScreenAddEdit: ! r:

	if ~setupScr1 then ! r:
		setupScr1=1
		dim sc2$(9)
		sc2$(1)='Client'
		sc2$(2)='Invoice'
		sc2$(3)='Date'
		sc2$(4)='Amount'
		sc2$(5)='Description'
		sc2$(6)='Cost of Goods'
		sc2$(7)=''
		sc2$(8)='G/L Account'
		sc2$(9)='Amount'
		if transType=transType_credit or transType=transType_collection then
			sc2$(7)='G/L # to Credit'
		else
			sc2$(7)='G/L # to Debit'
		end if
		if transType=transType_collection then
			sc2$(6)='Discount Amount'
		else
			sc2$(6)=''
		end if
		if gx=0 then sc2$(7)=' '
		pr newpage
		dim flo1$(11)
		flo1$( 1)=' 3, 5,c 20'
		flo1$( 2)=' 4, 5,c 20'
		flo1$( 3)=' 5, 5,c 20'
		flo1$( 4)=' 6, 5,c 20'
		flo1$( 5)=' 7, 5,c 20'
		flo1$( 6)=' 8, 5,c 20'
		flo1$( 7)=' 9, 5,c 20'
		flo1$( 8)='11,20,c 20'
		flo1$( 9)='11,40,c 20'
		flo1$(10)=' 1,15,c 40'
		flo1$(11)=' 2, 5,c 45'
	end if ! /r

	dim hd$(2)*50
	hd$(1)='Add '&fn_entryType$(transType)
	hd$(2)='Client Number as 0 to stop'

	pr f mat flo1$: mat sc2$,mat hd$
	ps1=0
	if vf then
		dim clientId$*5
		dim id$*20
		dim iv$*12
		if gx then
			pr f mat ot1$: clientId$,iv$,transDate,transAmt,id$,transDiscount,mat pgl,mat gl
		else
			ScreenSomething1b: !
			pr f mat ot1$: clientId$,iv$,transDate,transAmt,id$,transDiscount
		end if
	end if
	goto ScreenSomething2 ! 
	ScreenSomething2: !
	! r: set mat fli1$
		dim fli1$(49)
		fli1$(1 )='3,30,C 5,ut,n'
		fli1$(2 )='4,30,c 12,cu,n'
		fli1$(3 )='5,30,n 6,ut,n'
		fli1$(4 )='6,30,n 11.2,ut,n'
		fli1$(5 )='7,30,c 20,ut,n'
		fli1$(6 )='8,30,n 11.2,ut,n'
		fli1$(7 )='9,34,n 6,ut,n'
		fli1$(8 )='12,24,n 6,ut,n'
		fli1$(9 )='12,40,n 11.2,ut,n'
		fli1$(10)='13,24,n 6,ut,n'
		fli1$(11)='13,40,n 11.2,ut,n'
		fli1$(12)='14,24,n 6,ut,n'
		fli1$(13)='14,40,n 11.2,ut,n'
		fli1$(14)='15,24,n 6,ut,n'
		fli1$(15)='15,40,n 11.2,ut,n'
		fli1$(16)='16,24,n 6,ut,n'
		fli1$(17)='16,40,n 11.2,ut,n'
		fli1$(18)='17,24,n 6,ut,n'
		fli1$(19)='17,40,n 11.2,ut,n'
		fli1$(20)='18,24,n 6,ut,n'
		fli1$(21)='18,40,n 11.2,ut,n'
		fli1$(22)='19,24,n 6,ut,n'
		fli1$(23)='19,40,n 11.2,ut,n'
		fli1$(24)='20,24,n 6,ut,n'
		fli1$(25)='20,40,n 11.2,ut,n'
		fli1$(26)='21,24,n 6,ut,n'
		fli1$(27)='21,40,n 11.2,ut,n'
	! /r

	pr f '5,30,pic(zzzzzz)': transDate
	pr f '24,20,C 50,N': 'F1 Continue   F2 verify name    F4 Search'
	if ~gx then
		do
			input fields mat fli1$: clientId$,iv$,transDate,transAmt,id$,transDiscount conv L870
			if cmdkey=4 then
				gosub TmSrch
				goto ScreenSomething1b
			else
				clientId$=rpad$(trim$(clientId$),5)
				if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,'RC','U') : ce=0
			end if
			goto L1280
			L870: !
			if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,'RC','U') : ce=cnt+1
			fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,'U','RC')
		loop
	end if
	if ps1=1 or vf=1 then goto L1060
	Ss2rInput: !
	rinput fields '3,30,C 5,EU,n': clientId$ conv Ss2rInput
	if cmdkey=4 then gosub TmSrch : goto Ss2rInput
	clientId$=rpad$(trim$(clientId$),5)
	dim otgl$(3)
	otgl$(1)='9,30,pic(zzz)' : otgl$(2)='9,34,pic(zzzzzz)' : otgl$(3)='9,41,pic(zzz)'
	if ltrm$(clientId$)='-1' then
		pr f mat otgl$: mat gln1
	else
		pr f mat otgl$: mat gln2
	end if

	if ltrm$(clientId$)='0' or ltrm$(clientId$)='' then
		if vf=1 then goto RewriteTransBlank
		goto FinisAddEdit
	end if
	
	! r: clientName - manual insead of using fnClientName$
	if ltrm$(clientId$)='-1' then
		dim clientName$*30
		clientName$='CASH SALE'
	else
		read #hCl1,using FclientName,key=rpad$(trim$(clientId$),kln(hCl1)),release: clientName$ nokey ClientNokey
	end if
	pr f '3,40,C 30,N': clientName$
goto L1060 ! /r
	
ClientNokey: ! r:
	clientName$='***INVALID CLIENT NUMBER***'
	pr f '3,40,C 30,R,N': clientName$
goto Ss2rInput ! /r
	
L1060: ! r:
	fli1$(4)='6,30,n 11.2,ut,n'
	if r1>0 then goto L1170
	if transType=transType_collection then fli1$(4)='6,30,n 11.2,ue,n'
	input fields mat fli1$: clientId$,iv$,transDate,transAmt,id$,transDiscount,mat pgl,mat gl conv L1240
	if cmdkey=2 then goto Ss2rInput
	if transType<>transType_collection then goto L1200
	fli1$(4)='6,30,n 11.2,ut,n'
	! if sz=4 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=transAmt
	! if sz=3 then gl(1,1)=gln1(2): gl(1,2)=gln1(3): gl(1,3)=transAmt
	! if sz=2 then gl(1,2)=gln1(2): gl(1,1)=gln1(1): gl(1,3)=gln1(3): gl(1,4)=transAmt
	if sz=5 then gl(1,1)=gln1(2): gl(1,2)=transAmt
	
	L1170: !
	pr f mat ot1$: clientId$,iv$,transDate,transAmt,id$,transDiscount,mat pgl,mat gl
	
	L1180: !
	input fields mat fli1$: clientId$,iv$,transDate,transAmt,id$,transDiscount,mat pgl,mat gl conv L1240
	if cmdkey=2 then goto Ss2rInput
	
	L1200: ! 
	clientId$=rpad$(trim$(clientId$),5)
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,'RC','U')
	ce=0
	goto L1280
	
	L1240: !
	if ce>0 then fli1$(ce)=srep$(fli1$(ce),1,'RC','U')
	ce=cnt+1
	fli1$(ce)=srep$(uprc$(rtrm$(fli1$(ce))),1,'U','RC')
	if cnt<=4 then goto L1060
		goto L1180
	! end if 

	L1280: !
	if ltrm$(clientId$)='0' or ltrm$(clientId$)='' then
		if vf=1 then goto RewriteTransBlank
		goto FinisAddEdit
	end if
	ps1=1
	if transDate<10100 or transDate>123199 then
		pr f '5,48,c 20': 'Invalid Date'
		goto ScreenSomething2
	end if

	if transAmt then ! r: 
		if gx=0 then
			gosub AccumAndSave : goto FinisAddEdit
		else if pgl(gpx)>0 then
			gla=0
			for j=1 to 10
				if ~gl(j,gx) then goto Lx321
				gla+=gl(j,gx)
			next j
			Lx321: !
			
			if transType=transType_collection then gla-=transDiscount
			if gla<>transAmt then
				pr f '11,2,c 75': ' G/L allocations do not agree with total amount.  Press enter to continue.'
				input fields '11,78,c 1,EU,n': pause$
				pr f '11,2,c 75,n,n': ' '
				goto ScreenSomething2
			end if
			gosub AccumAndSave : goto FinisAddEdit
			
		end if
		pr f '9,45,c 30': 'G/L # REQUIRED'
	else
		pr f '6,48,c 20': 'NO AMOUNT ENTERED'
	end if ! /r
goto ScreenSomething2 ! /r

FinisAddEdit: ! 
	if doingFnEdit then 
		goto FinisFnEdit
	end if
goto ScreenBatch ! /r
	AccumAndSave: ! r:
		if ltrm$(clientId$)<>'-1' then
			totalAccount+=val(clientId$) conv ignore
			proofTotal(transType)+=transAmt
			if transType=transType_collection then tdt+=transDiscount
			if ltrm$(clientId$)='-1' then totalCashSales+=transAmt
		end if
		if vf=1 then goto RewrTransNow
		write #hTransBatch,using FtransBatch: clientId$,iv$,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6,id$,mat pgl,mat gl
		clientId$=''
		q2=0
	return ! r


! /r   (is this where this goes?)

RewriteTransBlank: ! r:  rewrite hTransBatch
	iv$=id$=''
	transDate=transDiscount=transAmt=unusedtr4=transType=unusedtr6=0
	mat gl=(0)
goto RewrTransNow ! /r
RewrTransNow: ! r:
	rewrite #hTransBatch,using FtransBatch,rec=r1: clientId$,iv$,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6,id$,mat pgl,mat gl
	clientId$=''
goto FinisAddEdit ! /r

def fn_printListing(hTransBatch; ___,r,clientId$,iv$,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6)
	r=0
	fnOpenPrn
	FprH: form pos 1,c 8,pos 21,Cc 50
	pr #255,using FprH: date$,env$('cnam')
	pr #255,using FprH: time$,'Input Edit List'
	pr #255: 'RefNo  Clnt  InvoiceNo';
	pr #255: tab(34);'Date     Amount             Description           Discount          Tr Code'
	restore #hTransBatch: ioerr Pel_finis
	do
		read #hTransBatch,using FtransBatchSimple: clientId$,iv$,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6,id$ eof Pel_finis
		if ltrm$(clientId$)<>'0' and ltrm$(clientId$)<>'' then
			pr #255,using FprEntryLine: rec(hTransBatch),clientId$,iv$,transDate,transAmt,0,fnClientName$(clientId$)(1:22),transDiscount,transType
			FprEntryLine: form pos 1,n 4,x 2,c 5,x 2,c 18,n 6,n 11.2,pic(zzzzzz),x 7,c 22,n 12.2,n 12
		end if
	loop
	Pel_finis: !
	fnClosePrn
fnend

def fn_edit(r1) ! uses mostly locals
	doingFnEdit=1
	! r:
	read #hTransBatch,using FtransBatch,rec=r1: clientId$,iv$,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6,id$,mat pgl,mat gl noRec FinisAddEdit
	if ltrm$(clientId$)='0' or ltrm$(clientId$)='' then goto FinisAddEdit
	if p><-1 then totalAccount-=val(clientId$) conv ignore
	proofTotal(transType)-=transAmt
	if ltrm$(clientId$)='-1' then totalCashSales-=transAmt
	if transType=transType_collection then tdt-=transDiscount
	
	hd$(1)='Edit '&fn_entryType$(transType)
	hd$(2)='Enter Client as 0 to Delete this entry'
	vf=1
goto ScreenAddEdit ! /r
	

	FinisFnEdit: ! returns here from FinisAddEdit
fnend

def fn_entryType$(transType)
	if ~setupEntryType then
		setupEntryType=1
		dim entryType$(6)
		entryType$(1)='Invoice'
		entryType$(2)=''
		entryType$(3)=''
		entryType$(4)='Collection'
		entryType$(5)='Debit'
		entryType$(6)='Credit'
	end if
	fn_entryType$=entryType$(transType)
fnend
ChainArMerge: !
fnChain('S:\Client Billing\Merge Transactions')
Xit: fnXit

TmSrch: ! r: search for customer #
	! uses hCl2, returns clientId$
	dim selection$*70
	fnTmSearch(hCl2,'form pos 1,c 5,pos 6,c 30,pos 66,c 15,pos 283,pd 5.2','pic($$$,$$$.##)',selection$,5)
	clientId$=selection$ ! pull key from first field in search line
return ! /r

def fn_gridDraw(row,col,hTransBatch,hCl1,&totalAccount,&tdt,&totalCashSales,mat proofTotal; ___,clientId$*5,iv$*12,id$*20,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6)

	if ~setupGridDraw then! r:
		setupGridDraw=1

		dim chdr$(8)*30
		mat chdr$(8)
		chdr$(1)='Rec'
		chdr$(2)='Client'
		chdr$(3)='InvoiceNo'
		chdr$(4)='Date '
		chdr$(5)='Amount'
		chdr$(6)='Description'
		chdr$(7)='Entry Type'
		chdr$(8)='Discount'

		dim cm$(8)
		mat cm$=('')
		! cm$(1)='32'
		! cm$(2)='32'
		! cm$(3)='10'
		cm$(4)='1'  ! Date
		cm$(5)='10' ! Amount
		cm$(7)='' ! '10'
		cm$(8)='10'

	end if ! /r
	fnFlexInit1('cbTrans',row,col,1,40,mat chdr$,mat cm$,1)
	restore #hTransBatch:
	
	totalAccount=tdt=totalCashSales=0
	mat proofTotal=(0)
	
	do
		read #hTransBatch,using FtransBatchSimple: clientId$,iv$,transDate,transDiscount,transAmt,unusedtr4,transType,unusedtr6,id$ eof GdFinis

		! if ltrm$(clientId$)<>'0' and ltrm$(clientId$)<>'' then
			dim m1_item$(8)*64
			m1_item$(1)=str$(rec(hTransBatch))
			m1_item$(2)=ltrm$(fnClientName$(clientId$))&' ( '&clientId$&')'
			m1_item$(3)=iv$
			m1_item$(4)=str$(transDate) 
			m1_item$(5)=str$(transAmt) 
			m1_item$(6)=fnClientName$(clientId$)
			m1_item$(7)=ltrm$(fn_entryType$(transType)&' ('&str$(transType)&')')
			m1_item$(8)=str$(transDiscount) 
			
	! if ltrm$(clientId$)<>'-1' then
			totalAccount+=val(clientId$) conv ignore
			proofTotal(transType)+=transAmt
			if transType=transType_collection then tdt+=transDiscount
			if ltrm$(clientId$)='-1' then totalCashSales+=transAmt
	! end if
			
			
			fnFlexAdd1(mat m1_item$)
		! end if
	loop
	GdFinis: !
fnend

include: ertn
