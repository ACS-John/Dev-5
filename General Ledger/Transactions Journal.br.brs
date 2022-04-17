! formerly S:\acsGL\acglTrJr
! pr Disbursements, Receipts, General adj/ap/pr/ar, Sales,
! and Purchases Journals a.k.a. Transaction Journals

autoLibrary
on error goto Ertn
fnTop(program$)

! r: set mat journalTitle$ journal options
	dim journalTitle$(8)*21 ! *30
	journalTitle$(1)='Disbursements Journal'   	! 'Disbursements Journal'
	journalTitle$(2)='Receipts Journal'         	! 'Receipts Journal'
	journalTitle$(3)='General Journal'          	! 'General Journal      (Adj)'
	journalTitle$(4)='General Journal'          	! 'General Journal      (A/P)'
	journalTitle$(5)='General Journal'          	! 'General Journal      (Payroll)'
	journalTitle$(6)='General Journal'          	! 'General Journal      (A/R)'
	journalTitle$(7)='Sales Journal'            	! 'Sales Journal'
	journalTitle$(8)='Purchases Journal'       	! 'Purchases Journal'
! /r
dim enableJournal(8)
if fnprocess=1 then
	currentOrPrior=1
	filterPeriod=0
	mat enableJournal=(1)
else
	if fn_askPeriod(currentOrPrior,filterPeriod,mat enableJournal)=5 then goto Xit
end if

! r: Print Journals


	! r: Index and Open hTrans (current or prior)
	dim tempSourceFile$*256
	tempSourceFile$='[Temp]\acsGLtrans[acsUserId]_data.h[cno]'
	dim tempSourceIndx$*256
	tempSourceIndx$='[Temp]\acsGLtrans[acsUserId]_indx.h[cno]'
	if currentOrPrior=1 then ! Current Period
		fnCopy('[Q]\GLmstr\GLtrans.h[cno]',tempSourceFile$)
		! fnIndex('[Q]\GLmstr\GLTrans.h[cno]','[Temp]\fsindex.h[cno]','25/29/1 2/12/12') ! transType+transRef$+transGl$
		! open #hTrans=fnH: 'Name=[Q]\GLmstr\GLtrans.h[cno],KFName=[Temp]\fsindex.h[cno],Shr',i,i,k ! formerly #3
	else if currentOrPrior=2 then ! Prior Period (Accumulated Transactions)
		fnCopy('[Q]\GLmstr\AcTrans.h[cno]',tempSourceFile$)
		! fnIndex('[Q]\GLmstr\AcTrans.h[cno]','[Temp]\fsindex.h[cno]','25/29/1 2/12/12') ! transType+transRef$+transGl$
		! open #hTrans=fnH: 'Name=[Q]\GLmstr\ACtrans.h[cno],KFName=[Temp]\fsindex.h[cno],Shr',i,i,k ! formerly #3
	end if
	! r: right align the reference numbers
		open #hTrans=fnH: 'Name='&tempSourceFile$&',Shr',i,outi
		do
			read #hTrans,using 'form pos 29,c 12': transRef$ eof EoTransRegFix
			transRef$=lpad$(trim$(transRef$),12)
			rewrite #hTrans,using 'form pos 29,cr 12': transRef$
		loop
		EoTransRegFix:!
		close #hTrans:
	! /r
	fnIndex(tempSourceFile$,tempSourceIndx$,'25/29/1 2/12/12') ! transType+transRef$+transGl$
	open #hTrans=fnH: 'Name='&tempSourceFile$&',KFName='&tempSourceIndx$&',Shr',i,i,k ! formerly #3
	! /r

	fnopenprn
	isFirstTransaction=1
	do ! r: main loop 
		dim transGl$*12,transDate,transAmt,transType,transRef$*12,transDesc$*30,transPeriod
		if fn_readTrans(currentOrPrior,transGl$,transDate,transAmt,transType,transRef$,transDesc$,transPeriod)=-4270 then goto EoJounrals
		if isFirstTransaction then
			isFirstTransaction=0
			dim transJournal$*21
			transJournal$=journalTitle$(transType)
			fn_prHeader(transType)
		else
			if journalTitle$(transType)><transJournal$ then
				fn_prNetAndBlankLine(net,transType)
				fn_journalTotals(net,totalDebits,totalCredits)
				fn_prNewpageAndHeader
				transJournal$=journalTitle$(transType)
				dim priorRef$*12
				priorRef$=''
			end if
			if transRef$<>priorRef$ or transRef$='999999999999' then
				fn_prNetAndBlankLine(net,transType)
				net=0
			end if
		end if

		priorRef$=transRef$
		! r: print report line
			if transRef$='999999999999' then transRef$=''
			if transAmt>0 then
				pr #255,using FlineDebit: transRef$,transDate,transDesc$,transGl$,transAmt pageoflow PgOf
				FlineDebit: form pos 3,c 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,c 12,pos 69,pic(------,---,---.##)
			else
				pr #255,using FlineCredit: transRef$,transDate,transDesc$,transGl$,transAmt pageoflow PgOf
				FlineCredit: form pos 3,c 12,pos 16,pic(zz/zz/zz),pos 26,c 30,pos 57,c 12,pos 82,pic(------,---,---.##)
			end if
		! /r
		! r: accumulate totals
			if transAmt<0 then  ! credits
				totalCredits+=transAmt
			else
				totalDebits+=transAmt
			end if
			if uprc$(transDesc$(1:6))<>'CONTRA' then ! no contra entries in net
				net+=transAmt
			end if
		! /r

	loop ! /r main loop
! /r

EoJounrals: ! r:
	if totalDebits or totalCredits then
		net=0
		fn_prNetAndBlankLine(net,transType,1)
		fn_journalTotals(net,totalDebits,totalCredits)
	end if
	fncloseprn
goto Xit ! /r
PgOf: ! r:
	pr #255: newpage
	fn_prHeader(transType)
continue  ! /r
def fn_readTrans(currentOrPrior, _
		&transGl$,&transDate,&transAmt,&transType,&transRef$,&transDesc$,&transPeriod; _
		___,returnN)
	ReadTrans: !
	if currentOrPrior=2 then
		! read period code if from history
		read #hTrans,using Ftrans: transGl$,transDate,transAmt,transType,transRef$,transDesc$,transPeriod eof ReadTransEoF
	else
		read #hTrans,using Ftrans: transGl$,transDate,transAmt,transType,transRef$,transDesc$ eof ReadTransEoF
	end if
	Ftrans: form pos 1,c 12,n 6,pd 6.2,n 2,x 2,c 12,c 30,n 2
	if currentOrPrior=2 and filterPeriod and filterPeriod<>transPeriod then goto ReadTrans
	if ~transType and ~transAmt then goto ReadTrans
	if ~enableJournal(transType) then goto ReadTrans
	if transType<1 or transType>9 then transType=1
	goto ReadTransFinis
	
	ReadTransEoF: !
		returnN=-4270
	goto ReadTransFinis
	
	ReadTransFinis: !
	fn_readTrans=returnN
fnend
def fn_prHeader(transType; ___,title$*21,b$*3)
	! uses global: mat journalTitle$
	pr #255,using Fhdr1: '',env$('cnam')
	pr #255,using Fhdr1: date$('mm/dd/yy'),env$('program_caption')
	if transType then
		title$=journalTitle$(transType)
		pr #255,using Fhdr1: time$,title$
	end if
	Fhdr1: form pos 1,c 8,pos 15,cc 50
	pr #255,using Fhdr1: '',fnpedat$
	pr #255: tab(115);'Page '&str$(pageNumber+=1)
	pr #255: '   Reference             Transaction';tab(79);'Debit';tab(92);'Credit'
	pr #255: '     Number      Date    Description';tab(61);'Account';
	if transType=1 then b$='Net'
	pr #255,using FHdrAmt: 'Amount       Amount',b$
	FHdrAmt: form pos 79,c 19,pos 111,c 3
	pr #255: ' _____________ ________  ____________________';tab(59);'___________';tab(79);'______       ______';
	if transType=1 then
		pr #255,using FHdrU1: '___'
	else
		pr #255,using FHdrU1: '   '
	end if
	FHdrU1: form pos 111,c 3
fnend

def fn_prNetAndBlankLine(net,transType; theEndisnow)
	! uses global: mat journalTitle$
	! pr #255: 'debug aaa PjPrNetAndNewPage ___________________'
	if transType=1 then ! Disbursements Journal
		pr #255,using Fnet: net pageoflow PgOf
		Fnet: form pos 100,pic(---,---,---.##)
	end if

	if ~theEndisnow then  ! if journalTitle$(transType)=transJournal$ and ~theEndisnow then
		pr #255: '' pageoflow PgOf
	end if

fnend
def fn_journalTotals(&net,&totalDebits,&totalCredits)

	pr #255: tab(72);'_____________';tab(86);'______________'
	pr #255,using FjournalTotal1: 'Journal Totals',totalDebits,totalCredits
	FjournalTotal1: form pos 55,c 14,pos 70,pic(----,---,---.##),pic(----,---,---.##)
	pr #255: tab(72);'=============';tab(86);'=============='

	net=totalDebits=totalCredits=0
fnend
def fn_prNewpageAndHeader
		pr #255: newpage
		fn_prHeader(transType)
fnend

def fn_askPeriod(&currentOrPrior,&filterPeriod,mat enableJournal; ___, _
	rc_enable1,rc_enable2,rc_enable3,rc_enable4,rc_enable5,rc_enable6,rc_enable7,rc_enable8, _
	rc_priorPeriod,rc_cp1,rc_cp2, _
	returnN,respc,ckey,fraWidth,frame)

	fnTos
	dim resp$(50)*50
	fraWidth=34 ! 48
	fnFra(1,1,3,fraWidth,'Print from current month files or history',' ') : frame=1
	fnOpt(1,3,'Current Period Transactions'  	, 0,frame)	: resp$(rc_cp1=respc+=1)=fnPcRegRead$('cp1','True' )
	fnOpt(2,3,'Prior Period Transactions'    	, 0,frame)	: resp$(rc_cp2=respc+=1)=fnPcRegRead$('cp2','False')
	! fnLbl(myline,mypos,t$*200; mylen,myalign,font_mod,container,tabcon,lbl_tooltip$*256)
	fnLbl(3,6,'Prior Period:'	,13,1,0,frame)
	fnTxt(3,20,2,0,1,'30',0,'Prior Period is only applicable if printing from history.  Select the closed period to print. Leave blank to print all periods.',frame)
	! fnLbl(2,6+26,'Prior Period:'	,13,1,0,frame)
	! fnTxt(2,20+26,2,0,1,'30',0,'Prior Period is only applicable if printing from history.  Select the closed period to print. Leave blank to print all periods.',frame)

	resp$(rc_priorPeriod=respc+=1)=fnPcRegRead$('filterPeriod')

	fnFra(6,1,8,fraWidth,'Select Journals to Print',' ') : frame=2
	fnChk(1,30,'Disbursements Journal'    	,1,frame) 	: resp$(rc_enable1=respc+=1)=fnPcRegRead$('enableJournal 1', 'True' )
	fnChk(2,30,'Receipts Journal'          	,1,frame) 	: resp$(rc_enable2=respc+=1)=fnPcRegRead$('enableJournal 2', 'True' )
	fnChk(3,30,'General Journal (Adj)'    	,1,frame) 	: resp$(rc_enable3=respc+=1)=fnPcRegRead$('enableJournal 3', 'True' )
	fnChk(4,30,'General Journal (A/P)'    	,1,frame) 	: resp$(rc_enable4=respc+=1)=fnPcRegRead$('enableJournal 4', 'False')
	fnChk(5,30,'General Journal (Payroll)'	,1,frame) 	: resp$(rc_enable5=respc+=1)=fnPcRegRead$('enableJournal 5', 'False')
	fnChk(6,30,'General Journal (A/R)'    	,1,frame) 	: resp$(rc_enable6=respc+=1)=fnPcRegRead$('enableJournal 6', 'False')
	fnChk(7,30,'Sales Journal'             	,1,frame) 	: resp$(rc_enable7=respc+=1)=fnPcRegRead$('enableJournal 7', 'False')
	fnChk(8,30,'Purchases Journal'         	,1,frame) 	: resp$(rc_enable8=respc+=1)=fnPcRegRead$('enableJournal 8', 'False')
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then
		fnPcReg_write('cp1',resp$(rc_cp1))
		fnPcReg_write('cp2',resp$(rc_cp2))
		currentOrPrior=1 : if resp$(rc_cp2)='True' then currentOrPrior=2
		filterPeriod=val(resp$(rc_priorPeriod))
		if filterPeriod<0 or filterPeriod>13 then filterPeriod=0
		fnPcReg_write('filterPeriod',str$(filterPeriod))
		mat enableJournal=(0)
		fnPcReg_write('enableJournal 1',resp$(rc_enable1)) : if resp$(rc_enable1)='True' then enableJournal(1)=1
		fnPcReg_write('enableJournal 2',resp$(rc_enable2)) : if resp$(rc_enable2)='True' then enableJournal(2)=1
		fnPcReg_write('enableJournal 3',resp$(rc_enable3)) : if resp$(rc_enable3)='True' then enableJournal(3)=1
		fnPcReg_write('enableJournal 4',resp$(rc_enable4)) : if resp$(rc_enable4)='True' then enableJournal(4)=1
		fnPcReg_write('enableJournal 5',resp$(rc_enable5)) : if resp$(rc_enable5)='True' then enableJournal(5)=1
		fnPcReg_write('enableJournal 6',resp$(rc_enable6)) : if resp$(rc_enable6)='True' then enableJournal(6)=1
		fnPcReg_write('enableJournal 7',resp$(rc_enable7)) : if resp$(rc_enable7)='True' then enableJournal(7)=1
		fnPcReg_write('enableJournal 8',resp$(rc_enable8)) : if resp$(rc_enable8)='True' then enableJournal(8)=1
	end if
	returnN=ckey
	fn_askPeriod=returnN
fnend
Xit: fnXit
include: ertn
