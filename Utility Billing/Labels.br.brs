! -- pr Customer Labels
fn_setup
fnTop(program$)
Screen1: ! r:
	fnTos
	fnLbl(1,1,'Sort by:',20,1)
	fnComboA('ublabels-ord',1,22,mat optSequence$,'Choose the sequence for the labels to be generated')
	fnreg_read('ublabel.sequence',resp$(1),optSequence$(sequence_account),1)

	fnLbl(3,1,'Print Address:',20,1)
	fnComboA('ubnamlst-act',3,22,mat optAddress$)
	fnreg_read('ublabel.address',resp$(2), optAddress$(ao_billing),1)

	fnLbl(5,1,'Line 1:',20,1)
	fnComboA('ublabel_tl1',5,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 1',resp$(3), optLineX$(1),1)

	fnLbl(6,1,'Line 2:',20,1)
	fnComboA('ublabel_tl2',6,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 2',resp$(4), optLineX$(2),1)

	fnLbl(7,1,'Line 3:',20,1)
	fnComboA('ublabel_tl3',7,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 3',resp$(5), optLineX$(3),1)

	fnLbl(8,1,'Line 4:',20,1)
	fnComboA('ublabel_tl4',8,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 4',resp$(6), optLineX$(4),1)

	fnLbl(9,1,'Line 5:',20,1)
	fnComboA('ublabel_tl5',9,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 5',resp$(7), optLineX$(5),1)

	fnCmdSet(2)
	ckey=fnAcs(mat resp$) ! select order of printing
	if ckey=5 then goto Xit
	! r: process mat resp$ into local variables
	fnreg_write('ublabel.sequence',resp$(1))
	fnreg_write('ublabel.address',resp$(2))
	fnreg_write('ublabel.line 1',resp$(3))
	fnreg_write('ublabel.line 2',resp$(4))
	fnreg_write('ublabel.line 3',resp$(5))
	fnreg_write('ublabel.line 4',resp$(6))
	fnreg_write('ublabel.line 5',resp$(7))
	! annbc=sequence_name ! default to name sequence
	pt$(5)=''
	if resp$(1)=optSequence$(sequence_account) then
		annbc=sequence_account
	! else if resp$(1)=optSequence$(sequence_name) then
	! 	annbc=sequence_name
	else if resp$(1)=optSequence$(sequence_bar_code) then
		annbc=sequence_bar_code
		pt$(5)='BAR'
	else if resp$(1)=optSequence$(sequence_route) then
		annbc=sequence_route
	else if resp$(1)=optSequence$(sequence_grid) then
		annbc=sequence_grid
	else if resp$(1)=optSequence$(sequence_bulk_sort) then
		annbc=sequence_bulk_sort
	else if resp$(1)=optSequence$(sequence_alphaSort) then
		annbc=sequence_alphaSort
	end if

	altaddr=srch(mat optAddress$,resp$(2))
	dim linePrint(5)
	linePrint(1)=srch(mat optLineX$,resp$(3))
	linePrint(2)=srch(mat optLineX$,resp$(4))
	linePrint(3)=srch(mat optLineX$,resp$(5))
	linePrint(4)=srch(mat optLineX$,resp$(6))
	linePrint(5)=srch(mat optLineX$,resp$(7))
	! /r

	! Skip ScreenFilter with these annbc sequences
	if annbc=sequence_route then
		filterSelected=filter_route
		gosub OpenFiles
		goto SelectByRoute ! ROUTE # SEQUENCE
	else if annbc=sequence_grid then
		gosub OpenFiles
		if fn_readFromGrid then
			goto Finis
		else
			goto Xit
		end if
	else if annbc=sequence_bulk_sort then
		hSort=fn_sort(2)
		gosub OpenFiles
		goto Top
	else if annbc=sequence_alphaSort then
		hSort=fn_sort(1)
		gosub OpenFiles
		goto Top
	end if

goto ScreenFilter ! /r

ScreenFilter: ! r:
	if ~setupScreen2 then
		setupScreen2=1
		dim optFilter$(7)*50
		optFilter$(filter_none     	)='[All]'
		optFilter$(filter_lastMonth	)='Customers billed last month'
		optFilter$(filter_unbilled 	)='Customers not billed last billing'
		optFilter$(filter_range    	)='Range of Accounts'
		optFilter$(filter_select   	)='Individual accounts'
		optFilter$(filter_route    	)='Route'
		optFilter$(filter_active   	)='Active Customers'
	end if


	! r: build mat optFilterEnabled$ from optFilter$ based on annbc setting
	if annbc=sequence_account then
		dim optFilterEnabled$(7)*50
		optFilterEnabled$(1)=optFilter$(1)
		optFilterEnabled$(2)=optFilter$(2)
		optFilterEnabled$(3)=optFilter$(3)
		optFilterEnabled$(4)=optFilter$(4)
		optFilterEnabled$(5)=optFilter$(5)
		optFilterEnabled$(6)=optFilter$(6)
		optFilterEnabled$(7)=optFilter$(7)
		mat optFilterEnabled$(7)
	! else if annbc=sequence_name then
	! 	optFilterEnabled$(1)=optFilter$(1)
	! 	optFilterEnabled$(2)=optFilter$(2)
	! 	optFilterEnabled$(3)=optFilter$(3)
	! 	optFilterEnabled$(4)=optFilter$(5)
	! 	optFilterEnabled$(5)=optFilter$(7)
	! 	mat optFilterEnabled$(5)
	else if annbc=sequence_bar_code then
		optFilterEnabled$(1)=optFilter$(1)
		optFilterEnabled$(2)=optFilter$(2)
		optFilterEnabled$(3)=optFilter$(3)
		optFilterEnabled$(4)=optFilter$(5)
		optFilterEnabled$(5)=optFilter$(6)
		mat optFilterEnabled$(5)
	else if annbc=sequence_route then
		optFilterEnabled$(1)=optFilter$(1)
		optFilterEnabled$(2)=optFilter$(2)
		optFilterEnabled$(3)=optFilter$(3)
		optFilterEnabled$(4)=optFilter$(6)
		mat optFilterEnabled$(4)
	end if
	! /r
	fnTos
	fnLbl(1,1,'Select by:',12,0,0)
	fnComboA('ublabels-ord',1,14,mat optFilterEnabled$,'',30)
	resp$(1)=optFilterEnabled$(1)
	gosub OpenFiles
	fnCmdSet(6)
	ckey=fnAcs(mat resp$) ! method of selection
	filterSelected=srch(mat optFilter$,resp$(1))
	if ckey=5 then goto Xit
	if ckey=2 then goto Screen1
	if annbc=sequence_route then goto SelectByRoute ! select by route
	if filterSelected=filter_range then goto SelectRangeOfAccounts ! select range of customers
	if filterSelected=filter_select then goto SelectIndividualAccounts ! select specific accounts
	if filterSelected=filter_route and annbc=sequence_bar_code then goto SelectByRoute
	if (filterSelected=filter_none or filterSelected=filter_lastMonth or filterSelected=filter_unbilled or filterSelected=filter_active) and annbc=sequence_account then goto SelectStartingCustomer
	! if (filterSelected=filter_none or filterSelected=filter_active) and annbc=sequence_name then goto SCR4F3 ! all customers in name sequence
goto Top ! /r

Top: ! r:
	if annbc=sequence_bar_code then
		! r: BarcodeRead
		read #hAddr,using 'form pos 1,PD 3': r6 eof Finis
		read #hSort,using 'form pos 1,C 16,C 10',rec=r6: srt$,z$ noRec Top
		
		dim extra1$*30 ! fields from Customer File
		dim e$(4)*30
		read #hCustomer,using Fcustomer1,key=z$: z$,mat e$,lastBillingDate,f3$,route,seq,extra1$,final nokey Top
		dim meter_address$*30
		meter_address$=e$(1)
		! /r
	else if annbc=sequence_bulk_sort or annbc=sequence_alphaSort then
		! r: SortRead
		read #hSort,using 'form pos 22,c 10': z$ eof Finis
		! sortReadCount+=1
		read #hCustomer,using Fcustomer2,key=z$: z$,mat e$,f$(1),lastBillingDate,f3$,route,seq,extra1$,bulksort$,final nokey Top
		meter_address$=e$(1)
		! /r
	else
		read #hCustomer,using Fcustomer2: z$,mat e$,f$(1),lastBillingDate,f3$,route,seq,extra1$,bulksort$,final eof Finis
		meter_address$=e$(1)
	end if

	PastRead: !
	! pr xx+=1 : pause
	if filterSelected=filter_route and bk>0 and bk<>route then goto Top ! skip if barcoded and by route, but not right route
	if filterSelected=filter_unbilled and d1=lastBillingDate then goto Top
	if filterSelected=filter_range and z$>h1$ then goto Finis
	if filterSelected=filter_route and bk>0 and route<>bk then goto Top
	if filterSelected=filter_lastMonth and d1><lastBillingDate then goto Top
	if final and final<>4 then goto Top  !  XXX 3/10/22 John - always filter inactive customers except if selected individually

	PastFilters: !
	fn_getAddressLines
	fn_addLabel(mat labeltext$,mat linePrint,final,filterSelected)

	if filterSelected=filter_select then goto SelectIndividualAccounts
goto Top
! /r


def fn_addLabel(mat labeltext$,mat linePrint,final,filterSelected)
	! requires local enum: filter_active

	! if annbc=sequence_bulk_sort then
	!   labeltext$(1)=labeltext$(1)&'  '&bulksort$ ! if bulk sort than auto add bulk sort code on to the end of the Top line
	! else
	fn_setLineText(labeltext$(1),linePrint(1))
	! end if
	fn_setLineText(labeltext$(2),linePrint(2))
	fn_setLineText(labeltext$(3),linePrint(3))
	fn_setLineText(labeltext$(4),linePrint(4))
	fn_setLineText(labeltext$(5),linePrint(5))
	if annbc=sequence_bar_code then
		gosub SortBarCode
	end if
	if final=0 or filterSelected<>filter_active then
		fnaddlabel(mat labeltext$)
	end if
fnend
	def fn_setLineText(&labeltext$,lineToPrint)
		if lineToPrint=line_x_blank then
			labeltext$=''
		else if lineToPrint=line_x_account_number then
			labeltext$=z$
		else if lineToPrint=line_x_accountRightJustified then
			labeltext$=lpad$(z$,22)
		else if lineToPrint=line_x_meter_address then
			labeltext$=meter_address$
		else if lineToPrint=line_x_customer_name then
			labeltext$=pe$(1)
		else if lineToPrint=line_x_mailing_address_line_1 then ! Mailing Address Line 1
			labeltext$=pe$(2)
		else if lineToPrint=line_x_mailing_address_line_2 then ! Mailing Address Line 2 or if blank City State Zip
			labeltext$=pe$(3)
		else if lineToPrint=line_x_mailing_address_line_3 then ! City State Zip if Mailing Address Line 2 not blank
			labeltext$=pe$(4)
		else if lineToPrint=line_x_meter_route_and_sequenc then ! City State Zip if Mailing Address Line 2 not blank
			labeltext$=f3$&' '&str$(route)&' '&str$(seq) ! just use meter 3 for now, French Settlement Gas is the only one that uses this option.
		else if lineToPrint=line_x_meterAndSequence then ! City State Zip if Mailing Address Line 2 not blank
			labeltext$=f3$&'    '&str$(seq) ! just use meter 3 for now, French Settlement Gas is the only one that uses this option.
		else if lineToPrint=line_x_account_meter4_and_seq then ! City State Zip if Mailing Address Line 2 not blank
			labeltext$=z$&' '&f3$&' '&str$(seq) ! just use meter 3 for now, French Settlement Gas is the only one that uses this option.
		else
			labeltext$='(invalid selection)'
		end if
	fnend

Finis: ! r:
	! pr 'sortReadCount=';sortReadCount : pause
	close #hCustomer: ioerr ignore
	close #hAddr: ioerr ignore
	
	if ~fnLabel(mat pt$) then fnChain(program$) ! just restart if it failed to produce labels (ie they canceled out)
	
goto Xit ! /r
Xit: fnXit

SelectRangeOfAccounts: ! r: select range of accounts
	fnTos
	fnLbl(1,1,'Starting Account:',20)
	fncmbact(1,22,1)
	resp$(1)=l1$
	fnLbl(2,1,'Ending Account:',20)
	fncmbact(2,22,1)
	resp$(2)=h1$
	fnCmdSet(22)
	ckey=fnAcs(mat resp$) ! select by range
	if ckey=5 then goto Xit
	l1$=lpad$(trim$(resp$(1)(1:10)),10)
	h1$=lpad$(trim$(resp$(2)(1:10)),10)
	if ckey=6 then fnCustomerSearch(resp$(1)) else goto L1470
	if trim$(l1$)='' then l1$=resp$(1) : goto SelectRangeOfAccounts
	if trim$(h1$)='' then h1$=resp$(1) : goto SelectRangeOfAccounts
	goto SelectRangeOfAccounts
	L1470: !
	if ckey=2 then goto ScreenFilter
	l1$=lpad$(l1$,10) : h1$=lpad$(h1$,10)
	if h1$<l1$ then
		mat msgline$(1)
		msgline$(1)='You have entered invalid accounts!'
		fnMsgBox(mat msgline$,resp$,'',48)
		goto SelectRangeOfAccounts
	end if
	restore #hCustomer,key=l1$: nokey SelectRangeOfAccounts
goto Top ! /r
SelectIndividualAccounts: ! r: select individual accounts
	fnTos
	fnLbl(1,1,'Account:',15)
	fncmbact(1,17)
	resp$(1)=selz$
	fnLbl(3,1,'Last selection: '&lastSelectedAcct$,35,0)
	fnLbl(2,17,sele$(2),20,0)
	fnCmdSet(23)
	ckey=fnAcs(mat resp$)
	if ckey=2 then
		fnCustomerSearch(resp$(1))
		selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
		read #hCustomer,using Fcustomer1,key=selz$: selz$,mat sele$,extra1$,final nokey ignore
		goto SelectIndividualAccounts
	end if
	if ckey=5 then goto Xit
	if ckey=4 or trim$(resp$(1)(1:10))='' then goto Finis
	lastSelectedAcct$=z$=lpad$(rtrm$(resp$(1)(1:10)),10)
	selz$='': mat sele$=('')
	read #hCustomer,using Fcustomer1,key=z$: z$,mat e$,lastBillingDate,f3$,route,seq,extra1$,final nokey SelectIndividualAccounts
	meter_address$=e$(1)
goto PastFilters ! /r
SelectByRoute: ! r: selects by route
	fnTos
	respc=0
	fnLbl(1,1,'Route Number:',15,0)
	fncmbrt2(1,17)
	resp$(respc+=1)='1'
	fnLbl(2,1,'Sequence Number:',15,0)
	fnTxt(2,17,7,7,1,'20',0,'The sequence number is only required if you wish to start in the middle of a route')
	resp$(respc+=1)=''
	fnCmdSet(22)
	ckey=fnAcs(mat resp$) ! select labels by route
	if ckey=5 then goto Xit
	if ckey=2 then goto ScreenFilter
	bk=0 : bk=val(resp$(1)) conv L1860
	seq=val(resp$(2)) conv L1860
	if annbc=sequence_bar_code and filterSelected=filter_route then goto Top ! must start at front of file if bar coded and specific route
	L1860: !
	routekey$=lpad$(str$(bk),2)&lpad$(str$(seq),7)
	restore #hCustomer,key>=routekey$: nokey SelectByRoute
goto Top ! /r
SortBarCode: ! r: hCass1,mat labeltext$,z2$,bc$
	hCass1=fn_openCass(hCass1)
	if file(hCass1)=-1 then goto SbcXit
	labeltext$(5)=''
	read #hCass1,using 'form pos 1,C 10,pos 96,C 12': z2$,bc$ nokey SbcXit
	for j=1 to 4
		labeltext$(j)=labeltext$(j+1) ! move everything up one to allow for BarCode
	next j
	labeltext$(5)=rtrm$(bc$)
	SbcXit: !
return  ! /r
SelectStartingCustomer: ! r: select customer to start with
	fnTos
	mylen=26 : mypos=mylen+2
	fnLbl(1,1,'Starting account:',mylen,0)
	fncmbact(1,mypos,1)
	if trim$(selz$)='' then resp$(1)='[All]' else resp$(1)=selz$
	fnCmdSet(5)
	ckey=fnAcs(mat resp$) ! select starting customer
	if ckey=6 then
		fnCustomerSearch(resp$(1))
		selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
		read #hCustomer,using Fcustomer1,key=selz$: selz$,mat sele$,extra1$,final nokey ignore
		goto SelectStartingCustomer
	end if
	if ckey=5 then goto Xit
	z$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(z$)='[All]' then
		restore #hCustomer:
		goto Top
	end if
	selz$='': mat sele$=('')
	read #hCustomer,using Fcustomer1,key=z$: z$,mat e$,lastBillingDate,f3$,route,seq,extra1$,final nokey SelectStartingCustomer
	meter_address$=e$(1)
goto PastFilters ! /r
def fn_readFromGrid(; ___,returnN) !  select customers from grid
	fnTos
	fnLbl(1,1,'Grid name (including folders):',30,0)
	fnTxt(1,30,30,66,0,'70',0,'You must first export a fixed width file from the gird program (remember the name!)')
	resp$(1)=''
	fnLbl(2,40,'',30,0)
	fnCmdSet(3)
	ckey=fnAcs(mat resp$) ! select starting customer #
	if ckey=5 then
		returnN=0 ! goto Xit
	else
		open #hGrid=fnH: 'Name='&trim$(resp$(1)),display,input ioerr EoReadFromGrid
		do
			NextGridRead: !
			linput #hGrid: line$ eof Finis
			z$=lpad$(trim$(line$(1:10)),10)
			read #hCustomer,using Fcustomer1,key=z$: z$,mat e$,lastBillingDate,f3$,route,seq,extra1$,final nokey NextGridRead
			meter_address$=e$(1)
			fn_getAddressLines
			fn_addLabel(mat labeltext$,mat linePrint,final,filterSelected)
		loop
		EoReadFromGrid: !
		close #hGrid: ioerr ignore
	end if
	returnN=1 ! goto Finis
fnend
def fn_sort(ab; ___ _
	,returnN,hScustomer,hSsequence, _
	z$*10,route,seq,alphaSort$,bulk$*12,finalBillingCode)
	! ab = 1  alpha
	! ab = 2  bulk
	open #hScustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]',i,i,k  ! open in Account order
	open #hSsequence=fnH: 'Name=[Temp]\Temp.[session],Replace,RecL=31',internal,output
	do
		read #hScustomer,using 'form pos 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 354,c 7,pos 1942,c 12,pos 1821,n 1': z$,route,seq,alphaSort$,bulk$,finalBillingCode eof SortFinis
		! if ~finalBillingCode or finalBillingCode=4 then
			if ab=1 then ! alpha sort
				write #hSsequence,using 'form pos 1,C 12,n 2,n 7,c 10': alphaSort$,route,seq,z$
				! sortAddCount+=1
			else if ab=2 then ! bulk sort
				write #hSsequence,using 'form pos 1,C 12,n 2,n 7,c 10': bulk$,route,seq,z$
				! sortAddCount+=1
			end if
		! end if
	loop
	SortFinis: !
	! pr 'sortAddCount=';sortAddCount
	! pause
	close #hScustomer: ioerr ignore
	close #hSsequence: ioerr ignore
	if fnIndex('[Temp]\Temp.[session]','[Temp]\Tempidx.[session]','1,19') then
		! fnStatusClose
		open #returnN=fnH: 'Name=[Temp]\Temp.[session],KFName=[Temp]\Tempidx.[session]',i,i,k
	end if
	fn_sort=returnN
fnend

OpenFiles: ! r:
	close #hCustomer=1: ioerr ignore
	if annbc=sequence_account or annbc=sequence_bar_code or annbc=sequence_grid or annbc=sequence_bulk_sort or annbc=sequence_alphaSort then
		open #hCustomer: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr',i,i,k
	! else if annbc=sequence_name then
	! 	open #hCustomer: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr',i,i,k
	else if annbc=sequence_route then
		open #hCustomer: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr',i,i,k
	end if
	Fcustomer1: form pos 1,C 10,4*C 30,pos 296,PD 4,pos 373,C 12,pos 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1
	Fcustomer2: form pos 1,C 10,4*C 30,C 12,pos 296,PD 4,pos 373,C 12,pos 1741,N 2,N 7,pos 1864,C 30,pos 1942,c 12,pos 1821,n 1
	open #hAdrBil=fnH: 'Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr',i,i,k
	execute 'drop [Temp]\label.dat -n' ioerr ignore
	fnLastBillingDate(d1)
	if annbc=sequence_bar_code and ~SortBarCodesetup then gosub SortBarCodeOpen
return  ! /r
	SortBarCodeOpen: ! r: Select & Sort
		! returns hSort,hAddr, etc
		hCass1=fn_openCass(hCass1)
		open #hSort=fnH: 'Name=[Temp]\Work.[session],Replace,RecL=26',internal,output
		SortBarCodesetup=1
		restore #hCustomer:
		do
			SortBarCodeNextCustomer: !
			read #hCustomer,using 'form pos 1,C 10,pos 296,PD 4,pos 1864,C 30,pos 1821,n 1': z$,lastBillingDate,extra1$,final eof SortBarCodeEoCustomer
			xcr$=bc$=''
			read #hCass1,using 'form pos 96,C 12,C 4': bc$,xcr$ nokey SortBarCodeNextCustomer ioerr SortBarCodeNextCustomer
			write #hSort,using 'form pos 1,C 16,C 10': bc$(1:5)&xcr$&bc$(6:12),z$
		loop
		SortBarCodeEoCustomer: !
		close #hSort: ioerr ignore
		hSort=0
		close #9: ioerr ignore
		open #9: 'Name=[Temp]\Control.[session],Size=0,RecL=128,Replace',internal,output
		write #9,using 'form pos 1,C 128': 'File [Temp]\Work.[session],,,[Temp]\Addr.[session],,,,,A,N'
		write #9,using 'form pos 1,C 128': 'Mask 1,26,C,A'
		close #9:
		execute 'Free [Temp]\Addr.[session] -n' ioerr ignore
		execute 'Sort [Temp]\Control.[session] -n'
		open #hSort=fnH: 'Name=[Temp]\Work.[session]',i,i,r
		open #hAddr=fnH: 'Name=[Temp]\Addr.[session]',i,i,r
	return  ! /r

def fn_openCass(hCass1; ___,returnN)
! OpenCass
	if ~hCass1 or file(hCass1)=-1 then
		open #hCass1=fnH: 'Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr',i,i,k ioerr OcFinis ! formerly #5
	end if
	returnN=hCass1
	OcFinis: !
	fn_openCass=returnN
fnend

def library fncustomer_address(z$*10,mat addr$; ca_address_type,ca_closeFiles)
	if ~setup then fn_setup
	fncustomer_address=fn_customerAddress(z$,mat addr$, ca_address_type,ca_closeFiles)
fnend
	def fn_customerAddress(z$*10,mat addr$; ca_address_type,ca_closeFiles,___,extra_22)
		if ~ca_setup then
			ca_setup=1
			open #h_ca_customer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]'&',Shr',i,i,k
			open #hAdrBil=fnH: 'Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr',i,i,k
		end if
		if ca_address_type=0 then ca_address_type=ao_billing
		altaddr=ca_address_type
		dim pe$(4)*30
		mat pe$=('')
		z$=lpad$(trim$(z$),kln(h_ca_customer))
		read #h_ca_customer,using 'form pos 11,4*C 30,pos 1864,C 30,pos 1854,PD 5.2',key=lpad$(z$,kln(h_ca_customer)): mat e$,extra1$,extra_22 nokey CA_FINIS
		fn_getAddressLines
		CA_FINIS: !
		if trim$(pe$(2))='' then pe$(2)=pe$(3): pe$(3)=''
		if trim$(pe$(3))='' then pe$(3)=pe$(4): pe$(4)=''
		mat addr$(4)
		mat addr$=pe$
		if ca_closeFiles then
			ca_setup=0
			close #h_ca_customer:
			h_ca_customer=0
			close #hAdrBil:
			hAdrBil=0
		end if
	fnend
		def fn_getAddressLines
			if altaddr=ao_alternate then
				goto Gal_useAlternate
			else if altaddr=ao_primary then
				goto Gal_usePrime
			else !   (default)   if altaddr=ao_billing then
				if hCustomer then read #hCustomer,using 'form pos 1854,PD 5.2',key=z$: extra_22 ! else it is called from the library function fncustomer_address, which already read it
				if extra_22=0 or extra_22=2 then
					do_not_use_alt_addr=1
				else
					do_not_use_alt_addr=0
				end if
				if do_not_use_alt_addr then
					goto Gal_usePrime
				else
					goto Gal_useAlternate
				end if
			end if
			goto Gal_xit
			Gal_useAlternate: ! r:
				dim ba$(4)*30
				read #hAdrBil,using 'form pos 11,4*C 30',key=z$: mat ba$ nokey Gal_usePrime
				fn_alternate_address(mat pe$,mat e$)
			goto Gal_xit ! /r
			Gal_usePrime: ! r:
				fn_primary_address(mat pe$,mat e$,extra1$)
			goto Gal_xit ! /r
			Gal_xit: !
		fnend
			def fn_alternate_address(mat pe$,mat e$; ___,j,e1)
				mat pe$=('')
				for j=1 to 4
					if rtrm$(ba$(j))<>'' then pe$(e1+=1)=ba$(j)
				next j
			fnend
			def fn_primary_address(mat pe$,mat e$,extra1$*30; ___,j,e1)
				mat pe$=('')
				for j=2 to 4
					if rtrm$(e$(j))<>'' then pe$(e1+=1)=e$(j)
				next j
				if trim$(extra1$)<>'' then pe$(4)=pe$(3) : pe$(3)=extra1$
			fnend


def fn_setup
	autoLibrary
	on error goto Ertn
! r: constants and dims
	on fkey 5 goto Finis
	dim resp$(10)*80
	dim labeltext$(5)*120,line$*512,z$*50

	dim srvName$(10)*20,srv$(10)*2
	fnGetServices(mat srvName$, mat srv$)

	filter_none      	=1
	filter_lastMonth 	=2
	filter_unbilled  	=3
	filter_range     	=4
	filter_select    	=5
	filter_route     	=6
	filter_active    	=7


	dim optLineX$(9)*70
	mat optLineX$(0)
	line_x_blank                    	=fnAddoneC(mat optLineX$,'(blank)'                                                        )
	line_x_account_number          	=fnAddoneC(mat optLineX$,'Account'                                                        )
	line_x_accountRightJustified  	=fnAddoneC(mat optLineX$,'Account (right justified)'                                      )
	line_x_meter_address           	=fnAddoneC(mat optLineX$,'Meter Address'                                                  )
	line_x_customer_name           	=fnAddoneC(mat optLineX$,'Name'                                                           )
	line_x_mailing_address_line_1 	=fnAddoneC(mat optLineX$,'Mailing Address Line 1'                                         )
	line_x_mailing_address_line_2 	=fnAddoneC(mat optLineX$,'Mailing Address Line 2 or if blank City State Zip'              )
	line_x_mailing_address_line_3 	=fnAddoneC(mat optLineX$,'City State Zip if Mailing Address Line 2 not blank'             )
	line_x_meter_route_and_sequenc	=fnAddoneC(mat optLineX$,trim$(srvName$(4))&' Meter, Route and Sequence Numbers'          )
	line_x_account_meter4_and_seq 	=fnAddoneC(mat optLineX$,'Account, '&trim$(srvName$(4))&' Meter, and Sequence Numbers'    )
	line_x_meterAndSequence        	=fnAddoneC(mat optLineX$,trim$(srvName$(4))&' Meter and Sequence Numbers'                 )

	dim optAddress$(3)*30
	optAddress$(ao_primary  :=1)='Primary Address'
	optAddress$(ao_alternate:=2)='Alternate Billing Address'
	optAddress$(ao_billing  :=3)='Billing Address'

	dim optSequence$(6)*22
	sequenceCount=0
	optSequence$(sequence_account  :=sequenceCount+=1)='Account'
	! optSequence$(sequence_name:=2)='Customer Name'
	optSequence$(sequence_bar_code :=sequenceCount+=1)='Bar Code'
	optSequence$(sequence_route    :=sequenceCount+=1)='Route'
	optSequence$(sequence_grid     :=sequenceCount+=1)='Grid'
	optSequence$(sequence_bulk_sort:=sequenceCount+=1)='Bulk Sort Code'
	optSequence$(sequence_alphaSort:=sequenceCount+=1)='AlphaSort'


! /r
fnend
include: ertn