! -- pr Customer Labels
fn_setup
fnTop(program$)
Screen1: ! r:
	fnTos
	fnLbl(1,1,"Sort by:",20,1)
	fncomboa("ublabels-ord",1,22,mat optSequence$,"The labels can be printed in customer # order,customer name order, or in bar code sequence")
	fnreg_read('ublabel.sequence',resp$(1),optSequence$(sequence_account),1)

	fnLbl(3,1,"Print Address:",20,1)
	fncomboa("ubnamlst-act",3,22,mat optAddress$)
	fnreg_read('ublabel.address',resp$(2), optAddress$(ao_billing),1)

	fnLbl(5,1,"Line 1:",20,1)
	fncomboa("ublabel_tl1",5,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 1',resp$(3), optLineX$(1),1)

	fnLbl(6,1,"Line 2:",20,1)
	fncomboa("ublabel_tl2",6,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 2',resp$(4), optLineX$(2),1)

	fnLbl(7,1,"Line 3:",20,1)
	fncomboa("ublabel_tl3",7,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 3',resp$(5), optLineX$(3),1)

	fnLbl(8,1,"Line 4:",20,1)
	fncomboa("ublabel_tl4",8,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 4',resp$(6), optLineX$(4),1)

	fnLbl(9,1,"Line 5:",20,1)
	fncomboa("ublabel_tl5",9,22,mat optLineX$,'',70)
	fnreg_read('ublabel.line 5',resp$(7), optLineX$(5),1)

	fnCmdSet(2)
	ckey=fnAcs(mat resp$) ! select order of printing
	if ckey=5 then goto Xit
	fnreg_write('ublabel.sequence',resp$(1))
	fnreg_write('ublabel.address',resp$(2))
	fnreg_write('ublabel.line 1',resp$(3))
	fnreg_write('ublabel.line 2',resp$(4))
	fnreg_write('ublabel.line 3',resp$(5))
	fnreg_write('ublabel.line 4',resp$(6))
	fnreg_write('ublabel.line 5',resp$(7))
	! annbc=sequence_name ! default to name sequence
	pt$(5)=""
	if resp$(1)=optSequence$(sequence_account) then
		annbc=sequence_account
	! else if resp$(1)=optSequence$(sequence_name) then
	! 	annbc=sequence_name
	else if resp$(1)=optSequence$(sequence_bar_code) then
		annbc=sequence_bar_code
		pt$(5)="BAR"
	else if resp$(1)=optSequence$(sequence_route) then
		annbc=sequence_route
	else if resp$(1)=optSequence$(sequence_grid) then
		annbc=sequence_grid
	else if resp$(1)=optSequence$(sequence_bulk_sort) then
		annbc=sequence_bulk_sort
	end if

	altaddr=srch(mat optAddress$,resp$(2))

	line_1_print=srch(mat optLineX$,resp$(3))
	lineToPrint=srch(mat optLineX$,resp$(4))
	line_3_print=srch(mat optLineX$,resp$(5))
	line_4_print=srch(mat optLineX$,resp$(6))
	line_5_print=srch(mat optLineX$,resp$(7))

	if annbc=sequence_route then
		filter_selection=6
		gosub OpenFiles
		goto SelectByRoute ! ROUTE # SEQUENCE
	else if annbc=sequence_grid then
		gosub OpenFiles
		goto ReadFromGrid
	else if annbc=sequence_bulk_sort then
		gosub BulkSort
		gosub OpenFiles
		goto BulkRead
	end if
Screen2: !
	fnTos
	fnLbl(1,1,"Select by:",12,0,0)
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
	fncomboa("ublabels-ord",1,14,mat optFilterEnabled$,'',30)
	resp$(1)=optFilterEnabled$(1)
	gosub OpenFiles
	fnCmdSet(6)
	ckey=fnAcs(mat resp$) ! method of selection
	filter_selection=srch(mat optFilter$,resp$(1))
	if ckey=5 then goto Xit
	if ckey=2 then goto Screen1
	if annbc=sequence_route then goto SelectByRoute ! select by route
	if filter_selection=4 then goto SelectRangeOfAccounts ! select range of customers
	if filter_selection=5 then goto SelectIndividualAccounts ! select specific accounts
	if filter_selection=6 and annbc=sequence_bar_code then goto SelectByRoute
	if (filter_selection=1 or filter_selection=2 or filter_selection=3 or filter_selection=7) and annbc=sequence_account then goto SelectStartingCustomer
	! if (filter_selection=1 or filter_selection=7) and annbc=sequence_name then goto SCR4F3 ! all customers in name sequence
goto Top ! /r

Top: ! r:
	if annbc=sequence_bar_code then
		BARCODE_READ_ADDR: !
		read #addr,using 'Form POS 1,PD 3': r6 eof Finis
		read #6,using "Form POS 1,C 16,C 10",rec=r6: srt$,z$ noRec Top
		if rtrm$(x$)<>"" and x$<>z$ then goto BARCODE_READ_ADDR
		x$=""
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey Top
		meter_address$=e$(1)
		if annbc=sequence_bar_code and filter_selection=6 and bk>0 and bk<>route then goto BARCODE_READ_ADDR ! skip if barcoded and by route, but not right route
	else if annbc=sequence_bulk_sort then
		BulkRead: !
		read #6,using 'form pos 22,c 10': z$ eof Finis
		if rtrm$(x$)<>"" and x$<>z$ then goto BulkRead
		x$=""
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1942,c 12,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),bulksort$,final nokey Top
		meter_address$=e$(1)
		if annbc=sequence_bulk_sort and filter_selection=6 and bk>0 and bk<>route then goto BulkRead ! skip if barcoded and by route, but not right route
	else
		read #customer,using 'Form POS 1,C 10,4*C 30,C 12,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1': z$,mat e$,f$(1),f,f3$,route,seq,extra$(1),final eof Finis
		meter_address$=e$(1)
	end if
	PAST_READ: !
	if filter_selection=3 and d1=f then goto Top
	if filter_selection=4 and z$>h1$ then goto Finis
	if filter_selection=6 and bk>0 and route<>bk then goto Top
	if filter_selection=2 and d1><f then goto Top
	THERE: !
	fn_getAddressLines
goto AddLabel ! /r
AddLabel: ! r:
! if annbc=sequence_bulk_sort then
!   labeltext$(1)=labeltext$(1)&"  "&bulksort$ ! if bulk sort than auto add bulk sort code on to the end of the Top line
! else
	fn_set_line_text(labeltext$(1),line_1_print)
! end if
	fn_set_line_text(labeltext$(2),lineToPrint)
	fn_set_line_text(labeltext$(3),line_3_print)
	fn_set_line_text(labeltext$(4),line_4_print)
	fn_set_line_text(labeltext$(5),line_5_print)
	if annbc=sequence_bar_code then
		gosub BarCode
	end if
	if final=0 or filter_selection<>7 then
		fnaddlabel(mat labeltext$)
	end if
	if annbc=sequence_grid then return
	if filter_selection=5 then goto SelectIndividualAccounts
	goto Top
! /r
Finis: ! r:
	close #1: ioerr ignore
	fnlabel(mat pt$)
goto Xit ! /r
def fn_set_line_text(&labeltext$,lineToPrint)
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
Xit: fnXit
SelectRangeOfAccounts: ! r: select range of accounts
	fnTos
	fnLbl(1,1,"Starting Account:",20)
	fncmbact(1,22,1)
	resp$(1)=l1$
	fnLbl(2,1,"Ending Account:",20)
	fncmbact(2,22,1)
	resp$(2)=h1$
	fnCmdSet(22)
	ckey=fnAcs(mat resp$) ! select by range
	if ckey=5 then goto Xit
	l1$=lpad$(trim$(resp$(1)(1:10)),10)
	h1$=lpad$(trim$(resp$(2)(1:10)),10)
	if ckey=6 then fnCustomerSearch(resp$(1)) else goto L1470
	if trim$(l1$)="" then l1$=resp$(1) : goto SelectRangeOfAccounts
	if trim$(h1$)="" then h1$=resp$(1) : goto SelectRangeOfAccounts
	goto SelectRangeOfAccounts
	L1470: !
	if ckey=2 then goto Screen2
	l1$=lpad$(l1$,10) : h1$=lpad$(h1$,10)
	if h1$<l1$ then
		mat msgline$(1)
		msgline$(1)="You have entered invalid accounts!"
		fnmsgbox(mat msgline$,resp$,'',48)
		goto SelectRangeOfAccounts
	end if
	restore #customer,key=l1$: nokey SelectRangeOfAccounts
goto Top ! /r
SelectIndividualAccounts: ! r: select individual accounts
	fnTos
	fnLbl(1,1,"Account:",15)
	fncmbact(1,17)
	resp$(1)=selz$
	fnLbl(3,1,"Last selection: "&hz$,35,0)
	if trim$(sele$(2))<>"" then fnLbl(2,17,sele$(2),20,0)
	fnCmdSet(23)
	ckey=fnAcs(mat resp$)
	if ckey=2 then
		fnCustomerSearch(resp$(1))
		selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=selz$: selz$,mat sele$,extra$(1),final nokey ignore
		goto SelectIndividualAccounts
	end if
	if ckey=5 then goto Xit
! if ckey=1 then goto L1660
	if ckey=4 then goto Finis
! L1660: !
	z$=lpad$(rtrm$(resp$(1)(1:10)),10)
	hz$=z$
	if rtrm$(z$)="" then goto Finis
	selz$="": mat sele$=("")
	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey SelectIndividualAccounts
	meter_address$=e$(1)
goto THERE ! /r
SelectByRoute: ! r: selects by route
	fnTos
	respc=0
	fnLbl(1,1,"Route Number:",15,0)
	fncmbrt2(1,17)
	resp$(respc+=1)="1"
	fnLbl(2,1,"Sequence Number:",15,0)
	fnTxt(2,17,7,7,1,"20",0,"The sequence number is only required if you wish to start in the middle of a route")
	resp$(respc+=1)=""
	fnCmdSet(22)
	ckey=fnAcs(mat resp$) ! select labels by route
	if ckey=5 then goto Xit
	if ckey=2 then goto Screen2
	bk=0 : bk=val(resp$(1)) conv L1860
	seq=val(resp$(2)) conv L1860
	if annbc=sequence_bar_code and filter_selection=6 then goto Top ! must start at front of file if bar coded and specific route
	L1860: !
	routekey$=lpad$(str$(bk),2)&lpad$(str$(seq),7)
	restore #customer,key>=routekey$: nokey SelectByRoute
goto Top ! /r
! SCR4F3: ! r: select starting account name
! 	fnTos
! 	fnLbl(1,1,"Customer Name:",15,0)
! 	fnTxt(1,17,30,30,1,"",0,"Search and find the exact customer name if you wish to start with a specific customer")
! 	resp$(1)=""
! 	if trim$(sele$(2))<>"" then
! 		fnLbl(2,17,sele$(2),20,0)
! 	end if
! 	fnCmdSet(21)
! 	ckey=fnAcs(mat resp$) ! select starting customer name
! 	if ckey=5 then goto Xit
! 	if ckey=6 then fnCustomerSearch(resp$(1))
! 	restore #customer,key>="       ": nokey ignore
! 	SCR4F3_READ_CUSTOMER: !
! 	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1': z$,mat e$,f,f3$,route,seq,extra$(1),final eof SCR4F3_NO_MATCH
! 	meter_address$=e$(1)
! 	if trim$(resp$(1))="" then goto SCR4F3_FINIS
! 	if lpad$(resp$(1),10)<>z$ then goto SCR4F3_READ_CUSTOMER
! 	SCR4F3_FINIS: !
! goto PAST_READ ! 
! SCR4F3_NO_MATCH: ! r:
! 	mat msgline$(1)
! 	msgline$(1)="No matching name found!"
! 	fnmsgbox(mat msgline$,resp$,'',48)
! goto SelectStartingCustomer ! /r
! ! /r
Sort1: ! r: SELECT & SORT
	gosub OpenCass
	close #6: ioerr ignore
	open #6: "Name=[Temp]\Work.[session],Replace,RecL=26",internal,output
	s5=1
	restore #1:
	do
		read #customer,using "Form POS 1,C 10,POS 296,PD 4,pos 1864,C 30,pos 1821,n 1": z$,f,extra$(1),final eof END5
		cr$=bc$=""
		read #5,using "Form POS 96,C 12,C 4": bc$,cr$ nokey SORT1_NEXT ioerr SORT1_NEXT
		write #6,using "Form POS 1,C 16,C 10": bc$(1:5)&cr$&bc$(6:12),z$
		SORT1_NEXT: !
	loop
	END5: !
	close #6: ioerr ignore
	close #9: ioerr ignore
	open #9: "Name=[Temp]\CONTROL.[session],Size=0,RecL=128,Replace",internal,output
	write #9,using 'Form POS 1,C 128': "File [Temp]\Work.[session],,,[Temp]\Addr.[session],,,,,A,N"
	write #9,using 'Form POS 1,C 128': "Mask 1,26,C,A"
	close #9:
	execute "Free [Temp]\Addr.[session] -n" ioerr ignore
	execute "Sort [Temp]\CONTROL.[session] -n"
	open #6: "Name=[Temp]\Work.[session]",internal,input,relative
	close #addr: ioerr ignore
	open #addr=7: "Name=[Temp]\Addr.[session]",internal,input,relative
return  ! /r
BarCode: ! r:
	gosub OpenCass
	if file(5)=-1 then goto BARCODE_XIT
	labeltext$(5)=""
	read #5,using 'Form POS 1,C 10,POS 96,C 12': z2$,bc$ nokey BARCODE_XIT
	for j=1 to 4
		labeltext$(j)=labeltext$(j+1) ! move everything up one to allow for BarCode
	next j
	labeltext$(5)=rtrm$(bc$)
	BARCODE_XIT: !
return  ! /r
SelectStartingCustomer: ! r: select customer to start with
	fnTos
	mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting account:",mylen,0)
	fncmbact(1,mypos,1)
	if trim$(selz$)='' then resp$(1)='[All]' else resp$(1)=selz$
	fnCmdSet(5)
	ckey=fnAcs(mat resp$) ! select starting customer
	if ckey=6 then
		fnCustomerSearch(resp$(1))
		selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=selz$: selz$,mat sele$,extra$(1),final nokey ignore
		goto SelectStartingCustomer
	end if
	if ckey=5 then goto Xit
	z$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(z$)="[All]" then restore #1: : goto Top
	selz$="": mat sele$=("")
	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey SelectStartingCustomer
	meter_address$=e$(1)
goto THERE ! /r
ReadFromGrid: ! r: select customers from grid
	fnTos
	fnLbl(1,1,"Grid name (including folders):",30,0)
	fnTxt(1,30,30,66,0,"70",0,"You must first export a fixed width file from the gird program (remember the name!)")
	resp$(1)=""
	fnLbl(2,40,"",30,0)
	fnCmdSet(3)
	ckey=fnAcs(mat resp$) ! select starting customer #
	if ckey=5 then goto Xit
	open #6: "Name="&trim$(resp$(1)),display,input ioerr ReadFromGrid
	do
		LIN6: !
		linput #6: x$ eof Finis
		z$=lpad$(trim$(x$(1:10)),10)
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey LIN6
		meter_address$=e$(1)
		fn_getAddressLines
		gosub AddLabel
	loop 
! /r (eof Finis)
def library fncustomer_address(z$*10,mat addr$; ca_address_type,ca_closeFiles)
	if ~setup then fn_setup
	fncustomer_address=fn_customerAddress(z$,mat addr$, ca_address_type,ca_closeFiles)
fnend
def fn_customerAddress(z$*10,mat addr$; ca_address_type,ca_closeFiles)
	if ~ca_setup then
		ca_setup=1
		open #h_ca_customer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]"&',Shr',internal,input,keyed
		open #adrbil=fnH: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed
	end if
	if ca_address_type=0 then ca_address_type=ao_billing
	altaddr=ca_address_type
	mat pe$=('')
	z$=lpad$(trim$(z$),kln(h_ca_customer))
	read #h_ca_customer,using 'Form POS 11,4*C 30,pos 1864,C 30,pos 1854,PD 5.2',key=lpad$(z$,kln(h_ca_customer)): mat e$,extra$(1),extra(22) nokey CA_FINIS
	fn_getAddressLines
	CA_FINIS: !
	if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
	if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
	mat addr$(4)
	mat addr$=pe$
	if ca_closeFiles then
		ca_setup=0
		close #h_ca_customer:
		h_ca_customer=0
		close #adrbil:
		adrbil=0
	end if
fnend
def fn_getAddressLines
	if altaddr=ao_alternate then
		goto GAL_USE_ALT_ADDR
	else if altaddr=ao_primary then
		goto GAL_USE_PRIME_ADR
	else !   (default)   if altaddr=ao_billing then
		if customer then read #customer,using 'Form pos 1854,PD 5.2',key=z$: extra(22) ! else it is called from the library function fncustomer_address, which already read it
		if extra(22)=0 or extra(22)=2 then
			do_not_use_alt_addr=1
		else
			do_not_use_alt_addr=0
		end if
		if do_not_use_alt_addr then
			goto GAL_USE_PRIME_ADR
		else
			goto GAL_USE_ALT_ADDR
		end if
	end if
	goto GAL_XIT
	GAL_USE_ALT_ADDR: ! r:
	read #adrbil,using "Form POS 11,4*C 30",key=z$: mat ba$ nokey GAL_USE_PRIME_ADR
	fn_alternate_address
	goto GAL_XIT ! /r
	GAL_USE_PRIME_ADR: ! r:
	fn_primary_address
	goto GAL_XIT ! /r
	GAL_XIT: !
fnend
def fn_alternate_address
	e1=0
	mat pe$=("")
	for j=1 to 4
		if rtrm$(ba$(j))<>"" then pe$(e1+=1)=ba$(j)
	next j
fnend
def fn_primary_address
	e1=0 : mat pe$=("")
	for j=2 to 4
		if rtrm$(e$(j))<>"" then pe$(e1+=1)=e$(j)
	next j
	if trim$(extra$(1))<>'' then pe$(4)=pe$(3) : pe$(3)=extra$(1)
fnend
BulkSort: ! r: bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]",internal,input,keyed  ! open in Account order
	open #6: "Name=[Temp]\Temp.[session],Replace,RecL=31",internal,output
	do
		read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof BULKSORT_FINIS
		write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	loop
	BULKSORT_FINIS: !
	close #1: ioerr ignore
	close #6: ioerr ignore
	execute "Index [Temp]\Temp.[session] [Temp]\Tempidx.[wsid] 1,19,Replace,DupKeys -n" ioerr BULKSORT_XIT
	open #6: "Name=[Temp]\Temp.[session],KFName=[Temp]\Tempidx.[wsid]",internal,input,keyed
	BULKSORT_XIT: !
return  ! /r
OpenFiles: ! r:
	close #customer=1: ioerr ignore
	if annbc=sequence_account or annbc=sequence_bar_code or annbc=sequence_grid or annbc=sequence_bulk_sort then
		open #customer: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed
	! else if annbc=sequence_name then
	! 	open #customer: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\UBIndx2.h[cno],Shr",internal,input,keyed
	else if annbc=sequence_route then
		open #customer: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed
	end if
	close #adrbil=3: ioerr ignore
	open #adrbil: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed
	execute "drop [Temp]\label.dat -n" ioerr ignore
	fnLastBillingDate(d1)
	if annbc=sequence_bar_code and s5=0 then gosub Sort1
return  ! /r
OpenCass: ! r:
	if file(5)=-1 then
		open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr ignore
	end if
return  ! /r
def fn_setup
	autoLibrary
	on error goto Ertn
! r: constants and dims
	on fkey 5 goto Finis
	dim e$(4)*30
	dim meter_address$*30
	dim pe$(4)*30,ba$(4)*30
	dim resp$(10)*80
	dim labeltext$(5)*120,x$*512,z$*50
	dim extra$(11)*30,extra(23) ! fields from Customer File
	dim srvName$(10)*20,srv$(10)*2

	fnGetServices(mat srvName$, mat srv$)

	dim optLineX$(9)*70
	mat optLineX$(0)
	line_x_blank                   =fnAddoneC(mat optLineX$,"(blank)"                                                        )
	line_x_account_number          =fnAddoneC(mat optLineX$,"Account"                                                        )
	line_x_accountRightJustified   =fnAddoneC(mat optLineX$,"Account (right justified)"                                      )
	line_x_meter_address           =fnAddoneC(mat optLineX$,"Meter Address"                                                  )
	line_x_customer_name           =fnAddoneC(mat optLineX$,"Name"                                                           )
	line_x_mailing_address_line_1  =fnAddoneC(mat optLineX$,"Mailing Address Line 1"                                         )
	line_x_mailing_address_line_2  =fnAddoneC(mat optLineX$,"Mailing Address Line 2 or if blank City State Zip"              )
	line_x_mailing_address_line_3  =fnAddoneC(mat optLineX$,"City State Zip if Mailing Address Line 2 not blank"             )
	line_x_meter_route_and_sequenc =fnAddoneC(mat optLineX$,trim$(srvName$(4))&" Meter, Route and Sequence Numbers"          )
	line_x_account_meter4_and_seq  =fnAddoneC(mat optLineX$,"Account, "&trim$(srvName$(4))&" Meter, and Sequence Numbers"    )
	line_x_meterAndSequence        =fnAddoneC(mat optLineX$,trim$(srvName$(4))&" Meter and Sequence Numbers"                 )

	dim optAddress$(3)*30
	optAddress$(ao_primary  :=1)="Primary Address"
	optAddress$(ao_alternate:=2)="Alternate Billing Address"
	optAddress$(ao_billing  :=3)="Billing Address"

	dim optSequence$(6)*22
	sequenceCount=0
	optSequence$(sequence_account  :=sequenceCount+=1)="Account"
	! optSequence$(sequence_name:=2)="Customer Name"
	optSequence$(sequence_bar_code :=sequenceCount+=1)="Bar Code"
	optSequence$(sequence_route    :=sequenceCount+=1)="Route"
	optSequence$(sequence_grid     :=sequenceCount+=1)="Grid"
	optSequence$(sequence_bulk_sort:=sequenceCount+=1)="Bulk Sort Code"

	dim optFilter$(7)*50
	optFilter$(1)="[All]"
	optFilter$(2)="Customers billed last month"
	optFilter$(3)="Customers not billed last billing"
	optFilter$(4)="Range of Accounts"
	optFilter$(5)="Individual accounts"
	optFilter$(6)="Route"
	optFilter$(7)="Active Customers"
! /r
fnend
include: ertn