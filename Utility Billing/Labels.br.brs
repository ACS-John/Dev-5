! -- pr Customer Labels
fn_setup
	fntop(program$)
SCR1: ! r:
	fnTos(sn$="ublabel-1b")
	fnLbl(1,1,"Sort by:",20,1)
	fncomboa("ublabels-ord",1,22,mat sequence_option$,"The labels can be printed in customer # order,customer name order, or in bar code sequence")
	fnreg_read('ublabel.sequence',resp$(1)) : if resp$(1)='' then resp$(1)=sequence_option$(sequence_account)
! 
	fnLbl(3,1,"Print Address:",20,1)
	fncomboa("ubnamlst-act",3,22,mat address_option$)
	fnreg_read('ublabel.address',resp$(2)) : if srch(mat address_option$,resp$(2))<=0 then resp$(2)=address_option$(ao_billing)
! 
	fnLbl(5,1,"Line 1:",20,1)
	fncomboa("ublabel_tl1",5,22,mat line_x_option$,'',70)
	fnreg_read('ublabel.line 1',resp$(3)) : if resp$(3)='' then resp$(3)=line_x_option$(1)
! 
	fnLbl(6,1,"Line 2:",20,1)
	fncomboa("ublabel_tl2",6,22,mat line_x_option$,'',70)
	fnreg_read('ublabel.line 2',resp$(4)) : if resp$(4)='' then resp$(4)=line_x_option$(2)
! 
	fnLbl(7,1,"Line 3:",20,1)
	fncomboa("ublabel_tl3",7,22,mat line_x_option$,'',70)
	fnreg_read('ublabel.line 3',resp$(5)) : if resp$(5)='' then resp$(5)=line_x_option$(3)
! 
	fnLbl(8,1,"Line 4:",20,1)
	fncomboa("ublabel_tl4",8,22,mat line_x_option$,'',70)
	fnreg_read('ublabel.line 4',resp$(6)) : if resp$(6)='' then resp$(6)=line_x_option$(4)
! 
	fnLbl(9,1,"Line 5:",20,1)
	fncomboa("ublabel_tl5",9,22,mat line_x_option$,'',70)
	fnreg_read('ublabel.line 5',resp$(7)) : if resp$(7)='' then resp$(7)=line_x_option$(5 )
! 
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ck) ! select order of printing
	if ck=5 then goto XIT
	fnreg_write('ublabel.sequence',resp$(1))
	fnreg_write('ublabel.address',resp$(2))
	fnreg_write('ublabel.line 1',resp$(3))
	fnreg_write('ublabel.line 2',resp$(4))
	fnreg_write('ublabel.line 3',resp$(5))
	fnreg_write('ublabel.line 4',resp$(6))
	fnreg_write('ublabel.line 5',resp$(7))
	! annbc=sequence_name ! default to name sequence
	pt$(5)=""
	if resp$(1)=sequence_option$(sequence_account) then 
		annbc=sequence_account
	! else if resp$(1)=sequence_option$(sequence_name) then 
	! 	annbc=sequence_name
	else if resp$(1)=sequence_option$(sequence_bar_code) then 
		annbc=sequence_bar_code
		pt$(5)="BAR"
	else if resp$(1)=sequence_option$(sequence_route) then 
		annbc=sequence_route
	else if resp$(1)=sequence_option$(sequence_grid) then 
		annbc=sequence_grid
	else if resp$(1)=sequence_option$(sequence_bulk_sort) then 
		annbc=sequence_bulk_sort
	end if 
! 
	altaddr=srch(mat address_option$,resp$(2))
! 
	line_1_print=srch(mat line_x_option$,resp$(3))
	lineToPrint=srch(mat line_x_option$,resp$(4))
	line_3_print=srch(mat line_x_option$,resp$(5))
	line_4_print=srch(mat line_x_option$,resp$(6))
	line_5_print=srch(mat line_x_option$,resp$(7))
! 
	if annbc=sequence_route then 
		filter_selection=6
		gosub OPEN_FILES
		goto SELBK ! ROUTE # SEQUENCE
	else if annbc=sequence_grid then 
		gosub OPEN_FILES
		goto READ_FROM_GRID
	else if annbc=sequence_bulk_sort then 
		gosub BULKSORT
		gosub OPEN_FILES
		goto BULK_READ
	end if 
SCR2: ! 
	fnTos(sn$="ublabel-2")
	fnLbl(1,1,"Select by:",12,0,0)
	if annbc=sequence_account then 
		filter_option_enabled$(1)=filter_option$(1)
		filter_option_enabled$(2)=filter_option$(2)
		filter_option_enabled$(3)=filter_option$(3)
		filter_option_enabled$(4)=filter_option$(4)
		filter_option_enabled$(5)=filter_option$(5)
		filter_option_enabled$(6)=filter_option$(6)
		filter_option_enabled$(7)=filter_option$(7)
		mat filter_option_enabled$(7)
	! else if annbc=sequence_name then 
	! 	filter_option_enabled$(1)=filter_option$(1)
	! 	filter_option_enabled$(2)=filter_option$(2)
	! 	filter_option_enabled$(3)=filter_option$(3)
	! 	filter_option_enabled$(4)=filter_option$(5)
	! 	filter_option_enabled$(5)=filter_option$(7)
	! 	mat filter_option_enabled$(5)
	else if annbc=sequence_bar_code then 
		filter_option_enabled$(1)=filter_option$(1)
		filter_option_enabled$(2)=filter_option$(2)
		filter_option_enabled$(3)=filter_option$(3)
		filter_option_enabled$(4)=filter_option$(5)
		filter_option_enabled$(5)=filter_option$(6)
		mat filter_option_enabled$(5)
	else if annbc=sequence_route then 
		filter_option_enabled$(1)=filter_option$(1)
		filter_option_enabled$(2)=filter_option$(2)
		filter_option_enabled$(3)=filter_option$(3)
		filter_option_enabled$(4)=filter_option$(6)
		mat filter_option_enabled$(4)
	end if 
	fncomboa("ublabels-ord",1,14,mat filter_option_enabled$,'',30)
	resp$(1)=filter_option_enabled$(1)
	gosub OPEN_FILES
	fnCmdSet(6)
	fnAcs(sn$,0,mat resp$,ckey) ! method of selection
	filter_selection=srch(mat filter_option$,resp$(1))
	if ckey=5 then goto XIT
	if ckey=2 then goto SCR1
	if annbc=sequence_route then goto SELBK ! select by route
	if filter_selection=4 then goto SELR ! select range of customers
	if filter_selection=5 then goto IACC ! select specific accounts
	if filter_selection=6 and annbc=sequence_bar_code then goto SELBK
	if (filter_selection=1 or filter_selection=2 or filter_selection=3 or filter_selection=7) and annbc=sequence_account then goto SELSTART
	! if (filter_selection=1 or filter_selection=7) and annbc=sequence_name then goto SCR4F3 ! all customers in name sequence
	goto TOP ! /r
!___
TOP: ! r:
	if annbc=sequence_bar_code then 
BARCODE_READ_ADDR: ! 
		read #addr,using 'Form POS 1,PD 3': r6 eof DONE
		read #6,using "Form POS 1,C 16,C 10",rec=r6: srt$,z$ noRec TOP
		if rtrm$(x$)<>"" and x$<>z$ then goto BARCODE_READ_ADDR
		x$=""
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey TOP
		meter_address$=e$(1)
		if annbc=sequence_bar_code and filter_selection=6 and bk>0 and bk<>route then goto BARCODE_READ_ADDR ! skip if barcoded and by route, but not right route
	else if annbc=sequence_bulk_sort then 
BULK_READ: ! 
		read #6,using 'form pos 22,c 10': z$ eof DONE
		if rtrm$(x$)<>"" and x$<>z$ then goto BULK_READ
		x$=""
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1942,c 12,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),bulksort$,final nokey TOP
		meter_address$=e$(1)
		if annbc=sequence_bulk_sort and filter_selection=6 and bk>0 and bk<>route then goto BULK_READ ! skip if barcoded and by route, but not right route
	else 
		read #customer,using 'Form POS 1,C 10,4*C 30,C 12,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1': z$,mat e$,f$(1),f,f3$,route,seq,extra$(1),final eof DONE
		meter_address$=e$(1)
	end if 
PAST_READ: ! 
	if filter_selection=3 and d1=f then goto TOP
	if filter_selection=4 and z$>h1$ then goto DONE
	if filter_selection=6 and bk>0 and route<>bk then goto TOP
	if filter_selection=2 and d1><f then goto TOP
THERE: ! 
	fn_get_address_lines
	goto ADDLABEL ! /r
ADDLABEL: ! r:
! if annbc=sequence_bulk_sort then
!   labeltext$(1)=labeltext$(1)&"  "&bulksort$ ! if bulk sort than auto add bulk sort code on to the end of the top line
! else 
	fn_set_line_text(labeltext$(1),line_1_print)
! end if
	fn_set_line_text(labeltext$(2),lineToPrint)
	fn_set_line_text(labeltext$(3),line_3_print)
	fn_set_line_text(labeltext$(4),line_4_print)
	fn_set_line_text(labeltext$(5),line_5_print)
	if annbc=sequence_bar_code then 
		gosub BARCODE
	end if 
	if final=0 or filter_selection<>7 then 
		fnaddlabel(mat labeltext$)
	end if 
	if annbc=sequence_grid then return 
	if filter_selection=5 then goto IACC
	goto TOP
! /r
DONE: ! r:
	close #1: ioerr ignore
	fnlabel(mat pt$)
	goto XIT ! /r
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
XIT: fnxit
SELR: ! r: select range of accounts
	fnTos(sn$="ublabel-3")
	fnLbl(1,1,"Starting Account:",20)
	fncmbact(1,22,1)
	resp$(1)=l1$
	fnLbl(2,1,"Ending Account:",20)
	fncmbact(2,22,1)
	resp$(2)=h1$
	fnCmdSet(22)
	fnAcs(sn$,0,mat resp$,ckey) ! select by range
	if ckey=5 then goto XIT
	l1$=lpad$(trim$(resp$(1)(1:10)),10)
	h1$=lpad$(trim$(resp$(2)(1:10)),10)
	if ckey=6 then let fncustomer_search(resp$(1)) else goto L1470
	if trim$(l1$)="" then l1$=resp$(1) : goto SELR
	if trim$(h1$)="" then h1$=resp$(1) : goto SELR
	goto SELR
L1470: ! 
	if ckey=2 then goto SCR2
	l1$=lpad$(l1$,10) : h1$=lpad$(h1$,10)
	if h1$<l1$ then 
		mat msgline$(1)
		msgline$(1)="You have entered invalid accounts!"
		fnmsgbox(mat msgline$,resp$,cap$,48)
		goto SELR
	end if 
	restore #customer,key=l1$: nokey SELR
	goto TOP ! /r
IACC: ! r: select individual accounts
	fnTos(sn$="ublabel-4")
	fnLbl(1,1,"Account:",15)
	fncmbact(1,17)
	resp$(1)=selz$
	fnLbl(3,1,"Last selection: "&hz$,35,0)
	if trim$(sele$(2))<>"" then let fnLbl(2,17,sele$(2),20,0)
	fnCmdSet(23)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=2 then 
		fncustomer_search(resp$(1))
		selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=selz$: selz$,mat sele$,extra$(1),final nokey ignore
		goto IACC
	end if 
	if ckey=5 then goto XIT
! if ckey=1 then goto L1660
	if ckey=4 then goto DONE
! L1660: !
	z$=lpad$(rtrm$(resp$(1)(1:10)),10)
	hz$=z$
	if rtrm$(z$)="" then goto DONE
	selz$="": mat sele$=("")
	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey IACC
	meter_address$=e$(1)
	goto THERE ! /r
SELBK: ! r: selects by route
	fnTos(sn$="ublabel-5")
	respc=0
	fnLbl(1,1,"Route Number:",15,0)
	fncmbrt2(1,17)
	resp$(respc+=1)="1"
	fnLbl(2,1,"Sequence Number:",15,0)
	fnTxt(2,17,7,7,1,"20",0,"The sequence number is only required if you wish to start in the middle of a route")
	resp$(respc+=1)=""
	fnCmdSet(22)
	fnAcs(sn$,0,mat resp$,ckey) ! select labels by route
	if ckey=5 then goto XIT
	if ckey=2 then goto SCR2
	bk=0 : bk=val(resp$(1)) conv L1860
	seq=val(resp$(2)) conv L1860
	if annbc=sequence_bar_code and filter_selection=6 then goto TOP ! must start at front of file if bar coded and specific route
	L1860: ! 
	routekey$=lpad$(str$(bk),2)&lpad$(str$(seq),7)
	restore #customer,key>=routekey$: nokey SELBK
goto TOP ! /r
SCR4F3: ! r: select starting account name
	fnTos(sn$="ublabel-7")
	fnLbl(1,1,"Customer Name:",15,0)
	fnTxt(1,17,30,30,1,"",0,"Search and find the exact customer name if you wish to start with a specific customer")
	resp$(1)=""
	if trim$(sele$(2))<>"" then 
		fnLbl(2,17,sele$(2),20,0)
	end if 
	fnCmdSet(21)
	fnAcs(sn$,0,mat resp$,ckey) ! select starting customer name
	if ckey=5 then goto XIT
	if ckey=6 then let fncustomer_search(resp$(1))
	restore #customer,key>="       ": nokey ignore
	SCR4F3_READ_CUSTOMER: ! 
	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1': z$,mat e$,f,f3$,route,seq,extra$(1),final eof SCR4F3_NO_MATCH
	meter_address$=e$(1)
	if trim$(resp$(1))="" then goto SCR4F3_FINIS
	if lpad$(resp$(1),10)<>z$ then goto SCR4F3_READ_CUSTOMER
	SCR4F3_FINIS: ! 
goto PAST_READ
!
SCR4F3_NO_MATCH: ! r:
	mat msgline$(1)
	msgline$(1)="No matching name found!"
	fnmsgbox(mat msgline$,resp$,cap$,48)
goto SELSTART ! /r
! /r
SORT1: ! r: SELECT & SORT
	gosub OPENCASS
	close #6: ioerr ignore
	open #6: "Name="&env$('temp')&"\Work."&session$&",Replace,RecL=26",internal,output 
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
	open #9: "Name="&env$('Temp')&"\CONTROL."&session$&",Size=0,RecL=128,Replace",internal,output 
	write #9,using 'Form POS 1,C 128': "File "&env$('temp')&"\Work."&session$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
	write #9,using 'Form POS 1,C 128': "Mask 1,26,C,A"
	close #9: 
	execute "Free "&env$('Temp')&"\Addr."&session$&" -n" ioerr ignore
	execute "Sort "&env$('Temp')&"\CONTROL."&session$&" -n"
	open #6: "Name="&env$('temp')&"\Work."&session$,internal,input,relative 
	close #addr: ioerr ignore
	open #addr=7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
return  ! /r
BARCODE: ! r:
	gosub OPENCASS
	if file(5)=-1 then goto BARCODE_XIT
	labeltext$(5)=""
	read #5,using 'Form POS 1,C 10,POS 96,C 12': z2$,bc$ nokey BARCODE_XIT
	for j=1 to 4
		labeltext$(j)=labeltext$(j+1) ! move everything up one to allow for barcode
	next j
	labeltext$(5)=rtrm$(bc$)
	BARCODE_XIT: ! 
return  ! /r
SELSTART: ! r: select customer to start with
	fnTos(sn$="ublabel-6")
	mylen=26 : mypos=mylen+2
	fnLbl(1,1,"Starting account:",mylen,0)
	fncmbact(1,mypos,1)
	if trim$(selz$)='' then resp$(1)='[All]' else resp$(1)=selz$
	fnCmdSet(5)
	fnAcs(sn$,0,mat resp$,ckey) ! select starting customer
	if ckey=6 then 
		fncustomer_search(resp$(1))
		selz$=lpad$(rtrm$(resp$(1)(1:10)),10)
		read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=selz$: selz$,mat sele$,extra$(1),final nokey ignore
		goto SELSTART
	end if 
	if ckey=5 then goto XIT
	z$=lpad$(trim$(resp$(1)(1:10)),10)
	if trim$(z$)="[All]" then restore #1: : goto TOP
	selz$="": mat sele$=("")
	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey SELSTART
	meter_address$=e$(1)
	goto THERE ! /r
READ_FROM_GRID: ! r: select customers from grid
	fnTos(sn$="ublabel-7")
	fnLbl(1,1,"Grid name (including folders):",30,0)
	fnTxt(1,30,30,66,0,"70",0,"You must first export a fixed width file from the gird program (remember the name!)")
	resp$(1)=""
	fnLbl(2,40,"",30,0)
	fnCmdSet(3)
	fnAcs(sn$,0,mat resp$,ckey) ! select starting customer #
	if ckey=5 then goto XIT
	open #6: "Name="&trim$(resp$(1)),display,input ioerr READ_FROM_GRID
	LIN6: linput #6: x$ eof DONE
	z$=lpad$(trim$(x$(1:10)),10)
	read #customer,using 'Form POS 1,C 10,4*C 30,POS 296,PD 4,POS 373,C 12,POS 1741,N 2,N 7,pos 1864,C 30,pos 1821,n 1',key=z$: z$,mat e$,f,f3$,route,seq,extra$(1),final nokey LIN6
	meter_address$=e$(1)
	fn_get_address_lines
	gosub ADDLABEL
goto LIN6 ! /r
def library fncustomer_address(z$*10,mat addr$; ca_address_type,ca_closeFiles)
	if ~setup then let fn_setup
	fncustomer_address=fn_customer_address(z$,mat addr$, ca_address_type,ca_closeFiles)
fnend
def fn_customer_address(z$*10,mat addr$; ca_address_type,ca_closeFiles)
	if ~ca_setup then 
		ca_setup=1
		open #h_ca_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]"&',Shr',internal,input,keyed
		open #adrbil:=fngethandle: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed 
	end if
	if ca_address_type=0 then ca_address_type=ao_billing
	altaddr=ca_address_type
	mat pe$=('')
	z$=lpad$(trim$(z$),kln(h_ca_customer))
	read #h_ca_customer,using 'Form POS 11,4*C 30,pos 1864,C 30,pos 1854,PD 5.2',key=lpad$(z$,kln(h_ca_customer)): mat e$,extra$(1),extra(22) nokey CA_FINIS
	fn_get_address_lines
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
def fn_get_address_lines
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
BULKSORT: ! r: bulk sort order
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno]",internal,input,keyed  ! open in Account order
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=31",internal,output 
	do 
		read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof BULKSORT_FINIS
		write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
	loop 
	BULKSORT_FINIS: ! 
	close #1: ioerr ignore
	close #6: ioerr ignore
	execute "Index "&env$('Temp')&"\Temp."&wsid$&" "&env$('Temp')&"\Tempidx."&wsid$&" 1,19,Replace,DupKeys -n" ioerr BULKSORT_XIT
	open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",KFName="&env$('Temp')&"\Tempidx."&wsid$,internal,input,keyed 
	BULKSORT_XIT: ! 
return  ! /r
OPEN_FILES: ! r:
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
	execute "drop "&env$('temp')&"\label.dat -n" ioerr ignore
	fnLastBillingDate(d1)
	if annbc=sequence_bar_code and s5=0 then gosub SORT1
return  ! /r
OPENCASS: ! r:
	if file(5)=-1 then 
		open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr ignore
	end if 
return  ! /r
IGNORE: continue 
def fn_setup
	library 'S:\Core\Library': fntop,fnxit, fnerror,fnAcs,fncomboa,fnLbl,fnTos,fnmsgbox,fnTxt,fncustomer_search,fncmbrt2,fncmbact,fnaddlabel,fnlabel,fnCmdSet,fnLastBillingDate,fngethandle,fnreg_read,fnreg_write,fnget_services
	library 'S:\Core\Library': fnAddOneC
	on error goto Ertn
! r: constants and dims
	on fkey 5 goto DONE
	dim filter_option_enabled$(7)*50,e$(4)*30
	dim meter_address$*30
	dim pe$(4)*30,ba$(4)*30,cap$*128,resp$(10)*80
	dim labeltext$(5)*120,x$*512,z$*50
	dim extra$(11)*30,extra(23) ! fields from Customer File
	dim srvnam$(10)*20,srv$(10)*2
! 
	fnget_services(mat srvnam$, mat srv$)
! 
	dim line_x_option$(9)*70
	mat line_x_option$(0)
	line_x_blank                   =fnAddoneC(mat line_x_option$,"(blank)"                                                        )
	line_x_account_number          =fnAddoneC(mat line_x_option$,"Account"                                                        )
	line_x_accountRightJustified   =fnAddoneC(mat line_x_option$,"Account (right justified)"                                      )
	line_x_meter_address           =fnAddoneC(mat line_x_option$,"Meter Address"                                                  )
	line_x_customer_name           =fnAddoneC(mat line_x_option$,"Name"                                                           )
	line_x_mailing_address_line_1  =fnAddoneC(mat line_x_option$,"Mailing Address Line 1"                                         )
	line_x_mailing_address_line_2  =fnAddoneC(mat line_x_option$,"Mailing Address Line 2 or if blank City State Zip"              )
	line_x_mailing_address_line_3  =fnAddoneC(mat line_x_option$,"City State Zip if Mailing Address Line 2 not blank"             )
	line_x_meter_route_and_sequenc =fnAddoneC(mat line_x_option$,trim$(srvnam$(4))&" Meter, Route and Sequence Numbers"          )
	line_x_account_meter4_and_seq  =fnAddoneC(mat line_x_option$,"Account, "&trim$(srvnam$(4))&" Meter, and Sequence Numbers"    )
	line_x_meterAndSequence        =fnAddoneC(mat line_x_option$,trim$(srvnam$(4))&" Meter and Sequence Numbers"                 )
! 
	dim address_option$(3)*30
	address_option$(ao_primary  :=1)="Primary Address"
	address_option$(ao_alternate:=2)="Alternate Billing Address"
	address_option$(ao_billing  :=3)="Billing Address"
! 
	dim sequence_option$(6)*22
	sequenceCount=0
	sequence_option$(sequence_account  :=sequenceCount+=1)="Account"
	! sequence_option$(sequence_name:=2)="Customer Name"
	sequence_option$(sequence_bar_code :=sequenceCount+=1)="Bar Code"
	sequence_option$(sequence_route    :=sequenceCount+=1)="Route"
	sequence_option$(sequence_grid     :=sequenceCount+=1)="Grid"
	sequence_option$(sequence_bulk_sort:=sequenceCount+=1)="Bulk Sort Code"
! 
	dim filter_option$(7)*50
	filter_option$(1)="[All]"
	filter_option$(2)="Customers billed last month"
	filter_option$(3)="Customers not billed last billing"
	filter_option$(4)="Range of Accounts"
	filter_option$(5)="Individual accounts"
	filter_option$(6)="Route"
	filter_option$(7)="Active Customers"
! /r
fnend 
include: ertn