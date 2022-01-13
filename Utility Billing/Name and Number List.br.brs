! formerly S:\acsUB\ubNamLst
! r: setup library, dims, constants, fnTop, etc
	autoLibrary
	on error goto Ertn
 
	dim z$*10,e$(4)*30,dat$*20,idx$(5)*40,resp$(10)*80
	dim item1$(6)*22,item2$(6)*32,x$*512,text$*50
	dim hd1$*30,a2(10),a1(10),a(10)
	dim ab$(4)*30,extra$(11)*30
 
	fnTop(program$)
	dat$=date$("Month DD, CCYY")

	idx$(1)="[Q]\UBmstr\ubIndex.h[cno]"
	idx$(2)="[Q]\UBmstr\ubIndx2.h[cno]"
	idx$(3)="[Q]\UBmstr\ubIndx3.h[cno]"
	idx$(4)="[Q]\UBmstr\ubIndex.h[cno]"
	idx$(5)="[Q]\UBmstr\ubIndx5.h[cno]"

	item1$(1)="Account"
	item1$(2)="Customer Name"
	item1$(3)="Street"
	item1$(4)="Street - Auto-Reversed"
	item1$(5)="Grid Selection"
	item1$(6)="Route Sequence"

	item2$(1)="Active Customers Only"
	item2$(2)="Inactive Customers Only"
	item2$(3)="All Customers (Regular Address)"
	item2$(4)="All using Mailing Address"
	item2$(5)="Only Alternate Addresses"
	item2$(6)="Active, But Not Being Billed"

	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,outIn,keyed
! /r
! MENU1: ! r:
	fnTos
	respc=0
	fnLbl(1,1,"Sequence:",23,1)
	fncomboa("ubnamlst-srt",1,25,mat item1$,"The auto-reversed option can turn all addresses around so the streets are sorted by name rather than number (ie Adams Streets together instead of 101s")
	resp$(respc+=1)=item1$(1)
	fnLbl(2,1,"Report Heading Date:",23,1)
	fnTxt(2,25,20)
	resp$(respc+=1)=dat$
	fnLbl(3,1,"Limit by:",23,1)
	fncomboa("ubnamlst-act",3,25,mat item2$)
	resp$(respc+=1)=item2$(3)
	fnChk(5,29,"Print Rate Codes")
	resp$(respc+=1)='False'
	fnChk(6,29,"Print Address")
	resp$(respc+=1)='False'
	fnChk(8,29,"Print Balance")
	fnLbl(8,45,"(Route Sequence never prints Balance)",23,1)
	resp$(resp_print_balance:=respc+=1)='True'
	fnChk(9,29,"Print Phone")
	resp$(resp_print_phone:=respc+=1)='False'
	fnChk(10,29,"Print Cell")
	resp$(resp_print_cell:=respc+=1)='False'

	fnChk(11,29,"Print Email")
	resp$(resp_print_email:=respc+=1)='False'


	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	q0=2 ! default to name sequence
	if resp$(1)=item1$(1) then
		q0=1 : opt=1 : turn$="N"
	else if resp$(1)=item1$(2) then
		q0=2 : opt=2 : turn$="N"
	else if resp$(1)=item1$(3) then
		q0=3 : opt=3 : turn$="N"
	else if resp$(1)=item1$(4) then
		q0=3 : opt=4 : turn$="Y"
	else if resp$(1)=item1$(5) then
		q0=4 : opt=5 : turn$="N"
	else if resp$(1)=item1$(6) then
		q0=5 : opt=6 : turn$="N"
	end if
	dat$=resp$(2)
	if resp$(4)='True' then ti3=1 else ti3=0
	if resp$(5)='True' then print_address=1 else print_address=0
	if resp$(resp_print_balance)='True' then print_balance=1 else print_balance=0
	if resp$(resp_print_phone)  ='True' then print_phone=1 else print_phone=0
	if resp$(resp_print_cell)   ='True' then print_cell=1 else print_cell=0
	if resp$(resp_print_email)  ='True' then print_email=1 else print_email=0
	if resp$(3)=item2$(1) then
		ti2=1
	else if resp$(3)=item2$(2) then
		ti2=2
	else if resp$(3)=item2$(3) then
		ti2=3
	else if resp$(3)=item2$(4) then
		ti2=4
	else if resp$(3)=item2$(5) then
		ti2=5
	else if resp$(3)=item2$(6) then
		ti2=6
	end if
goto ReportInit ! /r
ReportInit: ! r:
	if ti3=1 then gosub GET_AU
	on fkey 5 goto DONE
	if q0=3 and turn$="Y" then gosub STREET_REVERSE
	if uprc$(turn$)="Y" then
		open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName="&idx$(1)&",Shr",i,i,k
	else
		open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName="&idx$(q0)&",Shr",i,i,k
		if q0=4 then gosub OPEN_GRID ! OPEN GRID DISPLAY FILE
	end if
	dim email$*30
	F_CUSTOMER: form pos 1,c 10,pos 11,4*c 30,pos 143,5*pd 2,pos 1806,3*n 2,pos 153,2*pd 2,pos 1821,n 1,pos 292,pd 4.2,pos 1741,n 2,n 7,pos 1864,c 30,c 12,pos 1966,c 12,c 30
 
	fnopenprn
	gosub HEADER
goto LOOP_TOP ! /r
LOOP_TOP: ! r:
	if q0=4 then
		gosub READ_FROM_GRID
		goto L520 ! READ FROM GRID DISPLAY
	end if
	L490: !
	if uprc$(turn$)="Y" then
		read #7,using "Form POS 1,PD 3": addr eof DONE
	else
		goto L570
	end if
	read #6,using 'form pos 1,c 10',rec=addr: z$ noRec LOOP_TOP
	L520: !
	read #1,using F_CUSTOMER,key=z$: z$,mat e$,mat a,final,bal,route,sequence,extra$(1),extra$(2),extra$(8),email$ eof DONE nokey LOOP_TOP
	if ti2=2 and final=0 then goto L490 ! skip active
	if ti2=4 or ti2=5 then gosub CHECK_ALTERNATE
	goto L590
 
	L570: !
	read #1,using F_CUSTOMER: z$,mat e$,mat a,final,bal,route,sequence,extra$(1),extra$(2),extra$(8),email$ eof DONE
	if ti2=2 and (final=0 or final=3) then ! skip active (including the final code threes who are active but snow birding
		goto L570
	end if
	L590: !
	if ti3=1 then
		j1=1 : mat a2=(0)
		for j=1 to 10
			if a1(j)=1 then
				a2(j1)=a(j)
				j1=j1+1
			end if
		next j
	end if
	if ti2=3 then goto L690
	if ti2=1 and final><0 then goto LOOP_TOP
	if ti2=4 or ti2=5 then gosub CHECK_ALTERNATE
	if ti2=5 and trim$(ab$(1))="" and trim$(ab$(2))="" and trim$(ab$(3))="" and trim$(ab$(4))="" then
		goto LOOP_TOP
	end if
	if ti2=6 and final<>3 then goto L570
	L690: !
	if opt=6 then ! route sequence
		pr #255,using F_OUT_ROUTE_SEQ: z$,e$(2),e$(1),route,sequence,mat a2 pageoflow PgOf
		F_OUT_ROUTE_SEQ: form x 5,c 10,x 5,c 30,x 7,c 30,n 2,x 1,n 7,x 1,10*nz 3
	else if print_balance then
		pr #255,using F_OUT_W_BAL: z$,e$(2),e$(1),bal,mat a2 pageoflow PgOf
		F_OUT_W_BAL: form x 5,c 10,x 5,c 30,x 7,c 30,n 11.2,x 1,10*nz 3
	else
		pr #255,using F_OUT_NOBAL: z$,e$(2),e$(1),mat a2 pageoflow PgOf
		F_OUT_NOBAL: form x 5,c 10,x 5,c 30,x 7,c 30,x 12,10*nz 3
	end if
	if trim$(e$(3))="" then e$(3)=extra$(1): extra$(1)=""
	if trim$(extra$(1))="" then extra$(1)=e$(4): e$(4)=""
	if print_address=1 then
		pr #255,using "form pos 21,c 31": e$(3)
		pr #255,using "form pos 21,c 31": extra$(1)
		pr #255,using "form pos 21,c 30": e$(4)
	end if
	if print_phone=1 and trim$(extra$(2))<>'' then
		pr #255,using "form pos 21,c 31": '  Phone: '&extra$(2)
	end if
	if print_cell=1 and trim$(extra$(8))<>'' then
		pr #255,using "form pos 21,c 31": '   Cell: '&extra$(8)
	end if
	if print_email=1 and trim$(email$)<>'' then
		pr #255,using "form pos 21,c 40": '   Email: '&email$
	end if
goto LOOP_TOP ! /r
 
PgOf: ! r:
	pr #255: newpage
	gosub HEADER
continue ! /r
HEADER: ! r:
	pr #255: "\qc {\b "&env$('cnam')&"}"
	p2=p2+1
	pr #255: "\qc {\fs28 {\b "&env$('program_caption')&"}}"
	pr #255: "\qc {\b "&trim$(item1$(opt))&" Order}"
	pr #255: "\qc {\b "&trim$(item2$(ti2))&"}"
	pr #255,using 'form pos 21,cc 40,pos 71,c 5,pic(zzz)': dat$,"Page ",p2
	pr #255: ""
	if opt=6 then
		pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }  {\ul Rt-Seq   }  {\ul "&hd1$&"}"
	else if print_balance then
		pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }   {\ul   Balance} {\ul "&hd1$&"}"
	else
		pr #255: "\ql {\ul     Customer No}     {\ul Name                             }    {\ul Meter Address                }              {\ul "&hd1$&"}"
	end if
return  ! /r
DONE: ! r:
	close #1: ioerr ignore
	fncloseprn
goto Xit ! /r
Xit: fnXit

GET_AU: ! r:
	dim serviceName$(10)*20,service$(10)*2
	fnGetServices(mat serviceName$,mat service$)
	for j=1 to 10
		if trim$(service$(j))<>"" then
			hd1$=hd1$&lpad$(service$(j)(1:2),3)
			a1(j)=1
		end if
	next j
return  ! /r
STREET_REVERSE: ! r: FOR SORTING
	open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName="&idx$(q0)&",Shr",i,i,k
	open #10: "Name=[Temp]\Temp.[Session],RecL=40,Replace",internal,outIn
	do
		read #1,using 'form pos 1,c 10,c 30': z$,e$(1) eof SORT1
		x=y=0
		x=pos(e$(1)," ",1)
		if x>0 then y=val(e$(1)(1:x-1)) conv ignore
		if y>0 then e$(1)=rtrm$(e$(1)(x+1:30))&" "&e$(1)(1:x-1)
		write #10,using 'form pos 1,c 10,c 30': z$,e$(1)
	loop
	SORT1: !
	close #1:
	close #10:
	open #9: "Name=[Temp]\Control.[Session],Size=0,RecL=128,Replace",internal,output
	write #9,using 'form pos 1,c 128': "File [Temp]\Temp.[Session],,,[Temp]\Addr.[Session],,,,,A,N"
	write #9,using 'form pos 1,c 128': "Mask 11,30,C,A"
	close #9:
	execute "Free [Temp]\Addr."&session$ ioerr ignore
	execute "Sort [Temp]\Control."&session$
	open #6: "Name=[Temp]\Temp."&session$,i,i,r
	open #7: "Name=[Temp]\Addr."&session$,i,i,r
return  ! /r
OPEN_GRID: ! r: select customers from grid
	sn$="ublabel-7"
	fnTos(sn$)
	text$="Grid name (including folders):"
	fnLbl(1,1,text$,70,0)
	fnTxt(1,30,70,0,0,"70",0,"You must first export a fixed width file from the gird program (remember the name!)")
	resp$(1)=""
	fnCmdSet(3)
	ckey=fnAcs(mat resp$) ! Select starting customer #
	if ckey=5 then goto Xit
	open #6: "Name="&trim$(resp$(1)),display,input ioerr OPEN_GRID
return  ! /r
READ_FROM_GRID: ! r: READ CUSTOMER # FROM GRID
	linput #6: x$ eof DONE
	z$=lpad$(trim$(x$(1:10)),10)
return  ! /r
CHECK_ALTERNATE: ! r: check for alternate billing address
	mat ab$=('')
	read #3,using "Form POS 11,4*C 30",key=z$: mat ab$ nokey L1690
	if trim$(ab$(1))="" and trim$(ab$(2))="" and trim$(ab$(3))="" and trim$(ab$(4))="" then
		goto L1690
	end if
	if ti2=4 or ti2=5 then
		e$(2)=ab$(1)
		e$(3)=ab$(2)
		e$(4)=ab$(4)
		extra$(1)=ab$(3)
	end if
	L1690: !
return  ! /r
include: ertn
