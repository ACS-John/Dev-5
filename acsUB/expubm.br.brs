! Replace S:\acsUB\expubm
! -- Export UB Master File
! ______________________________________________________________________
	library 'S:\Core\Library': fntop,fnxit,fnTos,fnLbl,fnTxt,fnAcs,fnxit,fnerror,fnCmdSet,fntop,fnreg_read,fnreg_write,fncustomer_address,fnget_services
	library 'S:\Core\Library': fngethandle
! ______________________________________________________________________
	on error goto ERTN
! ______________________________________________________________________
	dim gb(10),ab$(3)*30,flob$(5),scrb$(3)*24,inb$(3)
	dim ri1$(20),rm$*60,rm$(20)*60,ra1(20),ra(2)
	dim wf$(13),ws$(13)*30,iow$(16),i$(16)*70
	dim fld$*60,nam$*30,n$*30,fm$(22)*80,ul$*186,hd$*186
	dim hln$*78,hk$*9,inh$(20),lnh$(20)*78,hhdr$*60
	dim x$*10,scrid$(2)*75,p$*10,o(2),txt$(3)*80
	dim tr(4),lnadr(20),tc$(4)*12,si$*25
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
	dim dest$*256,csz$*40,first_name$*30,last_name$*30,cap$*128
	dim delim$*1,streetnam$*30,streetnum$*30,state$*30,city$*30,zip$*30
	dim resp$(3)*256,extra$(11)*30
	fntop(program$,cap$="Export UB Master File")
	dim serviceName$(10)*20
	fnget_services(mat serviceName$) : for sNitem=1 to udim(mat serviceName$) : serviceName$(sNitem)=trim$(serviceName$(sNitem)) : nex sNitem
MENU1: ! r:
	fnTos(sn$="expubm")
	fnLbl(1,1,"Destination Path and File Name:",34,1)
	fnTxt(1,36,40,256,0,"71")
	fnreg_read('exp_ubm.path',resp$(1)) : if resp$(1)='' then resp$(1)=os_filename$(env$('Desktop'))&"\ubm.txt"
	fnLbl(2,1,"Delimiter (ASCII Code):" ,34,1)
	fnTxt(2,36,3,0,0,"30")
	resp$(2)="9"
	fnLbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
	fnCmdSet(2)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then goto XIT
	dest$=resp$(1)
	delas=val(resp$(2))
	fnreg_write('exp_ubm.path',dest$)
	goto OPENS
! /r
OP2ERR: ! r:
	goto MENU1
! /r
OPENS: ! 
	delim$=chr$(delas)
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno]"&',shr',internal,outIn,relative 
	! open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno]",internal,input,relative 
	open #2: "Name="&br_filename$(dest$)&",Size=0,RecL=2500,Replace,EOL=CRLF",display,output ioerr OP2ERR
! form pos 1,c 14,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1
! form pos 287,c 10,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1
	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed 
	gosub HEADER ! work in progress
	do 
		read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,finalBillingCode,mat extra$ eof DONE
		F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1821,n 1,pos 1864,c 30,c 12,c 12,c 12,c 12,c 12,c 12,c 12,c 30,c 30,c 30
		gosub ALT_BILL_ADR
		dim addr$(4)*40
		fncustomer_address(z$,mat addr$)
		pr #2: '"'&z$&'"'&delim$;
		pr #2: e$(1)&delim$&e$(2)&delim$&e$(3)&delim$&extra$(1)&delim$&e$(4)&delim$;
		pr #2: f$(1)&delim$&str$(a(1))&delim$&str$(a(2))&delim$&str$(a(3))&delim$&str$(a(4))&delim$&str$(a(5))&delim$&str$(a(6))&delim$&str$(a(7))&delim$;
		pr #2: str$(b(1))&delim$&str$(b(2))&delim$&str$(b(3))&delim$;
		pr #2: str$(b(4))&delim$&str$(b(5))&delim$&str$(b(6))&delim$&str$(b(7))&delim$&str$(b(8))&delim$&str$(b(9))&delim$&str$(b(10))&delim$&str$(b(11))&delim$;
		pr #2: str$(c(1))&delim$&str$(c(2))&delim$&str$(c(3))&delim$&str$(c(4))&delim$&str$(d(1))&delim$&str$(d(2))&delim$&str$(d(3))&delim$&str$(d(4))&delim$&str$(d(5))&delim$&str$(d(6))&delim$&str$(d(7))&delim$&str$(d(8))&delim$&str$(d(9))&delim$;
		pr #2: str$(d(10))&delim$&str$(d(11))&delim$&str$(d(12))&delim$&str$(d(13))&delim$&str$(d(14))&delim$&str$(d(15))&delim$&str$(bal)&delim$&str$(f)&delim$&str$(g(1))&delim$&str$(g(2))&delim$&str$(g(3))&delim$;
		pr #2: str$(g(4))&delim$&str$(g(5))&delim$&str$(g(6))&delim$&str$(g(7))&delim$&str$(g(8))&delim$&str$(g(9))&delim$&str$(g(10))&delim$&str$(g(11))&delim$&str$(g(12))&delim$&alp$&delim$&f$(2)&delim$&f$(3)&delim$;
		pr #2: str$(bra)&delim$&str$(gb(1))&delim$&str$(gb(2))&delim$&str$(gb(3))&delim$&str$(gb(4))&delim$&str$(gb(5))&delim$&str$(gb(6))&delim$&str$(gb(7))&delim$&str$(gb(8))&delim$&str$(gb(9))&delim$&str$(gb(10))&delim$&ab$(1)&delim$&ab$(2)&delim$&ab$(3)&delim$;
		pr #2: str$(finalBillingCode)&delim$;
		pr #2: addr$(1)&delim$;
		pr #2: addr$(2)&delim$;
		pr #2: addr$(3)&delim$;
		pr #2: addr$(4)&delim$
	loop 
DONE: ! r:
	close #h_customer: ioerr ignore
	close #2: ioerr ignore
	close #3: ioerr ignore
goto XIT ! /r
XIT: ! 
	fnxit
ALT_BILL_ADR: ! r:
	mat ab$=("")
	read #3,using 'Form POS 11,3*C 30',key=z$: mat ab$ nokey ignore
return  ! /r
HEADER: ! r:
	pr #2: 'Account Key'&delim$;           ! z$
	pr #2: 'Meter Address'&delim$;         ! e$(1)
	pr #2: 'Name'&delim$;                  ! e$(2)
	pr #2: 'Address 1 - Primary'&delim$;   ! e$(3)
	pr #2: 'Address 2 - Primary'&delim$;   ! extra$(1)
	pr #2: 'CSZ - Primary'&delim$;         ! e$(4)
	pr #2: serviceName$(1)&' Meter Number'&delim$; ! f$(1)&delim$;
	pr #2: serviceName$(1)&' Rate Code'&delim$; ! str$(a(1))&delim$;
	pr #2: serviceName$(2)&' Rate Code'&delim$; ! str$(a(2))&delim$;
	pr #2: serviceName$(3)&' Rate Code'&delim$; ! str$(a(3))&delim$;
	pr #2: serviceName$(4)&' Rate Code'&delim$; ! str$(a(4))&delim$;
	pr #2: serviceName$(5)&' Rate Code'&delim$; ! str$(a(5))&delim$;
	pr #2: serviceName$(9)&' Rate Code'&delim$; ! str$(a(6))&delim$;
	pr #2: serviceName$(10)&' Rate Code'&delim$; ! str$(a(7))&delim$;
	pr #2: serviceName$(1)&' Standard Charge'&delim$; ! str$(b(1))&delim$;
	pr #2: serviceName$(2)&' Standard Charge'&delim$; ! str$(b(2))&delim$;
	pr #2: serviceName$(3)&' Standard Charge'&delim$; ! str$(b(3))&delim$;
	pr #2: serviceName$(4)&' Standard Charge'&delim$; ! str$(b(4))&delim$;
	pr #2: serviceName$(5)&' Standard Charge'&delim$; ! str$(b(5))&delim$;
	pr #2: serviceName$(6)&' Standard Charge'&delim$; ! str$(b(6))&delim$;
	pr #2: serviceName$(7)&' Standard Charge'&delim$; ! str$(b(7))&delim$;
	pr #2: serviceName$(1)&' Deposit'&delim$; ! str$(b(8))&delim$;
	pr #2: serviceName$(2)&' Deposit'&delim$; ! str$(b(9))&delim$;
	pr #2: serviceName$(3)&' Deposit'&delim$; ! str$(b(10))&delim$;
	pr #2: serviceName$(4)&' Deposit'&delim$; ! str$(b(11))&delim$;
	pr #2: serviceName$(1)&' Deposit Date'&delim$; ! str$(c(1))&delim$;
	pr #2: serviceName$(2)&' Deposit Date'&delim$; ! str$(c(2))&delim$;
	pr #2: serviceName$(3)&' Deposit Date'&delim$; ! str$(c(3))&delim$;
	pr #2: serviceName$(4)&' Deposit Date'&delim$; ! str$(c(4))&delim$;
	pr #2: serviceName$(1)&' Reading - Current'&delim$; ! str$(d(1))&delim$;
	pr #2: serviceName$(1)&' Reading - Prior'&delim$; ! str$(d(2))&delim$;
	pr #2: serviceName$(1)&' Used - Current'&delim$; ! str$(d(3))&delim$;
	pr #2: serviceName$(1)&' Used - YTD'&delim$; ! str$(d(4))&delim$;
	pr #2: serviceName$(3)&' Reading - Current'&delim$; ! str$(d(5))&delim$;
	pr #2: serviceName$(3)&' Reading - Prior'&delim$; ! str$(d(6))&delim$;
	pr #2: serviceName$(3)&' KWH Used - Current'&delim$; ! str$(d(7))&delim$;
	pr #2: serviceName$(3)&' KWH Used -YTD'&delim$; ! str$(d(8))&delim$;
	pr #2: serviceName$(4)&' Reading - Current'&delim$; ! str$(d(9))&delim$;
	pr #2: serviceName$(4)&' Reading - Prior'&delim$; ! str$(d(10))&delim$;
	pr #2: serviceName$(4)&' Used-Current'&delim$; ! str$(d(11))&delim$;
	pr #2: serviceName$(4)&' Used-YTD'&delim$; ! str$(d(12))&delim$;
	pr #2: 'Units Per Meter'&delim$; ! str$(d(13))&delim$;
	pr #2: 'Demand Multiplier'&delim$; ! str$(d(14))&delim$;
	pr #2: 'Demand Reading'&delim$; ! str$(d(15))&delim$;
	pr #2: 'Current Balance'&delim$; ! str$(bal)&delim$;
	pr #2: 'Date of Charge'&delim$; ! str$(f)&delim$;
	pr #2: serviceName$(1)&' Charge'&delim$; ! str$(g(1))&delim$;
	pr #2: serviceName$(2)&' Charge'&delim$; ! str$(g(2))&delim$;
	pr #2: serviceName$(3)&' Charge'&delim$; ! str$(g(3))&delim$;
	pr #2: serviceName$(4)&' Charge'&delim$; ! str$(g(4))&delim$;
	pr #2: serviceName$(5)&' Charge'&delim$; ! str$(g(5))&delim$;
	pr #2: serviceName$(6)&' Charge'&delim$; ! str$(g(6))&delim$;
	pr #2: serviceName$(7)&' Charge'&delim$; ! str$(g(7))&delim$;
	pr #2: serviceName$(8)&' Charge'&delim$; ! str$(g(8))&delim$;
	pr #2: serviceName$(9)&' Charge'&delim$; ! str$(g(9))&delim$;
	pr #2: serviceName$(10)&' Charge'&delim$; ! str$(g(10))&delim$;
	pr #2: 'Current Bill – Net Bill'&delim$; ! str$(g(11))&delim$;
	pr #2: 'Current Bill – Gross Bill'&delim$; ! str$(g(12))&delim$;
	pr #2: 'Alpha Sort Field'&delim$; ! alp$&delim$;
	pr #2: serviceName$(3)&' Meter Number'&delim$; ! f$(2)&delim$;
	pr #2: serviceName$(4)&' Meter Number'&delim$; ! f$(3)&delim$;
	pr #2: 'Alternate Billing Address'&delim$; ! str$(bra)&delim$;
	pr #2: serviceName$(1)&' Breakdown'&delim$; ! str$(gb(1))&delim$;
	pr #2: serviceName$(2)&' Breakdown'&delim$; ! str$(gb(2))&delim$;
	pr #2: serviceName$(3)&' Breakdown'&delim$; ! str$(gb(3))&delim$;
	pr #2: serviceName$(4)&' Breakdown'&delim$; ! str$(gb(4))&delim$;
	pr #2: serviceName$(5)&' Breakdown'&delim$; ! str$(gb(5))&delim$;
	pr #2: serviceName$(6)&' Breakdown'&delim$; ! str$(gb(6))&delim$;
	pr #2: serviceName$(7)&' Breakdown'&delim$; ! str$(gb(7))&delim$;
	pr #2: serviceName$(8)&' Breakdown'&delim$; ! str$(gb(8))&delim$;
	pr #2: serviceName$(9)&' Breakdown'&delim$; ! str$(gb(9))&delim$;
	pr #2: serviceName$(10)&' Breakdown'&delim$; ! str$(gb(10))&delim$;
	pr #2: 'Name - Alternate Billing'&delim$; ! ab$(1)&delim$;
	pr #2: 'Address - Alternate Billing'&delim$; ! ab$(2)&delim$;
	pr #2: 'CSZ - Alternate Billing'&delim$; ! ab$(3)
	pr #2: 'Final Billing Code'&delim$; ! ab$(3)
	pr #2: 'Billing Address 1'&delim$; ! ab$(3)
	pr #2: 'Billing Address 2'&delim$; ! ab$(3)
	pr #2: 'Billing Address 3'&delim$;! ab$(3)
	pr #2: 'Billing Address 4'&delim$ ! ab$(3)
return  ! /r
include: ertn