! similar to S:\Utility Billing\Create Hand Held File
! -- Tranfer Data From Computer to Hand Held
! r: setup
	autoLibrary
	on error goto Ertn

	dim gb(10),ab$(3)*30
	dim z$*10
	dim rw4(22,13)
	dim z$*10,e$(4)*30,f$(3)*12,a(7),b(11),c(4),d(15),g(12),adr(2),alp$*7
	dim delim$*1
	dim resp$(3)*256
	dim serviceName$(10)*20
	dim extra(23),extra$(11)*30
	dim exp_filename$*256

	fnTop(program$)

	if ~fnClientHas('U5') then
		mat m$(2)
		m$(1)="You must purchase the ACS Utility Billing External Collections Processing"
		m$(2)="module to access these features"
		fnmsgbox(mat m$, response$, '',64)
		goto Xit
	end if

	fnGetServices(mat serviceName$) : for servicename_item=1 to udim(mat serviceName$) : serviceName$(servicename_item)=trim$(serviceName$(servicename_item)) : next servicename_item
	delim$=chr$(9)

	open #hCustomer=fnH: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",i,i,k
	open #h_alt_bill=fnH: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",i,i,k

	fnureg_read('ECP Export Filename',exp_filename$)
	if exp_filename$='' then exp_filename$=os_filename$(env$('Desktop'))&"\ACS_ECP_Export.txt"
! /r
MENU1: !
	fnTos
	fnLbl(1,1,"Destination Path and File Name:",34,1)
	fnTxt(1,36,40,256,0,"71")
	resp$(1)=exp_filename$
	fnLbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey=5 then goto Xit
	exp_filename$=resp$(1)

	open #h_ecp=fnH: "Name=[at]"&exp_filename$&",Size=0,RecL=2500,Replace,EOL=CRLF",d,o ioerr MENU1
	exp_filename$=os_filename$(file$(h_ecp))
	fnureg_write('ECP Export Filename',exp_filename$)
! restore #hCustomer:
! r: main loop
	gosub HEADER ! work in progress
	do
		read #hCustomer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,mat rw4,mat extra$ eof Finis
		F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,78*pd 5,13*pd 4.2,13*n 6,156*pd 4.2,13*n 6,13*pd 4.2,pos 1864,c 30,7*c 12,3*c 30
		gosub ALT_BILL_ADR
		pr #h_ecp: z$&delim$; !    1
		pr #h_ecp: e$(1)&delim$; !    1.5         Meter Address
		pr #h_ecp: e$(2)&delim$; !    2         Name
		pr #h_ecp: e$(3)&delim$; !    3          addr 1
		pr #h_ecp: extra$(1)&delim$; !    4          addr 2
		pr #h_ecp: e$(4)&delim$; !    5
		pr #h_ecp: f$(1)&delim$; !    6
		pr #h_ecp: str$(a(1))&delim$; !   7
		pr #h_ecp: str$(a(2))&delim$; !   8
		pr #h_ecp: str$(a(3))&delim$; !   9
		pr #h_ecp: str$(a(4))&delim$; !   10
		pr #h_ecp: str$(a(5))&delim$; !   11
		pr #h_ecp: str$(a(6))&delim$; !   12
		pr #h_ecp: str$(a(7))&delim$; !   13
		pr #h_ecp: str$(b(1))&delim$; !   14
		pr #h_ecp: str$(b(2))&delim$; !   15
		pr #h_ecp: str$(b(3))&delim$; !   16
		pr #h_ecp: str$(b(4))&delim$; !   17
		pr #h_ecp: str$(b(5))&delim$; !   18
		pr #h_ecp: str$(b(6))&delim$; !   19
		pr #h_ecp: str$(b(7))&delim$; !   20
		pr #h_ecp: str$(b(8))&delim$; !   21
		pr #h_ecp: str$(b(9))&delim$; !   22
		pr #h_ecp: str$(b(10))&delim$; !   23
		pr #h_ecp: str$(b(11))&delim$; !   24
		pr #h_ecp: str$(c(1))&delim$; !   25
		pr #h_ecp: str$(c(2))&delim$; !   26
		pr #h_ecp: str$(c(3))&delim$; !   27
		pr #h_ecp: str$(c(4))&delim$; !   28
		pr #h_ecp: str$(d(1))&delim$; !   29
		pr #h_ecp: str$(d(2))&delim$; !   30
		pr #h_ecp: str$(d(3))&delim$; !   31
		pr #h_ecp: str$(d(4))&delim$; !   32
		pr #h_ecp: str$(d(5))&delim$; !   33
		pr #h_ecp: str$(d(6))&delim$; !   34
		pr #h_ecp: str$(d(7))&delim$; !   35
		pr #h_ecp: str$(d(8))&delim$; !   36
		pr #h_ecp: str$(d(9))&delim$; !   37
		pr #h_ecp: str$(d(10))&delim$; !    38
		pr #h_ecp: str$(d(11))&delim$; !   39
		pr #h_ecp: str$(d(12))&delim$; !   40
		pr #h_ecp: str$(d(13))&delim$; !   41
		pr #h_ecp: str$(d(14))&delim$; !   42
		pr #h_ecp: str$(d(15))&delim$; !   43
		pr #h_ecp: str$(bal)&delim$; !    44
		pr #h_ecp: str$(f)&delim$; !    45
		pr #h_ecp: str$(g(1))&delim$; !   46
		pr #h_ecp: str$(g(2))&delim$; !   47
		pr #h_ecp: str$(g(3))&delim$; !   48
		pr #h_ecp: str$(g(4))&delim$; !   49
		pr #h_ecp: str$(g(5))&delim$; !   50
		pr #h_ecp: str$(g(6))&delim$; !   51
		pr #h_ecp: str$(g(7))&delim$; !   52
		pr #h_ecp: str$(g(8))&delim$; !   53
		pr #h_ecp: str$(g(9))&delim$; !   54
		pr #h_ecp: str$(g(10))&delim$; !   55
		pr #h_ecp: str$(g(11))&delim$; !   56
		pr #h_ecp: str$(g(12))&delim$; !   57
		pr #h_ecp: alp$&delim$; !    58
		pr #h_ecp: f$(2)&delim$; !    59
		pr #h_ecp: f$(3)&delim$; !    60
		pr #h_ecp: str$(bra)&delim$; !    61
		pr #h_ecp: str$(gb(1))&delim$; !   62
		pr #h_ecp: str$(gb(2))&delim$; !   63
		pr #h_ecp: str$(gb(3))&delim$; !   64
		pr #h_ecp: str$(gb(4))&delim$; !   65
		pr #h_ecp: str$(gb(5))&delim$; !   66
		pr #h_ecp: str$(gb(6))&delim$; !   67
		pr #h_ecp: str$(gb(7))&delim$; !   68
		pr #h_ecp: str$(gb(8))&delim$; !   69
		pr #h_ecp: str$(gb(9))&delim$; !   70
		pr #h_ecp: str$(gb(10))&delim$; !   71
		pr #h_ecp: ab$(1)&delim$; !    72
		pr #h_ecp: ab$(2)&delim$; !    73
		pr #h_ecp: ab$(3) !   74
	loop  ! /r
ALT_BILL_ADR: ! r:
	mat ab$=("")
	read #h_alt_bill,using 'form pos 11,3*C 30',key=z$: mat ab$ nokey ignore
	return  ! /r
HEADER: ! r:
	pr #h_ecp: 'Account Key'&delim$;
	pr #h_ecp: 'Meter Address'&delim$;
	pr #h_ecp: 'Name'&delim$;
	pr #h_ecp: 'Address 1 - Primary'&delim$;
	pr #h_ecp: 'Address 2 - Primary'&delim$;
	pr #h_ecp: 'CSZ - Primary'&delim$;
	pr #h_ecp: serviceName$(1)&' Meter Number'&delim$; ! f$(1)&delim$;
	pr #h_ecp: serviceName$(1)&' Rate Code'&delim$; ! str$(a(1))&delim$;
	pr #h_ecp: serviceName$(2)&' Rate Code'&delim$; ! str$(a(2))&delim$;
	pr #h_ecp: serviceName$(3)&' Rate Code'&delim$; ! str$(a(3))&delim$;
	pr #h_ecp: serviceName$(4)&' Rate Code'&delim$; ! str$(a(4))&delim$;
	pr #h_ecp: serviceName$(5)&' Rate Code'&delim$; ! str$(a(5))&delim$;
	pr #h_ecp: serviceName$(9)&' Rate Code'&delim$; ! str$(a(6))&delim$;
	pr #h_ecp: serviceName$(10)&' Rate Code'&delim$; ! str$(a(7))&delim$;
	pr #h_ecp: ''&delim$; ! str$(b(1))&delim$;
	pr #h_ecp: ''&delim$; ! str$(b(2))&delim$;
	pr #h_ecp: ''&delim$; ! str$(b(3))&delim$;
	pr #h_ecp: serviceName$(4)&' Standard Charge'&delim$; ! str$(b(4))&delim$;
	pr #h_ecp: ''&delim$; ! str$(b(5))&delim$;
	pr #h_ecp: ''&delim$; ! str$(b(6))&delim$;
	pr #h_ecp: ''&delim$; ! str$(b(7))&delim$;
	pr #h_ecp: serviceName$(1)&' Deposit'&delim$; ! str$(b(8))&delim$;
	pr #h_ecp: serviceName$(2)&' Deposit'&delim$; ! str$(b(9))&delim$;
	pr #h_ecp: serviceName$(3)&' Deposit'&delim$; ! str$(b(10))&delim$;
	pr #h_ecp: serviceName$(4)&' Deposit'&delim$; ! str$(b(11))&delim$;
	pr #h_ecp: serviceName$(1)&' Deposit Date'&delim$; ! str$(c(1))&delim$;
	pr #h_ecp: serviceName$(2)&' Deposit Date'&delim$; ! str$(c(2))&delim$;
	pr #h_ecp: serviceName$(3)&' Deposit Date'&delim$; ! str$(c(3))&delim$;
	pr #h_ecp: serviceName$(4)&' Deposit Date'&delim$; ! str$(c(4))&delim$;
	pr #h_ecp: serviceName$(1)&' Reading - Current'&delim$; ! str$(d(1))&delim$;
	pr #h_ecp: serviceName$(1)&' Reading - Prior'&delim$; ! str$(d(2))&delim$;
	pr #h_ecp: serviceName$(1)&' Used - Current'&delim$; ! str$(d(3))&delim$;
	pr #h_ecp: serviceName$(1)&' Used - YTD'&delim$; ! str$(d(4))&delim$;
	pr #h_ecp: serviceName$(3)&' Reading - Current'&delim$; ! str$(d(5))&delim$;
	pr #h_ecp: serviceName$(3)&' Reading - Prior'&delim$; ! str$(d(6))&delim$;
	pr #h_ecp: serviceName$(3)&' KWH Used - Current'&delim$; ! str$(d(7))&delim$;
	pr #h_ecp: serviceName$(3)&' KWH Used -YTD'&delim$; ! str$(d(8))&delim$;
	pr #h_ecp: serviceName$(4)&' Reading - Current'&delim$; ! str$(d(9))&delim$;
	pr #h_ecp: serviceName$(4)&' Reading - Prior'&delim$; ! str$(d(10))&delim$;
	pr #h_ecp: serviceName$(4)&' Used-Current'&delim$; ! str$(d(11))&delim$;
	pr #h_ecp: serviceName$(4)&' Used-YTD'&delim$; ! str$(d(12))&delim$;
	pr #h_ecp: 'Units Per Meter'&delim$; ! str$(d(13))&delim$;
	pr #h_ecp: 'Demand Multiplier'&delim$; ! str$(d(14))&delim$;
	pr #h_ecp: 'Demand Reading'&delim$; ! str$(d(15))&delim$;
	pr #h_ecp: 'Current Balance'&delim$; ! str$(bal)&delim$;
	pr #h_ecp: 'Date of Charge'&delim$; ! str$(f)&delim$;
	pr #h_ecp: serviceName$(1)&' Charge'&delim$; ! str$(g(1))&delim$;
	pr #h_ecp: serviceName$(2)&' Charge'&delim$; ! str$(g(2))&delim$;
	pr #h_ecp: serviceName$(3)&' Charge'&delim$; ! str$(g(3))&delim$;
	pr #h_ecp: serviceName$(4)&' Charge'&delim$; ! str$(g(4))&delim$;
	pr #h_ecp: serviceName$(5)&' Charge'&delim$; ! str$(g(5))&delim$;
	pr #h_ecp: serviceName$(6)&' Charge'&delim$; ! str$(g(6))&delim$;
	pr #h_ecp: serviceName$(7)&' Charge'&delim$; ! str$(g(7))&delim$;
	pr #h_ecp: serviceName$(8)&' Charge'&delim$; ! str$(g(8))&delim$;
	pr #h_ecp: serviceName$(9)&' Charge'&delim$; ! str$(g(9))&delim$;
	pr #h_ecp: 'Net Bill'&delim$; ! str$(g(10))&delim$;
	pr #h_ecp: 'Gross Bill'&delim$; ! str$(g(11))&delim$;
	pr #h_ecp: ''&delim$; ! str$(g(12))&delim$;
	pr #h_ecp: 'Alpha Sort Field'&delim$; ! alp$&delim$;
	pr #h_ecp: serviceName$(3)&' Meter Number'&delim$; ! f$(2)&delim$;
	pr #h_ecp: serviceName$(4)&' Meter Number'&delim$; ! f$(3)&delim$;
	pr #h_ecp: 'Alternate Billing Address'&delim$; ! str$(bra)&delim$;
	pr #h_ecp: serviceName$(1)&' Breakdown'&delim$; ! str$(gb(1))&delim$;
	pr #h_ecp: serviceName$(2)&' Breakdown'&delim$; ! str$(gb(2))&delim$;
	pr #h_ecp: serviceName$(3)&' Breakdown'&delim$; ! str$(gb(3))&delim$;
	pr #h_ecp: serviceName$(4)&' Breakdown'&delim$; ! str$(gb(4))&delim$;
	pr #h_ecp: serviceName$(5)&' Breakdown'&delim$; ! str$(gb(5))&delim$;
	pr #h_ecp: serviceName$(6)&' Breakdown'&delim$; ! str$(gb(6))&delim$;
	pr #h_ecp: serviceName$(7)&' Breakdown'&delim$; ! str$(gb(7))&delim$;
	pr #h_ecp: serviceName$(8)&' Breakdown'&delim$; ! str$(gb(8))&delim$;
	pr #h_ecp: serviceName$(9)&' Breakdown'&delim$; ! str$(gb(9))&delim$;
	pr #h_ecp: serviceName$(10)&' Breakdown'&delim$; ! str$(gb(10))&delim$;
	pr #h_ecp: 'Name - Alternate Billing'&delim$; ! ab$(1)&delim$;
	pr #h_ecp: 'Address - Alternate Billing'&delim$; ! ab$(2)&delim$;
	pr #h_ecp: 'CSZ - Alternate Billing'&delim$ ! ab$(3)
	return  ! /r

Finis: ! r: Transfer to or from Hand Held Computer
	close #hCustomer: ioerr ignore
	close #h_alt_bill: ioerr ignore
	close #h_ecp: ioerr ignore
	fn_report_created_file(exp_filename$)
	goto Xit ! /r
Xit: fnXit

	def fn_report_created_file(exp_filename_report$*512)
		dim m$(2)*512
		if exp_filename_report$<>'' and exp_filename_report$<>':CON:' then
			mat m$(2)
			m$(1)="External Collections File created:"
			m$(2)=os_filename$(exp_filename_report$)
			fnmsgbox(mat m$, response$, '',64)
		end if
	fnend
include: ertn
