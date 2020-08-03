! formerly S:\acsUB\expubm
fn_setup
fnTop(program$)
dim serviceName$(10)*20
fnGetServices(mat serviceName$) : for sNitem=1 to udim(mat serviceName$) : serviceName$(sNitem)=trim$(serviceName$(sNitem)) : nex sNitem
MENU1: ! r:
	dim resp$(3)*256
	fnTos
	fnLbl(1,1,"Destination Path and File Name:",34,1)
	fnTxt(1,36,40,256,0,'72')
	fnreg_read('exp_ubm.path',resp$(1)) : if resp$(1)='' then resp$(1)=fnSpecialFolderPath$('Desktop')&"\ubm.txt" ! =os_filename$(env$('Desktop'))&"\ubm.txt"
	! fnLbl(2,1,"Delimiter (ASCII Code):" ,34,1)
	! fnTxt(2,36,3,0,0,"30")
	! resp$(2)="9"
	fnLbl(5,1,"NOTE: If Destination exists it will be overwritten.",76,2)
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	dim dest$*256
	dest$=resp$(1)
	! delas=val(resp$(2))
	fnreg_write('exp_ubm.path',dest$)
	goto MainLoop
! /r
MainLoop: ! r:
	dim delim$*1
	delim$=tab$
	open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno]"&',shr',internal,outIn,relative
	! open #h_customer:=fngethandle: "Name=[Q]\UBmstr\Customer.h[cno]",internal,input,relative
	fnMakeSurePathExists(dest$)
	open #2: "Name="&br_filename$(dest$)&",RecL=2500,Replace,EOL=CRLF",display,output ioerr MENU1
	! form pos 1,c 14,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1
	! form pos 287,c 10,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1
	open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed
	gosub HEADER ! work in progress
	do
		dim z$*10
		dim e$(4)*30
		dim f$(3)*12
		dim a(7)
		dim b(11)
		dim c(4)
		dim d(15)
		dim g(12)
		dim adr(2)
		dim alp$*7
		dim gb(10)
		dim extra$(11)*30
		dim email$*30
		read #h_customer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat b,mat c,mat d,bal,f,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,finalBillingCode,mat extra$,email$ eof Finis
		F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1821,n 1,pos 1864,c 30,c 12,c 12,c 12,c 12,c 12,c 12,c 12,c 30,c 30,c 30,pos 1978,c 30
		gosub ALT_BILL_ADR
		dim addr$(4)*40
		fncustomer_address(z$,mat addr$)
		! r: pr #2 delimited field values
		pr #2: '"'&z$&'"'              	&delim$;
		pr #2: e$(1)                   	&delim$;
		pr #2: e$(2)                   	&delim$;
		pr #2: e$(3)                   	&delim$;
		pr #2: extra$(1)               	&delim$;
		pr #2: e$(4)                   	&delim$;
		pr #2: f$(1)                   	&delim$;
		pr #2: str$(a(1))              	&delim$;
		pr #2: str$(a(2))              	&delim$;
		pr #2: str$(a(3))              	&delim$;
		pr #2: str$(a(4))              	&delim$;
		pr #2: str$(a(5))              	&delim$;
		pr #2: str$(a(6))              	&delim$;
		pr #2: str$(a(7))              	&delim$;
		pr #2: str$(b(1))              	&delim$;
		pr #2: str$(b(2))              	&delim$;
		pr #2: str$(b(3))              	&delim$;
		pr #2: str$(b(4))              	&delim$;
		pr #2: str$(b(5))              	&delim$;
		pr #2: str$(b(6))              	&delim$;
		pr #2: str$(b(7))              	&delim$;
		pr #2: str$(b(8))              	&delim$;
		pr #2: str$(b(9))              	&delim$;
		pr #2: str$(b(10))             	&delim$;
		pr #2: str$(b(11))             	&delim$;
		pr #2: str$(c(1))              	&delim$;
		pr #2: str$(c(2))              	&delim$;
		pr #2: str$(c(3))              	&delim$;
		pr #2: str$(c(4))              	&delim$;
		pr #2: str$(d(1))              	&delim$;
		pr #2: str$(d(2))              	&delim$;
		pr #2: str$(d(3))              	&delim$;
		pr #2: str$(d(4))              	&delim$;
		pr #2: str$(d(5))              	&delim$;
		pr #2: str$(d(6))              	&delim$;
		pr #2: str$(d(7))              	&delim$;
		pr #2: str$(d(8))              	&delim$;
		pr #2: str$(d(9))              	&delim$;
		pr #2: str$(d(10))             	&delim$;
		pr #2: str$(d(11))             	&delim$;
		pr #2: str$(d(12))             	&delim$;
		pr #2: str$(d(13))             	&delim$;
		pr #2: str$(d(14))             	&delim$;
		pr #2: str$(d(15))             	&delim$;
		pr #2: str$(bal)               	&delim$;
		pr #2: str$(f)                 	&delim$;
		pr #2: str$(g(1))              	&delim$;
		pr #2: str$(g(2))              	&delim$;
		pr #2: str$(g(3))              	&delim$;
		pr #2: str$(g(4))              	&delim$;
		pr #2: str$(g(5))              	&delim$;
		pr #2: str$(g(6))              	&delim$;
		pr #2: str$(g(7))              	&delim$;
		pr #2: str$(g(8))             	&delim$;
		pr #2: str$(g(9))             	&delim$;
		pr #2: str$(g(10))            	&delim$;
		pr #2: str$(g(11))            	&delim$;
		pr #2: str$(g(12))            	&delim$;
		pr #2: alp$                   	&delim$;
		pr #2: f$(2)                  	&delim$;
		pr #2: f$(3)                  	&delim$;
		pr #2: extra$(3)               	&delim$;
		pr #2: extra$(4)               	&delim$;
		pr #2: extra$(5)               	&delim$;
		pr #2: str$(bra)               	&delim$;
		pr #2: str$(gb(1))             	&delim$;
		pr #2: str$(gb(2))             	&delim$;
		pr #2: str$(gb(3))             	&delim$;
		pr #2: str$(gb(4))             	&delim$;
		pr #2: str$(gb(5))             	&delim$;
		pr #2: str$(gb(6))             	&delim$;
		pr #2: str$(gb(7))             	&delim$;
		pr #2: str$(gb(8))             	&delim$;
		pr #2: str$(gb(9))             	&delim$;
		pr #2: str$(gb(10))            	&delim$;
		pr #2: ab$(1)                  	&delim$;
		pr #2: ab$(2)                  	&delim$;
		pr #2: ab$(3)                  	&delim$;
		pr #2: str$(finalBillingCode)	&delim$;
		pr #2: addr$(1)                	&delim$;
		pr #2: addr$(2)                	&delim$;
		pr #2: addr$(3)                	&delim$;
		pr #2: addr$(4)                	&delim$;
		pr #2: email$                 	&delim$
		! /r
	loop ! /r
Finis: ! r:
	close #h_customer: ioerr ignore
	close #2: ioerr ignore
	close #3: ioerr ignore
goto Xit ! /r
Xit: !
	fnXit
ALT_BILL_ADR: ! r:
	dim ab$(3)*30
	mat ab$=("")
	read #3,using 'Form POS 11,3*C 30',key=z$: mat ab$ nokey ignore
return  ! /r
HEADER: ! r:
	pr #2: 'Account Key'&delim$;                         ! z$
	pr #2: 'Meter Address'&delim$;                       ! e$(1)
	pr #2: 'Name'&delim$;                                ! e$(2)
	pr #2: 'Address 1 - Primary'&delim$;                 ! e$(3)
	pr #2: 'Address 2 - Primary'&delim$;                 ! extra$(1)
	pr #2: 'CSZ - Primary'&delim$;                       ! e$(4)
	pr #2: serviceName$(1)&' Meter Number'&delim$;       ! f$(1)&delim$;
	pr #2: serviceName$(1)&' Rate Code'&delim$;          ! str$(a(1))&delim$;
	pr #2: serviceName$(2)&' Rate Code'&delim$;          ! str$(a(2))&delim$;
	pr #2: serviceName$(3)&' Rate Code'&delim$;          ! str$(a(3))&delim$;
	pr #2: serviceName$(4)&' Rate Code'&delim$;          ! str$(a(4))&delim$;
	pr #2: serviceName$(5)&' Rate Code'&delim$;          ! str$(a(5))&delim$;
	pr #2: serviceName$(9)&' Rate Code'&delim$;          ! str$(a(6))&delim$;
	pr #2: serviceName$(10)&' Rate Code'&delim$;         ! str$(a(7))&delim$;
	pr #2: serviceName$(1)&' Standard Charge'&delim$;    ! str$(b(1))&delim$;
	pr #2: serviceName$(2)&' Standard Charge'&delim$;    ! str$(b(2))&delim$;
	pr #2: serviceName$(3)&' Standard Charge'&delim$;    ! str$(b(3))&delim$;
	pr #2: serviceName$(4)&' Standard Charge'&delim$;    ! str$(b(4))&delim$;
	pr #2: serviceName$(5)&' Standard Charge'&delim$;    ! str$(b(5))&delim$;
	pr #2: serviceName$(6)&' Standard Charge'&delim$;    ! str$(b(6))&delim$;
	pr #2: serviceName$(7)&' Standard Charge'&delim$;    ! str$(b(7))&delim$;
	pr #2: serviceName$(1)&' Deposit'&delim$;            ! str$(b(8))&delim$;
	pr #2: serviceName$(2)&' Deposit'&delim$;            ! str$(b(9))&delim$;
	pr #2: serviceName$(3)&' Deposit'&delim$;            ! str$(b(10))&delim$;
	pr #2: serviceName$(4)&' Deposit'&delim$;            ! str$(b(11))&delim$;
	pr #2: serviceName$(1)&' Deposit Date'&delim$;       ! str$(c(1))&delim$;
	pr #2: serviceName$(2)&' Deposit Date'&delim$;       ! str$(c(2))&delim$;
	pr #2: serviceName$(3)&' Deposit Date'&delim$;       ! str$(c(3))&delim$;
	pr #2: serviceName$(4)&' Deposit Date'&delim$;       ! str$(c(4))&delim$;
	pr #2: serviceName$(1)&' Reading - Current'&delim$;  ! str$(d(1))&delim$;
	pr #2: serviceName$(1)&' Reading - Prior'&delim$;    ! str$(d(2))&delim$;
	pr #2: serviceName$(1)&' Used - Current'&delim$;     ! str$(d(3))&delim$;
	pr #2: serviceName$(1)&' Used - YTD'&delim$;         ! str$(d(4))&delim$;
	pr #2: serviceName$(3)&' Reading - Current'&delim$;  ! str$(d(5))&delim$;
	pr #2: serviceName$(3)&' Reading - Prior'&delim$;    ! str$(d(6))&delim$;
	pr #2: serviceName$(3)&' KWH Used - Current'&delim$; ! str$(d(7))&delim$;
	pr #2: serviceName$(3)&' KWH Used -YTD'&delim$;      ! str$(d(8))&delim$;
	pr #2: serviceName$(4)&' Reading - Current'&delim$;  ! str$(d(9))&delim$;
	pr #2: serviceName$(4)&' Reading - Prior'&delim$;    ! str$(d(10))&delim$;
	pr #2: serviceName$(4)&' Used-Current'&delim$;       ! str$(d(11))&delim$;
	pr #2: serviceName$(4)&' Used-YTD'&delim$;           ! str$(d(12))&delim$;
	pr #2: 'Units Per Meter'&delim$;                     ! str$(d(13))&delim$;
	pr #2: 'Demand Multiplier'&delim$;                   ! str$(d(14))&delim$;
	pr #2: 'Demand Reading'&delim$;                      ! str$(d(15))&delim$;
	pr #2: 'Current Balance'&delim$;                     ! str$(bal)&delim$;
	pr #2: 'Date of Charge'&delim$;                      ! str$(f)&delim$;
	pr #2: serviceName$(1)&' Charge'&delim$;             ! str$(g(1))&delim$;
	pr #2: serviceName$(2)&' Charge'&delim$;             ! str$(g(2))&delim$;
	pr #2: serviceName$(3)&' Charge'&delim$;             ! str$(g(3))&delim$;
	pr #2: serviceName$(4)&' Charge'&delim$;             ! str$(g(4))&delim$;
	pr #2: serviceName$(5)&' Charge'&delim$;             ! str$(g(5))&delim$;
	pr #2: serviceName$(6)&' Charge'&delim$;             ! str$(g(6))&delim$;
	pr #2: serviceName$(7)&' Charge'&delim$;             ! str$(g(7))&delim$;
	pr #2: serviceName$(8)&' Charge'&delim$;             ! str$(g(8))&delim$;
	pr #2: serviceName$(9)&' Charge'&delim$;             ! str$(g(9))&delim$;
	pr #2: serviceName$(10)&' Charge'&delim$;            ! str$(g(10))&delim$;
	pr #2: 'Current Bill – Net Bill'&delim$;             ! str$(g(11))&delim$;
	pr #2: 'Current Bill – Gross Bill'&delim$;           ! str$(g(12))&delim$;
	pr #2: 'Alpha Sort Field'&delim$;                    ! alp$&delim$;
	pr #2: serviceName$(3)&' Meter Number'&delim$;       ! f$(2)&delim$;
	pr #2: serviceName$(4)&' Meter Number'&delim$;       ! f$(3)&delim$;
	pr #2: serviceName$(1)&' Serial Number'&delim$;      ! extra$(3)&delim$;
	pr #2: serviceName$(3)&' Serial Number'&delim$;      ! extra$(4)&delim$;
	pr #2: serviceName$(4)&' Serial Number'&delim$;      ! extra$(5)&delim$;
	pr #2: 'Alternate Billing Address'&delim$;           ! str$(bra)&delim$;
	pr #2: serviceName$(1)&' Breakdown'&delim$;          ! str$(gb(1))&delim$;
	pr #2: serviceName$(2)&' Breakdown'&delim$;          ! str$(gb(2))&delim$;
	pr #2: serviceName$(3)&' Breakdown'&delim$;          ! str$(gb(3))&delim$;
	pr #2: serviceName$(4)&' Breakdown'&delim$;          ! str$(gb(4))&delim$;
	pr #2: serviceName$(5)&' Breakdown'&delim$;          ! str$(gb(5))&delim$;
	pr #2: serviceName$(6)&' Breakdown'&delim$;          ! str$(gb(6))&delim$;
	pr #2: serviceName$(7)&' Breakdown'&delim$;          ! str$(gb(7))&delim$;
	pr #2: serviceName$(8)&' Breakdown'&delim$;          ! str$(gb(8))&delim$;
	pr #2: serviceName$(9)&' Breakdown'&delim$;          ! str$(gb(9))&delim$;
	pr #2: serviceName$(10)&' Breakdown'&delim$;         ! str$(gb(10))&delim$;
	pr #2: 'Name - Alternate Billing'&delim$;            ! ab$(1)&delim$;
	pr #2: 'Address - Alternate Billing'&delim$;         ! ab$(2)&delim$;
	pr #2: 'CSZ - Alternate Billing'&delim$;             ! ab$(3)
	pr #2: 'Final Billing Code'&delim$;
	pr #2: 'Billing Address 1'&delim$;
	pr #2: 'Billing Address 2'&delim$;
	pr #2: 'Billing Address 3'&delim$;
	pr #2: 'Billing Address 4'&delim$;
	pr #2: 'Email'&delim$
return  ! /r
include: fn_setup
