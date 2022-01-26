fn_setup
fnTop(program$)

! r: Main Screen
do
	dim resp$(10)*256
	fnTos : lc=rc=0
	lc+=3
	mat tableOption$(0) : mat tableRc(0) : mat tableRcEnable(0)
	table_Customer=fn_addExportOption('Customer'     	,rc_cust,rc_custEnable)
	lc+=1
	                 fn_addExportOption('Transaction' 	,rc_tran,rc_tranEnable)
	                 fn_addExportOption('Rate'         	,rc_rate,rc_rateEnable)
	lc+=1
	! fnLbl(lc+=1,1,'Delimiter (ASCII Code):' ,34,1) : fnTxt(lc,36,3,0,0,'30') : resp$(rc_delim=rc+=1)='9'
	lc+=1
	fnLbl(lc+=1,1,'NOTE: If Destination exists it will be overwritten.',76,2)
	fnCmdSet(2)
	ckey=fnAcs(mat resp$)
	if ckey<>5 then 
		for tableItem=1 to udim(mat tableOption$)
			fnureg_write('Export '&tableOption$(tableItem)&' Path'      	,resp$(tableRc(tableItem)))
			fnureg_write('Export '&tableOption$(tableItem)&' Enabled'   	,resp$(tableRcEnable(tableItem)))
		nex tableItem
		
		dim delim$*1
		delim$=tab$  	! delim$=chr$(val(resp$(rc_delim)))
		
		for tableItem=1 to udim(mat tableOption$)
			if resp$(tableRcEnable(tableItem))='True' then 
				if tableItem=table_Customer then fn_exportCustomer(resp$(rc_cust),delim$) : goto NexTable !   rc_cust==tableRc(tableItem)   could use either here
				fn_exportFio('UB '&tableOption$(tableItem),resp$(tableRc(tableItem)))
			end if
			NexTable: !
		nex tableItem

	end if
	
	
loop until ckey=5 
goto Xit! /r
	def fn_addExportOption(tableName$,&rc_table,&rc_tableEnable) ! extremely local
		fnLbl(lc+=1, 1,tableName$&' Export File:',22,1)
		fnTxt(lc   ,24,40,256,0,'72') 	: fnureg_read('Export '&tableName$&' Path'   	,resp$(rc_table=rc+=1)       	, fnSpecialFolderPath$('Desktop')&'\'&lwrc$(tableName$)&'.txt',1)
		fnChk(lc   ,80,'enable',1)   	: fnureg_read('Export '&tableName$&' Enabled'	,resp$(rc_tableEnable=rc+=1)	, 'True')
		fnAddOneC(mat tableOption$,tableName$) : fnAddOneN(mat tableRc,rc_table) : fnAddOneN(mat tableRcEnable,rc_tableEnable)
		fn_addExportOption=udim(mat tableOption$)
	fnend
Xit: fnXit

def fn_exportCustomer(dest$*256,delim$*1; ___, _
	hCustomer,hOut,hAdrBil, _
	z$*10,email$*30)

	open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],shr',i,outi,r
	! open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno]',i,i,r
	fnMakeSurePathExists(dest$)
	open #hOut:=fnH: 'Name='&br_filename$(dest$)&',RecL=2500,Replace,EOL=CRLF',d,o ioerr EoExportCustomer
	! form pos 1,c 14,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 4,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1,n 8.2,c 1
	! form pos 287,c 10,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 30,c 1,c 12,c 1
	open #hAdrBil=fnH: 'Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr',i,i,k
	fn_exportCustomerHeader(hOut,delim$)
	do ! r:
		dim e$(4)*30
		dim f$(3)*12
		dim a(7)
		dim xb(11)
		dim c(4)
		dim xd(15)
		dim g(12)
		dim adr(2)
		dim alp$*7
		dim gb(10)
		dim extra$(11)*30
		read #hCustomer,using F_CUSTOMER: z$,mat e$,f$(1),mat a,mat xb,mat c,mat xd,bal,xf,mat g,mat adr,alp$,f$(2),f$(3),bra,mat gb,finalBillingCode,mat extra$,email$ eof EoExportCustomer
		F_CUSTOMER: form pos 1,c 10,4*c 30,c 12,7*pd 2,11*pd 4.2,4*pd 4,15*pd 5,pd 4.2,pd 4,12*pd 4.2,2*pd 3,c 7,2*c 12,pd 3,10*pd 5.2,pos 1821,n 1,pos 1864,c 30,c 12,c 12,c 12,c 12,c 12,c 12,c 12,c 30,c 30,c 30,pos 1978,c 30

		dim ab$(3)*30
		mat ab$=('')
		read #hAdrBil,using 'form pos 11,3*C 30',key=z$: mat ab$ nokey ignore

		dim addr$(4)*40
		fncustomer_address(z$,mat addr$)
		! r: pr #hOut delimited field values
		pr #hOut: '"'&z$&'"'               	&delim$;
		pr #hOut: e$(1)                     	&delim$;
		pr #hOut: e$(2)                     	&delim$;
		pr #hOut: e$(3)                     	&delim$;
		pr #hOut: extra$(1)                	&delim$;
		pr #hOut: e$(4)                     	&delim$;
		pr #hOut: f$(1)                     	&delim$;
		pr #hOut: str$(a(1))               	&delim$;
		pr #hOut: str$(a(2))               	&delim$;
		pr #hOut: str$(a(3))               	&delim$;
		pr #hOut: str$(a(4))               	&delim$;
		pr #hOut: str$(a(5))               	&delim$;
		pr #hOut: str$(a(6))               	&delim$;
		pr #hOut: str$(a(7))               	&delim$;
		pr #hOut: str$(xb(1))              	&delim$;
		pr #hOut: str$(xb(2))              	&delim$;
		pr #hOut: str$(xb(3))              	&delim$;
		pr #hOut: str$(xb(4))              	&delim$;
		pr #hOut: str$(xb(5))              	&delim$;
		pr #hOut: str$(xb(6))              	&delim$;
		pr #hOut: str$(xb(7))              	&delim$;
		pr #hOut: str$(xb(8))              	&delim$;
		pr #hOut: str$(xb(9))              	&delim$;
		pr #hOut: str$(xb(10))             	&delim$;
		pr #hOut: str$(xb(11))             	&delim$;
		pr #hOut: str$(c(1))               	&delim$;
		pr #hOut: str$(c(2))               	&delim$;
		pr #hOut: str$(c(3))               	&delim$;
		pr #hOut: str$(c(4))               	&delim$;
		pr #hOut: str$(xd(1))              	&delim$;
		pr #hOut: str$(xd(2))              	&delim$;
		pr #hOut: str$(xd(3))              	&delim$;
		pr #hOut: str$(xd(4))              	&delim$;
		pr #hOut: str$(xd(5))              	&delim$;
		pr #hOut: str$(xd(6))              	&delim$;
		pr #hOut: str$(xd(7))              	&delim$;
		pr #hOut: str$(xd(8))              	&delim$;
		pr #hOut: str$(xd(9))              	&delim$;
		pr #hOut: str$(xd(10))             	&delim$;
		pr #hOut: str$(xd(11))             	&delim$;
		pr #hOut: str$(xd(12))             	&delim$;
		pr #hOut: str$(xd(13))             	&delim$;
		pr #hOut: str$(xd(14))             	&delim$;
		pr #hOut: str$(xd(15))             	&delim$;
		pr #hOut: str$(bal)                	&delim$;
		pr #hOut: str$(xf)                 	&delim$;
		pr #hOut: str$(g(1))               	&delim$;
		pr #hOut: str$(g(2))               	&delim$;
		pr #hOut: str$(g(3))               	&delim$;
		pr #hOut: str$(g(4))               	&delim$;
		pr #hOut: str$(g(5))               	&delim$;
		pr #hOut: str$(g(6))               	&delim$;
		pr #hOut: str$(g(7))               	&delim$;
		pr #hOut: str$(g(8))               	&delim$;
		pr #hOut: str$(g(9))               	&delim$;
		pr #hOut: str$(g(10))              	&delim$;
		pr #hOut: str$(g(11))              	&delim$;
		pr #hOut: str$(g(12))              	&delim$;
		pr #hOut: alp$                     	&delim$;
		pr #hOut: f$(2)                     	&delim$;
		pr #hOut: f$(3)                     	&delim$;
		pr #hOut: extra$(3)                	&delim$;
		pr #hOut: extra$(4)                	&delim$;
		pr #hOut: extra$(5)                	&delim$;
		pr #hOut: str$(bra)                	&delim$;
		pr #hOut: str$(gb(1))              	&delim$;
		pr #hOut: str$(gb(2))              	&delim$;
		pr #hOut: str$(gb(3))              	&delim$;
		pr #hOut: str$(gb(4))              	&delim$;
		pr #hOut: str$(gb(5))              	&delim$;
		pr #hOut: str$(gb(6))              	&delim$;
		pr #hOut: str$(gb(7))              	&delim$;
		pr #hOut: str$(gb(8))              	&delim$;
		pr #hOut: str$(gb(9))              	&delim$;
		pr #hOut: str$(gb(10))             	&delim$;
		pr #hOut: ab$(1)                   	&delim$;
		pr #hOut: ab$(2)                   	&delim$;
		pr #hOut: ab$(3)                   	&delim$;
		pr #hOut: str$(finalBillingCode)  	&delim$;
		pr #hOut: addr$(1)                 	&delim$;
		pr #hOut: addr$(2)                 	&delim$;
		pr #hOut: addr$(3)                 	&delim$;
		pr #hOut: addr$(4)                 	&delim$;
		pr #hOut: email$                   	&delim$
		! /r
	loop ! /r
	EoExportCustomer: !
	close #hCustomer: ioerr ignore
	close #hOut: ioerr ignore
	close #hAdrBil: ioerr ignore
fnend
	def fn_exportCustomerHeader(hOut,delim$; ___,sNitem)
		if ~setupCustomerHeader then
			setupCustomerHeader=1
			dim serviceName$(10)*20
			fnGetServices(mat serviceName$)
			for sNitem=1 to udim(mat serviceName$) : serviceName$(sNitem)=trim$(serviceName$(sNitem)) : nex sNitem
		end if
	
		pr #hOut: 'Account Key'&delim$;                           	! z$
		pr #hOut: 'Meter Address'&delim$;                         	! e$(1)
		pr #hOut: 'Name'&delim$;                                   	! e$(2)
		pr #hOut: 'Address 1 - Primary'&delim$;                   	! e$(3)
		pr #hOut: 'Address 2 - Primary'&delim$;                  	! extra$(1)
		pr #hOut: 'CSZ - Primary'&delim$;                         	! e$(4)
		pr #hOut: serviceName$(1)&' Meter Number'&delim$;        	! f$(1)
		pr #hOut: serviceName$(1)&' Rate Code'&delim$;           	! a(1)
		pr #hOut: serviceName$(2)&' Rate Code'&delim$;           	! a(2)
		pr #hOut: serviceName$(3)&' Rate Code'&delim$;           	! a(3)
		pr #hOut: serviceName$(4)&' Rate Code'&delim$;           	! a(4)
		pr #hOut: serviceName$(5)&' Rate Code'&delim$;           	! a(5)
		pr #hOut: serviceName$(9)&' Rate Code'&delim$;           	! a(6)
		pr #hOut: serviceName$(10)&' Rate Code'&delim$;          	! a(7)
		pr #hOut: serviceName$(1)&' Standard Charge'&delim$;    	! xb(1)
		pr #hOut: serviceName$(2)&' Standard Charge'&delim$;    	! xb(2)
		pr #hOut: serviceName$(3)&' Standard Charge'&delim$;    	! xb(3)
		pr #hOut: serviceName$(4)&' Standard Charge'&delim$;    	! xb(4)
		pr #hOut: serviceName$(5)&' Standard Charge'&delim$;    	! xb(5)
		pr #hOut: serviceName$(6)&' Standard Charge'&delim$;    	! xb(6)
		pr #hOut: serviceName$(7)&' Standard Charge'&delim$;    	! xb(7)
		pr #hOut: serviceName$(1)&' Deposit'&delim$;             	! xb(8)
		pr #hOut: serviceName$(2)&' Deposit'&delim$;             	! xb(9)
		pr #hOut: serviceName$(3)&' Deposit'&delim$;             	! xb(10)
		pr #hOut: serviceName$(4)&' Deposit'&delim$;             	! xb(11)
		pr #hOut: serviceName$(1)&' Deposit Date'&delim$;        	! c(1)
		pr #hOut: serviceName$(2)&' Deposit Date'&delim$;        	! c(2)
		pr #hOut: serviceName$(3)&' Deposit Date'&delim$;        	! c(3)
		pr #hOut: serviceName$(4)&' Deposit Date'&delim$;        	! c(4)
		pr #hOut: serviceName$(1)&' Reading - Current'&delim$;   	! xd(1)
		pr #hOut: serviceName$(1)&' Reading - Prior'&delim$;     	! xd(2)
		pr #hOut: serviceName$(1)&' Used - Current'&delim$;      	! xd(3)
		pr #hOut: serviceName$(1)&' Used - YTD'&delim$;          	! xd(4)
		pr #hOut: serviceName$(3)&' Reading - Current'&delim$;   	! xd(5)
		pr #hOut: serviceName$(3)&' Reading - Prior'&delim$;     	! xd(6)
		pr #hOut: serviceName$(3)&' KWH Used - Current'&delim$;  	! xd(7)
		pr #hOut: serviceName$(3)&' KWH Used -YTD'&delim$;       	! xd(8)
		pr #hOut: serviceName$(4)&' Reading - Current'&delim$;   	! xd(9)
		pr #hOut: serviceName$(4)&' Reading - Prior'&delim$;     	! xd(10)
		pr #hOut: serviceName$(4)&' Used-Current'&delim$;        	! xd(11)
		pr #hOut: serviceName$(4)&' Used-YTD'&delim$;            	! xd(12)
		pr #hOut: 'Units Per Meter'&delim$;                       	! xd(13)
		pr #hOut: 'Demand Multiplier'&delim$;                    	! xd(14)
		pr #hOut: 'Demand Reading'&delim$;                        	! xd(15)
		pr #hOut: 'Current Balance'&delim$;                      	! bal
		pr #hOut: 'Date of Charge'&delim$;                        	! xf
		pr #hOut: serviceName$(1)&' Charge'&delim$;              	! g(1)
		pr #hOut: serviceName$(2)&' Charge'&delim$;              	! g(2)
		pr #hOut: serviceName$(3)&' Charge'&delim$;              	! g(3)
		pr #hOut: serviceName$(4)&' Charge'&delim$;              	! g(4)
		pr #hOut: serviceName$(5)&' Charge'&delim$;              	! g(5)
		pr #hOut: serviceName$(6)&' Charge'&delim$;              	! g(6)
		pr #hOut: serviceName$(7)&' Charge'&delim$;              	! g(7)
		pr #hOut: serviceName$(8)&' Charge'&delim$;              	! g(8)
		pr #hOut: serviceName$(9)&' Charge'&delim$;              	! g(9)
		pr #hOut: serviceName$(10)&' Charge'&delim$;             	! g(10)
		pr #hOut: 'Current Bill – Net Bill'&delim$;              	! g(11)
		pr #hOut: 'Current Bill – Gross Bill'&delim$;            	! g(12)
		pr #hOut: 'Alpha Sort Field'&delim$;                      	! alp$
		pr #hOut: serviceName$(3)&' Meter Number'&delim$;        	! f$(2)
		pr #hOut: serviceName$(4)&' Meter Number'&delim$;        	! f$(3)
		pr #hOut: serviceName$(1)&' Serial Number'&delim$;       	! extra$(3)
		pr #hOut: serviceName$(3)&' Serial Number'&delim$;       	! extra$(4)
		pr #hOut: serviceName$(4)&' Serial Number'&delim$;       	! extra$(5)
		pr #hOut: 'Alternate Billing Address'&delim$;            	! bra
		pr #hOut: serviceName$(1)&' Breakdown'&delim$;           	! gb(1)
		pr #hOut: serviceName$(2)&' Breakdown'&delim$;           	! gb(2)
		pr #hOut: serviceName$(3)&' Breakdown'&delim$;           	! gb(3)
		pr #hOut: serviceName$(4)&' Breakdown'&delim$;           	! gb(4)
		pr #hOut: serviceName$(5)&' Breakdown'&delim$;           	! gb(5)
		pr #hOut: serviceName$(6)&' Breakdown'&delim$;           	! gb(6)
		pr #hOut: serviceName$(7)&' Breakdown'&delim$;           	! gb(7)
		pr #hOut: serviceName$(8)&' Breakdown'&delim$;           	! gb(8)
		pr #hOut: serviceName$(9)&' Breakdown'&delim$;           	! gb(9)
		pr #hOut: serviceName$(10)&' Breakdown'&delim$;          	! gb(10)
		pr #hOut: 'Name - Alternate Billing'&delim$;             	! ab$(1)
		pr #hOut: 'Address - Alternate Billing'&delim$;          	! ab$(2)
		pr #hOut: 'CSZ - Alternate Billing'&delim$;              	! ab$(3)
		pr #hOut: 'Final Billing Code'&delim$;
		pr #hOut: 'Billing Address 1'&delim$;
		pr #hOut: 'Billing Address 2'&delim$;
		pr #hOut: 'Billing Address 3'&delim$;
		pr #hOut: 'Billing Address 4'&delim$;
		pr #hOut: 'Email'&delim$
	fnend
def fn_exportFio(fioId$*64,dest$*300; ___,dialogType,includeRecNums,KeyNumber,StartKey$,KeyMatch$,Startrec,mat Records,SearchMatch$,Launch)
	! pr 'write '&fioId$&' export' : pause
	dialogType=1
	includeRecNums 	=1
	KeyNumber       	=0
	StartKey$       	=''
	KeyMatch$       	=''
	Startrec        	=0
	! mat Records    	=
	SearchMatch$    	=''
	Launch           	=0
	fnFileIoExport(fioId$, dialogType,dest$,includeRecNums,KeyNumber,StartKey$,KeyMatch$,Startrec,mat Records,SearchMatch$,Launch)
fnend
include: fn_setup
