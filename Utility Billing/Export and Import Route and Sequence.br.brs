fn_setup
dim serviceName$(10)*20
fnGetServices(mat serviceName$)
for sNitem=1 to udim(mat serviceName$) : serviceName$(sNitem)=trim$(serviceName$(sNitem)) : nex sNitem

fnTop(program$)

do ! r: Main Loop
	dim resp$(3)*256
	fnTos : lc=0
	fnLbl(lc+=1,1,'Path and File Name:',19,1)
	fnTxt(lc,21,40,256,0,'72')
	fnreg_read(env$('cap')&'.path and file',resp$(1), fnSpecialFolderPath$('Desktop')&'\Route and Sequence.txt')
	lc+=1
	fnLbl(lc+=1,1,'Notes:')
	fnLbl(lc   ,7,'Export will overwritten destination.')
	fnLbl(lc+=1,7,'In Excel use a Tab Delimited Files format.')
	fnLbl(lc+=1,7,'Be certain excel treats your account numbers as text or it will drop trailing 0s.')
	fnCmdKey('Export',ckey_export:=1, 0,0,'Export file to modify Route and Sequence numbers')
	fnCmdKey('Import',ckey_import:=2, 0,0,'Import modified Route and Sequence numbers from file')
	fnCmdKey('Exit'  ,5, 0,1)
	fnAcs(mat resp$,ckey)
	if ckey=5 then
		goto Xit
	else
		fnreg_write(env$('cap')&'.path and file',resp$(1))
		if ckey=ckey_export then
			fn_exportRouteAndSequence(resp$(1),tab$)
		else if ckey=ckey_import then
			fn_importRouteAndSequence(resp$(1),tab$)
		end if
	end if
loop ! /r
Xit: fnXit
dim c$(0)*256
dim cN(0)
dim mg$(0)*256,mgResp$*40
def fn_importRouteAndSequence(source$*256,delim$*1; ___,hIn,line$*2048,z$*10,pass,route,sequence,lineCount,failErrorNumber,bkno1$,bkno2$,bkno1,bkno2)
	fncreg_read('Route Low' ,bkno1$) : bkno1=val(bkno1$)
	fncreg_read('Route High',bkno2$) : bkno2=val(bkno2$)

	hCustomer=fn_open('UB Customer',mat c$,mat cN,mat form$)
	open #hIn=fnH: 'Name='&br_filename$(source$),display,input ioerr ImportFail
	dim header$(0)*256
	for pass=1 to 2
		restore #hIn:
		linput #hIn: line$
		str2mat(line$,mat header$,delim$,'Quotes:Trim')
		for x=1 to udim(mat header$)
			header$(x)=trim$(header$(x))
			header$(x)=lwrc$(header$(x))
		nex x
		lineCount=1
		in_acct     =srch(mat header$,'account key')
		in_route    =srch(mat header$,'route'      )
		in_sequence =srch(mat header$,'sequence'   )
		if in_acct<=0 or in_route<=0 or in_sequence<=0 then
			goto ImportFailHeadings
		end if
		do
			linput #hIn: line$ eof ImportEoF
			lineCount+=1
			dim item$(0)*256
			mat item$(0)
			str2mat(line$,mat item$,delim$,'Quotes:Trim')
			z$      =lpad$(trim$(item$(in_acct)),10)
			route   =val(item$(in_route   )) conv ImportFail
			sequence=val(item$(in_sequence)) conv ImportFail
			if route<bkno1 or route>bkno2 then
				goto ImportFailInvalidRoute
			end if
			if ~fnKeyExists(hCustomer,z$, 2) then goto ImportFailAccountKey
			read #hCustomer,using form$(hCustomer),key=z$: mat c$,mat cN ! nokey ImportFail
			cN(c_route   )=route
			cN(c_sequence)=sequence
			if pass=2 then
				rewrite #hCustomer,using form$(hCustomer),key=z$: mat c$,mat cN
			end if
		loop
		ImportEoF: !
	next pass
	close #hIn:
	mat mg$(0)
	fnAddOneC(mat mg$,'Succesfully completed import from:')
	fnAddOneC(mat mg$,source$)
	fnmsgbox(mat mg$,mgResp$,'',mb_information+mb_okonly)

	goto ImportFinis

	ImportFail: ! r:
		failErrorNumber=err
		mat mg$(0)
		fnAddOneC(mat mg$,'File failed to import due to a issue on')
		fnAddOneC(mat mg$,'line '&str$(lineCount)&'.')
		fnAddOneC(mat mg$,'Error: '&str$(failErrorNumber)&'.')
		fnmsgbox(mat mg$,mgResp$,'',mb_stop+mb_okonly)
	goto ImportFinis ! /r
	ImportFailAccountKey: ! r:
		mat mg$(0)
		fnAddOneC(mat mg$,'File failed to import due to an invalid Account Key ('&z$&') on line '&str$(lineCount)&'.')
		fnmsgbox(mat mg$,mgResp$,'',mb_stop+mb_okonly)
	goto ImportFinis ! /r
	ImportFailHeadings: ! r:
		mat mg$(0)
		fnAddOneC(mat mg$,'File failed to import due to improper formatting.')
		fnAddOneC(mat mg$,'The file must be tab delimited.')
		fnAddOneC(mat mg$,'The following headings must be in place:')
		fnAddOneC(mat mg$,tab$&'Account Key')
		fnAddOneC(mat mg$,tab$&'Route')
		fnAddOneC(mat mg$,tab$&'Sequence')
		fnmsgbox(mat mg$,mgResp$,'',mb_stop+mb_okonly)
	goto ImportFinis ! /r
	ImportFailInvalidRoute: ! r:
		mat mg$(0)
		fnAddOneC(mat mg$,'File failed to import due to')
		fnAddOneC(mat mg$,'an invalid route number on line '&str$(lineCount)&'.')
		fnAddOneC(mat mg$,'Valid route numbers must be from '&bkno1$&' to '&bkno2$&')')
		fnAddOneC(mat mg$,'This range can be changed in Company>Configuration.')
		fnmsgbox(mat mg$,mgResp$,'',mb_stop+mb_okonly)
	goto ImportFinis ! /r

	ImportFinis: !
	close #hIn: ioerr ignore
	fnCloseFile(hCustomer,'UB Customer')
fnend
def fn_exportRouteAndSequence(outFile$*256,delim$*1; ___,hCustomer)
	hCustomer=fn_open('UB Customer',mat c$,mat cN,mat form$, 1)
	! open #hCustomer=fnH: 'Name=[Q]\UBmstr\Customer.h[cno],shr',internal,input,relative
	fnMakeSurePathExists(outFile$)
	open #hOut=fnH: 'Name='&br_filename$(outFile$)&',RecL=2500,Replace,EOL=CRLF',display,output
	!  r: Header
	pr #hOut: 'Account Key'                  		&delim$;
	pr #hOut: 'Route'                        		&delim$;
	pr #hOut: 'Sequence'                     		&delim$;
	pr #hOut: 'Meter Address'                		&delim$;
	pr #hOut: 'Name'                         		&delim$;
	pr #hOut: 'Address 1 - Primary'         		&delim$;
	pr #hOut: 'Address 2 - Primary'         		&delim$;
	pr #hOut: 'CSZ - Primary'                		&delim$;
	pr #hOut: serviceName$(1)&' Meter Number'	&delim$;
	pr #hOut: serviceName$(3)&' Meter Number'	&delim$;
	pr #hOut: serviceName$(4)&' Meter Number'	&delim$;
	pr #hOut: serviceName$(1)&' Rate Code'   	&delim$;
	pr #hOut: serviceName$(2)&' Rate Code'   	&delim$;
	pr #hOut: serviceName$(3)&' Rate Code'   	&delim$;
	pr #hOut: serviceName$(4)&' Rate Code'   	&delim$;
	pr #hOut: serviceName$(5)&' Rate Code'   	&delim$;
	pr #hOut: serviceName$(9)&' Rate Code'   	&delim$;
	pr #hOut: serviceName$(10)&' Rate Code'  	&delim$;
	pr #hOut: 'Alpha Sort Field'              	&delim$;
	pr #hOut: ''
	! /r

	do
		read #hCustomer,using form$(hCustomer): mat c$,mat cN eof ExportRouteAndSequence_Finis
		if finalBillingCode=0 or finalBillingCode=3 then
			! r: pr #hOut delimited field values
			pr #hOut: '"'&c$(c_account)&'"'       	&delim$;
			pr #hOut: str$(cN(c_route))           	&delim$;
			pr #hOut: str$(cN(c_sequence))        	&delim$;
			pr #hOut: '"'&c$(c_meterAddress)&'"'  	&delim$;
			pr #hOut: '"'&c$(c_name)&'"'          	&delim$;
			pr #hOut: '"'&c$(c_addr1)&'"'         	&delim$;
			pr #hOut: '"'&c$(c_addr2)&'"'         	&delim$;
			pr #hOut: '"'&c$(c_csz)&'"'           	&delim$;
			pr #hOut: '"'&c$(c_s1meterNumber)&'"' 	&delim$;
			pr #hOut: '"'&c$(c_s03meterNumber)&'"'	&delim$;
			pr #hOut: '"'&c$(c_s04meterNumber)&'"'	&delim$;
			pr #hOut: str$(cN(c_s01rate))         	&delim$;
			pr #hOut: str$(cN(c_s02rate))         	&delim$;
			pr #hOut: str$(cN(c_s03rate))         	&delim$;
			pr #hOut: str$(cN(c_s04rate))         	&delim$;
			pr #hOut: str$(cN(c_s05rate))         	&delim$;
			pr #hOut: str$(cN(c_s09rate))         	&delim$;
			pr #hOut: str$(cN(c_s10rate))         	&delim$;
			pr #hOut: '"'&c$(c_alphaSort)&'"'     	&delim$;
			pr #hOut:''
			! /r
		end if
	loop
	ExportRouteAndSequence_Finis: !
	close #hCustomer: ioerr ignore
	close #hOut: ioerr ignore
	mat mg$(0)
	fnAddOneC(mat mg$,'Succesfully completed export to:')
	fnAddOneC(mat mg$,outFile$)
	fnmsgbox(mat mg$,mgResp$,'',mb_information+mb_okonly)
fnEnd
include: fn_setup
include: fn_open
