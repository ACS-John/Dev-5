! formerly S:\acsUB\ubNamLst
! r: setup library, dims, constants, fnTop, etc
	autoLibrary
	on error goto Ertn
 
	dim resp$(10)*256
	fnTop(program$)
! /r
! MENU1: ! r:
	fnTos("ubnamlst")
	respc=0
	col1_len=18
	col2_pos=col1_len+2
	fnLbl(2,1,"Rate to Export:",col1_len,1)
	fnComboF("nerd",2,col2_pos,55,"[Q]\UBmstr\ubData\RateMst.h[cno]",1,4,5,50,"[Q]\UBmstr\ubData\RateIdx1.h[cno]",1)
	resp$(resp_rate:=respc+=1)=''
	fnChk(4,1,'include Final Billed')
	resp$(resp_enableFinaled:=respc+=1)='false'
	fnLbl(6,1,"Filename:",col1_len,1)
	fnTxt(6,col2_pos,42,256,0,'70',0,'Choose the output file name. [Rate Code] will be replaced with the rate code selected.')
	resp$(resp_fileOut:=respc+=1)=env$('Desktop')&'\ACS-Customer-[Rate Code].txt'
	fnCmdSet(2)
	fnAcs(mat resp$,ckey)
	if ckey=5 then goto Xit
	if resp$(resp_enableFinaled)='True' then enableFinaled=1 else enableFinaled=0
	dim fileOut$*256
	fileOut$=resp$(resp_fileOut)
	dim rateCode$*256
	rateCode$=resp$(resp_rate)
	fileOut$=srep$(fileOut$,'[Rate Code]',rateCode$(1:4))
	enableFinaled$=resp$(resp_enableFinaled)
goto MainLoop ! /r
MainLoop: ! r:
	dim c$(0)*256
	dim cN(0)
	hCustomer=fn_open('UB Customer',mat c$,mat cN,mat form$)
	open #hOut:=fngethandle: 'name='&fileOut$&',recl=2048,eol=crlf,replace',d,o
	! r: Header
	delim$=chr$(9)
	pr #hOut: rpad$('Account   '       ,10)&delim$;
	pr #hOut: rpad$('Name'             ,30)&delim$;
	pr #hOut: rpad$('Billing Address 1',30)&delim$;
	pr #hOut: rpad$('Billing Address 2',30)&delim$;
	pr #hOut: rpad$('Billing Address 3',30)&delim$;
	pr #hOut: rpad$('Billing Address 4',30)
	! /r
	do
		read #hCustomer,using form$(hCustomer): mat c$,mat cN eof DONE
		if enableFinaled$='True' or cN(c_finalBilling)=0 then
			! pr "c$(c_account)="&c$(c_account)
			! pr "rateCode$="&rateCode$
			! pr "fnCustomerData$(c$(c_account),rateCode$(1:2)&' rate code')="&fnCustomerData$(c$(c_account),rateCode$(1:2)&' rate code')
			! pause
			
			if trim$(rateCode$)='' or trim$(rateCode$(3:4))=fnCustomerData$(c$(c_account),rateCode$(1:2)&' rate code') then
				pr #hOut: rpad$(c$(c_account)                                        ,10)&delim$;
				pr #hOut: rpad$(c$(c_name)                                           ,30)&delim$;
				pr #hOut: rpad$(fnCustomerData$(c$(c_account),'billing address 1', 1),30)&delim$;
				pr #hOut: rpad$(fnCustomerData$(c$(c_account),'billing address 2', 1),30)&delim$;
				pr #hOut: rpad$(fnCustomerData$(c$(c_account),'billing address 3', 1),30)&delim$;
				pr #hOut: rpad$(fnCustomerData$(c$(c_account),'billing address 4', 1),30)
			end if
		end if
	loop
! /r
 
DONE: ! r:
	fnCustomerData$('',''   )
	close #hOut: ioerr ignore
	fnclosefile(hCustomer,'UB Customer')
	dim ml$(0)*256
	mat ml$(2)
	ml$(1)=env$('program_caption')&' successfully completed.'
	ml$(2)=fileOut$
	fnmsgbox(mat ml$,resp$,"ACS",0)
goto Xit ! /r
Xit: fnXit
include: Ertn
include: fn_open
