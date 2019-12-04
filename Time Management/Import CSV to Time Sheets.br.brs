fn_setup
fntop(program$)
! r: Screens
if wbversion$(1:4)<"4.30" then pr "WBVersion is "&wbversion$&" and it must be 4.30 or higher for this program to run" : fnpause

dim filter_date(2)
filter_date(1)=val(date$(days(date$('ccyymm')&'01','ccyymmdd')-1,'ccyymm')&'01') ! low (beginning of last month)
filter_date(2)=date(days(date$('ccyymm')&'01','ccyymmdd')-1,'ccyymmdd') ! high (end of last month)
dim label$(2)*25
label$(1)='Starting Date'
label$(2)='Ending Date'
dim empName$(5)*64
dim filename$(5)*1024
fn_askDatesAndFile(mat label$,mat filter_date,mat empName$,mat filename$)
if fkey=93 or fkey=99 then goto XIT
goto MainBody ! /r
MainBody: ! r: 
fileNameCount=srch(mat filename$,'')-1
for fileItem=1 to fileNameCount
	open #h_in:=fngethandle: 'Name='&env$('at')&filename$(fileItem)&',RecL=100,Shr',external,input
	line_count=0
	the_date_prior=the_date=0
	! gnl_buffer$='' ! fn_getNextLine_reset
	! open #h_in:=fngethandle: 'Name=C:\ACS\Doc\Timesheets\Time Sheet - John Bowman.csv,RecL=100,Shr',external,input
	if fileItem=1 then
		open #h_out:=fngethandle: 'Name=S:\Core\Data\acsllc\TimeSheet.h[cno],RecL=86,KFName=S:\Core\Data\acsllc\TimeSheet-Idx.h[cno],Replace,KPs=1,KLn=5',internal,outIn,keyed
		! pr #255,using FORM_PRN_HEAD: 'Date','Client','Time','Cat','Month','Desc','Rate','Expenses'
		open #h_support:=fngethandle: "Name=S:\Core\Data\acsllc\SUPPORT.h[cno],KFName=S:\Core\Data\acsllc\support-idx.h[cno],Shr",internal,input,keyed
		fnopenprn
		! pr #255: filename$(fileItem)
		FORM_PRN: form pos 1,c 8,x 1,c 18,x 1,n 10.2,n 10,n 10,x 1,c 15,n 7.2,n 8.2
	else
		pr #255: ''
	end if
	pr #255: '_________________________________________________________________________________________'
	pr #255: empName$(fileItem)&' - '&filename$(fileItem)
	pr #255: ''
	pr #255,using FORM_PRN_HEAD: 'Date','Client','Time','Cat','Month','Desc','Rate','Expenses'
	FORM_PRN_HEAD: form pos 1,cc 8,x 1,c 18,x 1,5*cr 10,x 1,c 30,x 1,cr 7,c 8
	FORM_OUT: form pos 1,n 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,c 12,pd 3,c 30
	FMSUPPORT: form pos 1,g 6,n 2,c 2,x 8,x 2,n 8
	dim line$*512

	fn_get_next_line(h_in,line$) : line_count+=1 ! consume headings
	! if fileItem=2 then pr 'header:',line_count,line$ : pause
	if pos(line$,chr$(9))>0 then let delim$=chr$(9) else delim$=','
	do
		fn_get_next_line(h_in,line$) : line_count+=1
		the_date_prior=the_date
		isEmptyLine=fn_lineIsEmpty(line$)
		if ~isEmptyLine then
			dim item$(0)*512
			str2mat(line$,mat item$,delim$,"QUOTES:TRIM")
			if item$(1)<>'' then the_date=fn_get_the_date(item$(1))
			if the_date<the_date_prior and the_date_prior>20151218 then 
				pr file$(h_in)
				pr 'line '&str$(line_count)
				pr 'the_date('&str$(the_date)&')<the_date_prior('&str$(the_date_prior)&') - that indicates a problem'
				pause
			end if
			if the_date=>filter_date(1) and the_date<=filter_date(2) then
				! if fileItem=2 then pr 'body:',line_count,line$ : pause
				if udim(mat item$)>9 and item$(4)<>'#N/A' and val(item$(7))>0 then ! entry
					!       pr the_date;item$(4);' ';item$(7);' ';item$(9);' ';item$(10)
					client_id=val(item$(4))
					! if client_id=970 then pause
					hours=val(item$(7))
					! expense=val(item$(8))
					if rtrm$(item$(13),cr$)<>'' then sage_code$=rtrm$(item$(13),cr$)
					dim description$*512
					description$=item$(12)
					!         if client_id=3811 and the_date=20160716 then pr 'sage_code$='&sage_code$&' date:';the_date : pause
					fn_writeOutAcs(the_date,client_id,hours,val(item$(9)),val(item$(10)),item$(11)(1:30),sage_code$)
					! pr 'write ';writeCount+=1 : pause
					if client_id=client_id_sage_ax then
						fn_writeOutSage(the_date,hours,sage_code$,description$)
						!           pr the_date,hours,description$ : pause
					end if
					pr #255,using FORM_PRN: date$(days(the_date,'ccyymmdd'),'mm/dd/yy'),fnClientNameShort$(client_id),hours,val(item$(9)),val(item$(10)),item$(11)(1:15),inp(4),expense
					fn_clientTimesheet
				end if  ! item$(4)<>'#N/A' and  val(item$(7))>0
			end if  ! the_date=>filter_date(1) and <=filter_date(2)
		end if  ! line$<>''
	loop until isEmptyLine
	close #h_in:
	fn_getNextLine_reset
nex fileItem
fncloseprn
close #h_out:
goto Xit ! /r
XIT: fnxit
def fn_clientTimesheet(; ___,ctFile$*1024,ctNew)
	ctFile$=env$('at')&fnreport_cache_folder_current$&'\Client TimeSheets\'
	ctFile$(inf:inf)=fnClientNameShort$(client_id)&'\'
	ctFile$(inf:inf)=str$(filter_date(1))&'-'&str$(filter_date(2))&'.txt'
	fnmakesurepathexists(ctFile$)
	if ~exists(ctFile$) then ctNew=1
	open #hCt:=fngethandle: 'name='&ctFile$&',RecL=2048,use',display,output
	if ctNew then
		pr #hCt: 'Employee Name'&tab$;
		pr #hCt: 'Date'&tab$;
		pr #hCt: 'hours'&tab$;
		pr #hCt: 'item$(9)'&tab$;
		pr #hCt: 'item$(10)'&tab$;
		pr #hCt: 'item$(11)'&tab$;
		pr #hCt: 'inp(4)'&tab$;
		pr #hCt: 'Origional Entries'&tab$;
		pr #hCt: ''
	end if
  pr #hCt: empname$(fileitem)&tab$;
  pr #hCt: str$(the_date)&tab$;
	pr #hCt: str$(hours)&tab$;
	pr #hCt: item$(9)&tab$;
	pr #hCt: item$(10)&tab$;
	pr #hCt: item$(11)&tab$;
	pr #hCt: str$(inp(4))&tab$;
  pr #hCt: line$

	close #hCt:
fnend
def fn_lineIsEmpty(line$*1024; ___,returnN)
	line$=srep$(line$,'#N/A','')
	line$=srep$(line$,chr$(13),'')
	line$=srep$(line$,chr$(9),'')
	line$=srep$(line$,',','')
	line$=srep$(line$,'0','')
	line$=srep$(line$,'.','')
	line$=srep$(line$,' ','')
	if line$='' then 
		returnN=1
	else
		returnN=0
	end if
	fn_lineIsEmpty=returnN
fnend
def fn_writeOutAcs(wo_date,wo_client,wo_time,wo_cat,wo_month,wo_desc$*30; wo_sage_code$*128)
	dim inp(7)
	inp(1)=wo_client
	inp(2)=1 ! employee number
	inp(3)=wo_time
	!         if wo_client=3811 and wo_date=20160716 then pr 'wo_sage_code$=';wo_sage_code$ : pause
	inp(4)=fn_houryRateAcs(wo_client,the_date,wo_month, wo_cat,wo_sage_code$) ! hourly rate
	inp(5)=wo_time*inp(4)
	inp(6)=date(days(wo_date,'ccyymmdd'),'mmddyy') ! mmddyy
	inp(7)=wo_cat
	if wo_cat=6 then
		sc=601
	else if wo_cat=2 then
		sc=201
	else if wo_cat=11 then
		sc=1101
	else if wo_cat=23 then
		sc=2300
	else
		pr #255: '!!! wo_cat ('&str$(wo_cat)&') is unrecognized - enhance code'
		!   pr 'wo_cat (';wo_cat;') is unrecognized - enhance code' : pause
	end if
	write #h_out,using FORM_OUT: mat inp,0,1,wo_month,sc,'',0,wo_desc$
	! if expense<>0 then
	! 	inp(3)=1
	! 	inp(4)=expense
	! 	sc=601
	! 	wo_month=19
	! 	write #h_out,using FORM_OUT: mat inp,0,1,wo_month,sc,'',0,'Expenses'
	! end if
fnend  ! fn_writeOutAcs
def fn_writeOutSage(wo_date,wo_time,wo_sage_code$*128,wo_desc$*512)
	dim wo_sage_code_prior$*128
	if ~setup_sawo then
		setup_sawo=1
		open #sawo_h_out:=fngethandle: 'Name=[Q]\Sage_AX_'&str$(filter_date(1))&'-'&str$(filter_date(2))&'.csv,RecL=512,eol=crlf,Replace',display,output
	end if
	if wo_sage_code_prior$='' and wo_sage_code$='' then
		pr #255: '!!! Sage Code is blank !!!'
	end if
	if wo_sage_code$='' then
		wo_sage_code$=wo_sage_code_prior$
	else if lwrc$(wo_sage_code$)='glover' then ! Glover Oil
		wo_sage_code$='0052'
	else if lwrc$(wo_sage_code$)='pbj onsite' or lwrc$(wo_sage_code$)='pbj' then ! Payroll Based Journaling
		wo_sage_code$='004H'
	else if lwrc$(wo_sage_code$)='aca onsite' or lwrc$(wo_sage_code$)='file export' then ! ACA file exports
		wo_sage_code$='004J'
	else if lwrc$(wo_sage_code$)='onsite' or lwrc$(wo_sage_code$)='acc hourly' then ! hourly development for ACC
		wo_sage_code$='004T'
	! removed 1/9/2017  !  else if lwrc$(wo_sage_code$)='acc training' then ! Training on ACC
	! removed 1/9/2017  !    wo_sage_code$='004P'
	else if lwrc$(wo_sage_code$)='pbj offsite' then
		wo_sage_code$='004Y'
	else if lwrc$(wo_sage_code$)='aca offsite' or lwrc$(wo_sage_code$)='acc offsite' then
		wo_sage_code$='004Z'
	else if lwrc$(wo_sage_code$)='offsite' then
		wo_sage_code$='004X'
	else
		pr #255: '!!! Sage AX Project Code ('&wo_sage_code$&') is unrecognized - enhance table in csv2tm !!!'
	end if
	wo_sage_code_prior$=wo_sage_code$
	dim sawo_line$*512
	sawo_line$=''
	sawo_line$(inf:inf)=date$(days(wo_date,'ccyymmdd'),'ccyy/mm/dd')&tab$
	sawo_line$(inf:inf)=str$(wo_time)&tab$
	sawo_line$(inf:inf)=wo_sage_code$&tab$
	sawo_line$(inf:inf)=wo_desc$
	pr #sawo_h_out: sawo_line$
fnend  ! fn_writeOutAcs

def fn_get_next_line(h_in,&line$)
	dim gnl_block$*512
	dim gnl_buffer$*32767
	do until pos(gnl_buffer$,crlf$)>0 or gnl_eof
		gnl_block$=''
		read #h_in,using 'form pos 1,C 100': gnl_block$ ioerr GNL_H_IN_READ_IOERR
		gnl_buffer$=gnl_buffer$&gnl_block$
	loop
	pos_crlf=pos(gnl_buffer$,crlf$)
	line$=gnl_buffer$(1:pos_crlf)
	gnl_buffer$(1:pos_crlf+1)=''
	! line$=srep$(line$,cr$,'^') : line$=srep$(line$,lf$,'~')
	! pr 'line='&line$ : pause
	goto GNL_XIT
	GNL_H_IN_READ_IOERR: !
	gnl_block$=gnl_block$&crlf$
	gnl_eof=1
	continue  ! gnl_h_in_read_ioerr
	GNL_XIT: !
fnend
def fn_getNextLine_reset
	gnl_buffer$=''
	gnl_eof=0
fnend
def fn_get_the_date(gtd_source$*256)
	gtd_return=0
	! if pos(gtd_source$,'Thu, Jan 10, 19')>0 then 
	! 	pr gtd_source$
	! 	pause
	! end if
	if gtd_source$<>'' then
	!   pr 'set the_date from '&gtd_source$
		gtd_source$=srep$(gtd_source$,'Mon, ','')
		gtd_source$=srep$(gtd_source$,'Tue, ','')
		gtd_source$=srep$(gtd_source$,'Tues, ','')
		gtd_source$=srep$(gtd_source$,'Wed, ','')
		gtd_source$=srep$(gtd_source$,'Thu, ','')
		gtd_source$=srep$(gtd_source$,'Thur, ','')
		gtd_source$=srep$(gtd_source$,'Thurs, ','')
		gtd_source$=srep$(gtd_source$,'Fri, ','')
		gtd_source$=srep$(gtd_source$,'Sat, ','')
		gtd_source$=srep$(gtd_source$,'Sun, ','')
		gtd_source$=srep$(gtd_source$,'Sun ','')
		gtd_source$=srep$(gtd_source$,'Mon ','')
		gtd_source$=srep$(gtd_source$,'Tue ','')
		gtd_source$=srep$(gtd_source$,'Tues ','')
		gtd_source$=srep$(gtd_source$,'Wed ','')
		gtd_source$=srep$(gtd_source$,'Thu ','')
		gtd_source$=srep$(gtd_source$,'Thur ','')
		gtd_source$=srep$(gtd_source$,'Thurs ','')
		gtd_source$=srep$(gtd_source$,'Fri ','')
		gtd_source$=srep$(gtd_source$,'Sat ','')
		gtd_source$=srep$(gtd_source$,'Sun ','')
		gtd_source$=srep$(gtd_source$,'Sun ','')
		cc12_pos=pos(gtd_source$,', 12')
		cc13_pos=pos(gtd_source$,', 13')
		cc14_pos=pos(gtd_source$,', 14')
		cc15_pos=pos(gtd_source$,', 15')
		cc16_pos=pos(gtd_source$,', 16')
		cc17_pos=pos(gtd_source$,', 17')
		cc18_pos=pos(gtd_source$,', 18')
		cc19_pos=pos(gtd_source$,', 19')
		if cc13_pos<=0 then cc13_pos=pos(gtd_source$,', 2013')
		if cc14_pos<=0 then cc14_pos=pos(gtd_source$,', 2014')
		if cc15_pos<=0 then cc15_pos=pos(gtd_source$,', 2015')
		if cc16_pos<=0 then cc16_pos=pos(gtd_source$,', 2016')
		if cc17_pos<=0 then cc17_pos=pos(gtd_source$,', 2017')
		if cc18_pos<=0 then cc18_pos=pos(gtd_source$,', 2018')
		if cc19_pos<=0 then cc19_pos=pos(gtd_source$,', 2019')
		if cc12_pos>0 then
			gtd_source$(cc12_pos:len(gtd_source$))=''
			gtd_date_ccyy=2012
		else if cc13_pos>0 then
			gtd_source$(cc13_pos:len(gtd_source$))=''
			gtd_date_ccyy=2013
		else if cc14_pos>0 then
			gtd_source$(cc14_pos:len(gtd_source$))=''
			gtd_date_ccyy=2014
		else if cc15_pos>0 then
			gtd_source$(cc15_pos:len(gtd_source$))=''
			gtd_date_ccyy=2015
		else if cc16_pos>0 then
			gtd_source$(cc16_pos:len(gtd_source$))=''
			gtd_date_ccyy=2016
		else if cc17_pos>0 then
			gtd_source$(cc17_pos:len(gtd_source$))=''
			gtd_date_ccyy=2017
		else if cc18_pos>0 then
			gtd_source$(cc18_pos:len(gtd_source$))=''
			gtd_date_ccyy=2018
		else if cc19_pos>0 then
			gtd_source$(cc19_pos:len(gtd_source$))=''
			gtd_date_ccyy=2019
		else
			pr file$(h_in)
			pr 'line '&str$(line_count)
			pr 'unrecognized year - enhance code ('&gtd_source$&')' : pause
		end if  !
		if pos(gtd_source$,'Jan ')>0 then
			gtd_source$=srep$(gtd_source$,'Jan ','')
			gtd_date_mm=01
		else if pos(gtd_source$,'Feb ')>0 then
			gtd_source$=srep$(gtd_source$,'Feb ','')
			gtd_date_mm=02
		else if pos(gtd_source$,'Mar ')>0 then
			gtd_source$=srep$(gtd_source$,'Mar ','')
			gtd_date_mm=03
		else if pos(gtd_source$,'Apr ')>0 then
			gtd_source$=srep$(gtd_source$,'Apr ','')
			gtd_date_mm=04
		else if pos(gtd_source$,'Apr, ')>0 then
			gtd_source$=srep$(gtd_source$,'Apr, ','')
			gtd_date_mm=04
		else if pos(gtd_source$,'May ')>0 then
			gtd_source$=srep$(gtd_source$,'May ','')
			gtd_date_mm=05
		else if pos(gtd_source$,'Jun ')>0 then
			gtd_source$=srep$(gtd_source$,'Jun ','')
			gtd_date_mm=06
		else if pos(gtd_source$,'Jul ')>0 then
			gtd_source$=srep$(gtd_source$,'Jul ','')
			gtd_date_mm=07
		else if pos(gtd_source$,'Aug ')>0 then
			gtd_source$=srep$(gtd_source$,'Aug ','')
			gtd_date_mm=08
		else if pos(gtd_source$,'Sep ')>0 then
			gtd_source$=srep$(gtd_source$,'Sep ','')
			gtd_date_mm=09
		else if pos(gtd_source$,'Oct ')>0 then
			gtd_source$=srep$(gtd_source$,'Oct ','')
			gtd_date_mm=10
		else if pos(gtd_source$,'Nov ')>0 then
			gtd_source$=srep$(gtd_source$,'Nov ','')
			gtd_date_mm=11
		else if pos(gtd_source$,'Dec ')>0 then
			gtd_source$=srep$(gtd_source$,'Dec ','')
			gtd_date_mm=12
		else
			pr 'unrecognized month - enhance code' : pause
		end if
		gtd_date_dd=val(gtd_source$)
		gtd_return=val(str$(gtd_date_ccyy)&cnvrt$('pic(##)',gtd_date_mm)&cnvrt$('pic(##)',gtd_date_dd))
	end if  ! gtd_source$<>''
	fn_get_the_date=gtd_return
fnend
def fn_askDatesAndFile(mat label$,mat filter_date,mat empName$,mat filename$; ___,x)
	fnTos(sn$="ask_"&str$(udim(mat label$))&'_dates')
	respc=0
	for ad_line=1 to udim(mat label$)
		fnLbl(ad_line+1,1,label$(ad_line),25,1)
		fnTxt(ad_line+1,27,8,0,1,"3")
		resp$(respc+=1)=str$(filter_date(ad_line))
	next ad_line
	ad_line+=1
	fnLbl(ad_line+=1,1,'Employee 1:',25,1) : fnTxt(ad_line,27,8,64) : fnTxt(ad_line,37,40,256,0,"70")
	fnLbl(ad_line+=1,1,'Employee 2:',25,1) : fnTxt(ad_line,27,8,64) : fnTxt(ad_line,37,40,256,0,"70")
	fnLbl(ad_line+=1,1,'Employee 3:',25,1) : fnTxt(ad_line,27,8,64) : fnTxt(ad_line,37,40,256,0,"70")
	fnLbl(ad_line+=1,1,'Employee 4:',25,1) : fnTxt(ad_line,27,8,64) : fnTxt(ad_line,37,40,256,0,"70")
	fnLbl(ad_line+=1,1,'Employee 5:',25,1) : fnTxt(ad_line,27,8,64) : fnTxt(ad_line,37,40,256,0,"70")
	fnureg_read('TM Employee 1 Name',resp$(resp_e1name:=respc+=1)) : fnureg_read('TM Employee 1 TimeSheet CSV',resp$(resp_e1_file:=respc+=1))
	fnureg_read('TM Employee 2 Name',resp$(resp_e2name:=respc+=1)) : fnureg_read('TM Employee 2 TimeSheet CSV',resp$(resp_e2_file:=respc+=1))
	fnureg_read('TM Employee 3 Name',resp$(resp_e3name:=respc+=1)) : fnureg_read('TM Employee 3 TimeSheet CSV',resp$(resp_e3_file:=respc+=1))
	fnureg_read('TM Employee 4 Name',resp$(resp_e4name:=respc+=1)) : fnureg_read('TM Employee 4 TimeSheet CSV',resp$(resp_e4_file:=respc+=1))
	fnureg_read('TM Employee 5 Name',resp$(resp_e5name:=respc+=1)) : fnureg_read('TM Employee 5 TimeSheet CSV',resp$(resp_e5_file:=respc+=1))
	fnCmdSet(3)
	fnAcs(sn$,0,mat resp$,ckey)
	if ckey=5 then 
		fkey(99)
	else
		for ad_line=1 to udim(mat label$)
			filter_date(ad_line)=val(srep$(resp$(ad_line),'/',''))
		next ad_line
		mat filename$=('')
		filename$(1)=resp$(resp_e1_file)
		filename$(2)=resp$(resp_e2_file)
		filename$(3)=resp$(resp_e3_file)
		filename$(4)=resp$(resp_e4_file)
		filename$(5)=resp$(resp_e5_file)
		mat empName$=('')
		empName$(1)=resp$(resp_e1name)
		empName$(2)=resp$(resp_e2name)
		empName$(3)=resp$(resp_e3name)
		empName$(4)=resp$(resp_e4name)
		empName$(5)=resp$(resp_e5name)
		for x=1 to 5
			fnureg_write('TM Employee '&str$(x)&' Name',empName$(x)) : fnureg_write('TM Employee '&str$(x)&' TimeSheet CSV',filename$(x))
		nex x
	end if
	fn_askDatesAndFile=ckey
fnend
def fn_onsupport(wo_client,wo_month,the_date)
	os_return=0
	! try lpad first
	spk$=lpad$(str$(wo_client),6)&cnvrt$("n 2",wo_month)
	read #h_support,using FMSUPPORT,key=spk$: cln$,scode,scode$,sdt2 nokey OS_TRY_RPAD
	goto OS_FOUND_REC

	OS_TRY_RPAD: !
	spk$=rpad$(str$(wo_client),6)&cnvrt$("n 2",wo_month)
	read #h_support,using FMSUPPORT,key=spk$: cln$,scode,scode$,sdt2 nokey OS_FINIS
	goto OS_FOUND_REC

	OS_FOUND_REC: !
	if the_date<=sdt2 then os_return=1

	OS_FINIS: !
	fn_onsupport=os_return
fnend
def fn_houryRateAcs(wo_client,the_date,wo_month; hr_category,wo_sage_code$*128) ! inherrits client_id_sage_ax and client_id_brc
	if hr_category=23 or hr_category=11 then
		hr_return=0
	else if wo_client=client_id_brc then
		hr_return=60
	else if wo_client=client_id_sage_ax then
		hr_return=fn_houryRateSage(wo_sage_code$, the_date)
	else if fn_onsupport(wo_client,wo_month,the_date) then
		if hr_category=6 then
			hr_return=0
		else
			hr_return=75
		end if
	else if hr_category=6  and wo_month=31 then  ! support for Collection-Master is currently 100/hour without support
		hr_return=100
	else
		hr_return=150
	end if
	fn_houryRateAcs=hr_return
fnend
def fn_houryRateSage(wo_sage_code$; the_date)
	if lwrc$(wo_sage_code$)='glover' then
		if the_date<20180101 then shr_return=40
	else if lwrc$(wo_sage_code$)='pbj offsite' or lwrc$(wo_sage_code$)='acc offsite' or lwrc$(wo_sage_code$)='offsite' then
		if the_date<20180101 then shr_return=40 else shr_return=45
	else
		if the_date<20180101 then shr_return=48.5 else shr_return=53.5
	end if
	fn_houryRateSage=shr_return
fnend
def fn_setup
	if ~setup then 
		setup=1
		library 'S:\Core\Library': fnAcs,fnCmdSet
		library 'S:\Core\Library': fnLbl,fnTxt,fnTos,fnxit
		library 'S:\Core\Library': fntop
		library 'S:\Core\Library': fnpause
		library 'S:\Core\Library': fngethandle
		library 'S:\Core\Library': fnopenprn,fncloseprn
		library 'S:\Core\Library': fnureg_read,fnureg_write
		library 'S:\Core\Library': fnClientNameShort$
		library 'S:\Core\Library': fnmakesurepathexists
		library 'S:\Core\Library': fnreport_cache_folder_current$
		on error goto Ertn
		gosub Enum
		dim sage_code$*128
		dim resp$(20)*1048
		client_id_sage_ax=3811
		client_id_brc=90
	end if
fnend
include: enum
include: ertn