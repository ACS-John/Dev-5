def library fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$; cell$)
	if wop_setup<>val(env$('cno')) then
		wop_setup=val(env$('cno'))
		library 'S:\Core\Library': fnopenprn,fncloseprn,fnget_services,fnsavetoasstart
		dim srvnam$(10)*20
		fnget_services(mat srvnam$)
		if trim$(srvnam$(3))='Association Fee' then s3_non_metered=1 else s3_non_metered=0
	end if
	!
	fnsavetoasstart("[Q]\WorkOrder\"&trim$(z$)&date$("ccyymmdd")&".rtf")
	fnopenprn( 0,0,0,0,z$,'Work Order Add','Work Order')
	!
	pr #255: "\qc {\f181 {\fs32 {\b Utility Work Order}"
	pr #255: "{\fs24 "&env$('cnam')&"}}}"
	pr #255: "\qc {\fs20 "&trim$(i$(1))&"}"
	pr #255: "\ql "
	pr #255: "{\b                 Name: }{\ul "&e$(2)&"}{\b    Account: }{\ul "&z$&"}"
	pr #255: ""
	pr #255: "{\b      Service Address: }{\ul "&e$(1)&"}"
	pr #255: ""
	if fn_not_blank(i$(6)) then
		pr #255: "{\b                Phone: }"&i$(6)
	end if
	if fn_not_blank(cell$) then
		pr #255: '{\b           Cell Phone: }{\ul '&cell$&'}'
	else
		pr #255: ''
	end if
	
	if fn_not_blank(i$(5)) then
		pr #255: "{\b      Request made by: }"&i$(5)
		pr #255: ""
	end if
	if fn_not_blank(i$(2)) then
		pr #255: "{\b             Taken by: }"&i$(2)
		pr #255: ""
	end if
	if fn_not_blank(i$(7)) then
		pr #255: "{\b              Name In: }"&i$(7)
		pr #255: ""
	end if
	if fn_not_blank(i$(3)) then
		pr #255: "{\b    Date Last Reading: }"&i$(3)
		pr #255: ""
	end if
	if fn_not_blank(i$(4)) then
		pr #255: "{\b Date to be Completed: }"&i$(4)
		pr #255: ""
	end if
	if fn_not_blank(i$(8)) then
		pr #255: "{\b              Turn On: }{\ul "&date$(days(fn_clean_ul$(i$(8),1006),'mmddyy'),'mm/dd/ccyy')&'}'
		pr #255: ""
	end if
	if fn_not_blank(i$(9)) then
		pr #255: "{\b             Turn Off: }{\ul "&date$(days(fn_clean_ul$(i$(9),1006),'mmddyy'),'mm/dd/ccyy')&'}' 
		pr #255: ""
	end if
	if fn_not_blank(i$(10)) then
		pr #255: "{\b             Leave On: }"&i$(10)
		pr #255: ""
	end if
	if fn_not_blank(i$(11)&i$(12)) then
		pr #255: "{\b   Forwarding Address: }"&i$(11)&"  "&i$(12)
		pr #255: ""
	end if
	fn_pwo_service_data(srvnam$(1),d(1),f$(1) ,extra$(3),b(8),a(1)) ! Water
	fn_pwo_service_data(srvnam$(2),na=0,''    ,''       ,b(9) ,a(2) ,1) ! Sewer
	fn_pwo_service_data(srvnam$(3),d(5),f$(2),extra$(4),b(10),a(3),s3_non_metered) ! Electric or Lawn Meter
	fn_pwo_service_data(srvnam$(4),d(9),f$(3),extra$(5),b(11),a(4)) ! Gas
	pr #255: '<--------------------------------Comments-------------------------------->'
	pr #255: ""
	for j=1 to 5
		if trim$(line$(j))<>"" then 
			pr #255: line$(j)
		else 
			pr #255: rpt$("_",74)
		end if 
		pr #255: ""
	next j
	for j=1 to 2
		pr #255: rpt$("_",74)
		pr #255: ""
	next j
	pr #255: ""
	pr #255,using 'form pos 32,c 51': "{\b Date Order Completed: _____________________}"
	pr #255: ""
	pr #255,using 'form pos 32,c 51': "{\b By: _______________________________________}"
	fncloseprn
fnend 
def fn_pwo_service_data(service_name$*80,reading_prior,meter_number$,serial_number$; deposit_amt,rate_code,is_not_metered)
	if trim$(service_name$)<>"" and trim$(service_name$)<>"Capital Surcharge" then 
		dash_len=int((72-len(trim$(service_name$)))/2)
		pr #255: "{\b <"&rpt$('-',dash_len)&trim$(service_name$)&rpt$('-',dash_len)&">}"
		pr #255:   "{\b             Code: }{\ul "&str$(rate_code)&'}'
		pr #255:   "{\b          Deposit: }{\ul "&trim$(cnvrt$('N 8.2',deposit_amt))&"}"
		if trim$(serial_number$)<>'' then 
			pr #255: "{\b    Serial Number: }{\ul "&trim$(serial_number$)&"}"
		end if 
		if ~is_not_metered then ! if not not metered than it is metered
			if trim$(meter_number$)<>'' then 
				pr #255: "{\b     Meter Number: }{\ul "&trim$(meter_number$)&"}"
			end if 
			if reading_prior>0 then 
				pr #255: "{\b Previous Reading: }{\ul "&str$(reading_prior)&"}     {\b  Current Reading: }____________________"
				pr #255: ""
			else
				pr #255:   "                              {\b  Current Reading: }____________________"
				pr #255: ""
			end if 
		end if 
	end if 
fnend 
def fn_not_blank(nbTestText$*256)
	nbReturn=1
	nbTestText$=srep$(nbTestText$,'0','')
	nbTestText$=srep$(nbTestText$,'{\ul ','')
	nbTestText$=srep$(nbTestText$,'}','')
	nbTestText$=srep$(nbTestText$,' ','')
	nbTestText$=srep$(nbTestText$,'_','')
	if nbTestText$='' then
		nbReturn=0
	end if
	fn_not_blank=nbReturn
fnend
def fn_clean_ul$*256(cu_in$*256; cu_reformat)
	cu_len=len(rtrm$(cu_in$))
	if cu_in$(1:5)='{\ul ' and cu_in$(cu_len:cu_len)='}' then 
		cu_in$(cu_len:cu_len)=''
		cu_in$(1:5)=''
	end if
	if cu_reformat=1008 then ! means that it is a date and it may need a leading zero added
		if len(cu_in$)<8 then
			cu_in$='0'&cu_in$
		end if
	end if
	if cu_reformat=1006 then ! means that it is a date and it may need a leading zero added
		if len(cu_in$)<6 then
			cu_in$='0'&cu_in$
		end if
	end if
	fn_clean_ul$=cu_in$
	fnend