20000   def library fnWorkOrderPrint(z$,mat e$,mat i$,mat line$,mat a,mat b,mat d,mat f$,mat extra$)
20020     if wop_setup<>val(env$('cno')) then
20040       wop_setup=val(env$('cno'))
20060       library 'S:\Core\Library': fnopenprn,fncloseprn,fnget_services,fnsavetoasstart
20080       dim srvnam$(10)*20
20100       fnget_services(mat srvnam$)
20120       if trim$(srvnam$(3))='Association Fee' then let s3_non_metered=1 else s3_non_metered=0
20140     end if
24040     !
26000     let fnsavetoasstart(env$('Q')&"\WorkOrder\"&trim$(z$)&date$("ccyymmdd")&".rtf")
26020     let fnopenprn( 0,0,0,0,z$,'Work Order Add','Work Order')
26040     !
28000     pr #255: "\qc {\f181 {\fs32 {\b Utility Work Order}"
28020     pr #255: "{\fs24 "&env$('cnam')&"}}}"
28040     pr #255: "\qc {\fs20 "&trim$(i$(1))&"}"
28060     pr #255: "\ql "
28080     pr #255: "{\b                 Name: }{\ul "&e$(2)&"}{\b    Account: }{\ul "&z$&"}"
28100     pr #255: ""
28120     pr #255: "{\b      Service Address: }{\ul "&e$(1)&"}"
28140     pr #255: ""
28160     if fn_not_blank(i$(6)) then
28180       pr #255: "{\b                Phone: }"&i$(6)
28200       pr #255: ""
28220     end if
28240     if fn_not_blank(i$(5)) then
28260       pr #255: "{\b      Request made by: }"&i$(5)
28280       pr #255: ""
28300     end if
28320     if fn_not_blank(i$(2)) then
28340       pr #255: "{\b             Taken by: }"&i$(2)
28360       pr #255: ""
28380     end if
28400     if fn_not_blank(i$(7)) then
28420       pr #255: "{\b              Name In: }"&i$(7)
28440       pr #255: ""
28460     end if
28480     if fn_not_blank(i$(3)) then
28500       pr #255: "{\b    Date Last Reading: }"&i$(3)
28520       pr #255: ""
28540     end if
28560     if fn_not_blank(i$(4)) then
28580       pr #255: "{\b Date to be Completed: }"&i$(4)
28600       pr #255: ""
28620     end if
28640     if fn_not_blank(i$(8)) then
28660       pr #255: "{\b              Turn On: }{\ul "&date$(days(fn_clean_ul$(i$(8),1008),'mmddyy'),'mm/dd/ccyy')&'}'
28680       pr #255: ""
28700     end if
28720     if fn_not_blank(i$(9)) then
28740       pr #255: "{\b             Turn Off: }{\ul "&date$(days(fn_clean_ul$(i$(9),1008),'mmddyy'),'mm/dd/ccyy')&'}'
28760       pr #255: ""
28780     end if
28800     if fn_not_blank(i$(10)) then
28820       pr #255: "{\b             Leave On: }"&i$(10)
28840       pr #255: ""
28860     end if
28880     if fn_not_blank(i$(11)&i$(12)) then
28900       pr #255: "{\b   Forwarding Address: }"&i$(11)&"  "&i$(12)
28920       pr #255: ""
28940     end if

32000     fn_pwo_service_data(srvnam$(1),d(1),f$(1) ,extra$(3),b(8),a(1)) ! Water
32020     fn_pwo_service_data(srvnam$(2),na=0,''    ,''       ,b(9) ,a(2) ,1) ! Sewer
32040     fn_pwo_service_data(srvnam$(3),d(5),f$(2),extra$(4),b(10),a(3),s3_non_metered) ! Electric or Lawn Meter
32060     fn_pwo_service_data(srvnam$(4),d(9),f$(3),extra$(5),b(11),a(4)) ! Gas
32080     pr #255: '<--------------------------------Comments-------------------------------->'
32120     pr #255: ""
32160     for j=1 to 5
32180       if trim$(line$(j))<>"" then 
32200         pr #255: line$(j)
32220       else 
32240         pr #255: rpt$("_",74)
32260       end if 
32280       pr #255: ""
32300     next j
32320     for j=1 to 2
32340       pr #255: rpt$("_",74)
32360       pr #255: ""
32380     next j
32400     pr #255: ""
32420     pr #255,using 'form pos 32,c 51': "{\b Date Order Completed: _____________________}"
32440     pr #255: ""
32460     pr #255,using 'form pos 32,c 51': "{\b By: _______________________________________}"
32480     let fncloseprn
32500   fnend 

40000   def fn_pwo_service_data(service_name$*80,reading_prior,meter_number$,serial_number$; deposit_amt,rate_code,is_not_metered)
40020     if trim$(service_name$)<>"" then 
40040       dash_len=int((72-len(trim$(service_name$)))/2)
40060       pr #255: "{\b <"&rpt$('-',dash_len)&trim$(service_name$)&rpt$('-',dash_len)&">}"
40080       pr #255:   "{\b             Code: }{\ul "&str$(rate_code)&'}'
40100       pr #255:   "{\b          Deposit: }{\ul "&trim$(cnvrt$('N 8.2',deposit_amt))&"}"
40120       if trim$(serial_number$)<>'' then 
40140         pr #255: "{\b    Serial Number: }{\ul "&trim$(serial_number$)&"}"
40160       end if 
40180       if ~is_not_metered then ! if not not metered than it is metered
40200         if trim$(meter_number$)<>'' then 
40220           pr #255: "{\b     Meter Number: }{\ul "&trim$(meter_number$)&"}"
40240         end if 
40260         if reading_prior>0 then 
40280           pr #255: "{\b Previous Reading: }{\ul "&str$(reading_prior)&"}     {\b  Current Reading: }____________________"
40300           pr #255: ""
40320         else
40340           pr #255:   "                              {\b  Current Reading: }____________________"
40360           pr #255: ""
40380         end if 
40400       end if 
40420     end if 
40440   fnend 

42000 def fn_not_blank(nbTestText$*256)
42010   nbReturn=1
42020   nbTestText$=srep$(nbTestText$,'0','')
42030   nbTestText$=srep$(nbTestText$,'{\ul ','')
42040   nbTestText$=srep$(nbTestText$,'}','')
42050   nbTestText$=srep$(nbTestText$,' ','')
42060   nbTestText$=srep$(nbTestText$,'_','')
42080   if nbTestText$='' then
42100     nbReturn=0
42120   end if
42140   fn_not_blank=nbReturn
42160 fnend
50000 def fn_clean_ul$*256(cu_in$*256; cu_reformat)
50020   cu_len=len(rtrm$(cu_in$))
50040   if cu_in$(1:5)='{\ul ' and cu_in$(cu_len:cu_len)='}' then 
50060     cu_in$(cu_len:cu_len)=''
50080     cu_in$(1:5)=''
50100   end if
50120   if cu_reformat=1008 then ! means that it is a date and it may need a leading zero added
50140     if len(cu_in$)<8 then
50160       cu_in$='0'&cu_in$
50180     end if
50200   end if
50220   fn_clean_ul$=cu_in$
50240  fnend
