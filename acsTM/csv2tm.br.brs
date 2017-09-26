00400 ! r: setup
10000   library 'S:\Core\Library': fnacs,fncmdset,fnlbl,fntxt,fnerror,fntos,fncno,fnxit,fntop,fnpause,fngethandle,fnopenprn,fncloseprn
10200   on error goto ERTN
10600   dim cap$*128
10800   dim line$*512,item$(1)*512
11000   dim cnam$*40
11020   dim sage_code$*128
11400 ! constants
11600   let cr$=chr$(13) : let lf$=chr$(10) : let tab$=chr$(9)
11800   let crlf$=cr$&lf$
12000   let fncno(cno,cnam$)
12400 ! 
12600   let fntop(program$,cap$="Import CSV to Time Sheets")
12800   if wbversion$(1:4)<"4.30" then print "WBVersion is "&wbversion$&" and it must be 4.30 or higher for this program to run" : let fnpause
12820   let client_id_sage_ax=3811
12822   let client_id_brc=90
13000 ! ______________________________________________________________________
13200   let filter_date(1)=val(date$(days(date$('ccyymm')&'01','ccyymmdd')-1,'ccyymm')&'01') ! low (beginning of last month)
13400   let filter_date(2)=date(days(date$('ccyymm')&'01','ccyymmdd')-1,'ccyymmdd') ! high (end of last month)
13600 ! /r
13800   dim label$(2)*25,filter_date(2)
14000   let label$(1)='Starting Date'
14200   let label$(2)='Ending Date'
14400   let fn_ask_dates(mat label$,mat filter_date)
14600   if fkey=93 or fkey=99 then goto XIT
14800   open #h_in:=fngethandle: 'Name=C:\ACS\Doc\Timesheets\Time Sheet - John Bowman.csv,RecL=100,Shr',external,input 
15000   open #h_out:=fngethandle: "Name="&env$('Q')&"\TMmstr\TimeSheet.h"&str$(cno)&",RecL=86,KFName="&env$('Q')&"\TMmstr\TimeSheet-Idx.h"&str$(cno)&",Replace,KPs=1,KLn=5",internal,outin,keyed 
15200   open #h_support:=fngethandle: "Name="&env$('Q')&"\TMmstr\SUPPORT.h"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\support-idx.h"&str$(cno)&",Shr",internal,input,keyed 
15400 FMSUPPORT: form pos 1,g 6,n 2,c 2,x 8,x 2,n 8
15600   let fnopenprn
15800   print #255,using FORM_PRN_HEAD: 'date','client','time','cat','month','desc','rate'
16000 FORM_OUT: form pos 1,n 5,n 9,2*pd 3.2,pd 4.2,n 6,n 2,pd 2,pd 1,n 2,n 4,c 12,pd 3,c 30
16200 FORM_PRN: form pos 1,c 8,x 1,n 6,n 10.2,n 10,n 10,x 1,c 15,n 7.2
16400 FORM_PRN_HEAD: form pos 1,cc 8,x 1,c 6,x 1,5*cr 10,x 1,c 30,cr 7
16600   let fn_get_next_line(line$) : let line_count+=1 ! consume headings
16800   do 
17000     let fn_get_next_line(line$) : let line_count+=1
17064     let the_date_prior=the_date
17200     if line$<>'' and line$<>chr$(13) then 
17400       let str2mat(line$,mat item$,',',"QUOTES:TRIM")
17600       if item$(1)<>'' then let the_date=fn_get_the_date(item$(1))
17602       if the_date<the_date_prior and the_date_prior>20151218 then print 'the_date('&str$(the_date)&')<the_date_prior('&str$(the_date_prior)&') - that indicates a problem on line '&str$(line_count) : pause 
17800       if the_date=>filter_date(1) and the_date<=filter_date(2) then 
18000         if udim(mat item$)>9 and item$(4)<>'#N/A' and val(item$(7))>0 then ! entry
18020 !       print the_date;item$(4);' ';item$(7);' ';item$(9);' ';item$(10)
18040           let client_id=val(item$(4))
18042 ! if client_id=970 then pause
18060           let hours=val(item$(7))
18080           if rtrm$(item$(13),cr$)<>'' then let sage_code$=rtrm$(item$(13),cr$)
18100           dim description$*512
18120           let description$=item$(12)
18130 !         if client_id=3811 and the_date=20160716 then pr 'sage_code$='&sage_code$&' date:';the_date : pause
18140           let fn_acs_write_out(the_date,client_id,hours,val(item$(9)),val(item$(10)),item$(11)(1:30),sage_code$)
18160           if client_id=client_id_sage_ax then 
18180             let fn_sage_write_out(the_date,hours,sage_code$,description$)
18182 !           print the_date,hours,description$ : pause
18200           end if 
18220           print #255,using FORM_PRN: date$(days(the_date,'ccyymmdd'),'mm/dd/yy'),client_id,hours,val(item$(9)),val(item$(10)),item$(11)(1:15),inp(4)
18240         end if  ! item$(4)<>'#N/A' and  val(item$(7))>0
19000       end if  ! the_date=>filter_date(1) and <=filter_date(2)
19200     end if  ! line$<>''
19400   loop until line$=''
19600 ! THE_END: !
19800   close #h_in: 
20000   let fncloseprn
20200 XIT: let fnxit
20400   def fn_acs_write_out(wo_date,wo_client,wo_time,wo_cat,wo_month,wo_desc$*30; wo_sage_code$*128)
20600     dim inp(7)
20800     let inp(1)=wo_client
21000     let inp(2)=1 ! employee number
21200     let inp(3)=wo_time
21202 !         if wo_client=3811 and wo_date=20160716 then pr 'wo_sage_code$=';wo_sage_code$ : pause
21400     let inp(4)=fn_acs_hourly_rate(wo_client,the_date,wo_month, wo_cat,wo_sage_code$) ! hourly rate
21600     let inp(5)=wo_time*inp(4)
21800     let inp(6)=date(days(wo_date,'ccyymmdd'),'mmddyy') ! mmddyy
22000     let inp(7)=wo_cat
22200     let b6=0 ! ???
22400     let b7=1 ! ???
22800     if wo_cat=6 then 
23000       let sc=601
23200     else if wo_cat=2 then 
23400       let sc=201
23600     else if wo_cat=11 then 
23800       let sc=1101
24000     else if wo_cat=23 then 
24200       let sc=2300
24400     else 
24600       print #255: '!!! wo_cat ('&str$(wo_cat)&') is unrecognized - enhance code'
24800 !   print 'wo_cat (';wo_cat;') is unrecognized - enhance code' : pause
25000     end if 
25200     write #h_out,using FORM_OUT: mat inp,b6,b7,wo_month,sc,'',0,wo_desc$
25400   fnend  ! fn_acs_write_out
27600   def fn_get_next_line(&line$)
27800     dim gnl_block$*512
28000     dim gnl_buffer$*32767
28200     do until pos(gnl_buffer$,crlf$)>0 or gnl_eof
28400       let gnl_block$=''
28600       read #h_in,using 'form pos 1,C 100': gnl_block$ ioerr GNL_H_IN_READ_IOERR
28800       let gnl_buffer$=gnl_buffer$&gnl_block$
29000     loop 
29200     let pos_crlf=pos(gnl_buffer$,crlf$)
29400     let line$=gnl_buffer$(1:pos_crlf)
29600     let gnl_buffer$(1:pos_crlf+1)=''
29800 ! line$=srep$(line$,cr$,'^') : line$=srep$(line$,lf$,'~')
30000 ! print 'line='&line$ : pause
30200     goto GNL_XIT
30400 GNL_H_IN_READ_IOERR: ! 
30600     let gnl_block$=gnl_block$&crlf$
30800     let gnl_eof=1
31000     continue  ! gnl_h_in_read_ioerr
31200 GNL_XIT: ! 
31400   fnend  ! fn_get_next_line
31600   def fn_get_the_date(gtd_source$*256)
31800     let gtd_return=0
32000     if gtd_source$<>'' then 
32200 !   print 'set the_date from '&gtd_source$
32400       let gtd_source$=srep$(gtd_source$,'Mon, ','')
32600       let gtd_source$=srep$(gtd_source$,'Tue, ','')
32800       let gtd_source$=srep$(gtd_source$,'Wed, ','')
33000       let gtd_source$=srep$(gtd_source$,'Thu, ','')
33020       let gtd_source$=srep$(gtd_source$,'Fri, ','')
33040       let gtd_source$=srep$(gtd_source$,'Sat, ','')
33060       let gtd_source$=srep$(gtd_source$,'Sun, ','')
33080       let gtd_source$=srep$(gtd_source$,'Sun, ','')
33100       let cc12_pos=pos(gtd_source$,', 12')
33120       let cc13_pos=pos(gtd_source$,', 13')
33140       let cc14_pos=pos(gtd_source$,', 14')
33160       let cc15_pos=pos(gtd_source$,', 15')
33180       let cc16_pos=pos(gtd_source$,', 16')
33200       let cc17_pos=pos(gtd_source$,', 17')
33220       if cc13_pos<=0 then let cc13_pos=pos(gtd_source$,', 2013')
33240       if cc14_pos<=0 then let cc14_pos=pos(gtd_source$,', 2014')
33260       if cc15_pos<=0 then let cc15_pos=pos(gtd_source$,', 2015')
33280       if cc16_pos<=0 then let cc16_pos=pos(gtd_source$,', 2016')
33300       if cc17_pos<=0 then let cc17_pos=pos(gtd_source$,', 2017')
33320       if cc12_pos>0 then 
33340         let gtd_source$(cc12_pos:len(gtd_source$))=''
33360         let gtd_date_ccyy=2012
33380       else if cc13_pos>0 then 
33400         let gtd_source$(cc13_pos:len(gtd_source$))=''
33420         let gtd_date_ccyy=2013
33440       else if cc14_pos>0 then 
33460         let gtd_source$(cc14_pos:len(gtd_source$))=''
33480         let gtd_date_ccyy=2014
33500       else if cc15_pos>0 then 
33520         let gtd_source$(cc15_pos:len(gtd_source$))=''
33540         let gtd_date_ccyy=2015
33560       else if cc16_pos>0 then 
33580         let gtd_source$(cc16_pos:len(gtd_source$))=''
33600         let gtd_date_ccyy=2016
33620       else if cc17_pos>0 then 
33640         let gtd_source$(cc17_pos:len(gtd_source$))=''
33660         let gtd_date_ccyy=2017
33680       else 
33700         print 'unrecognized year - enhance code ('&gtd_source$&')' : pause 
36000       end if  ! 
36200       if pos(gtd_source$,'Jan ')>0 then 
36400         let gtd_source$=srep$(gtd_source$,'Jan ','')
36600         let gtd_date_mm=01
36800       else if pos(gtd_source$,'Feb ')>0 then 
37000         let gtd_source$=srep$(gtd_source$,'Feb ','')
37200         let gtd_date_mm=02
37400       else if pos(gtd_source$,'Mar ')>0 then 
37600         let gtd_source$=srep$(gtd_source$,'Mar ','')
37800         let gtd_date_mm=03
38000       else if pos(gtd_source$,'Apr ')>0 then 
38200         let gtd_source$=srep$(gtd_source$,'Apr ','')
38400         let gtd_date_mm=04
38402       else if pos(gtd_source$,'Apr, ')>0 then 
38404         let gtd_source$=srep$(gtd_source$,'Apr, ','')
38406         let gtd_date_mm=04
38600       else if pos(gtd_source$,'May ')>0 then 
38800         let gtd_source$=srep$(gtd_source$,'May ','')
39000         let gtd_date_mm=05
39200       else if pos(gtd_source$,'Jun ')>0 then 
39400         let gtd_source$=srep$(gtd_source$,'Jun ','')
39600         let gtd_date_mm=06
39800       else if pos(gtd_source$,'Jul ')>0 then 
40000         let gtd_source$=srep$(gtd_source$,'Jul ','')
40200         let gtd_date_mm=07
40400       else if pos(gtd_source$,'Aug ')>0 then 
40600         let gtd_source$=srep$(gtd_source$,'Aug ','')
40800         let gtd_date_mm=08
41000       else if pos(gtd_source$,'Sep ')>0 then 
41200         let gtd_source$=srep$(gtd_source$,'Sep ','')
41400         let gtd_date_mm=09
41600       else if pos(gtd_source$,'Oct ')>0 then 
41800         let gtd_source$=srep$(gtd_source$,'Oct ','')
42000         let gtd_date_mm=10
42200       else if pos(gtd_source$,'Nov ')>0 then 
42400         let gtd_source$=srep$(gtd_source$,'Nov ','')
42600         let gtd_date_mm=11
42800       else if pos(gtd_source$,'Dec ')>0 then 
43000         let gtd_source$=srep$(gtd_source$,'Dec ','')
43200         let gtd_date_mm=12
43400       else 
43600         print 'unrecognized month - enhance code' : pause 
43800       end if 
44000       let gtd_date_dd=val(gtd_source$)
44200       let gtd_return=val(str$(gtd_date_ccyy)&cnvrt$('pic(##)',gtd_date_mm)&cnvrt$('pic(##)',gtd_date_dd))
44400     end if  ! gtd_source$<>''
44600     let fn_get_the_date=gtd_return
44800   fnend  ! fn_get_the_date
45000 ! region: ertn
45200 ERTN: let fnerror(program$,err,line,act$,"xit")
45400   if uprc$(act$)<>"PAUSE" then goto L1710
45600   if env$("ACSDeveloper")<>"" then execute "list -"&str$(line) : pause : goto L1710
45800   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause 
46000 L1710: execute act$
46200   goto ERTN
46400 ! /region
46600   def fn_ask_dates(mat label$,mat filter_date)
46800     let fntos(sn$="ask_"&str$(udim(mat label$))&'_dates')
47000     let respc=0
47200     for ad_line=1 to udim(mat label$)
47400       let fnlbl(ad_line+1,1,label$(ad_line),25,1)
47600       let fntxt(ad_line+1,27,8,0,1,"3")
47800       let resp$(respc+=1)=str$(filter_date(ad_line))
48000     next ad_line
48200     let fncmdset(3)
48400     let fnacs(sn$,0,mat resp$,ckey)
48600     if ckey=5 then let fkey(99)
48800     for ad_line=1 to udim(mat label$)
49000       let filter_date(ad_line)=val(srep$(resp$(ad_line),'/',''))
49200     next ad_line
49400   fnend  ! fn_ask_dates
51000   def fn_acs_hourly_rate(wo_client,the_date,wo_month; hr_category,wo_sage_code$*128) ! inherrits client_id_sage_ax and client_id_brc
51040     if hr_category=23 or hr_category=11 then 
51060       let hr_return=0
51072     else if wo_client=client_id_brc then 
51074       let hr_return=60
51080     else if wo_client=client_id_sage_ax then 
51100       let hr_return=fn_sage_hourly_rate(wo_sage_code$)
51120     else if fn_onsupport(wo_client,wo_month,the_date) then 
51140       if hr_category=6 then 
51160         let hr_return=0
51180       else 
51200         let hr_return=125
51220       end if 
51240     else 
51260       let hr_return=250
51280     end if 
51300     let fn_acs_hourly_rate=hr_return
51320   fnend 
53000   def fn_onsupport(wo_client,wo_month,the_date)
53020     let os_return=0
53040 ! try lpad first
53060     let spk$=lpad$(str$(wo_client),6)&cnvrt$("n 2",wo_month)
53080     read #h_support,using FMSUPPORT,key=spk$: cln$,scode,scode$,sdt2 nokey OS_TRY_RPAD
53100     goto OS_FOUND_REC
53120 ! 
53140 OS_TRY_RPAD: ! 
53160     let spk$=rpad$(str$(wo_client),6)&cnvrt$("n 2",wo_month)
53180     read #h_support,using FMSUPPORT,key=spk$: cln$,scode,scode$,sdt2 nokey OS_FINIS
53200     goto OS_FOUND_REC
53220 ! 
53240 OS_FOUND_REC: ! 
53260     if the_date<=sdt2 then let os_return=1
53280 ! 
53300 OS_FINIS: ! 
53320     let fn_onsupport=os_return
53340   fnend 
54000   def fn_sage_hourly_rate(wo_sage_code$)
54020     if lwrc$(wo_sage_code$)='glover' then 
54040       let shr_return=40
54060     else if lwrc$(wo_sage_code$)='pbj offsite' or lwrc$(wo_sage_code$)='acc offsite' or lwrc$(wo_sage_code$)='offsite' then 
54120       let shr_return=40
54140     else 
54160       let shr_return=48.5
54180     end if 
54200     let fn_sage_hourly_rate=shr_return
54220   fnend 
56000   def fn_sage_write_out(wo_date,wo_time,wo_sage_code$*128,wo_desc$*512)
56020     dim wo_sage_code_prior$*128
56040     if ~setup_sawo then 
56060       let setup_sawo=1
56080       open #sawo_h_out:=fngethandle: 'Name='&env$('Q')&'\Sage_AX_'&str$(filter_date(1))&'-'&str$(filter_date(2))&'.csv,RecL=512,eol=crlf,Replace',display,output 
56100     end if 
56120     if wo_sage_code_prior$='' and wo_sage_code$='' then 
56140       print #255: '!!! Sage Code is blank !!!'
56160     end if 
56180     if wo_sage_code$='' then 
56200       let wo_sage_code$=wo_sage_code_prior$
56202     else if lwrc$(wo_sage_code$)='glover' then ! Glover Oil
56204       let wo_sage_code$='0052'
56220     else if lwrc$(wo_sage_code$)='pbj onsite' or lwrc$(wo_sage_code$)='pbj' then ! Payroll Based Journaling
56240       let wo_sage_code$='004H'
56260     else if lwrc$(wo_sage_code$)='aca onsite' or lwrc$(wo_sage_code$)='file export' then ! ACA file exports
56280       let wo_sage_code$='004J'
56300     else if lwrc$(wo_sage_code$)='onsite' or lwrc$(wo_sage_code$)='acc hourly' then ! hourly development for ACC
56320       let wo_sage_code$='004T'
56340     ! removed 1/9/2017  !  else if lwrc$(wo_sage_code$)='acc training' then ! Training on ACC
56360     ! removed 1/9/2017  !    let wo_sage_code$='004P'
56380     else if lwrc$(wo_sage_code$)='pbj offsite' then 
56400       let wo_sage_code$='004Y'
56420     else if lwrc$(wo_sage_code$)='aca offsite' or lwrc$(wo_sage_code$)='acc offsite' then 
56440       let wo_sage_code$='004Z'
56460     else if lwrc$(wo_sage_code$)='offsite' then 
56480       let wo_sage_code$='004X'
56500     else 
56520       print #255: '!!! Sage AX Project Code ('&wo_sage_code$&') is unrecognized - enhance table in csv2tm !!!'
56540     end if 
56560     let wo_sage_code_prior$=wo_sage_code$
56580     dim sawo_line$*512
56600     let sawo_line$=''
56620     let sawo_line$(inf:inf)=date$(days(wo_date,'ccyymmdd'),'ccyy/mm/dd')&tab$
56640     let sawo_line$(inf:inf)=str$(wo_time)&tab$
56660     let sawo_line$(inf:inf)=wo_sage_code$&tab$
56680     let sawo_line$(inf:inf)=wo_desc$
56700     print #sawo_h_out: sawo_line$
56720   fnend  ! fn_acs_write_out
