10000 ! Replace S:\acsGL\AcPrReg
10200 ! -- PAYROLL REGISTER
10400 ! ______________________________________________________________________
10600   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnpedat$,fnTos,fnFra,fnLbl,fnTxt,fnCmdKey,fnAcs,fndate_mmddyy_to_ccyymmdd,fngethandle
10800   on error goto ERTN
11000 ! ______________________________________________________________________
11200   dim miscname$(10)*20,dedcode(10),cap$*128,empd(22)
11400   dim k(1),k$(3)*25,l$(1)*11,d(22),m(36),r$*10,n$*5,n(2),dat$*20
11600   dim fa$(2),sa$(2)*40,fb$(2),ext(2),adr(2),report$*35,deposit(31,2)
11800 ! ______________________________________________________________________
12000   fntop(program$,cap$="Print Payroll Registers")
12400   fndat(dat$)
12600 ! ______________________________________________________________________
12800   fnTos(sn$="PayrollReg")
13000   rc=cf=0: mylen=22: mypos=mylen+3: frameno=1
13200   fnFra(1,1,3,40,"Date Range for Report","Enter the date range for the payrolls to be included.")
13400   fnLbl(1,1,"Beginning Date:",mylen,1,0,frameno)
13600   fnTxt(1,mypos,12,0,1,"3",0,"Enter the date of the first payroll to be included in this report. ",frameno)
13800   resp$(rc+=1)=str$(beg_date)
14000   fnLbl(2,1,"Ending Date:",mylen,1,0,frameno)
14200   fnTxt(2,mypos,12,0,1,"3",0,"Enter the last payroll date that should be included in this report. ",frameno)
14400   resp$(rc+=1)=str$(end_date)
14600   fnCmdKey("Next",1,1,0,"Calculate tax deposit.")
14800   fnCmdKey("Cancel",5,0,1,"Returns to menu without printing.")
15000   fnAcs(sn$,0,mat resp$,ckey)
15200   if ckey=5 then goto XIT
15400 ! 
15600   beg_date=val(resp$(1))
15800   end_date=val(resp$(2))
16000 ! 
16200   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,outIn,relative: read #1,using 'Form POS 386,PD 5.3,PD 5.2,PD 5.3,PD 5.2,POS 407,PD 5.3,PD 5.2,POS 418,10*C 20,10*N 1',rec=1: ficarate,ficawage,feducrat,feducwag,mcr,mcm,mat miscname$,mat dedcode : close #1: 
16400   ficarate=ficarate/100 : feducrat=feducrat/100 : mcr=mcr/100
16800   open #h_prmstr:=fngethandle: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&env$('cno')&",Shr",internal,outIn,keyed 
16900   fPrmstr: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
17000   open #h_acprcks:=fngethandle: "Name="&env$('Q')&"\GLmstr\ACPRCKS.h"&env$('cno')&",Shr",internal,outIn,relative 
17200   report$="Payroll Check Register"
17400   fnopenprn(cp,58,220,0)
17600   fn_hdr1
17800 L350: if d(1)>0 then goto L360 else goto L390
18000 L360: if sum(empd)=0 then goto L390
18200   pr #255,using L870: eno,"Total",empd(4),empd(5),empd(6),empd(7),empd9,empd(8),empd(22),empd(19),empd(21) pageoflow PGOF1
18400   pr #255: 
18600   mat empd=(0)
18800 L390: read #h_prmstr,using 'Form POS 1,N 4,3*C 25,POS 271,2*N 5': eno,mat k$,mat adr eof L650
18810 ! if eno=19 then pr eno : pause
19000   if adr(1)=0 then goto L390
19200   ca=adr(1)
19400   do 
19600     read #h_acprcks,using 'Form N 4,2*PD 4,19*PD 5.2,PD 3',rec=ca: mat d,nca conv L350
19800     if fndate_mmddyy_to_ccyymmdd(d(2))<beg_date or fndate_mmddyy_to_ccyymmdd(d(2))>end_date then goto L470
20000     fn_941_breakdown
20200     fn_accumulate_totals
20400     fn_print_1
20600 L470: if nca=0 then goto L350
20800     ca=nca
21000   loop 
21200 L650: ! pr TOTALS
21400   fn_totals_1
21600   pr #255: newpage
21800   fn_941_summary
22000   fn_report3
22200   goto FINIS
22400 ! ______________________________________________________________________
22600 def fn_header
22610   nametab=66-int(len(env$('cnam'))/2)
22800   pr #255,using L530: date$('mm/dd/yy'),time$,env$('cnam')
23000   L530: form skip 1,pos 1,c 8,skip 1,pos 1,c 8,pos nametab,c 40,skip 1
23200   p1=66-int(len(rtrm$(report$))/2)
23400   pr #255,using L560: rtrm$(report$)
23600   L560: form pos p1,c 50
23800   if report$="Payroll Check Register" then pr #255,using "form pos 40,cc 50": "From "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To "&cnvrt$("pic(zzzz/zz/zz)",end_date): goto L590
24000   p1=66-int(len(rtrm$(dat$))/2)
24200   pr #255,using L560: rtrm$(fnpedat$)
24300   L590: ! 
24400 fnend 
24600 def fn_hdr1
24800   fn_header
25000   pr #255:''
25020   pr #255,using 'form pos 1,c 132': "EMP #    EMPLOYEE NAME         GROSS   FED W/H  FICA W/H    ST W/H  MISC W/H   LOC W/H       NET     TIPS    EIC   DATE    CK #"
25040   pr #255:''
25400 fnend 
25600 def fn_accumulate_totals
25800   t1=t1+d(4) ! ACCUMULATE TOTALS
26000   t2=t2+d(5)
26200   t3=t3+d(6)
26400   t4=t4+d(7)
26600   d9=0
26800   for j=9 to 18
27000     if dedcode(j-8)=2 then d9=d9-d(j) else d9=d9+d(j)
27200   next j
27400   t5=t5+d9
27600   t6=t6+d(8)
27800   t7=t7+d(22)
28000   t8=t8+d(19)
28200   t9=t9+d(21)
28400   at8=at8+d(20)
28600 fnend 
28800 def fn_print_1
29000   pr #255,using L870: eno,k$(1)(1:19),d(4),d(5),d(6),d(7),d9,d(8),d(22),d(19),d(21),d(2),d(3) pageoflow PGOF1
29200   mat empd=empd+d
29400   L870: form pos 1,pic(zzzz),pos 8,c 19,7*n 10.2,pic(------.##),pic(----.##),pic(zzz/zz/zz),pic(zzzzzzz),skip 1
29600 fnend 
29800 PGOF1: ! r:
30000   pr #255: newpage
30200   fn_hdr1
30400 continue ! /r
30600 def fn_totals_1
30800   pr #255,using L930: "TOTALS",t1,t2,t3,t4,t5,t6,t7,t8,t9
31000   L930: form skip 1,pos 15,c 6,pos 27,7*n 10.2,pic(------.##),pic(----.##),skip 3
31200   u1=t2+round(t3*2,2)-t9 ! 2013
31400   pr #255,using L960: "TOTAL TAX DEPOSIT DUE", u1
31600   L960: form pos 1,c 21,pos 48,pic(---,---.##)
31800 fnend 
32000 def fn_report3
32200   t1=0
32400   t2=0
32600   t3=0
32800   t4=0
33000   t5=0
33200   t6=0
33400   t7=0
33600   t8=0
33800   t9=0
34000   fn_hdr3
34200   restore #h_prmstr: 
34220   ! restore #h_PRmstr,key>="    ": nokey L1140 eof L1140 ! nokey FINIS eof L1090
34400   do 
34600     ! L1090: ! 
34800     read #h_prmstr,using fPrmstr: eno,mat k$,mat l$,mat m,mat adr eof L1140
35200     fn_print_3
35400     fn_calk_3b
35600   loop 
35800   L1140: ! 
36000 fnend 
36200   def fn_hdr3
36400     report$="PAYROLL REGISTER - EARNINGS TO DATE"
36600     fn_header
36800     pr #255,using L1180: "*********************** Y.T.D. ***************************    **************** Q.T.D *******************"
37000 L1180: form skip 1,pos 23,c 105,skip 1
37200     pr #255: "EMP #  EMPLOYEE NAME    GROSS   FED W/H FICA W/H   ST W/H  LOC W/H    TIPS     EIC      GROSS  FED W/H FICA W/H  ST W/H  LOC W/H"
37400   fnend 
37600 FINIS: ! 
37800   close #h_acprcks: ioerr L1230
38000 L1230: ! 
38200   fn_totals_3
38400 ! pr #255,USING 40: HEX$("2B0205000A1042")
38600   fncloseprn
38800   goto XIT
39000   def fn_calk_3b
39200     t1=t1+m(1)
39400     t2=t2+m(3)
39600     t3=t3+m(5)
39800     t4=t4+m(7)
40000     l1=l1+m(9)
40200     l3=l3+m(31)
40400     l5=l5+m(35)
40600     t5=t5+m(2)
40800     t6=t6+m(4)
41000     t7=t7+m(6)
41200     t8=t8+m(8)
41400     l2=l2+m(10)
41600     l4=l4+m(32)
41800     l6=l6+m(36)
42000   fnend 
42200   def fn_print_3
42400     pr #255,using L1450: eno,k$(1)(1:12),m(1),m(3),m(5),m(7),m(9),m(31),m(35),m(2),m(4),m(6),m(8),m(10) pageoflow PGOF3
42600 L1450: form pos 1,pic(zzzz),pos 7,c 12,pic(----,---.##),pic(---,---.##),pic(--,---.##),pic(--,---.##),pic(--,---.##),pic(-----.##),pic(-----.##),pic(----,---.##),pic(------.##),pic(--,---.##),pic(-----.##),pic(-----.##),skip 1 ! 11/10/86 add 1 z to first pic
42800     goto PAST_PGOF3
43000 PGOF3: ! 
43200     pr #255: newpage
43400     fn_hdr3
43600 PAST_PGOF3: ! 
43800     fn_calc_3
44000     at6=at6+at5
44200     mcw2=mcw2+mcw1
44400     if m(1)-m(2)>=feducwag then goto L1550
44600     if m(1)<=feducwag then fuc1=m(2) else fuc1=feducwag-(m(1)-m(2))
44800     fuc2=fuc2+fuc1
45000 L1550: ! 
45200   fnend 
45400   def fn_totals_3
45600     pr #255,using L1570: "TOTALS",t1,t2,t3,t4,l1,l3,l5,t5,t6,t7,t8,l2
45800 L1570: form skip 1,pos 10,c 6,pic(zzz,zzz,zzz.##),n 10.2,3*n 9.2,2*n 8.2,pic(zzzz,zzz.##),2*n 9.2,2*n 8.2,skip 3
46000     pr #255,using L1600: "TAXABLE SOC-SEC. WAGES FOR QUARTER",at6
46200     pr #255,using L1600: "TAXABLE MEDICARE WAGES FOR QUARTER",mcw2
46400 L1600: form pos 41,c 35,pic(----,---,---.##),skip 1
46600     pr #255,using L1620: "TOTAL TIPS FOR QUARTER",l4
46800 L1620: form pos 50,c 30,pos 80,pic(----,---.##),skip 1
47000     pr #255,using L1620: "TOTAL EIC FOR QUARTER",l6
47200     pr #255,using L1620: "TOTAL FED U/C WAGES QTD",fuc2
47400     pr #255,using L1620: "TOTAL FED U/C TAX QTD",fuc2*feducrat
47600   fnend 
47800   def fn_941_breakdown
48000     ck$=lpad$(str$(d(2)),6)
48200     ckdat=val(ck$(3:4))
48400     if ckdat>0 and ckdat<32 then x=ckdat else x=31
48600     deposit(x,1)=deposit(x,1)+d(4)
48800     deposit(x,2)=deposit(x,2)+d(5)+d(6)+d(6)-d(21) ! FEDERAL + DOUBLE SS - EIC
49000   fnend 
49200   def fn_941_summary
49400     report$="941 BREAKDOWN"
49600     fn_header
49800     pr #255: 
50000     pr #255: tab(43);"GROSS";tab(63);"TAXES"
50200 L1790: form pos 5,c 30,pos 38,n 10.2,pos 58,n 10.2,skip 1
50400     for j=1 to 31
50600       pr #255,using L1790: "DAY "&str$(j),deposit(j,1),deposit(j,2)
50800       gross=gross + deposit(j,1)
51000       taxes=taxes+deposit(j,2)
51200     next j
51400     pr #255,using L1860: "----------","----------","       TOTALS",gross,taxes,"==========","=========="
51600 L1860: form pos 38,c 10,pos 58,c 10,skip 1,pos 5,c 25,pos 38,n 10.2,pos 58,n 10.2,skip 1,pos 38,c 10,pos 58,c 10,skip 1
51800     pr #255: newpage
52000   fnend 
52200   def fn_calc_3
52400     twy=m(1)
52600     twq=m(2)
52800     if twy<ficawage then at5=twq : goto L1940
53000     if twy-twq>ficawage then at5=0 : goto L1940
53200     at5=ficawage-(twy-twq)
53400 L1940: ! 
53600     if twy<mcm then mcw1=twq : goto L1970
53800     if twy-twq>mcm then mcw1=0 : goto L1970
54000     mcw1=mcm-(twy-twq)
54200 L1970: ! 
54400   fnend 
54600 XIT: fnxit
54800 ! ______________________________________________________________________
55000 ! <updateable region: ertn>
55200 ERTN: fnerror(program$,err,line,act$,"xit")
55400   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
55600   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
55800   pr "program pause: type go and press [enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
56000 ERTN_EXEC_ACT: execute act$ : goto ERTN
56200 ! /region
