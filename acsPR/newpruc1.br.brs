10020 ! State U/C Report
10060   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fnprocess,fntos,fnlbl,fntxt,fncmdset,fnacs,fnchk,fncomboa,fncombof,fncreg_read,fncreg_write,fnGetPayrollDates,fnDedNames
10080   on error goto ERTN
10100 ! r: setup
22000   dim ss$*11,em$(3)*30,department$*128
22020   dim a$(3)*40,b$(2)*12,d$(10)*8
22040   dim m(10),r(10),cap$*128,d1$*20
22060   dim e$(10)*12,dedcode(20)
22080   dim calcode(20)
22100   dim dedfed(20)
22120   dim fullname$(20)*20
22140   dim abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
22160   dim option1$(10)
22180   dim qtr(32)
22200   dim ytdtotal(32)
22220   dim tcp(32),tdc(10)
22240   dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32)
23000   dim qtr_option$(4)*3
23020   let qtr_option$(1)="1"
23040   let qtr_option$(2)="2"
23060   let qtr_option$(3)="3"
23080   let qtr_option$(4)="4"
23100   dim quarter_ending_date$*20
24000 ! 
24020   dim column$(4) ! columns to include on the report (True or False)
24040   dim ptotal(4)
24060   dim grand_total(4)
24080   dim col_amt_form$(4)*80
24100   let col_amt_form$(1)='pic(--,---,---.zz)  '
24120   let col_amt_form$(2)='pic(---,---,---.zz) '
24140   let col_amt_form$(3)='pic(-,---,---.zz) '
24160   let col_amt_form$(4)='pic(--,---,---.zz)'
24180   let total_underline$(1)="___________   "
24200   let total_underline$(2)=" ___________ "
24220   let total_underline$(3)="__________   "
24240   let total_underline$(4)="__________   "
24260   dim heading_1$(4)*20
24280   dim heading_2$(4)*20
24300   dim heading_3$(4)*20
24320   let heading_1$(1)="Total Wages"
24340   let heading_2$(1)="           "
24360   let heading_3$(1)="___________"
24380   let heading_1$(2)="   Excess Wages"
24400   let heading_2$(2)="   Over $"&cnvrt$("pic(zzzzz)",0)&' '
24420   let heading_3$(2)="   ____________"
24440   let heading_1$(3)="    Taxable"
24460   let heading_2$(3)="     Wages "
24480   let heading_3$(3)="   ________"
24500   let heading_1$(4)="     State   "
24520   let heading_2$(4)="      W/H    "
24540   let heading_3$(4)="  ___________"
28990 ! ______________________________________________________________________
32000   let fntop(program$,cap$="State UnEmp Report")
32080   let fncreg_read('calculation date text',quarter_ending_date$)
32160   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d1$)
32220   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
32240   read #1,using 'Form POS 1,3*C 40,2*C 12,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,PD 4.2,PD 4.2,10*PD 4.2,10*PD 3.3,10*C 12,POS 618,30*N 1': mat a$,mat b$,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
32260   close #1: 
32280   let ficamaxw=ficamaxw*10
32300   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
32360   for j=1 to 10
32380     if trim$(e$(j))<>"" then 
32400       let x+=1
32420       let option1$(x)=str$(j)&" = "&d$(j)
32440     end if 
32460   next j
32480   mat option1$(max(1,x))
32500   let fncreg_read('uc1 - quarter',quarter$) : if quarter$='' then let quarter$='1'
32520   let fncreg_read('uc1 - show total wage',column$(1)) : if column$(1)='' then let column$(1)='True'
32540   let fncreg_read('uc1 - show excess wage',column$(2)) : if column$(2)='' then let column$(2)='True'
32560   let fncreg_read('uc1 - show taxable wage',column$(3)) : if column$(3)='' then let column$(3)='True'
32580   let fncreg_read('uc1 - show state tax withheld',column$(4)) : if column$(4)='' then let column$(1)='True'
32590 ! /r
32600 ! r: main screen (falls through to next section)
34000   let fntos(sn$="pruc1b")
34020   let respc=0: let x=0
34040   let fnlbl(1,1,"Quarter Ending Date:",26,1)
34060   let fntxt(1,30,20,0,0,"",0,"Use alpha format (eg. March 31, 20xx)")
34080   if trim$(quarter_ending_date$)='' then let resp$(respc+=1)=date$("Month DD, CCYY") else let resp$(respc+=1)=quarter_ending_date$
34100   let fnlbl(2,1,"State Code:",26,1)
34120   let fncomboa("pruc1-2",2,30,mat option1$,"Enter the state code from the company information file for the state being processed.")
34140   let resp$(respc+=1)=option1$(1)
34180   let fnlbl(3,1,"Quarter Code:",26,1)
34200   let fncomboa("pruc1-3",3,30,mat qtr_option$,"Enter the quarter you are processing.")
34220   let resp$(respc+=1)=quarter$
34240   let fnchk(5,40,"Round to Whole Dollars:",1)
34260   let resp$(respc+=1)="False"
34280   let fnchk(6,40,"Show Total Column:",1)
34300   let resp$(respc+=1)=column$(1)
34320   let fnchk(7,40,"Show Excess Wage Column:",1)
34340   let resp$(respc+=1)=column$(2)
34360   let fnchk(8,40,"Show Taxable Column:",1)
34380   let resp$(respc+=1)=column$(3)
34400   let fnchk(8,40,"Show State Tax Withheld:",1)
34420   let resp$(respc+=1)=column$(4)
34440   let fnlbl(10,1,"Payroll Department:",26,1)
34460   let fncombof("DeptName",10,30,29,env$('Q')&"\PRmstr\DeptName.h"&env$('cno'),1,3,4,25,env$('Q')&"\PRmstr\DeptNameIdx.h"&env$('cno'),2,0, " ",0,0)
34480   let resp$(respc+=1)='[All]'
34580   let fncmdset(2)
34600   let fnacs(sn$,0,mat resp$,ck)
36000   if ck=5 then goto XIT
36020   let quarter_ending_date$=resp$(1) ! quarter ending date
36040   let stcode=val(resp$(2)(1:2)) ! state code
36050   let heading_2$(2)="   Over $"&cnvrt$("pic(zzzzz)",m(stcode))&' '
36060   let quarter_code=val(resp$(3)) ! quarter to analyze earnings
36080   let fncreg_write('uc1 - quarter',resp$(3))
36100   if resp$(4)(1:1)="T" then let round$="Y"
36120   let column$(1)=resp$(5) ! want total wage column
36140   let column$(2)=resp$(6) ! want excess wage column
36160   let column$(3)=resp$(7) ! want taxable wage column
36180   let column$(4)=resp$(8) ! True=want state tax withheld column
36200   let fncreg_write('uc1 - show total wage',column$(1))
36220   let fncreg_write('uc1 - show excess wage',column$(2))
36240   let fncreg_write('uc1 - show taxable wage',column$(3))
36260   let fncreg_write('uc1 - show state tax withheld',column$(4))
36280   let department$=resp$(9)
36300   if round$="Y" then let m(stcode)=round(m(stcode),0)
36320   if quarter_code=1 then let ending_date=qtr2
36340   if quarter_code=2 then let ending_date=qtr3
36360   if quarter_code=3 then let ending_date=qtr4
36380   if quarter_code=4 then let ending_date=end_date
38000 ! /r
38020   on fkey 5 goto FINIS
38040   let fnopenprn
38060   if file$(255)(1:3)="PRN" then let redir=0 else let redir=1
38080 ! ______________________________________________________________________
38100   open #2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
38120   open #h_department:=3: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",Shr, KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
38140   open #h_payrollchecks:=4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
38160   gosub HDR
42000 TOP: ! 
42020   read #2,using "Form POS 1,N 8,3*C 30,C 11": eno,mat em$,ss$ eof DONE
42040   let m1=m2=h2=h3=dcq=dcy=0 : mat ytdtotal=(0)
42060   mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
42080   mat ytdtota(0)
42100   let checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
42120   restore #h_payrollchecks,key>=checkkey$: nokey ANALYZE_WAGES
46000 MAIN_LOOP: ! r:
46020   read #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
46040   if heno<>eno then goto ANALYZE_WAGES
46060   if prd<beg_date or prd>end_date then goto MAIN_LOOP ! not this year
46080   if department$<>'[All]' and tdn<>val(department$(1:3)) then goto MAIN_LOOP
46100   let deptkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)
46120   read #h_department,using L810,key=deptkey$: sc1 nokey L830 ! no state code (assume this state)
46140 L810: form pos 48,n 2
46160   if sc1<>stcode then goto MAIN_LOOP ! must be right state code
46180 L830: if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp ! 1st qtr earnings
46200   if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp
46220   if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp
46240   if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp
46260   if prd>=qtr1 and prd<ending_date then mat ytdtotal=ytdtotal+tcp ! only total year to date wages to end of current quarter
46280   goto MAIN_LOOP ! /r
48000 ANALYZE_WAGES: ! r: analyze wages on each person
48020   if quarter_code=1 then mat qtr=qtr1tcp
48040   if quarter_code=2 then mat qtr=qtr2tcp
48060   if quarter_code=3 then mat qtr=qtr3tcp
48080   if quarter_code=4 then mat qtr=qtr4tcp
48100   let dcq=0 ! total wage for quarter
48140   for j=1 to 20
48160     if dedcode(j)=1 and deduc(j)=1 then 
48180       let dcy=dcy+ytdtotal(j+4)
48200       let dcq=dcq+qtr(j+4)
48220     end if 
48240   next j
48260 ! if env$('client')="Washington Parrish" then
48280 !   let m2=m2+ytdtotal(31)-dcy+ytdtotal(6)
48300 !   goto L1010 ! add deferred comp match to uc wages on Wash Par
48320 ! end if
48340   let m2=m2+ytdtotal(31)-dcy
48360 ! L1010: !
48380 ! if env$('client')="Washington Parrish" then
48400 !   let m1=m1+qtr(31)-dcq+qtr(6)
48420 !   goto L1030 ! add deferred comp to qtr uc wages
48440 ! end if
48460   let m1=m1+qtr(31)-dcq
48480 ! L1030: !
48482   if m2=0 then goto TOP
48500   let state_wh=qtr(4)
48520   gosub SUBTOTALS
48540   goto TOP
48560 ! /r
52000 DONE: ! r:
52020   let eofcode=1
52040   gosub PAGE_TOTALS
52060   gosub HDR
52080   print #255,using L1460: "Grand Totals:",grand_total(1),grand_total(2),grand_total(3)
52100 L1460: form pos 5,c 15,pos 42,pic(--,---,---.zz),pos 57,pic(--,---,---.zz),pos 70,pic(----,---.zz),skip redir
52120   print #255,using L1480: "U/C Tax Due:",round(grand_total(3)*r(stcode)*.01,2)
52140 L1480: form pos 5,c 15,pic(--,---,---.zz)
52160 FINIS: close #2: ioerr ignore
52180   close #h_department: ioerr ignore
52200   let fncloseprn
52220   let fnxit ! /r
54000 SUBTOTALS: ! r:
54020   if m1=0 then goto L1780 ! SKIP IF QUARTERLY WAGE=0
54040   let p3=p3+1
54060   if round$="Y" then let m1=round(m1,0)
54080   if m2<m(stcode) then goto L1640
54100   if m2-m1>m(stcode) then goto L1620
54120   let h2=m(stcode)-(m2-m1)
54140   if round$="Y" then let h2=round(h2,0)
54160   goto L1650
54180 L1620: ! 
54200   let h2=0
54220   goto L1650
54240 L1640: ! 
54260   let h2=m1
54280 L1650: ! 
54300   let h3=m1-h2
54320   if round$="Y" then let h3=round(h3,0)
54340   if column$(1)<>"True" then let m1=0 ! no total column
54360   if column$(2)<>"True" then let h3=0 ! no excess column
54380   if column$(3)<>"True" then let h2=0 ! no taxable column
54400   if column$(4)<>"True" then let state_wh=0 ! no state withholding column
54420 ! print #255,using L1710: ss$,em$(1)(1:28),m1,h3,h2,state_wh
54440 ! L1710: form pos 1,c 11,pos 14,c 28,pos 42,pic(--,---,---.zz),pos 57,pic(--,---,---.zz),pos 70,pic(----,---.zz),pos 83,pic(----,---.zz),skip 1
54460   print #255: rpad$(ss$,11)&'   '&em$(1)(1:28);
54480   let col_amt(1)=m1
54500   let col_amt(2)=h3
54520   let col_amt(3)=h2
54540   let col_amt(4)=state_wh
54560   let fn_print_line_amt(mat col_amt,mat col_amt_form$)
54580   for col_item=1 to udim(mat column$)
54600     let ptotal(col_item)+=col_amt(col_item)
54620     let grand_total(col_item)+=col_amt(col_item) ! grand totals
54640   next col_item
54660   print #255: pageoflow PGOF
54680   let p1=p1+2
54700 L1780: ! 
54720   return  ! /r
56000 PAGE_TOTALS: ! r:
56180   let fn_print_line_str(mat total_underline$,44)
56200   print #255: "Employees on this page:";p3;"    Page Totals";
56220 ! print #255,using L1880: ptotal(1),ptotal(2),ptotal(3),ptotal(4)
56260   let fn_print_line_amt(mat ptotal,mat col_amt_form$)
56360   if nw=1 and eofcode=1 then goto L1910
56380   print #255: newpage
56400 L1910: let p3=0
56420   mat ptotal=(0)
56440   return  ! /r
58000 PGOF: ! r:
58020   gosub PAGE_TOTALS
58040   gosub HDR
58060   continue  ! /r
61020 ! <Updateable Region: ERTN>
61040 ERTN: let fnerror(program$,err,line,act$,"xit")
61060   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
61080   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
61100   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
61120 ERTN_EXEC_ACT: execute act$ : goto ERTN
61140 ! /region
61160 ! ______________________________________________________________________
61180 XIT: let fnxit
61190 IGNORE: continue 
64000 HDR: ! r:
64020 ! r: page heading
64040   print #255,using L1090: "Page ",p2+=1
64060 L1090: form pos 70,c 5,pic(zzz)
64080   print #255: ''
64100   print #255: "\qc  {\f201 \fs24 \b Schedule A - Employer's Report of Wages Paid to Each Employee}"
64120   print #255: "\ql   "
64140   print #255,using L1150: "For quarter ended "&quarter_ending_date$
64160   if department$<>'[All]' then print #255,using L1150: "Department "&department$
64180 L1150: form pos 20,cc 40
64200   print #255: 
64220   print #255,using L1180: "     Rate",a$(1),"Fed ID",b$(1)
64240 L1180: form pos 1,c 9,pos 17,c 40,pos 59,c 6,pos 69,c 40,skip 1
64260   if stcode=0 then let stcode=1
64280   print #255,using L1210: r(stcode),a$(2),"State ID",e$(stcode)
64300 L1210: form pos 3,pic(zzzz.##),pos 17,c 40,pos 59,c 8,pos 69,c 12,skip 1
64320   print #255,using L1230: a$(3),"STATE",d$(stcode)
64340 L1230: form pos 17,c 40,pos 59,c 5,pos 69,c 8,skip 1
64360   print #255: 
64380 ! /r
65000 ! r: column headings
65320   let fn_print_line_str(mat heading_1$, 44)
65340   print #255: " SS Number             Name              ";
65360   let fn_print_line_str(mat heading_2$, 44)
65380   print #255: "___________  __________________________";
65400   let fn_print_line_str(mat heading_3$, 44) ! underlines
65420 ! /r
65440   return  ! /r
68000   def fn_print_line_str(mat pl_str$; pl_pos)
68002     dim pl_line$*256
68010     let pl_line$=''
68020     if pl_pos=0 then let pl_pos=42
68040     for pl_item=1 to udim(mat column$)
68060       if column$(pl_item)="True" then 
68080         let pl_line$=pl_line$&pl_str$(pl_item)
68100       end if 
68120     next pl_item
68140     print #255,using 'form pos pl_pos,c': pl_line$
68180   fnend 
68200   def fn_print_line_amt(mat pl_amt,mat pl_form$; pl_pos)
68210     let pl_line$=''
68220     if pl_pos=0 then let pl_pos=42
68240     for pl_item=1 to udim(mat column$)
68260       if column$(pl_item)="True" then 
68280         let pl_line$=pl_line$&cnvrt$(pl_form$(pl_item),pl_amt(pl_item))
68300       end if 
68320     next pl_item
68340     print #255,using 'form pos pl_pos,c': pl_line$
68360   fnend 
