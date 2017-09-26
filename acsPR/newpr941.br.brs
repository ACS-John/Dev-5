00010 ! Replace S:\acsPR\newPR941  ! fix the count (box 1; needs logic to look thru history and count the active employees on a certain date)
00020 ! 941 Summary  ( Prints a detail of employees and the complete 941 using priint ace
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fntos,fnlbl,fnGetPayrollDates,fnerror,fnopenprn,fncloseprn,fntxt,fnchk,fncmdset,fnacs,fncomboa,fnfra,fnmsgbox,fnpa_finis,fnpa_open,fnpa_newpage,fncreg_read,fnpa_txt,fnpa_pic,fnpa_fontsize,fnDedNames
00050   let fntop(program$,cap$="941 Summary")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim dedcode(20),calcode(20),dedfed(20),option1$(4)*20
00090   dim fullname$(20)*20,abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
00100   dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10),em$(3)*30
00110   dim e$(10)*12,tpt(32),cap$*128,resp$(15)*30 ! option$(4)*15,message$*40,
00120   dim tcp(32),tdc(10) ! qtr1ytd(32),qtr1ytd(32),qtr3ytd(32),qtr4ytd(32)
00130   dim qtr1tcp(32),qtr2tcp(32),qtr3tcp(32),qtr4tcp(32),qtr(32)
00140   dim ytdtotal(32),ss$*11,m$*20
00150   dim city$*15,state$*2,zip$*9,csz$*40,ml$(2)*80
00160 ! ______________________________________________________________________
00220   fncreg_read('calculation date text',m$)
00230   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
00240   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1)
00250   open #20: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00260   read #20,using L280: mat a$,b$(1),mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$
00270   let ficamaxw=ficamaxw*10
00280 L280: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,pd 4.2,pd 4.2,10*pd 4.2,10*pd 3.3,10*c 12
00290 ! ______________________________________________________________________
00300   let ficarate=ficarate/100
00310   let mcr=mcr*.01
00320   close #20: 
00330 ! ______________________________________________________________________
00340 MENU1: ! 
00350   let fntos(sn$="pr941")
00352   let respc=0
00360   if val(date$(4:5))=1 then let taxyear=val(date$(1:2))+2000-1 else let taxyear =val(date$(1:2))+2000 ! current tax year (if processing in jan, assume last year)
00370   let fnlbl(1,1,"Tax Year:",26,1)
00380   let fntxt(1,30,4,0,0,"30",0,"")
00382   let resp$(respc+=1)=str$(taxyear)
00390   let option1$(1)="March 31"
00400   let option1$(2)="June 30"
00410   let option1$(3)="September 30"
00420   let option1$(4)="December 31"
00430   let fnlbl(2,1,"Quarter Ending Date:",26,1)
00440   let fncomboa("pr941-yr",2,30,mat option1$,"Enter the quarter ending date")
00450   if val(date$(4:5))=3 or val(date$(4:5))=4 or val(date$(4:5))=5 then let resp$(respc+=1)=option1$(1) ! march filing
00460   if val(date$(4:5))=6 or val(date$(4:5))=7 or val(date$(4:5))=8 then let resp$(respc+=1)=option1$(2) ! June  filing
00470   if val(date$(4:5))=9 or val(date$(4:5))=10 or val(date$(4:5))=11 then let resp$(respc+=1)=option1$(3) ! September filing
00480   if val(date$(4:5))=12 or val(date$(4:5))=1 or val(date$(4:5))=2 then let resp$(respc+=1)=option1$(4) ! December
00490   let fnchk(3,30,"Print Worksheet:",1) 
00492   let resp$(respc+=1)="True"
00500   let fnfra(5,1,4,30,"Tax Liability","Enter the total tax liability by month")
00510   let fnlbl(1,1,"Month 1:",10,1,0,1)
00520   let fntxt(1,13,12,0,1,"10",0,"",1) 
00522   let resp$(respc+=1)=""
00530   let fnlbl(2,1,"Month 2:",10,1,0,1)
00540   let fntxt(2,13,12,0,1,"10",0,"",1) !:
        let resp$(respc+=1)=""
00550   let fnlbl(3,1,"Month 3:",10,1,0,1)
00560   let fntxt(3,13,12,0,1,"10",0,"",1) !:
        let resp$(respc+=1)=""
00570   let fnfra(11,1,7,72,"Adjustments","Enter any applicable adjustments")
00580   let mylen=52
00590   let fnlbl(1,1,"Current quarter's fraction of cents:",mylen,1,0,2)
00600   let fntxt(1,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00610   let fnlbl(2,1,"Current quarter's sick pay:",mylen,1,0,2)
00620   let fntxt(2,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00630   let fnlbl(3,1,"Current quarter's adjustments for tips and ins:",mylen,1,0,2)
00640   let fntxt(3,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00650   let fnlbl(4,1,"Current year's income tax withholding:",mylen,1,0,2)
00660   let fntxt(4,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00670   let fnlbl(5,1,"Prior quarters' ss and medicare taxes:",mylen,1,0,2)
00680   let fntxt(5,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00690   let fnlbl(6,1,"Special Additions to Federal income taxes:",mylen,1,0,2)
00700   let fntxt(6,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00710   let fnlbl(7,1,"Special Additions to ss and medicare:",mylen,1,0,2)
00720   let fntxt(7,mylen+3,12,0,1,"10",0,"",2) !:
        let resp$(respc+=1)=""
00730   let fnlbl(20,1,"Total deposits for quarter including overpayments:",mylen+1,1,0,0)
00740   let fntxt(20,mylen+4,12,0,1,"10",0,"",0) !:
        let resp$(respc+=1)=""
00750   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
00760   if ck=5 then goto XIT
00770   let taxyear=val(resp$(1)) ! tax year
00780   if taxyear<2000 then goto L810
00790   let ending_date=taxyear*10000+1231 conv L810
00800   goto L820
00810 L810: mat ml$(2) !:
        let ml$(1)="You must enter a valid tax year such as 2007." !:
        let ml$(2)="Take OK to enter the year." !:
        let fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto MENU1
00820 L820: for j=1 to 4
00830     if resp$(2)=option1$(j) then let qtr=j: let m$=option1$(j): goto L850 ! quarter ending date
00840   next j
00850 L850: if qtr=1 then let begdate=taxyear*10000+0312: let enddate=val(taxyear$)*10000+0318
00860   if qtr=2 then let begdate=taxyear*10000+0612: let enddate=val(taxyear$)*10000+0618
00870   if qtr=3 then let begdate=taxyear*10000+0912: let enddate=val(taxyear$)*10000+0918
00880   if qtr=4 then let begdate=taxyear*10000+1212: let enddate=val(taxyear$)*10000+1218
00890   if resp$(3)="True" then let frm=2 else let frm=1 ! need a worksheet
00900   let box15a=val(resp$(4)) ! first month liability
00910   let box15b=val(resp$(5))
00920   let box15c=val(resp$(6))
00930   let box7a=val(resp$(7)) ! fractions
00940   let box7b=val(resp$(8)) ! sick pay
00950   let box7c=val(resp$(9)) ! tips
00960   let box7d=val(resp$(10)) ! tax wh
00970   let box7e=val(resp$(11)) ! prior qtr
00980   let box7f=val(resp$(12)) ! special add
00990   let box7g=val(resp$(13)) ! special add - ss
01000   let box11=val(resp$(14))
01008   let fnopenprn
01010   let fn_start_print
01020   let fn_build_941
01022   let fn_print941_info
01030 ! gosub LASER_941
01040   goto FINIS
01050 ! ______________________________________________________________________
10000 def fn_start_print
10020   on pageoflow goto PGOF
10040   open #2: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
10060   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
10080   open #3: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",Shr, KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
10100   if frm=2 then gosub WK_HEADER
10120   L1140: read #2,using L1150: eno,mat em$,ss$,em5,em6 eof WK_END
10140   L1150: form pos 1,n 8,3*c 30,c 11,pos 120,2*n 2
10160   let m1=m2=h2=h3=dedytdfica=dedqtrfica=dedytdfederal=dedqtrfederal=m4=0
10180   mat qtr1tcp=(0): mat qtr2tcp=(0): mat qtr3tcp=(0): mat qtr4tcp=(0)
10200   mat ytdtotal=(0)
10220   let checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
10240   restore #4,key>=checkkey$: nokey ANALYZE_WAGES
10260   L1210: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof ANALYZE_WAGES
10280   if heno<>eno then goto ANALYZE_WAGES
10300   if prd<beg_date or prd>end_date then goto L1210 ! not this year
10320   if em5=1 then let pedate=begdate+19: let box1+=1 ! monthly pay period
10340   if em5=2 then let pedate=begdate+15 : let box1+=1 ! semi-monthly
10360   if em5=3 then let pedate=begdate+14 : let box1+=1 ! bi-weekly
10380   if em5=4 then let pedate=begdate+7: let box1+=1 ! weekly
10400   !   let deptkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",tdn)
10420   ! Form POS 48,N 2
10440   if prd>=qtr1 and prd<qtr2 then mat qtr1tcp=qtr1tcp+tcp: mat tpt=tpt+tcp ! 1st qtr earnings
10460   if prd>=qtr2 and prd<qtr3 then mat qtr2tcp=qtr2tcp+tcp : mat tpt=tpt+tcp
10480   if prd>=qtr3 and prd<qtr4 then mat qtr3tcp=qtr3tcp+tcp : mat tpt=tpt+tcp
10500   if prd>=qtr4 and prd<=end_date then mat qtr4tcp=qtr4tcp+tcp : mat tpt=tpt+tcp
10520   if prd>=qtr1 and prd<ending_date then mat ytdtotal=ytdtotal+tcp ! only total year to date wages to end of current quarter
10540   goto L1210
10560   ANALYZE_WAGES: ! analyze wages on each person
10580   if qtr=1 then mat qtr=qtr1tcp
10600   if qtr=2 then mat qtr=qtr2tcp
10620   if qtr=3 then mat qtr=qtr3tcp
10640   if qtr=4 then mat qtr=qtr4tcp
10660   !   let dcq=0 ! total wage for quarter
10680   !   let tcy=0 ! total wages to end of quarter
10700   for j=1 to 20
10720     if dedfed(j)=1 and dedcode(j)=1 then let dedytdfederal+=ytdtotal(j+4): let dedqtrfederal+=qtr(j+4) ! TOTAL DEDUCTION FROM WAGES  TO TAXABEL FEDERAL WAGE
10740     if dedfica(j)=1 and dedcode(j)=1 then let dedytdfica+=ytdtotal(j+4) : let dedqtrfica+=qtr(j+4) ! TOTAL DEDUCTIONS FOR FICA FOR QUARTER
10760   next j
10780   let m2=m2+ytdtotal(31)-dedytdfica ! TOTAL WAGES less deductions FOR THIS EMPLOYEE FOR YEAR
10800   let m1=m1+qtr(31)-dedqtrfica ! TOTAL WAGES less deductions FOR QURATER
10820   let m4=m4+qtr(31)-dedqtrfederal ! TOTAL Taxable WAGES less deductions FOR QURATER
10840   let fedwh=fedwh+qtr(1) ! FEDERAL WH FOR QUARTER
10860   let eicqtr+=qtr(25) ! EIC FOR QUARTER
10880   if m2=0 then goto L1140
10900   let fn_print_details
10920   goto L1140
10940   ! ______________________________________________________________________
10960   WK_HEADER: ! 
10980   let p2=p2+1
11000   print #255,using L1600: "Page ",p2
11020   print #255: ""
11040   L1600: form pos 70,c 5,pic(zzz)
11060   print #255: tab(15);"Employer's Quarterly Federal Tax Return Worksheet"
11080   print #255,using L1630: "For quarter ended "&m$&", "&str$(taxyear)
11100   L1630: form pos 20,cc 40
11120   print #255: ""
11140   print #255: ""
11160   if eof=1 then goto L1790
11180   print #255,using L1680: a$(1),"Fed ID",b$(1)
11200   L1680: form pos 17,c 40,pos 59,c 6,pos 69,c 40
11220   print #255,using L1700: a$(2),"State ID",e$(1)
11240   L1700: form pos 17,c 40,pos 59,c 8,pos 69,c 12,skip 1
11260   print #255,using L1720: a$(3),"State",d$(1)
11280   L1720: form pos 17,c 40,pos 59,c 5,pos 69,c 8,skip 1
11300   print #255: ""
11320   print #255: tab(41);"Total Wages    Social-Sec.      Medicare"
11340   print #255: " SS Number             Name";
11360   print #255: tab(41);"For Quarter        Wages";tab(75);"Wages"
11380   print #255: "___________  __________________________";
11400   print #255: tab(41);"___________   ____________  ____________"
11420   L1790: return 
11440   ! ______________________________________________________________________
11460   WK_END: ! 
11480   let fn_totals
11530   print #255: newpage
11532   on pageoflow ignore 
11540 fnend 
11560 FINIS: ! 
11580   let fncloseprn
11600 XIT: let fnxit
11640   def fn_print_details: ! detailed listing
11660     if m1=0 then goto L2130
11680     let p3=p3+1
11700     let h2=h3=0
11720     if em6=2 or em6=9 then goto L2010
11740     if m2<ficamaxw then goto L2000
11760     if m2-m1>ficamaxw then goto L1980
11780     let h2=ficamaxw-(m2-m1)
11800     goto L2010
11820 L1980: let h2=0
11840     goto L2010
11860 L2000: let h2=m1
11880 L2010: if em6=1 or em6=9 then goto L2050
11900     if m2<mcm then let h3=m1 : goto L2050 ! MCM = MEDICARE MAXIMUM WAGE
11920     if m2-m1>mcm then let h3=0 : goto L2050
11940     let h3=mcm-(m2-m1)
11960 L2050: if frm=1 then goto L2090
11980     print #255,using L2080: ss$,em$(1)(1:27),m4,h2,h3
12000     print #255: ""
12020 L2080: form pos 1,c 11,pos 14,c 27,pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
12040 L2090: let t1=t1+m4
12060     let t3=t3+h3
12080     let t2=t2+h2
12100     let p1=p1+2
12120 L2130: ! 
12140   fnend 
12160   def fn_totals
12180     if frm=1 then goto L2230
12200     print #255,using L2180: "___________    ___________  ____________"
12220 L2180: form pos 41,c 41,skip 1
12240     print #255: "       Total Employees:";p3;"     Totals";
12260     print #255,using L2210: t1,t2,t3
12280 L2210: form pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
12300 L2230: let p3=0
12320     let gt1=gt1+t1
12340     let gt2=gt2+t2
12360     let gt3=gt3+t3
12380     let gt4=gt4+t4
12400     let t1=0
12420     let t2=0
12440     let t3=0
12460     let t4=0
12480   fnend 
12520 PGOF: ! r:
12540   print #255: newpage
12560   gosub WK_HEADER
12580 continue ! /r
12620   def fn_build_941
12640 !   if env$('client')="Washington Parrish" then let tpt(30)=0 ! used tips for something else 
12660     let wagefica=round((gt2-tpt(30))*(ficarate*2),2) ! 2013
12680     let taxfica=round(tpt(30)*(ficarate*2),2) ! 2013 requirements that employee fica be 4.2 and employer be 6.2  (was ficarate*2)
12700 !   let tipfica= round((tpt(30))*(ficarate*2),2) ! 2013    FICARATE)
12720     let mcwh=round((gt3)*mcr*2,2)
12740 !   BOX1   ! count on 12th of each qtr (analyze history for this in another section)
12760     let box2=gt1
12780     let box3=fedwh
12800     let box4=0 ! ( unused )
12820     let box5a1=gt2-tpt(30) ! FICA WAGES LESS TIPS
12840     let box5a2=wagefica
12860     let box5b1=tpt(30) ! TIPS
12880     let box5b2=taxfica
12900     let box5c1=gt3
12920     let box5c2=mcwh
12940     let box5d=wagefica+taxfica+mcwh
12960     let box6=box3+box5d ! total taxes before adj
12980     let box7h=box7a+box7b+box7d+box7e+box7f+box7g ! total adjustments
13000     let box8=box6-box7h ! total due after adjustments
13020     let box9=eicqtr ! eic
13040     let box10=box8-box9 ! total taxes after eic
13060     let box11=box11 ! total deposits
13080     let box12=box10-box11 ! blance due
13100     if box10-box11<0 then let box13=abs(box10-box11) : let box12=0 else let box13=0 ! overpayment if any
13120     let box15d=box15a+box15b+box15c ! total deposits for quarter
13140   fnend 
13180 ! LASER_941: ! r:
13200   let fnpa_open
13220   let fn_print941_origional
13240   close #2: ioerr ignore
13260   close #3: ioerr ignore
13280   ! 
13300   let fnpa_finis
13320   return  ! /r
13360 ! <Updateable Region: ERTN>
13380 ERTN: let fnerror(program$,err,line,act$,"xit")
13400   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
13420   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
13440   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
13460 ERTN_EXEC_ACT: execute act$ : goto ERTN
13480 ! /region
13520   def fn_print941_origional
13540     fnpa_fontsize(12)
13560     fnpa_pic("S:\acsPR\941.bmp",1,1)
13580 ! fnpa_pic("S:\acsPR\941_2012_A.bmp",1,1)
13600     print #20: 'Call Print.MyFont("Courier New")'
13620     for j=1 to 10
13640       let x=val(b$(1)(j:j)) conv L2960 ! pull any spaces or non-numeric characters out of federal id#
13660       goto L2970
13680 L2960: let b$(1)(j:j)=""
13700 L2970: if b$(1)(j:j)=" " then let b$(1)(j:j)=""
13720       if b$(1)(j:j)="-" then let b$(1)(j:j)=""
13740     next j
13760     fnpa_fontsize(16)
13780     let lyne=15 ! starting line of fed id
13800     fnpa_txt(b$(1)(1:1),47,lyne)
13820     fnpa_txt(b$(1)(2:2),56,lyne)
13840     fnpa_txt(b$(1)(3:3),70,lyne)
13860     fnpa_txt(b$(1)(4:4),79,lyne)
13880     fnpa_txt(b$(1)(5:5),88,lyne)
13900     fnpa_txt(b$(1)(6:6),97,lyne)
13920     fnpa_txt(b$(1)(7:7),106,lyne)
13940     fnpa_txt(b$(1)(8:8),115,lyne)
13960     fnpa_txt(b$(1)(9:9),124,lyne)
13980     fnpa_fontsize(12)
14000     fnpa_txt(trim$(a$(1)),35,32)
14020     fnpa_txt(trim$(a$(2)),35,39)
14040     let csz$=a$(3): let fn_csz
14060     fnpa_txt(trim$(city$),35,48)
14080     fnpa_txt(trim$(state$),93,48)
14100     fnpa_txt(trim$(zip$),108,48)
14120     if qtr=1 then let quarter=29 ! 1st quarter
14140     if qtr=2 then let quarter=35 ! 2nd quarter
14160     if qtr=3 then let quarter=41 ! 3rd quarter
14180     if qtr=4 then let quarter=47 ! 4rd quarter
14200     fnpa_txt("X",143,quarter)
14220     let tab1=63: let tab2=113: let tab3=160 ! WAS 62,112,159  ! 76,126,173 WORKED WHEN TO FAR RIGHT
14240     fnpa_txt(cnvrt$("pic(zzzzzzzzzzzzzz)",box1),tab3,72)
14260     fnpa_txt(cnvrt$("pic(-,---,---.##)",box2),tab3,80)
14280     fnpa_txt(cnvrt$("pic(-,---,---.##)",box3),tab3,87)
14300 ! fnpa_txt(CNVRT$("pic(-,---,---.##)",BOX4),TAB3,90)
14320     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5a1),tab1,108)
14340     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5a2),tab2,108)
14360     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5b1),tab1,116)
14380     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5b2),tab2,116)
14400     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5c1),tab1,123)
14420     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5c2),tab2,123)
14440     fnpa_txt(cnvrt$("pic(-,---,---.##)",box5d),tab3,131)
14460     fnpa_txt(cnvrt$("pic(-,---,---.##)",box6),tab3,138)
14480     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7a),tab2,150)
14500     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7b),tab2,158)
14520     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7c),tab2,165)
14540     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7d),tab2,172)
14560     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7e),tab2,180)
14580     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7f),tab2,187)
14600     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7g),tab2,194)
14620     fnpa_txt(cnvrt$("pic(-,---,---.##)",box7h),tab3,202)
14640     fnpa_txt(cnvrt$("pic(-,---,---.##)",box8),tab3,209)
14660     fnpa_txt(cnvrt$("pic(-,---,---.##)",box9),tab3,216)
14680     fnpa_txt(cnvrt$("pic(-,---,---.##)",box10),tab3,224)
14700     fnpa_txt(cnvrt$("pic(-,---,---.##)",box11),tab3,232)
14720     fnpa_txt(cnvrt$("pic(-,---,---.##)",box12),tab3,238)
14740     fnpa_txt(cnvrt$("pic(-,---,---.##)",box13),tab2,247)
14760     let fnpa_newpage
14780     fnpa_pic("S:\acsPR\941-back.bmp",1,1)
14800     fnpa_txt(trim$(state$(1:1)),15,33)
14820     fnpa_txt(trim$(state$(2:2)),21,33)
14840     let tab4=75
14860     fnpa_txt(cnvrt$("pic(-,---,---.##)",box15a),tab4,61)
14880     fnpa_txt(cnvrt$("pic(-,---,---.##)",box15b),tab4,68)
14900     fnpa_txt(cnvrt$("pic(-,---,---.##)",box15c),tab4,74)
14920     fnpa_txt(cnvrt$("pic(-,---,---.##)",box15a+box15b+box15c),tab4,82)
14940   fnend 
14960   def fn_csz ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
14980 L3680: let p1=pos(csz$,".",1)
15000     if p1>0 then let csz$(p1:p1)=" ": let p2=p1: goto L3680
15020 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN LET CSZ$(P2+1:P2+1)="" ! dump any extra spaces
15040     let p2=0
15060 L3720: let p1=pos(csz$,",",1)
15080     if p1>0 then let csz$(p1:p1)=" ": let p2=p1: goto L3720
15100 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN LET CSZ$(P2+1:P2+1)="" ! dump any extra spaces
15120 L3750: let p1=pos(rtrm$(csz$),"  ",1)
15140     if p1>0 then let csz$(p1+1:p1+1)="" : goto L3750
15160     let csz$=ltrm$(rtrm$(csz$)): let p1=pos(csz$," ",-1)
15180     let zip$=csz$(p1+1:len(csz$)): let zip$=ltrm$(rtrm$(zip$))
15200     let p2=pos(csz$(1:p1-1)," ",-1) : let state$=csz$(p2+1:p1-1)(1:2) : let state$=ltrm$(rtrm$(state$))
15220     let city$=csz$(1:p2-1)(1:15): let city$=ltrm$(rtrm$(city$))
15240   fnend 
15260   def fn_print941_info ! print 941 information
15280     print #255: "Employer ID # (EIN): ";b$(1)
15300     print #255: "Name (not trade name): "
15320     print #255: "Trade Name: ";a$(1)
15340     print #255: "Address: ";a$(2)
15360     print #255: "City, State Zip: ";a$(3)
15380     print #255: 
15400     print #255,using "form pos 5, cr 50,pic(zzzzzzzzzzz#)": "Number of Employees receiving pay for Quarter: ",box1
15420     print #255,using "form pos 5, cr 50,pic(-,---,---.##)": "Wages, Tips & Other Compensation: ",box2
15440     print #255,using "form pos 5, cr 50,pic(-,---,---.##)": "Income Tax Withheld: ",box3
15460     print #255: 
15480     print #255,using "form pos 5,cr 18,x 1,2*pic(---,---,---.##)": "5a",box5a1,box5a2
15500     print #255,using "form pos 5,cr 18,x 1,2*pic(---,---,---.##)": "5b",box5b1,box5b2
15520     print #255,using "form pos 5,cr 18,x 1,2*pic(---,---,---.##)": "5c",box5c1,box5c2
15540     print #255: 
15560     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "5d",box5d
15580     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "6",box6
15600     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7a",box7a
15620     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7b",box7b
15640     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7c",box7c
15660     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7d",box7d
15680     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7e",box7e
15700     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7f",box7f
15720     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7g",box7g
15740     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "7h",box7h
15760     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "8",box8
15780     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "9",box9
15800     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "10",box10
15820     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "11",box11
15840     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "12",box12
15860     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "13",box13
15880     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15a",box15a
15900     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15b",box15b
15920     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15c",box15c
15940     print #255,using "form pos 5,cr 20,x 1,pic(-,---,---.##)": "15a+15b+15c",box15a+box15b+box15c
15960   fnend  ! fn_Print941_info
