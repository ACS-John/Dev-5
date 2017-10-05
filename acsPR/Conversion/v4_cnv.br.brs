10000 ! Replace S:\acsPR\conversion\v4.cnv
10020 !    medicare_is_seperated     ...         look for "Fica_Combined" if medicare not separated and remove both exclamations
10040   if ~setup then let fn_setup
10200 ! ______________________________________________________________________
10220   fntop(program$,cap$="Department Conversion")
12000 ! r: do every company - loop top
12020   fngetdir2(env$('Q')&'\'&fncursys$&"mstr",mat filename$,'/od /ta',"Company.*")
12040   filename_item=0
12060   for filename_item=1 to udim(mat filename$)
12080     tmp_cno=val(filename$(filename_item)(10:14)) conv ACNO_CONV
12100     if tmp_cno<>99999 and filename$(filename_item)<>'' then ! don't display company 99999
12120       cno=tmp_cno
12140 ! 
12160 ! fncno(cno)
12180 ! 
12200 ! /r
14000       fn_pr_conversion_department(cno)
16000 ! r: do every company - loop bottom
16020     end if 
16040 ACNO_CONV: ! 
16050     close #h_rpmstr: ioerr ignore
16060   next filename_item
16080 ! /r
17000 !  fnstatus_pause
18000   chain "S:\acsPR\conversion\v4_part2"
20000   def fn_setup
20020     setup=1
20040     library 'S:\Core\Library': fntop, fnerror,fndate_mmddyy_to_ccyymmdd
20042     library 'S:\Core\Library': fngetdir2,fnstatus,fnindex_it,fncursys$
20044     library 'S:\Core\Library': fnstatus_pause,fnCopy,fngethandle,fnDedNames
20060     on error goto ERTN
20080 ! ______________________________________________________________________
20100     dim a$*40,em$*30,ta(2),cp(32),tcp(22) ! ,hc(5),thc(5),d$*20,whc(10),message$*40
20120     dim dedcode(10),calcode(10),dedfed(10),cap$*128
20140     dim tcp(32),newtdc(10),newtdet(23),tdt(4),tcd(3),tdet(17),tdy(6),tdc(6)
20160     dim ty(21),tqm(17),tcp(22),tdet(17),dednames$(20)*20,d1$*20
20180   fnend 
20200   def library fnpr_conversion_department(cno; medicare_is_seperated)
20220     if ~setup then let fn_setup
20240     fnpr_conversion_department=fn_pr_conversion_department(cno, medicare_is_seperated)
20260   fnend 
24000   def fn_pr_conversion_department(cno; medicare_is_seperated)
24010 !  pr 'all files should be closed now' : pause
24020     on error goto ERTN
24040 !   fncno(cno)
24060     fnstatus('Department Conversion(v4_cnv) for Company '&str$(cno))
24070     payrollcheck_write_count=0
24080 !      fnstatus_pause
24140     dim fullname$(10)*20
25000     fnDedNames(mat fullname$)
28000 RPMSTR_OPEN: ! 
28020     if ~exists(env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)) then 
28040       open #h_rpmstr:=1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",RecL=196,Use",internal,outin 
28060       close #h_rpmstr: 
28080     end if 
28100     fnindex_it(env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno),env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno),"1 8")
28120     fnindex_it(env$('Q')&"\PRmstr\RPMSTR.H"&str$(cno),env$('Q')&"\PRmstr\RPINDX2.H"&str$(cno),"9 30")
28140     open #h_rpmstr:=1: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&str$(cno)&",Shr",internal,outin,keyed 
28160     if rln(h_rpmstr)<196 then 
28180       close #h_rpmstr: 
28200       fnCopy(env$('Q')&"\PRmstr\RPMstr.h"&str$(cno),env$('Q')&"\PRmstr\RPMstr.h"&str$(cno),196)
28220       goto RPMSTR_OPEN
28240     end if 
32000 ! 
32020     if exists(env$('Q')&"\PRmstr\RPTrail.h"&str$(cno)) then 
32040       open #h_rptrail:=2: "Name="&env$('Q')&"\PRmstr\RPTrail.h"&str$(cno)&",Shr",internal,input,relative 
32060     else 
32080       open #h_rptrail:=2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&str$(cno)&",RecL=474,Use,Shr",internal,outin,relative 
32100     end if 
32120 ! 
32140     if fnindex_it(env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno),env$('Q')&"\PRmstr\PRCKINDX.h"&str$(cno),"1 14") then 
32160       open #h_prckhist:=fngethandle: "Name="&env$('Q')&"\PRmstr\PRCkHist.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\PRCkIndx.h"&str$(cno)&",Shr",internal,outin,keyed ioerr L320
32180       foundhistory=1 ! pr 'foundhistory : lrec(h_prckhist)='&str$(lrec(h_prckhist)) : pause
32182     else 
32184       foundhistory=0 ! pr 'was not able to index it - setting foundhistory to ZERO - history will not be created' : pause
32200     end if 
32220 L320: ! 
32240     open #12: "Name="&env$('Q')&"\PRmstr\Department.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&str$(cno)&",RecL=149,kps=1,kln=11,replace",internal,outin,keyed 
32260 ! L330: !
32280     open #h_payrollchecks:=fngethandle: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&str$(cno)&",RecL=224,kps=1,kln=17,replace",internal,outin,keyed 
32300     if exists(env$('Q')&"\PRmstr\dd.h"&str$(cno))=0 then 
32302       open #30: "Name="&env$('Q')&"\PRmstr\dd.h"&str$(cno)&",RecL=72,KFName="&env$('Q')&"\PRmstr\DDidx1.h"&str$(cno)&",kps=1,kln=10,Use",internal,outin,keyed 
32304       close #30: ioerr ignore
32380     end if 
32460 ! L370: !
32500     fn_prcode_validate
32550     goto TOPOFLOOP
32640 ! ______________________________________________________________________
34000 TOPOFLOOP: ! 
34020     read #h_rpmstr,using 'form pos 1,n 8,c 30,pos 122,n 2,pos 162,n 6,pos 173,2*pd 3': eno,em$,em6,lpd,mat ta eof DONE
34060     adr=ta(1)
34070     do 
34080       read #h_rptrail,using 'Form POS 1,N 8,N 3,N 3,N 6,N 3,4*N 6,3*N 2,24*PD 4.2,5*PD 3.2,POS 471,PD 4.2,POS 165,PD 3.2,60*PD 5.2,PD 3',rec=adr: teno,tdn,gl1,gl2,gl3,mat tdt,mat tcd,tli,mat tdet,mat tdy,mat tdc,mcwh,mat ty,mat tqm,mat tcp,nta
34120       gosub CONVERSION
34140       if nta<>0 then adr=nta
34150     loop until nta=0
34160     if foundhistory=1 then goto PULLFROMHISTORY
34180     goto TOPOFLOOP
34190 ! ______________________________________________________________________
46000 CONVERSION: ! 
46020     tdn=tdn ! department #
46040     prd=fndate_mmddyy_to_ccyymmdd(tdt(4)) ! payroll date to be used
46060     newtdet(1)=tdet(1) ! salary
46080     newtdet(2)=tdet(2) ! hourly rate
46100     newtdet(3)=tdet(3) ! ot rate
46120     for j=1 to 10
46140       newtdet(j+3)=tdet(j+3) ! 10 standard deductions
46160     next j
46180     newtdc(1)=tdy(1) ! regular hours for year
46200     newtdc(2)=tdy(2) ! ot hours
46220     newtdc(3)=tdy(3) ! sick hours
46240     newtdc(4)=tdy(4) ! vac hrs
46260     newtdc(5)=tdy(5) ! holiday hrs
46280     newtdc(6)=ty(21) ! workmans comp wage  (make all wages = total pay
46300     newtdc(7)=ty(21) ! ss wage  ! needs help What about cafiteria
46320     newtdc(8)=ty(21) ! medicaid
46340     newtdc(9)=ty(21) ! federal uc
46360     newtdc(10)=ty(21) ! state uc wage
46380     cp(1)=ty(1) ! fed wh year to date
46400     cp(2)=ty(2) ! ss wh year to date
46420     cp(3)=ty(15) ! medicaid ytd
46440     if medicare_is_seperated then gosub FICA_COMBINED
46460     cp(4)=ty(3) ! state wh ytd
46480     for j=1 to 10
46500       cp(j+4)=ty(j+3) ! 10 miscellaneous deductions'
46520     next j
46540     cp(25)=ty(14) ! eic
46560     cp(26)=ty(16) ! reg pay
46580     cp(27)=ty(17) ! ot pay
46600     cp(28)=ty(18) ! other comp
46620     cp(29)=ty(19) ! meals
46640     cp(30)=ty(20) ! tips
46660     cp(31)=ty(21) ! total wage
46680     cp(32)=0 ! don't have net
46700     gl$=cnvrt$("pic(zz#)",gl1)&cnvrt$("pic(zzzzz#)",gl2)&cnvrt$("pic(zz#)",gl3)
46720     write #12,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2': eno,tdn,gl$,mat tdt,mat tcd,tli,mat newtdet ! department record
46740     if ~foundhistory then ! else  write check file later
46760       fn_payrollchecks_write
46780     end if 
46800     return 
46820 ! 
50000 PULLFROMHISTORY: ! 
50020     hsk$=lpad$(str$(eno),8)
50040     restore #h_prckhist,search>=hsk$: nokey TOPOFLOOP
50060 PFH_READ_PRCKHIST: ! 
50080 ! If env$('client')="Franklinton" Then
50100 !    read #h_prckhist,using 'form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2': heno,prd,ckno,mat tdc,mat tcp eof TOPOFLOOP conv ignore
50120 ! pRD=fndate_mmddyy_to_ccyymmdd(PRD): Goto PFH_POSTREAD_PRCKHIST
50140     read #h_prckhist,using 'form pos 1,n 8,pd 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2': heno,prd,ckno,mat tdc,mat tcp eof TOPOFLOOP conv CKHIST_READ_CONV
50160 PFH_POSTREAD_PRCKHIST: ! 
50180     if heno<>eno then goto TOPOFLOOP
50200     if prd<20000101 then goto PFH_READ_PRCKHIST ! don't allow any checks before 2000
50220     tdn=tdn ! use last department since old check history is not by dept
50240     ckno=ckno
50260     for j=1 to 5
50280       newtdc(j)=tdc(j) ! hours etc
50300     next j
50320     newtdc(6)=newtdc(7)=newtdc(8)=newtdc(9)=newtdc(10)=0 ! set these wage figures to zero
50340     cp(1)=tcp(1) ! fed wh year to date
50360     cp(2)=tcp(2) ! ss wh year to date
50380     cp(3)=tcp(15) ! medicaid ytd
50400     if medicare_is_seperated then gosub FICA_COMBINED
50420     cp(4)=tcp(3) ! state wh ytd
50440     for j=1 to 10
50460       cp(j+4)=tcp(j+3) ! 10 miscellaneous deductions'
50480     next j
50500     cp(25)=tcp(14) ! eic
50520     cp(26)=tcp(16) ! reg pay
50540     cp(27)=tcp(17) ! ot pay
50560 ! 
50580     cp(28)=tcp(18) ! other comp
50600     cp(29)=tcp(19) ! meals
50620     cp(30)=tcp(20) ! tips
50640     cp(31)=tcp(21) ! total wage
50660     cp(32)=tcp(22) ! net
50680     fn_payrollchecks_write
50700     goto PFH_READ_PRCKHIST
50720 ! ______________________________________________________________________
52000 DONE: ! 
52020     gosub CREATENAMES
52040     open #h_deptname:=fngethandle: "Name="&env$('Q')&"\PRmstr\DeptName.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\DeptNameIdx.h"&str$(cno)&",replace,RecL=32,kps=1,kln=3,Shr",internal,outin,keyed 
52060     close #h_deptname: 
52080     close #12: ioerr ignore
52100     fnindex_it(env$('Q')&"\PRmstr\Department.h"&str$(cno),env$('Q')&"\PRmstr\DeptIdx.h"&str$(cno),"1 11")
52120     close #h_payrollchecks: 
52140     fnindex_it(env$('Q')&"\PRmstr\PayrollChecks.h"&str$(cno),env$('Q')&"\PRmstr\checkidx.h"&str$(cno),"1 17")
52142     fnindex_it(env$('Q')&"\PRmstr\dd.H"&str$(cno),env$('Q')&"\PRmstr\ddidx1.H"&str$(cno),"1,10")
52150     close #h_rpmstr: ioerr ignore
52160     fnindex_it(env$('Q')&"\PRmstr\RPMSTR.H"&str$(cno),env$('Q')&"\PRmstr\RPINDEX.H"&str$(cno),"1,8")
52200     fnindex_it(env$('Q')&"\PRmstr\RPMSTR.H"&str$(cno),env$('Q')&"\PRmstr\RPINDX2.H"&str$(cno),"9 30")
52220 !   end if  ! cno_current<>0
52240 ! next company_item
54000 XIT: ! 
54010     close #h_prckhist: ioerr ignore
54020     fnstatus('payrollcheck_write_count='&str$(payrollcheck_write_count))
54040 ! fnstatus_pause
54060   fnend 
56000 ! <Updateable Region: ERTN>
56020 ERTN: fnerror(program$,err,line,act$,"xit")
56040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
56060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
56080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
56100 ERTN_EXEC_ACT: execute act$ : goto ERTN
56120 ! /region
56140 IGNORE: continue 
58000 CREATENAMES: ! r:
58020   dim a$(3)*40,d$(10)*8,r(10),e$(10)*12,gln$(15)*12,dedcode(10),calcode(10)
58040   dim dedfed(10),rpnames2$(10)*6
58060   dim fullname$(20)*20,abrevname$(20)*8,newdedcode(20),newcalcode(20)
58080   dim newdedfed(20),dedfica(20),dedst(20),deduc(20),gl$(20)*12
58100   if exists(env$('Q')&"\PRmstr\PRCOINFO.h"&str$(cno)) and ~exists(env$('Q')&"\PRmstr\Company.h"&str$(cno)) then 
58110     execute "Rename "&env$('Q')&"\PRmstr\PRCOINFO.h"&str$(cno)&' '&env$('Q')&"\PRmstr\Company.h"&str$(cno)
58112   end if 
58120 ! close #1: ioerr ignore
58140   open #h_company:=fngethandle: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno),internal,input 
58160   read #h_company,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2': mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$ ! eof L370 ioerr L330
58180   close #h_company: 
58200   for j=1 to 10
58220     newdedcode(j)=dedcode(j)
58240     if dedfed(j)>0 then newdedfed(j)=1
58260     newcalcode(j)=calcode(j)
58280     fullname$(j)=rpnames2$(j)
58300     abrevname$(j)=fullname$(j)(1:6)
58320     if dedfed(j)=1 then dedst(j)=1 ! if pension type code make deductible for state
58340     if dedfed(j)=2 then dedfica(j)=1 ! if pension type code =2 then cafiteria
58360     gl$(j)=gln$(j+3)
58380   next j
58400   close #h_rptrail: ioerr ignore
58440   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$,1)
58520 return  ! /r
62000 CKHIST_READ_CONV: ! r:
62020   reread #h_prckhist,using 'form pos 1,n 8,n 6,n 7,5*pd 3.2,pd 4.2,22*pd 5.2': heno,prd,ckno,mat tdc,mat tcp eof TOPOFLOOP conv L1820
62040 ! pause
62060   prd=fndate_mmddyy_to_ccyymmdd(prd)
62080   goto PFH_POSTREAD_PRCKHIST ! /r
62100 L1820: ! r:
62120   reread #h_prckhist,using 'form pos 1,c 5': heno$ eof TOPOFLOOP
62140   goto PFH_READ_PRCKHIST ! /r
63000 FICA_COMBINED: ! r:
63020   med$="N": ss=cp(2)
63040   cp(2)=ss*.810458: cp(3)=ss-cp(2) ! just use a ratio to calculate breakdwon
63060   if uprc$(med$)="N" and em6=1 then cp(3)=0 : cp(2)=ss ! NO MC ALL SS ! change to seperate medicare
63080   if uprc$(med$)="N" and em6=2 then cp(2)=0 : cp(3)=ss ! NO SS ALL MC ! change to seperate medicare
63100   if em6=9 then cp(2)=cp(3)=0 ! NO SS OR MC
63120   return  ! /r
64000   def fn_prcode_validate !  PRCODE - verify it exist, if not create it with one blank record
64020     open #20: "Name="&env$('Q')&"\PRmstr\prCode.h"&str$(cno),internal,output ioerr PRCODE_CREATE_NEW
64040     close #20: 
64060     goto L378
64080 PRCODE_CREATE_NEW: ! 
64100     open #20: "Name="&env$('Q')&"\PRmstr\prCode.h"&str$(cno)&",RecL=512,new",internal,output  ! ioerr PRCODE_CREATE_NEW
64120     write #20,using 'form pos 1,c 512': ""
64140     close #20: 
64160 L378: ! 
64180   fnend 
66000   def fn_payrollchecks_write
66020     if newtdc(1)>999 then newtdc(1)=999
66040     write #h_payrollchecks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": eno,tdn,prd,ckno,mat newtdc,mat cp ! payroll check history
66050     payrollcheck_write_count+=1
66080   fnend 
