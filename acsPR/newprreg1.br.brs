00010 ! Replace S:\acsPR\newprReg1
00015 ! Payroll Register
00020 ! ______________________________________________________________________
00025 ! r: general setup: libraries, dims, fntop,fncno,read defaults, open files, etc
00030   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fnGetPayrollDates,fnerror,fnprocess,fndate_mmddyy_to_ccyymmdd,fndat,fntos,fnlbl,fntxt,fncmdkey,fnacs,fnpayroll_register_2,fncreg_read,fncreg_write,fnchk,fnreg_read,fnreg_write,fncreg_read,fnDedNames
00035   on error goto ERTN
00040 ! ______________________________________________________________________
00045   dim em$*30,cp(32),tcp(32),tdc(10),ttdc(10),d$*20
00046   dim fullname$(20), abrevname$(20)
00050   dim newdedcode(20),newcalcode(20),newdedfed(20),cap$*128
00055   dim dedfica(20),dedst(20),deduc(20),thc(5)
00060 ! ______________________________________________________________________
00065   fntop(program$,cap$="Payroll Registers")
00076   fncreg_read('prreg2.include_tips_in_other_wh',include_tips_in_other_wh$) : if include_tips_in_other_wh$='' then let include_tips_in_other_wh$='True'
00078   fnreg_read('prreg2.append_reg1',append_reg1$) : if append_reg1$='' then append_reg1$='False'
00080   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
00082   fncreg_read('CL Bank Code',bankcode$) : bankcode=val(bankcode$) : if bankcode=0 then bankcode=1
00090 ! 
00120   open #12: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&env$('cno')&",Shr",internal,input,keyed ioerr L240
00130   read #12,using 'Form POS 57,G 8',key=lpad$(str$(bankcode),2),release: cl_bank_last_check$ nokey ignore
00140   close #12: ioerr ignore
00145 L240: ! 
00150   open #1: "Name="&env$('Q')&"\PRmstr\prCode.h"&env$('cno')&",Shr",internal,input ioerr L270
00155   read #1,using 'Form POS 5,N 8': ckno
00160   close #1: 
00165 L270: ! 
00170   ckno+=1
00175 ! 
00180   fnGetPayrollDates(beg_date,end_date,qtr1,qtr2,qtr3,qtr4,d1,d$)
00195   let d1$=cnvrt$("pic(zzzzzzzz)",d1)
00200   let ppd=val(d1$(5:6))*10000+val(d1$(7:8))*100+val(d1$(3:4))
00205 L320: if trim$(cl_bank_last_check$)<>"" then ckno=val(cl_bank_last_check$)+1 conv ignore
00210 ! 
00250 ! 
00265   open #1: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00270   open #h_dd=30: "Name="&env$('Q')&"\PRmstr\DD.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DDidx1.h"&env$('cno')&",Shr",internal,input,keyed 
00275   open #h_checks:=4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&',Shr',internal,input,keyed 
00280 ! /r
00340   if fnprocess=1 then goto START_REPORT else goto ASK_CHECK_NO
46000 ASK_CHECK_NO: ! r:
46020   fntos(sn$="Payrollreg1")
46040   let respc=0
46060   fnlbl(1,1,"Beginning Check Number:",20,1)
46080   fntxt(1,23,8,0,1,"30",0," ")
46100   let resp$(respc+=1)=str$(ckno)
46120   fnlbl(1,1,"",34,1) ! bigger screen
46140   fnlbl(2,1,"Payroll Date:",20,1)
46160   fntxt(2,23,10,0,1,"1",0,"For current payroll, always use the calculation date.  You can reprint older payroll registers by using that date.")
46180   let resp$(respc+=1)=str$(ppd)
46190   fnchk(4,2,'Combine both registers into one multi-page report',50)
46192   let resp$(resp_append_reg1:=respc+=1)=append_reg1$
46200   if env$('ACSDeveloper')<>'' then ! option under development for West Accounting... held until they decide if they actually want/nedd this - currently causes mismatch (in their cno 18) in other_wh in 1st and 2nd PR Registers
46202     fnchk(6,2,'Include Tips in Other Withholdings',50)
46204     let resp$(resp_include_tips_in_other_wh:=respc+=1)=include_tips_in_other_wh$
46206   else 
46208     let resp$(resp_include_tips_in_other_wh:=respc+=1)='False'
46210   end if 
46240   fncmdkey("&Next",1,1,0,"Proceed with pr the payroll register." )
46260   fncmdkey("E&xit",5,0,1,"Returns to menu")
48000   fnacs(sn$,0,mat resp$,ckey) ! ask employee #
48020   if ckey=5 then goto XIT
48040   ckno=val(resp$(1))
48060   let ppd=val(resp$(2))
48070   append_reg1$=resp$(resp_append_reg1)
48080   let include_tips_in_other_wh$=resp$(resp_include_tips_in_other_wh)
48100   fncreg_write('prreg2.include_tips_in_other_wh',include_tips_in_other_wh$) : if include_tips_in_other_wh$='True' then let include_tips_in_other_wh=1 else let include_tips_in_other_wh=0
48110   fnreg_write('prreg2.append_reg1',append_reg1$) : if append_reg1$='True' then append_reg1=1 else append_reg1=0
48120   goto START_REPORT ! /r
51000 START_REPORT: ! r:
51020   if append_reg1 then 
51040     fnopenprn( 0,0,0,fnprocess,' (Check and Departmental Registers)')
51060   else 
51080     fnopenprn( 0,0,0,fnprocess,' (Check Register)')
51100   end if 
51120   gosub HDR
51140   goto LOOP_TOP
51160 ! ______________________________________________________________________
52000 LOOP_TOP: ! 
52020   read #1,using 'form pos 1,n 8,c 30,pos 162,n 6,pos 173,2*pd 3': eno,em$,lpd eof FINIS
52120 ! if eno=307 then pr 'eno '&str$(eno) : exe 'break other_wh' : break_is_on=1 else if break_is_on then exe 'break other_wh off' : break_is_on=0
52140   if lpd><ppd then goto LOOP_TOP
52160   mat thc=(0) : mat tcp=(0) : mat ttdc=(0) : let holdrealckno=realckno=0
52180   checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
52200   let dirdep$=rpad$(str$(eno),10)
52220   let dd$=""
52240   read #h_dd,using "Form pos 1,C 10,C 1,N 9,N 2,N 17",key=dirdep$: key$,dd$,rtn,acc,acn nokey ignore
52260 ! if env$('client')="West Rest Haven" then goto L690
52280   a=pos(rtrm$(em$)," ",1) : b=pos(rtrm$(em$)," ",a+1)
52300   let em$=rtrm$(em$(max(a,b):30))&" "&em$(1:a) error ignore
52320 ! L690: !
52340   restore #h_checks,key>=checkkey$: nokey LOOP_TOP
52360 L700: ! 
52380   read #h_checks,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,realckno,mat tdc,mat cp eof L760
52400   if heno<>eno then goto L760
52420   if prd><fndate_mmddyy_to_ccyymmdd(ppd) then goto L700
52440   if prd=fndate_mmddyy_to_ccyymmdd(ppd) then let holdrealckno=realckno
52460   mat tcp=tcp+cp : mat ttdc=ttdc+tdc
52480   goto L700
52500 L760: ! 
52520 ! let oi=tcp(28)+tcp(29)+tcp(30)+tcp(27)
52540   let other_wh=-tcp(25)
52560   for j=5 to 24
52580     if newdedcode(j-4)=3 then goto L810
52600     if newdedcode(j-4)=2 then 
52602       let other_wh=other_wh-tcp(j) ! if break_is_on and tcp(j)<>0 then pr 'tcp('&str$(j)&') deducts '&str$(tcp(j))
52604     else 
52606       let other_wh=other_wh+tcp(j) ! if break_is_on and tcp(j)<>0 then pr 'tcp('&str$(j)&')    adds '&str$(tcp(j))
52608     end if 
52620 L810: ! 
52640   next j
52642   if include_tips_in_other_wh then ! include tips in Other Withholdings added for West Accounting on 1/18/2016
52650     let other_wh+=tcp(30) ! if break_is_on and tcp(30)<>0 then pr 'tcp('&str$(30)&') TIPS    adds '&str$(tcp(30))
52652   end if 
52660   let tothrs=0
52680   for j=1 to 5
52700     let tothrs=tothrs+ttdc(j)
52720     let thc(j)=ttdc(j)
52740   next j
52760   if holdrealckno=0 then ckn2=ckno : ckno+=1 else ckn2=holdrealckno
52780   if uprc$(dd$)="Y" then goto L915
52800   pr #255,using L900: ckn2,eno,em$(1:11),mat thc,tothrs,tcp(31),tcp(3),tcp(2),tcp(1),tcp(4),other_wh,tcp(32) pageoflow PGOF
52820 L900: form pos 1,n 5,n 8,x 1,c 12,6*n 7.2,7*n 9.2,skip 1
52840   goto L940
52860 L915: if tcp(22)=0 then let tcp(22)=tcp(32)
52880   pr #255,using L930: "DD",eno,em$(1:11),mat thc,tothrs,tcp(31),tcp(3),tcp(2),tcp(1),tcp(4),other_wh,tcp(22) pageoflow PGOF
52900 L930: form pos 1,c 5,n 8,x 1,c 12,6*n 7.2,7*n 9.2,skip 1
53000 L940: ! 
53020   let total_hours+=sum(mat thc)
53040   let total_net_pay+=tcp(32)
53050   let total_gross_pay+=tcp(31)
53060   let other_wh=0
53080   goto LOOP_TOP ! /r
53100 ! ______________________________________________________________________
56000 TOTALS: ! r:
56020   pr #255: ''
56040   pr #255: '    Total Hours: '&lpad$(str$(total_hours),26)
56060   pr #255: 'Total Gross Pay: '&cnvrt$('pic(-$$,$$$,$$$,$$$,$$$,$$#.##)',total_gross_pay)
56080   pr #255: '  Total Net Pay: '&rpt$(' ',88)&cnvrt$('pic(-$$,$$$,$$$,$$$,$$$,$$#.##)',total_net_pay)
56900   return  ! /r
58000 PGOF: ! r:
58020   pr #255: newpage
58040   gosub HDR
58060   continue  ! /r
60000 HDR: ! r:
60020   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
60040   pr #255: "\qc  {\f221 \fs22 \b "&env$('cnam')&"}"
60060   pr #255: "\qc  {\f201 \fs20 \b Payroll Check Register}" ! pr #255: "\qc  {\f201 \fs20 \b "&env$('program_caption')&"}"
60080   pr #255: "\qc  {\f181 \fs16 \b Payroll Date: "&cnvrt$("pic(zz/zz/zz)",ppd)&"}"
60100 ! pr #255: "\qc  {\f181 \fs16 \b "&TRIM$(D$)&"}"
60120   pr #255: "\ql   "
60160   pr #255: tab(29);"<----------------Hours----------------->";
60180   pr #255: tab(71);"<-Pay->";
60200   pr #255: tab(79);"<-----------------Deductions---------------->";
60220   pr #255: tab(129);"Net"
60240   pr #255: "CK #   Emp #  Name";
60260   pr #255: tab(29);"  Reg    O/T   Sick    Vac    Hol  Total";
60280   pr #255: tab(71);"  Total   Med WH     FICA  Federal    State    Other      Pay"
60300   return  ! /r
62000 FINIS: ! r:
62010   gosub TOTALS
62020   close #1: ioerr ignore
62040   close #2: ioerr ignore
62050   close #h_checks: ioerr ignore
62052   close #h_dd: ioerr ignore
62060   if append_reg1 then 
62062     if env$('ACSDeveloper')<>'' then 
62064       pr #255: '----newpage (ACSDeveloper Style)----'
62066     else 
62068       pr #255: newpage
62070     end if 
62072   else 
62074     fncloseprn
62076   end if 
62080   fnpayroll_register_2(0,include_tips_in_other_wh,append_reg1)
62100   goto XIT ! /r let fnchain("S:\acsPR\newprReg2")
64000 XIT: let fnxit
66000 IGNORE: continue 
68000 ! <Updateable Region: ERTN>
68020 ERTN: let fnerror(program$,err,line,act$,"xit")
68040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
68060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
68080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
68100 ERTN_EXEC_ACT: execute act$ : goto ERTN
68120 ! /region
