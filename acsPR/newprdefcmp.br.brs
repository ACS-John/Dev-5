00020   on fkey 5 goto END1
00030   on error goto ERTN
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn, fncloseprn,fnerror,fntos,fnfra,fnopt,fnlbl,fntxt,fncmbact,fncmdkey,fnacs,fnchk,fnDedNames
00050   let fntop(program$,cap$="Deferred Compensation Report")
00070 ! 
00080   def fndate_mmddyy_to_ccyymmdd(x)
00090     let x2=(x-int(x*.01)*100)*10000+int(x*.01)
00100     if int(x2*.0001)<90 then let x2=x2+20000000 else let x2=x2+19000000
00110     let fndate_mmddyy_to_ccyymmdd=x2
00120   fnend 
00130   dim em$*30,tcp(32),tdc(10),cp(32),ttdc(10)
00140   dim dedcode(20),calcode(20),dedfed(20),fullname$(20)*20,resp$(50)*60
00150   dim abbrevname$(20)*8,dedfica(20),dedst(20),deduc(20)
00160   dim sel_ded(20),sel_pen(20),cap$*128
00170   fnDedNames(mat fullname$,mat abbrevname$,mat dedcode,mat calcode,mat dedfed,mat dedfica,mat dedst,mat deduc)
00190   gosub L710
00200   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",Shr",internal,input,relative 
00210   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00220   open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno')&",Shr",internal,input,relative 
00230   let fnopenprn
00240   gosub HDR
00250 L250: read #1,using L260: eno,em$,ss$ eof END1
00260 L260: form pos 1,n 8,c 30,pos 99,c 11
00270   let a=pos (rtrm$(em$)," ",1)
00280   let b=pos (rtrm$(em$)," ",a+1)
00290   let em$=rtrm$(em$(max(a+1,b+1):30))&" "&em$(1:a)
00300   let reg_earnings=deferred_comp_wh=deferred_comp_match=0
00310   goto L560
00320 L320: if deferred_comp_wh=0 and deferred_comp_match=0 then goto L360 ! skip if no deferred comp
00330   print #255,using L340: em$(1:24),ss$,reg_earnings,deferred_comp_wh,deferred_comp_match pageoflow L1020
00340 L340: form pos 1,c 24,c 12,4*n 12.2
00350   let total_salary+=reg_earnings !:
        let total_wh+=deferred_comp_wh !:
        let total_deferred_comp+=deferred_comp_match
00360 L360: goto L250
00370 END1: close #1: ioerr L380
00380 L380: close #2: ioerr L400
00390   print #255: "                                      ----------  ----------  ---------- "
00400 L400: print #255,using L410: " "," ",total_salary,total_wh,total_deferred_comp
00410 L410: form pos 1,c 24,c 12,4*n 12.2
00420   print #255: "                                      =========-  ==========  ========== "
00430   form pos 1,c 1,c 32,n 6,3*n 9,c 14,c 2
00440   let fncloseprn
00450   close #25: ioerr XIT
00460 XIT: let fnxit
00470 ! ______________________________________________________________________
00480 ! <Updateable Region: ERTN>
00490 ERTN: let fnerror(program$,err,line,act$,"xit")
00500   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00510   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00520   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00530 ERTN_EXEC_ACT: execute act$ : goto ERTN
00540 ! /region
00550 ! ______________________________________________________________________
00560 L560: let checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
00570   form pos 9,c 30
00580   restore #4,key>=checkkey$: nokey L250
00590   mat tcp=(0): mat ttdc=(0)
00600 L600: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L320
00610   if heno<>eno then goto L320
00620   if prd<beg_date or prd>end_date then goto L600
00630   mat tcp=tcp+cp : mat ttdc=ttdc+tdc
00640   for j=1 to 20
00650     if sel_ded(j)=1 and dedcode(j)=1 then let deferred_comp_wh+=cp(j+4) ! Deferred_Comp wH
00660     if sel_ded(j)=1 and dedcode(j)>1 then let deferred_comp_wh-=cp(j+4) ! Deferred_Comp wH
00670     if sel_pen(j)=1 then let deferred_comp_match+=cp(j+4) ! Deferred_Comp match
00680   next j
00690   let reg_earnings+=cp(31) ! REGULAR EARNINGS
00700   goto L600
00710 L710: let fntos(sn$="Deferred-1") !:
        let rc=cf=0
00720   let fnfra(1,1,20,23,"Deferred Comp W/H","Mark the Deferred Comp Withholding deduction",0) !:
        let cf+=1 : let fratype=cf
00730   for j=1 to 20
00740     let fnchk(j,3,fullname$(j),0,fratype) !:
          let resp$(rc+=1)="False"
00750   next j
00760   let fnfra(1,30,20,23,"Deferred Comp Match","Mark the deferred compensation match.",0) !:
        let cf+=1 : let fratype=cf
00770   for j=1 to 20
00780     let fnopt(j,3,fullname$(j),0,fratype) !:
          let resp$(rc+=1)="False"
00790   next j
00800   let fnfra(1,60,3,42,"Date Range","Enter the beginning and ending date range covered by this report.") !:
        let cf+=1 : let fradate=cf : let mylen=26 : let mypos=mylen+2
00810   let fnlbl(1,1,"Starting Date:",mylen,1,0,fradate)
00820   let fntxt(1,mypos,10,0,1,"3",0,empty$,fradate) !:
        let resp$(rc+=1)=str$(beg_date)
00830   let fnlbl(2,1,"Ending Date:",mylen,1,0,fradate)
00840   let fntxt(2,mypos,10,0,1,"3",0,empty$,fradate) !:
        let resp$(rc+=1)=str$(end_date)
00850   let fncmdkey("Next",1,1,0,"Prints the report")
00860   let fncmdkey("Cancel",5,0,1,"Returns to menu")
00870   let fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
00880   for j=1 to 20
00890     if resp$(j)="True" then let sel_ded(j)=1
00900   next j
00910   for j=1 to 20
00920     if resp$(j+20)="True" then let sel_pen(j)=1
00930   next j
00940   let beg_date=val(resp$(41)) !:
        let end_date=val(resp$(42))
00950   return 
00960 HDR: ! 
00970   print #255: "\qc  {\f181 \fs18 \b "&trim$(env$('cnam'))&"}"
00980   print #255: "\qc  {\f181 \fs24 \b "&env$('program_caption')&"}"
00990   print #255: "\qc  {\f181 \fs16 \b From: "&cnvrt$("pic(zzzz/zz/zz)",beg_date)&" To: "&cnvrt$("pic(zzzz/zz/zz)",end_date)&"}"
01000   print #255: "\ql   " !:
        print #255: "Name                    SS Number     Total Wage     Comp WH  Comp Match"
01010   return 
01020 L1020: print #255: newpage
01030   gosub HDR
01040   continue 
