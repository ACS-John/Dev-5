00020   library 'S:\Core\Library': fntop,fnxit, fnerror,fnss_employee,fnss_employer,fnDedNames
00030   on error goto ERTN
00040 ! ______________________________________________________________________
00050   dim em$(3)*30,tdc(10),tcp(32)
00060   dim a$(3)*40,d$(10)*8,io1$(11),i$*62,j$*70,gln$(15)*12 ! old S(7,8),
00070   dim tad(29),tradesc$*30,lcn$*8,tr$(5)*35
00080   dim rpnames2$(10)*6,dept(6),dedcode(10),bankgl$*12,gl$*12,bn$*30
00090   dim cap$*128
00130   fntop(program$,cap$="Update Checkbook From Check History")
00150 ! 
00160   gosub READ_COMPANY_INFO
00170   let mcr=mcr*.01
00180   ssrate1=fnss_employee*.01
00190   ssrate2=fnss_employer*.01
00200   open #1: "Name="&env$('Q')&"\PRmstr\RPMstr.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPIndex.h"&env$('cno')&",Shr",internal,outin,keyed 
00210   open #3: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,input,keyed 
00220   open #4: "Name="&env$('Q')&"\PRmstr\PayrollChecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno')&",Shr",internal,outin,keyed 
00230   open #7: "Name="&env$('Q')&"\PRmstr\MGLMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\MGLIDX1.h"&env$('cno')&",Shr",internal,input,keyed 
00240   gosub CHECK_ACSCL
00250   do 
00260     read #4,using F_HIST: heno,tdn,prd,ckno,mat tdc,mat tcp eof END_HIST
00270 F_HIST: form pos 1,n 8,n 3,pd 6,n 7,5*pd 3.2,37*pd 5.2
00280     if tcp(2)=round(tcp(31)*ssrate1,2) then let tdc(7)=tcp(31) else let tdc(7)=round(tcp(2)/ssrate1,2)
00290     if tcp(3)=round(tcp(31)*mcr,2) then let tdc(8)=tcp(31) else let tdc(8)=round(tcp(3)/mcr,2) ! calculate medicare wages
00300     if prd=20120224 then gosub BUILD_CHECK_RECORD
00310   loop 
00320 CHECK_ACSCL: ! r:
00330   open #12: "Name="&env$('Q')&"\CLmstr\BankMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00340   open #15: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,outin,relative 
00350   open #8: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx1.h"&env$('cno')&",Shr",internal,outin,keyed 
00360   open #22: "Name="&env$('Q')&"\CLmstr\TrMstr.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrIdx2.h"&env$('cno')&",Shr",internal,outin,keyed 
00370   open #tralloc:=23: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&env$('cno')&",Shr",internal,outin,keyed 
00380   read #15,using L3830,rec=1,release: bankcode,prenum,port
00390 L3830: form pos 152,n 2,pos 406,n 1,pos 788,n 1
00400   read #12,using FM_BANK,key=lpad$(str$(bankcode),2),release: bn$,bal,upi,nckno nokey L3870
00410 FM_BANK: form pos 3,c 30,pos 45,pd 6.2,pd 6.2,g 8
00420 L3870: return  ! /r
00430 BUILD_CHECK_RECORD: ! 
00440   eno$=lpad$(str$(heno),8)
00450   em$(1)=''
00460   read #1,using FM_PRMSTR,key=eno$: em$(1) nokey FM_PRMSTR
00470 FM_PRMSTR: form pos 9,c 30
00480   let tr$(1)=cnvrt$("n 8",ckno)
00490   let dat$=str$(prd)
00500   let dat=val(dat$(5:6)&dat$(7:8)&dat$(3:4))
00510   let tdn$=cnvrt$("n 3",tdn)
00520   read #3,using FM_DEPT,key=eno$&tdn$: pgl$
00530 FM_DEPT: form pos 12,c 12
00540 DELETE_ACSCL: ! delete old check records
00550   clk$=lpad$(str$(bankcode),2)&"1"&tr$(1)
00560   read #8,using F_TRMSTR,key=clk$: bc$,tcde$,otr1$,otr2$,otr3 nokey WRITE_ACSCL
00570   bal=bal-otr3
00580   delete #8,key=clk$: 
00590   restore #tralloc,key>=clk$: nokey WRITE_ACSCL
00600 RD_TRALLOC: read #tralloc,using 'Form Pos 1,C 11': newkey$ eof WRITE_ACSCL
00610   if newkey$=clk$ then delete #tralloc: : goto RD_TRALLOC
00620 WRITE_ACSCL: ! 
00630   let tr$(2)=lpad$(str$(dat),6)
00640   let tr3=tcp(32)
00650   let tr$(4)=eno$
00660   let tr$(5)=em$(1)
00670   write #8,using F_TRMSTR: bankcode,1,tr$(1),tr$(2),tr3,tr$(4),tr$(5),0,0,4
00680 F_TRMSTR: form pos 1,g 2,g 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1
00690   bal=bal+tr3
00700   let tragl$=pgl$
00710   let traamt=tcp(31)
00720   let tradesc$="Gross Pay"
00730   let traivd$=str$(dat)
00740   write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
00750 WRITE_TRALLOC: ! 
00760   for j=1 to 25
00770     if j=1 then let tragl$=gln$(1): let tradesc$="Federal WH"
00780     if j=2 then let tragl$=gln$(2): let tradesc$="Soc-Sec WH"
00790     if j=3 then let tragl$=gln$(2): let tradesc$="Medicare WH"
00800     if j=4 then let tragl$=gln$(3): let tradesc$="State WH"
00810     if j>4 and j<25 then let tragl$=gl$(j-4): let tradesc$=fullname$(j-4)
00820     if j=25 then let tragl$=gln$(14) : let tradesc$="EIC"
00830     let traamt=-tcp(j)
00840     if j>4 and j<25 and newdedcode(j-4)>1 then let traamt=-traamt
00850     if traamt=0 then goto NXTJ
00860     write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
00870 FM_TRALLOC: form pos 1,c 11,c 12,pd 5.2,c 30,g 6,c 3,c 12,n 1
00880 NXTJ: next j
00890   let tragl$=gln$(2): let tradesc$="Employer's Soc-Sec Match" : let traamt=-round(tdc(7)*ssrate2,2)
00900   write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
00910   let traamt2=traamt
00920   let tragl$=gln$(2): let tradesc$="Employer's Medicare Match" : let traamt=-round(tdc(8)*mcr,2)
00930   write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
00940   let traamt2=-(traamt2+traamt)
00950   let tragl$=gln$(2): let tradesc$="Off Set SS & Medicare Match" : let traamt=traamt2
00960   read #7,using FM_MGLMSTR,key=tdn$: tragl$ nokey WR_SSMATCH
00970 FM_MGLMSTR: form pos 4,11*c 12
00980 WR_SSMATCH: write #tralloc,using FM_TRALLOC: clk$,tragl$,traamt,tradesc$,traivd$,"",trapos$,tragde
00990   rewrite #12,using FM_BANK,rec=1: bn$,bal
01000 END_CHECK_RECORD: return 
01010 READ_COMPANY_INFO: ! 
01020   dim fullname$(20)*20,abrevname$(20)*8,newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20),newdedcode(20),gl$(20)*12
01030   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno'),internal,outin,relative 
01040   read #1,using F_company,rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$
01050 F_company: form pos 1,3*c 40,c 12,pd 6.3,pd 6.2,pd 5.2,10*c 8,n 2,pd 4.2,pd 3.3,12*pd 4.2,10*pd 3.3,25*c 12,31*n 1,10*c 6,3*pd 4.3,3*pd 3.2,4*pd 4.2,n 1,2*c 6,n 2
01060   close #1: 
01070 READNAMES: ! 
01080   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
01100 FM_DNAM: form pos 1,20*c 20,20*c 8,120*n 1,20*c 12
01110   close #2: 
01120   return 
01130 END_HIST: stop 
01140 XIT: stop  ! fnxit
01150 ERTN: let fnerror(program$,err,line,act$,"xit")
01160   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01170   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01180   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01190 ERTN_EXEC_ACT: execute act$ : goto ERTN
