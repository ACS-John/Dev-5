00010 ! formerly S:\acsPR\newprRevCal
00020 ! Reverse Calculation
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fnerror,fndate_mmddyy_to_ccyymmdd,fntos,fnlbl,fntxt,fncmdkey,fnacs,fncombof,fnmsgbox,fnchain,fnDedNames
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim x$*8,tdc(6)
00080   dim bankgl$*12,bk$*24,glwk$*256,cap$*128
00090   dim tr$(5)*35,tr(2),resp$(5)*40
00100   dim tgl$*12,t(26),prgl(15,3),prgl$(15)*12 ! ,desc$*50
00110   dim tr$*12,td$*30,dat$*20,cp(32),tdc(10)
00120   dim a$*40,em$*30,tgl(3),tcp(32),eno$*8,ttgl(3)
00130   dim ml$(3)*100,fullname$(20)*20,abrevname$(20)*8,newcalcode(20)
00140   dim newdedfed(20),dedfica(20),dedst(20),deduc(20),dedcode(20),gl$(20)*12
00150 ! ______________________________________________________________________
00160   let fntop(program$,cap$="Reverse Calculation")
00180 ! 
00190   let cd1=val(date$(4:5)&date$(7:8)&date$(1:2))
00200   fnDedNames(mat fullname$,mat abrevname$,mat dedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
00210   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00212   read #1,using 'Form POS 605,C 12,N 1': bankgl$,gli
00214   close #1: 
00220   if gli=1 then 
00222     gosub POSTGL0
00224     open #14: "Name="&env$('Q')&"\GLmstr\GLBRec.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLRecIdx.h"&env$('cno')&",Shr",internal,outin,keyed ioerr L250
00226     goto L260
00228 L250: ! 
00230     let gli=0
00260 L260: ! 
00262   end if 
00264   open #6: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TRIDX1.H"&env$('cno')&",Shr",internal,outin,keyed ioerr L330
00270   open #7: "Name="&env$('Q')&"\CLmstr\TRMSTR.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\TRIDX2.H"&env$('cno')&",Shr",internal,outin,keyed 
00280   if exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then 
00282     open #tralloc:=8: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",Version=2,KFName="&env$('Q')&"\CLmstr\TrAlloc-Idx.h"&env$('cno')&",Shr",internal,outin,keyed 
00284   else 
00286     open #tralloc:=8: "Name="&env$('Q')&"\CLmstr\TrAlloc.h"&env$('cno')&",Shr",internal,outin,relative 
00288   end if 
00290   open #9: "Name="&env$('Q')&"\CLmstr\BankMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\BankIdx1.H"&env$('cno')&",Shr",internal,outin,keyed 
00300   open #20: "Name="&env$('Q')&"\CLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative ioerr L330
00310   read #20,using 'Form POS 152,N 2',rec=1: bcde
00312   close #20: 
00320   let cli=1
00330 L330: ! 
00340   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,outin,keyed 
00350   open #hDepartment:=5: "Name="&env$('Q')&"\PRmstr\Department.h"&env$('cno')&",Shr, KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&env$('cno')&",Shr",internal,outin,keyed 
00370   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
00380 ! ______________________________________________________________________
00390   open #11: "Name="&env$('Q')&"\PRmstr\Dates.h"&env$('cno')&",Shr",internal,input,relative 
00400   read #11,using "form pos 1,x 16,x 32,n 8",rec=1: d1 norec ignore
00410   close #11: 
00420   let dat$=cnvrt$("pic(########)",d1)
00430   let dat=val(dat$(5:6))*10000 +val(dat$(7:8))*100 +val(dat$(3:4)) ! set payroll date back to mmddyy format for some files
00440 ! ______________________________________________________________________
00450 ASK_EMPLOYEE_NO: ! 
00460   if reverse_all=1 then goto L730
00470   let fntos(sn$="Prrevcal")
00472   let respc=0
00480   let fnlbl(1,1,"Employee to Reverse:",25,1)
00490   let fncombof("Employee",1,28,0,env$('Q')&"\PRmstr\rpmstr.h"&env$('cno'),1,8,9,20,env$('Q')&"\PRmstr\Rpindex.h"&env$('cno'),2,0, "Select the employee to reverse.")
00492   let resp$(respc+=1)="[All]"
00500   let fnlbl(2,1,"Payroll Date:",25,1)
00510   let fntxt(2,28,12,0,1,"3",0,"You can reverse a check from any pay period.  Be sure the payroll date is correct.")
00520   let resp$(respc+=1)=str$(d1)
00530   let fnlbl(3,1,"Check Number:",25,1)
00540   let fntxt(3,28,8,0,1,"30",0,"Only applicable if checks have been printed. ")
00550   let resp$(respc+=1)= ""
00560   if exists(env$('Q')&'\CLmstr\BankMstr.h'&env$('cno')) then 
00570     let fnlbl(4,1,"Bank Code for Checkbook:",25,1)
00580     let fncombof('Bank',4,28,0,env$('Q')&"\CLmstr\BankMstr.h"&env$('cno'),1,2,3,30,env$('Q')&"\CLmstr\BankIdx1.h"&env$('cno'),limit_to_list,0,'',frame)
00582     let resp$(resp_cl_bank_code=respc+=1)=str$(bcde)
00584   end if 
00590   if success=1 then 
00592     let fnlbl(6,1,"Employee Number "&str$(eno)&" successfully reversed!",40,1)
00594   end if 
00600   let fncmdkey("&Next",1,1,0,"Proceed with reversing of payroll." )
00602   let fncmdkey("E&xit",5,0,1,"Returns to menu")
00610 ! 
00620   let fnacs(sn$,0,mat resp$,ckey) ! ask employee #
00630   if ckey=5 then goto XIT
00640   let success=0
00650   if resp$(1)="[All]" then let reverse_all=1 : goto L670
00660   let eno=val(resp$(1)(1:8))
00670 L670: let d1=val(resp$(2))
00680   if d1=0 then 
00682     mat ml$(2)
00684     let ml$(1)="You must enter the payroll date!"
00686     let fnmsgbox(mat ml$,resp$,cap$,0)
00688     goto ASK_EMPLOYEE_NO
00689   end if 
00690   let w1=val(resp$(3))
00700   if resp_cl_bank_code then let bcde=val(resp$(resp_cl_bank_code)(1:2))
00710   mat tcp=(0)
00720   if reverse_all=1 then gosub CREATE_LIST
00730   L730: ! r: main loop
00732   if reverse_all=1 then read #13,using "form pos 1,n 8,n 7": eno,w1 eof FINIS
00740   gosub REVERSE_BANK_REC
00750   if cli=1 then gosub UPDATE_CHECKBOOK ! update checkbook
00760   let x$=lpad$(str$(eno),8)
00770   read #1,using L780,key=x$,release: em10,em11 nokey L1190
00780 L780: form pos 132,2*pd 4.2
00790   if pgl1=1 then gosub POSTGL2
00800   let ck=0
00810 ! DELETE_OLD_PAYROLL_CHECK: !
00820   let checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
00830   restore #4,key>=checkkey$: nokey UPDATE_MASTER
00840 L840: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof UPDATE_MASTER
00850   if heno<>eno then goto UPDATE_MASTER
00860   if prd=d1 then mat tcp=tcp+cp : delete #4: 
00870   let ck=1
00890   if prd=d1 then let em10=em10+tdc(3) ! add sick hours back
00900   if prd=d1 then let em11=em11+tdc(4) ! add vacation hours back
00910   goto L840
00920 UPDATE_MASTER: ! 
00930   if ck=0 then goto L1220
00940   rewrite #1,using L950,key=x$: em10,em11,0 ! WRITE 0 IN LAST PAYROLL DATE IN MASTER RECORD
00950 L950: form pos 132,2*pd 4.2,pos 162,n 6
00960   let success=1
00970 ! UPDATE_DEPARTMENT: !
00980   restore #hDepartment,key>=cnvrt$("pic(zzzzzzz#)",eno)&"   ": nokey L1040
00990   do
00992     read #hDepartment,using 'Form POS 1,N 8,n 3,pos 42,n 6': teno,tdn,lastpd eof L1040
01000     if teno=eno and fndate_mmddyy_to_ccyymmdd(lastpd)=d1 then 
01020       rewrite #hDepartment,using "form pos 42,n 6": 0
01022     end if
01030   loop while teno=eno
01040   L1040: !
01042   goto ASK_EMPLOYEE_NO
01044  FINIS: !
01050  if pgl1=1 then goto POSTGL4
01060  close #1: ioerr ignore 
01070  close #2: ioerr ignore 
01080  goto XIT
01090 ! /r
01100 REVERSE_BANK_REC: ! r:
01110   let hw1=0
01120   if gli<>1 then goto L1170
01130   let bk$=bankgl$&lpad$(str$(w1),12)
01140   rewrite #14,using L1150,key=bk$: 0 nokey L1170
01150 L1150: form pos 63,pd 5.2
01160   let hw1=w1
01170 L1170: ! 
01180   return  ! /r
01190 L1190: mat ml$(2) 
01192   let ml$(1)="Employee Number "&ltrm$(x$)&" does not exist!" 
01194   let ml$(2)="Please select a different Employee Number." 
01196   let fnmsgbox(mat ml$,resp$,cap$,0)
01200   goto ASK_EMPLOYEE_NO
01210 ! ______________________________________________________________________
01220 L1220: mat ml$(2) 
01222   let ml$(1)="No information found for Employee Number "&ltrm$(x$) 
01224   let ml$(2)="Please select a different Employee Number." 
01226   let fnmsgbox(mat ml$,resp$,cap$,0)
01230   goto ASK_EMPLOYEE_NO
01240 ! ______________________________________________________________________
01250 UPDATE_CHECKBOOK: ! r:
01260   if bcde=0 and w1=0 then goto L1510 ! no bank info
01270   let k$=cnvrt$("N 2",bcde)&"1"&lpad$(str$(w1),8)
01280   read #6,using L1320,key=k$: bcde,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd nokey L1300
01290   goto L1320
01300 L1300: mat ml$(2) 
01302   let ml$(1)="Did not find check # "&str$(w1)&" in the Check " 
01304   let ml$(2)="Book system. Will proceed without voiding." 
01306   let fnmsgbox(mat ml$,resp$,cap$,0)
01310   goto L1510
01320 L1320: form pos 1,n 2,n 1,c 8,g 6,pd 10.2,c 8,c 35,n 1,n 6,n 1
01330   let adr=tr(1)
01340   read #9,using L1350,key=cnvrt$("N 2",bcde): bal nokey L1390
01350 L1350: form pos 45,2*pd 6.2
01360   let bal=bal+tr3
01370   rewrite #9,using L1350,key=cnvrt$("N 2",bcde): bal
01380   let tr3=0
01390 L1390: let tr$(3)=tr$(4)=""
01400   let tr$(5)="VOID"
01410   let adr=tr(1)
01420   mat tr=(0)
01430   if clr=0 then let clr=cd1
01440   rewrite #6,using L1320,key=k$: bcde,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd
01450   if exists(env$('Q')&"\CLmstr\Tralloc-Idx.h"&env$('cno')) then goto DELETE4_02ALLOC
01460 L1460: if adr=0 then goto L1510
01470   read #8,using 'form pos 65,pd 3',rec=adr: nta norec L1510
01480   delete #8,rec=adr: 
01490   let adr=nta: goto L1460
01510 L1510: return ! /r
01520 DELETE4_02ALLOC: ! r: delete allocations in 4.02 checkbook system
01530   restore #tralloc,key=k$: nokey L1570
01540   L1540: read #8,using "form pos 1,c 11": trallockey$
01550   if trallockey$<>k$ then goto L1510
01560   delete #tralloc,key=k$: 
01570 L1570: goto L1540
01580 ! /r
01590 POSTGL0: ! r:
01600   mat ml$(2) 
01602   let ml$(1)="Do you wish to create reversing" 
01604   let ml$(2)="General Ledger entries? (Y/N)?" 
01606   let fnmsgbox(mat ml$,resp$,cap$,292)
01610   if resp$(1:1)="Y" then let pgl1=1 else let pgl1=0
01620   if pgl1=1 then gosub POSTGL1
01630   return  ! /r
01640 ! ______________________________________________________________________
01650 POSTGL1: ! r:
01652   let glinstal=1
01660   let fli2$(1)="11,64,n 3,u"
01670   let fli2$(2)="11,68,n 6,u"
01680   let fli2$(3)="11,75,n 3,u"
01690   open #1: "Name="&env$('Q')&"\GLmstr\GLBUCKET.H"&env$('cno')&",Shr",internal,input,relative ioerr L1740
01700   read #1,using 'form pos 1,n 1',rec=1: glb norec ignore
01720   close #1: 
01730   if glb=2 then gosub L3690
01740   L1740: !
01780   let fntos(sn$="Prrevcal2")
01782   let respc=0
01790   let fnlbl(1,1,"General Ledger Posting Date:",25,1)
01800   let fntxt(1,28,12,0,1,"3",0,"If this revesing entry should be posted to the general ledger, what date should be used?")
01810   let fncmdkey("&Next",1,1,0,"Proceed with reversing entry." )
01812   let fncmdkey("E&xit",5,0,1,"Don't Post")
01820   let fnacs(sn$,0,mat resp$,ckey) ! posting date
01830   if ckey<>5 then 
01840     let dat1=val(resp$(1))
01850     if glb=2 then let glwk$=env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",dat1)&".H"&env$('cno')
01860     if glb><2 then let glwk$=env$('Q')&"\GLmstr\GL_Work_"&env$('acsUserId')&".h"&env$('cno')
01870     if glb=2 and uprc$(rtrm$(accrue$))="Y" then open #11: "Name="&env$('Q')&"\GLmstr\GL"&cnvrt$("PIC(######)",d2)&".H"&env$('cno')&",RecL=104,USE",internal,output 
01880     open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
01890     read #1,using 'form pos 1,c 40,pos 437,15*c 12': a$,mat prgl$
01910     close #1: 
01920     for j=1 to 15
01930       let prgl(j,1)=val(prgl$(j)(1:3))
01940       let prgl(j,2)=val(prgl$(j)(4:9))
01950       let prgl(j,3)=val(prgl$(j)(10:12))
01960     next j
01970     let nametab=36-len(rtrm$(a$))/2
01980     let fnopenprn
01990     gosub glDistHeaders
02000   end if
02010 return  ! /r
02020 POSTGL2: ! r:
02030   let oldteno=teno
02040   let rec1=ta(1)
02050   L2050: if rec1=0 then goto L2340
02060   read #2,using L2070,rec=rec1: teno,mat tgl,dat,mat tcp,nta
02070   L2070: form pos 1,n 8,x 3,n 3,n 6,n 3,pos 42,n 6,pos 358,22*pd 5.2,pos 468,pd 3
02080   if dat><d1 then goto L2340
02090   if tgl(2)=0 then goto L2340
02100   if mastercd=0 then gosub OPNWORK_DUESTUFF
02110   let tgl$=lpad$(str$(tgl(1)),3)&lpad$(str$(tgl(2)),6)&lpad$(str$(tgl(3)),3)
02120   if tgl(1)=0 or tgl(1)=oldtgl then goto L2130 else gosub OPNWORK_DUESTUFF
02130   L2130: if oldteno=teno then goto L2160
02140   let eno$=lpad$(str$(teno),8)
02150   read #1,using 'form pos 9,c 30',key=eno$,release: em$ nokey L2340
02160   L2160: !
02170   print #255,using L2180: teno,em$,mat tgl,-tcp(31)+tcp(29)+tcp(30) pageoflow PGOF
02180   L2180: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),n 12.2,skip 1
02190   for j=1 to 24 ! ACCUMULATE 24 WITHHOLDINGS
02200     if j<=4 then goto L2240
02210     if dedcode(j-4)=1 then goto L2240
02220     let t(j)=t(j)+tcp(j)
02230     goto L2250
02240   L2240: let t(j)=t(j)-tcp(j)
02250   L2250: next j
02260   let t(25)=t(25)+tcp(25) ! EIC
02270   let t(26)=t(26)-tcp(32) ! ACCUMULATE NET
02280   let subtotal=subtotal+tcp(31)-tcp(29)-tcp(30) ! ACCUMULATE TOTAL BY ACCT TO BE POSTED TO GL
02290   let totaldue=totaldue-tcp(31)+tcp(29)+tcp(30) ! DUE TO PAYROLL CLEARING
02300   let totaldr=totaldr+tcp(31)-tcp(29)-tcp(30)
02310   let totalrec=totalrec+tcp(31)-tcp(29)-tcp(30) ! TOTAL DUE FRO OTHER FUNDS
02320   mat ttgl=tgl : gosub POSTGL3
02330   let rec1=nta: goto L2050
02340   L2340: ! 
02350 return  ! /r
02360 POSTGL3: ! r: SUBTOTAL ROUTINE AND WRITE GL TRANS
02370   if glinstal=0 then goto L2460
02380   if diskin=0 then gosub L3340
02390   let td$="Payroll summary"
02400   if uprc$(accrue$)<>"Y" then goto L2440
02410   let accrued=round(subtotal/day*dayslm,2)
02420   write #11,using L2450: mat ttgl,d2,accrued,5,0,tr$,"Payroll accrual",prgl$(15)
02430   let totacc=totacc+accrued
02440 L2440: write #3,using L2450: mat ttgl,dat,-subtotal+accrued,5,0,tr$,"Reversing "&em$,prgl$(15)
02450 L2450: form pos 1,n 3,n 6,n 3,n 6,pd 6.2,n 2,n 2,c 12,c 52,c 12
02460 L2460: print #255,using L2470: "-----------",-subtotal
02470 L2470: form pos 65,c 11,skip 1,pos 64,pic(---------.##),skip 1
02480   if accrued<>0 then print #255,using L2490: "ACCRUED PORTION",accrued else print #255: 
02490 L2490: form pos 45,c 16,pos 64,pic(---------.##),skip 2
02500   let subtotal=0
02510   return  ! /r
02530 glDistHeaders: ! r: headers for General Ledger Distribution for Payroll
02532   let p1=p1+1
02540   print #255,using L2550: date$,a$,"PAGE",p1
02550 L2550: form skip 1,pos 1,c 8,pos nametab,c 40,pos 77,c 5,pic(zzz),skip 1
02560   print #255,using L2570: time$,"General Ledger Distribution for Payroll",dat1
02570 L2570: form pos 1,c 8,pos 17,c 40,skip 1,pos 29,pic(zz/zz/zz),skip 2
02580   print #255: "Employee                                               G/L                 Amount"
02590   print #255: " Number        Name                                  Account         Debits     Credits"
02600   print #255: ""
02610   return  ! /r
02630 POSTGL4: ! r:
02640   mat ttgl=tgl
02650   ! GOSUB 1790 ! WRITE LAST ENTRY
02660   if multigl=1 then  ! ONLY ONE FUND OR COMPANY
02670     gosub OPNWORK_DUESTUFF
02680     gosub L3520
02690   end if
02692   gosub POST_WH_AND_NET
02700   gosub PRINT_TOTALS
02710   let fncloseprn
02720   if glinstal=0 then goto XIT
02730   if glb=2 then goto XIT
02740 let fnchain("S:\acsGL\ACGLMRGE") ! /r
02750 XIT: let fnxit
02760 PGOF: ! r:
02762   print #255: newpage
02770   gosub glDistHeaders
02780 continue  ! /r
02800 POST_WH_AND_NET: ! r: ASSIGN G/L NUMBERS AND POST TO GL WORK FILE
02810   for j=1 to 26
02820     if t(j)=0 then goto L3030
02830     if t(j)<0 then goto L2950
02840     if j<=4 then goto L2850 else goto L2870
02850     L2850: ! 
02852     print #255,using L2180: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),-t(j) pageoflow PGOF
02860     goto L2930
02870     L2870: ! 
02872     print #255,using L2920: 0," ",gl$(j-4),-t(j) pageoflow PGOF
02880     if j<25 then goto L2890 else goto L2910
02890     L2890: ! 
02892     print #255,using L2920: 0," ",gl$(j-4),-t(j) pageoflow PGOF
02900     goto L2920
02910     L2910: ! 
02912     print #255,using L2920: 0," ",gl$(j-11),-t(j) pageoflow PGOF
02920     L2920: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,c 12,n 12.2,skip 1
02930     L2930: ! 
02932     let totaldr=totaldr+t(j)
02940     goto L2980
02950     L2950: ! 
02952     print #255,using L2960: 0," ",prgl(j,1),prgl(j,2),prgl(j,3),-t(j) pageoflow PGOF
02960     L2960: form pos 1,pic(zzzzzzzz),pos 15,c 30,pos 50,pic(zzz),x 1,pic(zzzzz#),x 1,pic(zzz),x 12,n 12.2,skip 1
02970     let totalcr=totalcr+t(j)
02980     L2980: ! 
02982     if glinstal=0 then goto L3030
02990     if j<=4 then write #3,using L2450: prgl(j,1),prgl(j,2),prgl(j,3),dat,-t(j),5,0,tr$,td$,prgl$(15)
03000     if j>4 and j<25 then write #3,using L3010: gl$(j-4),dat,-t(j),5,0,tr$,td$,prgl$(15)
03010     L3010: form pos 1,n 3,c 12,pd 6.2,n 2,n 2,c 12,c 52,c 12
03020     if j>24 then write #3,using L2450: prgl(j-11,1),prgl(j-11,2),prgl(j-11,3),dat,-t(j),5,0,tr$,td$,prgl$(15)
03030     L3030: ! 
03032   next j
03040   if uprc$(accrue$)<>"Y" then goto L3070
03050   write #11,using L2450: g1,g2,g3,d2,-totacc,5,0,tr$,"Payroll accrual",prgl$(15)
03060   write #3,using L2450: g1,g2,g3,dat,-totacc,5,0,tr$,"Payroll accrual",prgl$(15)
03070   L3070: ! 
03080 return  ! /r
03090 OPNWORK_DUESTUFF: ! r: OPEN G/L WORK FILES AND CREATE DUE TO AND DUE FROM ENTRIES
03100   if tgl(1)=0 then goto L3330
03110   if mastercd=1 then goto L3160
03120   mat ml$(2) 
03122   let ml$(1)="The G/L accounts you are using indicate you have seperate funds or" 
03124   let ml$(2)="cost centers on the system.  Enter yes if you have more than one fund." 
03126   let fnmsgbox(mat ml$,resp$,cap$,4)
03130   if resp$(1:1)="T" then let multigl=1
03140   if multigl><1 then goto L3160
03160   L3160: if multigl=2 then goto L3330
03170   ! CREATE DUE TO PAYROLL FUND ENTRIES
03180   if mastercd=0 then goto L3290 ! FIRST TIME THRU ROUTINE
03190   print fields "10,2,c 78": "ENTER THE G/L ACCOUNT # FOR YOUR 'DUE TO PAYROLL CLEARING ACCOUNT '"
03200   print fields "11,2,c 60": "ON FUND # "&tgl$(1:3)
03210   L3210: input fields mat fli2$: mat ttgl conv L3210
03220   print #255,using L2960: 0," ",mat ttgl,-totaldue
03230   let totalcr=totalcr+totaldue
03240   gosub PRINT_TOTALS
03250   gosub glDistHeaders
03260   if glinstal=0 then goto L3290
03270   write #3,using L2450: mat ttgl,dat,-totaldue,5,0," ","Reversing Payroll summary",prgl$(15)
03280   close #3: 
03290   L3290: let totaldue=0
03300   let totalcr=0
03310   let totaldr=0
03330   L3330: let mastercd=1
03340   L3340: if glinstal=0 then goto L3490
03380   let diskin=1
03390   open #3: "Name="&glwk$,internal,outin ioerr L3450
03400   read #3,using L3410: dat2,trcode eof L3410
03410   L3410: form pos 13,n 6,pos 25,n 2
03420   if dat2=dat and trcode=5 then goto L3490
03430   if glb=2 then goto L3490
03440   close #3,free: 
03450   L3450: open #3: "Name="&glwk$&",size=0,RecL=104",internal,output ioerr L3480
03460   goto L3490
03470   close #3: 
03480   L3480: open #3: "Name="&glwk$,internal,output ioerr L3490
03490   L3490: let oldtgl=tgl(1)
03500 return  ! /r
03520 L3520: ! r: FINAL PAGE FOR CONTROL SET OF BOOKS  (MULTI-FUNDS ONLY)
03530   print fields "10,2,c 78": "ENTER THE G/L ACCOUNT # FOR YOUR 'DUE FROM OTHER FUNDS '"
03540   print fields "11,2,c 60": "ON YOUR CONTROL SET OF BOOKS"
03550   input fields mat fli2$: mat ttgl conv L3210
03560   print #255,using L2180: 0," ",mat ttgl,-totalrec
03570   let totaldr=totaldr+totalrec
03580   if glinstal=0 then goto L3600
03590   write #3,using L2450: mat ttgl,dat,-totalrec,5,0," ","Reversing Payroll summary",prgl$(15)
03600 L3600: ! 
03610   return  ! /r
03620 PRINT_TOTALS: ! r: AND UNDERLINES
03630   if totacc<>0 then print #255,using L2180: 0," ",g1,g2,g3,-totacc
03640   print #255,using L3650: "___________","___________",-totaldr,-totalcr
03650 L3650: form pos 65,c 11,x 1,c 11,skip 1,pos 64,pic(---------.##),pic(---------.##),skip 1
03660   print #255,using L3650: "===========","==========="
03670   return  ! /r
03690 L3690: ! r:
03692   close #101: ioerr ignore
03700   open #12: "Name="&env$('Q')&"\GLmstr\GLmstr.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLINDEX.H"&env$('cno')&",Shr",internal,input,keyed ioerr ignore
03720   mat ml$(2) 
03722   let ml$(1)="Did you accrue part of this payroll" 
03724   let ml$(2)="in the previous month? (Y/N)" 
03726   let fnmsgbox(mat ml$,resp$,cap$,36)
03730   let accrue$=resp$(1)(1:1)
03740   return  ! /r
03760 ! ACCRUAL: ! r:
03770 !   let fntos(sn$="Prrevcal3") 
03772   !   let respc=0: let mypos=50
03780 !   let fnlbl(1,1,"Number of Days in this Pay Period:",mypos,1)
03790 !   let fntxt(1,mypos+3,10,0,1,"30",0,"In order to know how much to accure, the system needs to know the days to accure.") 
03792   !   let resp$(1)=str$(day)
03800 !   let fnlbl(2,1,"Number of Days to Expense in Last Month:",mypos,1)
03810 !   let fntxt(2,mypos+3,10,0,1,"30",0,"In order to know how much to accure, the system needs to know the days to accure.") 
03812   !   let resp$(2)=str$(dayslm)
03820 !   let fnlbl(3,1,"G/L # for Due From Other Funds on Fund # "&oldtgl$(1:3)&":",mypos,1)
03830 !   let fnqgl(3,mypos+3,0,2,pas) 
03832   !   let resp$(3)=fnrgl$(bankgl$)
03840 !   let fnlbl(4,1,"Last Day of Previous Month:",mypos,1)
03850 !   let fntxt(4,mypos+3,10,0,1,"1",0,"Enter the month end date.") 
03852   !   let resp$(4)=str$(d2)
03860 !   let fncmdkey("&Next",1,1,0,"Continue posting." ) 
03862   !   let fncmdkey("E&xit",5,0,1,"Returns to menu")
03870 !   let fnacs(sn$,0,mat resp$,ckey) ! ask accrual info
03880 !   if ckey=5 then goto XIT
03890 !   let day=val(resp$(1)) ! days in pay period
03900 !   let dayslm=val(resp$(2)) ! days last month
03910 !   let key$=fnagl$(resp$(3))
03920 !   let g1=val(key$(1:3)): let g2=val(key$(4:9)) : let g3=val(key$(10:12))
03930 !   let d2=val(resp$(4)) ! last day previous month
03940 !   let acgl$=cnvrt$("N 3",g1)&cnvrt$("N 6",g2)&cnvrt$("N 3",g3)
03950 !   if trim$(acgl$)<>"" then read #12,using L3960,key=acgl$: desc$ nokey L3970
03960 ! L3960: form pos 13,c 30
03970 ! L3970: !
03980 ! return ! /r
03985 CREATE_LIST: ! r:
03990   open #13: "Name="&env$('temp')&"\prreverse"&session$&",size=0,RecL=128,replace",internal,outin 
04000 L4000: read #1,using "FORM pos 1,n 8,pos 132,2*PD 4.2",release: eno,em10,em11 eof L4100
04020   let checkkey$=cnvrt$("pic(ZZZZZZZ#)",eno)&"         "
04030   restore #4,key>=checkkey$: nokey L4000
04040 L4040: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat cp eof L4000
04050   if heno<>eno then goto L4000
04060   if prd=d1 then goto L4080
04070   goto L4040
04080 L4080: write #13,using 'form pos 1,n 8,n 7': eno,ckno
04090   goto L4000
04100 L4100: restore #13: 
04110   return  ! /r
04120 IGNORE: continue 
04130 ! <Updateable Region: ERTN>
04140 ERTN: let fnerror(program$,err,line,act$,"xit")
04150   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
04160   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
04170   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
04180 ERTN_EXEC_ACT: execute act$ : goto ERTN
04190 ! /region
