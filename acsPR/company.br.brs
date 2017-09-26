00020 ! Company Information File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnstyp,fntos,fnlbl,fntxt,fnchk,fnqgl,fnrgl$,fncomboa,fncmdkey,fnacs,fnagl$,fnmsgbox,fnfra,fnDedNames,fnclient_has
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   if fnclient_has('P2') then let fnstyp(11) else let fnstyp(14) !  styp=11 for jobcost; styp=14 for regular payroll
00080   let fntop(program$,cap$="Company Information")
00090   let fncno(cno)
00100 ! 
00110 ! ______________________________________________________________________
00120   dim a$(3)*40,b$(2)*12,d$(10)*8,label1$(10)*20,m(10),r(10)
00130   dim fa$(10)*26,e$(10)*12,prh$(2)*40
00140   dim rpnames$(86)*20,rpnames2$(10)*6,x$(10)*20,io2$(50)*24,na$(125)*8
00150   dim gln(45),glnt(45),gln$(15)*12,dedcode(10),calcode(10),dedfed(10)
00160   dim prgl(15,3),label5$(15)*13,iolabel5$(17),io5$(45)*20,win6$*40
00170   dim x1(3),x2(3),x3(3),x4(3),x5(3),x6(3),x7(3),x8(3),x9(3),x10(3),sck(4)
00180   dim wcm(4),io6$(11)*16,jn$(2)*6,iolabel1$*10,cap$*128
00190   dim resp$(200)*40,ml$(3)*100,fullname$(20)*20,abrevname$(20)*8
00200   dim newcalcode(20),newdedfed(20),dedfica(20),dedst(20),deduc(20)
00210   dim newdedcode(20)
00212   dim gl$(20)*12,fid$*12
00214 ! 
00220   dim opt_ded_or_add$(3)*7
00221   let opt_ded_or_add$(1)="Deduct"
00222   let opt_ded_or_add$(2)="Add"
00223   let opt_ded_or_add$(3)="Benefit"
00224 ! 
00225   dim opt_std_or_percent$(2)*8
00226   let opt_std_or_percent$(1)="Standard"
00227   let opt_std_or_percent$(2)="Percent"
00230 ! ______________________________________________________________________
00240 ! ______________________________________________________________________
00250   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno)&',recl=759,version=0,use',internal,outin,relative 
00260   if lrec(1)=0 then write #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2': mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
00270   read #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc ioerr L290
00280   let ficamaxw=ficamaxw*10
00290 L290: close #1: 
00300 READNAMES: ! 
00310   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
00380 ! ______________________________________________________________________
00390 SCREEN_1: ! 
00400   let resp=0
00410   let fntos(sn$="Company-1") !:
        let mylen=30: let mypos=mylen+3 : let right=1
00420   let fram1=1: let fnfra(1,1,10,80,"Company # "&str$(cno))
00430   let fnlbl(1,1,"Company Name:",mylen,right,0,fram1)
00440   let fntxt(1,mypos,40,0,left,"",0,"",fram1) !:
        let resp$(1)=a$(1)
00450   let fnlbl(2,1,"Company Address:",mylen,right,0,fram1)
00460   let fntxt(2,mypos,40,0,left,"",0,"",fram1) !:
        let resp$(2)=a$(2)
00470   let fnlbl(3,1,"City, State, Zip:",mylen,right,0,fram1)
00480   let fntxt(3,mypos,40,0,left,"",0,"",fram1) !:
        let resp$(3)=a$(3)
00490   let fnlbl(4,1,"Federal ID #:",mylen,right,0,fram1)
00500   let fntxt(4,mypos,12,0,left,"",0,"",fram1) !:
        let resp$(4)=fid$
00510   let fnlbl(5,1,"Federal U/C Rate:",mylen,right,0,fram1)
00520   let fntxt(5,mypos,10,0,left,"33",0,"In 2007 the rate was .8% and should be entered as .8 and not .008",fram1) !:
        let resp$(5)=str$(feducrat)
00530   let fnlbl(6,1,"Federal U/C Maximum Wage:",mylen,right,0,fram1)
00540   let fntxt(6,mypos,12,0,left,"10",0,"",fram1) !:
        let resp$(6)=str$(feducmax)
00550   let fnlbl(7,1,"Social Security Rate:",mylen,right,0,fram1)
00560   let fntxt(7,mypos,10,0,left,"33",0,"Sample format 6.2",fram1 ) !:
        let resp$(7)=str$(ficarate)
00570   let fnlbl(8,1,"SS Maximum Wage:",mylen,right,0,fram1)
00580   let fntxt(8,mypos,12,0,left,"10",0,"The maximum was 97500 for the year 2007.  See a 941 form.",fram1)
00852   let resp$(8)=str$(ficamaxw)
00590   let fnlbl(9,1,"Medicare Rate:",mylen,right,0,fram1)
00600   let fntxt(9,mypos,10,0,left,"33",0,"Format would be 1.450",fram1) !:
        let resp$(9)=str$(mcr)
00610   let fnlbl(10,1,"Medicare Maximum Wage:",mylen,right,0,fram1)
00620   let fntxt(10,mypos,12,0,left,"10",0,"Use 999999.99 since there no maximum wage at this time.",fram1) !:
        let resp$(10)=str$(mcm)
00630   let fram2=2: let fnfra(13,1,8,90,"General Ledger Information")
00640   let fnchk(1,30,"General Ledger Installed:",1,fram2)
00650   if gli=1 then let resp$(11)="True" else let resp$(11)="False"
00660   let fnlbl(2,1,"Cash In Bank:",mylen,right,0,fram2)
00670   let fnqgl(2,32,fram2,2,pas) 
00672   let resp$(12)=fnrgl$(gln$(15))
00680   let fnlbl(3,1,"Federal W/H:",mylen,right,0,fram2)
00690   let fnqgl(3,32,fram2,2,pas) 
00692   let resp$(13)=fnrgl$(gln$(1))
00700   let fnlbl(4,1,"SS & Medicare W/H:",mylen,right,0,fram2)
00710   let fnqgl(4,32,fram2,2,pas) 
00712   let resp$(14)=fnrgl$(gln$(2))
00720   let fnlbl(5,1,"State W/H:",mylen,right,0,fram2)
00730   let fnqgl(5,32,fram2,2,pas) 
00732   let resp$(15)=fnrgl$(gln$(3))
00740   let fnlbl(6,1,"EIC:",mylen,right,0,fram2)
00750   let fnqgl(6,32,fram2,2,pas) 
00752   let resp$(16)=fnrgl$(gln$(14))
00760   let fncmdkey("&Next",1,1,0,"Moves to 2nd screen of company information.")
00770   let fncmdkey("&Save and Exit",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
00780   let fncmdkey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
00790   let fnacs(sn$,0,mat resp$,ckey)
00800   if ckey=5 then goto CONFIRMEXIT
00810   let a$(1)=resp$(1)
00820   let a$(2)=resp$(2)
00830   let a$(3)=resp$(3)
00840   let fid$=resp$(4)
00850   let feducrat=val(resp$(5))
00860   let feducmax=val(resp$(6))
00870   let ficarate=val(resp$(7))
00880   let ficamaxw=val(resp$(8))
00890   let mcr=val(resp$(9))
00900   let mcm=val(resp$(10))
00910   if resp$(11)="True" then let gli=1 else let gli=0
00920   let gln$(15)=fnagl$(resp$(12)) ! bank
00930   let gln$(1)=fnagl$(resp$(13)) ! fed
00940   let gln$(2)=fnagl$(resp$(14)) ! fica
00950   let gln$(3)=fnagl$(resp$(15)) ! state
00960   let gln$(14)=fnagl$(resp$(16)) ! eic
00970   if mcr<=0 then let mcr=1.45
00980   if mcm<=0 then let mcm=999999
00990   if feducrat>5 then goto L1000 else goto L1010
01000 L1000: mat ml$(2) !:
        let ml$(1)="The Federal Unemployment Rate appears to be wrong!" !:
        let ml$(2)="Do you wish to continue anyway?" !:
        let fnmsgbox(mat ml$,resp$,cap$,52) !:
        if resp$<>"Yes" then goto SCREEN_1
01010 L1010: goto L1030 ! If FEDUCMAX<=0 Then Goto 1020 Else Goto 1120
01020   mat ml$(2) !:
        let ml$(1)="The Federal U/C Maximum wage appears to be wrong!" !:
        let ml$(2)="Do you wish to continue anyway?" !:
        let fnmsgbox(mat ml$,resp$,cap$,52) !:
        if resp$="Yes" then goto SCREEN_2 else goto SCREEN_1
01030 L1030: if ckey=4 then goto DONE
01040 ! ______________________________________________________________________
01050 SCREEN_2: ! 
01052   let fntos(sn$="Company-2")
01054   let mylen=32: let mypos=mylen+3 : let right=1
01055   let pos_col(1)=1 ! deduction numbers
01056   let pos_col(2)=5 ! deduction name
01058   let pos_col(3)=23 ! name
01060   let pos_col(4)=33 ! ded/add
01062   let pos_col(5)=43 ! std/pct
01064   let pos_col(6)=57 ! ded fed
01066   let pos_col(7)=62 ! ded fica
01068   let pos_col(8)=67 ! ded state
01070   let pos_col(9)=72 ! ded u/c
01072   let pos_col(10)=75
01074   let fnlbl(1,1,"Enter your deductions names. Mark whether a deduction or addition.",90,left)
01076   let fnlbl(2,1,"A check mark will indicate 'yes' to deduct before calculating Federal w/h, etc.",90,left)
01078   let fnlbl(5,pos_col(2),"Deduction Name")
01080   let fnlbl(5,pos_col(3),"Abbr")
01082   let fnlbl(5,pos_col(3),"Name")
01084   let fnlbl(5,pos_col(4),"Ded/Add")
01086   let fnlbl(5,pos_col(5),"Std/Pct")
01088   let fnlbl(4,pos_col(6)-2,"Ded")
01090   let fnlbl(5,pos_col(6)-2,"Fed")
01092   let fnlbl(4,pos_col(7)-2,"Ded")
01094   let fnlbl(5,pos_col(7)-2,"FICA")
01096   let fnlbl(4,pos_col(8)-2,"Ded")
01098   let fnlbl(5,pos_col(8)-2,"State")
01100   let fnlbl(4,pos_col(9)-2,"Ded")
01102   let fnlbl(5,pos_col(9)-2,"U/C")
01115   let fnlbl(5,pos_col(10),"GL Number")
01120   let resp=0
01125   for j=1 to 20
01130     let fnlbl(j+5,pos_col(1),str$(j)&'.',3,1)
01131     let fntxt(j+5,pos_col(2),15,20,left,"",0,"Enter your deduction name.",0 )
01132     let resp$(resp+=1)=fullname$(j)
01140     let fntxt(j+5,pos_col(3),8,0,left,"",0,"Enter an abbreviated name that will be used in report headings.",0 )
01142     let resp$(resp+=1)=abrevname$(j)
01160     let fncomboa("MiscDeduct",j+5,pos_col(4),mat opt_ded_or_add$,"Indicate whether the amont should be deducted from the check or added to the check.",6)
01170     if newdedcode(j)=0 then let newdedcode(j)=1
01180     let resp$(resp+=1)=opt_ded_or_add$(newdedcode(j))
01200     let fncomboa("std_or_percent",j+5,pos_col(5),mat opt_std_or_percent$,"Standard would a fixed amount each pay period.  Percent would indicate the deduction is a percent of gross pay.",8)
01210     if newcalcode(j)=0 then let newcalcode(j)=1 ! stop subscript error
01220     let resp$(resp+=1)=opt_std_or_percent$(newcalcode(j))
01230     let fnchk(j+5,pos_col(6),"",1)
01240     if newdedfed(j)>0 then let resp$(resp+=1)="True" else let resp$(resp+=1)="False"
01250     let fnchk(j+5,pos_col(7),"",1)
01260     if dedfica(j)>0 then let resp$(resp+=1)="True" else let resp$(resp+=1)="False"
01270     let fnchk(j+5,pos_col(8),"",1)
01280     if dedst(j)>0 then let resp$(resp+=1)="True" else let resp$(resp+=1)="False"
01290     let fnchk(j+5,pos_col(9),"",1)
01300     if deduc(j)>0 then let resp$(resp+=1)="True" else let resp$(resp+=1)="False"
01310     let linecount=j+5
01320     let fnqgl(linecount,pos_col(10),0,2,pas)
01323     let resp$(resp+=1)=fnrgl$(gl$(j))
01330   next j
01340   let fncmdkey("&Next",1,1,0,"Moves to next screen of company information.")
01350   let fncmdkey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
01360   let fncmdkey("&Back",2,0,0,"Returns to previous screen.")
01370   let fncmdkey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
01380   let fnacs(sn$,0,mat resp$,ckey)
01390   if ckey=5 then goto CONFIRMEXIT
01400   let resp=0
01410   for j=1 to 20
01420     let fullname$(j)=resp$(resp+=1)
01430     let abrevname$(j)=resp$(resp+=1)
01440     if resp$(resp+=1)=opt_ded_or_add$(1) then let newdedcode(j)=1 ! deduction
01450     if resp$(resp)=opt_ded_or_add$(2) then let newdedcode(j)=2 ! addition
01460     if resp$(resp)=opt_ded_or_add$(3) then let newdedcode(j)=3 ! benefit
01470     if resp$(resp+=1)=opt_std_or_percent$(1) then let newcalcode(j)=1
01480     if resp$(resp)=opt_std_or_percent$(2) then let newcalcode(j)=2 ! percent method of calucalating
01490     if resp$(resp+=1)="True" then let newdedfed(j)=1 else let newdedfed(j)=0
01500     if resp$(resp+=1)="True" then let dedfica(j)=1 else let dedfica(j)=0
01510     if resp$(resp+=1)="True" then let dedst(j)=1 else let dedst(j)=0
01520     if resp$(resp+=1)="True" then let deduc(j)=1 else let deduc(j)=0
01530     let gl$(j)=fnagl$(resp$(resp+=1))
01540   next j
01550   if ckey=2 then goto SCREEN_1
01560   if ckey=4 then goto DONE
01570   if ckey=5 then goto CONFIRMEXIT
01580   if ckey=1 then goto SCREEN_3
01590 ! ______________________________________________________________________
01600 SCREEN_3: ! 
01610   let fntos(sn$="Company-3") !:
        let mylen=32: let mypos=mylen+3 : let right=1
01620   let fnlbl(1,10,"STATE CODES AND UNEMPLOYMENT INFORMATION",0,0)
01630   let fnlbl(3,1,"Code State Name     State ID    U/C Maximum      U/C Rate",0,0)
01640   let resp=0
01650 ! 
01660   for j=1 to 10
01670     let fnlbl(j+3,3,str$(j),mylen,0,0)
01680     let fntxt(j+3,6,8,0,left,"",0,"Enter your state name.",0 ) !:
          let resp$(resp+=1)=d$(j)
01690     let fntxt(j+3,19,12,0,left,"",0,"Enter the state id #.",0 ) !:
          let resp$(resp+=1)=e$(j)
01700     let fntxt(j+3,32,12,0,left,"10",0,"Enter the maximum wage subject to state unemployment (See your state u/c report.",0 ) !:
          let resp$(resp+=1)=str$(m(j))
01710     let fntxt(j+3,49,8,0,left,"33",0,"Enter the state unemployment rate (See your state u/c report. Enter in percent format. Example: 5% as 5.00",0 ) !:
          let resp$(resp+=1)=str$(r(j))
01720   next j
01730   let fncmdkey("&Next",1,1,0,"Moves to next screen of company information.")
01740   let fncmdkey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
01750   let fncmdkey("&Back",2,0,0,"Returns to previous screen.")
01760   let fncmdkey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
01770   let fnacs(sn$,0,mat resp$,ckey)
01780   if ckey=5 then goto CONFIRMEXIT
01790   let resp=0
01800   for j=1 to 10
01810     let d$(j)=resp$(resp+=1)
01820     let e$(j)=resp$(resp+=1)
01830     let m(j)=val(resp$(resp+=1))
01840     let r(j)=val(resp$(resp+=1))
01850   next j
01860   if ckey=2 then goto SCREEN_2
01870   if ckey=4 then goto DONE
01880   if ckey=5 then goto CONFIRMEXIT
01890 ! ______________________________________________________________________
01900 SCREEN_4: ! 
01910   let fntos(sn$="Company-4") !:
        let mylen=45: let mypos=mylen+3 : let right=1: let resp=0
01920   let fram3=1: let fnfra(1,1,6,60,"Vacation and Sick Pay Information")
01930   let fnlbl(1,1,"Days employed before accruing sick hours",mylen,right,0,fram3)
01940   let fntxt(1,mypos,8,0,0,"30",0,"",fram3) !:
        let resp$(1)=str$(sck(1))
01950   let fnlbl(2,1,"Sick hours accrued after eligibility period:",mylen,right,0,fram3)
01960   let fntxt(2,mypos,8,0,0,"33",0,"",fram3) !:
        let resp$(2)=str$(sck(2))
01970   let fnlbl(3,1,"Sick hours to accrue once eligible:",mylen,right,0,fram3)
01980   let fntxt(3,mypos,8,0,0,"33",0,"",fram3) !:
        let resp$(3)=str$(sck(3))
01990   let fnlbl(4,1,"Maximum Sick Hours:",mylen,right,0,fram3)
02000   let fntxt(4,mypos,8,0,0,"32",0,"",fram3) !:
        let resp$(4)=str$(sck(4))
02010   let fnlbl(5,1,"Maximum Vacation Hours:",mylen,right,0,fram3)
02020   let fntxt(5,mypos,8,0,0,"32",0,"",fram3) !:
        let resp$(5)=str$(vacm)
02030   let fram4=2: let fnfra(9,1,3,60,"Miscellaneous Payroll Information")
02040   let fnlbl(1,1,"Minimum Hourly Wage:",mylen,right,0,fram4)
02050   let fntxt(1,mypos,10,0,0,"10",0,"",fram4) !:
        let resp$(6)=str$(mhw)
02060   let fnlbl(2,1,"Local Withholding Code:",mylen,right,0,fram4)
02070   let fntxt(2,mypos,2,0,0,"30",0,"If one the twenty miscellaneous dedutions is used for local withholdings, then enter the number of the deduction.",fram4) !:
        let resp$(7)=str$(loccode)
02080   let fram5=3: let fnfra(14,1,5,60,"Workman's Compensation Limits")
02090   let fnlbl(1,1,"Monthly:",mylen,right,0,fram5)
02100   let fntxt(1,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        let resp$(8)=str$(wcm(1))
02110   let fnlbl(2,1,"Semi-Monthly:",mylen,right,0,fram5)
02120   let fntxt(2,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        let resp$(9)=str$(wcm(2))
02130   let fnlbl(3,1,"Bi-Weekly:",mylen,right,0,fram5)
02140   let fntxt(3,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        let resp$(10)=str$(wcm(3))
02150   let fnlbl(4,1,"Weekly:",mylen,right,0,fram5)
02160   let fntxt(4,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        let resp$(11)=str$(wcm(4))
02170   let fncmdkey("&Next",1,1,0,"Moves to next screen of company information.")
02180   let fncmdkey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
02190   let fncmdkey("&Back",2,0,0,"Returns to previous screen.")
02200   let fncmdkey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
02210   let fnacs(sn$,0,mat resp$,ckey)
02220   if ckey=5 then goto CONFIRMEXIT
02230   let sck(1)=val(resp$(1))
02240   let sck(2)=val(resp$(2))
02250   let sck(3)=val(resp$(3))
02260   let sck(4)=val(resp$(4))
02270   let vacm=val(resp$(5))
02280   let mhw=val(resp$(6))
02290   let loccode=val(resp$(7))
02300   let wcm(1)=val(resp$(8))
02310   let wcm(2)=val(resp$(9))
02320   let wcm(3)=val(resp$(10))
02330   let wcm(4)=val(resp$(11))
02340   if ckey=2 then goto SCREEN_3
02350   if ckey=4 then goto DONE
02360   if ckey=1 then goto SCREEN_5
02370 ! ______________________________________________________________________
02380 SCREEN_5: ! 
02390   goto DONE ! will need the job cost screen someday
02400   if ckey=2 then goto SCREEN_4
02410   if ckey=5 then goto DONE
02420 ! ______________________________________________________________________
02430 SCREEN_6: ! 
02440   close #106: ioerr L2450
02450 L2450: if fnstyp=14 then let win6$="SRow=14,SCol=12,ECol=67"
02460   if fnstyp<>14 then let win6$="SRow=09,SCol=11,ECol=69"
02470   open #106: win6$&",ERow=22,Border=SR,Caption=<"&cap$,display,outin 
02480   print #106: newpage
02490   print #106,fields "2,2,Cr 44,N": "ACS General Ledger Installed (Y/N):"
02500   print #106,fields "3,2,Cr 44,N": "Days employed before accruing Sick Hours:"
02510   print #106,fields "4,2,Cr 44,N": "Sick Hours accrued after eligibility period:"
02520   print #106,fields "5,2,Cr 44,N": "Sick Hours to accrue each time accumulated:"
02530   print #106,fields "6,2,Cr 44,N": "Maximum Sick Hours:"
02540   print #106,fields "7,2,Cr 44,N": "Maximum Vacation Hours:"
02550   print #106,fields "8,2,Cr 44,N": "Minimum Hourly Wage:"
02560   let io6$(1)="2,47,Cu 1,UT,N"
02570   let io6$(2)="3,47,N 8.3,UT,N"
02580   let io6$(3)="4,47,N 8.3,UT,N"
02590   let io6$(4)="5,47,N 8.3,UT,N"
02600   let io6$(5)="6,47,N 6.2,UT,N"
02610   let io6$(6)="7,47,N 6.2,UT,N"
02620   let io6$(7)="8,47,N 6.2,UT,N"
02630   if fnstyp=11 then goto L2640 else goto L2700 ! (skips Job Cost Questions)
02640 L2640: print #106,fields "10,2,Cr 50,N": "Retain Transactions until Jobs are complete (Y/N):"
02650   print #106,fields "11,2,Cr 50,N": "Starting of Range for Non-Productive Jobs:"
02660   print #106,fields "12,2,Cr 50,N": "Ending of Range for Non-Productive Jobs:"
02670   print #106,fields "13,2,Cr 50,N": "Number of Deduction used for Union Dues (if any):"
02680   let io6$(8)="10,53,Cu 1,UT,N" : let io6$(9)="11,53,C 6,UT,N" !:
        let io6$(10)="12,53,C 6,UT,N" : let io6$(11)="13,53,Nz 2,UT,N"
02690   if tc=1 then let tc$="Y" else let tc$="N"
02700 L2700: print fields "23,25,C 09,B,1": "Next (F1)"
02710   print fields "23,35,C 09,B,2": "Back (F2)"
02720   print fields "23,45,C 09,B,5": "Done (F5)"
02730   if gli=1 then let gli$="Y" else let gli$="N"
02740 L2740: if fnstyp=11 then !:
          rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw,tc$,mat jn$,dc conv CONV7
02750   if fnstyp<>11 then !:
          rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw conv CONV7
02760   if ce>0 then let io6$(ce)(ce1:ce2)="U": let ce=0
02770   if ckey>0 then goto L2840 else let ce=curfld+1
02780   if ce>udim(io6$) then let ce=1
02790 L2790: let io6$(ce)=rtrm$(io6$(ce)) : let ce1=pos(io6$(ce),"U",1)
02800   let ce2=ce1+1 : let io6$(ce)(ce1:ce1)="UC" : goto L2740
02810 CONV7: if ce>0 then let io6$(ce)(ce1:ce2)="U"
02820   let ce=cnt+1
02830 ERR7: print fields "24,78,C 1": bell : goto L2790
02840 L2840: if gli$<>"Y" and gli$<>"N" then let ce=1 : goto ERR7
02850   if gli$="Y" then let gli=1 else let gli=0
02860   if fnstyp=14 then goto L2900
02870   if tc$="Y" then let tc=1 else let tc=0
02880   if fnstyp=11 and tc$<>"Y" and tc$<>"N" then let ce=8 : goto ERR7
02890   if dc<0 or dc>10 then let ce=11: goto ERR7
02900 L2900: if ckey=5 or ckey=1 then goto CONFIRMEXIT
02910   if ckey=2 then goto SCREEN_5
02920   goto L2740
02930 CONFIRMEXIT: ! 
02940   mat ml$(2) !:
        let ml$(1)="You have chosen to exit without saving any changes." !:
        let ml$(2)="Save changes now?" !:
        let fnmsgbox(mat ml$,resp$,cap$,52) !:
        if resp$="Yes" then goto DONE else goto XIT
02950 ! ______________________________________________________________________
02960 DONE: ! 
02970   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&str$(cno),internal,outin,relative 
02980   let ficamaxw=ficamaxw*.1
02990   rewrite #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
03000   close #1: 
03010   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$,1)
03040   goto XIT
03050 ! ______________________________________________________________________
03060 XIT: let fnxit
03070 ! ______________________________________________________________________
03080 ! <Updateable Region: ERTN>
03090 ERTN: let fnerror(program$,err,line,act$,"xit")
03100   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03110   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03120   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
03130 ERTN_EXEC_ACT: execute act$ : goto ERTN
03140 ! /region
03150 ! ______________________________________________________________________
