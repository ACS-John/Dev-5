00020 ! Company Information File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnstyp,fnTos,fnLbl,fnTxt,fnChk,fnqgl,fnrgl$,fncomboa,fnCmdKey,fnAcs,fnagl$,fnmsgbox,fnFra,fnDedNames,fnclient_has
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   if fnclient_has('P2') then let fnstyp(11) else let fnstyp(14) !  styp=11 for jobcost; styp=14 for regular payroll
00080   fntop(program$,cap$="Company Information")
00090   fncno(cno)
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
00221   opt_ded_or_add$(1)="Deduct"
00222   opt_ded_or_add$(2)="Add"
00223   opt_ded_or_add$(3)="Benefit"
00224 ! 
00225   dim opt_std_or_percent$(2)*8
00226   opt_std_or_percent$(1)="Standard"
00227   opt_std_or_percent$(2)="Percent"
00230 ! ______________________________________________________________________
00240 ! ______________________________________________________________________
00250   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&',recl=759,version=0,use',internal,outIn,relative 
00260   if lrec(1)=0 then write #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2': mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
00270   read #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc ioerr L290
00280   ficamaxw=ficamaxw*10
00290 L290: close #1: 
00300 READNAMES: ! 
00310   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$)
00380 ! ______________________________________________________________________
00390 SCREEN_1: ! 
00395   resp=0
00400   fnTos(sn$="Company-1") 
00405   mylen=30: mypos=mylen+3 : right=1
00410   fram1=1: fnFra(1,1,10,80,"Company # "&env$('cno'))
00415   fnLbl(1,1,"Company Name:",mylen,right,0,fram1)
00420   fnTxt(1,mypos,40,0,left,"",0,"",fram1) 
00425   resp$(1)=a$(1)
00430   fnLbl(2,1,"Company Address:",mylen,right,0,fram1)
00435   fnTxt(2,mypos,40,0,left,"",0,"",fram1) 
00440   resp$(2)=a$(2)
00445   fnLbl(3,1,"City, State, Zip:",mylen,right,0,fram1)
00450   fnTxt(3,mypos,40,0,left,"",0,"",fram1) 
00455   resp$(3)=a$(3)
00460   fnLbl(4,1,"Federal ID #:",mylen,right,0,fram1)
00465   fnTxt(4,mypos,12,0,left,"",0,"",fram1) 
00470   resp$(4)=fid$
00475   fnLbl(5,1,"Federal U/C Rate:",mylen,right,0,fram1)
00480   fnTxt(5,mypos,10,0,left,"33",0,"In 2007 the rate was .8% and should be entered as .8 and not .008",fram1) 
00485   resp$(5)=str$(feducrat)
00490   fnLbl(6,1,"Federal U/C Maximum Wage:",mylen,right,0,fram1)
00495   fnTxt(6,mypos,12,0,left,"10",0,"",fram1) 
00500   resp$(6)=str$(feducmax)
00505   fnLbl(7,1,"Social Security Rate:",mylen,right,0,fram1)
00510   fnTxt(7,mypos,10,0,left,"33",0,"Sample format 6.2",fram1 ) 
00515   resp$(7)=str$(ficarate)
00520   fnLbl(8,1,"SS Maximum Wage:",mylen,right,0,fram1)
00525   fnTxt(8,mypos,12,0,left,"10",0,"The maximum was 97500 for the year 2007.  See a 941 form.",fram1)
00530   resp$(8)=str$(ficamaxw)
00535   fnLbl(9,1,"Medicare Rate:",mylen,right,0,fram1)
00540   fnTxt(9,mypos,10,0,left,"33",0,"Format would be 1.450",fram1) 
00545   resp$(9)=str$(mcr)
00550   fnLbl(10,1,"Medicare Maximum Wage:",mylen,right,0,fram1)
00555   fnTxt(10,mypos,12,0,left,"10",0,"Use 999999.99 since there no maximum wage at this time.",fram1) 
00560   resp$(10)=str$(mcm)
00630   fram2=2: fnFra(13,1,8,90,"General Ledger Information")
00640   fnChk(1,30,"General Ledger Installed:",1,fram2)
00650   if gli=1 then resp$(11)="True" else resp$(11)="False"
00660   fnLbl(2,1,"Cash In Bank:",mylen,right,0,fram2)
00670   fnqgl(2,32,fram2,2,pas) 
00672   resp$(12)=fnrgl$(gln$(15))
00680   fnLbl(3,1,"Federal W/H:",mylen,right,0,fram2)
00690   fnqgl(3,32,fram2,2,pas) 
00692   resp$(13)=fnrgl$(gln$(1))
00700   fnLbl(4,1,"SS & Medicare W/H:",mylen,right,0,fram2)
00710   fnqgl(4,32,fram2,2,pas) 
00712   resp$(14)=fnrgl$(gln$(2))
00720   fnLbl(5,1,"State W/H:",mylen,right,0,fram2)
00730   fnqgl(5,32,fram2,2,pas) 
00732   resp$(15)=fnrgl$(gln$(3))
00740   fnLbl(6,1,"EIC:",mylen,right,0,fram2)
00750   fnqgl(6,32,fram2,2,pas) 
00752   resp$(16)=fnrgl$(gln$(14))
00760   fnCmdKey("&Next",1,1,0,"Moves to 2nd screen of company information.")
00770   fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu without reviewing remainter of screens.")
00780   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
00790   fnAcs(sn$,0,mat resp$,ckey)
00800   if ckey=5 then goto CONFIRMEXIT
00810   a$(1)=resp$(1)
00820   a$(2)=resp$(2)
00830   a$(3)=resp$(3)
00840   fid$=resp$(4)
00850   feducrat=val(resp$(5))
00860   feducmax=val(resp$(6))
00870   ficarate=val(resp$(7))
00880   ficamaxw=val(resp$(8)) ! pr ficamaxw : pause
00890   mcr=val(resp$(9))
00900   mcm=val(resp$(10))
00910   if resp$(11)="True" then gli=1 else gli=0
00920   gln$(15)=fnagl$(resp$(12)) ! bank
00930   gln$(1)=fnagl$(resp$(13)) ! fed
00940   gln$(2)=fnagl$(resp$(14)) ! fica
00950   gln$(3)=fnagl$(resp$(15)) ! state
00960   gln$(14)=fnagl$(resp$(16)) ! eic
00970   if mcr<=0 then mcr=1.45
00980   if mcm<=0 then mcm=999999
00990   if feducrat>5 then goto L1000 else goto L1010
01000 L1000: mat ml$(2) !:
        ml$(1)="The Federal Unemployment Rate appears to be wrong!" !:
        ml$(2)="Do you wish to continue anyway?" !:
        fnmsgbox(mat ml$,resp$,cap$,52) !:
        if resp$<>"Yes" then goto SCREEN_1
01010 L1010: goto L1030 ! If FEDUCMAX<=0 Then Goto 1020 Else Goto 1120
01020   mat ml$(2) !:
        ml$(1)="The Federal U/C Maximum wage appears to be wrong!" !:
        ml$(2)="Do you wish to continue anyway?" !:
        fnmsgbox(mat ml$,resp$,cap$,52) !:
        if resp$="Yes" then goto SCREEN_2 else goto SCREEN_1
01030 L1030: if ckey=4 then goto DONE
01040 ! ______________________________________________________________________
01050 SCREEN_2: ! 
01052   fnTos(sn$="Company-2")
01054   mylen=32: mypos=mylen+3 : right=1
01055   pos_col(1)=1 ! deduction numbers
01056   pos_col(2)=5 ! deduction name
01058   pos_col(3)=23 ! name
01060   pos_col(4)=33 ! ded/add
01062   pos_col(5)=43 ! std/pct
01064   pos_col(6)=57 ! ded fed
01066   pos_col(7)=62 ! ded fica
01068   pos_col(8)=67 ! ded state
01070   pos_col(9)=72 ! ded u/c
01072   pos_col(10)=75
01074   fnLbl(1,1,"Enter your deductions names. Mark whether a deduction or addition.",90,left)
01076   fnLbl(2,1,"A check mark will indicate 'yes' to deduct before calculating Federal w/h, etc.",90,left)
01078   fnLbl(5,pos_col(2),"Deduction Name")
01080   fnLbl(5,pos_col(3),"Abbr")
01082   fnLbl(5,pos_col(3),"Name")
01084   fnLbl(5,pos_col(4),"Ded/Add")
01086   fnLbl(5,pos_col(5),"Std/Pct")
01088   fnLbl(4,pos_col(6)-2,"Ded")
01090   fnLbl(5,pos_col(6)-2,"Fed")
01092   fnLbl(4,pos_col(7)-2,"Ded")
01094   fnLbl(5,pos_col(7)-2,"FICA")
01096   fnLbl(4,pos_col(8)-2,"Ded")
01098   fnLbl(5,pos_col(8)-2,"State")
01100   fnLbl(4,pos_col(9)-2,"Ded")
01102   fnLbl(5,pos_col(9)-2,"U/C")
01115   fnLbl(5,pos_col(10),"GL Number")
01120   resp=0
01125   for j=1 to 20
01130     fnLbl(j+5,pos_col(1),str$(j)&'.',3,1)
01131     fnTxt(j+5,pos_col(2),15,20,left,"",0,"Enter your deduction name.",0 )
01132     resp$(resp+=1)=fullname$(j)
01140     fnTxt(j+5,pos_col(3),8,0,left,"",0,"Enter an abbreviated name that will be used in report headings.",0 )
01142     resp$(resp+=1)=abrevname$(j)
01160     fncomboa("MiscDeduct",j+5,pos_col(4),mat opt_ded_or_add$,"Indicate whether the amont should be deducted from the check or added to the check.",6)
01170     if newdedcode(j)=0 then newdedcode(j)=1
01180     resp$(resp+=1)=opt_ded_or_add$(newdedcode(j))
01200     fncomboa("std_or_percent",j+5,pos_col(5),mat opt_std_or_percent$,"Standard would a fixed amount each pay period.  Percent would indicate the deduction is a percent of gross pay.",8)
01210     if newcalcode(j)=0 then newcalcode(j)=1 ! stop subscript error
01220     resp$(resp+=1)=opt_std_or_percent$(newcalcode(j))
01230     fnChk(j+5,pos_col(6),"",1)
01240     if newdedfed(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01250     fnChk(j+5,pos_col(7),"",1)
01260     if dedfica(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01270     fnChk(j+5,pos_col(8),"",1)
01280     if dedst(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01290     fnChk(j+5,pos_col(9),"",1)
01300     if deduc(j)>0 then resp$(resp+=1)="True" else resp$(resp+=1)="False"
01310     linecount=j+5
01320     fnqgl(linecount,pos_col(10),0,2,pas)
01323     resp$(resp+=1)=fnrgl$(gl$(j))
01330   next j
01340   fnCmdKey("&Next",1,1,0,"Moves to next screen of company information.")
01350   fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
01360   fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
01370   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
01380   fnAcs(sn$,0,mat resp$,ckey)
01390   if ckey=5 then goto CONFIRMEXIT
01400   resp=0
01410   for j=1 to 20
01420     fullname$(j)=resp$(resp+=1)
01430     abrevname$(j)=resp$(resp+=1)
01440     if resp$(resp+=1)=opt_ded_or_add$(1) then newdedcode(j)=1 ! deduction
01450     if resp$(resp)=opt_ded_or_add$(2) then newdedcode(j)=2 ! addition
01460     if resp$(resp)=opt_ded_or_add$(3) then newdedcode(j)=3 ! benefit
01470     if resp$(resp+=1)=opt_std_or_percent$(1) then newcalcode(j)=1
01480     if resp$(resp)=opt_std_or_percent$(2) then newcalcode(j)=2 ! percent method of calucalating
01490     if resp$(resp+=1)="True" then newdedfed(j)=1 else newdedfed(j)=0
01500     if resp$(resp+=1)="True" then dedfica(j)=1 else dedfica(j)=0
01510     if resp$(resp+=1)="True" then dedst(j)=1 else dedst(j)=0
01520     if resp$(resp+=1)="True" then deduc(j)=1 else deduc(j)=0
01530     gl$(j)=fnagl$(resp$(resp+=1))
01540   next j
01550   if ckey=2 then goto SCREEN_1
01560   if ckey=4 then goto DONE
01570   if ckey=5 then goto CONFIRMEXIT
01580   if ckey=1 then goto SCREEN_3
01590 ! ______________________________________________________________________
01600 SCREEN_3: ! 
01610   fnTos(sn$="Company-3") !:
        mylen=32: mypos=mylen+3 : right=1
01620   fnLbl(1,10,"STATE CODES AND UNEMPLOYMENT INFORMATION",0,0)
01630   fnLbl(3,1,"Code State Name     State ID    U/C Maximum      U/C Rate",0,0)
01640   resp=0
01650 ! 
01660   for j=1 to 10
01670     fnLbl(j+3,3,str$(j),mylen,0,0)
01680     fnTxt(j+3,6,8,0,left,"",0,"Enter your state name.",0 ) !:
          resp$(resp+=1)=d$(j)
01690     fnTxt(j+3,19,12,0,left,"",0,"Enter the state id #.",0 ) !:
          resp$(resp+=1)=e$(j)
01700     fnTxt(j+3,32,12,0,left,"10",0,"Enter the maximum wage subject to state unemployment (See your state u/c report.",0 ) !:
          resp$(resp+=1)=str$(m(j))
01710     fnTxt(j+3,49,8,0,left,"33",0,"Enter the state unemployment rate (See your state u/c report. Enter in percent format. Example: 5% as 5.00",0 ) !:
          resp$(resp+=1)=str$(r(j))
01720   next j
01730   fnCmdKey("&Next",1,1,0,"Moves to next screen of company information.")
01740   fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
01750   fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
01760   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
01770   fnAcs(sn$,0,mat resp$,ckey)
01780   if ckey=5 then goto CONFIRMEXIT
01790   resp=0
01800   for j=1 to 10
01810     d$(j)=resp$(resp+=1)
01820     e$(j)=resp$(resp+=1)
01830     m(j)=val(resp$(resp+=1))
01840     r(j)=val(resp$(resp+=1))
01850   next j
01860   if ckey=2 then goto SCREEN_2
01870   if ckey=4 then goto DONE
01880   if ckey=5 then goto CONFIRMEXIT
01890 ! ______________________________________________________________________
01900 SCREEN_4: ! 
01910   fnTos(sn$="Company-4") !:
        mylen=45: mypos=mylen+3 : right=1: resp=0
01920   fram3=1: fnFra(1,1,6,60,"Vacation and Sick Pay Information")
01930   fnLbl(1,1,"Days employed before accruing sick hours",mylen,right,0,fram3)
01940   fnTxt(1,mypos,8,0,0,"30",0,"",fram3) !:
        resp$(1)=str$(sck(1))
01950   fnLbl(2,1,"Sick hours accrued after eligibility period:",mylen,right,0,fram3)
01960   fnTxt(2,mypos,8,0,0,"33",0,"",fram3) !:
        resp$(2)=str$(sck(2))
01970   fnLbl(3,1,"Sick hours to accrue once eligible:",mylen,right,0,fram3)
01980   fnTxt(3,mypos,8,0,0,"33",0,"",fram3) !:
        resp$(3)=str$(sck(3))
01990   fnLbl(4,1,"Maximum Sick Hours:",mylen,right,0,fram3)
02000   fnTxt(4,mypos,8,0,0,"32",0,"",fram3) !:
        resp$(4)=str$(sck(4))
02010   fnLbl(5,1,"Maximum Vacation Hours:",mylen,right,0,fram3)
02020   fnTxt(5,mypos,8,0,0,"32",0,"",fram3) !:
        resp$(5)=str$(vacm)
02030   fram4=2: fnFra(9,1,3,60,"Miscellaneous Payroll Information")
02040   fnLbl(1,1,"Minimum Hourly Wage:",mylen,right,0,fram4)
02050   fnTxt(1,mypos,10,0,0,"10",0,"",fram4) !:
        resp$(6)=str$(mhw)
02060   fnLbl(2,1,"Local Withholding Code:",mylen,right,0,fram4)
02070   fnTxt(2,mypos,2,0,0,"30",0,"If one the twenty miscellaneous dedutions is used for local withholdings, then enter the number of the deduction.",fram4) !:
        resp$(7)=str$(loccode)
02080   fram5=3: fnFra(14,1,5,60,"Workman's Compensation Limits")
02090   fnLbl(1,1,"Monthly:",mylen,right,0,fram5)
02100   fnTxt(1,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        resp$(8)=str$(wcm(1))
02110   fnLbl(2,1,"Semi-Monthly:",mylen,right,0,fram5)
02120   fnTxt(2,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        resp$(9)=str$(wcm(2))
02130   fnLbl(3,1,"Bi-Weekly:",mylen,right,0,fram5)
02140   fnTxt(3,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        resp$(10)=str$(wcm(3))
02150   fnLbl(4,1,"Weekly:",mylen,right,0,fram5)
02160   fnTxt(4,mypos,10,0,0,"10",0,"If you pay workman's comp and there are limits on the maximum wage subject to workman's comp, then enter that maximum wage under the proper pay period.",fram5) !:
        resp$(11)=str$(wcm(4))
02170   fnCmdKey("&Next",1,1,0,"Moves to next screen of company information.")
02180   fnCmdKey("&Save and Exit",4,0,0,"Saves any changes and returns to menu.")
02190   fnCmdKey("&Back",2,0,0,"Returns to previous screen.")
02200   fnCmdKey("&Cancel",5,0,1,"Returns to menu without saving any changes on any screen.")
02210   fnAcs(sn$,0,mat resp$,ckey)
02220   if ckey=5 then goto CONFIRMEXIT
02230   sck(1)=val(resp$(1))
02240   sck(2)=val(resp$(2))
02250   sck(3)=val(resp$(3))
02260   sck(4)=val(resp$(4))
02270   vacm=val(resp$(5))
02280   mhw=val(resp$(6))
02290   loccode=val(resp$(7))
02300   wcm(1)=val(resp$(8))
02310   wcm(2)=val(resp$(9))
02320   wcm(3)=val(resp$(10))
02330   wcm(4)=val(resp$(11))
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
02450 L2450: if fnstyp=14 then win6$="SRow=14,SCol=12,ECol=67"
02460   if fnstyp<>14 then win6$="SRow=09,SCol=11,ECol=69"
02470   open #106: win6$&",ERow=22,Border=SR,Caption=<"&cap$,display,outIn 
02480   pr #106: newpage
02490   pr #106,fields "2,2,Cr 44,N": "ACS General Ledger Installed (Y/N):"
02500   pr #106,fields "3,2,Cr 44,N": "Days employed before accruing Sick Hours:"
02510   pr #106,fields "4,2,Cr 44,N": "Sick Hours accrued after eligibility period:"
02520   pr #106,fields "5,2,Cr 44,N": "Sick Hours to accrue each time accumulated:"
02530   pr #106,fields "6,2,Cr 44,N": "Maximum Sick Hours:"
02540   pr #106,fields "7,2,Cr 44,N": "Maximum Vacation Hours:"
02550   pr #106,fields "8,2,Cr 44,N": "Minimum Hourly Wage:"
02560   io6$(1)="2,47,Cu 1,UT,N"
02570   io6$(2)="3,47,N 8.3,UT,N"
02580   io6$(3)="4,47,N 8.3,UT,N"
02590   io6$(4)="5,47,N 8.3,UT,N"
02600   io6$(5)="6,47,N 6.2,UT,N"
02610   io6$(6)="7,47,N 6.2,UT,N"
02620   io6$(7)="8,47,N 6.2,UT,N"
02630   if fnstyp=11 then goto L2640 else goto L2700 ! (skips Job Cost Questions)
02640 L2640: pr #106,fields "10,2,Cr 50,N": "Retain Transactions until Jobs are complete (Y/N):"
02650   pr #106,fields "11,2,Cr 50,N": "Starting of Range for Non-Productive Jobs:"
02660   pr #106,fields "12,2,Cr 50,N": "Ending of Range for Non-Productive Jobs:"
02670   pr #106,fields "13,2,Cr 50,N": "Number of Deduction used for Union Dues (if any):"
02680   io6$(8)="10,53,Cu 1,UT,N" : io6$(9)="11,53,C 6,UT,N" !:
        io6$(10)="12,53,C 6,UT,N" : io6$(11)="13,53,Nz 2,UT,N"
02690   if tc=1 then tc$="Y" else tc$="N"
02700 L2700: pr f "23,25,C 09,B,1": "Next (F1)"
02710   pr f "23,35,C 09,B,2": "Back (F2)"
02720   pr f "23,45,C 09,B,5": "Done (F5)"
02730   if gli=1 then gli$="Y" else gli$="N"
02740 L2740: if fnstyp=11 then !:
          rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw,tc$,mat jn$,dc conv CONV7
02750   if fnstyp<>11 then !:
          rinput #106,fields mat io6$: gli$,mat sck,vacm,mhw conv CONV7
02760   if ce>0 then io6$(ce)(ce1:ce2)="U": ce=0
02770   if ckey>0 then goto L2840 else ce=curfld+1
02780   if ce>udim(io6$) then ce=1
02790 L2790: io6$(ce)=rtrm$(io6$(ce)) : ce1=pos(io6$(ce),"U",1)
02800   ce2=ce1+1 : io6$(ce)(ce1:ce1)="UC" : goto L2740
02810 CONV7: if ce>0 then io6$(ce)(ce1:ce2)="U"
02820   ce=cnt+1
02830 ERR7: pr f "24,78,C 1": bell : goto L2790
02840 L2840: if gli$<>"Y" and gli$<>"N" then ce=1 : goto ERR7
02850   if gli$="Y" then gli=1 else gli=0
02860   if fnstyp=14 then goto L2900
02870   if tc$="Y" then tc=1 else tc=0
02880   if fnstyp=11 and tc$<>"Y" and tc$<>"N" then ce=8 : goto ERR7
02890   if dc<0 or dc>10 then ce=11: goto ERR7
02900 L2900: if ckey=5 or ckey=1 then goto CONFIRMEXIT
02910   if ckey=2 then goto SCREEN_5
02920   goto L2740
02930 CONFIRMEXIT: ! 
02940   mat ml$(2) !:
        ml$(1)="You have chosen to exit without saving any changes." !:
        ml$(2)="Save changes now?" !:
        fnmsgbox(mat ml$,resp$,cap$,52) !:
        if resp$="Yes" then goto DONE else goto XIT
02950 ! ______________________________________________________________________
02960 DONE: ! 
02970   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno'),internal,outIn,relative 
02980   ficamaxw=ficamaxw*.1
02990   rewrite #1,using 'Form POS 1,3*C 40,C 12,PD 6.3,PD 6.2,PD 5.2,10*C 8,N 2,PD 4.2,PD 3.3,12*PD 4.2,10*PD 3.3,25*C 12,31*N 1,10*C 6,3*PD 4.3,3*PD 3.2,4*PD 4.2,N 1,2*C 6,N 2',rec=1: mat a$,fid$,mcr,mcm,feducrat,mat d$,loccode,feducmax,ficarate,ficamaxw,ficawh,mat m,mat r,mat e$,mat gln$,gli,mat dedcode,mat calcode,mat dedfed,mat rpnames2$,mat sck,vacm,mhw,mat wcm,tc,mat jn$,dc
03000   close #1: 
03010   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc,mat gl$,1)
03040   goto XIT
03050 ! ______________________________________________________________________
03060 XIT: fnxit
03070 ! ______________________________________________________________________
03080 ! <Updateable Region: ERTN>
03090 ERTN: fnerror(program$,err,line,act$,"xit")
03100   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
03110   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03120   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03130 ERTN_EXEC_ACT: execute act$ : goto ERTN
03140 ! /region
03150 ! ______________________________________________________________________
