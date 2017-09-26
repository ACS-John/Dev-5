00010 ! REPLACE S:\acsPR\newprElecW2correction
00020 ! never could get it work with an RO record if all zeroes; without !:
        ! RO record i had to put an RE record in front of every RW record and !:
        ! had to follow with an RT record - Right now this program will not !:
        ! create an RO record
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnerror,fnwin3b,fnwait,fnDedNames,fnoldmsgbox,fntop,fnxit,fnconsole,fnacs,fntop,fnlbl,fntxt,fntos,fnfra,fncmdkey,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim em$(3)*30,ss$*11,d(14),tcp(32),s(9),t(9),z$*8,cap$*128,message$*40
00080   dim tdc(10),newdedcode(20),newcalcode(20),newdedfed(20),dedfica(20)
00090   dim dedst(20),deduc(20),abrevname$(20)*8,fullname$(20)*20
00100   dim a$(3)*40,b$*12,g$*12,d$(10)*8,tty(10),e$(10)*12,s2(2)
00110   dim fa$(1),fb$(1),fc$(1),fd$(1),l$(10),dedfed(10),w3(2),i2(2),t2(2)
00120   dim emppin$*17,tlcn$*6,contact$*27,contactph$*15,phoneext$*5,email$*40
00130   dim ml$(2)*80,orgw32$*11,w32$*11
00140   dim w2(9),i1(9),t1(9),ct$*20,st$*2,ibm$*8,namcde$*1,typemp$*1,io1$(18)
00150   dim orgt1(9),orgt2(2)
00160   dim orgw2(9),orgw3(2)
00170   dim terminat$*1,first$*15,mid$*15,last$*20,resp$(20)*40,path$*30
00180 ! ______________________________________________________________________
00190   let fntop("S:\acsPR\prElecW2",cap$="Electronic W-2")
00230   on fkey 5 goto XIT
00250 ! 
00260   open #1: "Name="&env$('Q')&"\PRmstr\Company.h"&env$('cno')&",Shr",internal,input 
00270   read #1,using L280: mat a$,b$,mat d$,loccode,mat e$,mat dedfed,oldmax
00280 L280: form pos 1,3*c 40,c 12,pos 150,10*c 8,n 2,pos 317,10*c 12,pos 638,10*n 1,pos 239,pd 4.2
00290   close #1: 
00300 ! ______________________________________________________________________
00310 DATE_SCREEN: ! 
00320 L320: let fntos(sn$="W2-1") !:
        let rc=cf=0: let mylen=34 : let mypos=mylen+3
00330   let fnfra(1,1,3,60,"Date Range for Corrected W2's","Normally this would the first and last day of the calendar year",0) !:
        let cf+=1 : let fratype=cf
00340   let fnlbl(1,1,"Starting Date:",mylen,1,0,1)
00350   let fntxt(1,mypos,10,0,1,"3",0,"First day of calendar year",1) !:
        let resp$(rc+=1)=str$(beg_date)
00360   let fnlbl(2,1,"Ending Date:",mylen,1,0,1)
00370   let fntxt(2,mypos,10,0,1,"3",0,"Last day of calendar year",1) !:
        let resp$(rc+=1)=str$(end_date)
00380   let fnlbl(3,1,"Output File Designation and Name:",mylen,1,0,1)
00390   let fntxt(3,mypos,30,0,0,"",0,"Destination and file name you wish to use.",1) !:
        let resp$(rc+=1)="c:\w2report"
00400   let fnfra(7,1,3,60,"Date Range used on Original W2's","This could be any date rqnge entered by mistake",0) !:
        let cf+=1 : let fratype=cf
00410   let fnlbl(1,1,"Original Starting Date:",mylen,1,0,2)
00420   let fntxt(1,mypos,10,0,1,"3",0,"First day of calendar year used on original submission of W2s",2) !:
        let resp$(rc+=1)=str$(orgbeg_date)
00430   let fnlbl(2,1,"Original Ending Date:",mylen,1,0,2)
00440   let fntxt(2,mypos,10,0,1,"3",0,"Last day of calendar year used on original submission of W2s",2) !:
        let resp$(rc+=1)=str$(orgend_date)
00450   let fncmdkey("Next",1,1,0,"Prints the report")
00460   let fncmdkey("Cancel",5,0,1,"Returns to menu")
00470   let fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
00480   let beg_date=val(resp$(1))
00490   let end_date=val(resp$(2))
00500   let path$=resp$(3)
00510   let orgbeg_date=val(resp$(4))
00520   let orgend_date=val(resp$(5))
00530   if beg_date=0 or end_date=0 then goto L320
00540   if orgbeg_date=0 or orgend_date=0 then goto L320
00550   let fnconsole(1)
00560 L560: let p1=pos(b$,"-",1)
00570   if p1=0 then goto L600
00580   let b$(p1:p1)=""
00590   goto L560
00600 L600: ! 
00610   let b1=val(b$)
00620   let p1=pos(a$(3),",",1): let comma=1
00630   if p1=0 then let p1=pos(a$(3)," ",1): let comma=0
00640   let ct$=a$(3)(1:p1-1)
00650   if comma=1 then let st$=a$(3)(p1+2:p1+3) else let st$=a$(3)(p1+1:p1+2)
00660   let p2=len(rtrm$(a$(3)))
00670   let p1=p2-4
00680   let zip$=a$(3)(p1:p2)
00690   if val(date$(1:2))-1 <70 then let yr=2000+val(date$(1:2))-1 else let yr=1900+val(date$(1:2))-1
00700   if date$(4:5)="12" then let yr=yr+1 ! if you run in december, add 1 year back
00710   let io1$(1)="5,25,C 40,UT,N"
00720   let io1$(2)="6,25,C 40,UT,N"
00730   let io1$(3)="7,25,C 20,UT,N"
00740   let io1$(4)="8,25,C 2,UT,N"
00750   let io1$(5)="9,25,C 5,UT,N"
00760   let io1$(6)="10,25,N 9,UT,N"
00770   let io1$(7)="11,25,N 4,UT,N"
00780   let io1$(8)="12,25,N 10.2,UT,N"
00790   let io1$(9)="13,25,N 10.4,UT,N"
00800   let io1$(10)="14,25,N 10.2,UT,N"
00810   let io1$(11)="15,25,N 10.4,UT,N"
00820   let io1$(12)="16,75,N 2,UT,N"
00830   let io1$(13)="17,47,N 2,UT,N"
00840   let io1$(14)="18,61,N 2,UT,N"
00850   let io1$(15)="19,65,N 2,UT,N"
00860   let io1$(16)="20,35,C 8,UT,N"
00870   let io1$(17)="21,52,C 1,UT,N"
00880   let io1$(18)="22,38,C 1,UT,N"
00890   let ibm$="IBM"
00900   let namcde$="F"
00910   let typemp$="R"
00920 ! ______________________________________________________________________
00930 SCR1: ! 
00940   goto L1220
00950   let fntos(sn$="ElecW2-2") !:
        let rc=cf=0: let mylen=30 : let mypos=mylen+3
00960   let fnlbl(1,1,"Position Diskette in Drive A",mylen+25,1,0,0)
00970   let fnlbl(3,1,"Company Name:",mylen,1,0,0)
00980   let fntxt(3,mypos,40,0,0,"",0,"Enter the name of the company submitting the files",0) !:
        let resp$(rc+=1)=a$(1)
00990   let fnlbl(4,1,"Street Address:",mylen,1,0,0)
01000   let fntxt(4,mypos,40,0,0,"",0,"Enter the address of the company submitting the files",0) !:
        let resp$(rc+=1)=a$(2)
01010   let fnlbl(5,1,"City:",mylen,1,0,0)
01020   let fntxt(5,mypos,22,0,0,"",0,"Enter the city of the company submitting the files",0) !:
        let resp$(rc+=1)=ct$
01030   let fnlbl(6,1,"State:",mylen,1,0,0)
01040   let fntxt(6,mypos,2,0,0,"",0,"Enter the state forthe company being submitted.",0) !:
        let resp$(rc+=1)=st$
01050   let fnlbl(7,1,"Zip Code:",mylen,1,0,0)
01060   let fntxt(7,mypos,5,0,0,"",0,"Enter the zip code for the company being submitted.",0) !:
        let resp$(rc+=1)=zip$
01070   let fnlbl(8,1,"Federal ID #:",mylen,1,0,0)
01080   let fntxt(8,mypos,9,0,0,"30",0,"Enter the Federal Id number without slashes or dashes.",0) !:
        let resp$(rc+=1)=str$(b1)
01090   let fnlbl(9,1,"Payment Year:",mylen,1,0,0)
01100   let fntxt(9,mypos,4,0,0,"30",0,"Enter the year for which the wages were paid in ccyy format.",0) !:
        let resp$(rc+=1)=str$(yr)
01110   let fnlbl(10,1,"Social Security Maximum Wage:",mylen,1,0,0)
01120   let fntxt(10,mypos,10,0,0,"10",0,"Enter the social security maximum wage for the year just completed.",0) !:
        let resp$(rc+=1)=str$(ssmax)
01130   let fnlbl(11,1,"Social Security Rate:",mylen,1,0,0)
01140   let fntxt(11,mypos,6,0,0,"34",0,"Enter the social security rate for the year just completed.",0) !:
        let resp$(rc+=1)=str$(ssrate)
01150   let fnlbl(12,1,"Medicare Maximum Wage:",mylen,1,0,0)
01160   let fntxt(12,mypos,10,0,0,"10",0,"Enter the medicare maximum wage for the year just completed.",0) !:
        let resp$(rc+=1)=str$(mcmax)
01170   let fnlbl(13,1,"Medicare Rate:",mylen,1,0,0)
01180   let fntxt(13,mypos,6,0,0,"34",0,"Enter the medicare rate for the year just completed.",0) !:
        let resp$(rc+=1)=str$(mcrate)
01190   let fncmdkey("Next",1,1,0,"Proceed with submission")
01200   let fncmdkey("Cancel",5,0,1,"Returns to menu")
01210   let fnacs(sn$,0,mat resp$,ckey) !:
        if ckey=5 then goto XIT
01220 L1220: print newpage
01230   close #101: ioerr L1240
01240 L1240: open #101: "SROW=2,SCOL=3,EROW=23,ECOL=77,BORDER=DR,CAPTION=<Create Electronic W2 Diskette for I.R.S.",display,outin 
01250   print fields "3,15,C 51,R,N": "  INSERT DISKETTE FOR ELECTRONIC W2'S IN DRIVE A:"
01260   print fields "5,5,C 60": "Company Name:"
01270   print fields "6,5,C 60": "Street Address:"
01280   print fields "7,5,C 60": "City:"
01290   print fields "8,5,C 60": "State:"
01300   print fields "9,5,C 60": "Zip Code:"
01310   print fields "10,5,C 60": "Federal ID #:"
01320   print fields "11,5,C 60": "Payment Year:"
01330   print fields "12,5,C 60": "Soc-Sec Maximum:"
01340   print fields "13,5,C 60": "Soc-Sec Rate:"
01350   print fields "14,5,C 60": "Medicare Maximum:"
01360   print fields "15,5,C 60": "Medicare Rate:"
01370   print fields "16,5,C 70": "Miscellaneous Deduction Containing Employer Cost Group-Term Life Ins:"
01380   print fields "17,5,C 70": "Miscellaneous Deduction Used For Pension:"
01390   print fields "18,5,C 70": "Miscellaneous Deduction Used For Deferred Compensation:"
01400   print fields "19,5,C 70": "Miscellaneous Deduction Used For Dependent Care Assistance:"
01410   print fields "20,5,C 60": "Computer Manufacturer's Name:"
01420   print fields "21,5,C 60,N": "F=First Name First or S=Surname First on File:"
01430   print fields "22,5,C 60": "Type of Business Code R=Regular:"
01440   print fields "24,28,C 9,B,1": "Next (F1)"
01450   print fields "24,39,C 11,B,5": "Cancel (F5)"
01460   print fields mat io1$: a$(1),a$(2),ct$,st$,zip$,b1,yr,87900,.062,999999,.0145,ins,pen,dfc,dcan,ibm$,namcde$,typemp$
01470 L1470: input fields mat io1$,attr "R": a$(1),a$(2),ct$,st$,zip$,b1,yr,ssmax,ssrate,mcmax,mcrate,ins,pen,dfc,dcan,ibm$,namcde$,typemp$ conv CONV1
01480   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
01490   if cmdkey>0 then goto L1560 else let ce=curfld+1
01500   if ce>udim(io1$) then let ce=1
01510 L1510: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : let ce1=pos(io1$(ce),"U",1)
01520   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L1470
01530 CONV1: if ce>0 then let io1$(ce)(ce1:ce2)="U"
01540   let ce=cnt+1
01550 ERR1: print fields "24,78,C 1": bell : goto L1510
01560 L1560: ! 
01570   if cmdkey=5 then goto XIT
01580   if rtrm$(a$(1))="" then let ce=1: goto ERR1
01590   if rtrm$(a$(2))="" then let ce=2: goto ERR1
01600   if rtrm$(ct$)="" then let ce=3: goto ERR1
01610   if rtrm$(st$)="" then let ce=4: goto ERR1
01620   if rtrm$(zip$)="" then let ce=5: goto ERR1
01630   if b1=0 then let ce=6: goto ERR1
01640   if yr<2001 then let ce=7: goto ERR1
01650   let ficarate=ssrate+mcrate
01660   if ssmax<53400 then let ce=8: goto ERR1
01670   if ins<0 or ins>10 then let ce=9: goto ERR1
01680   if pen<0 or pen>10 then let ce=10: goto ERR1
01690   if dfc<0 or dfc>10 then let ce=11: goto ERR1
01700 ! ______________________________________________________________________
01710   mat io1$(2)
01720   let io1$(1)="12,71,N 2,UT,N"
01730   let io1$(2)="14,71,N 2,UT,N"
01740   close #101: ioerr L1750
01750 L1750: print newpage
01760   open #101: "SROW=7,SCOL=2,EROW=16,ECOL=79,BORDER=DR,CAPTION=<Electronic W-2   State Reporting Information",display,outin 
01770   print fields "8,4,C 72": "Some states require filing W2's on diskette.  Answer the following"
01780   print fields "9,4,C 72": "questions if you wish to create 'RS' records during this run."
01790   print fields "12,8,Cr 62": "State code used in your record to identify the selected state:"
01800   print fields "14,8,Cr 62": "Appropriate FIPS postal numeric code:"
01810   print fields "15,8,C 70": "(See an appendix in your electronic booklet for the postal code!)"
01820   print fields "17,28,C 9,B,1": "Next (F1)"
01830   print fields "17,39,C 11,B,5": "Cancel (F5)"
01840 L1840: input fields mat io1$: sr1,sr2 conv L1840
01850   if ce>0 then let io1$(ce)(ce1:ce2)="U": let ce=0
01860   if cmdkey>0 then goto L1930 else let ce=curfld+1
01870   if ce>udim(io1$) then let ce=1
01880 L1880: let io1$(ce)=rtrm$(uprc$(io1$(ce))) : let ce1=pos(io1$(ce),"U",1)
01890   let ce2=ce1+1 : let io1$(ce)(ce1:ce1)="UC" : goto L1840
01900 CONV2: if ce>0 then let io1$(ce)(ce1:ce2)="U"
01910   let ce=cnt+1
01920 ERR2: print fields "24,78,C 1": bell : goto L1880
01930 L1930: if cmdkey=5 then goto XIT
01940   if sr1<0 or sr1>udim(e$) then let ce=1: goto ERR2
01950   if sr1>0 and sr2=0 then let ce=2: goto ERR2
01960 ! ______________________________________________________________________
01970   gosub SCR2
01980   print newpage
01990   let win=101
02000   let message$=""
02020 ! ______________________________________________________________________
02030   fnDedNames(mat fullname$,mat abrevname$,mat newdedcode,mat newcalcode,mat newdedfed,mat dedfica,mat dedst,mat deduc)
02040   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
02050 ! Open #2: "Name="&env$('Q')&"\PRmstr\RPTRAIL.h"&env$('cno')&",Shr",Internal,Input,Relative
02060   open #4: "Name="&env$('Q')&"\PRmstr\payrollchecks.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\checkidx.h"&env$('cno'),internal,outin,keyed 
02070 L2070: open #22: "Name=W2REPORT,RecL=1024,eol=crlf,replace",display,output 
02080   goto L2140
02090   print newpage
02100   let msgline$(1)="Insert Diskette"
02110   let mtype=1
02120   if err=4221 then gosub L4930 ! fnOldMsgBox(MAT RESPONSE$,CAP$,MAT MSGLINE$,MTYPE)
02130   goto L2070
02140 L2140: gosub RECRCA
02150 ! GOSUB RECRE
02160 L2160: ! Print Fields "12,32,N 3,UT,N": R1/LREC(1)*100
02170 L2170: read #1,using L2190: eno,mat em$,ss$,em6,ta eof END1
02180   gosub L4380
02190 L2190: form pos 1,n 8,3*c 30,c 11,pos 122,n 2,pos 173,pd 3
02200   let r1=r1+1
02210   let p1=pos(em$(3),",",1) : let comma=1
02220   if p1=0 then let p1=pos(em$(3)," ",1): let comma=0
02230   let emct$=em$(3)(1:p1-1)
02240   gosub L5770: let emst$=holdst$ ! If COMMA=1 Then Let EMST$=EM$(3)(P1+2:P1+3) Else Let EMST$=EM$(3)(P1+1:P1+2)
02250   let p2=len(rtrm$(em$(3)))
02260   let p1=p2-4
02270   let emzip$=em$(3)(p1:p2)
02280 L2280: let p1=pos(ss$,"-",1)
02290   if p1>0 then let ss$(p1:p1)="": goto L2280 else let ssn=val(ss$)
02300   let checkkey$=cnvrt$("pic(zzzzzzz#)",eno)&cnvrt$("pic(zz#)",0)&cnvrt$("pd 6",0) ! index employee#,department# and payroll date
02310   restore #4,key>=checkkey$: nokey L2170
02320 L2320: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L2680
02330   if heno<>eno then goto L2680
02340   if prd<beg_date or prd>end_date then goto L2320 ! not this year
02350   form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
02360   if tcd<1 or tcd>10 then let tcd=1
02370 ! finish with this employee
02380   let dedret=0
02390   let cafded=0
02400   for j=1 to 20
02410     if newdedfed(j)=1 then goto L2420 else goto L2430
02420 L2420: let dedret=dedret+tcp(j+4)
02430 L2430: if dedfica(j)=1 then goto L2440 else goto L2450
02440 L2440: let cafded=cafded+tcp(j+4)
02450 L2450: next j
02460   let w2(1)=min(w2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
02470   let w3=w3+tcp(2) ! TOTAL FICA WITHHELD
02480   let w3(1)=w3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
02490 ! if env$('client')="Washington Parrish" then let w3(1)=w3(1)+tcp(5) ! add deferred comp match to medicare wages
02500   let w3(1)=min(mcmax,w3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
02510   if uprc$(med$)="Y" then let w2=w2+tcp(2) else let w2=round(min(w3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
02520   if uprc$(med$)="Y" then let w3(2)=w3(2)+tcp(3) else let w3(2)=w3-w2 ! MEDICARE WITHHELD
02530   let w2(2)=w2(2)+tcp(30) ! FICA TIPS YTD
02540   let w2(3)=w2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
02550   let w2(4)=w2 ! W2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
02560 ! LET W2(4)=W2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
02570   let w2(5)=w2(5)+tcp(1) ! FED W/H YTD
02580   if ins>0 then let w2(6)=w2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
02590   let w2(7)=w2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
02600   let w2(8)=w2(8)+tcp(24) ! EIC TOTAL
02610   let w2(9)=w2(9)+0 ! ALLOCATED TIPS
02620   if dfc>0 then let dc1=dc1+tcp(4+dfc)*100 ! DEFERRED COMPENSATION
02630   if dcan>0 then let dca=dca+tcp(4+dcan)*100 ! DEPENDENT CARE ASSISTANCE
02640   if sr1><tcd then goto L2670
02650   let s2(1)=s2(1)+(tcp(31)*100)
02660   let s2(2)=s2(2)+(tcp(4)*100)
02670 L2670: goto L2320
02680 L2680: gosub EXTRACT_ORIGINAL
02690   if em6=9 then let w2(1)=w2(4)=w3(1)=w3(2)=0: let orgw2(1)=orgw2(4)=orgw3(1)=orgw3(2)=0 ! NO SS OR MC
02700   if em6=1 then let w3(1)=w3(2)=0: let orgw3(1)=orgw3(2)=0 ! NO MEDICARE
02710   if em6=2 then let w2(1)=w2(4)=0: let orgw2(1)=orgw2(4)=0 ! NO SOC-SEC
02720   if w2(3)=0 and w2(1)=0 then goto L2160
02725   if sum(w2)=sum(orgw2) and sum(w3)=sum(orgw3) then goto L2160 ! no changes to any dollar figusres
02730   gosub RECRCE
02740   gosub RECRCW
02750   gosub RECRS
02760   let tw1=tw1+1
02770   let tw2=tw2+1
02780   gosub RECRCT
02790   let tw2=0
02800   goto L2160
02810 ! ______________________________________________________________________
02820 RECRCA: print #22,using L2830: "RCA",b1,emppin$(1:8),"","98",a$(1),"",a$(2)(1:22),ct$,st$,zip$,"","","","","",contact$,contactph$,phoneext$,"",email$,"","","2","L","1",tlcn$,""
02830 L2830: form pos 1,c 3,pic(#########),c 8,c 9,c 2,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 6,c 23,c 15,c 2,c 27,c 15,c 5,c 3,c 40,c 3,c 10,c 1,c 1,c 1,c 6,c 701
02840   return 
02850 ! ______________________________________________________________________
02860 RECRCE: print #22,using L2870: "RCE",yr,"",b1,"","","","",a$(1)(1:37),"",a$(2)(1:22),ct$,st$,zip$(1:5),"","","","","","","R","","",""
02870 L2870: form pos 1,c 3,pic(####),c 9,pic(#########),c 1,c 9,c 4,c 4,c 57,c 22,c 22,c 22,c 2,c 5,c 4,c 4,c 23,c 15,c 2,c 1,c 1,c 1,c 1,c 799
02880   return 
02890 ! ______________________________________________________________________
02900   form pos 1,c 2,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,18*pic(###########),c 22,2*pic(###########),c 56,n 1,c 1,c 1,n 1,c 23,
02910   print #22,using L2920: "2E",ct$,st$,"",zip$,namcde$,typemp$,"","","",""
02920 L2920: form pos 1,c 2,g 25,g 10,2*g 5,2*g 1,g 2,g 4,g 2,c 71
02930   return 
02940 ! ______________________________________________________________________
02950 RECRCW: ! 
02960   for j=1 to 9
02970     let w2(j)=w2(j)*100
02980     let orgw2(j)=orgw2(j)*100
02990   next j
03000   for j=1 to 2
03010     let w3(j)=w3(j)*100
03020     let orgw3(j)=orgw3(j)*100
03030   next j
03040   if pen=0 then let pen$=" " else let pen$="1"
03050   if dfc=0 then let dfc$="" else let dfc$="D"
03060 ! medicare withholdings  exceptions
03070   let orgw32$="           ": let w32$="           "
03080   if orgw3(2)=w3(2) then let orgw32$="           ": let w32$="           " : goto L3100 ! if amounts are the same,then report both as blanks
03090   if orgw3(2)>0 or w3(2)>0 then let orgw32$=cnvrt$("pic(###########)",orgw3(2)): let w32$=cnvrt$("pic(###########)",w3(2)) ! try to stop accuwage errors when either the original medicare withholdings or the new one is >0
03100 L3100: ! federal withholdings exceptions
03110   let w25$="           ": let orgw25$="           "
03120   if w2(5)=orgw2(5) then goto L3140 ! try to stop accuwage errors when both both federal wh are same
03130   if w2(5)>0 or orgw2(5)>0 then let w25$=cnvrt$("pic(###########)",w2(5)): let orgw25$=cnvrt$("pic(###########)",orgw2(5)) ! try to stop accuwage errors when both both federal wh are zeros on indiviuals
03140 L3140: !  total wages
03150   let w23$="           ": let orgw23$="           "
03160   if w2(3)=orgw2(3) then goto L3180 ! try to stop accuwage errors when both both total wages
03170   if w2(3)>0 or orgw2(3)>0 then let w23$=cnvrt$("pic(###########)",w2(3)): let orgw23$=cnvrt$("pic(###########)",orgw2(3)) ! try to stop accuwage errors when both both total wages are zeros on indiviuals
03180 L3180: ! ss wages
03190   let w21$="           ": let orgw21$="           "
03200   if w2(1)=orgw2(1) then goto L3220 ! try to stop accuwage errors when both both ss wages agree
03210   if w2(1)>0 or orgw2(1)>0 then let w21$=cnvrt$("pic(###########)",w2(1)): let orgw21$=cnvrt$("pic(###########)",orgw2(1)) ! try to stop accuwage errors when both both ss wages are zeros on indiviuals
03220 L3220: ! medicare wages
03230   let w31$="           ": let orgw31$="           "
03240   if w3(1)=orgw3(1) then goto L3260 ! try to stop accuwage errors when both both medicare wages agree
03250   if w3(1)>0 or orgw3(1)>0 then let w31$=cnvrt$("pic(###########)",w3(1)): let orgw31$=cnvrt$("pic(###########)",orgw3(1)) ! try to stop accuwage errors when both both medicare wages are zeros on indiviuals
03260 L3260: ! ss wh
03270   let w24$="           ": let orgw24$="           "
03280   if w2(4)=orgw2(4) then let orgw2(4)=0: goto L3300 ! try to stop accuwage errors when both both medicare wages agree !! had to set the orgw2(4) to zero because I could never change the print line to allow me to make it a alpha field  ??? still don't know why i cannot change it
03290   if w2(4)>0 or orgw2(4)>0 then let w24$=cnvrt$("pic(###########)",w2(4)): let orgw24$=cnvrt$("pic(###########)",orgw2(4)) ! try to stop accuwage errors when both both ss wh are zeros
03300 L3300: print #22,using L3310: "RCW",0,ssn,first$,mid$,last$,first$,mid$,last$,"",em$(2)(1:22),emct$,emst$,emzip$,"","","","","",orgw23$,w23$,orgw25$,w25$,orgw21$,w21$,orgw2(4),w24$,orgw31$,w31$,orgw32$,w32$,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",pen$,pen$,"","",""
03310 L3310: form pos 1,c 3,2*pic(#########),c 15,c 15,c 20,c 15,c 15,c 20,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,6*c 11,pic(###########),5*c 11,28*c 11,c 22,2*c 11,8*c 11,c 187,c 1,c 1,c 1,c 1,c 1,c 1,c 16
03320   return 
03330 RECRS: ! STATE RECORD
03340   if sr1=0 then goto L3370 ! NO STATE SELECTED
03350   if s2(1)=0 and s2(2)=0 then goto L3370 ! NO STATE WAGES
03360   form pos 1,c 2,g 2,c 5,pic(#########),c 15,c 15,c 20,c 4,c 22,c 22,c 22,c 2,c 5,c 4,c 5,c 23,c 15,c 2,c 2,c 6,2*pic(###########),pic(##),2*pic(########),c 5,c 20,c 6,g 2,2*pic(###########),c 10,c 1,2*pic(###########),c 7,c 75,c 75,c 25
03370 L3370: let t1=t1+1: mat t1=t1+w2 : let orgt1=orgt1+1: mat orgt1=orgt1+orgw2
03380   mat i1=i1+w2
03390   mat i2=i2+w3
03400   mat t2=t2+w3 : mat orgt2=orgt2+orgw3
03410   let dc2=dc2+dc1
03420   let dc3=dc3+dc1
03430   let dca2=dca2+dca
03440   let dca3=dca3+dca
03450   let w2=w3=dca=dc1=orgw2=wrfw3=orgdca=orgdc1=0
03460   mat w2=(0)
03470   mat w3=(0)
03480   mat s2=(0)
03490   mat orgw2=(0)
03500   mat orgw3=(0)
03510   return 
03520 ! ______________________________________________________________________
03530 ! ______________________________________________________________________
03540 RECRCT: ! 
03550 ! total wages
03560   let t13$="               " : let orgt13$="               "
03570   if orgt1(3)=t1(3) then goto L3590 ! both equal
03580   if orgt1(3)>0 or t1(3)>0 then let orgt13$=cnvrt$("pic(###############)",orgt1(3)): let t13$=cnvrt$("pic(###############)",t1(3)) ! try to stop accuwage errors when either total wages are >0
03590 L3590: ! ss wages
03600   let t11$="               " : let orgt11$="               "
03610   if orgt1(1)=t1(1) then goto L3630 ! both equal
03620   if orgt1(1)>0 or t1(1)>0 then let orgt11$=cnvrt$("pic(###############)",orgt1(1)): let t11$=cnvrt$("pic(###############)",t1(1)) ! try to stop accuwage errors when either total ss wages are >0
03630 L3630: ! ss wh
03640   let t14$="               " : let orgt14$="               "
03650   if orgt1(4)=t1(4) then goto L3670 ! both equal
03660   if orgt1(4)>0 or t1(4)>0 then let orgt14$=cnvrt$("pic(###############)",orgt1(4)): let t14$=cnvrt$("pic(###############)",t1(4)) ! try to stop accuwage errors when either total ss wh are >0
03670 L3670: ! total medicare withholding
03680   let t22$="               " : let orgt22$="               "
03690   if orgt2(2)=t2(2) then goto L3710 ! both equal
03700   if orgt2(2)>0 or t2(2)>0 then let orgt22$=cnvrt$("pic(###############)",orgt2(2)): let t22$=cnvrt$("pic(###############)",t2(2)) ! try to stop accuwage errors when either medicare withholdings are >0
03710 L3710: ! medicare wages
03720   let orgt21$="               " : let t21$="               "
03730   if orgt2(1)=t2(1) then goto L3750
03740   if orgt2(1)>0 or t2(1)>0 then let orgt21$=cnvrt$("pic(###############)",orgt2(1)): let t21$=cnvrt$("pic(###############)",t2(1)) ! try to stop accuwage errors when either the old or new is greater than zero
03750 L3750: ! federal wh
03760   let orgt15$="             " : let t15$="             "
03770   if t1(5)=orgt1(5) then goto L3800 ! both are same
03780   if t1(5)>0 or orgt1(5)>0 then let t15$=cnvrt$("pic(###############)",t1(5)): let orgt15$=cnvrt$("pic(###############)",orgt1(5)) ! try to stop accuwage errors when both were either fed wh >0
03800 L3800: print #22,using L3820: "RCT",tw2,orgt13$,t13$,orgt15$,t15$,orgt11$,t11$,orgt14$,t14$,orgt21$,t21$,orgt22$,t22$,orgt12$,t12$,"","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","","",""
03810   let dc3=0 ! kj 120805
03820 L3820: form pos 1,c 3,pic(#######),14*c 15,26*c 15,c 15,2*c 15,8*c 15,c 234
03830   form pos 1,c 2,pic(#######),7*pic(###############),c 240,9*pic(###############),c 23
03840   let t1=0: let orgt1=0: mat t1=(0) : mat orgt1=(0)
03850   mat t2=(0): mat orgt2=(0)
03860   return 
03870 ! ______________________________________________________________________
03880 RECRCF: print #22,using L3890: "RCF",tw1,""
03890 L3890: form pos 1,c 3,pic(#########),c 1012
03900   return 
03910 ! ______________________________________________________________________
03920 END1: ! 
03930 ! Gosub RECRCT
03940   gosub RECRCF
03950   gosub L3980
03960 XIT: let fnxit
03970 ! ______________________________________________________________________
03980 L3980: close #24: ioerr L4000
03990   dim a$*1024
04000 L4000: close #22: ioerr L4010
04010 L4010: open #24: "Name=X,RecL=1025,EOL=NONE,REPLACE",external,output 
04020   open #22: "Name=w2report,RecL=1024",display,input 
04030 L4030: linput #22: a$ eof L4080
04040   if a$(1024:1024)="X" then let a$(1024:1024)=""
04050   write #24,using L4060: rpad$(a$,1024),chr$(10)
04060 L4060: form pos 1,c 1024,c 1
04070   goto L4030
04080 L4080: close #24: 
04090   close #22: 
04100   execute "COPY x "&path$&" -n"
04110   return 
04120 SCR2: ! 
04130   dim contact$*27,email$*40
04140   let win=101
04150   let win_height=12: let win_width=75: let display_cnam=1: let button_option=2: gosub L4560 ! Let fnWin3b(WIN,CAP$,12,62,1,2)
04160   print #win,fields "04,2,Cr 31,N": "Personal ID Number:" !:
        print #win,fields "05,2,Cr 31,N": "Resub Indicator:" !:
        print #win,fields "06,2,Cr 31,N": "Resub TLCN:" !:
        print #win,fields "07,2,Cr 31,N": "Contact Name:" !:
        print #win,fields "08,2,Cr 31,N": "Contact Phone Number:" !:
        print #win,fields "09,2,Cr 31,N": "Contact Phone Extension:" !:
        print #win,fields "10,2,Cr 31,N": "Contact E-Mail:" !:
        print #win,fields "11,2,Cr 31,N": "Terminating Business Indicator:"
04170   let scr2_io$(1)="04,34,C 17,UT,N" !:
        let scr2_io$(2)="05,34,C 01,UT,N" !:
        let scr2_io$(3)="06,34,C 06,UT,N" !:
        let scr2_io$(4)="07,34,C 27,UT,N" !:
        let scr2_io$(5)="08,34,C 15,UT,N" !:
        let scr2_io$(6)="09,34,C 05,UT,N" !:
        let scr2_io$(7)="10,34,C 40,UT,N" !:
        let scr2_io$(8)="11,34,C 01,UT,N"
04180   if resub$="" then let resub$="0"
04190 ! If TLCN$="" Then Let TLCN$="0"
04200   if terminat$="" then let terminat$="0"
04210 L4210: rinput #win,fields mat scr2_io$: emppin$,resub$,tlcn$,contact$,contactph$,phoneext$,email$,terminat$ conv CONV_SCR2
04220   let med$="Y"
04230   if ce>0 then let scr2_io$(ce)(ce1:ce2)="U": let ce=0
04240   if cmdkey>0 then goto L4310 else let ce=curfld
04250 L4250: let ce=ce+1: if ce>udim(scr2_io$) then let ce=1
04260 L4260: let scr2_io$(ce)=rtrm$(scr2_io$(ce)) : let ce1=pos(scr2_io$(ce),"U",9) : if ce1=0 then goto L4250
04270   let ce2=ce1+1 : let scr2_io$(ce)(ce1:ce1)="UC" : goto L4210
04280 CONV_SCR2: if ce>0 then let scr2_io$(ce)(ce1:ce2)="U"
04290   let ce=cnt+1
04300 ERR_SCR2: print fields "24,78,C 1": bell : goto L4260
04310 L4310: if resub$<>"0" and resub$<>"1" then let ce=2 !:
          goto ERR_SCR2
04320   if resub$="1" and rtrm$(tlcn$)="" then let ce=3 !:
          goto ERR_SCR2
04330   if terminat$<>"0" and terminat$<>"1" then let ce=8 !:
          goto ERR_SCR2
04340   if uprc$(med$)="Y" or uprc$(med$)="N" then goto L4350 else let ce=9: goto ERR_SCR2
04350 L4350: close #win: 
04360   if cmdkey=5 then goto SCR1
04370   return 
04380 L4380: dim first$*15,mid$*15,last$*20,em$(3)*30
04390   let em$(1)=uprc$(rtrm$(em$(1))): ! Let NAMCDE$="s"
04400   let x1=pos(em$(1)," ",1)
04410   let x2=pos(em$(1)," ",x1+1)
04420   let x3=pos(em$(1)," ",x2+1)
04430   if uprc$(namcde$)="S" then goto L4480
04440   let first$=em$(1)(1:min(15,max(x1-1,1)))
04450   if x2>0 then let mid$=em$(1)(x1+1:x2-1): let last$=em$(1)(x2+1:len(em$(1)))
04460   if x2=0 then let last$=em$(1)(x1+1:len(em$(1))): let mid$=""
04470   goto L4540
04480 L4480: ! last name first
04490   if x1=0 then let x1=pos(em$(1),",",1)
04500   if x1>0 and em$(1)(x1-1:x1-1)="," then let last$=em$(1)(1:x1-2) else let last$=em$(1)(1:max(x1-1,1))
04510   if x2>0 then let first$=em$(1)(x1+1:x2-1): let mid$=em$(1)(x2+1:len(em$(1)))
04520   if x2=0 then let first$=em$(1)(x1+1:len(em$(1)))(1:15): let mid$=""
04530   let x=pos(first$,",",1): if x>0 then let first$(x:x)=""
04540 L4540: ! Print FIRST$,MID$,LAST$
04550   return 
04560 L4560: ! Def Library fnWin3b(WIN,&CAP$,WIN_HEIGHT,WIN_WIDTH,DISPLAY_CNAM,BUTTON_OPTION)
04570 ! Win= Window number to be opened
04580 ! Cap$= Caption to be used
04590 ! Win_Height and Win_Width= seems pretty obvious don't it
04600 ! Display_CNam= 1. Display Cnam and Cno in top 2 lines of window !:
        !               2. Display only Cno in top 1 line
04610 ! button_option= 0. Does nothing. !:
        !                1. Cancel(F5)  !:
        !                2. Next(F1),Cancel(F5) !:
        !                3. Print(F1),Cancel(F5)  !:
        !                4. Save (F1),Cancel(F5) !:
        !                5. Next (F1),Cancel(F5),Search(F6) !:
        !                6. Next (F1),Back(F2),Cancel(F5) !:
        !                7. Save (F1),Delete(F4),Cancel(F5) !:
        !                8. (undefinded)  !:
        ! (button_option>0 is set to have Cancel(F5))
04630   if exists("C:\ACS\Local\Settings\No_Print_Newpage.txt") then goto L4640 else print newpage
04640 L4640: let screen_width=80
04650   let screen_height=24
04660   if display_cnam=0 then goto L4680
04680 L4680: let sc=max(int(((screen_width-win_width)/2)+1),2)
04690   let ec=min(sc+win_width-1,79)
04700   let sr=max(int(((screen_height-win_height)/2)+1),2)
04710   let er=min(sr+win_height-1,23)
04720 !     Print "win_height="&STR$(WIN_HEIGHT),"win_width="&STR$(WIN_WIDTH)
04730 !     Print "sr="&STR$(SR),"sc="&STR$(SC)
04740 !     Print "er="&STR$(ER),"ec="&STR$(EC) : Pause
04750   close #win: ioerr L4760
04760 L4760: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outin 
04770   print #win: newpage
04780   if display_cnam=0 then goto L4810
04790   if display_cnam=1 then !:
          print #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width)) !:
          print #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
04800   if display_cnam=2 then !:
          print #win,fields "1,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
04810 L4810: if button_option=0 then goto L4920
04820   mat fkey$=("") : let em$="" : let es=0
04830   let fkey$(5)="Cancel" ! included by default
04840   if button_option=2 then !:
          let fkey$(1)="Next"
04850   if button_option=3 then !:
          let fkey$(1)="Print"
04860   if button_option=4 then !:
          let fkey$(1)="Save"
04870   if button_option=5 then !:
          let fkey$(1)="Next" !:
          let fkey$(6)="Search"
04880   if button_option=6 then !:
          let fkey$(1)="Next" !:
          let fkey$(2)="Back"
04890   if button_option=7 then !:
          let fkey$(1)="Save" !:
          let fkey$(4)="Delete"
04900   let scrline=er+1: gosub L5560 !  Let FNFKEY(ER+1,MAT FKEY$,MAT DISFK,EM$,ES)
04910 ! 
04920 L4920: return  ! Fnend
04930 L4930: ! mtype=0 means splash    - returns no response                                 ! mostly for "please wait..." and "printing..."                                 ! (anywhere no response is required - no buttons are displyed either)
04940 ! mtype=1 means OK only   - returns no response
04950 ! mtype=2 means Yes or No - returns "Y" or "N"
04960 ! mtype=3 means Yes, No, Cancel - returns "Y" or "N" or ""
04970 ! response$(1)= code you're looking for 2-5 are reserved for future use
04980   close #104: ioerr L4990
04990 L4990: let endrow=12
05000   for j=2 to udim(msgline$)
05010     if msgline$(j)<>"" then let endrow=endrow+1
05020   next j
05030   open #104: "SRow=10,SCol=09,ERow="&str$(endrow)&",ECol=70,Border=SR,Caption=<"&cap$,display,outin 
05040   print #104: newpage
05050   let mglinerow=2
05060   for j=1 to udim(msgline$)
05070     print #104,fields str$(mglinerow+j-1)&",2,Cc 60,N": msgline$(j)
05080   next j
05090   if mtype=1 then print fields str$(endrow+1)&",38,Cc 4,B,1": "Ok"
05100   if mtype=1 then input fields str$(endrow)&",09,C 1,AE,N": pause$
05110   if mtype=2 then print fields str$(endrow+1)&",35,Cc 4,B,21": "Yes"
05120   if mtype=2 then print fields str$(endrow+1)&",40,Cc 4,B,22": "No"
05130 L5130: if mtype=2 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
05140   if mtype=2 and cmdkey=22 then let response$(1)="N"
05150   if mtype=2 and cmdkey=21 then let response$(1)="Y"
05160   if mtype=2 and response$(1)<>"Y" and response$(1)<>"N" then print fields "24,1,C 7,N": bell$ : goto L5130
05170   if mtype=3 then print fields str$(endrow+1)&",29,Cc 4,B,21": "Yes"
05180   if mtype=3 then print fields str$(endrow+1)&",34,Cc 4,B,22": "No"
05190   if mtype=3 then print fields str$(endrow+1)&",39,C 12,B,22": "Cancel (Esc)"
05200   if mtype=3 then input fields str$(endrow)&",09,Cu 1,AE,N": response$(1)
05210   if mtype=3 and cmdkey=22 then let response$(1)="N"
05220   if mtype=3 and cmdkey=21 then let response$(1)="Y"
05230   if mtype=3 and cmdkey=99 then let response$(1)=""
05240   if mtype=3 and response$(1)<>"Y" and response$(1)<>"N" and response$(1)<>"" then print fields "24,1,C 7,N": bell$ : goto L5130
05250   close #104: ioerr ignore
05260 return  ! Fnend
05410 ! Def Library FNOPENWIN(WIN,SR,SC,ER,EC,&CAP$)
05430   if sr<1 then let sr=10
05440   if sc<1 then let sc=20
05450   if er<1 then let er=14
05460   if ec<1 then let ec=59
05470   let win_width=ec-sc+1
05480   close #win: ioerr L5490
05490 L5490: open #win: "SRow="&str$(sr)&",SCol="&str$(sc)&",ERow="&str$(er)&",ECol="&str$(ec)&",Border=Sr,Caption=<"&cap$,display,outin 
05500   print #win: newpage
05510   print #win,fields "1,1,Cc "&str$(win_width)&",R,N": env$('cnam')(1:min(40,win_width))
05520   print #win,fields "2,1,Cc "&str$(win_width)&",R,N": "Company Number "&env$('cno')(1:min(40,win_width))
05530 ! 
05540 ! 
05550   return  ! Fnend
05560 L5560: ! Def Library FNFKEY(SCRLINE,MAT FKEY$,MAT DISFK,&EM$,ES)
05570   let totallen=0 !:
        let startpos=0
05580   for j=1 to udim(fkey$) ! add ' (Fx)' to each button
05590     if fkey$(j)="" then goto L5620
05600     let fkey$(j)=fkey$(j)&" (F"&str$(j)&")" !:
          ! add ' (Fx)' to each button
05610     let totallen=totallen+len(fkey$(j))+1
05620 L5620: next j
05630   let totallen=totallen+len(rtrm$(em$))+min(len(rtrm$(em$)),1)+es
05640   let totallen=totallen-1
05650   let startpos=int((80-totallen)/2)+1
05660   print fields str$(scrline)&","&str$(startpos)&",C "&str$(totallen)&",N": rpt$("Ä",totallen)
05670   for j=1 to udim(fkey$)
05680     if fkey$(j)="" then goto L5730
05690     if disfk(j)=1 then print fields str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",R,"&str$(j): fkey$(j)
05700     if disfk(j)=1 then goto L5720
05710     print fields str$(scrline)&","&str$(startpos)&",C "&str$(len(fkey$(j)))&",B,"&str$(j): fkey$(j)
05720 L5720: let startpos=startpos+len(fkey$(j))+1
05730 L5730: next j
05740   if rtrm$(em$)="" then goto L5760
05750   print fields str$(scrline)&","&str$(startpos)&",C "&str$(len(rtrm$(em$))+es)&",R,N": rtrm$(em$)
05760 L5760: return  ! Fnend
05770 L5770: ! extract state name
05780   let holdst$="          "
05790   let p3=oldp3=0
05800   let p4=10
05810   for j=1 to 10
05820     let p3=pos(rtrm$(em$(3))," ",p3+1)
05830     if oldp3>p3 then goto L5860 ! end of address reached
05840     if p3>0 then let oldp3=p3 else goto L5850
05850 L5850: next j
05860 L5860: for j=1 to 10
05870     if rtrm$(em$(3)(oldp3-j:oldp3-j))="" or em$(3)(oldp3-j:oldp3-j)="," then goto L5880 else let p4=p4-1: let holdst$(p4:p4)=em$(3)(oldp3-j:oldp3-j): goto L5890
05880 L5880: if rtrm$(holdst$)="" then goto L5890 else goto L5900
05890 L5890: next j
05900 L5900: let holdst$=ltrm$(holdst$)(1:2)
05910   if holdst$="TE" then let holdst$="TX"
05920   return 
05930 ! ______________________________________________________________________
05940 ! <Updateable Region: ERTN>
05950 ERTN: let fnerror(program$,err,line,act$,"xit")
05960   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
05970   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
05980   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
05990 ERTN_EXEC_ACT: execute act$ : goto ERTN
06000 ! /region
06010 ! ______________________________________________________________________
06020 EXTRACT_ORIGINAL: ! 
06030   restore #4,key>=checkkey$: nokey L2170
06040 L6040: read #4,using "Form POS 1,N 8,n 3,PD 6,N 7,5*PD 3.2,37*PD 5.2": heno,tdn,prd,ckno,mat tdc,mat tcp eof L6370
06050   if heno<>eno then goto L6370
06060   if prd<orgbeg_date or prd>orgend_date then goto L6040 ! not this year
06070   form pos 1,n 8,pos 48,n 2,pos 168,21*pd 5.2,pos 468,pd 3
06080   if tcd<1 or tcd>10 then let tcd=1
06090 ! finish with this employee
06100   let dedret=0
06110   let cafded=0
06120   for j=1 to 20
06130     if newdedfed(j)=1 then goto L6140 else goto L6150
06140 L6140: let dedret=dedret+tcp(j+4)
06150 L6150: if dedfica(j)=1 then goto L6160 else goto L6170
06160 L6160: let cafded=cafded+tcp(j+4)
06170 L6170: next j
06180   let orgw2(1)=min(orgw2(1)+tcp(31)-tcp(30)-cafded,ssmax-tcp(30)) ! TOTAL SOC-SEC WAGES
06190   let orgw3=orgw3+tcp(2) ! TOTAL FICA WITHHELD
06200   let orgw3(1)=orgw3(1)+tcp(31)-cafded ! TOTAL MEDICARE WAGES & TIPS
06210 ! if env$('client')="Washington Parrish" then let orgw3(1)=orgw3(1)+tcp(5) ! add deferred comp match to medicare wages
06220   let orgw3(1)=min(mcmax,orgw3(1)) ! MC WAGES CANNOT EXCEED MAXIMUM
06230   if uprc$(med$)="Y" then let orgw2=orgw2+tcp(2) else let orgw2=round(min(orgw3/(ssrate+mcrate)*ssrate,ssmax*ssrate),2) ! SS WH
06240   if uprc$(med$)="Y" then let orgw3(2)=orgw3(2)+tcp(3) else let orgw3(2)=orgw3-orgw2 ! MEDICARE WITHHELD
06250   let orgw2(2)=orgw2(2)+tcp(30) ! FICA TIPS YTD
06260   let orgw2(3)=orgw2(3)+tcp(31)-dedret ! TOTAL FEDERAL WAGES
06270   let orgw2(4)=orgw2 ! orgw2(4)+tcp(2) ! FICA W/H YTD       (COULD BE +W2 INSTEAD OF +tcp(2)IF ONLY FICA PORTION GOES INTO W2 RECORD)
06280 ! LET orgw2(4)=orgW2 ! PUT SS W/H ONLY IN 2-W RECORD (EXCLUDE MEDICARE W/H)
06290   let orgw2(5)=orgw2(5)+tcp(1) ! FED W/H YTD
06300   if ins>0 then let orgw2(6)=orgw2(6)+tcp(4+ins) ! EMPLOYER COST GROUP LIFE INS.
06310   let orgw2(7)=orgw2(7)+0 ! UNCOLLECTED EMPLOYEE FICA TAX ON TIPS
06320   let orgw2(8)=orgw2(8)+tcp(24) ! EIC TOTAL
06330   let orgw2(9)=orgw2(9)+0 ! ALLOCATED TIPS
06340   if orgdfc>0 then let orgdc1=orgdc1+tcp(4+orgdfc)*100 ! DEFERRED COMPENSATION
06350   if orgdcan>0 then let orgdca=orgdca+tcp(4+orgdcan)*100 ! DEPENDENT CARE ASSISTANCE
06360   goto L6040
06370 L6370: if em6=9 then let orgw2(1)=orgw2(4)=orgw3(1)=orgw3(2)=0 ! NO SS OR MC
06380   if em6=1 then let orgw3(1)=orgw3(2)=0 ! NO MEDICARE
06390   if em6=2 then let orgw2(1)=orgw2(4)=0 ! NO SOC-SEC
06400   return 
