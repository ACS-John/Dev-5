00010 ! Replace S:\acsGL\PR941  ! fix the count (box 1; needs logic to look thru history and count the active employees on a certain date)
00020 ! 941 Summary  ( Prints a detail of employees and the complete 941 using priint ace
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnerror,fnoldmsgbox,fnopenprn,fncloseprn,fnprocess,fntop,fnchain,fntos,fnlbl,fntxt,fnchk,fncmdset,fnacs,fncomboa,fnfra,fnconsole,fnpa_finis,fnpa_newpage
00050   let fntop(program$,cap$="Print 941 Report")
00060   on error goto ERTN
00070 ! ______________________________________________________________________
00080   dim io1$(2),ss$*11,em$(3)*30,ty(21),tqm(17),frm_wrd$(2)*11
00090   dim dedcode(10),calcode(10),dedfed(10),option1$(4)*20
00100   dim a$(3)*40,b$(2)*12,d$(10)*8,m(10),r(10),msgline$(2)*60
00110   dim e$(10)*12,tpt(26),pt(26),cap$*128,message$*40,option$(4)*15,resp$(15)*30
00120   dim d(2),e$(2)*12,prgl(5,3),miscname$(10)*20,dedfica(10),dedst(10),deduc(10),miscgl$(10)*12
00130   dim tb$*30,m(36),k(1),l$(1)*11,k$(3)*30
00140   dim city$*15,state$*2,zip$*9,csz$*40,x$*40
00150 ! ______________________________________________________________________
00170   let fnconsole(off=1)
00180 ! ______________________________________________________________________
00190   def fna(r)=int(r*100+.5)/100 ! /r
00200 ! ______________________________________________________________________
00280   open #1: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno')&",Shr",internal,input,relative 
00290   read #1,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,2*C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
00300   let ficarate=ficarate/100
00310   let mcr=mcr*.01
00320 ! Close #20:
00330 ! ______________________________________________________________________
00340 MENU1: ! 
00350   let fntos(sn$="pr941") !:
        let respc=0
00360   if val(date$(4:5))=1 then let taxyear=val(date$(1:2))+2000-1 else let taxyear =val(date$(1:2))+2000 ! current tax year (if processing in jan, assume last year)
00370   let fnlbl(1,1,"Tax Year:",26,1)
00380   let fntxt(1,30,4,0,0,"30",0,"") !:
        let resp$(respc+=1)=str$(taxyear)
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
00490   let fnchk(3,30,"Print Worksheet:",1) !:
        let resp$(respc+=1)="True"
00500   let fnfra(5,1,4,30,"Tax Liability","Enter the total tax liability by month")
00510   let fnlbl(1,1,"Month 1:",10,1,0,1)
00520   let fntxt(1,13,12,0,1,"10",0,"",1) !:
        let resp$(respc+=1)=""
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
00725   let fnlbl(20,1,"Total deposits for quarter including overpayments:",mylen+1,1,0,0) !:
        let fntxt(20,mylen+4,12,0,1,"10",0,"",0) !:
        let resp$(respc+=1)=""
00730   let fncmdset(2): let fnacs(sn$,0,mat resp$,ck)
00740   if ck=5 then goto XIT
00750   let taxyear$=resp$(1) ! tax year
00760   for j=1 to 4
00770     if resp$(2)=option1$(j) then let qtr=j: let m$=option1$(j): goto L790 ! quarter ending date
00780   next j
00790 L790: if qtr=1 then begdate=val(taxyear$)*10000+0312: let enddate=val(taxyear$)*10000+0318
00800   if qtr=2 then begdate=val(taxyear$)*10000+0612: let enddate=val(taxyear$)*10000+0618
00810   if qtr=3 then begdate=val(taxyear$)*10000+0912: let enddate=val(taxyear$)*10000+0918
00820   if qtr=4 then begdate=val(taxyear$)*10000+1212: let enddate=val(taxyear$)*10000+1218
00830   if resp$(3)="True" then let frm=2 else let frm=1 ! need a worksheet
00840   box15a=val(resp$(4)) ! first month liability
00850   box15b=val(resp$(5))
00860   box15c=val(resp$(6))
00870   box7a=val(resp$(7)) ! fractions
00880   box7b=val(resp$(8)) ! sick pay
00890   box7c=val(resp$(9)) ! tips
00900   box7d=val(resp$(10)) ! tax wh
00910   box7e=val(resp$(11)) ! prior qtr
00920   box7f=val(resp$(12)) ! special add
00930   box7g=val(resp$(13)) ! special add - ss
00935   box11=val(resp$(14)) ! total deposits
00940 ! Gosub GET_MAT_TPT
00950   gosub START_PRINT
00960   gosub BUILD_941
00970   gosub LASER_941
00980   goto DONE
00990 ! ______________________________________________________________________
01000 START_PRINT: ! 
01010   if frm=1 then goto L1070
01020   let message$="Printing: please wait..."
01030   let fnwait(105,cap$,message$,1)
01040   on fkey 5 goto DONE
01050   let fnopenprn
01060   on pageoflow goto PGOF
01070 L1070: open #2: "Name="&env$('Q')&"\GLmstr\PRmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRIndex.h"&env$('cno')&",Shr",internal,input,keyed 
01080   if frm=2 then gosub WK_HEADER
01090 L1090: let m1=0
01100   let m2=0
01110   let h2=0
01120   let h3=0
01130   let extram1=0
01140   let dfy=dfq=0
01150 ! Read #2,Using 1120: ENO,MAT EM$,SS$,EM5,EM6,TA Eof WK_END
01160   read #2,using L1170: mat k,mat k$,mat l$,mat m eof WK_END
01170 L1170: form pos 1,n 4,3*c 25,c 11,36*pd 5.2,2*n 5
01180 !  Form POS 1,N 8,3*C 30,C 11,POS 120,2*N 2,POS 173,PD 3
01190   for j=1 to 10
01200     if dedfed(j)=1 and dedcode(j)=1 then let dwy+=m(j+10): let dwq+=m(j+11)
01210     if dedfica(j)=1 and dedcode(j)=1 then let dfy=dfy+m(j+10): let dfq=dfq+m(j+11)
01220   next j
01230   form pos 168,21*pd 5.2,pos 273,17*pd 5.2,pos 468,pd 3
01240   let m2=m2+m(1)
01250   let m1=m1+m(2)
01260   let tqm1=tqm1+m(4)
01270   let tqm14=tqm14+m(36)
01280   if m2=0 then goto L1090
01290 ! will need a routine here to read payroll dates, but think the checks are cleared each month
01300 ! Let KEY$=CNVRT$("pic(ZZZZZZZZ)",ENO)&RPT$(CHR$(0),6) !:
        ! Restore #4,Key>=KEY$:
01310 ! Read #4,Using 1270: DEPENO,PRD Nokey 1360 Eof 1360
01320 ! Form POS 1,N 8,PD 6
01330 ! If EM5=1 Then Let PEDATE=BEGDATE+19 ! monthly pay period
01340 ! If EM5=2 Then Let PEDATE=BEGDATE+15 ! semi-monthly
01350 ! If EM5=3 Then Let PEDATE=BEGDATE+14 ! bi-weekly
01360 ! If EM5=4 Then Let PEDATE=BEGDATE+7 ! weekly
01370 ! If DEPENO<>ENO Then Goto 1360
01380 ! If DEPENO=ENO AND PRD<BEGDATE Then Goto 1260 ! search for next check
01390 ! If DEPENO=ENO AND PRD>PEDATE Then Goto 1360 ! do't count
01400 ! If DEPENO=ENO Then bOX1+=1 ! count employees who worked the twelfth day of the first month of the quarter
01410   gosub PRINT_DETAILS
01420   goto L1090
01430 ! ______________________________________________________________________
01440 WK_HEADER: ! 
01450   let p2=p2+1
01460   pr #255,using L1480: "Page ",p2
01470   pr #255: ""
01480 L1480: form pos 70,c 5,pic(zzz)
01490   pr #255: tab(15);"Employer's Quarterly Federal Tax Return Worksheet"
01500   pr #255,using L1510: "For quarter ended "&m$&", "&taxyear$
01510 L1510: form pos 20,cc 40
01520   pr #255: ""
01530   pr #255: ""
01540   if eof=1 then goto L1670
01550   pr #255,using L1560: a$(1),"Fed ID",b$(1)
01560 L1560: form pos 17,c 40,pos 59,c 6,pos 69,c 40
01570   pr #255,using L1580: a$(2),"State ID",b$(2)
01580 L1580: form pos 17,c 40,pos 59,c 8,pos 69,c 12,skip 1
01590   pr #255,using L1600: a$(3),"State",d$(1)
01600 L1600: form pos 17,c 40,pos 59,c 5,pos 69,c 8,skip 1
01610   pr #255: ""
01620   pr #255: tab(41);"Total Wages    Social-Sec.      Medicare"
01630   pr #255: " SS Number             Name";
01640   pr #255: tab(41);"For Quarter        Wages";tab(75);"Wages"
01650   pr #255: "___________  __________________________";
01660   pr #255: tab(41);"___________   ____________  ____________"
01670 L1670: return 
01680 ! ______________________________________________________________________
01690 WK_END: ! 
01700   gosub TOTALS
01710   if frm=1 then goto L1730
01720   let fncloseprn
01730 L1730: return 
01740 DONE: ! 
01750 XIT: let fnxit
01760 ! ______________________________________________________________________
01770 PRINT_DETAILS: ! detailed listing
01780   if m1=0 then goto L2030
01790   let p3=p3+1
01800   let h2=h3=0
01810 ! If EM6=2 OR EM6=9 Then Goto 1840
01820   if m2-dfy<ficawage then goto L1880
01830   if (m2-dfy)-(m1-dfq)>ficawage then goto L1860
01840   let h2=ficawage-(m2-dfy-m1-dfq)
01850   goto L1890
01860 L1860: let h2=0
01870   goto L1890
01880 L1880: let h2=m1-dfq
01890 L1890: ! If EM6=1 OR EM6=9 Then Goto 1880
01900   if m2-dfy<mcm then let h3=m1-dfq : goto L1930
01910   if m2-dfy-m1-dfq>mcm then let h3=0 : goto L1930
01920   let h3=mcm-(m2-dfy-m1-dfq)
01930 L1930: ! 
01940   if frm=1 then goto L1980
01950   pr #255,using L1970: ss$,k$(1)(1:27),m1-dfq,h2,h3
01960   pr #255: ""
01970 L1970: form pos 1,c 11,pos 14,c 27,pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
01980 L1980: let t1=t1+m1-dfq
01990   let t5=t5+m1
02000   let t3=t3+h3
02010   let t2=t2+h2
02020   let p1=p1+2
02030 L2030: return 
02040 ! ______________________________________________________________________
02050 TOTALS: ! 
02060   if frm=1 then goto L2130
02070   pr #255,using L2080: "___________    ___________  ____________"
02080 L2080: form pos 41,c 41,skip 1
02090   pr #255: "       Total Employees:";p3;"     Totals";
02100   pr #255,using L2110: t1,t2,t3
02110 L2110: form pos 41,pic(----,---.##),pos 56,pic(----,---.##),pos 67,pic(---,---,---.##),skip 1
02120   let fncloseprn
02130 L2130: let p3=0
02140   let gt1=gt1+t5-dwq
02160   let gt2=gt2+t2
02170   let gt3=gt3+t3
02180   let gt4=gt4+t4
02190   let t1=t2=t3=t4=t5=0
02200   return 
02210 ! ______________________________________________________________________
02220 PGOF: ! 
02230   pr #255: newpage
02240   gosub WK_HEADER
02250   continue 
02260 ! ______________________________________________________________________
02270 SUMMARY: ! 
02280   let fnopenprn
02290   let eof=1: gosub WK_HEADER
02300   let wagefica=fna((gt2-m(32))*(ficarate+ficarate+.02)) ! FICARATE*2) ! pull tips out  2011
02310   let taxfica=fna(m(32)*(ficarate+ficarate+.02)) ! FICARATE*2)  2011
02320   let tipfica= fna((m(2)-m(32))*ficarate)
02330   let mcwh=fna((gt3)*mcr*2)
02340   pr #255: ""
02350   pr #255,using L2360: " 2.  Total wages and tips subject to withholding",gt1
02360 L2360: form pos 1,c 60,n 10.2
02370   pr #255,using L2360: " 3.  Total income tax withheld",tqm1
02380   pr #255: "" !:
        pr #255: ""
02390   pr #255,using L2400: " 6ab. Taxable social security wages paid",gt2-m(32),wagefica ! 6a. and 6b.
02400 L2400: form pos 1,c 40,n 10.2,x 10,n 10.2
02410   pr #255,using L2400: " 6cd. Taxable tips reported",tpt(20),taxfica ! 6c. and 6d.
02420   pr #255,using L2400: " 7ab. Taxable medicare insurance wages",gt3,mcwh ! 7a. and 7b.
02430   pr #255,using L2360: " 8.  Total FICA taxes",wagefica+taxfica+mcwh
02440   for j=1 to 5 !:
          pr #255: "" !:
        next j
02450   pr #255,using L2360: "11.  Total taxes excluding any adjustments you have made",tqm1+wagefica+taxfica+mcwh
02460   pr #255,using L2360: "12.  Advanced earned income credit",tqm14
02470   pr #255,using L2360: "13.  Net Taxes        ",(tqm1+wagefica+taxfica+mcwh)-tqm14
02480   for j=1 to 6 !:
          pr #255: "" !:
        next j
02490   return 
02500 ! ______________________________________________________________________
02510 GET_MAT_TPT: ! 
02520   open #19: "Name="&env$('Q')&"\GLmstr\PRTOT.H"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\PRTOTIDX.H"&env$('cno')&",Shr",internal,input,keyed 
02530 L2530: read #19,using L2540: mo,mat pt eof L2600
02540 L2540: form pos 1,n 2,pos 10,25*pd 5.2,n 4
02550   if qtr=1 and mo>0 and mo<4 then mat tpt=tpt+pt
02560   if qtr=2 and mo>3 and mo<7 then mat tpt=tpt+pt
02570   if qtr=3 and mo>6 and mo<10 then mat tpt=tpt+pt
02580   if qtr=4 and mo>10 then mat tpt=tpt+pt
02590   goto L2530
02600 L2600: close #19: 
02610   return 
02620 ! ______________________________________________________________________
02630 BUILD_941: ! 
02640   let wagefica=fna((gt2-tpt(20))*(ficarate+ficarate+.02)) ! FICARATE*2)  2011
02650   let taxfica=fna(tpt(20)*(ficarate+ficarate+.02)) ! FICARATE*2) 2011
02660   let tipfica= fna((tpt(20)-tpt(15))*ficarate)
02670   let mcwh=fna((gt3)*mcr*2)
02680 !   BOX1   ! count on 12th of each qtr (analyze history for this in another section)
02690   box2=gt1
02700   box3=tqm1
02710   box4=0 ! ( unused )
02720   box5a1=gt2-tpt(20)
02730   box5a2=wagefica
02740   box5b1=tpt(20)
02750   box5b2=taxfica
02760   box5c1=gt3
02770   box5c2=mcwh
02780   box5d=wagefica+taxfica+mcwh
02790   box6=box3+box5d ! total taxes before adj
02800   box7h=box7a+box7b+box7d+box7e+box7f+box7g ! total adjustments
02810   box8=box6-box7h ! total due after adjustments
02820   box9=tqm14 ! eic
02830   box10=box8-box9 ! total taxes after eic
02850   box12=box10-box11 ! blance due
02860   if box10-box11<0 then box13=abs(box10-box11) : box12=0 else box13=0 ! overpayment if any
02870   box15d=box15a+box15b+box15c ! total deposits for quarter
02880   return 
02890 ! ______________________________________________________________________
02900 LASER_941: ! 
02910   gosub OPEN_PRINTER
02920   gosub PRINT941
02930   gosub RELEASE_PRINT
02940   return 
02950 ! ______________________________________________________________________
02960 ! <Updateable Region: ERTN>
02970 ERTN: let fnerror(program$,err,line,act$,"xit")
02980   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02990   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
03000   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
03010 ERTN_EXEC_ACT: execute act$ : goto ERTN
03020 ! /region
03030 ! ______________________________________________________________________
03040 OPEN_PRINTER: ! 
03050   if file(20)=-1 then 
03060     open #20: "Name="&env$('Q')&"\GLmstr\Pr941"&wsid$&".txt,Replace,RecL=5000",display,output 
03070     pr #20: 'Call Print.MyOrientation("Portrait")'
03080   end if 
03090   return 
03100 ! ______________________________________________________________________
03110 PRINT941: ! 
03120   pr #20: 'Call Print.MyFontSize(12)'
03130   pr #20: 'Call Print.addPicture("S:\acsGL\941.bmp",1,1)'
03140   pr #20: 'Call Print.MyFont("Courier New")'
03150   for j=1 to 10
03160     let x=val(b$(1)(j:j)) conv L3180 ! pull any spaces or non-numeric characters out of federal id#
03170     goto L3190
03180 L3180: b$(1)(j:j)=""
03190 L3190: if b$(1)(j:j)=" " then b$(1)(j:j)=""
03200   next j
03210   pr #20: 'Call Print.MyFontSize(16)'
03220   let lyne=15 ! starting line of fed id
03230   pr #20: 'Call Print.AddText("'&b$(1)(1:1)&'",'&str$(47)&','&str$(lyne)&')'
03240   pr #20: 'Call Print.AddText("'&b$(1)(2:2)&'",'&str$(56)&','&str$(lyne)&')'
03250   pr #20: 'Call Print.AddText("'&b$(1)(3:3)&'",'&str$(70)&','&str$(lyne)&')'
03260   pr #20: 'Call Print.AddText("'&b$(1)(4:4)&'",'&str$(79)&','&str$(lyne)&')'
03270   pr #20: 'Call Print.AddText("'&b$(1)(5:5)&'",'&str$(88)&','&str$(lyne)&')'
03280   pr #20: 'Call Print.AddText("'&b$(1)(6:6)&'",'&str$(97)&','&str$(lyne)&')'
03290   pr #20: 'Call Print.AddText("'&b$(1)(7:7)&'",'&str$(106)&','&str$(lyne)&')'
03300   pr #20: 'Call Print.AddText("'&b$(1)(8:8)&'",'&str$(115)&','&str$(lyne)&')'
03310   pr #20: 'Call Print.AddText("'&b$(1)(9:9)&'",'&str$(124)&','&str$(lyne)&')'
03320   pr #20: 'Call Print.MyFontSize(12)'
03330   pr #20: 'Call Print.AddText("'&trim$(a$(1))&'",'&str$(35)&','&str$(32)&')'
03340   pr #20: 'Call Print.AddText("'&trim$(a$(2))&'",'&str$(35)&','&str$(39)&')'
03350   csz$=a$(3): gosub CSZ
03360   pr #20: 'Call Print.AddText("'&trim$(city$)&'",'&str$(35)&','&str$(48)&')'
03370   pr #20: 'Call Print.AddText("'&trim$(state$)&'",'&str$(93)&','&str$(48)&')'
03380   pr #20: 'Call Print.AddText("'&trim$(zip$)&'",'&str$(108)&','&str$(48)&')'
03390   if qtr=1 then let quarter=29 ! 1st quarter
03400   if qtr=2 then let quarter=35 ! 2nd quarter
03410   if qtr=3 then let quarter=41 ! 3rd quarter
03420   if qtr=4 then let quarter=47 ! 4rd quarter
03430   pr #20: 'Call Print.AddText("X",'&str$(143)&','&str$(quarter)&')'
03440   let tab1=63: let tab2=113: let tab3=160 ! WAS 62,112,159  ! 76,126,173 WORKED WHEN TO FAR RIGHT
03450   pr #20: 'Call Print.AddText("'&cnvrt$("pic(zzzzzzzzzzzzzz)",box1)&'",'&str$(tab3)&','&str$(72)&')'
03460   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box2)&'",'&str$(tab3)&','&str$(80)&')'
03470   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box3)&'",'&str$(tab3)&','&str$(87)&')'
03480 ! pr #20: 'Call Print.AddText("'&CNVRT$("pic(-,---,---.##)",BOX4)&'",'&STR$(TAB3)&','&STR$(90)&')'
03490   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5a1)&'",'&str$(tab1)&','&str$(108)&')'
03500   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5a2)&'",'&str$(tab2)&','&str$(108)&')'
03510   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5b1)&'",'&str$(tab1)&','&str$(116)&')'
03520   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5b2)&'",'&str$(tab2)&','&str$(116)&')'
03530   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5c1)&'",'&str$(tab1)&','&str$(123)&')'
03540   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5c2)&'",'&str$(tab2)&','&str$(123)&')'
03550   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box5d)&'",'&str$(tab3)&','&str$(131)&')'
03560   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box6)&'",'&str$(tab3)&','&str$(138)&')'
03570   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7a)&'",'&str$(tab2)&','&str$(150)&')'
03580   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7b)&'",'&str$(tab2)&','&str$(158)&')'
03590   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7c)&'",'&str$(tab2)&','&str$(165)&')'
03600   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7d)&'",'&str$(tab2)&','&str$(172)&')'
03610   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7e)&'",'&str$(tab2)&','&str$(180)&')'
03620   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7f)&'",'&str$(tab2)&','&str$(187)&')'
03630   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7g)&'",'&str$(tab2)&','&str$(194)&')'
03640   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box7h)&'",'&str$(tab3)&','&str$(202)&')'
03650   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box8)&'",'&str$(tab3)&','&str$(209)&')'
03660   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box9)&'",'&str$(tab3)&','&str$(216)&')'
03670   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box10)&'",'&str$(tab3)&','&str$(224)&')'
03680   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box11)&'",'&str$(tab3)&','&str$(232)&')'
03690   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box12)&'",'&str$(tab3)&','&str$(238)&')'
03700   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box13)&'",'&str$(tab2)&','&str$(247)&')'
03710   let fnpa_newpage 
03720   pr #20: 'Call Print.addPicture("S:\acsGL\941-back.bmp",1,1)'
03730   pr #20: 'Call Print.AddText("'&trim$(state$(1:1))&'",'&str$(15)&','&str$(33)&')'
03740   pr #20: 'Call Print.AddText("'&trim$(state$(2:2))&'",'&str$(21)&','&str$(33)&')'
03750   let tab4=75
03760   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15a)&'",'&str$(tab4)&','&str$(61)&')'
03770   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15b)&'",'&str$(tab4)&','&str$(68)&')'
03780   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15c)&'",'&str$(tab4)&','&str$(74)&')'
03790   pr #20: 'Call Print.AddText("'&cnvrt$("pic(-,---,---.##)",box15a+box15b+box15c)&'",'&str$(tab4)&','&str$(82)&')'
03800   return 
03810 RELEASE_PRINT: ! 
03820   close #2: ioerr L3830
03830 L3830: close #3: ioerr L3840
03840 L3840: ! 
03850   let fnpa_finis
03870   return 
03880 CSZ: ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
03890 L3890: let p1=pos(csz$,".",1)
03900   if p1>0 then csz$(p1:p1)=" ": let p2=p1: goto L3890
03910 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
03920   let p2=0
03930 L3930: let p1=pos(csz$,",",1)
03940   if p1>0 then csz$(p1:p1)=" ": let p2=p1: goto L3930
03950 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN cSZ$(P2+1:P2+1)="" ! dump any extra spaces
03960 L3960: let p1=pos(rtrm$(csz$),"  ",1)
03970   if p1>0 then csz$(p1+1:p1+1)="" : goto L3960
03980   csz$=ltrm$(rtrm$(csz$)): let p1=pos(csz$," ",-1)
03990   let x$=csz$(p1+1:len(csz$)): let zip$=ltrm$(rtrm$(zip$))
03992   let zip$=x$(1:5)
04000   let p2=pos(csz$(1:p1-1)," ",-1) : let state$=csz$(p2+1:p1-1)(1:2) : let state$=ltrm$(rtrm$(state$))
04010   city$=csz$(1:p2-1)(1:15): city$=ltrm$(rtrm$(city$))
04020   return 
