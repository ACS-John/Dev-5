00010 ! Replace S:\acsPR\newprRpt3
00020 ! User Designed Reports
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnDedNames,fnerror,fntos,fnlbl,fncmdkey,fnacs,fncombof,fnmsgbox,fnchk,fntxt,fnfra,fncomboa,fnindex_it,fngethandle,fnstatus_close,fnprint_designed_report
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rt$*78,ch$(2)*132,psc(100)
00080   dim inp(20),pp(20),ti(20),rt40$*40
00090   dim rptn$*2,cap$*128,resp$(100)*150
00110   dim ml$(4)*128
00120 ! ______________________________________________________________________
00130   fntop(program$,cap$="Design Reports")
00150   fnindex_it(env$('Q')&"\PRmstr\PRReport.h"&env$('cno'),env$('Q')&"\PRmstr\PRRptIdx.h"&env$('cno'),"1 2")
00152   fnstatus_close
00160   open #h_prreport:=1: "Name="&env$('Q')&"\PRmstr\PRReport.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\PRRptIdx.h"&env$('cno')&",RecL=1049,KLn=2,KPs=1,Use,Shr",internal,outin,keyed 
00162 F_PRREPORT: form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
00163 ! 
00180 ! ______________________________________________________________________
00190   gosub DATANAMES
00200 SCR1: ! 
00210 ASKREPORT: ! 
00220   fntos(sn$="Report-ask")
00222   let respc=0
00230   fnlbl(1,1,"Report:",11,1)
00240   fncombof("Report",1,14,43,env$('Q')&"\PRmstr\prreport.h"&env$('cno'),1,2,3,30,env$('Q')&"\PRmstr\prrptidx.h"&env$('cno'),1+addall,0,"Select from the list of reports. To add a report, click the Add button.",container)
00242   let resp$(respc+=1)=""
00250   fncmdkey("&Add",1,0,0,"Add a new employee" )
00252   fncmdkey("E&dit",2,0,0,"Modify the selected report")
00254   fncmdkey("Print",4,1,0,"Run the selected report")
00258   fncmdkey("E&xit",5,0,1,"Return to menu")
00260   fnacs(sn$,0,mat resp$,ckey) ! ask report #
00270   if ckey=5 then goto XIT
00280   editrec=addone=0
00292   let rptn=val(resp$(1)(1:2))
00294   if ckey=1 then 
00296     addone=1
00298     let rptn=0
00302     goto CONTINUE_FOR_ADD_AND_EDIT
00304   else if ckey=2 then 
00306     editrec=1
00310     goto CONTINUE_FOR_ADD_AND_EDIT
00312   else if ckey=4 then 
00320     fnprint_designed_report(rptn)
00328   end if 
00330   goto ASKREPORT
00340 CONTINUE_FOR_ADD_AND_EDIT: ! 
00350   let rptn$=lpad$(str$(rptn),2)
00360   if addone=1 then 
00362     read #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti nokey SCR2
00370   else if editrec=1 then 
00372     read #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti nokey EDIT_READ_NOKEY
00374   end if 
00380   goto L400
00384 EDIT_READ_NOKEY: ! r:
00386   mat ml$(2)
00388   let ml$(1)="A record with this number does not exist!"
00390   let ml$(2)="Select a different numbe if you wish to add a new report."
00392   fnmsgbox(mat ml$,resp$,cap$,48)
00394   goto ASKREPORT ! /r
00400 L400: ! 
00402   let holdrn=rn
00410 ! 
00412 SCR2: ! 
00414   if addone=1 then ! r: initialize variables to 0 or blank
00416     let rn=0
00418     let rt$=""
00420     mat ch$=("")
00422 !   mat tempch$=("")
00424     let ips=0
00426     sd$=""
00428     sd=cp=0
00430     mat psc=(0)
00432     mat pp=(0)
00434     mat ti=(0)
00436     let holdrn=0
00438   end if  ! /r
00440   fntos(sn$="Report-add")
00442   let respc=0: let mylen=15: let mypos=mylen+3
00450   fnlbl(1,1,"Report #:",mylen,1)
00460   fntxt(1,mypos,2,2,0,"30",0,"")
00462   let resp$(respc+=1)=str$(rn)
00470   fnlbl(2,1,"Report Title:",mylen,1)
00480   fntxt(2,mypos,78,0,0,"",0,"")
00482   let resp$(respc+=1)=rt$
00490   fnlbl(3,1,"Column Headings:",mylen,1)
00500   fnlbl(4,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
00510   fntxt(5,1,132,0,0,"",0,"The heading can be two lines.  This will be the 1st line.")
00512   let resp$(respc+=1)=ch$(1)
00520   fnlbl(6,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
00530   fntxt(7,1,132,0,0,"",0,"This is the 2nd line of the heading line.")
00532   let resp$(respc+=1)=ch$(2)
00540   let mylen=50
00550   fnlbl(12,1,"Item for pr Selection (blank for all):",mylen,1)
00560   if ips>0 and ips=<udim(code$) then let resp$(respc+=1)=code$(ips+1) else let resp$(respc+=1)=""
00570   fncomboa("DataNames2",12,mylen+3,mat code$,"If you want limit the report to a value in a particular field in the employee record, Indicate which field it is by locatiing the ID number of the field using Help button.",25,0)
00580 !  fnTXT(12,MYLEN+3,3,0,0,"30",0,"If you want limit the report to a value in a particular field in the employee record, Indicate which field it is by locatiing the ID number of the field using Help button.")
00582 ! Let RESP$(RESPC+=1)=STR$(IPS)
00590   fnchk(13,mylen+3,"Summarize Departmental Records:",1)
00600   if sd= 1 then let resp$(respc+=1)="TRUE" else let resp$(respc+=1)="FALSE"
00610   fncmdkey("&Next",1,1,0,"Save changes and move to next questions" )
00612   fncmdkey("&Delete",4,0,0,"Deletes this report from your system.")
00614   fncmdkey("&Cancel",5,0,1,"Return to selection screen.")
00620   fnacs(sn$,0,mat resp$,ckey) ! ask report #
00630   addone=0
00640   if ckey=5 then goto SCR1
00650   let rn=val(resp$(1)(1:2))
00660   if holdrn>0 and rn<>holdrn then 
00664 ! r: confirm_key_change
00666     mat ml$(3)
00668     let ml$(1)="You are attempting to change report number"
00670     let ml$(2)="from "&str$(holdrn)& " to "&str$(rn)&"."
00672     let ml$(3)="Take OK to continue, else Cancel."
00674     fnmsgbox(mat ml$,resp$,cap$,49)
00676     if resp$="OK" then 
00678       let holdrn=rn
00682     else 
00684       goto SCR2
00686     end if  ! /r
00690   end if 
00692   let rt40$=resp$(2)(1:40)
00700   ch$(1)=resp$(3)
00710   ch$(2)=resp$(4)
00720   let ips=0
00730   for j=1 to udim(code$)
00740     if resp$(5)=code$(j) then let ips=j-1: goto L760
00750   next j
00760 L760: ! 
00762   if resp$(6)(1:1)="T" then sd$="Y": sd=1 else sd$="N": sd=0
00770   if ips<0 or ips>126 or (ips>1 and ips<6) then 
00772     mat ml$(2)
00774     let ml$(1)="You can not use "&code$(ips+1)&" as selection criteria!"
00776     let ml$(2)=" Take OK to select a different item."
00778     fnmsgbox(mat ml$,resp$,cap$,48)
00780     goto SCR2
00782   end if 
00800   if sd$="Y" then sd=1 else sd=0
00810   let rt$=rt40$
00820   if ckey=4 then 
00822     goto DELETEIT
00824 DELETEIT: ! 
00826     mat ml$(2)
00828     let ml$(1)="You have chosen to delete report # "&rptn$
00830     let ml$(2)="Take Ok to continue, else Cancel to keep the report."
00832     fnmsgbox(mat ml$,resp$,cap$,49)
00834     if resp$="OK" then 
00836       delete #h_prreport,key=rptn$: 
00838     else 
00840       goto SCR1
00842     end if 
00844   end if 
00890 ! 
00900 ! SCR3: !
00910 ! If RN=0 Then Goto DONE  ! ain't this just redundant?
00920   if ips=0 then goto SCR4
00930   fntos(sn$="Report-sel")
00932   let respc=0: let mylen=15: let mypos=mylen+3
00940   fnlbl(1,1,"Print Selection Criteria:",30,1)
00950   let z=0
00960   for x=1 to 5
00970     for j=2 to 21
00980       fntxt(j,x*16,12,0,0,"33",0,"If you chosen to limit the report to certain criteria, enter the values here that should match information in the employee's record.")
00982       let resp$(respc+=1)=str$(psc(z+=1))
00990     next j
01000   next x
01010   fncmdkey("&Next",1,1,0,"Save changes and move to next questions" )
01012   fncmdkey("&Back",6,0,0,"Back up a screen.")
01014   fncmdkey("&Cancel",5,0,1,"Return to selection screen.")
01020   fnacs(sn$,0,mat resp$,ckey) ! ask matching criteria
01030   if ckey=5 then goto SCR1
01040   for j=1 to 100
01050     let psc(j)=val(resp$(j))
01060   next j
01070   if ckey=6 then goto SCR2
01080   goto SCR4
01090 ! ______________________________________________________________________
01100 SCR4: ! 
01110   fntos(sn$="Report-scr4") !:
        let respc=0: let mylen=15: let mypos=mylen+3
01120   fnfra(1,1,23,90,"Selection of Column Information","Select the informatin that should be printed in each column of your report.")
01130   fnlbl(1,1,"Print       Want ",50,1,0,1)
01140   fnlbl(2,1,"Item to pr                 Position    A Total",52,1,0,1)
01150   for j=1 to 20
01160     if inp(j)>0 and inp(j)=<udim(code$) then let resp$(respc+=1)=code$(inp(j)+1) else let resp$(respc+=1)=" "
01170 ! If INP(J)>0 AND INP(J)=<UDIM(CODE$) Then Let RESP$(RESPC+=1)=CODE$(INP(J)) Else Let RESP$(RESPC+=1)=" "
01180     fncomboa("DataNames",j+2,1,mat code$,"",25,1)
01190     fntxt(j+2,37,3,0,0,"30",0,"The position is the starting position acress the page where this column should print.",1) !:
          let resp$(respc+=1)=str$(pp(j))
01200     if ti(j)=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
01210     fnchk(j+2,48,"",1,1) ! total the column
01220   next j
01230   fncmdkey("&Next",1,1,0,"Save changes on this report design." ) !:
        !:
        fncmdkey("&Back",6,0,0,"Back up a screen.") !:
        fncmdkey("&Cancel",5,0,1,"Return to report selection screen without saving any changes.")
01240   fnacs(sn$,0,mat resp$,ckey) ! enter column information
01250   if ckey=5 then goto SCR1
01260   let x=0
01270   for j=3 to 60 step 3
01280     let x+=1
01290     for j1=1 to udim(code$)
01300       if resp$(j-2)=code$(j1) then let inp(x)=j1-1: goto L1330
01310 ! If RESP$(J-2)=CODE$(J1) Then Let INP(X)=J1+1: Goto 1310
01320     next j1
01330 L1330: let pp(x)=val(resp$(j-1))
01340     if resp$(j)="True" then let ti(x)=1 else let ti(x)=0
01350   next j
01360   if rptn=rn then goto L1370 else goto L1390
01370 L1370: rewrite #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
01380   goto L1460
01390 L1390: read #h_prreport,using L1400,key=lpad$(str$(rn),2): rn nokey CHANGETHENUMBER
01400 L1400: form pos 1,n 2
01410   goto SCR1
01420 ! ______________________________________________________________________
01430 CHANGETHENUMBER: ! 
01440   write #h_prreport,using F_PRREPORT: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
01450   delete #h_prreport,key=rptn$: nokey ignore
01460 L1460: ! 
01480   goto SCR1
01490 ! ______________________________________________________________________
01500 XIT: close #h_prreport: ioerr ignore
01510   fnxit
01520 ! ______________________________________________________________________
01530 ! L1530: pr newpage ! r:
01532 !   dim io5$(2),wrd5$(2)*38
01540 !   close #105: ioerr ignore
01550 !   open #105: "SRow=8,Scol=20,ERow=15,ECol=59,Border=Sr,Caption=<"&cap$,display,outin
01560 !   pr #105: newpage
01580 !   pr #105,fields "2,1,Cc 40,R,N": "Company Number "&env$('cno')
01590 !   pr #105,fields "4,2,Cc 38,N": "Report Number "&str$(rptn)&" does not exist."
01600 !   let io5$(1)="6,2,Cc 38,N"
01610 !   let io5$(2)="7,2,Cc 38,N"
01620 !   let wrd5$(1)="Reselect Report Number"
01630 !   let wrd5$(2)="Add Reports"
01640 !   pr f "16,35,C 09,B,5": "Exit (F5)"
01650 !   rinput #105,select mat io5$,attr "H": mat wrd5$
01660 !   if cmdkey=5 or cmdkey=99 then goto XIT
01670 !   if curfld=2 then goto L1770 else goto SCR1 ! /r
01680 ! ______________________________________________________________________
01690 ! <Updateable Region: ERTN>
01700 ERTN: let fnerror(program$,err,line,act$,"xit")
01710   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01720   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01740 ERTN_EXEC_ACT: execute act$ : goto ERTN
01750 ! /region
01760 IGNORE: continue 
01770 ! L1770: ! r: ADD NEW RECORD
01780 !   let rt$="": mat ch$=("")
01790 !   let ips=sd=cp=0
01800 !   mat psc=(0): mat inp=(0): mat pp=(0): mat ti=(0)
01810 !   write #h_prreport,using F_PRREPORT: rptn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti
01820 !   let rptn$=lpad$(str$(rptn),2)
01830 !   read #h_prreport,using F_PRREPORT,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,mat psc,mat inp,mat pp,mat ti ! nokey L1530
01840 !   goto L400 ! /r
62000   def fn_add_dn(dn_x_1$*30,dn_x_2$*30,dn_x_3$*30)
62020     let dn_counter+=1
62040     let datanames$(dn_counter,1)=dn_x_1$
62060     let datanames$(dn_counter,2)=dn_x_2$
62080     let datanames$(dn_counter,3)=dn_x_3$
62100   fnend 
64000 DATANAMES: ! r:
64020   dim datanames$(104,3)*30
64040   let dn_counter=0
64060   fn_add_dn('Employee Number','eno','N 8')
64080   fn_add_dn('Name','EM$(1)','C 30')
64100   fn_add_dn('Address','EM$(2)','C 30')
64120   fn_add_dn('City State Zip Code','EM$(3)','C 30')
64140   fn_add_dn('Social Security #','SS$','C 11')
64160   fn_add_dn('Race','RS(1)','N 1')
64180   fn_add_dn('Sex','RS(2)','N 1')
64200   fn_add_dn('Marital Status','EM(1)','N 2')
64220   fn_add_dn('Federal Exemptions','EM(2)','N 2')
64240   fn_add_dn('State Exemptions','EM(3)','N 2')
64260   fn_add_dn('Employment Status','EM(4)','N 2')
64280   fn_add_dn('Pay Code','EM(5)','N 2')
64300   fn_add_dn('Fica Code','EM(6)','N 2')
64320   fn_add_dn('Eic Code','EM(7)','N 2')
64340   fn_add_dn('Sick Pay Code','EM(8)','PD 3.3')
64360   fn_add_dn('Vacation Pay Code','EM(9)','PD 3.3')
64380   fn_add_dn('Sick Hours Accrued','EM(10)','PD 4.2')
64400   fn_add_dn('Vacation Hours Accrued','EM(11)','PD 4.2')
64420   fn_add_dn('Standard Federal W/H','EM(12)','PD 4.2')
64440   fn_add_dn('Federal Tax Add-Om','EM(13)','PD 4.2')
64460   fn_add_dn('Standard State W/H','EM(14)','PD 4.2')
64480   fn_add_dn('State Tax Add-On','EM(15)','PD 4.2')
64500   fn_add_dn('Date Hired','EM(16)','N 6')
64520   fn_add_dn('Last Payroll Date','EM(17)','N 6')
64540   fn_add_dn('Telephone Number','PH$','C 12')
64560   fn_add_dn('Birth Date','BD','N 6')
64580   fn_add_dn('Department Number','TDN','N 3')
64600   fn_add_dn('G/L Account #','GL$','C 12')
64620   fn_add_dn('Last Review Date','TDT(1)','N 6')
64640   fn_add_dn('Next Review Date','TDT(2)','N 6')
64660   fn_add_dn('Last Increase Date','TDT(3)','N 6')
64680   fn_add_dn('Last Payroll Date','TDT(4)','N 6')
64700   fn_add_dn('State Code','TCD(1)','N 2')
64720   fn_add_dn('Worknams Comp Code','TCD(2)','N 2')
64740   fn_add_dn('Union Code','TCD(3)','N 2')
64760   fn_add_dn('Amount of Last Increase','tli','pd 4.2')
64780   fn_add_dn('Salary','TDET(1)','PD 4.2')
64800   fn_add_dn('Regular Hourly Rate','TDET(2)','PD 4.2')
64820   fn_add_dn('Overtime Hourly Rate','TDET(3)','PD 4.2')
64840   fn_add_dn('Misc - 1','TDET(4)','PD 4.2')
64860   fn_add_dn('Misc - 2','TDET(5)','PD 4.2')
64880   fn_add_dn('Misc - 3','TDET(6)','PD 4.2')
64900   fn_add_dn('Misc - 4','TDET(7)','PD 4.2')
64920   fn_add_dn('Misc - 5','TDET(8)','PD 4.2')
64940   fn_add_dn('Misc - 6','TDET(9)','PD 4.2')
64960   fn_add_dn('Misc - 7','TDET(10)','PD 4.2')
64980   fn_add_dn('Misc - 8','TDET(11)','PD 4.2')
65000   fn_add_dn('Misc - 9','TDET(12)','PD 4.2')
65020   fn_add_dn('Misc - 10','TDET(13)','PD 4.2')
65040   fn_add_dn('Misc - 11','TDET(14)','PD 4.2')
65060   fn_add_dn('Misc - 12','TDET(15)','PD 4.2')
65080   fn_add_dn('Misc - 13','tdet(16)','PD 4.2')
65100   fn_add_dn('Misc - 14','tdet(17)','PD 4.2')
65120   fn_add_dn('Misc - 15','tdet(18)','PD 4.2')
65140   fn_add_dn('Misc - 16','tdet(19)','PD 4.2')
65160   fn_add_dn('Misc - 17','tdet(20)','PD 4.2')
65180   fn_add_dn('Misc - 18','tdet(21)','PD 4.2')
65200   fn_add_dn('Misc - 19','tdet(22)','PD 4.2')
65220   fn_add_dn('Misc - 20','tdet(23)','PD 4.2')
65240   fn_add_dn('Department Number','TDN','N 3')
65260   fn_add_dn('Payroll Date','prd','pd 6')
65280   fn_add_dn('Check Number','ckno','N 7')
65300   fn_add_dn('Regular Hours','tdc(1)','PD 3.2')
65320   fn_add_dn('Overtime Hours','tdc(2)','Pd 3.2')
65340   fn_add_dn('Sick Hours','tdc(3)','PD 3.2')
65360   fn_add_dn('Vacation Hours','tdc(4)','PD 3.2')
65380   fn_add_dn('Holiday Hours','tdc(5)','PD 3.2')
65400   fn_add_dn('Workmans Comp Wage''s','tdc(6)','Pd 5.2')
65420   fn_add_dn('SS Wages','tdc(7)','PD 5.2')
65440   fn_add_dn('Medicare Wages','tdc(8)','PD 5.2')
65460   fn_add_dn('Federal U/C Wage','tdc(9)','PD 5.2')
65480   fn_add_dn('State U/c Wage','tdc(10)','PD 5.2')
65500   fn_add_dn('Federal Withholdings','tcp(1)','PD 5.2')
65520   fn_add_dn('SS Withholdings','tcp(2)','PD 5.2')
65540   fn_add_dn('Medicare Withholdings','tcp(3)','PD 5.2')
65560   fn_add_dn('State Withholdings','tcp(4)','PD 5.2')
65580   fn_add_dn('Misc - 1','tcp(5)','PD 5.2')
65600   fn_add_dn('Misc - 2','tcp(6)','pd 5.2')
65620   fn_add_dn('Misc - 3','tcp(7)','pd 5.2')
65640   fn_add_dn('Misc - 4','tcp(8)','pd 5.2')
65660   fn_add_dn('Misc - 5','tcp(9)','pd 5.2')
65680   fn_add_dn('Misc - 6','tcp(10)','pd 5.2')
65700   fn_add_dn('Misc - 7 ','tcp(11)','pd 5.2')
65720   fn_add_dn('Misc - 8 ','tcp(12)','pd 5.2')
65740   fn_add_dn('Misc - 9 ','tcp(13)','pd 5.2')
65760   fn_add_dn('Misc - 10','tcp(14)','pd 5.2')
65780   fn_add_dn('Misc - 11','tcp(15)','pd 5.2')
65800   fn_add_dn('Misc - 12','tcp(16)','pd 5.2')
65820   fn_add_dn('Misc - 13','tcp(17)','pd 5.2')
65840   fn_add_dn('Misc - 14','tcp(18)','pd 5.2')
65860   fn_add_dn('Misc - 15','tcp(19)','pd 5.2')
65880   fn_add_dn('Misc - 16','tcp(20)','pd 5.2')
65900   fn_add_dn('Misc - 17','tcp(21)','pd 5.2')
65920   fn_add_dn('Misc - 18','tcp(22)','pd 5.2')
65940   fn_add_dn('Misc - 19','tcp(23)','pd 5.2')
65960   fn_add_dn('Misc - 20','tcp(24)','pd 5.2')
65980   fn_add_dn('Eic','tcp(25)','pd 5.2')
66000   fn_add_dn('Regular Earnings','tcp(26)','pd 5.2')
66020   fn_add_dn('OT Earnings','tcp(27)','pd 5.2')
66040   fn_add_dn('Other Compensation','tcp(28)','pd 5.2')
66060   fn_add_dn('Meals','tcp(29)','pd 5.2')
66080   fn_add_dn('Tips','tcp(30)','pd 5.2')
66100   fn_add_dn('Total Wage','tcp(31)','pd 5.2')
66120   fn_add_dn('Net Pay','tcp(32)','pd 5.2')
66140 ! 
66160   dim fullname$(20)*20
66180   fnDedNames(mat fullname$)
66240 ! 
66260   dim code$(105)*30
66280   code$(1)=""
66300   for j=1 to udim(datanames$)
66320     code$(j+1)=datanames$(j,1)
66340     if j>39 and j<60 and trim$(fullname$(j-39))<>"" then 
66360       code$(j+1)=trim$(fullname$(j-39))(1:22)
66380     else if j>39 and j<60 and trim$(fullname$(j-39))="" then 
66400       code$(j+1)="Misc -"&str$(j-39)&"-Std Ded"
66420     else if j>76 and j<95 and trim$(fullname$(j-76))<>"" then ! miscellaneous withholding amounts
66440       code$(j+1)=trim$(fullname$(j-76))(1:27)&"-Wh"
66460     else if j>76 and j<94 and trim$(fullname$(j-76))="" then ! plug names if none
66480       code$(j+1)="Misc - "&str$(j-76)
66500     end if 
66520   next j
66540   return  ! /r
