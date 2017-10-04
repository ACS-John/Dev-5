00010 ! Replace S:\acsPR\newJCRptFM
00020 ! Job Cost User-Designed Report File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnopenprn,fncloseprn,fnerror,fnchain,fnrx,fntos,fncombof,fncmdset,fnacs,fncmdkey,fnlbl,fntxt,fnchk,fnmsgbox,fncomboa,fnfra,fnDedNames
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,message$*40,msgline$(2)*60,resp$(102)*150
00080   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,iom$(3),scm$(3)*40
00090   dim io1$(10),io2$(7),fd$(20),rptemp(20),tempch$(4)*256,rptn$*6,rnew$*6
00100   dim rt$*51,ch$(2)*132,psc(100),f$(20)*50,pp(20),ppr(20),dp(20),fc(20)
00110   dim tcj(20),tcs(20),rno$(50)*2,em$*40,wrd3$(2)*23,io3$(2),code1$(4)*30
00120   dim rt40$*51
00130   dim abbrevanme$(20)*8,fullname$(20)*20,ml$(3)*90,ty$(24)*50,ty2$(25)*30
00140 ! ______________________________________________________________________
00150   fntop(program$,cap$="User Designed Reports - JC")
00170   let pg=3
00180 ! ______________________________________________________________________
00190   open #1: "Name=S:\acsPR\JCREPORT.MST,KFName=S:\acsPR\JCREPORT.idx,Shr",internal,outin,keyed 
00210   fnDedNames(mat fullname$,mat abbrevname$)
00220 ! ______________________________________________________________________
00230   gosub DATA_FOR_COLUMNS
00240 SCR1: ! ask report #
00250 ASKREPORT: ! 
00260   if rn=0 then let rn=1
00270   fntos(sn$="jcReport-ask") !:
        let respc=0
00280   fnlbl(1,1,"Report #:",11,right)
00290   let df$="S:\acsPR\Jcreport.mst" : let if$="S:\acsPR\jcreport.idx" !:
        fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1,0,"Select from the list of reports. To add a report, click the Add button.")
00300   let resp$(1)=str$(rn)
00310   fncmdkey("&Add",1,0,0,"Add a new report" ) !:
        fncmdkey("E&dit",2,1,0,"Access the highlited record") !:
        fncmdkey("&Next Record",3,0,0,"Access next record in report # order") !:
        fncmdkey("E&xit",5,0,1,"Returns to menu")
00320   fnacs(sn$,0,mat resp$,ckey) ! ask report #
00330   if ckey=5 then goto XIT
00340   editrec=addone=0
00350   let hreport$=resp$(1)(1:2) !:
        let rptn=val(resp$(1)(1:2)) !:
        let rptn$=lpad$(str$(rptn),2)
00360   if ckey=1 then addone=1: let rptn=0: goto ADD_EDIT
00370   if ckey=2 then editrec=1: goto ADD_EDIT
00380 RECREATE_GRID: ! 
00390 ! ______________________________________________________________________
00400 ADD_EDIT: ! 
00410   let rptn$=lpad$(str$(rptn),2)
00420   if addone=1 then let rn=0: let rt$="": mat ch$=(""): mat tempch$=(""): let ips=0: sd$="": sd=cp=0: mat psc=(0): mat pp=(0) : mat ti=(0): let holdrn=0 : goto SCR2
00430   if editrec=1 then read #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L460
00440   goto L470
00450 ! 
00460 L460: mat ml$(2) !:
        let ml$(1)="A record with this number does not exist!" !:
        let ml$(2)="Select a different numbe if you wish to add a new report." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto ASKREPORT
00470 L470: let holdrn=rn
00480 ! ______________________________________________________________________
00490 SCR2: ! add/edit first screen of report
00500   fntos(sn$="Report-add") !:
        let respc=0: let mylen=15: let mypos=mylen+3
00510   fnlbl(1,1,"Report #:",mylen,1)
00520   fntxt(1,mypos,2,2,0,"30",0,"") !:
        let resp$(respc+=1)=str$(rn)
00530   fnlbl(2,1,"Report Title:",mylen,1)
00540   fntxt(2,mypos,78,78,0,"",0,"") !:
        let resp$(respc+=1)=rt$
00550 ! 
00560   fnlbl(3,1,"Column Headings:",mylen,1)
00570   fnlbl(4,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
00580   fntxt(5,1,132,132,0,"",0,"The heading can be two lines.  This will be the 1st line.") !:
        let resp$(respc+=1)=ch$(1)
00590   fnlbl(6,7,"1    2    3    4    5    6    7    8    9    0    1    2    3 ",132,0)
00600   fntxt(7,1,132,0,0,"",0,"This is the 2nd line of the heading line.") !:
        let resp$(respc+=1)=ch$(2)
00610   let mylen=50
00620   fnlbl(12,1,"Item for pr Selection (blank for all):",mylen,1)
00630   if ips>0 and ips=<udim(ty2$) then let resp$(respc+=1)=ty2$(ips+1) else let resp$(respc+=1)=""
00640   fncomboa("DataNames2",12,mylen+3,mat ty2$,"If you want limit the report to a value in a particular field in the report record, Indicate which field it is by locating the ID number.",25,0)
00650   let resp$(respc+=1)=str$(psc)
00660   fnchk(13,mylen+3,"Summarize Category Records:",1)
00670   if sd= 1 then let resp$(respc+=1)="TRUE" else let resp$(respc+=1)="FALSE"
00680   fnlbl(14,1,"Selection Codes:",mylen,1)
00690   code1$(1)="1 - Equal to" !:
        code1$(2)="2 - Equal to or greater than" !:
        code1$(3)="3 - Equal to or less than" !:
        code1$(4)="4 - Range of numbers" !:
        let respc+=1: for j=1 to udim(code1$) !:
          if sc=val(code1$(j)(1:1)) then let resp$(respc)=code1$(j)(1:1) !:
          next j
00700   fncomboa("selCode",14,mylen+3,mat code1$,"",30)
00710   fncmdkey("&Next",1,1,0,"Save changes and move to next questions" ) !:
        fncmdkey("&Delete",4,0,0,"Deletes this report from your system.") !:
        fncmdkey("&Cancel",5,0,1,"Return to selection screen.")
00720   fnacs(sn$,0,mat resp$,ckey) ! edit first screen for report format
00730   addone=0
00740   if ckey=5 then goto SCR1
00745   if ckey=4 then goto DELETEIT : goto SCR3
00750   let rn=val(resp$(1)(1:2))
00760   if holdrn>0 and rn<>holdrn then goto L770 else goto L790
00770 L770: mat ml$(3) !:
        let ml$(1)="You are attempting to change report " !:
        let ml$(2)="# "&str$(holdrn)& " to report # "&str$(rn)&"." !:
        let ml$(3)="Take OK to continue, else Cancel." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00780   if resp$="OK" then let holdrn=rn: goto L790 else goto SCR2
00790 L790: let rt40$=resp$(2)
00800   ch$(1)=resp$(3)
00810   ch$(2)=resp$(4)
00820   let ips=0
00830   for j=1 to udim(ty2$)
00840     if resp$(5)=trim$(ty2$(j)) then let ips=val(ty2$(j)(1:2)): goto L860
00850   next j
00860 L860: if resp$(6)(1:1)="T" then sd$="Y": sd=1 else sd$="N": sd=0
00870   if ips>1 and (ps>1 and ps<5) then goto L880 else goto L890 ! can't use name fields fro selection criteria
00880 L880: mat ml$(2) !:
        let ml$(1)="You can not use "&trim$(ty2$(ips+1))&" as selection criteria!" !:
        let ml$(2)=" Take OK to select a different item." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto SCR2
00890 L890: if sd$="Y" then sd=1 else sd=0
00900   let rt$=rt40$
00910   if ckey=1 then rewrite #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
00920   if ips>0 then goto SCR5 else : goto SCR3 ! ask criteris for pr selection
00930   goto SCR3
00940 ! ______________________________________________________________________
00950 DELETEIT: ! 
00960   mat ml$(2) !:
        let ml$(1)="You have chosen to delete report # "&rptn$ !:
        let ml$(2)="Take Ok to continue, else Cancel to keep the report." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
00970   if resp$="OK" then goto L980 else goto L990
00980 L980: delete #1,key=rptn$: 
00990 L990: goto SCR1
01000 SCR3: ! ask column # to edit
01010   if column=0 then column=1
01020   fntos(sn$="ask-column") !:
        let respc=0: let mylen=15: let mypos=mylen+3
01030   fnlbl(1,1,"Column #:",mylen,1)
01040   fntxt(1,mypos,2,2,0,"30",0,"Column numbers must be from 1 to 20,") !:
        let resp$(respc+=1)=str$(column)
01050   fncmdset(2)
01060   fnacs(sn$,0,mat resp$,ckey) ! acs column
01070   if ckey=5 then goto SCR1
01080   column=val(resp$(1))
01090   if column=0 then goto SCR1
01100   if column<0 or column>20 then goto SCR2
01110   goto SCR4 ! allow to edit
01120 SCR4: ! edit/columns
01130   fntos(sn$="columns") !:
        let mylen=25 : let mypos=mylen+2: let respc=0: left=1
01140   fnlbl(1,1,"Report #:  "&str$(rn),mylen,left)
01150   fnlbl(2,1,"Column #:  "&str$(column),mylen,left)
01160   fnlbl(3,1,"Formula for printing:",mylen,left)
01170   fntxt(3,mypos,50,50,0,"",0,"See instructions for creating the formula for the information that is to pr in this column.") !:
        let resp$(respc+=1)=f$(column)
01180   fnlbl(4,1,"Starting Position:",mylen,left)
01190   fntxt(4,mypos,3,3,0,"30",0,"") !:
        let resp$(respc+=1)=str$(pp(column))
01200   fnlbl(5,1,"Field Size:",mylen,left)
01210   fntxt(5,mypos,3,3,0,"30",0,"") !:
        let resp$(respc+=1)=str$(ppr(column))
01220   fnlbl(6,1,"Decimal Positions:",mylen,left)
01230   fntxt(6,mypos,1,1,0,"30",0,"") !:
        let resp$(respc+=1)=str$(dp(column))
01240   fnchk(7,mypos,"Detail Print:",left)
01250   if fc(column)=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
01260   fnchk(8,mypos,"Total by Job:",left)
01270   if tcj(column)=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
01280   fnchk(9,mypos,"Grand Totals:",left)
01290   if tcs(column)=1 then let resp$(respc+=1)="True" else let resp$(respc+=1)="False"
01300   fncmdkey("&Next",1,1,0,"Save changes and move to next column" ) !:
        fncmdkey("&Review Variables",2,0,0,"Get a list of variables that can be used in a formula.") !:
        fncmdkey("&Delete",4,0,0,"Deletes this column from the report.") !:
        fncmdkey("C&omplete",3,0,1,"Save changes and return to main screen.") !:
        fncmdkey("&Cancel",5,0,1,"Return to main screen without saving any changes on this screen.")
01310   fnacs(sn$,0,mat resp$,ckey)
01320   if ckey=5 then goto SCR1
01330   let psc(column)=column ! set array to column number
01340   let f$(column)=resp$(1) ! formula
01350   let pp(column)=val(resp$(2)) ! starting position
01360   let ppr(column)=val(resp$(3)) ! field size
01370   let dp(column)=val(resp$(4)) ! decimal position
01380   if resp$(5)='True' then let fc(column)=1 else let fc(column)=0
01390   if resp$(6)='True' then let tcj(column)=1 else let tcj(column)=0
01400   if resp$(7)='True' then let tcs(column)=1 else let tcs(column)=0
01410   rewrite #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
01420   if ckey=1 then column=min(column+1,20): goto SCR4
01430   if ckey=4 then let f$(column)="": let pp(column)=0: let ppr(column)=0: let dp(column)=0 !:
          let fc(column)=0: let tcj(column)=0: let tcs(column)=0: rewrite #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs !:
          goto SCR4
01440   if ckey=3 then goto L1790
01450   if ckey=2 then goto REVIEW_VARIABLES
01460   goto SCR1
01470 ! ______________________________________________________________________
01480 CHANGETHENUMBER: ! 
01490   write #1,using L1780: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs
01500   delete #1,key=rptn$: nokey L1510
01510 L1510: lst=0
01520   form pos 1,n 2,c 78,2*c 132,n 3,2*n 1,100*pd 6.3,40*pd 2,20*n 1
01530   goto SCR1
01540 MAIN_SCREEN: ! 
01550   if rno=0 then let rno=1
01560   fntos(sn$="user1") !:
        let mylen=25 : let mypos=mylen+2: let respc=0: left=1
01570   let df$="S:\acsPR\Jcreport.mst" : let if$="S:\acsPR\jcreport.idx" !:
        fncombof("CRjcreport",1,1,80,df$,1,2,3,74,if$,1) !:
        fncombof("CRjcreportALL",1,1,80,df$,1,2,3,74,if$,2)
01580   let resp$(1)=str$(rno)
01590   fncmdkey("&Add",1,0,0,"Add a new customer" ) !:
        fncmdkey("E&dit",2,1,0,"Review or change the record.") !:
        fncmdkey("&Cancel",5,0,1,"Exit the program.")
01600   fnacs(sn$,0,mat resp$,ckey)
01610   if ckey=5 then goto XIT
01620   let rno=val(resp$(1)(1:2))
01630   if ckey=1 then addone=1: let rt$="" : mat ch$=("") : let ips=sd=cp=sc=0 : mat ps=(0) !:
          mat f$=("") : mat pp=(0) : mat ppr=(0) : mat dp=(0) : mat fc=(0) !:
          mat tcj=(0) : mat tcs=(0): goto EDIT_ADD_REPORT
01640   if ckey=2 then let rptn$=lpad$(str$(rno),2) !:
          read #1,using L1780,key=rptn$: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey MAIN_SCREEN : goto EDIT_ADD_REPORT
01650 ! ______________________________________________________________________
01660 EDIT_ADD_REPORT: ! 
01670   fntos(sn$="namlst1") !:
        let mylen=25 : let mypos=mylen+2: let respc=0: left=1
01680   fnlbl(1,1,"Report #:",mylen,left)
01690   fntxt(1,mypos,2,0,0,"30",0,"Each report must be assigned a unique number between 1 and 100.") !:
        let resp$(respc+=1)=str$(rn)
01700   fnlbl(2,1,"Report Name:",mylen,left)
01710   fntxt(2,mypos,51,0,0,"",0,"Give each report a unique descriptive name.") !:
        let resp$(respc+=1)=rt$
01720   fnlbl(4,1,"Colort #:",mylen,left)
01730   fnchk(2,mypos,"Print All Jobs:",left) !:
        let resp$(respc+=1)="False"
01740   fnchk(3,mypos,"Print One Job Per Page:",left) !:
        let resp$(respc+=1)="False"
01750   fncmdset(2)
01760   fnacs(sn$,0,mat resp$,ckey)
01770   if ckey=5 then goto XIT
01780 L1780: form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
01790 L1790: close #1: 
01800   fnrx(rn)
01810   execute "INDEX S:\acsPR\JCREPORT.MST,S:\acsPR\JCREPORT.idx,1,2,Replace,DupKeys -n"
01820   fnchain('S:\acsPR\newjcRptS1')
01830 ! ______________________________________________________________________
01850   restore #1,key>="  ": nokey L2490
01860   fnopenwin(win=102,10,28,15,52,cap$)
01870   let wrd3$(1)="Print All Report Files"
01880   let wrd3$(2)="Select Reports to Print"
01890   let io3$(1)="4,2,C 23,N"
01900   let io3$(2)="5,2,C 23,N"
01910   pr f "16,34,C 11,B,5": "Cancel (F5)"
01920   rinput #win,select mat io3$,attr "H": mat wrd3$
01930   let prtall=curfld-1
01940   close #win: ioerr L1950
01950 L1950: if cmdkey=5 then goto SCR1
01960   if prtall=0 then goto L2060
01970   for j=1 to 20
01980     fnopenwin(win=103,10,20,15,59,cap$)
01990     if j>1 then pr #win,fields "6,1,Cc 40,R,N": "Last Report Number Entered was "&rno$(j-1)
02000     pr #win,fields "4,2,C 23,N": "Report Number to Print:"
02010     pr f "16,35,C 09,B,5": "Done (F5)"
02020 L2020: input #win,fields "4,26,N 2,UET,N": rno(j) conv L2020
02030     let rno$(j)=lpad$(str$(rno(j)),2)
02040     if cmdkey=5 or rno(j)=0 then goto L2060
02050   next j
02060 L2060: on fkey 5 goto L2490
02070   fnopenprn
02080   let k=0
02090 L2090: if prtall=0 then goto L2140
02100 L2100: let k=k+1
02110   if val(rno$(k))=0 then goto L2490
02120   read #1,using L1780,key=rno$(k): rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs nokey L2100
02130   goto L2160
02140 L2140: read #1,using L1780: rn,rt$,mat ch$,ips,sd,cp,sc,mat psc,mat f$,mat pp,mat ppr,mat dp,mat fc,mat tcj,mat tcs eof L2490
02150   form pos 1,n 2,c 51,x 27,2*c 132,n 3,3*n 1,100*pd 6.3,20*c 50,40*pd 2,80*n 1
02160 L2160: pr #255,using L2170: "Job Cost Report File Proof List"
02170 L2170: form skip 2,pos 50,c 32
02180   pr #255,using L2190: "Report Number",rn
02190 L2190: form pos 1,c 13,pos 20,pic(zz)
02200   pr #255,using L2210: "Report Title",rt$
02210 L2210: form pos 1,c 12,pos 13,cc 66
02220   pr #255,using L2230: "Column Headings",ch$(1)
02230 L2230: form pos 1,c 15,skip 2,c 132
02240   pr #255,using L2250: ch$(2)
02250 L2250: form pos 1,c 132,skip 2
02260   pr #255,using L2270: "Item # for Selection",ips
02270 L2270: form pos 1,c 20,pos 30,pic(zz#)
02280   pr #255,using L2290: "Summarize Categories",sd
02290 L2290: form pos 1,c 26,pos 32,pic(#)
02300   pr #255,using L2290: "Condense Print",cp
02310   pr #255,using L2290: "Selection Code",sc
02320   pr #255,using L2330: "Print Selection Criteria"
02330 L2330: form skip 1,pos 1,c 30,skip 2
02340   for j=1 to 20
02350     pr #255,using L2360: psc(j),psc(j+20),psc(j+40),psc(j+60),psc(j+80)
02360 L2360: form pos 1,5*n 20.3
02370   next j
02380   pr #255,using L2390: "Formula for Value","Starting","# of pr Positions","# of Decimal","Skip Detail Print","Total Column","Overall Totals"
02390 L2390: form skip 1,pos 1,c 17,pos 39,c 8,pos 48,c 20,pos 71,c 12,pos 84,c 17,pos 103,c 12,pos 119,c 14
02400   pr #255,using L2410: "to be Printed","Print Position","Required","Positions","by Job","by System"
02410 L2410: form pos 1,c 13,pos 38,c 14,pos 53,c 8,pos 72,c 9,pos 107,c 6,pos 123,c 9,skip 2
02420   for j=1 to 20
02430     pr #255,using L2440: f$(j),pp(j),ppr(j),dp(j),fc(j),tcj(j),tcs(j)
02440 L2440: form pos 1,c 50,pos 52,n 3,pos 56,n 3,pos 76,n 1,pos 93,n 1,pos 110,n 1,pos 127,n 1
02450   next j
02460   pr #255: newpage
02470   goto L2090
02480 ! ______________________________________________________________________
02490 L2490: let fncloseprn
02500   on fkey 5 ignore 
02510   goto SCR1
02520 ! ______________________________________________________________________
02530 ! ______________________________________________________________________
02540 SRCH: ! 
02550   bk=0
02560 ! ______________________________________________________________________
02570 XIT: let fnxit
02580 ! ______________________________________________________________________
02590 ! <Updateable Region: ERTN>
02600 ERTN: let fnerror(program$,err,line,act$,"xit")
02610   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
02620   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
02630   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
02640 ERTN_EXEC_ACT: execute act$ : goto ERTN
02650 ! /region
02660 ! ______________________________________________________________________
02670 SCR5: ! selection criteria
02680   if ips=0 then goto SCR4
02690   fntos(sn$="Report-sel") !:
        let respc=0: let mylen=15: let mypos=mylen+3
02700   fnlbl(1,1,"Print Selection Criteria:",30,1)
02710   let z=0
02720   for x=1 to 5
02730     for j=2 to 21
02740       fntxt(j,x*16,12,0,0,"33",0,"If you chosen to limit the report to certain criteria, enter the values here that should match information in the employee's record.") !:
            let resp$(respc+=1)=str$(psc(z+=1))
02750     next j
02760   next x
02770   fncmdkey("&Next",1,1,0,"Save changes and move to next questions" ) !:
        !:
        : let fncmdkey("&Back",6,0,0,"Back up a screen.") !:
        fncmdkey("&Cancel",5,0,1,"Return to selection screen.")
02780   fnacs(sn$,0,mat resp$,ckey) ! ask matching criteria
02790   if ckey=5 then goto SCR1
02800   for j=1 to 100
02810     let psc(j)=val(resp$(j))
02820   next j
02830   if ckey=6 then goto SCR2
02840   goto SCR3
02850 DATA_FOR_COLUMNS: ! 
02860   let ty$(1)= "X1  Job Number                 6"
02870   let ty$(2)= "x2  Job Name                  40"
02880   let ty$(3)= "x3  Job Address               30"
02890   let ty$(4)= "x4  Job Address               30"
02900   let ty$(5)= "x5  City, State Zip           30"
02910   let ty$(6)= "X6  Estimated Completion Date  6"
02920   let ty$(7)= "X7  Contract Amount           10"
02930   let ty$(8)= "X8  Billing to Date           10"
02940   let ty$(9)= "X9  Billing Status             2"
02950   let ty$(10)= "x10 Category Number            5"
02960   let ty$(11)= "x11 Category Name             25"
02970   let ty$(12)= "X12 Labor Estimate            10"
02980   let ty$(13)= "X13 Hours Estimate            10"
02990   let ty$(14)= "X14 Other Estimate            10"
03000   let ty$(15)= "X15 Labor Cost to Date        10"
03010   let ty$(16)= "X16 Hours Worked to Date      10"
03020   let ty$(17)= "X17 Other Cost to Date        10"
03030   let ty$(18)= "X18 Labor Cost - Current      10"
03040   let ty$(19)= "X19 Hours Worked - Current    10"
03050   let ty$(20)= "X20 Other Cost - Current      10"
03060   let ty$(21)= "X21 Units Completed           10"
03070   let ty$(22)= "X22 Estimated Units            8"
03080   let ty$(23)= "X23 Labor % Complete           3"
03090   let ty$(24)= "X24 Other % Complete           3"
03100   let ty1$(1)=""
03110   for j=1 to 24
03120     let ty2$(j+1)=str$(j)&" = "&ty$(j)(5:24)
03130   next j
03140   return 
03150 REVIEW_VARIABLES: ! 
03160   fntos(sn$="ask-column") !:
        let respc=0: let mylen=15: let mypos=mylen+3
03170   fncomboa("Variables",1,mylen+3,mat ty$,"Listing of variables that can be used in a formula.",60,0)
03180   fncmdset(2)
03190   fnacs(sn$,0,mat resp$,ckey) ! variables
03200   goto SCR4
