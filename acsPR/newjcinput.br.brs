00010 ! Replace S:\acsPR\newJCInput
00020 ! Enter (Job Cost) Time
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnoldmsgbox, fnopenprn,fncloseprn,fnerror,fnchain,fnmsgbox,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fncmbemp,fncomboa,fncombof,fncmbjob,fncmbsubcat,fnflexinit1,fnflexadd1,fncmbcategory,fnDedNames
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
00080   dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,io2b$(2)*20
00090   dim ji1(6),jn$*6,ji2(5),ch2$(14),cm2$(14),itme2$(14)
00100   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
00110   dim message$*40,ml$(3)*80,resp$(30)*60,fullname$(20)*20,comboname$(22)*23
00120   dim item2$(14)*20,tdt(4),tcd(3),tdet(23)
00130 ! ______________________________________________________________________
00140   fntop(program$,cap$="Enter Time")
00160 ! 
00170 ! ___________________________
00180   fnDedNames(mat fullname$)
00200   for j=1 to 20: comboname$(j+1)=cnvrt$("pic(zz)",j)&" "&fullname$(j): next j
00210   comboname$(1)=" 0 Not Applicable"
00220   comboname$(22)="21 Add Amount to Pay"
00230 ! ______________________________________________________________________
00240   open #1: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDEX.h[cno],Shr",internal,input,keyed 
00250   open #5: "Name=[Q]\PRmstr\RPMSTR.h[cno],KFName=[Q]\PRmstr\RPINDX2.h[cno],Shr",internal,input,keyed 
00260   open #7: "Name=[Q]\PRmstr\Burden.H[cno],KFName=[Q]\PRmstr\BurdenIdx.H[cno],Shr",internal,input,keyed 
00270   open #2: "Name=[Q]\PRmstr\Department.h[cno],KFName=[Q]\PRmstr\DeptIdx.h[cno]",internal,outIn,keyed 
00280   if exists("jcWork."&session$) >0 then goto L280 else goto L300
00285 L280: mat ml$(2) !:
        ml$(1)="An unposted file appears to exist! " !:
        ml$(2)="Enter Yes to work with this file, else No to create a new batch of entries." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00290   if resp$="Yes" then goto L320 else goto L300
00300 L300: open #3: "Name=jcWork."&session$&",SIZE=0,RecL=84,Replace",internal,outIn,relative 
00310   goto L330
00320 L320: open #3: "Name=jcWork."&session$,internal,outIn,relative 
00330 L330: open #11: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,input,keyed 
00340   open #14: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,input,keyed 
00350   open #12: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,input,keyed 
00360   open #13: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,input,keyed 
00370 ! ______________________________________________________________________
00380   addone=1 ! set code as adding when first entering
00390 ! ______________________________________________________________________
00400 TRANSACTION_ENTRY: ! 
00410   if addone=1 then ji1(5)=ji1(6)=ji2(3)=0: ji1(2)=2
00420 L420: fnTos(sn$="jobinput") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+3
00430   fnLbl(1,1,"Employee #:",mylen,1)
00440   fncmbemp(1,mypos) !:
        resp$(respc+=1)=str$(ji1(1))
00450   fnLbl(2,1,"Method of Payment:",mylen,1)
00460   opt$(1)="1 = Salary" : opt$(2)= "2 = Hourly" !:
        opt$(3)= "3 = Both" !:
        fncomboa("Methods",2,mypos,mat opt$,empty$,13)
00470   if ji1(2)=1 then resp$(respc+=1)=opt$(1)
00480   if ji1(2)=2 then resp$(respc+=1)=opt$(2)
00490   if ji1(2)=3 then resp$(respc+=1)=opt$(3)
00500   fnLbl(3,1,"Date:",mylen,1)
00510   fnTxt(3,mypos,8,8,0,"1",0,"Date of transaction") !:
        resp$(respc+=1)=str$(ji1(3))
00520   fnLbl(4,1,"Payroll Department:",mylen,1)
00530   fncombof("Deptname",4,mypos,25,"[Q]\PRmstr\DeptName.h[cno]",1,3,4,25,"[Q]\PRmstr\DeptNameIdx.h[cno]",0,0, " ",0,0) !:
        resp$(respc+=1)=str$(ji1(4))
00540   fnLbl(5,1,"Regular Hours:",mylen,1)
00550   fnTxt(5,mypos,8,8,0,"32",0,"") !:
        resp$(respc+=1)=str$(ji1(5))
00560   fnLbl(6,1,"Overtime Hours:",mylen,1)
00570   fnTxt(6,mypos,8,8,0,"32",0,"") !:
        resp$(respc+=1)=str$(ji1(6))
00580   fnLbl(7,1,"Job Number:",mylen,1)
00590   fncmbjob(7,mypos) !:
        resp$(respc+=1)=jn$
00600   fnLbl(8,1,"Category:",mylen,1)
00610   fncmbcategory(8,mypos) !:
        resp$(respc+=1)=str$(ji2(1))
00620   fnLbl(9,1,"Sub-Category:",mylen,1)
00630   fncmbsubcat(9,mypos) !:
        resp$(respc+=1)=str$(ji2(2))
00640   fnLbl(10,1,"Amount:",mylen,1)
00650   fnTxt(10,mypos,10,10,0,"10",0,"Amount to be charged to job. Payroll will be extended as it is posted.") !:
        resp$(respc+=1)=str$(ji2(3))
00660   fnLbl(11,1,"Deduction/Addition Code:",mylen,1)
00670   fncomboa("Deductions",11,mypos,mat comboname$,empty$,23)
00680   if ji2(4)=0 then resp$(respc+=1)=comboname$(1): goto L700
00690   if ji2(4)>0 and ji2(4)<=20 then resp$(respc+=1)=comboname$(ji2(4)) else resp$(respc+=1)=""
00700 L700: fnLbl(12,1,"Units:",mylen,1)
00710   fnTxt(12,mypos,6,6,0,"30",0,"Enter units, if applicable.") !:
        resp$(respc+=1)=str$(ji2(5))
00720   fnLbl(13,1,"Personnel Burden:",mylen,1)
00730   fnTxt(13,mypos,10,10,0,"10",0,"Personnel burden will calculated automatically if you have the burden % set up on each employee.") !:
        resp$(respc+=1)=str$(pt)
00740   picture=0
00750   fnCmdKey("&Save",1,1,0,"Saves all changes.")
00760   fnCmdKey("Co&rrection",7,0,0,"Make a correction to any entry.")
00770   fnCmdKey("&LIsting",9,0,0,"Print a listing of all entries.")
00780   fnCmdKey("De&lete",4,0,0,"Deletes this job.")
00790   fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
00800   fnCmdKey("&Post",8,0,1,"Post these entries to the job files.")
00810   fnAcs(sn$,0,mat resp$,ckey) ! detail job screen     editrec
00820   if ckey=5 then goto L830 else goto L850
00830 L830: mat ml$(2) !:
        ml$(1)="You have chosen to cancel without postng these entries!  " !:
        ml$(2)="Take Yes to Exit, else take No to return to the entry screens." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00840   if resp$="Yes" then goto XIT else goto TRANSACTION_ENTRY
00850 L850: if ckey=7 then goto CORRECTIONS
00860   if ckey=8 then goto POSTTOJOBS
00870   if ckey=9 then goto PRINTPROOFLIST
00880   ji1(1)=val(resp$(1)(1:8)) ! employee number
00890   ji1(2)=val(resp$(2)(1:1)) ! Method of pay
00900   ji1(3)=val(resp$(3)) ! date
00910   ji1(4)=val(resp$(4)(1:3)) ! dept #
00920   ji1(5)=val(resp$(5)) ! regular hours
00930   ji1(6)=val(resp$(6)) ! ot hours
00940   jn$=resp$(7)(1:6) ! job #
00950   if trim$(jn$)="" then goto L960 else goto L980
00960 L960: mat ml$(2) !:
        ml$(1)="You failed to enter a job number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the job number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00970   if resp$="Yes" then goto L980 else goto L420
00980 L980: ji2(1)=val(resp$(8)(1:3)) ! category
00990   if ji2(1)=0 and dontwarnsubcat=0 then goto L1000 else goto L1020
01000 L1000: mat ml$(2) !:
        ml$(1)="You failed to enter a category number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the category number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
01010   if resp$="Yes" then dontwarnsubcat=1: goto L1020 else goto L420
01020 L1020: ji2(2)=val(resp$(9)(1:3)) ! sub-category
01030   if ji2(2)=0 and dontwarnsubcat=0 then goto L1040 else goto L1060
01040 L1040: mat ml$(2) !:
        ml$(1)="You failed to enter a sub-category number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the sub-category number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
01050   if resp$="Yes" then dontwarnsubcat=1 : goto L1060 else goto L420
01060 L1060: ji2(3)=val(resp$(10)) ! amount
01070   ji2(4)=val(resp$(11)(1:2)) ! deduction code
01080   ji2(5)=val(resp$(12)) ! units
01090   pt=val(resp$(13)) ! personnel burden
01100   gosub UPDATE_AMOUNT
01110   empnam$="": read #1,using "FORM POS 9,C 30",key=cnvrt$("pic(ZZZZZZZ#)",ji1(1)): empnam$ nokey L1110
01115 L1110: if addone=1 then goto L1120 else goto L1150
01120 L1120: write #3,using L1140: mat ji1, jn$, mat ji2, pt, empnam$, sal
01130   goto L1160
01140 L1140: form pos 1,n 8,n 1,pd 4,pd 2,2*pd 4.2,c 6,2*pd 3,pd 5.2,n 2,2*pd 4.2,c 30,pd 4.2
01150 L1150: rewrite #3,using L1140,rec=editrec: mat ji1, jn$, mat ji2, pt, empnam$, sal noRec CORRECTIONS
01160 L1160: if addone=1 then goto TRANSACTION_ENTRY else goto CORRECTIONS
01170 ! ______________________________________________________________________
01180 PRINTPROOFLIST: ! 
01190   on fkey 5 goto PROOF_LIST_DONE
01200   fnopenprn
01210   goto L1480
01220 ! ______________________________________________________________________
01230 PROOF_LIST_HDR: ! 
01240   pr #255,using L1440: env$('cnam')
01250   pr #255,using L1440: "Job Cost Input Proof List"
01260   pr #255,using L1440: "Date: "&date$&"      Time: "&time$
01440 L1440: form pos 1,cc 113,skip 1
01450   pr #255: "Ref #   Emp #  Method-Pay  Date   Dept  Reg-Hrs   OT-Hrs  Job #   Category  Sub-Category   Amount  Ded-Add  Units"
01460   return 
01470 ! ______________________________________________________________________
01480 L1480: gosub PROOF_LIST_HDR
01490   for j=1 to lrec(3)
01500     read #3,using L1140,rec=j: mat ji1,jn$,mat ji2,pt
01510     if j=1 then goto L1550
01520     if ji1(1)=en then goto L1590
01530     pr #255,using L1540: " ________"," ________"," ____________",t5,t6,t10 pageoflow PROOF_LIST_NWPG
01540 L1540: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Total",pos 38,2*n 9.2,x 29,n 13.2,skip 2
01550 L1550: en=ji1(1)
01560     t5=0
01570     t6=0
01580     t10=0
01590 L1590: pr #255,using L1600: j,mat ji1,jn$,mat ji2 pageoflow PROOF_LIST_NWPG
01600 L1600: form pos 1,n 5,n 8,n 6,n 13,n 5,2*n 9.2,x 2,c 6,n 11,n 10,n 13.2,n 6,n 10.2,skip 1
01610     t5=t5+ji1(5)
01620     t6=t6+ji1(6)
01630     t10=t10+ji2(3)
01640     gt5=gt5+ji1(5)
01650     gt6=gt6+ji1(6)
01660     gt10=tg10+ji2(3)
01670   next j
01680   pr #255,using L1540: " ________"," ________"," ____________",t5,t6,t10
01690   pr #255,using L1700: " ________"," ________"," ____________",gt5,gt6,gt10
01700 L1700: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Grand Totals",pos 38,2*n 9.2,x 29,n 13.2,skip 2
01710 PROOF_LIST_DONE: ! 
01720   gt5=gt6=gt10=0
01730   fncloseprn
01740   goto TRANSACTION_ENTRY
01750 ! ______________________________________________________________________
01760 POSTTOJOBS: ! 
01770   close #1: 
01780 L1780: close #2: 
01790   close #3: 
01800   close #11: 
01810   close #12: 
01820   close #13: 
01830   fnchain("S:\acsPR\newJCMerge")
01840 ! ______________________________________________________________________
01850 PROOF_LIST_NWPG: ! 
01860   pr #255: newpage
01870   gosub PROOF_LIST_HDR
01880   continue 
01890 ! ______________________________________________________________________
01900 ! <Updateable Region: ERTN>
01910 ERTN: fnerror(program$,err,line,act$,"xit")
01920   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01930   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01940   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01950 ERTN_EXEC_ACT: execute act$ : goto ERTN
01960 ! /region
01970 ! ______________________________________________________________________
01980   if err=61 then goto TRANSACTION_ENTRY
01990 ! ______________________________________________________________________
02000 XIT: fnxit
02010 ! ______________________________________________________________________
02020 ! INPUT FROM DISKETTE FILE    ! took this option out on new system
02030 ! ______________________________________________________________________
02040 CORRECTIONS: ! 
02050   addone=0: editone=0
02060   fnTos(sn$="EntryCorrection")
02070   ch2$(1)="Rec #": ch2$(2)="Employee #": ch2$(3)="MOP" !:
        ch2$(4)="Date" !:
        ch2$(5)="Dept #": ch2$(6)="RegHrs": ch2$(7)="OTHrs" !:
        ch2$(8)="Job #": ch2$(9)="Cat #": ch2$(10)="Su-Cat" !:
        ch2$(11)="Amount": ch2$(12)="Ded #": ch2$(13)="Units" !:
        ch2$(14)="Burden" !:
        mat ch2$(14) ! : Mat CM2$(14) : Mat ITEM2$(14)
02080   cm2$(1)="30": cm2$(2)="30": cm2$(3)="30" !:
        cm2$(4)="1" !:
        cm2$(5)="30": cm2$(6)="32": cm2$(7)="32" !:
        cm2$(8)="": cm2$(9)="30": cm2$(10)="30" !:
        cm2$(11)="10": cm2$(12)="30": cm2$(13)="30": cm2$(14)="10"
02090   fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
02100   restore #3: 
02110 READ_FILE: ! 
02120   read #3,using L1140: mat ji1, jn$, mat ji2, pt, empnam$, sal eof L2160
02130   item2$(1)=str$(rec(3)): item2$(2)=str$(ji1(1)) !:
        item2$(3)=str$(ji1(2)): item2$(4)=str$(ji1(3)) !:
        item2$(5)=str$(ji1(4)): item2$(6)=str$(ji1(5)) !:
        item2$(7)=str$(ji1(6)) : item2$(8)=jn$ !:
        item2$(9)=str$(ji2(1)): item2$(10)=str$(ji2(2)) !:
        item2$(11)=str$(ji2(3)) : item2$(12)=str$(ji2(4)) !:
        item2$(13)=str$(ji2(5)): item2$(14)=str$(pt)
02140   fnflexadd1(mat item2$)
02150   goto READ_FILE
02160 L2160: fnCmdKey("&Add",1,0,0,"Add a new transaction." ) !:
        fnCmdKey("E&dit",2,1,0,"Edit the highlited record") !:
        fnCmdKey("&Delete",4,0,0,"Deletes the highlited record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new transaction information") !:
        fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
02170   fnAcs(sn$,0,mat resp$,ckey) ! review_details  grid of transactions
02180   if ckey=5 then goto TRANSACTION_ENTRY
02190   editrec=val(resp$(1))
02200   if ckey=1 then addone=1: mat ji1=(0): mat ji2=(0): jn$="": goto TRANSACTION_ENTRY
02210   if ckey=2 then read #3,using L1140,rec=editrec: mat ji1, jn$, mat ji2, pt, empnam$, sal: editone=1 : goto TRANSACTION_ENTRY
02220   if ckey=4 then delete #3,rec=editrec: : goto CORRECTIONS
02230   goto CORRECTIONS
02240 UPDATE_AMOUNT: ! 
02250   read #2,using 'Form POS 1,N 8,n 3,c 12,4*N 6,3*N 2,pd 4.2,23*PD 4.2',key=cnvrt$("pic(ZZZZZZZ#)",ji1(1))&cnvrt$("pic(ZZ#)",ji1(4)): teno,tdn,gl$,mat tdt,mat tcd,tli,mat tdet nokey L2270
02260   goto L2280
02270 L2270: mat ml$(2) !:
        ml$(1)="There is no department number "&str$(ji1(4))&" on employee number "&str$(ji1(1))&"!" !:
        ml$(2)="Take OK to correct." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto L420
02280 L2280: if ji2(3)=0 then ji2(3)=tdet(2)*ji1(5)+tdet(3)*ji1(6)
02290   if rtrm$(jn$)="" and ji2(1)=0 then goto L2390
02300   read #11,using L2310,key=lpad$(rtrm$(jn$),6): n$ nokey L2330
02310 L2310: form pos 7,c 40
02320   goto L2340
02330 L2330: mat ml$(2) !:
        ml$(1)="The job # number appears to be an incorrect number!  " !:
        ml$(2)="Take OK to correct." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto L420
02340 L2340: cn$=lpad$(rtrm$(jn$),6)&lpad$(str$(ji2(1)),5)
02350 ! Read #12,Using 2360,Key=CN$: KC$ Nokey 2380     ! dont verify this kj
02360   form pos 1,c 11
02370   goto L2390
02380   mat ml$(2) !:
        ml$(1)="Category # "&str$(ji2(1))&" has never been used on this job before!  " !:
        ml$(2)="Do you wish to use this number or select a different one?" !:
        fnmsgbox(mat ml$,resp$,cap$,36)
02390   if resp$="Yes" then goto L2390 else goto L420
02395 L2390: if ji2(2)=0 then goto L2450
02400   read #13,using L2420,key=lpad$(str$(ji2(2)),3): sub$ nokey L2440
02420 L2420: form pos 4,c 30
02430   goto L2450
02440 L2440: mat ml$(2) !:
        ml$(1)="The subcategory # number appears to be an incorrect number!  " !:
        ml$(2)="Take OK to correct." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto L420
02450 L2450: if lrec(13)=0 then goto L2480 ! if they are not using subcategory, skip warning
02460   if ji2(2)=0 then goto L2470
02470   goto L2480
02475 L2470: mat ml$(2) !:
        ml$(1)="The subcategory # number cannot be zero.!  " !:
        ml$(2)="Take OK to correct." !:
        fnmsgbox(mat ml$,resp$,cap$,0) !:
        goto L420
02480 L2480: if ji2(4)<1 or ji2(4)>20 then pt=ptp/100*ji2(3) else pt=0
02490   eno$=cnvrt$("n 8",ji1(1)) : ptp=0
02500   read #7,using "form pos 39,n 6.3",key=eno$: ptp nokey L2510
02510 L2510: if ji2(4)<1 or ji2(4)>20 then pt=ptp/100*ji2(3) else pt=0
02520   if c1=2 then goto L1780
02530   return 
