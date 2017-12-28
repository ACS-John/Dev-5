00010 ! Replace S:\acsPR\newjcipbil
00020 ! Enter (Job Cost) billings
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnopenprn,fncloseprn,fncno,fnerror,fnmsgbox,fnTos,fnLbl,fnTxt,fnCmdKey,fnAcs,fnemployee_srch,fncmbjob,fncmbsubcat,fnflexinit1,fnflexadd1
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
00080   dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,cnam$*40,io2b$(2)*20
00090   dim jn$*6,inp(3),ch2$(5),cm2$(5),d$*30,item2$(5)*30
00100   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
00110   dim message$*40,ml$(3)*80,resp$(30)*60,fullname$(20)*20,comboname$(21)*23
00120 ! ______________________________________________________________________
00130   fntop("S:\acsPR\NewJCIpbil",cap$="Enter Billings")
00140   fncno(cno,cnam$)
00150 ! 
00160 ! ______________________________________________________________________
00170   if exists("jcbillings."&wsid$) >0 then goto L180 else goto L200
00180 L180: mat ml$(2) !:
        ml$(1)="An unposted file appears to exist! " !:
        ml$(2)="Enter Yes to work with this file, else No to create a new batch of entries." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00190   if resp$="Yes" then goto L220 else goto L200
00200 L200: open #3: "Name=jcbillings."&wsid$&",SIZE=0,RecL=84,Replace",internal,outIn,relative 
00210   goto L230
00220 L220: open #3: "Name=jcbillings."&wsid$,internal,outIn,relative 
00230 L230: open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&env$('cno')&",Shr",internal,outIn,keyed 
00240   open #14: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\JCINDX2.H"&env$('cno')&",Shr",internal,input,keyed 
00250   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&env$('cno')&",Shr",internal,input,keyed 
00260   open #13: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&env$('cno')&",KFName="&env$('Q')&"\PRmstr\SCIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00270 ! ______________________________________________________________________
00280   addone=1 ! set code as adding when first entering
00290 ! ______________________________________________________________________
00300 TRANSACTION_ENTRY: ! 
00310   if addone=1 then inp(1)=inp(3)=0
00320   fnTos(sn$="billinginput") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+3
00330   fnLbl(1,1,"Job Number:",mylen,1)
00340   fncmbjob(1,mypos) !:
        resp$(respc+=1)=jn$
00350   fnLbl(2,1,"Amount:",mylen,1)
00360   fnTxt(2,mypos,10,10,0,"10",0,"Amount to be charged to job.") !:
        resp$(respc+=1)=str$(inp(1))
00370   fnLbl(3,1,"Date:",mylen,1)
00380   fnTxt(3,mypos,8,8,0,"1",0,"Date of transaction") !:
        resp$(respc+=1)=str$(inp(2))
00390   fnLbl(4,1,"Status:",mylen,1)
00400   fnTxt(4,mypos,2,2,0,"30",0,"") !:
        resp$(respc+=1)=str$(inp(3))
00410   fnCmdKey("&Save",1,1,0,"Saves all changes.")
00420   fnCmdKey("Co&rrection",7,0,0,"Make a correction to any entry.")
00430   fnCmdKey("&LIsting",9,0,0,"Print a listing of all entries.")
00440   fnCmdKey("De&lete",4,0,0,"Deletes this entry.")
00450   fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
00460   fnCmdKey("&Post",8,0,1,"Post these entries to the job files.")
00470   fnAcs(sn$,0,mat resp$,ckey) ! detail job screen     editrec
00480   if ckey=5 then goto L490 else goto L510
00490 L490: mat ml$(2) !:
        ml$(1)="You have chosen to cancel without postng these entries!  " !:
        ml$(2)="Take Yes to Exit, else take No to return to the entry screens." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00500   if resp$="Yes" then goto XIT else goto TRANSACTION_ENTRY
00510 L510: if ckey=7 then goto CORRECTIONS
00520   if ckey=8 then goto POSTTOJOBS
00530   if ckey=9 then goto PRINTPROOFLIST
00540   jn$=lpad$(trim$(resp$(1)(1:6)),6) ! job #
00550   inp(1)=val(resp$(2)) ! amount
00560   inp(2)=val(resp$(3)) ! date
00570   inp(3)=val(resp$(4)) ! status
00580   if trim$(jn$)="" then goto L590 else goto L610
00590 L590: mat ml$(2) !:
        ml$(1)="You failed to enter a job number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the job number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00600   if resp$="Yes" then goto L610 else goto TRANSACTION_ENTRY
00610 L610: if addone=1 then goto L620 else goto L650
00620 L620: write #3,using L640: jn$,mat inp
00630   goto L660
00640 L640: form pos 1,c 6,pd 5.2,pd 4,n 2
00650 L650: rewrite #3,using L640,rec=editrec: jn$,mat inp noRec CORRECTIONS
00660 L660: if addone=1 then goto TRANSACTION_ENTRY else goto CORRECTIONS
00670 ! ______________________________________________________________________
00680 PRINTPROOFLIST: ! 
00690   on fkey 5 goto PROOF_LIST_DONE
00700   fnopenprn
00710   goto L810
00720 ! ______________________________________________________________________
00730 PROOF_LIST_HDR: ! 
00740   pr #255,using L770: cnam$
00750   pr #255,using L770: "Billing Proof List"
00760   pr #255,using L770: "Date: "&date$&"      Time: "&time$
00770 L770: form pos 1,cc 113,skip 1
00780   pr #255: "Job #      Amount     Date   Status"
00790   return 
00800 ! ______________________________________________________________________
00810 L810: gosub PROOF_LIST_HDR
00820   for j=1 to lrec(3)
00830     read #3,using L640,rec=j: jn$,mat inp
00840     form pos 10,c 9,skip 1,pos 10,pic(-----,---.##),skip 1
00850     t1=0
00860     pr #255,using L870: jn$,mat inp pageoflow PROOF_LIST_NWPG
00870 L870: form pos 1,c 6,x 1,pic(----,---.##),x 2,pic(zz/zz/zz),x 2,n 2,skip 1
00880     t1=t1+inp(1)
00890   next j
00900   pr #255,using L910: " __________",t1
00910 L910: form pos 8,c 11,skip 1,pos 7,pic(-----,---.##),skip 1
00920 PROOF_LIST_DONE: ! 
00930   gt1=0
00940   fncloseprn
00950   goto TRANSACTION_ENTRY
00960 ! ______________________________________________________________________
00970 POSTTOJOBS: ! 
00980   for j=1 to lrec(3)
00990     read #3,using L640,rec=j: jn$,mat inp noRec L1050
01000     if ltrm$(jn$)="" or ltrm$(rtrm$(jn$))="0" then goto L1050
01010     read #11,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4 nokey L1050
01020     b3=b3+inp(1)
01030     if inp(3)><0 then b4=inp(3)
01040     rewrite #11,using 'Form POS 150,PD 7.2,N 2',key=jn$: b3,b4
01050 L1050: next j
01060   close #3,free: 
01070   goto XIT
01080 ! ______________________________________________________________________
01090 PROOF_LIST_NWPG: ! 
01100   pr #255: newpage
01110   gosub PROOF_LIST_HDR
01120   continue 
01130 ! ______________________________________________________________________
01140 ! <Updateable Region: ERTN>
01150 ERTN: fnerror(program$,err,line,act$,"xit")
01160   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01170   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01180   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01190 ERTN_EXEC_ACT: execute act$ : goto ERTN
01200 ! /region
01210 ! ______________________________________________________________________
01220   if err=61 then goto TRANSACTION_ENTRY
01230 ! ______________________________________________________________________
01240 XIT: fnxit
01250 ! ______________________________________________________________________
01260 ! INPUT FROM DISKETTE FILE    ! took this option out on new system
01270 ! ______________________________________________________________________
01280 CORRECTIONS: ! 
01290   addone=0: editone=0
01300   fnTos(sn$="EntryCorrection")
01310   ch2$(1)="Rec #": ch2$(2)="Job #": ch2$(3)="Amount": ch2$(4)="Date #" !:
        ch2$(5)="Status" !:
        mat ch2$(5) ! : Mat CM2$(5) : Mat ITEM2$(5)
01320   cm2$(1)="30": cm2$(2)="": cm2$(3)="10" !:
        cm2$(4)="1" !:
        cm2$(5)="30" !:
        cm2$(5): ch2$(5): item2$(5)
01330   fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
01340   restore #3: 
01350 READ_FILE: ! 
01360   read #3,using L640: jn$,mat inp eof L1400
01370   item2$(1)=str$(rec(3)): item2$(2)=jn$ !:
        item2$(3)=str$(inp(1)): item2$(4)=str$(inp(2)) !:
        item2$(5)=str$(inp(3))
01380   fnflexadd1(mat item2$)
01390   goto READ_FILE
01400 L1400: fnCmdKey("&Add",1,0,0,"Add a new transaction." ) !:
        fnCmdKey("E&dit",2,1,0,"Edit the highlited record") !:
        fnCmdKey("&Delete",4,0,0,"Deletes the highlited record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new transaction information") !:
        fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
01410   fnAcs(sn$,0,mat resp$,ckey) ! review_details  grid of transactions
01420   if ckey=5 then goto TRANSACTION_ENTRY
01430   editrec=val(resp$(1))
01440   if ckey=1 then addone=1: mat inp=(0): jn$="": goto TRANSACTION_ENTRY
01450   if ckey=2 then read #3,using L640,rec=editrec: jn$,mat inp: editone=1 : goto TRANSACTION_ENTRY
01460   if ckey=4 then delete #3,rec=editrec: : goto CORRECTIONS
01470   goto CORRECTIONS
