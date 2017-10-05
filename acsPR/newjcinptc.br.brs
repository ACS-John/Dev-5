00010 ! Replace S:\acsPR\newjcInptC
00020 ! Enter (Job Cost) Time
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnwait,fnoldmsgbox, fnopenprn,fncloseprn,fncno,fnerror,fnchain,fnmsgbox,fntos,fnlbl,fntxt,fncmdkey,fnacs,fncmbjob,fncmbcat,fncmbsubcat,fnflexinit1,fnflexadd1,fncmbcategory
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cap$*128,em$(3)*30,sub$*30,nam$*28,wrd1$(2)*38,wrd3$(4)*38,ln$*132
00080   dim cn$*11,k$*6,n$*40,en$*8,hr(2),empnam$*30,cnam$*40,io2b$(2)*20
00090   dim jn2$*6,ji2(3),ch2$(8),cm2$(8),d$*30,item2$(8)*30
00100   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,b(4),a$(3)*30,sc$(20)*80
00110   dim message$*40,ml$(3)*80,resp$(30)*60,fullname$(20)*20,comboname$(21)*23
00120 ! ______________________________________________________________________
00130   fntop("S:\acsPR\NewJCInptc",cap$="Enter Charges")
00140   fncno(cno,cnam$)
00150 ! 
00160 ! ______________________________________________________________________
00170   open #1: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDEX.h"&str$(cno)&",Shr",internal,input,keyed 
00180   open #5: "Name="&env$('Q')&"\PRmstr\RPMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\RPINDX2.h"&str$(cno)&",Shr",internal,input,keyed 
00190   open #2: "Name="&env$('Q')&"\PRmstr\Department.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\DeptIdx.h"&str$(cno),internal,outin,keyed 
00200   if exists("jccharges."&wsid$) >0 then goto L220 else goto L240
00220 L220: mat ml$(2) !:
        ml$(1)="An unposted file appears to exist! " !:
        ml$(2)="Enter Yes to work with this file, else No to create a new batch of entries." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00230   if resp$="Yes" then goto L260 else goto L240
00240 L240: open #3: "Name=jccharges."&wsid$&",SIZE=0,RecL=84,Replace",internal,outin,relative 
00250   goto L270
00260 L260: open #3: "Name=jccharges."&wsid$,internal,outin,relative 
00270 L270: open #11: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00280   open #14: "Name="&env$('Q')&"\PRmstr\JCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\JCINDX2.H"&str$(cno)&",Shr",internal,input,keyed 
00290   open #12: "Name="&env$('Q')&"\PRmstr\JCCAT.H"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\CatIndx.h"&str$(cno)&",Shr",internal,input,keyed 
00300   open #13: "Name="&env$('Q')&"\PRmstr\SCMSTR.h"&str$(cno)&",KFName="&env$('Q')&"\PRmstr\SCIndex.h"&str$(cno)&",Shr",internal,input,keyed 
00310 ! ______________________________________________________________________
00320   addone=1 ! set code as adding when first entering
00330 ! ______________________________________________________________________
00340 TRANSACTION_ENTRY: ! 
00350   if addone=1 then ji2(3)=0
00360 L360: fntos(sn$="changeinput") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+3
00370   fnlbl(1,1,"Reference #:",mylen,1)
00380   fntxt(1,mypos,12,12,0,"",0,"Use any reference # that will help you identify the entry later on.") !:
        resp$(respc+=1)=rn$
00390   fnlbl(2,1,"Date:",mylen,1)
00400   fntxt(2,mypos,8,8,0,"1",0,"Date of transaction") !:
        resp$(respc+=1)=str$(dat)
00410   fnlbl(3,1,"Job Number:",mylen,1)
00420   fncmbjob(3,mypos) !:
        resp$(respc+=1)=jn2$
00430   fnlbl(4,1,"Category:",mylen,1)
00440   fncmbcategory(4,mypos) !:
        resp$(respc+=1)=str$(ji2(1))
00450   fnlbl(5,1,"Sub-Category:",mylen,1)
00460   fncmbsubcat(5,mypos) !:
        resp$(respc+=1)=str$(ji2(2))
00470   fnlbl(6,1,"Amount:",mylen,1)
00480   fntxt(6,mypos,10,10,0,"10",0,"Amount to be charged to job.") !:
        resp$(respc+=1)=str$(ji2(3))
00490   fnlbl(7,1,"Description:",mylen,1)
00500   fntxt(7,mypos,30,30,0,"",0,"Use any description you choose.") !:
        resp$(respc+=1)=d$
00510   fncmdkey("&Save",1,1,0,"Saves all changes.")
00520   fncmdkey("Co&rrection",7,0,0,"Make a correction to any entry.")
00530   fncmdkey("&LIsting",9,0,0,"Print a listing of all entries.")
00540   fncmdkey("De&lete",4,0,0,"Deletes this entry.")
00550   fncmdkey("&Cancel",5,0,1,"Stops without applying any changes.")
00560   fncmdkey("&Post",8,0,1,"Post these entries to the job files.")
00570   fnacs(sn$,0,mat resp$,ckey) ! detail job screen     editrec
00580   if ckey=5 then goto L590 else goto L610
00590 L590: mat ml$(2) !:
        ml$(1)="You have chosen to cancel without postng these entries!  " !:
        ml$(2)="Take Yes to Exit, else take No to return to the entry screens." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00600   if resp$="Yes" then goto XIT else goto TRANSACTION_ENTRY
00610 L610: if ckey=7 then goto CORRECTIONS
00620   if ckey=8 then goto POSTTOJOBS
00630   if ckey=9 then goto PRINTPROOFLIST
00640   rn$=resp$(1) ! reference #
00650   dat=val(resp$(2)) ! date
00660   jn2$=lpad$(trim$(resp$(3)(1:6)),6) ! job number
00670   if trim$(jn2$)="" then goto L690 else goto L710
00690 L690: mat ml$(2) !:
        ml$(1)="You failed to enter a job number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the job number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00700   if resp$="Yes" then goto L710 else goto L740
00710 L710: ji2(1)=val(resp$(4)(1:5)) ! category
00720   if ji2(1)=0 and dontwarnsubcat=0 then goto L730 else goto L750
00730 L730: mat ml$(2) !:
        ml$(1)="You failed to enter a category number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the category number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00740 L740: if resp$="Yes" then dontwarnsubcat=1: goto L750 else goto L360
00750 L750: ji2(2)=val(resp$(5)(1:3)) ! sub-category
00760   if ji2(2)=0 and dontwarnsubcat=0 then goto L770 else goto L790
00770 L770: mat ml$(2) !:
        ml$(1)="You failed to enter a sub-category number. Take Yes to continue;" !:
        ml$(2)="else take No to return to previous screen and enter the sub-category number." !:
        fnmsgbox(mat ml$,resp$,cap$,52)
00780   if resp$="Yes" then dontwarnsubcat=1 : goto L790 else goto L360
00790 L790: ji2(3)=val(resp$(6)) ! amount
00800   d$=resp$(7) ! description
00810   if addone=1 then goto L820 else goto L850
00820 L820: write #3,using L840: rn$,dat,jn2$, mat ji2,d$
00830   goto L860
00840 L840: form pos 1,c 12,pd 4,c 6,2*pd 3,pd 5.2,c 30
00850 L850: rewrite #3,using L840,rec=editrec: rn$,dat,jn2$, mat ji2,d$ norec CORRECTIONS
00860 L860: if addone=1 then goto TRANSACTION_ENTRY else goto CORRECTIONS
00870 ! ______________________________________________________________________
00880 PRINTPROOFLIST: ! 
00890   on fkey 5 goto PROOF_LIST_DONE
00900   fnopenprn
00910   goto L1010
00920 ! ______________________________________________________________________
00930 PROOF_LIST_HDR: ! 
00940   pr #255,using L970: cnam$
00950   pr #255,using L970: "Charges Proof List"
00960   pr #255,using L970: "Date: "&date$&"      Time: "&time$
00970 L970: form pos 1,cc 113,skip 1
00980   pr #255: "Ref #      Date     Job #  Category  Sub-Cat Amount  Description"
00990   return 
01000 ! ______________________________________________________________________
01010 L1010: gosub PROOF_LIST_HDR
01020   for j=1 to lrec(3)
01030     read #3,using L840,rec=j: rn$,dat,jn2$, mat ji2,d$
01040     if j=1 then goto L1080
01050     if ji1(1)=en then goto L1120
01060     pr #255,using L1070: " ________"," ________"," ____________",t5,t6,t10 pageoflow PROOF_LIST_NWPG
01070 L1070: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Total",pos 38,2*n 9.2,x 29,n 13.2,skip 2
01080 L1080: en=ji1(1)
01090     t5=0
01100     t6=0
01110     t10=0
01120 L1120: pr #255,using L1130: rn$,dat,jn2$,mat ji2 pageoflow PROOF_LIST_NWPG
01130 L1130: form pos 1,c 12,x 1,n 8,x 1,n 5,x 1,pic(---,---.##),x 2,c 30,skip 1
01140     t5=t5+ji1(5)
01150     t6=t6+ji1(6)
01160     t10=t10+ji2(3)
01170     gt5=gt5+ji1(5)
01180     gt6=gt6+ji1(6)
01190     gt10=tg10+ji2(3)
01200   next j
01210   pr #255,using L1070: " ________"," ________"," ____________",t5,t6,t10
01220   pr #255,using L1230: " ________"," ________"," ____________",gt5,gt6,gt10
01230 L1230: form pos 38,2*c 9,x 29,c 13,skip 1,pos 8,"Grand Totals",pos 38,2*n 9.2,x 29,n 13.2,skip 2
01240 PROOF_LIST_DONE: ! 
01250   gt5=gt6=gt10=0
01260   fncloseprn
01270   goto TRANSACTION_ENTRY
01280 ! ______________________________________________________________________
01290 POSTTOJOBS: ! 
01300   close #1: 
01310   close #2: 
01320   close #3: 
01330   close #11: 
01340   close #12: 
01350   close #13: 
01360   fnchain("S:\acsPR\NEWJCMRGC")
01370 ! ______________________________________________________________________
01380 PROOF_LIST_NWPG: ! 
01390   pr #255: newpage
01400   gosub PROOF_LIST_HDR
01410   continue 
01420 ! ______________________________________________________________________
01430 ! <Updateable Region: ERTN>
01440 ERTN: fnerror(program$,err,line,act$,"xit")
01450   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01460   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01470   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01480 ERTN_EXEC_ACT: execute act$ : goto ERTN
01490 ! /region
01500 ! ______________________________________________________________________
01510   if err=61 then goto TRANSACTION_ENTRY
01520 ! ______________________________________________________________________
01530 XIT: fnxit
01540 ! ______________________________________________________________________
01550 ! INPUT FROM DISKETTE FILE    ! took this option out on new system
01560 ! ______________________________________________________________________
01570 CORRECTIONS: ! 
01580   addone=0: editone=0
01590   fntos(sn$="EntryCorrection")
01600   ch2$(1)="Rec #": ch2$(2)="Ref #": ch2$(3)="Date": ch2$(4)="Job #" !:
        ch2$(5)="Cat" !:
        ch2$(6)="Sub-Cat": ch2$(7)="Amount": ch2$(8)="Description" !:
        mat ch2$(8) ! : Mat CM2$(8) : Mat ITEM2$(8)
01610   cm2$(1)="30": cm2$(2)="": cm2$(3)="1" !:
        cm2$(4)="" !:
        cm2$(5)="30": cm2$(6)="30": cm2$(7)="10" !:
        cm2$(8)="" !:
        cm2$(8): ch2$(8): item2$(8)
01620   fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
01630   restore #3: 
01640 READ_FILE: ! 
01650   read #3,using L840: rn$,dat,jn2$, mat ji2,d$ eof L1690
01660   item2$(1)=str$(rec(3)): item2$(2)=rn$ !:
        item2$(3)=str$(dat): item2$(4)=jn2$ !:
        item2$(5)=str$(ji2(1)): item2$(6)=str$(ji2(2)) !:
        item2$(7)=str$(ji2(3)) : item2$(8)=d$
01670   fnflexadd1(mat item2$)
01680   goto READ_FILE
01690 L1690: fncmdkey("&Add",1,0,0,"Add a new transaction." ) !:
        fncmdkey("E&dit",2,1,0,"Edit the highlited record") !:
        fncmdkey("&Delete",4,0,0,"Deletes the highlited record") !:
        fncmdkey("&Refresh",7,0,0,"Updates search grids and combo boxes with new transaction information") !:
        fncmdkey("E&xit",5,0,1,"Returns to main screen.")
01700   fnacs(sn$,0,mat resp$,ckey) ! review_details  grid of transactions
01710   if ckey=5 then goto TRANSACTION_ENTRY
01720   editrec=val(resp$(1))
01730   if ckey=1 then addone=1: mat ji2=(0): jn2$="": goto TRANSACTION_ENTRY
01740   if ckey=2 then read #3,using L840,rec=editrec: rn$,dat,jn2$, mat ji2,d$: editone=1 : goto TRANSACTION_ENTRY
01750   if ckey=4 then delete #3,rec=editrec: : goto CORRECTIONS
01760   goto CORRECTIONS
