00010 ! Replace S:\acsPR\newJCMaint
00020 ! Job Cost Master File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fndat,fnprocess,fnTos,fnLbl,fnCmdKey,fnAcs,fnTxt,fnmsgbox,fncombof,fnjob_srch,fncmbjob ,fncategory_srch,fncat_srch2,fncategory_srch,fncmbcat,fnflexinit1,fnflexadd1,fncmbcategory
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim fl1$(9),sc1$(9)*20,io1$(9),hd$(2)*78,msgline$(2)*60,response$(5)*1
00080   dim sc2$(15)*20,fl2$(15),io2$(15),sc3$(12),fl3$(13),io3$(12),dup$*11
00090   dim dat$*20,cnam$*40,jn$*6,hjn$*6,n$*40,a$(3)*30,b(4)
00100   dim cn$*11,k$*25,l(13),ta(2),wrd1$(5)*38,cap$*128,message$*40
00110   dim contact$*30,ph$*12,email$*60,ml$(3)*80,resp$(20)*60
00120   dim eno$*12,jno$*6,tr(9),pd$*30,ln$(2,3)*30,ln(13,3),dup$*11
00130   dim hlp$(20)*78,flh$(22)*18,hhd$*60,ch2$(13),cm2$(13),item2$(13)*30
00140   dim bk$(20)*28,nam$*28,ios$(2),wrds$(2)*30,df$*256,if$*256
00150 ! ______________________________________________________________________
00160   fntop("S:\acsPR\nEWJCMaint",cap$="Job Cost")
00170   fncno(cno,cnam$) !:
        fndat(dat$)
00180 ! 
00190 ! ______________________________________________________________________
00200   open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed ioerr L4890
00210   open #4: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,outIn,keyed ioerr L220
00220 L220: open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed ioerr L230
00230 L230: open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],Shr",internal,outIn,relative ioerr L240
00240 L240: open #16: "Name=[Q]\PRmstr\Category.H[cno],KFName=[Q]\PRmstr\categoryIDX.H[cno],Shr",internal,outIn,keyed ioerr L250
00250 L250: ! ______________________________________________________________________
00260 MENU1: ! 
00270   ndep=0
00280 ASKJOB: ! 
00290   addjob=0 ! add code - used to tell other parts of the program, !:
        ! that I am currently adding an job record.
00300   fnTos(sn$="job-ask") !:
        respc=0
00310   fnLbl(1,1,"Job #:",7,right)
00320   fncmbjob(1,9)
00330   if hact$="" then !:
          resp$(respc+=1)="" else !:
          resp$(respc+=1)=hact$
00340   fnCmdKey("&Add",1,0,0,"Add a new job" ) !:
        fnCmdKey("E&dit",2,1,0,"Access the highlited record") !:
        fnCmdKey("&Next Record",3,0,0,"Access next record injob # order") !:
        fnCmdKey("&Search",8,0,0,"Search forjob record") !:
        fnCmdKey("&Proof",11,0,0,"Prints proof listing of all joub information.") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new job information") !:
        fnCmdKey("E&xit",6,0,1,"Returns to menu")
00350   fnAcs(sn$,0,mat resp$,ckey) ! ask job #
00360   hact$=lpad$(rtrm$(resp$(1)(1:6)),6) !:
        jn$=lpad$(rtrm$(resp$(1)(1:6)),6)
00370 ! 
00380   if ckey=1 then ti1=addjob=1 : goto ADDREC else !:
          if ckey=2 then goto EDITREC else !:
            if ckey=3 then read #1,using L1350: jn$,n$,mat a$,mat b,contact$,ph$,email$ eof L660: jn$=lpad$(trim$(jn$),6): holdjn$=jn$: goto L670
00390   if ckey=8 then let fnjob_srch(x$,fixgrid) : jn$=x$: goto EDITREC else !:
          if ckey=6 then goto XIT else !:
            if ckey=7 then gosub RECREATE_GRID !:
              goto ASKJOB
00400   if ckey=11 then goto JOB_LISTING
00410   goto ASKJOB
00420 ! ______________________________________________________________________
00430 ADDREC: ! 
00440   addjob=1: mat ta=(0): dup=0: jn$="": n$="": mat a$=(''): mat b=(0) !:
        contact$="": ph$="": email$=""
00450   fnTos(sn$="jobfm") !:
        respc=0 : frac=0 !:
        mylen=25 : mypos=mylen+2
00460   fnLbl(1,1,"Job Number:",mylen,1)
00470   fnTxt(1,mylen+3,6,6,1,"",0,"Enter the job number to be assigned to this new job.") !:
        resp$(respc+=1)=""
00480   fnCmdKey("&Next",1,1,0,"Enter job information.")
00490   fnCmdKey("&Cancel",5,0,1,"Returns to main screen.")
00500   fnAcs(sn$,0,mat resp$,ckey) ! ASK NEW JOB #
00510 ! 
00520   if ckey=5 then goto ASKJOB
00530   addjob=1
00540   jn$=lpad$(trim$(resp$(1)(1:6)),6)
00550   read #1,using L1350,key=jn$: tempjn$ nokey L570
00560   mat ml$(2) !:
        ml$(1)="A record with this number already exists!" !:
        ml$(2)="Select a different job number." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto ADDREC
00570 L570: k$=resp$(1)(7:36): mat ln=(0): mat ta=(0)
00580   goto L670
00590 EDITREC: ! 
00600   if trim$(jn$)="" or trim$(jn$)="0" then goto ASKJOB
00610   tjn$=jn$ : hjn$=""
00620   jn$=lpad$(trim$(jn$),6)
00630   read #1,using L1350,key=jn$: hjn$,n$,mat a$,mat b,contact$,ph$,email$ nokey L660
00640   holdjn$=jn$
00650   goto L670
00660 L660: mat ml$(2) !:
        ml$(1)="A record with this number does not exist!" !:
        ml$(2)="Select a differentjob number." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto ASKJOB
00670 L670: fnTos(sn$="jobedit") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+2
00680   fnLbl(1,1,"Job Number:",mylen,1)
00690 L690: fnTxt(1,mylen+3,6,6,1,"",0,"") !:
        resp$(respc+=1)=jn$
00700   fnLbl(2,1,"Job Name:",mylen,1)
00710   fnTxt(2,mylen+3,40,40,0,"",0,"Any name to identify the job.") !:
        resp$(respc+=1)=n$
00720   fnLbl(3,1,"Job Address:",mylen,1)
00730   fnTxt(3,mylen+3,30,30,0,"",0,"") !:
        resp$(respc+=1)=a$(1)
00740   fnLbl(4,1,"Job Address:",mylen,1)
00750   fnTxt(4,mylen+3,30,30,0,"",0,"") !:
        resp$(respc+=1)=a$(2)
00760   fnLbl(5,1,"City, State Zip:",mylen,1)
00770   fnTxt(5,mylen+3,30,30,0,"",0,"") !:
        resp$(respc+=1)=a$(3)
00780   fnLbl(6,1,"Est Completion Date:",mylen,1)
00790   fnTxt(6,mylen+3,10,10,0,"1",0,"The estimated completion date is only used on some reports and is optional.") !:
        resp$(respc+=1)=str$(b(1))
00800   fnLbl(7,1,"Contract Amount:",mylen,1)
00810   fnTxt(7,mylen+3,12,12,0,"10",0,"") !:
        resp$(respc+=1)=str$(b(2))
00820   fnLbl(8,1,"Billings to Date:",mylen,1)
00830   fnTxt(8,mylen+3,12,12,0,"10",0,"The billings to date field will be updated each time an invoice is processed.") !:
        resp$(respc+=1)=str$(b(3))
00840   fnLbl(9,1,"Billing Status:",mylen,1)
00850   fnTxt(9,mylen+3,2,2,0,"30",0,"The status code should be ????.") !:
        resp$(respc+=1)=str$(b(4))
00860   fnLbl(10,1,"Contact Name:",mylen,1)
00870   fnTxt(10,mylen+3,30,30,0,"",0,"") !:
        resp$(respc+=1)=contact$
00880   fnLbl(11,1,"Phone Number:",mylen,1)
00890   fnTxt(11,mylen+3,12,12,0,"",0,"") !:
        resp$(respc+=1)=ph$
00900   fnLbl(12,1,"E-mail Address:",mylen,1)
00910   fnTxt(12,mylen+3,60,60,0,"",0,"") !:
        resp$(respc+=1)=email$
00920   picture=0
00930   fnCmdKey("&Save",1,1,0,"Saves all changes.")
00940   fnCmdKey("&Review Category Records",10,0,0,"Review category records assigned to this job.")
00950   if addjob=0 then !:
          fnCmdKey("De&lete",4,0,0,"Deletes this job.")
00960   fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
00970   fnAcs(sn$,0,mat resp$,ckey) ! detail job screen     editrec
00980   if ckey=5 then goto ASKJOB
00990   jn$=lpad$(trim$(resp$(1)(1:6)),6)
01000   if ckey=4 then goto DELETE_ENTIRE_JOB
01010   n$=resp$(2) ! name
01020   a$(1)=resp$(3) ! address
01030   a$(2)=resp$(4) ! address
01040   a$(3)=resp$(5) ! city, st zip
01050   b(1)=val(resp$(6)) ! est completion date
01060   b(2)=val(resp$(7)) ! contrcat amount
01070   b(3)=val(resp$(8)) ! Billing date
01080   b(4)=val(resp$(9)) ! Billing status
01090   contact$=resp$(10)
01100   ph$=resp$(11)
01110   email$=resp$(12) ! e-mail address
01120   if addjob=1 then goto L1150
01130   rewrite #1,using L1350,key=jn$: jn$,n$,mat a$,mat b,contact$,ph$,email$ nokey L1150
01140   goto L1160
01150 L1150: write #1,using L1350: jn$,n$,mat a$,mat b,contact$,ph$,email$
01160 L1160: if ckey=10 then goto L1180 else goto L1250
01170 GET_CATEGORY_LISTING: ! 
01180 L1180: cn$=jn$: fncat_srch2(cn$,ckey,x)
01190   if ckey=97 then holdckey=ckey: goto ADDCAT
01200   if ckey=98 then holdckey=ckey: goto EDITCATEGORY
01210   if ckey=95 then cn$(1:6)=lpad$(rtrm$(jn$),6): gosub REVIEW_DETAILS : goto GET_CATEGORY_LISTING
01220   if ckey=96 then gosub DELETE_CATEGORY : goto GET_CATEGORY_LISTING
01230   if ckey=12 then gosub DUPLICATE_CATEGORIES: goto GET_CATEGORY_LISTING
01240   if ckey=6 then goto EDITREC
01250 L1250: goto ASKJOB
01260 ! wRD1$(5)="5. Reassign Transaction Addresses"  KJ  what to do with this
01270 ! ______________________________________________________________________
01280 DONE: ! 
01290   close #1: ioerr L1300
01300 L1300: close #4: ioerr L1310
01310 L1310: close #2: ioerr L1320
01320 L1320: if addjob=1 or cont=1 then goto L1820
01330   goto XIT
01340 ! ______________________________________________________________________
01350 L1350: form pos 1,c 6,c 40,3*c 30,n 6,2*pd 7.2,n 2,c 30,c 12,c 60
01360 L1360: form pos 118,2*pd 3
01370 L1370: form pos 1,c 11,c 25,11*pd 7.2,2*pd 2,2*pd 3
01380 L1380: form pos 1,c 12,c 6,n 5,pd 3,pd 2,n 6,4*pd 4.2,pd 5.2,c 30,pd 3
01390 L1390: form pos 86,pd 3
01400 ! ______________________________________________________________________
01410 DELETE_ENTIRE_JOB: ! 
01420   mat ml$(2) !:
        ml$(1)="You chosen to Delete job # "&jn$ !:
        ml$(2)="Did you wish to continue?" !:
        fnmsgbox(mat ml$,resp$,cap$,35)
01430   if resp$="Yes" then goto L1440 else goto ASKJOB
01440 L1440: delete #1,key=hjn$: nokey L1580
01450   restore #2,key>=hjn$&"     ": nokey L1580
01460 L1460: read #2,using L1370: cn$,k$,mat l,mat ta eof L1580
01470   ojn$=cn$(1:6)
01480   if hjn$<>ojn$ then goto L1580
01490   delete #2,key=cn$: nokey L1460
01500   gosub L1510: goto L1460
01510 L1510: adr=ta(1)
01520 L1520: if adr=0 then goto L1580
01530   read #3,using L1390,rec=adr: nta noRec L1580
01540   delete #3, rec=adr: 
01550   form pos 13,c 6
01560   adr=nta
01570   goto L1520
01580 L1580: return 
01590 ! ______________________________________________________________________
01600   close #1: ioerr L1610
01610 L1610: close #4: ioerr L1620
01620 L1620: open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno]",internal,input ioerr L1640
01630   close #1,free: 
01640 L1640: open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],SIZE=0,RecL=300",internal,output 
01650   cont=1
01660   close #2: ioerr L1670
01670 L1670: open #2: "Name=[Q]\PRmstr\JCCAT.H[cno]",internal,input ioerr L1690
01680   close #2,free: 
01690 L1690: open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],SIZE=0,RecL=123",internal,output 
01700   close #2: 
01710   close #3: ioerr L1730
01730 L1730: open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno]",internal,output ioerr L1750
01740   close #3,free: 
01750 L1750: open #3: "Name=[Q]\PRmstr\JCTRANS.h[cno],SIZE=0,RecL=88",internal,output 
01760   write #3,using L1380: " ","",mat tr," ",1
01770   close #3: 
01780   open #1: "Name=[Q]\PRmstr\JCPRH1.H[cno]",internal,output ioerr L1800
01790   close #1,free: 
01800 L1800: open #1: "Name=[Q]\PRmstr\JCPRH1.H[cno],SIZE=0,RecL=40",internal,output 
01810   close #1: 
01820 L1820: execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCIndx.h[cno],1,6,Replace,DupKeys -N"
01830   execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCINDX2.H[cno],7,25,Replace,DupKeys -N"
01840   execute "Index [Q]\PRmstr\JCCAT.H[cno],[Q]\PRmstr\CatIndx.h[cno],1,11,Replace,DupKeys -N"
01850   goto XIT
01860 ! ______________________________________________________________________
01870 JOB_LISTING: ! 
01880   fst=0
01890   fnopenprn
01900 L1900: read #1,using L1350,release: jn$,n$,mat a$,mat b,contact$,ph$,email$ eof L2620
01910   if fst=1 then goto L1940
01920   fst=1
01930   goto L1950
01940 L1940: pr #255: newpage
01950 L1950: jcp=1
01960   x2=0
01970   gosub L2330
01980   restore #2,key>=jn$&"     ": nokey L2080
01990 L1990: read #2,using L1370,release: cn$,k$,mat l eof L1900
02000   if cn$(1:6)><jn$ then goto L2080
02010   x1=x1+1
02020   ln$(1,x1)=cn$(7:11)
02030   ln$(2,x1)=k$
02040   for j=1 to 13
02050     ln(j,x1)=l(j)
02060   next j
02070   if x1<3 then goto L1990
02080 L2080: if x1=0 then goto L2260
02090   gosub L2110
02100   goto L2260
02110 L2110: x1=0
02120   for j=1 to 2
02130     pr #255,using L2140: sc2$(j),ln$(j,1),ln$(j,2),ln$(j,3)
02140 L2140: form pos 1,c 20,pos 23,3*c 35,skip 1
02150   next j
02160   for j=1 to 13
02170     pr #255,using L2180: sc2$(j+2),ln(j,1),ln(j,2),ln(j,3)
02180 L2180: form pos 1,c 20,pos 23,n 10.2,pos 58,n 10.2,pos 93,n 10.2,skip 1
02190   next j
02200   pr #255: 
02210   x2=x2+1
02220   mat ln$=("")
02230   mat ln=(0)
02240   return 
02250 ! ______________________________________________________________________
02260 L2260: if x2<3 then goto L2310
02270   pr #255: newpage
02280   x2=0
02290   jcp=0
02300   gosub L2330
02310 L2310: if cn$(1:6)<=jn$ then goto L1990
02320   goto L1900
02330 L2330: gosub HDR
02340   if jcp=0 then goto L2490
02350   pr #255,using L2360: sc1$(1),jn$
02360 L2360: form pos 1,c 20,pos 23,c 6,skip 1
02370   pr #255,using L2380: sc1$(2),n$
02380 L2380: form pos 1,c 20,pos 23,c 40,skip 1
02390   for j=1 to 3
02400     pr #255,using L2410: sc1$(j+2),a$(j)
02410 L2410: form pos 1,c 20,pos 23,c 30,skip 1
02420   next j
02430   for j=1 to 4
02440     pr #255,using L2450: sc1$(j+5),b(j)
02450 L2450: form pos 1,c 21,pos 23,n 12.2,skip 1
02460   next j
02470   pr #255: 
02480   pr #255: 
02490 L2490: return 
02500 ! ______________________________________________________________________
02510 L2510: fnTos(sn$="startprint") !:
        respc=0
02520   fnLbl(1,1,"Job # to Start:",16,right)
02530   fncmbjob(1,19,1)
02540   if hact$="" then !:
          resp$(respc+=1)="" else !:
          resp$(respc+=1)=hact$
02550   fnCmdKey("&Next",1,1,0,"Duplicate this job") !:
        fnCmdKey("&Search",8,0,0,"Search forjob record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new job information") !:
        fnCmdKey("&Cancel",5,0,1,"Returns to previous screen")
02560   fnAcs(sn$,0,mat resp$,ckey) ! ask job # to start printing
02570   if ckey=5 then goto ASKJOB
02580   jn$=lpad$(rtrm$(resp$(1)(1:6)),6)
02590   restore #1,key>=jn$: nokey L2510
02600   goto JOB_LISTING
02610 ! ______________________________________________________________________
02620 L2620: if x1>0 then gosub L2110
02630   on fkey 5 ignore 
02640   fncloseprn
02650   if fnprocess=1 then goto XIT else goto MENU1
02660 ! ______________________________________________________________________
02670 HDR: ! 
02680   pr #255,using "form pos 1,c 25": "Page "&str$(pgno+=1)&" "&date$
02690   pr #255: "\qc  {\f201 \fs24 \b "&env$('cnam')&"}"
02700   pr #255: "\qc  {\f221 \fs22 \b Job Cost Proof List}"
02710   pr #255: "\qc  {\f181 \fs16 \b "&cnvrt$("pic(zzzz/zz/zz)",dat)&"}"
02720   pr #255: "\ql   "
02730   return 
02740 ! ______________________________________________________________________
02750 REASSIGN: ! pr NEWPAGE
02760   restore #2,key>="           ": eof L2770
02770 L2770: read #2,using L1360: mat ta eof L2800
02780   rewrite #2,using L1360: 0,0
02790   goto L2770
02800 L2800: lr3=lrec(3)
02810   rewrite #3,using L1390,rec=1: lr3
02820   for j=2 to lr3
02830     read #3,using L2840,rec=j: cn$,nta noRec L2910
02840 L2840: form pos 13,c 11,pos 86,pd 3
02850     read #2,using L1360,key=cn$: mat ta nokey L2910
02860     if ta(1)=0 then ta(1)=j
02870     if ta(2)>0 then rewrite #3,using L1390,rec=ta(2): j
02880     ta(2)=j
02890     rewrite #2,using L1360,key=cn$: mat ta
02900     rewrite #3,using L1390,rec=j: 0
02910 L2910: next j
02920   goto MENU1
02930 ! ______________________________________________________________________
02940 DUPLICATE_CATEGORIES: ! 
02950   fnTos(sn$="duplicate") !:
        respc=0
02960   fnLbl(1,1,"Job # to Duplicate:",20,right)
02970   fncmbjob(1,23)
02980   if hact$="" then !:
          resp$(respc+=1)="" else !:
          resp$(respc+=1)=hact$
02990   fnCmdKey("&Next",1,1,0,"Duplicate this job") !:
        fnCmdKey("&Search",8,0,0,"Search forjob record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new job information") !:
        fnCmdKey("&Cancel",5,0,1,"Returns to previous screen")
03000   fnAcs(sn$,0,mat resp$,ckey) ! ask job # to duplicate
03010   if ckey=5 then goto ASKJOB
03020   djn$=lpad$(rtrm$(resp$(1)(1:6)),6)
03030   dup$=lpad$(rtrm$(djn$),6)&"     "
03040   restore #2,key>=dup$: nokey DUPLICATE_CATEGORIES
03050 L3050: read #2,using L1370,release: dupcn$,k$,mat l,mat ta eof GET_CATEGORY_LISTING
03060   if dupcn$(1:6)<>djn$ then goto GET_CATEGORY_LISTING
03070   cn$=lpad$(jn$&dupcn$(7:11),11)
03080   mat l=(0): mat ta=(0)
03090   read #2,using L1370,key=cn$: cn$,k$,mat l,mat ta nokey L3110 ! check for duplicate catergory records
03100   goto L3120
03110 L3110: write #2,using L1370: cn$,k$,mat l,mat ta
03120 L3120: goto L3050
03130 ! ______________________________________________________________________
03140 EDITCAT: ! 
03150   addcat=0 ! add code - used to tell other parts of the program, !:
        ! that I am currently adding a category record.
03160   fnTos(sn$="Cat-ask") !:
        respc=0
03170   fnLbl(1,1,"Category #:",10,right)
03180   fncmbcat(1,12)
03190   resp$(respc+=1)=cn$
03200   fnCmdKey("&Add",1,0,0,"Add a new category record." ) !:
        fnCmdKey("E&dit",2,1,0,"Access this record") !:
        fnCmdKey("&Next Record",3,0,0,"Access next record in category file.") !:
        fnCmdKey("&Search",8,0,0,"Search for category record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information") !:
        fnCmdKey("&Complete",6,0,1,"Returns to job screen.")
03210   fnAcs(sn$,0,mat resp$,ckey) ! ask category #
03220   hcat$=lpad$(rtrm$(resp$(1)(1:11)),11) !:
        cn$=lpad$(rtrm$(resp$(1)(1:11)),11)
03230   if ckey=1 then goto ADDCAT else !:
          if ckey=2 then cn$(1:6)=lpad$(rtrm$(jn$),6): goto EDITCATEGORY else !:
            if ckey=3 then read #2,using L1370: cn$,k$,mat l,mat ta eof L690: cn$=lpad$(trim$(cn$),11): holdcn$=cn$: goto EDITCATEGORY
03240   if ckey=8 then let fncategory_srch(x$,fixgrid) : cn$=x$: goto EDITCAT else !:
          if ckey=6 then goto ASKJOB else !:
            if ckey=7 then gosub RECREATE_CAT_GRID !:
              goto EDITCAT
03250 ADDCAT: ! 
03260   addcat=1
03270   fnTos(sn$="add-cat") !:
        respc=0
03280   fnLbl(1,1,"Category # to Add:",17,right)
03290   fncmbcategory(1,20)
03300 !  fnTxt(1,20,5,5,1,"30",0,"Category number must be numeric between 1 and 99999.") !:
        resp$(respc+=1)=""
03310   fnCmdKey("&Next",1,1,0,"Adds the new category record." ) !:
        fnCmdKey("&Cancel",5,0,1,"Stops without adding this category record and returns to category listing.")
03320   fnAcs(sn$,0,mat resp$,ckey) ! ask new category #
03330   if ckey=5 then goto GET_CATEGORY_LISTING
03340   cn$=jn$&lpad$(trim$(resp$(1)(1:5)),5)
03350   k$=resp$(1)(7:36)
03360   read #2,using L1370,key=cn$: tempcn$ nokey L3380
03370   mat ml$(2) !:
        ml$(1)="A category record with this number already exists!" !:
        ml$(2)="Select a different category number." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto ADDCAT
03380 L3380: mat l=(0): mat ta=(0)
03390   write #2,using L1370: cn$,k$,mat l,mat ta
03400   goto EDITCATEGORY
03410 EDITCATEGORY: ! 
03420   if trim$(cn$)="" or trim$(cn$)="0" then goto EDITCAT
03430   tcn$=cn$ : hcn$=""
03440   cnkey$=cn$
03450   read #2,using L1370,key=cnkey$: cn$,k$,mat l,mat ta eof EDITCAT nokey EDITCAT
03460   fnTos(sn$="catedit") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+2
03470   holdcn$=cn$
03480   fnLbl(1,1,"Category Number:",mylen,1)
03490   fnTxt(1,mylen+3,5,5,1,"",0,"") !:
        resp$(respc+=1)=cn$(7:11)
03500   fnLbl(2,1,"Description:",mylen,1)
03510   fnTxt(2,mylen+3,25,25,0,"",0,"Any name to identify the category.") !:
        resp$(respc+=1)=k$
03520   fnLbl(3,1,"Labor Estimate:",mylen,1)
03530   fnTxt(3,mylen+3,14,14,0,"10",0,"Labor estimate for this job category.") !:
        resp$(respc+=1)=str$(l(1))
03540   fnLbl(4,1,"Hours Estimate:",mylen,1)
03550   fnTxt(4,mylen+3,14,14,0,"10",0,"Hours estimate for this job category.") !:
        resp$(respc+=1)=str$(l(2))
03560   fnLbl(5,1,"Other Estimate:",mylen,1)
03570   fnTxt(5,mylen+3,14,14,0,"10",0,"Other estimate for this job category.") !:
        resp$(respc+=1)=str$(l(3))
03580   fnLbl(6,1,"Labor to Date:",mylen,1)
03590   fnTxt(6,mylen+3,14,14,0,"10",0,"Labor to Date for this job category.") !:
        resp$(respc+=1)=str$(l(4))
03600   fnLbl(7,1,"Hours to Date:",mylen,1)
03610   fnTxt(7,mylen+3,14,14,0,"10",0,"Hours to Date for this job category.") !:
        resp$(respc+=1)=str$(l(5))
03620   fnLbl(8,1,"Other to Date:",mylen,1)
03630   fnTxt(8,mylen+3,14,14,0,"10",0,"Other to Date for this job category.") !:
        resp$(respc+=1)=str$(l(6))
03640   fnLbl(9,1,"Labor Current Period:",mylen,1)
03650   fnTxt(9,mylen+3,14,14,0,"10",0,"Labor Current Period for this job category.") !:
        resp$(respc+=1)=str$(l(7))
03660   fnLbl(10,1,"Hours Current Period:",mylen,1)
03670   fnTxt(10,mylen+3,14,14,0,"10",0,"Hours Current Period for this job category.") !:
        resp$(respc+=1)=str$(l(8))
03680   fnLbl(11,1,"Other Current Period:",mylen,1)
03690   fnTxt(11,mylen+3,14,14,0,"10",0,"Other Current Period for this job category.") !:
        resp$(respc+=1)=str$(l(9))
03700   fnLbl(12,1,"Units Used or Completed:",mylen,1)
03710   fnTxt(12,mylen+3,14,14,0,"10",0,"Units required for this job category.") !:
        resp$(respc+=1)=str$(l(10))
03720   fnLbl(13,1,"Estimated Units:",mylen,1)
03730   fnTxt(13,mylen+3,14,14,0,"30",0,"Estimated units used so far.") !:
        resp$(respc+=1)=str$(l(11))
03740   fnLbl(14,1,"Labor Percent Complete:",mylen,1)
03750   fnTxt(14,mylen+3,5,5,0,"10",0,"% complete for labor.") !:
        resp$(respc+=1)=str$(l(12))
03760   fnLbl(15,1,"Other Percent Complete:",mylen,1)
03770   fnTxt(15,mylen+3,5,5,0,"10",0,"% complete for other.") !:
        resp$(respc+=1)=str$(l(13))
03780   fnCmdKey("&Save",1,1,0,"Saves all changes.")
03790   fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
03800   fnAcs(sn$,0,mat resp$,ckey) ! full edit on category  edit_category
03810   if ckey=5 then goto GET_CATEGORY_LISTING
03820   cn$=jn$&lpad$(trim$(resp$(1)(1:5)),5)
03830   k$=resp$(2) ! name
03840   if holdcn$=cn$ then goto L3870
03850   mat ml$(2) !:
        ml$(1)="You have chosen to change the category number from "&holdcn$ !:
        ml$(2)="to "&cn$&". Take OK to change, else Cancel." !:
        fnmsgbox(mat ml$,resp$,cap$,49)
03860   if resp$="OK" then goto L3870 else goto EDITCAT
03870 L3870: l(1)=val(resp$(3)) ! labor estimate
03880   l(2)=val(resp$(4)) ! hours estimate
03890   l(3)=val(resp$(5)) ! other estimate
03900   l(4)=val(resp$(6)) ! labor to date
03910   l(5)=val(resp$(7)) ! hours to date
03920   l(6)=val(resp$(8)) ! other to date
03930   l(7)=val(resp$(9)) ! labor currrent period
03940   l(8)=val(resp$(10)) ! hours currrent period
03950   l(9)=val(resp$(11)) ! other currrent period
03960   l(10)=val(resp$(12)) ! units
03970   l(11)=val(resp$(13)) ! estimated units
03980   l(12)=val(resp$(14)) ! labor % complete
03990   l(13)=val(resp$(15)) ! labor % complete
04000   rewrite #2,using L1370: cn$,k$,mat l,mat ta
04010   if holdckey=98 then holdckey=0: goto GET_CATEGORY_LISTING ! return to caterory grid listing from editcategory
04020   if holdckey=99 then holdckey=0: goto GET_CATEGORY_LISTING
04030   if addcat=1 then goto ADDCAT
04040   goto ASKJOB
04050 ! ______________________________________________________________________
04060 REVIEW_DETAILS: ! 
04070   fnTos(sn$="DetailSrch")
04080   ch2$(1)="Rec #": ch2$(2)="Reference #": ch2$(3)="Job #" !:
        ch2$(4)="Cat #" !:
        ch2$(5)="Sub #": ch2$(6)="P/R Dept": ch2$(7)="Date" !:
        ch2$(8)="Reg Hrs": ch2$(9)="OT Hours": ch2$(10)="Units" !:
        ch2$(11)="Pay Tax": ch2$(12)="Amount" !:
        ch2$(13)="Description" !:
        mat ch2$(13) ! : Mat CM2$(13) : Mat ITEM2$(13)
04090   cm2$(1)="30": cm2$(2)="": cm2$(3)="30" !:
        cm2$(4)="30" !:
        cm2$(5)="30": cm2$(6)="30": cm2$(7)="3" !:
        cm2$(8)="32": cm2$(9)="32": cm2$(10)="10" !:
        cm2$(11)="10": cm2$(12)="10": cm2$(13)=""
04100   fnflexinit1('Cat',1,1,10,70,mat ch2$,mat cm2$,1,usefile)
04110 ! Restore #2,Key>=JN$&CN$: Nokey 4200
04120   catkey$=cn$ ! $(TRIM$(JN$),6)&LPAD$(TRIM$(CN$),5)
04130 READ_FILE: ! 
04140   read #2,using L1360,key=catkey$: mat ta nokey L4160
04150   goto L4170
04160 L4160: mat ml$(2) !:
        ml$(1)="There no transactions for this category!" !:
        ml$(2)="Select a different category." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto GET_CATEGORY_LISTING
04170 L4170: nta=ta(1)
04180 L4180: if nta=0 then goto L4240
04190   read #3,using L1380,rec=nta: eno$,jno$,mat tr,pd$,nta
04200   item2$(1)=str$(rec(3)): item2$(2)=eno$: item2$(3)=jno$: item2$(4)=str$(tr(1)) !:
        item2$(5)=str$(tr(2)): item2$(6)=str$(tr(3)) !:
        item2$(7)=str$(tr(4)) !:
        item2$(8)=str$(tr(5)) !:
        item2$(9)=str$(tr(6)): item2$(10)=str$(tr(7)) !:
        item2$(11)=str$(tr(8)) : item2$(12)=str$(tr(9)) !:
        item2$(13)=pd$
04210   fnflexadd1(mat item2$)
04220   goto L4180
04230 ! ______________________________________________________________________
04240 L4240: fnCmdKey("&Add",1,0,0,"Add a new category record." ) !:
        fnCmdKey("&Delete",4,0,0,"Deletes the highlited record") !:
        fnCmdKey("&Refresh",7,0,0,"Updates search grids and combo boxes with new category information") !:
        fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
04250   fnAcs(sn$,0,mat resp$,ckey) ! review_details  grid of transactions
04260   if ckey=5 then goto GET_CATEGORY_LISTING
04270   detaileditrec=val(resp$(1))
04280   if ckey=1 then adddetails=1: mat tr=(0): eno$="": jno$=jn$: goto EDIT_DETAILS
04290   if ckey=2 then read #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$,nta : editdetails=1 : goto EDIT_DETAILS
04300   if ckey=4 then read #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$,nta !:
          eno$=jno$=pd$="": mat tr=(0) !:
          rewrite #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$,nta !:
          goto GET_CATEGORY_LISTING
04310   goto GET_CATEGORY_LISTING
04320 EDIT_DETAILS: ! 
04330   fnTos(sn$="detailedit") !:
        respc=0 : frac=0 !:
        mylen=28 : mypos=mylen+2
04340   holdcn$=cn$
04350   fnLbl(1,1,"Ref/Employee #:",mylen,1)
04360   fnTxt(1,mylen+3,12,12,1,"",0,"Can contain a check number, an employee number, or some other reference number.") !:
        resp$(respc+=1)=eno$
04370   fnLbl(2,1,"Job #:",mylen,1)
04380   fnTxt(2,mylen+3,6,6,0,"",0,"Job # to which this charge or cost was posted.") !:
        resp$(respc+=1)=jno$
04390   fnLbl(3,1,"Category #:",mylen,1)
04400   fnTxt(3,mylen+3,6,6,0,"",0,"Labor estimate for this job category.") !:
        resp$(respc+=1)=str$(tr(1))
04410   fnLbl(4,1,"Sub-Category:",mylen,1)
04420   fnTxt(4,mylen+3,2,2,0,"10",0,"Sub category to which it should be classified") !:
        resp$(respc+=1)=str$(tr(2))
04430   fnLbl(5,1,"P/R Dept:",mylen,1)
04440   fnTxt(5,mylen+3,3,3,0,"30",0,"Other estimate for this job category.") !:
        resp$(respc+=1)=str$(tr(3))
04450   fnLbl(6,1,"Date:",mylen,1)
04460   fnTxt(6,mylen+3,8,8,0,"1",0,"Labor to Date for this job category.") !:
        resp$(respc+=1)=str$(tr(4))
04470   fnLbl(7,1,"Reg Hrs:",mylen,1)
04480   fnTxt(7,mylen+3,10,10,0,"32",0,"Regular hours associated with this cost.") !:
        resp$(respc+=1)=str$(tr(5))
04490   fnLbl(8,1,"O/T Hrs:",mylen,1)
04500   fnTxt(8,mylen+3,10,10,0,"32",0,"Overtime hours associated with is cost.") !:
        resp$(respc+=1)=str$(tr(6))
04510   fnLbl(9,1,"Units:",mylen,1)
04520   fnTxt(9,mylen+3,10,10,0,"30",0,"Only applicable if units are being tracked.") !:
        resp$(respc+=1)=str$(tr(7))
04530 ! 
04540   fnLbl(10,1,"Payroll Tax:",mylen,1)
04550   fnTxt(10,mylen+3,14,14,0,"10",0,"If this cost is for earnings, the associated payroll tax expense should be entered here.") !:
        resp$(respc+=1)=str$(tr(8))
04560   fnLbl(11,1,"Amount:",mylen,1)
04570   fnTxt(11,mylen+3,14,14,0,"10",0,"Amount of cost being charged to the job.") !:
        resp$(respc+=1)=str$(tr(9))
04580   fnLbl(12,1,"Description:",mylen,1)
04590   fnTxt(12,mylen+3,30,30,0,"",0,"Brief description of cost.") !:
        resp$(respc+=1)=pd$
04600   fnCmdKey("&Save",1,1,0,"Saves all changes.")
04610   fnCmdKey("&Cancel",5,0,1,"Stops without applying any changes.")
04620   fnAcs(sn$,0,mat resp$,ckey) ! full edit details
04630   if ckey=5 then goto REVIEW_DETAILS
04640   eno$=resp$(1) ! employee #/ ref #
04650   jno$=resp$(2) ! job number
04660   tr(1)=val(resp$(3)) ! category
04670   tr(2)=val(resp$(4)) ! sub-category
04680   tr(3)=val(resp$(5)) ! pr dept
04690   tr(4)=val(resp$(6)) ! date
04700   tr(5)=val(resp$(7)) ! regular hours
04710   tr(6)=val(resp$(8)) ! ot hours
04720   tr(7)=val(resp$(9)) ! units
04730   tr(8)=val(resp$(10)) ! paroll taxes
04740   tr(9)=val(resp$(11)) ! amount
04750   pd$=resp$(12) ! description
04760   if editdetails=1 then rewrite #3,using L1380,rec=detaileditrec: eno$,jno$,mat tr,pd$ : editdetails=0: goto REVIEW_DETAILS
04770   if adddetails=1 then goto L4780 else goto L4860
04780 L4780: read #2,using L1360,key=cn$: mat ta
04790 L4790: lrec3=lrec(3)+1
04800   write #3,using L1380,rec=lrec3: eno$,jno$,mat tr,pd$,0 ioerr L4790
04810   if ta(2)>0 then rewrite #3,using L1390,rec=ta(2): lrec3
04820   if ta(1)=0 then ta(1)=ta(2)=lrec3
04830   if ta(2)=0 then ta(2)=lrec3
04840   rewrite #2,using L1360,key=cn$: mat ta
04850   adddetails=0: goto REVIEW_DETAILS
04860 L4860: ! 
04870   goto REVIEW_DETAILS
04880 ! ______________________________________________________________________
04890 L4890: if err=4152 then goto L1640 else goto ERTN
04900   if err=4152 then goto L1690 else goto ERTN
04910   if err=4152 then goto L1750 else goto ERTN
04920   if err=4152 then goto L1800 else goto ERTN
04930   if err=4152 then goto L1820 else goto ERTN
04940 ! ______________________________________________________________________
04950 ! <Updateable Region: ERTN>
04960 ERTN: fnerror(program$,err,line,act$,"xit")
04970   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
04980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
04990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
05000 ERTN_EXEC_ACT: execute act$ : goto ERTN
05010 ! /region
05020 ! ______________________________________________________________________
05030 XIT: fnxit
05040 ! ______________________________________________________________________
05050 ! ______________________________________________________________________
05060 ! ______________________________________________________________________
05070 RECREATE_GRID: ! 
05080   close #1: ioerr L5090
05090 L5090: close #4: ioerr L5100
05100 L5100: fnjob_srch(x$,99)
05110   df$="[Q]\PRmstr\jcmstr.h[cno]" : if$="[Q]\PRmstr\jcindx.h[cno]" !:
        fncombof("CJob.h[cno]",lyne,mypos,43,df$,1,6,7,25,if$,1) !:
        fncombof("CJobALL.h[cno]",lyne,mypos,43,df$,1,6,7,25,if$,2)
05120   execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCIndx.h[cno],1,6,Replace,DupKeys -N" ioerr L5130
05130 L5130: execute "Index [Q]\PRmstr\JCMSTR.h[cno],[Q]\PRmstr\JCINDX2.H[cno],7,25,Replace,DupKeys -N" ioerr L5140
05140 L5140: open #1: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCIndx.h[cno],Shr",internal,outIn,keyed ioerr XIT
05150   open #4: "Name=[Q]\PRmstr\JCMSTR.h[cno],KFName=[Q]\PRmstr\JCINDX2.H[cno],Shr",internal,outIn,keyed ioerr XIT
05160   return 
05170 RECREATE_CAT_GRID: ! 
05180   close #2: ioerr L5190
05190 L5190: fncategory_srch(x$,99)
05200   df$="[Q]\PRmstr\jccat.h[cno]" : if$="[Q]\PRmstr\catindx.h[cno]" !:
        fncombof("CCat.h[cno]",lyne,mypos,43,df$,1,11,12,25,if$,1) !:
        fncombof("CCatALL.h[cno]",lyne,mypos,43,df$,1,11,12,25,if$,2)
05210   execute "Index [Q]\PRmstr\JCCAT.H[cno],[Q]\PRmstr\CatIndx.h[cno],1,11,Replace,DupKeys -N" ioerr L5220
05220 L5220: open #2: "Name=[Q]\PRmstr\JCCAT.H[cno],KFName=[Q]\PRmstr\CatIndx.h[cno],Shr",internal,outIn,keyed ioerr XIT
05230   return 
05240 DELETE_CATEGORY: ! 
05250   read #2,using L1360,key=cn$: mat ta nokey L5360
05260   if sum(ta)<>0 then goto L5270 else goto L5290
05270 L5270: mat ml$(2) !:
        ml$(1)="This category contains detail transactions about the job!" !:
        ml$(2)="Are you sure you wish to delete it?" !:
        fnmsgbox(mat ml$,resp$,cap$,33)
05280   if resp$="OK" then goto L5290 else goto GET_CATEGORY_LISTING
05290 L5290: delete #2,key=cn$: ioerr GET_CATEGORY_LISTING
05300   if sum(ta)>0 then goto L5310 else goto L5360
05310 L5310: nta=ta(1)
05320 L5320: if nta>0 then holdnta=nta: goto L5330 else goto L5360
05330 L5330: read #3,using L2840,rec=nta: cn$,nta noRec L5360
05340   delete #3,rec=holdnta: ioerr L5350
05350 L5350: goto L5320
05360 L5360: return 
