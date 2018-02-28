00010 ! Replace S:\acsPR\newjcSCMint
00020 ! Sub-Category Description File
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenwin,fnwait,fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit,fnTos,fnLbl,fnCmdKey,fnAcs,fnsubcat_srch ,fncmbsubcat,fnTxt,fnmsgbox
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim iom$(4),scm$(4)*27,resp$(5)*50
00080   dim cde$*3,des$*30,cnam$*40,sc$*3,cnt$*25,cap$*128,message$*40
00090 ! ______________________________________________________________________
00100   fntop("S:\acsPR\jcSCMaint",cap$="Sub-Category Description")
00110   fncno(cno,cnam$)
00120 ! 
00130   open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],KFName=[Q]\PRmstr\SCIndex.h[cno],Shr",internal,outIn,keyed 
00140 ! ______________________________________________________________________
00150 ASKSUBCAT: ! 
00160 L160: fnsubcat_srch(cde$,ckey,fixgrid)
00170   cde$=lpad$(rtrm$(cde$),3)
00180   if ckey=97 then ti1=addsubcat=1 : goto ADDREC else !:
          if ckey=98 then goto EDITREC else !:
            if ckey=3 then read #1,using "Form POS 1,C 3,C 25": cde$,des$ eof L160: cde$=uprc$(lpad$(rtrm$(cde$),3)): holdcde$=cde$: goto EDITREC
00190   if ckey=96 then goto DELETE_RECORD
00200   if ckey=94 then gosub SUBCAT_LISTING
00210   if ckey=5 then goto XIT
00220   goto ASKSUBCAT
00230 ! ______________________________________________________________________
00240 ADDREC: ! 
00250   cde$="": des$=""
00260   fnTos(sn$="Ask-sub-cat") !:
        respc=0
00270   fnLbl(1,1,"Sub-category #:",16,right)
00280   fnTxt(1,19,3,3,0,"30",0,"Assign any number that has not been used before.") !:
        resp$(respc+=1)=""
00290   fnCmdKey("&Next",1,1,0,"Record this sub category record.") !:
        fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
00300   fnAcs(sn$,0,mat resp$,ckey) ! add sub-category #
00310   if ckey=5 then goto ASKSUBCAT
00320   cde$=lpad$(rtrm$(resp$(1)),3)
00330   read #1,using L760,key=cde$: cde$,des$ nokey L350
00340   goto EDITREC
00350 L350: write #1,using L760: cde$,des$: new1=1
00360   goto EDITREC
00370   mat ml$(2) !:
        ml$(1)="A record with this number already exists!" !:
        ml$(2)="Select a different subcategory number." !:
        fnmsgbox(mat ml$,resp$,cap$,48) !:
        goto ADDREC
00380 ! ______________________________________________________________________
00390 EDITREC: ! 
00400   holdcde$=cde$
00410   read #1,using L760,key=cde$: cde$,des$ nokey L420
00420 L420: fnTos(sn$="Edit-sub-cat") !:
        respc=0
00430   fnLbl(1,1,"Sub-category #:",16,right)
00440   fnTxt(1,19,3,3,0,"30",0,"Can be any three digit number.") !:
        resp$(respc+=1)=cde$
00450   fnLbl(2,1,"Description:",16,right)
00460   fnTxt(2,19,30,30,0,"",0,"") !:
        resp$(respc+=1)=des$
00470   fnCmdKey("&Next",1,1,0,"Record any changes & return to main screen.") !:
        fnCmdKey("&Add",2,0,0,"Save these changes and then add a new record." ) !:
        fnCmdKey("&Delete",4,0,0,"Delete this sub-category record." ) !:
        fnCmdKey("E&xit",5,0,1,"Returns to main screen.")
00480   fnAcs(sn$,0,mat resp$,ckey) ! edit sub-category
00490   if ckey=5 then goto ASKSUBCAT
00500   cde$=lpad$(rtrm$(resp$(1)),3)
00510   des$=resp$(2)
00520   if ckey=4 then goto DELETE_RECORD
00530   if holdcde$<>cde$ then goto L540 else goto L560
00540 L540: mat ml$(2) !:
        ml$(1)="You are attempting to change the sub-category # from "&holdcde$ !:
        ml$(2)="to "&cde$&".  Take OK to continue, else cancel." !:
        fnmsgbox(mat ml$,resp$,cap$,48)
00550   if resp$="OK" then goto L560 else goto EDITREC
00560 L560: rewrite #1,using L760,key=cde$: cde$,des$ nokey L590
00570   if ckey=2 then goto ADDREC
00580   goto ASKSUBCAT
00590 L590: write #1,using L760: cde$,des$
00600   if ckey=2 then goto ADDREC
00610   goto ASKSUBCAT
00620 ! ______________________________________________________________________
00630 DELETE_RECORD: ! 
00640   delete #1,key=cde$: nokey L650
00650 L650: goto ASKSUBCAT
00660 ! ______________________________________________________________________
00670 ! the following lines are not used and are only there if necessary to fix a file
00680   close #1: ioerr L690
00690 L690: open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno]",internal,output 
00700   close #1,free: ioerr L710
00710 L710: open #1: "Name=[Q]\PRmstr\SCMSTR.h[cno],SIZE=0,RecL=33",internal,output 
00720   close #1: 
00730   execute "Index [Q]\PRmstr\SCMSTR.h[cno],[Q]\PRmstr\SCIndex.h[cno],1,3,Replace,DupKeys"
00740   goto XIT
00750 ! ______________________________________________________________________
00760 L760: form pos 1,c 3,c 30
00770 ! ______________________________________________________________________
00780 SUBCAT_LISTING: ! 
00790   on fkey 5 goto L910
00800   fnopenprn
00810   restore #1,key>="   ": nokey ASKSUBCAT
00820   gosub L950
00830 L830: read #1,using L760: cde$,des$ eof L910
00840   pr #255,using L850: cde$,des$ pageoflow L870
00850 L850: form pos 16,c 5,c 30,skip 1
00860   goto L830
00870 L870: pr #255: newpage
00880   gosub L950
00890   continue 
00900 ! ______________________________________________________________________
00910 L910: on fkey 5 ignore 
00920   fncloseprn
00930   goto ASKSUBCAT
00940 ! ______________________________________________________________________
00950 L950: pr #255,using L960: date$,cnam$
00960 L960: form pos 1,c 8,cc 52
00970   pr #255,using L980: time$,"Sub-Category File Listing"
00980 L980: form pos 1,c 8,pos 11,cc 50,skip 2
00990   pr #255: tab(15);"Code  Description"
01000   pr #255: tab(15);"____  ______________________________"
01010   return 
01020 ! ______________________________________________________________________
01030 XIT: fnxit
01040 ! ______________________________________________________________________
01050 SRCHEND: ! 
01060   close #win: ioerr L1070
01070 L1070: goto ASKSUBCAT
01080 ! ______________________________________________________________________
01090   goto ASKSUBCAT
01100 ! ______________________________________________________________________
01110 ! <Updateable Region: ERTN>
01120 ERTN: fnerror(program$,err,line,act$,"xit")
01130   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
01140   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01150   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01160 ERTN_EXEC_ACT: execute act$ : goto ERTN
01170 ! /region
01180 ! ______________________________________________________________________
