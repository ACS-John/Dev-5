00010 ! Replace S:\acsGL\acglWkSh
00020 ! pr Trial Balance Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit,fnopenprn,fncloseprn,fncno,fnerror,fnprocess,fnpedat$, fnconsole,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs
00050   on error goto Ertn
00060 ! ______________________________________________________________________
00070   dim a$(9)*3,glsum$(2)*12,u$*12,address$(2)*40,b$(2)*12,c$*5,d(2),p$*62
00080   dim cnam$*40,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
00090   dim cogl$*12,pedat$*20,cap$*128
00100 ! ______________________________________________________________________
00110   right=1
00120   fntop(program$,cap$="Trial Balance Worksheet")
00130   fnconsole(off=0)
00140   first=1
00150   fncno(cno,cnam$)
00160   open #20: "Name=[Q]\GLmstr\Company.h[cno]",internal,input,relative  !:
        read #20,using 'Form POS 150,2*N 1',rec=1: d(1) !:
        read #20,using 'Form POS 176,C 12',rec=1: cogl$ !:
        close #20: 
00170   a$(1)="C/D" : a$(2)="C/R" : a$(3)="ADJ" !:
        a$(4)="A/P" : a$(5)="PR" : a$(6)="A/R" !:
        a$(7)="S/J" : a$(8)="P/J" : a$(9)=" "
00180   p$="|        *         |         *         |         *        |"
00190   open #glmstr=1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\glIndex.h[cno],Shr",internal,input,keyed 
00200   if fnprocess=1 or d(1)=0 then goto START_REPORT !:
          ! Skip Cost Center Question if not applicable or in Automatic Processing
00210 SCREEN1: ! 
00220   fnTos(sn$="GLTBwksheet") !:
        mylen=12 : mypos=mylen+2
00230   fnLbl(1,1,"Cost Center:",mylen,right)
00240   fnTxt(1,mypos,3,0,0,'number') !:
        resp$(1)=""
00250   fnCmdSet(3)
00260   fnAcs(sn$,0,mat resp$,ckey)
00270   if ckey=5 then goto XIT
00280   costcent=val(resp$(1)) !:
        n$=lpad$(str$(costcent),3)&"     0  0"
00290   read #glmstr,using 'Form POS 1,C 12,C 50,POS 81,2*PD 6.2',key>=n$: n$,d$,bb,cb nokey SCREEN1
00300 ! pr NEWPAGE
00310 ! pr f "10,2,C 78": "Enter the Cost Center (Fund or Company Num) If you wish to only pr a work"
00320 ! pr f "11,2,C 69": "sheet for one Cost Center (Blank for All)"
00330 ! pr f "13,34,C 11,B,5": "Cancel (F5)"
00340 ! Input Fields "11,70,Nz 3,EUT,N": COSTCENT Conv 250
00350 ! If CMDKEY=5 Then Goto XIT
00360 ! n$=LPAD$(STR$(COSTCENT),3)&"     0  0"
00370 ! Read #GLmstr,Using 'Form POS 1,C 12,C 50,POS 81,2*PD 6.2',Key>=N$: N$,D$,BB,CB Nokey SCREEN1
00380 START_REPORT: ! 
00390 ! fnwait !:
        ! pr f "10,25,C 30,R,N": "G/L WORKSHEET IN PROCESS" !:
        ! pr f "12,20,Cc 30,B,5": "Cancel (F5)" !:
        on fkey 5 goto END1
00400   fnopenprn
00410   gosub HDR
00420   if fnprocess=1 or d(1)=0 then goto READ_GLMSTR else goto L500
00430 ! ______________________________________________________________________
00440 READ_GLMSTR: ! 
00450   read #glmstr,using 'Form POS 1,C 12,C 50,POS 81,2*PD 6.2': n$,d$,bb,cb eof END1
00460   if first=0 and n$(1:3)<>oldn$(1:3) then gosub TOTALS
00470   first=0
00480   oldn$=n$
00490   if costcent><0 and val(n$(1:3))><costcent then goto END1
00500 L500: dno=val(n$(1:3)) : ano=val(n$(4:9)) : sno=val(n$(10:12))
00510   gosub SOMETHING
00520   if n$<>cogl$ then goto READ_GLMSTR
00530   pr #255,using L540: "**Net Income or Loss",drtotal+crtotal
00540 L540: form pos 17,c 20,pos 38,pic(---,---,---.##),skip 2
00550   goto READ_GLMSTR
00560 ! ______________________________________________________________________
00570 TOTALS: pr #255: 
00580   pr #255: tab(13);"Worksheet Proof Totals";
00590   oldn$=n$
00600   pr #255,using L610: drtotal,crtotal,page$ !:
        pr #255: ""
00610 L610: form pos 39,pic(---,---,---.##),pos 53,pic(---,---,---.##),c 80
00620   if end1=1 then return 
00630   drtotal=crtotal=0
00640   pr #255: newpage
00650   gosub HDR
00660   return 
00670 ! ______________________________________________________________________
00680 END1: end1=1
00690   gosub TOTALS
00700   close #glmstr: 
00710   fncloseprn
00720   goto XIT
00730 ! ______________________________________________________________________
00740 HDR: ! 
00750   pr #255,using "Form pos 1,C 20,Cc 72": date$("mm/dd/yy"),cnam$
00760   pr #255,using "form pos 1,c 20,cc 72": time$,cap$
00770   pr #255,using 'Form Pos 21,Cc 72': fnpedat$
00780   pr #255: ""
00790   pr #255: tab(6);"Account";
00800   pr #255: tab(50);"Trial Balance";tab(73);"Adjustments";
00810   pr #255: tab(90);"Profit and Loss";tab(112);"Balance Sheet"
00820   pr #255,using 'Form POS 6,C 6,POS 17,C 11,POS 47,C 5,POS 60,C 6,POS 69,C 5,SKIP 0': "Number","Description","Debit","Credit","Debit"
00830   pr #255,using 'Form POS 80,C 6,POS 89,C 5,POS 101,C 6,POS 111,C 5,POS 121,C 6': "Credit","Debit","Credit","Debit","Credit" !:
        pr #255: "" !:
        pr #255: ""
00840   return 
00850 ! ______________________________________________________________________
00860 SOMETHING: ! 
00870   if cb<0 then !:
          crtotal+=cb : p1=54 else !:
          drtotal+=cb : p1=40
00880   pr #255,using 'Form Pos 1,pic(ZZZ),X 1,pic(ZZZZZZ),X 1,pic(ZZZ),X 2,C 22,POS P1,N 12.2,POS 68,C 62': dno,ano,sno,d$(1:22),cb,p$ pageoflow PGOF !:
        pr #255,using "Form Pos 68,C 62": p$ pageoflow PGOF
00890   return 
00900 ! ______________________________________________________________________
00910 PGOF: pr #255: newpage : gosub HDR : continue 
00920 ! ______________________________________________________________________
00930 XIT: fnxit
00940 ! ______________________________________________________________________
00950 ! <Updateable Region: ERTN>
00960 ERTN: fnerror(program$,err,line,act$,"xit")
00970   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00980   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00990   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01000 ERTN_EXEC_ACT: execute act$ : goto ERTN
01010 ! /region
01020 ! ______________________________________________________________________
