00020 ! Replace S:\acsTM\CliLabel
00040 ! pr client labels
00060 ! _______________________________________________________________
00080   library 'S:\Core\Library': fnopenprn,fncloseprn,fnaddlabel,fnlabel,fncno,fnxit,fntop
00100   fntop(program$,cap$="Client Labels")
00120   on error goto L850
00140 ! _______________________________________________________________
00160   dim label_text$(5)*40,cap$*128
00180   dim z$*5,a$(3)*30,prg$*20,bk$(20)*30,nam$*25,a1$*30
00200   dim ma(20),app(20),totalapp(20),totalma(20)
00220   dim a2$*30,a3$*30
00240 ! _______________________________________________________________
00260   on fkey 5 goto L710
00280   fncno(cno,cnam$)
00300 ! needs something like Let FNPRG$
00320   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.H"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndex.H"&env$('cno')&",Shr",internal,input,keyed 
00340   open #32: "Name="&env$('Q')&"\TMmstr\CLmstr.H"&env$('cno')&",KFName="&env$('Q')&"\TMmstr\CLIndx2.H"&env$('cno')&",Shr",internal,input,keyed 
00360 L190: pr newpage
00380   if prtall=1 then let fnopenprn
00400   close #101: ioerr L220
00420 L220: open #101: "SROW=10,SCOL=20,EROW=12,ECOL=60,BORDER=DR,CAPTION=PRINT LABELS FOR SELECTED CLIENTS",display,outin 
00440   pr f "11,21,C 14,N": "Client Number:"
00460   pr f "13,22,C 32,B,5": "Cancel (Esc)"
00480   pr f "14,22,C 32,B,6": "Search (F6)"
00500   pr f "15,22,C 32,B,10": "Print all (F10)"
00520 L250: input fields "11,46,C 5,UE,N": k$
00540   if cmdkey=99 or cmdkey=5 then goto XIT
00560   if cmdkey=10 then let prtall=1 : goto PRINT_EM
00580   if cmdkey=6 then goto SRCH1
00600   let z$=lpad$(rtrm$(k$),5)
00620   read #1,using F_CLIENT,key=z$: z$,mat a$,mat app,mat ma nokey L250
00640   gosub PRINT_LABEL : goto L190
00660 ! ______________________________________________________________________
00680   dim st2$(20)*24,ap2(20),ma2(20)
00700 ! ______________________________________________________________________
00720 PRINT_EM: pr newpage
00740   pr f "10,25,c 50,n": "PRINT CLIENT LABELS IN PROCESS"
00760   pr f "12,34,C 11,B,5": "Cancel (F5)"
00780   let j=1
00800 READ_CLIENT: ! 
00820   read #1,using F_CLIENT: z$,mat a$,mat app,mat ma,mat ap2,mat ma2 eof L710 ioerr L850
00840   if trim$(a$(3))='' then goto READ_CLIENT ! skip clients with no CSZ
00860 ! If APP(4)<1 Then Goto 440
00880   for j=1 to min(20, udim(app))
00900     if app(j)<>0 then let totalapp(j)+=1
00920     if ma(j)<>0 then let totalma(j)+=1
00940   next j
00960 ! if ma(14)<>0 then goto L470 else goto READ_CLIENT
00980 !     ONLY pr CERTAIN TYPES OF MAINTENANCE - type 16=MC
01000 !     app(x) = system
01020 !     ma(x) = support
01040 ! end if
01060 F_CLIENT: form pos 1,c 5,3*c 30,pos 375,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
01080 L470: ! 
01100   let p1=pos(a$(3),",",1)
01120   if p1=0 then goto L520
01140   st$=uprc$(ltrm$(rtrm$(a$(3)(p1+1:p1+3))))
01160   if st$="MO" then goto L520
01180 L520: gosub PRINT_LABEL
01200   goto READ_CLIENT
01220 ! ______________________________________________________________________
01240 PRINT_LABEL: ! r:
01260   a$(1)=rtrm$(a$(1))
01280   l2=len(a$(1))
01300   l1=l2-3
01320   c$=a$(1)(l1:l2)
01340   if c$="CPA." or c$=" CPA" then goto L640
01360   if c$="INC." or c$=" INC" then goto L640
01380   let p1=pos(a$(1),",",1)
01400   if p1=0 then goto L640
01420   if a$(1)(p1+1:p1+1)=" " then let p2=2 else let p2=1
01440   a$(1)=rtrm$(a$(1)(p1+p2:30))&" "&a$(1)(1:p1-1)
01460 L640: ! 
01480   mat label_text$=("")
01500   for j=1 to 3 : label_text$(j)=a$(j) : next j
01520   fnaddlabel(mat label_text$)
01540   labelcount+=1
01560   return  ! /r
01580 ! ______________________________________________________________________
01600 L710: close #1: ioerr ignore
01620   fnlabel(win=101,cap$, mat linestyle$, cp, nw)
01640   labelcount=0
01660   if uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" or uprc$(rtrm$(prg$))="S:\Time Management\Client Legacy" then chain prg$ ! XXX
01680 XIT: ! 
01700   if labelcount>0 then 
01720     fnlabel(win=101,cap$, mat linestyle$, cp, nw)
01740   end if 
01760   fnxit
01780 L850: if err=61 then pr f "23,3,C 75,N": "THIS PROGRAM IS TRYING TO ACCESS A RECORD THAT IS IN USE!" else goto L870
01800   goto L910
01820 L870: pr newpage
01840   if err=4148 then pr f "23,3,C 78,N": "THIS PROGRAM IS TRYING TO ACCESS A FILE THAT IS IN USE AND CANNOT BE SHARED!" else goto L900
01860   goto L910
01880 L900: pr f "23,3,C 75,N": "YOU HAVE A WORKSTATION BASIC ERROR # "&str$(err)&" AT LINE # "&str$(line)&"."
01900 L910: pr f "24,3,C 70,N": "PRESS ENTER TO RETRY; ELSE ENTER  Q  TO QUIT"
01920   input fields "24,60,C 1,N": quitcode$
01940   if rtrm$(uprc$(quitcode$))="Q" then goto XIT
01960   pr f "23,3,C 78,N": ""
01980   pr f "24,3,C 78,N": ""
02000   retry 
02020 SRCH1: s1=1 ! NAME SEARCH
02040   open #127: "SROW=1,SCOL=1,EROW=24,ECOL=80",display,outin  ! SAVE SCREEN
02060 L990: pr #127: newpage
02080   close #101: ioerr L1010
02100 L1010: open #101: "SROW=6,SCOL=3,EROW=08,ECOL=78,BORDER=DR,CAPTION=BUSINESS NAME SEARCH",display,outin 
02120   let prtall=0
02140   pr f "7,4,C 55,H,N": "Enter beginning search info. or blank for all:"
02160   pr f "9,32,C 16,R,N": "Press F5 to stop"
02180 L1050: input fields "7,50,C 25,UE,N": nam$
02200   if cmdkey=5 then goto SRCHEND
02220   let nam$=rtrm$(nam$)
02240   l1=len(nam$)
02260   restore #32,search>=nam$: nokey L1050
02280   close #101: ioerr L1110
02300 L1110: pr newpage
02320   pr f "1,10,C 5,R,N": "ACCT#"
02340   pr f "1,17,C 30,R,N": "COMPANY NAME"
02360   cde=0
02380   for j=1 to 20
02400     read #32,using F_CLIENT,release: k$,a1$,a2$,a3$ eof L1280
02420     form pos 1,c 5,c 30
02440 ! IF UPRC$(A3$)(1:3)><"RIC" THEN 940
02460     if a1$(1:l1)=nam$ or prtall=1 then goto L1200 else goto L1280
02480 L1200: cde=1
02500     pr f str$(j+1)&",10,C 5,U,N": k$
02520     pr f str$(j+1)&",17,C 30,U,N": a1$
02540     if j>1 then goto L1270
02560     bk=bk+1
02580     if bk>20 then bk=1
02600     bk$(bk)=a1$
02620 L1270: next j
02640 L1280: if j>1 then let j=j-1
02660   mat in2$(j)
02680 L1300: pr f "24,08,C 60,R,N": "Enter to continue; F5 to stop or enter ACCOUNT #:"
02700 L1310: input fields "24,58,C 5,RE,N": k$
02720   alp=0
02740   if cmdkey=5 then goto SRCHEND
02760   if rtrm$(k$)="" then goto L1380
02780   let z$=lpad$(rtrm$(k$),5)
02800   read #1,using F_CLIENT,key=z$: z$,mat a$,mat app,mat ma nokey L1310
02820   gosub PRINT_LABEL : goto L1300
02840 L1380: if cmdkey><2 then goto L1430
02860   bk=bk-1
02880   if bk<1 then goto L1450
02900   restore #32,key>=bk$(bk): nokey L1450
02920   bk=bk-1
02940 L1430: selclp=1
02960   goto L1110
02980 L1450: selclp=0
03000   goto L990
03020 SRCHEND: close #101: ioerr ignore
03040 L1480: close #127: ioerr ignore
03060   goto L250
03080 IGNORE: continue 
