00010 ! Replace S:\acsGL\acglChrt
00020 ! General Ledger Chart of Accounts
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fndat,fnchain,fnprocess,fnTos,fnFra,fnOpt,fnCmdSet,fnAcs,fnLbl,fnqgl,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim gl(2,3),io2$(6),io1$(2),wrd1$(2)*46,cap$*128,p$(20)*50
00080   dim d$*50,dat$*20,cnam$*40,cap$*128,resp$(10)*50
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Chart of Accounts")
00110   fncno(cno,cnam$)
00120   fndat(dat$)
00130   process=fnprocess
00140 ! ______________________________________________________________________
00150   cap$="General Ledger Chart of Accounts"
00160   if process=1 then sel=1 : goto L330
00170 ! ______________________________________________________________________
00180   pr newpage
00190   fnTos(sn$="ChartAccoutnts") !:
        mylen=50: mypos=mylen+3 : right=1
00200   fnFra(1,1,2,70,"Chart of Accounts"," ",0)
00210   fnOpt(1,3,"Print Financial Statement Reference Numbers",0,1) !:
        resp$(rc+=1)="False"
00220   fnOpt(2,3,"Print Account Numbers and Names only",0,1) !:
        resp$(rc+=1)="True"
00230   fnLbl(5,1,"Beginning General Ledger Number (blank for all):",mylen,right)
00240   fnqgl(5,mypos,0,2) !:
        resp$(1)=""
00250   fnLbl(6,1,"Ending General Ledger Number (blank for all):",mylen,right)
00260   fnqgl(6,mypos,0,2) !:
        resp$(1)=""
00270   fnCmdSet(2)
00280   fnAcs(sn$,0,mat resp$,ckey)
00290   if ckey=5 then goto XIT
00300   if resp$(1)="True" then sel=1 else sel=2
00310   if trim$(resp$(3))<>"" then gl1$=fnagl$(resp$(3))
00320   if trim$(resp$(4))<>"" then gl2$=fnagl$(resp$(4))
00330 L330: open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],KFName=[Q]\GLmstr\GLIndex.h[cno],Shr",internal,input,keyed 
00340 ! ______________________________________________________________________
00350   pr newpage
00360   fnopenprn
00370   pr #255,using "Form pos 1,C 20,Cc 90": date$('mm/dd/yy'),cnam$
00380   pr #255,using "Form pos 1,C 20,Cc 90": time$,"Chart of Accounts"
00390   pr #255,using 'form pos 1,Cc 130': dat$
00400   pr #255: 
00410   gosub L680
00420 L420: read #1,using L430: dno,ano,sno,d$,br,sbr,ir,sir,fr,sfr eof L650
00430 L430: form pos 1,n 3,n 6,n 3,c 50,6*pd 3
00450   gl3$=cnvrt$("N 3",dno)&cnvrt$("N 6",ano)&cnvrt$("N 3",sno)
00451   if trim$(gl1$)="" then goto L461
00460   if gl3$<gl1$ then goto L420
00461 L461: if trim$(gl2$)="" then goto L480
00470   if gl3$>gl2$ then goto L650
00480 L480: if sel=1 then goto L490 else goto L530
00490 L490: pr #255,using L500: dno,ano,sno,d$,br,ir,fr,sbr,sir,sfr pageoflow L570
00500 L500: form pos 1,pic(zzz),x 1,n 6,x 1,pic(zzz),pos 20,c 50,x 3,n 5,x 5,n 5,x 5,n 5,x 7,n 5,x 5,n 5,x 5,n 5
00510   goto L420
00520 ! ______________________________________________________________________
00530 L530: pr #255,using L540: dno,ano,sno,d$ pageoflow L570
00540 L540: form pos 1,pic(zzz),x 1,n 6,x 1,pic(zzz),pos 20,c 50,skip 1
00550   goto L420
00560 ! ______________________________________________________________________
00570 L570: pr #255: newpage
00580   pr #255,using "Form pos 1,C 20,Cc 90": date$('mm/dd/yy'),cnam$
00590   pr #255: time$;tab(57);"Chart of Accounts"
00600   pr #255,using 'form pos 1,Cc 130': dat$
00610   pr #255: 
00620   gosub L680
00630   goto L420
00640 ! ______________________________________________________________________
00650 L650: fncloseprn
00660   goto XIT
00670 ! ______________________________________________________________________
00680 L680: if sel=2 then goto L730
00690   pr #255,using L700: "********** Primary *********","********* Secondary ********"
00700 L700: form pos 71,c 28,pos 103,c 28,skip 2
00710   pr #255,using L720: "Bal Sheet","Income","Chg Fin","Bal Sheet","Income","Chg Fin"
00720 L720: form pos 71,c 9,x 4,c 6,x 2,c 7,x 4,c 9,x 4,c 6,x 2,c 7
00730 L730: if sel=1 then goto L780
00740   pr #255: "" !:
        pr #255,using L750: "Account #","Description"
00750 L750: form pos 2,c 9,pos 38,c 11
00760   goto L810
00770 ! ______________________________________________________________________
00780 L780: pr #255,using L800: "Account #","Description","Reference","Reference","Position","Reference","Reference","Position"
00790   pr #255: ""
00800 L800: form pos 2,c 9,pos 38,c 11,pos 71,c 9,x 1,c 9,x 1,c 8,x 4,c 9,x 1,c 9,x 1,c 8
00810 L810: return 
00820 ! ______________________________________________________________________
00830 XIT: fnxit
00840 ! ______________________________________________________________________
00850 ! <Updateable Region: ERTN>
00860 ERTN: fnerror(program$,err,line,act$,"xit")
00870   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00880   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00890   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00900 ERTN_EXEC_ACT: execute act$ : goto ERTN
00910 ! /region
00920 ! ______________________________________________________________________
