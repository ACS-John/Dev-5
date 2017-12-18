00010 ! Replace S:\acsGL\YearendTB
00020 ! reprint trial balance for last year end
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fntos,fnlbl,fntxt,fnchk,fnqgl,fncmdset,fnacs,fnagl$
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,d$*50,tr(7),tr$*12,td$*30,n$*12,t$*12,x$*3
00080   dim a$(9)*3,cogl$(2)*12,u$*12,c$*5,d(2),ta(2)
00090   dim wrddetail$(2),p$(20)*50,resp$(10)*80,bp(13),cap$*128
00100 ! ______________________________________________________________________
00110   right=1
00120   fntop(program$,cap$="Reprint Year End Trial Balance")
00140   fncno(cno,cnam$)
00150   open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&env$('cno'),internal,input,relative  !:
        read #20,using 'Form POS 150,2*N 1',rec=1: d(1),d(2) !:
        read #20,using 'Form POS 152,2*C 12',rec=1: mat cogl$ !:
        close #20: 
00160   a$(1)="C/D" : a$(2)="C/R" : a$(3)="ADJ" !:
        a$(4)="A/P" : a$(5)="PR" : a$(6)="A/R" !:
        a$(7)="S/J" : a$(8)="P/J" : a$(9)=" "
00170   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLIndex.H"&env$('cno')&",Shr",internal,input,keyed 
00180   goto START_REPORT
00190 SCREEN1: ! 
00200   fntos(sn$="GLTB") !:
        lc=0 : mylen=25 : mypos=mylen+2
00210   fnchk(lc+=1,mypos,"List All Details",right) !:
        resp$(1)="True"
00220   fnlbl(lc+=1,1,"Cost Center:",mylen,right)
00230   fntxt(lc,mypos,5,0,0,'number') !:
        resp$(2)=""
00240   fnchk(lc+=1,mypos,"Subtotal after each fund",right) !:
        resp$(3)="True"
00250   fnlbl(lc+=1,1,"Starting Account:",mylen,right)
00260   fnqgl(lc,mypos,0,1) !:
        resp$(4)="[All]"
00270   fnlbl(lc+=1,1,"Ending Account:",mylen,right)
00280   fnqgl(lc,mypos,0,1) !:
        resp$(5)="[All]"
00290   fncmdset(3)
00300   fnacs(sn$,0,mat resp$,ckey)
00310   if ckey=5 then goto XIT
00320   if resp$(1)="True" then pt=0 else pt=1
00330   costcent=val(resp$(2)) !:
        n$=lpad$(str$(costcent),3)&"     0  0"
00340   if resp$(3)="True" then subt=1 else subt=0
00350   sl1$=fnagl$(resp$(4)) !:
        sl2$=fnagl$(resp$(5))
00360 START_REPORT: ! 
00370   fnopenprn
00380   gosub HDR2
00390 READ_1: ! 
00400   read #1,using L410: n$,d$,bb,cb,mat bp eof L580
00410 L410: form pos 1,c 12,c 50,pos 81,2*pd 6.2,pos 171,13*pd 6.2
00420   olddno=dno
00430   if costcent><0 and val(n$(1:3))><costcent then goto READ_1
00440   if sgl(4)=0 and sgl(5)=0 and sgl(6)=0 then goto L460
00450   if n$>sl2$ then goto L590
00460 L460: dno=val(n$(1:3))
00470   if subt=1 and olddno>0 and olddno<>dno then pr #255,using L480: "FUND "&str$(olddno)&" TOTALS",fundt3 else goto L500
00480 L480: form skip 1,pos 30,c 20,pos 80,pic(zz,zzz,zzz.## cr),skip 2
00490   fundt1=fundt2=fundt3=0
00500 L500: ano=val(n$(4:9))
00510   sno=val(n$(10:12))
00520   begbal=0
00530   curbal=curbal+bp(12)
00540   fundt1=fundt1+0
00550   fundt3=fundt3+bp(12)
00560   gosub L840
00570   goto READ_1
00580 L580: olddno=dno
00590 L590: pr #255: 
00600   if subt=1 then !:
          pr #255,using L480: "Fund "&str$(olddno)&" Totals",0 ,0,fundt3
00610   pr #255: ,"Trial Balance Proof Totals";
00620   pr #255,using L630: 0,0,curbal
00630 L630: form pos 80,pic(zz,zzz,zzz.## cr)
00640   close #1: ioerr L650
00650 L650: close #2: ioerr L660
00660 L660: fncloseprn
00670 ! 
00680   goto XIT
00690 ! ______________________________________________________________________
00700 HDR2: ! 
00710   pr #255,using L730: date$('mm/dd/yy'),cnam$
00720   pr #255,using L730: time$,cap$
00730 L730: form pos 1,c 8,pos 15,cc 50
00740   pr #255,using L750: fnpedat$,"Page ",p1+=1
00750 L750: form pos 15,cc 50,pos 80,c 5,n 4,skip 2
00760   pr #255,using L770: "Account","Reference","Ending"
00770 L770: form pos 6,c 7,pos 70,c 9,pos 84,c 9,pos 99,c 7,pos 116,c 6,skip 1
00780   pr #255,using L790: "Number","Account Name/Transaction Description","Date  Source","Number","Balance"
00790 L790: form pos 6,c 6,pos 17,c 36,pos 54,c 13,pos 71,c 6,pos 85,c 7
00800   pr #255,using L810: "__________","____________________________________","____","______","___________","_________"
00810 L810: form pos 4,c 10,pos 17,c 36,pos 54,c 4,pos 60,c 6,pos 69,c 11,pos 84,c 10,skip 2
00820   return 
00830 ! ______________________________________________________________________
00840 L840: ! 
00850   pr #255,using L860: dno,ano,sno,d$,bp(12) pageoflow L890
00860 L860: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 50,pos 80,pic(nz,zzz,zzz.## cr),skip 2
00870   return 
00880 ! ______________________________________________________________________
00890 L890: pr #255: newpage
00900   gosub HDR2
00910   continue 
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
