00010 ! Replace S:\acsGL\BudWksh
00020 ! pr Budget Worksheet
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fnpedat$,fnprocess, fnTos,fnLbl,fnTxt,fnqgl,fnChk,fnCmdSet,fnAcs,fnagl$,fnrgl$,fnconsole
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim cnam$*40,d$*50,n$*12,bp(13),bm(13),name$*30,gln1$*12,gln2$*12
00080   dim cap$*128,resp$(10)*80,revb(13)
00090 ! ______________________________________________________________________
00100   right=1 : center=2 : left=0
00110   fntop(program$,cap$="Budget Worksheet")
00120   fnconsole(off=0)
00130   fncno(cno,cnam$)
00140   gosub DETERMINE_DATE
00150   on fkey 5 goto L760
00160   open #1: "Name="&env$('Q')&"\GLmstr\GLmstr.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\GLINDEX.h"&env$('cno')&",Shr",internal,input,keyed 
00170   open #12: "Name="&env$('Q')&"\GLmstr\BudgetInfo.h"&env$('cno')&",KFName="&env$('Q')&"\GLmstr\BudIndx.h"&env$('cno')&",Use,RecL=28,KPs=1,KLn=14,Shr",internal,outIn,keyed 
00180   if fnprocess=1 then goto L440
00190 ! defaults !:
        pba$='False' : bud$="False"
00200 SCREEN1: ! 
00210   fnTos(sn$='BudgetWksht') !:
        lc=0 : mylen=40 : mypos=mylen+2
00220   fnLbl(lc+=1,1,'Fund:',mylen,right)
00230   fnTxt(lc,mypos,40) !:
        resp$(1)=name$
00240   fnLbl(lc+=1,1,'Starting Account:',mylen,right)
00250   fnqgl(lc,mypos) !:
        resp$(2)=fnrgl$(gln1$)
00260   fnLbl(lc+=1,1,'Ending Account:',mylen,right)
00270   fnqgl(lc,mypos) !:
        resp$(3)=fnrgl$(gln2$)
00280   fnChk(lc+=1,mypos,'Enter proposed budget amounts',right) !:
        resp$(4)=pba$
00290   fnChk(lc+=1,mypos,'Revised Budget instead of Origional',right) !:
        resp$(5)=bud$
00300   fnLbl(lc+=1,1,'Closing date for previous budget year:',mylen,right)
00310   fnTxt(lc,mypos,8,8,right,"1",0,"Reqired for prior year's budget to appear on worksheet.") !:
        resp$(6)=str$(priordate) ! previous year end
00320   fnLbl(lc+=1,1,'Closing date for two years ago:',mylen,right)
00330   fnTxt(lc,mypos,8,8,right,"1",0,"Reqired only if you want balance and budget from two years ago.") !:
        resp$(7)=str$(priorpriordate) ! two years ago
00340   fnChk(lc+=1,mypos,"Year Already Closed:",right)
00350   resp$(8)="False"
00360   fnCmdSet(3)
00370   fnAcs(sn$,0,mat resp$,ckey)
00380   if ckey=5 then goto XIT
00390   name$=resp$(1) !:
        gln1$=fnagl$(resp$(2)) !:
        gln2$=fnagl$(resp$(3)) !:
        pba$=resp$(4) !:
        bud$=resp$(5)
00400   priordate=val(resp$(6)) !:
        priorpriordate=val(resp$(7))
00410   if resp$(8)="True" then yearclosed=1
00420   yr1$=resp$(6)(5:6) !:
        yr2$=resp$(7)(5:6)
00430   read #1,using L530,key>=gln1$: n$,d$,cb,mat bp,mat bm,mat revb nokey L440
00440 L440: namtab=66-int(len(rtrm$(name$))/2)
00450   fnopenprn
00460   gosub HEADING
00470   goto L540
00480 L480: read #1,using L530: n$,d$,cb,mat bp,mat bm,mat revb eof L760 ! READ MASTER FILE
00490   if trim$(gln1$)="" then goto L510
00500   if n$<gln1$ then goto L480
00510 L510: if trim$(gln2$)="" then goto L530
00520   if n$>gln2$ then goto L760
00530 L530: form pos 1,c 12,c 50,pos 87,pd 6.2,pos 171,26*pd 6.2,pos 339,13*pd 6.2
00540 L540: dno=val(n$(1:3))
00550   ano=val(n$(4:9))
00560   sno=val(n$(10:12))
00570   if yearclosed=1 then cb=bp(12) ! use last year end balance if year already closed.
00580   if uprc$(bud$)="R" then mat bm=revb
00590   cyb=bm(1)+bm(2)+bm(3)+bm(4)+bm(5)+bm(6)+bm(7)+bm(8)+bm(9)+bm(10)+bm(11)+bm(12)
00600   tpriorcb+=priorcb
00610   tpriorbud+=priorbud
00620   toldcb+=oldcb
00630   toldbud+=oldbud
00640   tpyb=tpyb+bp(12)
00650   tcb=tcb+cb
00660   tcyb=tcyb+cyb
00670   if yr1$="" then goto L710
00680   budkey$=n$&yr1$ ! gl number plus year for last year
00690   budacno$="": yr$="": oldcb=oldbud=0
00700   read #12,using "form pos 1,c 12,c 2,2*pd 6.2",key=budkey$: budacno$,yr$,oldcb,oldbud nokey L720 ! read old budget history record
00710 L710: if yr2$="" then goto L740
00720 L720: priorbudkey$=n$&yr2$ ! get two years ago
00730   read #12,using "form pos 1,c 12,c 2,2*pd 6.2",key=priorbudkey$: budacno$,prioryr$,priorcb,priorbud nokey L740 ! read old budget history record from two years ago
00740 L740: gosub PRINT_MASTER_RECORD
00750   goto L480
00760 L760: ! EOF OR EOJ ON MASTER FILE
00770   pr #255: 
00780   if priordate=0 and priorpriordate=0 then pr #255,using L810: "Totals",tpyb,tcb,tcyb,tbud
00790   if priordate>0 and priorpriordate=0 then pr #255,using L820: "Totals",toldcb,toldbud,tcb,tcyb,tbud
00800   if priordate>0 and priorpriordate>0 then pr #255,using L820: "Totals",tpriorcb,tpriorbud,toldcb,toldbud,tcb,tcyb,tbud
00810 L810: form pos 38,c 6,pos 45,pic(---,---,---.##),pos 73,pic(---,---,---.##),pos 102,pic(---,---,---.##),pos 123,pic(----,---,---.zz),skip 2
00820 L820: form pos 25,c 6,pos 34,7*pic(----,---,---.##),skip 1
00830   tpyb=0
00840   tcb=0
00850   tcyb=0
00860   tbud=0
00870   tpriorcb=tpriorcb=toldcb=toldbud=0
00880   fncloseprn
00890   goto SCREEN1
00900   goto XIT
00910 ! ______________________________________________________________________
00920 HEADING: ! pr PAGE HEADING
00930   pr #255: 
00940   pr #255,using L950: rtrm$(cnam$)
00950 L950: form pos 20,cc 40
00960   pr #255,using L950: "Budget Worksheet"
00970   if trim$(name$)<>"" then pr #255,using L950: rtrm$(name$)
00980   pr #255,using L950: rtrm$(fnpedat$)
00990   pr #255: ""
01000   if priordate=0 and priorpriordate=0 then goto L1010 else goto L1030
01010 L1010: pr #255,using L1080: "Account"
01020   goto L1090
01030 L1030: if priordate>0 and priorpriordate=0 then goto L1040 else goto L1060
01040 L1040: pr #255,using L1080: "Account","         "&str$(priordate),"         "&str$(priordate),"        Current","        Current","       Proposed"
01050   goto L1090
01060 L1060: if priordate>0 and priorpriordate>0 then goto L1070 else goto L1090
01070 L1070: pr #255,using L1080: "Account","         "&str$(priorpriordate),"         "&str$(priorpriordate),"         "&str$(priordate),"         "&str$(priordate),"        Current","        Current","       Proposed"
01080 L1080: form pos 3,c 8,pos 34,7*c 15
01090 L1090: if priordate=0 and priorpriordate=0 then goto L1100 else goto L1120
01100 L1100: pr #255,using L1190: "Number","Description","Prior Year Balance","Current Balance","Current Budget","Proposed Budget"
01110   goto L1200
01120 L1120: if priordate>0 and priorpriordate=0 then goto L1130 else goto L1160
01130 L1130: pr #255,using L1180: "Number","Description","        Balance","         Budget","        Balance","         Budget","         Budget"
01140   form pos 4,c 6,pos 22,c 11,pos 34,5* c 15
01150   goto L1200
01160 L1160: if priordate>0 and priorpriordate>0 then goto L1170 else goto L1200
01170 L1170: pr #255,using L1180: "Number","Description","        Balance","         Budget","        Balance","         Budget","        Balance","         Budget","         Budget"
01180 L1180: form pos 4,c 6,pos 22,c 11,pos 34,7*c 15
01190 L1190: form pos 4,c 6,pos 22,c 11,pos 44,c 18,pos 74,c 15,pos 101,c 19,pos 123,c 15
01200 L1200: return 
01210 ! ______________________________________________________________________
01220 PRINT_MASTER_RECORD: ! 
01230   if pba$="True" then 
01240     fnTos(sn$='Budwksh-add-ba') !:
          lc=0 : mylen=50 : mypos=mylen+2
01250     fnLbl(lc+=1,1,'Enter Proposed Budget',40,center,+2)
01260     lc+=1
01270     fnLbl(lc+=1,1,"Proposed Budget for Account "&str$(dno)&"-"&str$(ano)&"-"&str$(sno)&":",mylen,right)
01280     fnTxt(lc,mypos,12,0,0,'currency') !:
          resp$=''
01290     fnLbl(lc+=1,1,d$,mylen,right)
01300     fnCmdSet(3)
01310     fnAcs(sn$,0,mat resp$,ckey)
01320     if ckey=5 then goto XIT
01330     bud=val(resp$(1)) : tbud+=bud
01340   end if 
01350   if priordate=0 and priorpriordate=0 then goto L1360 else goto L1430
01360 L1360: if pba$="True" then goto L1400
01370   pr #255,using L1380: dno,ano,sno,d$(1:22),bp(12),cb,cyb," ______________" pageoflow NWPGE
01380 L1380: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 47,pic(-----,---.##),pos 75,pic(-----,---.##),pos 104,pic(-----,---.##),pos 122,c 15,skip 1
01390   goto L1410
01400 L1400: pr #255,using L1410: dno,ano,sno,d$(1:22),bp(12),cb,cyb,cnvrt$("PIC(----,---,---.##)",bud) pageoflow NWPGE
01410 L1410: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 47,pic(-----,---.##),pos 75,pic(-----,---.##),pos 104,pic(-----,---.##),pos 122,c 15,skip 1
01420   goto L1580
01430 L1430: if priordate>0 and priorpriordate=0 then goto L1440 else goto L1510
01440 L1440: if pba$="True" then goto L1450 else goto L1480
01450 L1450: pr #255,using L1460: dno,ano,sno,d$(1:22),oldcb,oldbud,cb,cyb,cnvrt$("PIC(----,---,---.zz)",bud) pageoflow NWPGE
01460 L1460: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,4*pic(----,---,---.##),c 15,skip 2
01470   goto L1580
01480 L1480: pr #255,using L1490: dno,ano,sno,d$(1:22),oldcb,oldbud,cb,cyb," _____________" pageoflow NWPGE
01490 L1490: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,4*pic(----,---,---.##),c 15,skip 2
01500   goto L1580
01510 L1510: if priordate>0 and priorpriordate>0 then goto L1520 else goto L1580
01520 L1520: if pba$="True" then goto L1530 else goto L1560
01530 L1530: pr #255,using L1540: dno,ano,sno,d$(1:22),priorcb,priorbud,oldcb,oldbud,cb,cyb,cnvrt$("PIC(----,---,---.zz)",bud) pageoflow NWPGE
01540 L1540: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,6*pic(----,---,---.##),c 15,skip 2
01550   goto L1580
01560 L1560: pr #255,using L1570: dno,ano,sno,d$(1:22),priorcb,priorbud,oldcb,oldbud,cb,cyb," ______________" pageoflow NWPGE
01570 L1570: form pos 1,pic(zzz),x 1,pic(zzzzzz),x 1,pic(zzz),x 2,c 22,pos 34,6*pic(----,---,---.##),c 15,skip 2
01580 L1580: return 
01590 ! ______________________________________________________________________
01600 NWPGE: ! SPACE TO NEWPAGE
01610   pr #255: newpage
01620   gosub HEADING
01630   continue 
01640 ! ______________________________________________________________________
01650 XIT: fnxit
01660 ! ______________________________________________________________________
01670 ! <Updateable Region: ERTN>
01680 ERTN: fnerror(program$,err,line,act$,"xit")
01690   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01700   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01710   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01720 ERTN_EXEC_ACT: execute act$ : goto ERTN
01730 ! /region
01740 ! ______________________________________________________________________
01750 DETERMINE_DATE: ! 
01760   endingdate$=rtrm$(fnpedat$)
01770   x=pos(endingdate$," ",1)
01780   month$=endingdate$(1:x-1)
01790   day=val(endingdate$(x+1:x+2)) conv L1800
01800 L1800: year=val(endingdate$(len(endingdate$)-1:len(endingdate$))) : prioryear=year-1
01810   dim month$(12),payrolldate$*20
01820   month$(1)="January": month$(2)="February" !:
        month$(3)="March": month$(4)="April": month$(5)="May" !:
        month$(6)="June" : month$(7)="July" !:
        month$(8)="August": month$(9)="September" !:
        month$(10)="October": month$(11)="November": month$(12)="December"
01830   for j=1 to 12
01840     if uprc$(month$)=uprc$(month$(j)) then month=j: goto L1860
01850   next j
01860 L1860: priordate=(month*10000)+day*100+prioryear
01870   priorpriordate=priordate-1
01880   return 
