00010 ! Replace S:\acsCL\DueBy
00020 ! pr Report of Invoices Due By Selected Dates
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fnerror,fncno,fndat,fnlbl,fntxt,fntos,fncmdset,fnacs,fndate_mmddyy_to_ccyymmdd
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim dat$*20,cnam$*40,vnam$*30,de$*50,fd$*30,ade$*50
00080   dim t2(10),d2(10),cap$*128
00090 ! ______________________________________________________________________
00100   fntop(program$,cap$="Report of Invoices Due By Selected Dates")
00110   fncno(cno,cnam$)
00120   cancel=99
00130   fndat(dat$)
00140   gosub ASK
00150   execute "Index "&env$('Q')&"\CLmstr\PayTrans.h"&env$('cno')&' '&env$('Q')&"\CLmstr\Unpdidx2.H"&env$('cno')&" 31/27/1 2/4/26 Replace DupKeys -n" ! index in year,monthday,reference
00160   open #paymstr:=13: "Name="&env$('Q')&"\CLmstr\PayMstr.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\PayIdx1.H"&env$('cno')&",Shr",internal,input,keyed 
00170   open #paytrans:=4: "Name="&env$('Q')&"\CLmstr\PayTrans.H"&env$('cno')&",KFName="&env$('Q')&"\CLmstr\Unpdidx2.H"&env$('cno')&",Shr",internal,input,keyed 
00180   fnopenprn
00190   gosub HDR
00200   vn$=iv$=""
00210 READ_INVOICES: ! 
00220   read #paytrans,using 'Form POS 1,C 8,C 12,2*G 6,C 12,C 18,N 10.2,N 1,N 2,G 8,G 6': vn$,iv$,ivd,dd,po$,ade$,amt,cde eof END1
00230 L230: if fndate_mmddyy_to_ccyymmdd(dd)>d2(wd2) then goto TOT1
00240   vnam$=""
00250   read #paymstr,using 'Form POS 9,C 30',key=vn$,release: vnam$ nokey PRINT_IT
00260 PRINT_IT: ! 
00270   pr #255,using 'Form POS 1,C 10,C 32,C 12,2*PIC(ZZZZ/ZZ/ZZ),X 2,C 18,POS 99,N 10.2': vn$, vnam$,iv$,ivd,dd,ade$(1:18),amt pageoflow NEWPGE
00280   t1+=amt
00290   goto READ_INVOICES
00300 ! ______________________________________________________________________
00310 NEWPGE: pr #255: newpage: gosub HDR : continue 
00320 ! ______________________________________________________________________
00330 TOT1: ! pr DATE TOTAL
00340   pr #255,using 'Form POS 99,"----------",SKIP 1,POS 76,C 23,N 10.2': "Total Due by "&cnvrt$("PIC(####/##/##)",d2(wd2)),t1 !:
        pr #255: ""
00350   t2(wd2)=t1
00360   if wd2=d2 or end1=1 then goto END1
00370   wd2+=1
00380   if wd2>d2 or fndate_mmddyy_to_ccyymmdd(dd)>d2(d2) then wd2=d2 : goto END1
00390   goto L420 ! DONT SKIP BETWEEN DATES
00400   pr #255: newpage
00410   gosub HDR
00420 L420: goto L230
00430 ! ______________________________________________________________________
00440 HDR: f1=1: pg=pg+1
00450   pr #255,using L460: date$,cnam$
00460 L460: form pos 1,c 8,pos 20,cc 40,skip 1
00470   pr #255,using L480: time$,"Invoices Due by Selected Dates"
00480 L480: form pos 1,c 8,pos 20,cc 40,skip 1
00490   pr #255,using L500: "Page",pg,dat$
00500 L500: form pos 1,c 4,n 4,pos 20,cc 40,skip 1
00510   hp3=51-int(len(rtrm$(fd$))/2)
00520   pr #255,using L530: fd$
00530 L530: form pos 20,cc 40,skip 2
00540   pr #255: "                                                        Invoice     Due                           Due By"
00550   pr #255: "Payee  #  Payee Name                      Invoice Numb    Date      Date    Description         "&cnvrt$("PIC(####/##/##)",d2(wd2))
00560   pr #255: "________  ______________________________  ____________  ________  ________  __________________  ____________"
00570   f1=1
00580   return 
00590 ! ______________________________________________________________________
00600 END1: if end1=1 then goto L630
00610   end1=1 : goto TOT1
00620   t2=0
00630 L630: for j=1 to d2
00640     pr #255,using L650: "      Due by "&cnvrt$("PIC(####/##/##)",d2(j)),t2(j)
00650 L650: form pos 10,c 23,pic(zzz,zzz,zzz,zzz.##bcr),skip 1
00660     t2=t2+t2(j)
00670   next j
00680   pr #255: tab(37);"______________"
00690   pr #255,using L650: "Total Due by "&cnvrt$("PIC(####/##/##)",d2(d2)),t2
00700   fncloseprn
00710 XIT: fnxit
00720 ! ______________________________________________________________________
00730 ASK: ! 
00740   fntos(sn$="Dueby") !:
        respc=0
00750   fnlbl(1,40,"",1,1)
00760   fnlbl(1,1,"1st Due By Date:",25,1)
00770   fntxt(1,27,10,0,1,"3",0,"Normally these would be dates in the future such as how much will be due by the 15th and the 30th.  Use ccyymmdd format" ) !:
        resp$(respc+=1)=""
00780   fnlbl(2,1,"2nd Due by Date:",25,1)
00790   fntxt(2,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00800   fnlbl(3,1,"3nd Due by Date:",25,1)
00810   fntxt(3,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00820   fnlbl(4,1,"4th Due By Date:",25,1)
00830   fntxt(4,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00840   fnlbl(5,1,"5th Due by Date:",25,1)
00850   fntxt(5,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00860   fnlbl(6,1,"6th Due by Date:",25,1)
00870   fntxt(6,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00880   fnlbl(7,1,"7th Due By Date:",25,1)
00890   fntxt(7,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00900   fnlbl(8,1,"8th Due by Date:",25,1)
00910   fntxt(8,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00920   fnlbl(9,1,"9th Due by Date:",25,1)
00930   fntxt(9,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00940   fnlbl(10,1,"10th Due By Date:",25,1)
00950   fntxt(10,27,10,0,1,"3") !:
        resp$(respc+=1)=""
00960   fncmdset(2): fnacs(sn$,0,mat resp$,ckey)
00970   if ckey=5 then goto XIT
00980   for j=1 to 10
00990     d2(j)=val(resp$(j))
01000   next j
01010   for j=1 to 10
01020     if j=1 then goto L1050
01030     if d2(j)=0 then d2=j-1 : goto L1060 else d2=j
01040     if d2(j)<d2(j-1) then goto ASK ! probably message box (dates out of order)
01050 L1050: next j
01060 L1060: wd2=1
01070   return 
01080 ! ______________________________________________________________________
01090 ! <Updateable Region: ERTN>
01100 ERTN: fnerror(program$,err,line,act$,"xit")
01110   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
01120   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
01130   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
01140 ERTN_EXEC_ACT: execute act$ : goto ERTN
01150 ! /region
01160 ! ______________________________________________________________________
