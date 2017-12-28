00020 ! ______________________________________________________________________
00030   library 'S:\Core\Library': fncno,fnerror,fnwait,fnopenprn,fncloseprn,fnAcs,fnflexadd1,fnflexinit1,fnTos,fncustomer_search,fnLbl,fnTxt,fnOpt,fnmsgbox,fncomboa,fnButton,fnpic,fnFra,fnChk,fndat,fncmbact,fncombof,fncmbrt2,fnxit,fnCmdSet,fnCmdKey,fntop,fndate_mmddyy_to_ccyymmdd,fnpause,fngethandle
00040   library 'S:\Core\Library': fndat
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim rate(18,20),usage(18,20),cde(20),d(12),t(18,2),a(4),message$*40
00080   dim usagtot(18,20),ratetot(18,20),customer(18,20),cap$*128
00090   dim fa$(5),sa$(4),fb$(1),fc$(1),sb$(1)*38,fd$(1),z$(4)*11,svce$*11
00100   dim io2$(38),cnam$*40,code$(4)
00110 ! ______________________________________________________________________
00120   fncno(cno,cnam$)
00130   fndat(bdate)
00140   fntop("S:\acsUB\ubanlyze",cap$="Analyze Charges")
00150 MAIN: ! 
00160   sn$ = "UBAnalyze" !:
        fnTos(sn$) !:
        mylen=20 !:
        mypos=mylen+2
00170   text$="Billing Date:" !:
        fnLbl(1,1,text$,mylen,1)
00180   fnTxt(1,mypos,8,8,0,"1") !:
        resp$(1)=str$(bdate)
00190   text$="Type of Service:" !:
        fnLbl(2,1,text$,mylen,1)
00200   code$(1)="Water" !:
        code$(2)="Sewer" !:
        code$(3)="Electric" !:
        code$(4)="Gas" !:
        fncomboa("Service",2,mylen+3,mat code$,"",16)
00204   text$="Rate Code" !:
        fnLbl(2,1,text$,mylen,1)
00205   fnTxt(3,mypos,3,3,0,"30") !:
        resp$(3)=""
00210   fnCmdSet(3): fnAcs(sn$,0,mat resp$,ck)
00220   if ck=5 then goto XIT
00230   bdate= val(resp$(1))
00240   if resp$(2)="Water" then !:
          svce=1
00241   if resp$(2)="Sewer" then !:
          svce=2
00242   if resp$(3)="Sewer" then !:
          svce=3
00243   if resp$(4)="Sewer" then !:
          svce=4
00260   fnopenprn
00350   open #1: "Name="&env$('Q')&"\UBmstr\ubMaster.h"&env$('cno')&",KFName="&env$('Q')&"\UBmstr\ubIndex.h"&env$('cno')&",Shr",internal,input,keyed 
00510 L510: ! 
00540   for k9=1 to 20
00550     pr f "20,13,C 11,N": "Rate Code:"
00560     pr f "22,11,C 22,N": "(Blank when complete)"
00570     pr f "24,8,C 13,B,4;24,23,c 9,B,5": "Complete (F4)","Exit (F5)"
00580     cde(k9)=0
00590     rinput fields "20,24,Nz 2,UT,N": cde(k9)
00600     if cmdkey=4 then goto L620
00610     if cmdkey=5 then goto XIT
00620 L620: if cde(k9)>0 then goto L640
00630     if cde(k9)=0 then goto L930
00640 L640: if cde(k9)<1 or cde(k9)>20 then goto L510
00650     close #105: ioerr L660
00660 L660: open #105: "SROW=2,SCOL=42,EROW=23,ECOL=77,BORDER=SR,CAPTION=<Print Analysis Report",display,outIn 
00670     pr #105: newpage
00680     pr f "3,47,C 27,H,N": "  Beginning Usage   Rate"
00690     for j=1 to 18
00700       pr f str$(j+4)&",47,C 4,N": str$(j)&"."
00710       io2$(j*2-1)=str$(j+4)&",53,N 6,UT,N"
00720       io2$(j*2)=str$(j+4)&",65,N 9.5,UT,N"
00730     next j
00740     pr f "24,43,C 09,B,1": "Next (F1)"
00750     pr f "24,53,C 09,B,5": "Exit (F5)"
00760 L760: input fields mat io2$: mat t conv CONV2
00770     if ce>0 then io2$(ce)(ce1:ce2)="U": ce=0
00780     if cmdkey>0 or curfld>36 then goto L850 else ce=curfld
00790 L790: ce=ce+1: if ce>udim(io2$) then ce=1
00800 L800: io2$(ce)=rtrm$(uprc$(io2$(ce))) : ce1=pos(io2$(ce),"U",1) !:
          if ce1=0 then goto L790
00810     ce2=ce1+1 : io2$(ce)(ce1:ce1)="UC" : goto L760
00820 CONV2: if ce>0 then io2$(ce)(ce1:ce2)="U"
00830     ce=cnt+1
00840 ERR2: pr f "24,78,C 1": bell : goto L800
00850 L850: if cmdkey=5 then goto XIT
00860     close #105: ioerr L870
00870 L870: for k8=1 to 18
00880       usage(k8,k9)=t(k8,1)
00890       rate(k8,k9)=t(k8,2)
00900     next k8
00910     mat t=(0)
00920   next k9
00930 L930: message$="Printing: Please wait..." !:
        fnwait(message$,1)
00940   on fkey 5 goto DONE
00950   fnopenprn(cp,58,220,process)
00960 L960: read #1,using L970: mat a,mat d,f eof L1250
00970 L970: form pos 143,4*pd 2,pos 217,12*pd 5,pos 296,pd 4
00980   if f<>bdate then goto L960
00990   if a(svce)<>cde(1) then goto L960
01000   numbcust=numbcust+1
01010   s9=3
01020   if svce<3 then goto L1060
01030   s9=s9+4
01040   if svce=3 then goto L1060
01050   s9=s9+4
01060 L1060: for k9=1 to 20
01070     if a(svce)=cde(k9) then goto L1100
01080   next k9
01090   goto L960
01100 L1100: if d(s9)<usage(1,k9) then goto L1220
01110   for k7=1 to 17
01120     if d(s9)>=usage(k7,k9) and d(s9)<usage(k7+1,k9) then goto L1160
01130     if d(s9)>=usage(k7,k9) and usage(k7+1,k9)=0 then goto L1160
01140   next k7
01150   k7=18
01160 L1160: ds9=d(s9)
01170   for j7=1 to k7-1
01180     usagtot(j7,k9)=usagtot(j7,k9)+(usage(j7+1,k9)-usage(j7,k9))
01190     ds9=ds9-(usage(j7+1,k9)-usage(j7,k9))
01200   next j7
01210   usagtot(k7,k9)=usagtot(k7,k9)+ds9
01220 L1220: customer(k7,k9)=customer(k7,k9)+1
01230   goto L960
01240 ! ______________________________________________________________________
01250 L1250: for k5=1 to 18
01260     for k3=1 to 20
01270       ratetot(k5,k3)=usagtot(k5,k3)*rate(k5,k3)
01280     next k3
01290   next k5
01300   for k2=1 to 20
01310     if cde(k2)=0 then goto L1540
01320     pr #255,using L1330: "Utility Billing Rare and Usage Analyzer"
01330 L1330: form skip 3,pos 47,c 39,skip 2
01340     on svce goto L1360,L1370,L1380,L1390 none L1400
01350 ! ___________________________
01360 L1360: svce$="Water" !:
          goto L1410 !:
          ! ___________________________
01370 L1370: svce$="Sewer" !:
          goto L1410 !:
          ! ___________________________
01380 L1380: svce$="Electricity" !:
          goto L1410 !:
          ! ___________________________
01390 L1390: svce$="Gas" !:
          goto L1410 !:
          ! ___________________________
01400 L1400: svce$=" "
01410 L1410: pr #255,using L1420: "Service Analyzed - ",svce$
01420 L1420: form pos 51,c 19,c 11,skip 2
01430     pr #255,using L1440: "Rate Code",cde(k2)
01440 L1440: form pos 60,c 9,pos 73,pic(zz)
01450     pr #255,using L1460: "Beginning Usage","Rate","Dollars Generated","# of Customers"
01460 L1460: form pos 25,c 15,pos 45,c 4,pos 65,c 17,x 3,c 15,skip 2
01470     for k1=1 to 18
01480       if usage(k1,k2)=0 and rate(k1,k2)=0 then goto L1510
01490       pr #255,using L1500: usage(k1,k2),rate(k1,k2),ratetot(k1,k2),customer(k1,k2)
01500 L1500: form pos 25,pic(zzz,zzz,zz#),pos 43,pic(zz.#####),pos 65,pic(zzz,zzz,zzz.##),x 7,pic(zzzzzzzzzzzzz),skip 1
01510 L1510: next k1
01520     pr #255: newpage
01530   next k2
01540 L1540: pr #255,using L1550: "Number of Customers ",numbcust
01550 L1550: form skip 1,c 20,pos 45,pic(zzz,zzz,zzz),skip 1
01560   pr #255,using L1570: "Number of Customers under Minumum Usage",mincust
01570 L1570: form skip 1,c 40,pos 45,pic(zzz,zzz,zzz),skip 1
01580 DONE: close #1: ioerr L1590
01590 L1590: fncloseprn
01600 XIT: fnxit
01610 ! ______________________________________________________________________
01620 ERTN: fnerror(program$,err,line,act$,"xit")
01630   if uprc$(act$)<>"PAUSE" then goto L1660
01640   execute "list -"&str$(line) !:
        pause  !:
        goto L1660
01650   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01660 L1660: execute act$
01670   goto ERTN
01680 ! ______________________________________________________________________
