00010 ! Replace S:\acsUB\ubprtthree_Brier
00020 ! pr bills for Village of Monticello (full page)
00030 !
00040   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fncmbrt2,fncombof,fnChk,fnerror,fnTos,fncmbact,fnLastBillingDate,fnxit,fnCmdSet,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt,fnpa_fontsize
00050   on error goto Ertn
00060 !
00070   dim resp$(12)*60,txt$*100,mg$(3)*60,fb$(3)*60
00080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),b(11),extra1$*30
00090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40
00100   dim prebal$*30
00110 !
00120   fnLastBillingDate(d1)
00130   open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input  !:
        read #21,using "Form POS 41,2*C 40": at$(2),at$(3) !:
        close #21: 
00140   open #ratemst:=8: "Name=[Q]\UBmstr\ubData\RateMst.h[cno],KFName=[Q]\UBmstr\ubData\RateIdx1.h[cno],Shr",internal,input,keyed 
00150   at$(1)=env$('cnam') !:
        z=21 !:
        at$(1)=trim$(at$(1))(1:z) !:
        x=len(at$(1)) : y=z-x !:
        at$(1)=rpt$(" ",int(y/2))&at$(1)
00160   z=26 !:
        for j=2 to udim(at$) !:
          at$(j)=trim$(at$(j))(1:z) !:
          x=len(at$(j)) : y=z-x !:
          at$(j)=rpt$(" ",int(y/2))&at$(j) !:
        next j
00170   linelength=62
00180 ! 
00200   gosub BULKSORT
00210   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
00220   open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence #
00230   open #ubtransvb=15: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,outIn,keyed 
00250 !
00260   prebal$="10:00 AM, xxxxxxx  xx"
00270 SCREEN1: ! 
00280   a$="" : prtbkno=0
00290   fnTos(sn$="UBPrtBl1-1") !:
        pf=26 : ll=24 !:
        respc=0
00300   fnLbl(1,1,"Current Reading Date:",ll,1)
00310   fnTxt(1,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d2)
00320   fnLbl(2,1,"Previous Reading Date:",ll,1)
00330   fnTxt(2,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d3)
00340   fnLbl(3,1,"Penalty Due Date:",ll,1)
00350   fnTxt(3,pf,8,8,1,"1",0,tt$) !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d4)
00360   fnLbl(4,1,"Message on Bill:",ll,1)
00370   fnTxt(4,pf,60,60) !:
        resp$(respc+=1)=mg$(1)
00380   fnTxt(5,pf,60,60) !:
        resp$(respc+=1)=mg$(2)
00390   fnTxt(6,pf,60,60) !:
        resp$(respc+=1)=mg$(3)
00400   fnLbl(7,1,"Date of Billing:",ll,1)
00410   fnTxt(7,pf,8,8,1,"1") !:
        resp$(respc+=1)=cnvrt$("pic(zzzzzz)",d1)
00420   fnLbl(8,1,"Starting Account:",ll,1)
00430   fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1741,9,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2) !:
        resp$(respc+=1)="[All]"
00440   fnLbl(9,1,"Route Number:",ll,1)
00450   fncmbrt2(9,pf) !:
        resp$(respc+=1)="[All]"
00460   fnChk(10,pf,"Select Accounts to Print",1) !:
        resp$(respc+=1)="False"
00470   fnCmdSet(3) !:
        fnAcs(sn$,0,mat resp$,ck)
00480   if ck=5 then goto ENDSCR
00490   d2x= val(resp$(1)) !:
        d3x= val(resp$(2)) !:
        d4 = val(resp$(3)) !:
        mg$(1) = resp$(4) !:
        mg$(2) = resp$(5) !:
        mg$(3) = resp$(6) !:
        d1 = val(resp$(7))
00500   if resp$(8)="[All]" then !:
          a$="" else !:
          a$ = lpad$(trim$(resp$(8)(1:9)),9)
00510   if resp$(9)="[All]" then !:
          prtbkno=0 else !:
          prtbkno = val(resp$(9))
00520   if resp$(10)="True" then sl1=1: z$="" else sl1=0
00530   if trim$(a$)<>"" then read #2,using L540,key=a$: z$,route,sequence nokey SCREEN1 !:
          holdz$=z$: begin=1 !:
          st1=1
00540 L540: form pos 1,c 10,pos 1741,n 2,n 7
00550   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
00560   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
00570   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
00580 !
00590   open #3: "Name=[Q]\UBmstr\UBAdrBil.h[cno],KFName=[Q]\UBmstr\adrIndex.h[cno],Shr",internal,input,keyed 
00600   fnpa_open
00610 !
00620   on fkey 5 goto RELEASE_PRINT
00630 L630: if sl1=1 then goto SCREEN3
00640 L640: read #6,using L670: z$ eof RELEASE_PRINT
00650   if trim$(a$)<>"" and begin=1 and z$<>holdz$ then goto L640 ! start with
00660   begin=0 ! cancel starting account
00670 L670: form pos 22,c 10
00680   read #1,using L690,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate nokey L640
00690 L690: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1750,2*n 6,pos 1942,c 12,pos 1864,c 30,pos 1831,n 9,pos 1821,n 1
00700   if prtbkno=0 then goto L720
00710   if prtbkno><route then goto RELEASE_PRINT
00720 L720: if f><d1 then goto L630
00730 L730: e1=0 : mat pe$=("")
00740   for j=2 to 4
00750     if rtrm$(e$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=e$(j)
00760   next j
00770   if st1=0 then goto READALTADR
00780 ! If ST1$=Z$ Then sT1=0 Else Goto 560
00790 READALTADR: ! 
00800 ! read alternate billing address
00810   read #3,using L820,key=z$: mat ba$ nokey L910
00815   if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then goto L910
00820 L820: form pos 11,4*c 30
00830   e1=0 : mat pe$=("")
00840   for j=1 to 4
00850     if rtrm$(ba$(j))<>"" then !:
            e1=e1+1 : pe$(e1)=ba$(j)
00860   next j
00870   if trim$(pe$(2))="" then pe$(2)=pe$(3): pe$(3)=""
00880   if trim$(pe$(3))="" then pe$(3)=pe$(4): pe$(4)=""
00890   goto L1030
00900 !
00910 L910: ! 
00920   if trim$(extra1$)<>"" then pe$(4)=pe$(3): pe$(3)=extra1$ ! set third address line to extra1$ (2nd address)
00930   goto L1030
00940 !
00950 RELEASE_PRINT: ! 
00960   close #1: ioerr ignore
00970   close #3: ioerr ignore
00980   fnpa_finis
01010   goto ENDSCR
01020 !
01030 L1030: ! 
01040   pb=bal-g(11)
01050   if bal<=0 then g(9)=g(10)=0 ! don't show penalty if balance 0 or less
01060   fb$(1)=mg$(1)
01070   fb$(2)=mg$(2)
01080   fb$(3)=mg$(3)
01090   if c4>0 then fb$(1)="          Final Bill" : fb$(2)="": fb$(3)=""
01100 ! ______________print bill routine______________________________________
01110   gosub VBPRINT
01120 ! _____________end of pr routine______________________________________
01130   bct(2)=bct(2)+1 !:
        ! accumulate totals
01140   goto L630
01150 !
01160 SCREEN3: ! 
01170   fnTos(sn$:= "UBPrtBl1-2")
01180   fnLbl(1,1,"Account (blank to stop)",31,1)
01200   if trim$(z$)<>"" then !:
          fnLbl(3,1,"Last Account entered was "&z$,44,1)
01210   fncmbact(1,17) !:
        resp$(1)=a$
01220   fnCmdSet(3): fnAcs(sn$,0,mat resp$,ck)
01230   a$ = lpad$(trim$(resp$(1)(1:10)),10) !:
        if trim$(a$)="" then goto RELEASE_PRINT
01240   if ck=5 then goto RELEASE_PRINT
01250   read #1,using L690,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,d3,d2,bulk$,extra1$,estimatedate,final nokey SCREEN3
01260   goto L730
01550 !
01560 ENDSCR: ! pr totals screen
01570   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
01580   fnTos(sn$="Bills-Total") !:
        mylen=23 : mypos=mylen+2 !:
        respc=0
01590   fnLbl(1,1,"Total Bills Printed:",mylen,1)
01600   fnTxt(1,mypos,8,0,1,"",1) !:
        resp$(respc+=1)=cnvrt$("N 8",sum(bct))
01670   fnCmdSet(52) !:
        fnAcs(sn$,0,mat resp$,ck)
01680 XIT: fnxit
01690 IGNORE: continue 
01700 ERTN: fnerror(program$,err,line,act$,"xit")
01710   if uprc$(act$)<>"PAUSE" then goto L1740
01720   execute "list -"&str$(line) : pause : goto L1740
01730   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
01740 L1740: execute act$
01750   goto ERTN
01760 !
01860 VBPRINT: ! r: Brier Lake - 3 per page  Utility Bills  requires: (z$,fb$(1),mat d, mat g,mat pe$,d2,d3,pb)
01890   fnpa_fontsize
01900   fnpa_txt(trim$(z$),62,lyne+7)
01910   fnpa_txt(trim$(fb$(1)),95,lyne+7)
01920   if pb<>0 then de$="Prv" else de$="   "
01932   fnpa_txt(rpt$(' ',23)&de$&cnvrt$("pic(-----.--)",pb),1,lyne+25)
01940   if g(1)>0 then de$="Wat" else de$="   "
01950   fnpa_txt(cnvrt$("pic(zzzzzzzz)",d(1))&cnvrt$("pic(zzzzzzzz)",d(2))&cnvrt$("pic(zzzzzz)",d(3))&" "&de$&cnvrt$("pic(-----.--)",g(1))&"     "&pe$(1)(1:22),1,lyne+30)
01960   if g(2)>0 then de$="Sew" else de$="   "
01970   fnpa_txt(rpt$(' ',23)&de$&cnvrt$("pic(-----.##)",g(2))&"     "&pe$(2)(1:22),1,lyne+35)
01980   if g(3)>0 then de$="Fee" else de$="   "
01990   fnpa_txt(rpt$(' ',23)&de$&cnvrt$("pic(-----.--)",g(3))& "     "&pe$(3)(1:22),1,lyne+40)
58000   if g(5)>0 then 
58020     fnpa_txt(rpt$(' ',23)&"P/T"&cnvrt$("pic(-----.##)",g(5))&"     "&pe$(4)(1:22) ,1,lyne+45)
58060   end if
58080   if g(6)>0 then 
58100     fnpa_txt(rpt$(' ',23)&'DEQ'&cnvrt$("pic(-----.##)",g(6)),1,lyne+50)
58120   end if
58140   if g(8)>0 then
58160     fnpa_txt("Other Charge"&cnvrt$("pic(-------.##)",g(8)),1,lyne+55)
58180   end if
58200   if g(9)>0 then de$="Tax" else de$="   "
58220   if d2=0 then d2=d3x
58240   if d3=0 then d3=d2x
58260   txt$=cnvrt$("pic(zzbzzbbzz)",d3x)&" "&cnvrt$("pic(zzbzzbbzz)",d2x)&cnvrt$("pic(----.##)",g(12)+pb) &cnvrt$("pic(------.##)",g(11)+pb)&"    "&cnvrt$("pic(-----.##)",g(12)+pb)&"   "&cnvrt$("pic(zzbzzbzz)",d4)&cnvrt$("pic(-----.##)",g(11)+pb) 
58280   fnpa_txt(txt$,1,lyne+76)
58300   bills+=1
58320   if int(bills/3)=bills/3 then 
58340     fnpa_newpage 
58360     lyne=0 
58380   else
58400     lyne=lyne+90
58420   end if
58440 return ! /r
62000 BULKSORT: ! r: bulk sort order
62020   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in Account order
62040   open #6: "Name="&env$('Temp')&"\Temp."&session$&",Replace,RecL=31",internal,output 
62060   L2190: read #1,using "Form POS 1,C 10,pos 1741,n 2,pos 1743,n 7,pos 1942,c 12": z$,route,seq,bulk$ eof L2220
62080   write #6,using "Form POS 1,C 12,n 2,n 7,c 10": bulk$,route,seq,z$
62100   goto L2190
62120   L2220: close #1: ioerr ignore
62140   close #6: ioerr ignore
62160   execute "Index "&env$('Temp')&"\Temp."&session$&" "&env$('Temp')&"\Tempidx."&session$&" 1,19,Replace,DupKeys -n" ioerr L2260
62180   open #6: "Name="&env$('Temp')&"\Temp."&session$&",KFName="&env$('Temp')&"\Tempidx."&session$,internal,input,keyed 
62200 L2260: return ! /r
