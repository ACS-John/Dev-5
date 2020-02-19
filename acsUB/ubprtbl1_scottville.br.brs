10000 ! Replace S:\acsUB\ubprtbl1
10010 ! pr bills (new format)
10020 !
10030   library 'S:\Core\Library': fnAcs,fnLbl,fnTxt,fnwait,fncmbrt2,fncombof,fnChk,fnerror,fnOpt,fnTos,fncmbact,fncno,fnLastBillingDate,fnxit,fnCmdSet,fntop,fnformnumb$,fnpause,fnpa_finis,fnpa_open,fnpa_newpage,fnpa_txt
10040   fntop("S:\acsUB\ubprtbl1",cap$="Print Bills")
10050   on error goto Ertn
10060 !
10070   dim resp$(11)*40,txt$*40,mg$(3)*30,rw(22,13),cap$*128
10080   dim z$*10,e$(4)*30,f$*12,g(12),d(15),w$*31,y$*39,x$*70,b(11)
10090   dim gb(10),pe$(4)*30,ba$(4)*30,at$(3)*40,cnam$*40
10100 !
10110   fncno(cno,cnam$)
10120   fnLastBillingDate(d1)
10130   open #21: "Name=[Q]\UBmstr\Company.h[cno],Shr",internal,input 
10140   read #21,using "Form POS 41,2*C 40": at$(2),at$(3)
10150   close #21: 
10160   at$(1)=cnam$
10170   z=21
10180   at$(1)=trim$(at$(1))(1:z)
10190   x=len(at$(1)) : y=z-x
10200   at$(1)=rpt$(" ",int(y/2))&at$(1)
10210   z=26
10220   for j=2 to udim(at$)
10230     at$(j)=trim$(at$(j))(1:z)
10240     x=len(at$(j)) : y=z-x
10250     at$(j)=rpt$(" ",int(y/2))&at$(j)
10260   next j
10270   linelength=62
10280   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed  ! open in account order
10290   open #2: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndx5.h[cno],Shr",internal,input,keyed  ! open in route-sequence
10300 !
10310 SCREEN1: ! 
10320   a$="" : prtbkno=0
10330   fnTos(sn$="UBPrtBl1-1")
10340   pf=26 : ll=24 : respc=0
10350 ! fnLbl(1,1,"Service From:",LL,1)
10360 !  fnTxt(1,PF,8,8,1,"1",0,TT$)
10370 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D2)
10380 ! fnLbl(2,1,"Service To:",LL,1)
10390 !  fnTxt(2,PF,8,8,1,"1")
10400 !  rESP$(RESPC+=1)=CNVRT$("pic(zzzzzz)",D3)
10410   fnLbl(1,1,"Penalty Due Date:",ll,1)
10420   fnTxt(1,pf,8,8,1,"1",0,tt$)
10430   resp$(d4_i=respc+=1)=cnvrt$("pic(zzzzzz)",d4)
10440   fnLbl(2,1,"Meter Reading Date:",ll,1)
10450   fnTxt(2,pf,8,8,1,"1",0,tt$)
10460   resp$(d5_i=respc+=1)=cnvrt$("pic(zzzzzz)",d5)
10470   fnLbl(4,1,"Message on Bill:",ll,1)
10480   fnTxt(4,pf,30,30)
10490   resp$(mg1_i=respc+=1)=mg$(1)
10500   fnTxt(5,pf,30,30)
10510   resp$(mg2_i=respc+=1)=mg$(2)
10520   fnTxt(6,pf,30,30)
10530   resp$(mg3_i=respc+=1)=mg$(3)
10540   fnLbl(7,1,"Date of Billing:",ll,1)
10550   fnTxt(7,pf,8,8,1,"1")
10560   resp$(d1_i=respc+=1)=cnvrt$("pic(zzzzzz)",d1)
10570   fnLbl(8,1,"Starting Account:",ll,1)
10580   fncombof("ubm-act-nam",8,pf,40,"[Q]\UBmstr\Customer.h[cno]",1,10,41,30,"[Q]\UBmstr\ubindx5.h[cno]",2)
10590   resp$(acct_i=respc+=1)="[All]"
10600   fnLbl(9,1,"Route Number:",ll,1)
10610   fncmbrt2(9,pf)
10620   resp$(rt_i=respc+=1)="[All]"
10630   fnChk(10,pf,"Select Accounts to Print",1)
10640   resp$(selacct_i=respc+=1)="False"
10650   fnCmdSet(3)
10660   fnAcs(sn$,0,mat resp$,ck)
10670   if ck=5 then goto ENDSCR
10680   d1=val(resp$(d1_i))
10690   d4=val(resp$(d4_i))
10700   d5=val(resp$(d5_i))
10710   mg$(1)=resp$(mg1_i)
10720   mg$(2)=resp$(mg2_i)
10730   mg$(3)=resp$(mg3_i)
10740   if resp$(acct_i)="[All]" then 
10750     a$=""
10760   else 
10770     a$=lpad$(trim$(resp$(acct_i)(1:10)),10)
10780     st1$=a$
10790   end if 
10800   if resp$(rt_i)="[All]" then 
10810     prtbkno=0
10820   else 
10830     prtbkno=val(resp$(rt_i))
10840   end if 
10850   if resp$(selacct_i)="True" then sl1=1 else sl1=0
10860   if trim$(a$)<>"" then 
10870     read #1,using L480,key=a$: z$,route,sequence nokey SCREEN1
10880     st1=1
10890   end if 
10900 L480: form pos 1,c 10,pos 1741,n 2,n 7
10910   if trim$(a$)="" and prtbkno=0 then restore #2,key>="         ": ! if no beginning account or starting route #, start at beginning of file
10920   if trim$(a$)<>"" then restore #2,key=cnvrt$("pic(zz)",route)& cnvrt$("pic(zzzzzzz)",sequence): nokey SCREEN1
10930   if trim$(a$)="" and prtbkno>0 then restore #2,key>=cnvrt$("pic(zz)",prtbkno)&"       ": ! selected a route and no beginning Account
10940 !
10950   open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,input,keyed 
10960 ! fnOPENPRN
10970 !
10980 ! IF SL1=0 THEN GOSUB SORT1
10990 L570: if sl1=1 then goto SCREEN3
11000   if s5=0 then goto L640
11010 L590: read #7,using L600: r6 eof F5_CANCEL
11020 L600: form pos 1,pd 3
11030   read #6,using "Form POS 1,C 5,C 4,C 10",rec=r6: zip5$,cr$,z$ noRec L590
11040   read #1,using L650,key=z$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,estimatedate nokey L590
11050   goto L650
11060 L640: read #2,using L650: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,estimatedate eof F5_CANCEL
11070 L650: form pos 1,c 10,4*c 30,c 12,pos 147,pd 2,pos 157,11*pd 4.2,pos 1821,n 1,pos 217,15*pd 5,pd 4.2,pd 4,12*pd 4.2,pos 385,pd 3,pos 388,10*pd 5.2,pos 1741,n 2,pos 1831,n 9
11080   if prtbkno=0 then goto L680
11090   if prtbkno><route then goto F5_CANCEL
11100 L680: if f><d1 then goto L570
11110   if st1=0 then goto HERE
11120   if st1$=z$ then st1=0 else goto L570
11130 HERE: ! 
11140 ! read alternate billing address
11150   mat ba$=("") : read #3,using L740,key=z$: mat ba$ nokey L810
11160 L740: form pos 11,4*c 30
11170 !
11180 L810: e1=0 : mat pe$=("")
11190   for j=2 to 4
11200     if rtrm$(e$(j))<>"" then 
11210       e1=e1+1
11220       pe$(e1)=e$(j)
11230     end if 
11240   next j
11250   goto L950
11260 !
11270 F5_CANCEL: ! 
11280   close #1: ioerr L890
11290 L890: close #3: ioerr L900
11300 L900: ! 
11320 L920: fnpa_finis
11330   goto ENDSCR
11340 !
11350 L950: ! 
11360   pb=bal-g(11)
11370 ! ______________print bill routine______________________________________
11380   fn_vbprint
11390 ! _____________end of pr routine______________________________________
11400   bct(2)=bct(2)+1
11410 ! accumulate totals
11420   goto L570
11430 !
11440 SCREEN3: ! 
11450   sn$="UBPrtBl1-2"
11460   fnTos(sn$)
11470   txt$="Account (blank to stop)"
11480   fnLbl(1,1,txt$,31,1)
11490   if trim$(a$)="" then goto L1070 else goto L1080
11500 L1070: if z$<>"" then 
11510     txt$="Last Account entered was "&z$
11520     fnLbl(3,1,txt$,44,1)
11530   else 
11540     txt$=""
11550     fnLbl(3,1,txt$,44,1)
11560   end if 
11570 L1080: fncmbact(1,17) ! 
11580   resp$(1)=a$
11590   fnCmdSet(11): fnAcs(sn$,0,mat resp$,ck)
11600   if ck=5 then goto F5_CANCEL
11610   a$=lpad$(trim$(resp$(1)(1:10)),10)
11620   if trim$(a$)="" then goto F5_CANCEL
11630   read #1,using L650,key=a$: z$,mat e$,f$,a3,mat b,final,mat d,bal,f,mat g,bra,mat gb,route,estimatedate nokey SCREEN3
11640   goto HERE
11650 !
11660 SORT1: ! SELECT & SORT
11670   open #5: "Name=[Q]\UBmstr\Cass1.h[cno],KFName=[Q]\UBmstr\Cass1Idx.h[cno],Shr",internal,input,keyed ioerr L1410
11680   open #6: "Name="&env$('Temp')&"\Temp."&wsid$&",Replace,RecL=19",internal,output 
11690   s5=1
11700   if prtbkno=0 then routekey$="" else routekey$=cnvrt$("N 2",prtbkno)&"       " ! key off first record in route (route # no longer part of customer #)
11710   restore #2,search>=routekey$: 
11720 L1210: read #2,using L1220: z$,f,route eof END5
11730 L1220: form pos 1,c 10,pos 296,pd 4,pos 1741
11740   if prtbkno=0 then goto L1250
11750   if prtbkno><route then goto END5
11760 L1250: if f><d1 then goto L1210
11770   zip5$=cr$=""
11780   read #5,using "Form POS 96,C 5,POS 108,C 4",key=z$: zip5$,cr$ nokey L1280
11790 L1280: write #6,using "Form POS 1,C 5,C 4,C 10": zip5$,cr$,z$
11800   goto L1210
11810 !
11820 END5: close #6: 
11830   open #9: "Name="&env$('Temp')&"\Control."&session$&",Size=0,RecL=128,Replace",internal,output 
11840 L1330: form pos 1,c 128
11850   write #9,using L1330: "File "&env$('Temp')&"\Temp."&wsid$&",,,"&env$('Temp')&"\Addr."&session$&",,,,,A,N"
11860   write #9,using L1330: "Mask 1,19,C,A"
11870   close #9: 
11880   execute "Free "&env$('Temp')&"\Addr."&session$ ioerr L1380
11890 L1380: execute "Sort "&env$('Temp')&"\Control."&session$
11900   open #6: "Name="&env$('Temp')&"\Temp."&wsid$,internal,input,relative 
11910   open #7: "Name="&env$('Temp')&"\Addr."&session$,internal,input,relative 
11920 L1410: return 
11930 !
11940 ENDSCR: ! pr totals screen
11950   if sum(bct)=0 then pct=0 else pct=bct(2)/sum(bct)*100
11960   fnTos(sn$="Bills-Total")
11970   mylen=23 : mypos=mylen+2
11980   respc=0
11990   fnLbl(1,1,"Total Bills Printed:",mylen,1)
12000   fnTxt(1,mypos,8,0,1,"",1)
12010   resp$(respc+=1)=cnvrt$("N 8",sum(bct))
12040   fnCmdSet(52)
12050   fnAcs(sn$,0,mat resp$,ck)
12060 XIT: fnxit
12070 !
12080 ERTN: fnerror(program$,err,line,act$,"xit")
12090   if uprc$(act$)<>"PAUSE" then goto L1550
12100   execute "List -"&str$(line) : pause : goto L1550
12110   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause 
12120 L1550: execute act$
12130   goto ERTN
12140 !
12150   def fn_vbprint
12160     if file(20)=-1 then 
12170       fnPa_open("Landscape")
12210       lyne=3
12230     end if 
12240 ! -- Standard 4 Per Page Even Perferated Card Stock Bills
12250     billcounter+=1
12260     if billcounter=1 then xmargin=1 : ymargin=10
12270     if billcounter=2 then xmargin=140 : ymargin=10
12280     if billcounter=3 then xmargin=1 : ymargin=118
12290     if billcounter=4 then xmargin=140 : ymargin=118 : billcounter=0
12300 !
12310 ! pr #20: 'Call Print.AddLine('&str$(xmargin+5)&','&str$(ymargin+2)&',57,'&str$(lyne*3+3)&',True)'
12320     if reading_date_cur_s1=0 then reading_date_cur=d3 else reading_date_cur=reading_date_cur_s1
12330     if reading_date_prior_s1=0 then reading_date_prior=d2 else reading_date_prior=reading_date_prior_s1
12340 ! fnpa_txt('#'&trim$(z$),xmargin+4,lyne*5+ymargin)
12350 ! pr #20: 'Call Print.AddText("'&e$(1)&'",'&str$(xmargin+4)&','&str$(lyne*6+ymargin)&')'
12360 ! pr #20: 'Call Print.AddText("From: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_prior/100))&'  To: '&cnvrt$("PIC(ZZ/ZZ)",int(reading_date_cur/100))&'",'&str$(xmargin+2)&','&str$(lyne*7+ymargin)&')'
12370 ! pr #20: 'Call Print.AddText("Is due now and payable.",'&str$(xmargin+2)&','&str$(lyne*8+ymargin)&')'
12380 ! pr #20: 'Call Print.AddText("Billing Date: '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d1)&'",'&str$(xmargin+2)&','&str$(lyne*11+ymargin)&')'
12390 ! pr #20: 'Call Print.AddLine('&str$(xmargin+1)&','&str$(lyne*12+1+ymargin)&',62,0)'
12400 ! fnpa_line(xmargin+1,lyne*12+1+ymargin,62,0)
12410 ! pr #20: 'Call Print.AddText("Reading",'&str$(xmargin+10)&','&str$(lyne*13+ymargin)&')'
12420 ! pr #20: 'Call Print.AddText("Usage",'&str$(xmargin+33)&','&str$(lyne*13+ymargin)&')'
12430 ! pr #20: 'Call Print.AddText("Charge",'&str$(xmargin+50)&','&str$(lyne*13+ymargin)&')'
12440 !
12450 PRINTGRID: ! 
12460     meter=3
12470     pr #20: 'Call Print.MyFontSize(8)'
12480     pr #20: 'Call Print.AddText("Reading Date",'&str$(xmargin+1)&','&str$(-1*lyne+ymargin)&')'
12490     pr #20: 'Call Print.AddText("'&trim$(cnvrt$("PIC(ZZ/ZZ/ZZ",d5))&'",'&str$(xmargin+1)&','&str$(ymargin)&')'
12500     pr #20: 'Call Print.AddText("'&cnvrt$("PIC(ZZ/ZZ/ZZ",d1)&'",'&str$(xmargin+48)&','&str$(ymargin)&')'
12510     if g(1)<>0 then 
12520       pr #20: 'Call Print.AddText("WTR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12530       pr #20: 'Call Print.AddText("'&fnformnumb$(d(1),0,9)&'",'&str$(xmargin+8)&','&str$(lyne*meter+ymargin)&')'
12540       pr #20: 'Call Print.AddText("'&fnformnumb$(d(3),0,9)&'",'&str$(xmargin+27)&','&str$(lyne*meter+ymargin)&')'
12550       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12560     end if 
12570     if g(2)<>0 then 
12580       pr #20: 'Call Print.AddText("SWR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12590       pr #20: 'Call Print.AddText("'&fnformnumb$(g(2),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12600     end if 
12610     if g(3)<>0 then 
12620       pr #20: 'Call Print.AddText("Admin",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12630       pr #20: 'Call Print.AddText("'&fnformnumb$(g(3),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12640     end if 
12650     if a4=1 then 
12660       gcode$="RSGS"
12670     else if a4=2 then 
12680       gcode$="CMGS"
12690     else if a4=3 then 
12700       gcode$="INGS"
12710     else 
12720       gcode$="GAS"
12730     end if 
12740     if g(4)<>0 then 
12750       pr #20: 'Call Print.AddText("'&gcode$&'",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12760       pr #20: 'Call Print.AddText("'&fnformnumb$(d(9),0,9)&'",'&str$(xmargin+8)&','&str$(lyne*meter+ymargin)&')'
12770       pr #20: 'Call Print.AddText("'&fnformnumb$(d(11),0,9)&'",'&str$(xmargin+27)&','&str$(lyne*meter+ymargin)&')'
12780       pr #20: 'Call Print.AddText("'&fnformnumb$(g(4),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12790     end if 
12800     if g(5)<>0 or g(6)<>0 or g(7)<>0 then 
12810 ! pr #20: 'Call Print.AddText("PEN",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12820 ! pr #20: 'Call Print.AddText("'&fnformnumb$(g(5)+g(6)+g(7),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12830     end if 
12840 ! if g(6)<>0 then
12850 !  pr #20: 'Call Print.AddText("FUR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12860 !  pr #20: 'Call Print.AddText("'&fnformnumb$(g(6),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12870 ! . end if
12880     if g(10)<>0 then 
12890 ! pr #20: 'Call Print.AddText("Penalty",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12900 ! pr #20: 'Call Print.AddText("'&fnformnumb$(g(10),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12910     end if 
12920     if g(8)<>0 then 
12930       pr #20: 'Call Print.AddText("MISC",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12940       pr #20: 'Call Print.AddText("'&fnformnumb$(g(8),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12950     end if 
12960     if g(9)<>0 then 
12970       pr #20: 'Call Print.AddText("TAX",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
12980       pr #20: 'Call Print.AddText("'&fnformnumb$(g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
12990     end if 
13000     if pb><0 then pr #20: 'Call Print.AddLine('&str$(xmargin+46)&','&str$(lyne*(meter+=1)+ymargin)&',15,0)'
13010     if pb><0 then 
13020       pr #20: 'Call Print.AddText("   Subtotal",'&str$(xmargin+1)&','&str$(lyne*(meter+=.25)+ymargin)&')'
13030       pr #20: 'Call Print.AddText("'&fnformnumb$(g(1)+g(2)+g(3)+g(4)+g(5)+g(6)+g(7)+g(8)+g(9),2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
13040     end if 
13050     if pb<>0 then 
13060       pr #20: 'Call Print.AddText("Previous Balance",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
13070       pr #20: 'Call Print.AddText("'&fnformnumb$(pb,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'
13080     end if 
13081     if estimatedate=d1 then 
13082       fnpa_txt("Bill estimated!",xmargin+1,11*lyne+ymargin)
13083       fnpa_txt(mg$(1),xmargin+1,12*lyne+ymargin)
13084       fnpa_txt(mg$(2),xmargin+1,13*lyne+ymargin)
13085     else 
13090       fnpa_txt(mg$(1),xmargin+1,11*lyne+ymargin)
13100       fnpa_txt(mg$(2),xmargin+1,12*lyne+ymargin)
13110       fnpa_txt(mg$(3),xmargin+1,13*lyne+ymargin)
13111     end if 
13120 ! If ESCROW>0 Then
13130 ! pr #20: 'Call Print.AddText("Escrow CR",'&str$(xmargin+1)&','&str$(lyne*(meter+=1)+ymargin)&')'
13140 ! pr #20: 'Call Print.AddText("'&fnformnumb$(escrow,2,9)&'",'&str$(xmargin+45)&','&str$(lyne*meter+ymargin)&')'\
13150 ! end if
13160     pr #20: 'Call Print.MyFontSize(9)'
13170 !
13180 ! if estimatedate=d1 then let fnpa_line("Bill estimated!",xmargin+1,lyne*29+ymargin)
13190 ! fnpa_line(xmargin+1,lyne*25+1+ymargin,63,0)
13200     if bal>0 then 
13210       fnpa_txt(fnformnumb$(round(bal*1.1,2),2,8),xmargin-4,lyne*19.5+ymargin)
13220     else 
13230       fnpa_txt(fnformnumb$(bal,2,8),xmargin-4,lyne*19.5+ymargin)
13240     end if 
13250     fnpa_txt(cnvrt$("PIC(ZZ/ZZ/ZZ)",d4),xmargin+20,lyne*19.5+ymargin)
13260     fnpa_txt(fnformnumb$(bal,2,9),xmargin+39,lyne*19.5+ymargin)
13270     addy=23.6 ! 14
13280     fnpa_txt('#'&trim$(z$),xmargin+8,lyne*23.5+ymargin)
13290 ! if pe$(1)<>"" then let fnpa_txt(trim$(pe$(1)),xmargin+9,lyne*(addy+=1.1)+ymargin)
13300     if trim$(ba$(1))="" and trim$(ba$(2))="" and trim$(ba$(3))="" and trim$(ba$(4))="" then 
13310       if trim$(pe$(2))<>"" then let fnpa_txt(trim$(pe$(2)),xmargin+8,lyne*(addy+=1.1)+ymargin)
13320       if trim$(pe$(3))<>"" then let fnpa_txt(trim$(pe$(3)),xmargin+8,lyne*(addy+=1.1)+ymargin)
13330       if trim$(pe$(4))<>"" then let fnpa_txt(trim$(pe$(4)),xmargin+8,lyne*(addy+=1.1)+ymargin)
13340     else 
13350       if trim$(ba$(2))<>"" then let fnpa_txt(trim$(ba$(2)),xmargin+8,lyne*(addy+=1.1)+ymargin)
13360       if trim$(ba$(3))<>"" then let fnpa_txt(trim$(ba$(3)),xmargin+8,lyne*(addy+=1.1)+ymargin)
13370       if trim$(ba$(4))<>"" then let fnpa_txt(trim$(ba$(4)),xmargin+8,lyne*(addy+=1.1)+ymargin)
13380     end if 
13390     fnpa_txt(trim$(e$(1)),xmargin+8,lyne*(addy+=1.5)+ymargin)
13400 ! fnpa_txt("  Office 217-628-3416",xmargin+1,lyne*28.5+ymargin)
13410 !
13420     special=28
13430 !
13440     pr #20: 'Call Print.MyFontSize(10)'
13450     if bal>0 then 
13460 ! fnpa_txt('3Pay By '&cnvrt$("PIC(ZZ/ZZ/ZZ)",d4)&':',csp-2,factor+line_height*11)
13470       fnpa_txt(fnformnumb$(round(bal*1.1,2),2,9),xmargin+68,lyne*7+ymargin)
13480     else 
13490       fnpa_txt(fnformnumb$(bal,2,9),xmargin+68,lyne*7+ymargin)
13500     end if 
13510     fnpa_txt(fnformnumb$(bal,2,9),xmargin+102,lyne*7+ymargin)
13520     pr #20: 'Call Print.MyFontSize(12)'
13530     addy=10
13540     fnpa_txt('#'&trim$(z$),xmargin+71,lyne*(addy+=1.3)+ymargin)
13550     if pe$(1)<>"" then let fnpa_txt(trim$(pe$(1)),xmargin+71,lyne*(addy+=1.5)+ymargin)
13560     if pe$(2)<>"" then let fnpa_txt(trim$(pe$(2)),xmargin+71,lyne*(addy+=1.3)+ymargin)
13570     if pe$(3)<>"" then let fnpa_txt(trim$(pe$(3)),xmargin+71,lyne*(addy+=1.3)+ymargin)
13580     if pe$(4)<>"" then let fnpa_txt(trim$(pe$(4)),xmargin+71,lyne*(addy+=1.3)+ymargin)
13590     pr #20: 'Call Print.MyFontSize(10)'
13600     if final>0 then let fnpa_txt("Final Bill",xmargin+75,lyne*(addy+5)+ymargin)
13610 ! 
13620     if billcounter=0 then 
13630       fnpa_newpage
13640     end if 
13650   fnend 
13660 !
