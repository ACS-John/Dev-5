00010 ! Replace S:\acsCL\VnFund
00020 ! pr Vendor Fund Listing
00030 !
00040   library 'S:\Core\Library': fntop,fnxit, fnopenprn,fncloseprn,fncno,fnerror,fntop,fnxit,fnTos,fnLbl,fnTxt,fnCmdSet,fnAcs,fndate_mmddyy_to_ccyymmdd
00050   on error goto Ertn
00060 !
00070   dim cnam$*40,cap$*128,p$(20)*50
00080   dim tr$(5)*35,tr(2),p1$*30,gl(3),f1(1000),in1(3),f2(1000)
00090 !
00100   fncno(cno,cnam$)
00110   fntop(program$, cap$="Payee Fund Listing")
00120   cancel=99
00130 !
00140   fnTos(sn$="vnfund") !:
        respc=0
00150   fnLbl(1,40,"",1,1)
00160   fnLbl(1,1,"Starting Date:",25,1)
00170   fnTxt(1,27,8,0,1,"1") !:
        resp$(respc+=1)=""
00180   fnLbl(2,1,"Ending Date:",25,1)
00190   fnTxt(2,27,8,0,1,"1") !:
        resp$(respc+=1)=""
00200   fnLbl(3,1,"Minimum Amount to Print:",25,1)
00210   fnTxt(3,27,10,0,1,"10") !:
        resp$(respc+=1)=""
00220   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
00230   if ckey=5 then goto XIT
00240   in1(1)=val(resp$(1))
00250   in1(2)=val(resp$(2))
00260   in1(3)=val(resp$(3))
00270   open #paymstr=13: "Name=[Q]\CLmstr\PayMstr.H[cno],KFName=[Q]\CLmstr\PayIdx1.H[cno],Shr",internal,outIn,keyed 
00280   open #trmstr1=1: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx1.H[cno],Shr",internal,outIn,keyed 
00290   open #trmstr2=2: "Name=[Q]\CLmstr\TrMstr.H[cno],KFName=[Q]\CLmstr\TrIdx2.H[cno],Shr",internal,outIn,keyed 
00300   open #tralloc=3: "Name=[Q]\CLmstr\TrAlloc.H[cno],Version=2,KFName=[Q]\CLmstr\TrAlloc-Idx.h[cno],Shr",internal,outIn,keyed 
00310   fnopenprn
00320   gosub HDR
00330 READ_TRMSTR2: ! 
00340   read #trmstr2,using 'Form POS 1,N 2,N 1,C 8,G 6,pd 10.2,C 8,C 35,N 1,N 6,N 1': bank_code,tcde,tr$(1),tr$(2),tr3,tr$(4),tr$(5),pcde,clr,scd eof END1 !:
        tr$(3)=str$(tr3)
00345   if tcde=1 or tcde=4 then goto L350 else goto READ_TRMSTR2
00350 L350: if scd=1 or scd=8 then goto L360 else goto READ_TRMSTR2
00360 L360: d1=fndate_mmddyy_to_ccyymmdd(val(tr$(2)))
00365 ! If BANK_CODE=77 Then Pause
00370   if d1<fndate_mmddyy_to_ccyymmdd(in1(1)) or d1>fndate_mmddyy_to_ccyymmdd(in1(2)) then goto READ_TRMSTR2
00380   if trim$(tr$(4))="" then goto READ_TRMSTR2 !:
          ! don't try to analyze any transaction that does not have a vendor #
00390   if rtrm$(vn$)="" then goto L410
00400   if vn$><tr$(4) then gosub PRINTARRAY
00410 L410: vn$=tr$(4)
00420   p1$=tr$(5)(1:30)
00430   key$=cnvrt$("Pic(zz)",bank_code)&str$(tcde)&tr$(1) !:
        restore #tralloc,key>=key$: nokey READ_TRMSTR2
00440 READ_TRALLOC: ! 
00450   read #tralloc,using 'Form Pos 1,C 11,N 3,N 6,N 3,PD 5.2': newkey$,mat gl,amt eof READ_TRMSTR2 !:
        if newkey$<>key$ then goto READ_TRMSTR2
00460   if gl(1)=0 then !:
          f1(1000)+=amt else !:
          f1(gl(1))+=amt
00470   goto READ_TRALLOC
00480 !
00490 PRINTARRAY: ! pr ARRAY
00500   if sum(f1)<in1(3) then goto RESET_F1
00510   if trim$(vn$)<>'' then !:
          read #paymstr,using 'Form POS 9,C 30',key=vn$: p1$ nokey L520
00520 L520: pr #255,using 'Form POS 1,C 10,C 30': vn$,p1$
00530 PRINTARRAY_2: ! 
00540   for j=1 to 1000
00550     if f1(j)<>0 then !:
            pr #255,using 'Form POS 10,C 12,PIC(----,---,---,---.##)': "Fund: "&str$(j),f1(j) pageoflow NEWPGE
00560   next j
00570   pr #255,using 'Form POS 10,C 12,PIC(----,---,---,---.##)': " Total  ",sum(f1)
00580   pr #255: ""
00590   mat f2=f2+f1
00600 RESET_F1: mat f1=(0)
00610   return 
00620 !
00630 NEWPGE: ! 
00640   pr #255: newpage
00650   gosub HDR
00660   continue 
00670 !
00680 HDR: ! 
00690   pr #255,using 'Form POS 1,C 4,N 4,Cc 72': "Page",pg+=1,cnam$
00700   pr #255,using 'Form POS 1,c 8,Cc 72': date$,"Date From: "&cnvrt$("pic(zz/zz/zz)",in1(1))&" To: "&cnvrt$("pic(zz/zz/zz)",in1(2))
00710   pr #255,using 'Form POS 9,Cc 72': "Vendor Fund Listing (Minimum Amount: "&ltrm$(cnvrt$("PIC($$$$,$$$,$$$.##BCR)",in1(3)))&")" !:
        pr #255: ""
00720   return 
00730 !
00740 END1: gosub PRINTARRAY
00750   pr #255,using 'Form POS 1,C 10,C 30': "  Grand","Total"
00760   mat f1=f2
00770   gosub PRINTARRAY_2
00780   fncloseprn
00790   goto XIT
00800 !
00810 XIT: fnxit
00820 !
00830 ! <Updateable Region: ERTN>
00840 ERTN: fnerror(program$,err,line,act$,"xit")
00850   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00860   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00870   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00880 ERTN_EXEC_ACT: execute act$ : goto ERTN
00890 ! /region
00900 !
