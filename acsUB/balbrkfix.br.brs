00010 ! (foundone is special for montincello. Take out refereence to foundone on any others,, they have 3 cycles.
00020   library 'S:\Core\Library': fntop,fnxit, fnAcs,fnwait,fnopenprn,fncloseprn,fnerror,fnmsgbox,fnTxt,fnLbl,fnTos,fnCmdSet,fnget_services
00030   form pos 1,c 9,skip 0
00040   fntop("S:\acsUB\balbrkfix",cap$="Fix Balance Breakdown")
00050   dim dat$*20,ln$*132,sde$*30,cb(13),a$(61)*30,u(61),gb(10),a(7)
00070 ! :  !
00080   dim o(2),alloc(10),g(10),answer(10,3)
00090   dim adr(2),gb(10),tgb(10),a$(61)*30,u(61)
00100   dim serviceName$(10)*20,service$(10)*2,tax_code$(10)*1,penalty$(10)*1,subjectto(10)
00110   dim t1$(11)*25,ta(2),t2$(4)*25,io1$(3),dt1(31),dt2(31),cap$*128,txt$*80
00180   fnopenprn
00190   gosub SCR1
00200   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,outIn,keyed 
00210   open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],KFName=[Q]\UBmstr\UBTrIndx.h[cno],Shr",internal,input,keyed 
00220   fnget_services(mat serviceName$,mat service$,mat tax_code$,mat penalty$,mat subjectto,mat apply)
00230   for j=1 to 10
00240     serviceName$(j)=lpad$(rtrm$(serviceName$(j)(1:8)),8)
00250   next j
00260   gosub NWPGE
00270 L270: read #1,using L280: z$,mat a,bal,mat g,mat gb eof DONE
00280 L280: form pos 1,c 10,pos 143,7*pd 2,pos 292,pd 4.2,pos 300,10*pd 4.2,pos 388,10*pd 5.2
00290   foundone=0
00300 ! ------------------------------
00310   if bal=sum(gb) then goto L270
00320   if bal=0 then mat gb=(0): cause$="one": goto REW
00330   if bal<0 and a(1)>0 then mat gb=(0) : gb(1)=bal : cause$="two": goto REW
00340   if bal<0 and a(2)>0 then mat gb=(0) : gb(2)=bal : cause$="three": goto REW
00350   if bal<0 and a(3)>0 then mat gb=(0) : gb(3)=bal : cause$="three": goto REW
00360   if bal<0 and a(4)>0 then mat gb=(0) : gb(4)=bal : cause$="three": goto REW
00370   if bal<0 then mat gb=(0) : gb(5)=bal : cause$="three": goto REW ! if credit balance and no water,sewer,elec,or gas then credit show in gb(5)
00380   runtotal=0
00390   mat gb=(0)
00400   for j=1 to 10 ! types of charges  (OWE CURRENT BILL)
00410     if penalty$(j)="Y" and tcode=1 then goto L450 ! SKIP PENALTY RECORDS
00420     gb(j)=min(g(j),(bal-runtotal))
00430     if sum(gb)=bal then goto REW
00440     runtotal=runtotal+gb(j)
00450 L450: next j
00460   if sum(gb)= bal then goto REW
00470   restore #2,key>=z$&"         ": nokey L570 ! FIND IN TRANSACTION HISTORY
00480   mat answer=(0): mat gb=(0): runtotal=0
00490 L490: read #2,using L500: p$,tdate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode eof L650
00500 L500: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00510   if p$<>z$ then goto L650 !:
          ! history record must belong to this customer
00520   if tcode=1 or tcode=2 then goto L530 else goto L490 ! charge and penalty transaction
00530 L530: for j=1 to 3
00540     if str$(tdate)=resp$(j) then foundone=1: goto L570
00550     if str$(tdate)=resp$(j+3) then goto L570 ! penalties
00560     goto L630
00570 L570: if sum(gb)= bal then goto REW
00580     for j2=1 to 10
00590 ! If PENALTY$(J2)="Y" AND TCODE=1 Then Goto 570 ! SKIP PENALTY RECORDS
00600       answer(j2,j)+=tg(j2)
00610     next j2
00620     goto L490 ! read next transaction
00630 L630: next j
00640   goto L490
00650 L650: x=1
00660 L660: for k=1 to 10
00670     if apply(k)=0 then goto L710
00680     gb(k)+=min(answer(k,x),max(0,(bal-runtotal)))
00690     if sum(gb)=bal then goto REW
00700     runtotal=runtotal+min(answer(k,x),max(0,(bal-runtotal)))
00710 L710: next k
00720   if x<3 then x+=1: goto L660
00730   cause$="four"
00740   for z=1 to 10
00750     if trim$(service$(z))<>"" then gb(z)+=bal-runtotal: pr #255,using "form pos 1,c 50": "Plugging first service by "&str$(bal-runtotal): goto L780 ! plug anything LEFT OVER TO first service  (change <>"" to ="PN" to plug to penalty
00760   next z
00770 ! Next J
00780 L780: if foundone=0 then goto L820 ! one of dates does not match the last billing date don't change
00790   goto REW
00800 L800: pr #255,using L810: z$,bal,sum(gb),mat gb
00810 L810: form pos 1,c 10,x 1,12*n 9.2
00820 L820: goto L270
00830 NWPGE: ! pr #255: NEWPAGE
00840   pr #255: "Account    Balance   TotBrk   "&serviceName$(1)(1:8)&" "&serviceName$(2)(1:8)&" "&serviceName$(3)(1:8)&" "&serviceName$(4)(1:8)&" "&serviceName$(5)(1:8)&" "&serviceName$(6)(1:8)&" "&serviceName$(7)(1:8)&" "&serviceName$(8)(1:8)&" "&serviceName$(9)(1:8)&" "&serviceName$(10)(1:8)&" "
00850   pr #255: "__________ _________ ________ ________ ________ ________ ________ ________ ________ ________ ________ ________ ________"
00860   return 
00870 DONE: fncloseprn
00880 XIT: fnxit
00890 REW: ! 
00900   rewrite #1,using L910,key=z$: mat gb
00910 L910: form pos 388,10*pd 5.2
00920 ! pr #255: "Rewrote next record "&CAUSE$
00930   goto L800
00940 SCR1: ! 
00950   sn$="balbrkfix" !:
        fnTos(sn$) !:
        mylen=62 : mypos=50
00960   txt$="Billing Dates for last three months:" !:
        fnLbl(1,1,txt$,mylen,1)
00970   for j=1 to 3 !:
          fnTxt(j+1,mypos,10,0,0,"3",0,"Put your most recent billing date first and then in order from there.") !:
          resp$(j)="" !:
        next j
00980   txt$="Penalty Dates for last three months:" !:
        fnLbl(5,1,txt$,mylen,1)
00990   for j=1 to 3 !:
          fnTxt(j+5,mypos,10,0,0,"3",0,"Put your most recent penalty date first and then in order from there.") !:
          resp$(j+3)="" !:
        next j
01000   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
01010   if ckey=5 then goto XIT
01020   for j=1 to 6
01030 L1030: x=pos(resp$(j),"/",1)
01040     if x>0 then resp$(j)(x:x)="": goto L1030
01050   next j
01060   for j=1 to 6 !:
          cd1(j)=val(resp$(j)) conv SCR1 !:
        next j
01070   if cd1(1)=0 then !:
          mat message$(1): mytype=0 !:
          message$(1)="You must enter at least one date!" !:
          fnmsgbox(mat message$,resp$,cap$,mytype) !:
          goto SCR1
01080   return 
01090 ! ______________________________________________________________________
