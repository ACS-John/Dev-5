00010 ! Replace S:\acsUB\UBRECRTR
00020 !
00030   library 'S:\Core\Library': fncno,fnxit,fnerror,fnwait,fnAcs,fnTos,fnLbl,fnTxt,fnCmdSet,fntop
00040   on error goto Ertn
00050 !
00060   dim p$*10,z$*10,o(2),adr(2),cap$*128,txt$*40,gb(10),tg(11),d(15)
00070 !
00080   fncno(cno)
00090 ! 
00100   fntop("S:\acsUB\UbRecrTr",cap$="Recreate Transaction File")
00110   sn$="ubRecrTr" !:
        fnTos(sn$) !:
        mylen=30 !:
        mypos=mylen+2
00120   txt$="Transaction Date:" !:
        fnLbl(1,1,txt$,mylen,1)
00130   fnTxt(1,mypos,10,0,0,"3") !:
        resp$(1)=date$("ccyymmdd")
00140   fnLbl(3,1,"Warning ! Do not continue",mylen,1)
00150   fnLbl(4,1,"without consulting ACS",mylen,1)
00160   fnLbl(4,15,"",mylen,1)
00170   fnCmdSet(2): fnAcs(sn$,0,mat resp$,ckey)
00180 L180: x=pos(resp$(1),"/",1) !:
        if x>0 then resp$(1)(x:x)="": goto L180
00190   trandate=val(resp$(1))
00200   if ckey=5 then goto XIT
00210   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],NoShr",internal,outIn,keyed 
00220   open #2: "Name=[Q]\UBmstr\UBTransVB.h[cno],RecL=102,Replace",internal,output 
00230 READ_CUSTOMER: ! 
00240 L240: read #1,using L250: z$,bal,mat d,mat gb eof READ_CUSTOMER_EOF
00250 L250: form pos 1,c 10,pos 292,pd 4.2,pos 217,15*pd 5,pos 388,10*pd 5.2
00260   if bal=0 then goto L240
00270   if bal<0 then !:
          tcode=3 else !:
          tcode=1
00280   if tcode=3 then tamount=-bal else tamount=bal
00290   for j=1 to 10
00300     tg(j)=gb(j)
00310     if tcode=3 then tg(j)=-tg(j)
00320   next j
00330   tbal=bal
00340   wr=d(1): wu=d(3): er=d(5): eu=d(7): gr=d(9): gu=d(11)
00350   write #2,using L360: z$,trandate,tcode,tamount,mat tg,wr,wu,er,eu,gr,gu,tbal,pcode
00360 L360: form pos 1,c 10,n 8,n 1,12*pd 4.2,6*pd 5,pd 4.2,n 1
00370   goto READ_CUSTOMER
00380 !
00390 READ_CUSTOMER_EOF: ! 
00400   close #1: 
00410   close #2: 
00420   execute "Index [Q]\UBmstr\UBTransVB.h[cno]"&' '&"[Q]\UBmstr\UBTrIndx.h[cno] 1 19 Replace DupKeys -n"
00430   goto XIT
00440 !
00450 XIT: fnxit
00460 !
00470 ! <Updateable Region: ERTN>
00480 ERTN: fnerror(program$,err,line,act$,"NO")
00490   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00500   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00510   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00520 ERTN_EXEC_ACT: execute act$ : goto ERTN
00530 ! /region
00540 !
