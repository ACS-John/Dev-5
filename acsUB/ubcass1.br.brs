20000 ! Replace S:\acsUB\ubCass1
20020 ! -- Creates Address Certification File:
20040 !
20060 !
20080   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnxit,fnTos,fnAcs,fnLbl,fnwait,fncsz,fnCmdSet,fntop,fngethandle
20100   on error goto Ertn
20120 !
20140   dim z$,nam$*30,sta$*30,sta2$*30,csz$*30,opt1$(7),txt$*80,cap$*128
20160 !
20180   fntop("S:\acsUB\ubCass1",cap$="Create Certification File")
20200   fncno(cno)
20220 !
20240 ! r: a screen
20260 ! fnTos(sn$:="ubCass1")
20280 ! respc=0
20300 ! fnLbl(1,1,"Destination Path for Cass file:",33,1)
20320 ! opt1$(1)="A:\"
20340 ! opt1$(2)="B:\"
20360 ! opt1$(3)="C:\"
20380 ! opt1$(4)="E:\"
20400 ! opt1$(5)="F:\"
20420 ! opt1$(6)="G:\"
20440 ! opt1$(7)="H:\"
20460 ! fncomboa("AB",1,35,mat opt1$)
20480 ! resp$(respc+=1)=opt1$(1)
20500 ! fnCmdSet(2)
20520 ! L170: fnAcs(sn$,0,mat resp$,ckey)
20540 ! if ckey=5 then goto XIT
20560 ! dv$=resp$(1)
25000 ! /r
25020   open #h_out:=fngethandle: "Name=SAVE:ubCass1.dat,RecL=112,EOL=None,Replace",external,output ioerr XIT
25040 !  pr file$(h_tmp);" ";lrec(h_tmp)
25060 !  save_name$=os_filename$(file$(h_tmp))
25080 !  close #h_tmp,free:
25100 ! 
25120 ! open #h_out:=2: "Name="&dv$&"ubCass1.dat,RecL=112,EOL=None,Replace",external,output ! ioerr l170
25140   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
25160   open #3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,outIn,keyed 
28000 ! r:  main loop
28020   do 
28040     read #1,using L250: z$,nam$,sta$,csz$,fb1,lbd,altcode eof XIT
28060 L250: form pos 1,c 10,pos 41,3*c 30,pos 213,pd 4,pos 296,pd 4,pos 385,pd 3,pos 1854,pd 5.2
28080     if altcode=2 then goto L350 ! alternate address coded as not being used at this time.
28100     read #3,using L280,key=z$: nam$,sta$,sta2$,csz$ nokey L300
28120 L280: form pos 11,4*c 30
28140     if trim$(csz$)="" then csz$=sta2$ : sta2$=""
28160 L300: csz$=trim$(csz$)
28180     if csz$(len(csz$)-5:len(csz$)-5) =" " then goto L330
28200     if csz$(len(csz$)-5:len(csz$)-5) < "0" or csz$(len(csz$)-5:len(csz$)-5)>"9" then csz$=csz$(1:len(csz$)-5) &" "&csz$((len(csz$)-4):len(csz$))
28220 L330: fncsz(csz$,city$,state$,zip$)
28240     zip$=trim$(zip$)(1:5)
28260 L350: write #h_out,using F_OUT_FIXED: z$,nam$,sta$,city$,state$,zip$,chr$(10)
28280 F_OUT_FIXED: form pos 1,c 10,c 30,c 30,c 23,c 2,c 5,pos 112,c 1
28300   loop 
28320 ! /r
28340 XIT: fnxit
30000 ! <Updateable Region: ERTN>
30020 ERTN: fnerror(program$,err,line,act$,"xit")
30040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
30060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
30080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
30100 ERTN_EXEC_ACT: execute act$ : goto ERTN
30120 ! /region
