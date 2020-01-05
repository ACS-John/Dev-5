20000 ! Replace S:\acsUB\ubCass1
20020 ! -- Creates Address Certification File:
20040 ! ______________________________________________________________________
20060 ! ______________________________________________________________________
20080   library 'S:\Core\Library': fntop,fnxit, fnerror,fncno,fnxit,fnTos,fnAcs,fnLbl,fnwait,fncsz,fnCmdSet,fntop,fngethandle
20100   on error goto Ertn
20120 ! ______________________________________________________________________
20140   dim z$,nam$*30,sta$*30,sta2$*30,csz$*30,opt1$(7),txt$*80,cap$*128
20160 ! ______________________________________________________________________
20180   fntop(program$,cap$="Export File for IMSB")
20200   fncno(cno)
20220 ! ______________________________________________________________________
20240 ! r: a screen
25000 ! /r
25020   open #h_out:=fngethandle: "Name=SAVE:text documents (*.txt) |*.txt,RecL=112,EOL=CRLF,Replace",display,output ioerr XIT
25030 F_OUT_FIXED: form pos 1,c 10,c 30,c 30,c 23,c 2,c 5,pos 112,c 1
25040 !  pr file$(h_tmp);" ";lrec(h_tmp)
25060 !  save_name$=os_filename$(file$(h_tmp))
25080 !  close #h_tmp,free:
25100 ! 
25120 ! open #h_out:=2: "Name="&dv$&"ubCass1.dat,RecL=112,EOL=None,Replace",external,output ! ioerr l170
25140   open #1: "Name=[Q]\UBmstr\Customer.h[cno],KFName=[Q]\UBmstr\ubIndex.h[cno],Shr",internal,input,keyed 
25150 F_CUSTOMER: form pos 1,c 10,pos 41,3*c 30,pos 213,pd 4,pos 296,pd 4,pos 385,pd 3,pos 1854,pd 5.2
25160   open #h_adrbil:=3: "Name=[Q]\UBmstr\ubAdrBil.h[cno],KFName=[Q]\UBmstr\AdrIndex.h[cno],Shr",internal,outIn,keyed 
25180 F_ADRBIL: form pos 11,4*c 30
26000   pr #h_out,using F_OUT_FIXED: 'Account','Name','Street Address','City','State','Zip Code'
28000 ! r:  main loop
28020   do 
28040     read #1,using F_CUSTOMER: z$,nam$,sta$,csz$,fb1,lbd,altcode eof XIT
28060     if altcode=2 then goto L350 ! alternate address coded as not being used at this time.
28100     read #h_adrbil,using F_ADRBIL,key=z$: nam$,sta$,sta2$,csz$ nokey L300
28120     if trim$(csz$)="" then csz$=sta2$ : sta2$=""
28140 L300: ! 
28160     csz$=trim$(csz$)
28180     if csz$(len(csz$)-5:len(csz$)-5) =" " then goto L330
28200     if csz$(len(csz$)-5:len(csz$)-5) < "0" or csz$(len(csz$)-5:len(csz$)-5)>"9" then 
28220       csz$=csz$(1:len(csz$)-5) &" "&csz$((len(csz$)-4):len(csz$))
28240     end if 
28260 L330: ! 
28280     fncsz(csz$,city$,state$,zip$)
28300     zip$=trim$(zip$)(1:5)
28320 L350: ! 
28340     pr #h_out,using F_OUT_FIXED: z$,nam$,sta$,city$,state$,zip$,chr$(10)
28360   loop 
28380 ! /r
29000 XIT: fnxit
30000 ! <Updateable Region: ERTN>
30020 ERTN: fnerror(program$,err,line,act$,"xit")
30040   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
30060   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
30080   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
30100 ERTN_EXEC_ACT: execute act$ : goto ERTN
30120 ! /region
