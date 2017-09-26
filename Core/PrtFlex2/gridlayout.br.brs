00010 ! Replace Core\PrtFlex\GridLayout
00020 ! layout for grid program
00030 ! ______________________________________________________________________
00040   library 'Core\Library': fnacs,fnlbl,fntxt,fntos,fncmdset,fnerror,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(6)*60,text$*50,outputfile$*60,fieldnam$*30,vn$*20,ft$*11,an$*20
00080 ! ______________________________________________________________________
00090   let fntop("Core\PrtFlex\GridLayout","Grid Layout")
00100 L100: let fntos(sn$="file_layout") !:
        let lablen=15
00110   let fnlbl(1,1,"File Name:",lablen,1)
00120   let fntxt(1,lablen+2,60,0,0,"") !:
        let resp$(1)="programfolder\grid\data_base_name\filename"
00130   let fncmdset(2)
00140   let fnacs(sn$,0,mat resp$,ckey)
00150   let outputfile$=trim$(resp$(1))&".fil"
00160   open #10: "Name="&outputfile$&",RecL=87,Use",display,output ioerr L100
00170 L170: let fntos(sn$="file_layout") !:
        let lablen=30
00180   let fnlbl(1,1,"Field name:",lablen,1)
00190   let fntxt(1,lablen+2,30,0,0,"") !:
        let resp$(1)=""
00200   let fnlbl(2,1,"Variable Name:",lablen,1)
00210   let fntxt(2,lablen+2,20,0,0,"") !:
        let resp$(1)=""
00220   let fnlbl(3,1,"Field Length:",lablen,1)
00230   let fntxt(3,lablen+2,4,0,0,"20") !:
        let resp$(1)=""
00240   let fnlbl(4,1,"# of Decimal Positions:",lablen,1)
00250   let fntxt(4,lablen+2,2,0,0,"20") !:
        let resp$(1)=""
00260   let fnlbl(5,1,"Format (eg. C 30,pd 4.2):",lablen,1)
00270   let fntxt(5,lablen+2,11,0,0,"") !:
        let resp$(5)=""
00280   let fnlbl(6,1,"Abbreviated Name:",lablen,1)
00290   let fntxt(6,lablen+2,20,0,0,"") !:
        let resp$(6)=""
00300   let fncmdset(11)
00310   let fnacs(sn$,0,mat resp$,ckey)
00320   if ckey=5 then goto XIT
00330   let fieldnam$=trim$(resp$(1))
00340   let vn$=trim$(resp$(2))
00350   let fl=val(resp$(3))
00360   let dp=val(resp$(4))
00370   let ft$=trim$(resp$(5))
00380   let an$=trim$(resp$(6))
00390   print #10,using 'Form POS 1,C 30,C 20,N 4,N 2,C 11,C 20': fieldnam$,vn$,fl,dp,ft$,an$
00400   goto L170
00410 ! ______________________________________________________________________
00420 XIT: stop 
00430 ! ______________________________________________________________________
00440 ! <Updateable Region: ERTN>
00450 ERTN: let fnerror(program$,err,line,act$,"xit")
00460   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00470   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00480   print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00490 ERTN_EXEC_ACT: execute act$ : goto ERTN
00500 ! /region
00510 ! ______________________________________________________________________
