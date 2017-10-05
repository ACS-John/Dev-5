00010 ! Replace S:\acsSU\Company
00020 ! maintain company information file for checkbook management
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fncursys$,fntos,fnlbl,fnacs,fncmdset,fntxt,fncombof,fnchk,fnbutton,fnfra
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim a$(3)*40,b$(2)*12,c$*5,d(2),e$(2)*12,lastact$*12,tb$*30,actrcde$*1
00080   dim hlp$(20)*78,flh$(22)*18,cap$*128
00090   dim rpnames$(86)*20,rpnames2$(10)*6,x$(10)*20,rpscr1$(90)*24,na$(125)*8
00100   dim miscname$(10)*20,dedcode(10),dedfed(10),dedfica(10),dedst(10)
00110   dim prgl(5,3),fl3$(13),sc3$(11)*20,io3$(27)
00120   dim io1$(17),sc1$(15)*50,fl1$(16),io2$(7)*20,misc$(90)*20,d$(2)*1
00130   dim deduc(10),miscgl$(10)*12,actr$*1,reccode$*1,ar1$*1
00140   dim resp$(150)*40
00150 ! ______________________________________________________________________
00160   fntop("S:\acsSU\Company",cap$="Company Information")
00170   fncno(cno)
00180   cancel=99 : right=1 : center=2 : left=0 !:
        ccyymmdd$='3' : mmddyy$='1' : on=1 : off=0 !:
        cancel=5 : save=1 : limit_to_list=1 : pointtwo$='32' !:
        pointthree$='33'
00190   open #company=1: "Name="&env$('Q')&"\"&fncursys$&"mstr\Company.h"&str$(cno)&",Shr",internal,outin,relative ioerr BUILD_COMPANY
00200   goto READ_COMPANY
00210 ! ______________________________________________________________________
00220 READ_COMPANY: ! 
00230   read #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr$,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
00240   if actr$="0" or actr$="1" then actr=val(actr$)
00250   if uprc$(actr$)="Y" then actr=1 else !:
          if uprc$(actr$)="N" then actr=0
00260   gosub NEWSCREEN !:
        if ckey=save then gosub SAVE : goto XIT else !:
          if ckey=cancel then goto XIT !:
            ! to do it the old way change this whole line to read  GoTo Screen1
00270   if ckey=page1 then page=1 : gosub NEWSCREEN else !:
          if ckey=page2 then page=2 : gosub NEWSCREEN else !:
            if ckey=page3 then page=3 : gosub NEWSCREEN else !:
              if ckey=page4 then page=4 : gosub NEWSCREEN else !:
                if ckey=save then gosub SAVE : goto XIT else !:
                  if ckey=cancel then goto XIT !:
                    ! to do it the old way change this whole line to read  GoTo Screen1
00280 ! ______________________________________________________________________
00290 NEWSCREEN: ! 
00300   fntos(sn$='Company-Pg'&str$(page)) !:
        lc=0
00310   page1=6 : page2=07 : page3=08 : page4=09
00320   page=1 : gosub PAGE1
00330   fncmdset(4) ! Save and Cancel
00340   fnacs(sn$,0,mat resp$,ckey)
00350   if page=1 then 
00360     a$(1)=resp$(1) !:
          a$(2)=resp$(2) !:
          a$(3)=resp$(3) !:
          b$(1)=resp$(4) !:
          b$(2)=resp$(5) !:
          tb$=resp$(6) !:
          nap=val(resp$(7)) !:
          let wbc=val(resp$(8)(1:2))
00370     if resp$(9)='True' then prenum=1 else prenum=0
00380     if resp$(10)='True' then reccode=1 else reccode=0
00390   end if 
00400   if ckey=save then gosub SAVE : goto XIT else !:
          if ckey=cancel then goto XIT
00410   goto NEWSCREEN
00420 PAGE1: ! _____________________________________________________________ !:
        lc=3 : mylen=40 : mypos=mylen+2
00430   fnlbl(lc+=1,1,'Company Name:',mylen,right)
00440   fntxt(lc,mypos,40,0,left) !:
        resp$(1)=a$(1)
00450   fnlbl(lc+=1,1,'Address:',mylen,right)
00460   fntxt(lc,mypos,40,0,left) !:
        resp$(2)=a$(2)
00470   fnlbl(lc+=1,1,'City State and Zip Code:',mylen,right)
00480   fntxt(lc,mypos,40,0,left) !:
        resp$(3)=a$(3)
00490   fnlbl(lc+=1,1,'Federal Identification Number:',mylen,right)
00500   fntxt(lc,mypos,12,0,left) !:
        resp$(4)=b$(1)
00510   fnlbl(lc+=1,1,'State Identification Number:',mylen,right)
00520   fntxt(lc,mypos,12,0,left) !:
        resp$(5)=b$(2)
00530   fnlbl(lc+=1,1,'Type of Business:',mylen,right)
00540   fntxt(lc,mypos,30,0,left) !:
        resp$(6)=tb$
00550   fnlbl(lc+=1,1,'Number of Periods:',mylen,right)
00560   fntxt(lc,mypos,30,0,left,number$) !:
        resp$(7)=str$(nap)
00570   return 
00580 ! ______________________________________________________________________
00590 BUILD_COMPANY: ! 
00600   open #company=1: "Name="&env$('Q')&"\TMmstr\Company.h"&str$(cno)&",Size=0,RecL=882,Replace",internal,outin,relative 
00610   write #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,1,0,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
00620   goto READ_COMPANY
00630 ! ______________________________________________________________________
00640 SAVE: ! 
00650   rewrite #company,using 'Form POS 1,3*C 40,2*C 12,C 5,2*N 1,N 2,N 1,C 9,C 12,N 3,N 6,N 3,PD 7.2,C 30,POS 298,15*PD 4,POS 382,N 2,N 2,PD 5.3,PD 5.2,PD 5.3,PD 5.2,G 1,PD 5.3,PD 5.2,N 1,10*C 20,50*N 1,10*C 12',rec=1: mat a$,mat b$,c$,mat d,wbc,ar1,mat e$,a1,a2,a3,ucm,tb$,mat prgl,jccode,nap,ficarate,ficawage,feducrat,feducwag,actr,mcr,mcm,reccode,mat miscname$,mat dedcode,mat dedfed,mat dedfica,mat dedst,mat deduc,mat miscgl$
00660   return 
00670 ! ______________________________________________________________________
00680 XIT: ! 
00690   close #company: 
00700   fnxit
00710 ! ______________________________________________________________________
00720 ! <Updateable Region: ERTN>
00730 ERTN: fnerror(program$,err,line,act$,"xit")
00740   if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00750   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00760   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00770 ERTN_EXEC_ACT: execute act$ : goto ERTN
00780 ! /region
00790 ! ______________________________________________________________________
