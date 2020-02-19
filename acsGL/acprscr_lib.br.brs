00010 ! Replace S:\acsGL\acprScr.br
00020 ! job cost screens???
00030   def library fnacprscr
00040     library 'S:\Core\Library': fntop,fnxit, fncno,fnerror,fnacglbld
00050 !
00060     fntop(program$,"CHANGE_ME")
00070     on error goto Ertn
00080 !
00090     dim a$(23)*20,floa$(23),io1$(23)
00100     dim b$(14)*20,flob$(14),io2$(14)
00110 !
00120     fncno(cno)
00130 !
00140     data "Employee #"
00150     data "Name: F/M/L"
00160     data "Address"
00170     data "City,St Zip"
00180     data "Soc. Sec. Numb"
00190     data "Gross Wages Y.T.D."
00200     data "Gross Wages Q.T.D."
00210     data "Fed W/H Y.T.D."
00220     data "Fed W/H Q.T.D."
00230     data "FICA W/H Y.T.D."
00240     data "FICA W/H Q.T.D."
00250     data "State W/H Y.T.D."
00260     data "State W/H Q.T.D."
00270     data "Local W/H Y.T.D."
00280     data "Local W/H Q.T.D."
00290     data "Misc-1 W/H Y.T.D."
00300     data "Misc-2 W/H Y.T.D."
00310     data "Tips Y.T.D."
00320     data "Tips Q.T.D."
00330     data "Weeks Worked Y.T.D."
00340     data "Weeks Worked Q.T.D."
00350     data "EIC Y.T.D."
00360     data "EIC Q.T.D."
00370     read mat a$
00380     data "Employee #"
00390     data "Date"
00400     data "Check #"
00410     data "Gross Pay"
00420     data "FICA W/H"
00430     data "Fed W/H"
00440     data "State W/H"
00450     data "Local W/H"
00460     data "Misc-1 W/H"
00470     data "Misc-2 W/H"
00480     data "Tips"
00490     data "Weeks Worked"
00500     data "EIC"
00510     data "Net Pay"
00520     read mat b$
00530     for j=1 to 5
00540       floa$(j)=str$(j+3)&",10,C 20,N"
00550       if j>1 and j<5 then io1$(j)=str$(j+3)&",32,C 25,UT,N"
00560       if j=1 then io1$(j)="4,32,N 4,UT,N"
00570       if j=5 then io1$(j)="8,32,C 11,UT,N"
00580     next j
00590     x=6
00600     for j=6 to 22 step 2
00610       x=x+1
00620       floa$(j)=str$(x+3)&",3,C 20,N"
00630       floa$(j+1)=str$(x+3)&",41,C 20,N"
00640       io1$(j)=str$(x+3)&",23,N 11.2,UT,N"
00650       io1$(j+1)=str$(x+3)&",62,N 11.2,UT,N"
00660     next j
00670     for j=1 to 14
00680       flob$(j)=str$(j+3)&",2,C 20,N"
00690       if j>3 then goto L720
00700       if j=1 then io2$(j)="4,22,N 4,UT,N" else !:
              io2$(j)=str$(j+3)&",22,N 6,UT,N"
00710       goto L730
00720 L720: io2$(j)=str$(j+3)&",22,N 11.2,UT,N"
00730 L730: next j
00740     open #1: "Name=[Q]\GLmstr\ACPRSCF.h[cno],NoShr",internal,output ioerr L760
00750     close #1,free: 
00760 L760: open #1: "Name=[Q]\GLmstr\ACPRSCF.h[cno],SIZE=0,RecL=1288,NoShr",internal,output 
00770     write #1,using 'Form POS 1,23*C 20,46*C 18': mat a$,mat floa$,mat io1$
00780     write #1,using 'Form POS 1,14*C 20,28*C 18': mat b$,mat flob$,mat io2$
00790     close #1: 
00800     open #1: "Name=[Q]\GLmstr\GLmstr.h[cno],Shr",internal,outIn ioerr L830
00810     close #1: 
00820     goto XIT
00830 L830: fnacglbld
00840     goto XIT
00850 !
00860 ! <updateable region: ertn>
00870 ERTN: fnerror(program$,err,line,act$,"xit")
00880     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00890     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00900     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00910 ERTN_EXEC_ACT: execute act$ : goto ERTN
00920 ! /region
00930 !
00940 XIT: fnend 
