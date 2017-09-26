00010 ! Replace S:\acsGL\fnacglBldS
00020 ! this -library function- builds the file    "&env$('Q')&"\GLmstr\ACGLScr.h
00030   def library fnacglblds
00040     library 'S:\Core\Library': fntop,fnxit,fnerror,fncno, fnacprscr
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim flo$(31),fli$(65),scr$(30)*20,otd$(65)*30,d(2)
00080 ! ______________________________________________________________________
00090     let fncno(cno)
00100     let fntop(program$,"CHANGE_ME")
00110     open #20: "Name="&env$('Q')&"\GLmstr\Company.h"&str$(cno)&",Shr",internal,input  !:
          read #20,using 'Form POS 150,2*N 1': mat d !:
          close #20: 
00120 ! ______________________________________________________________________
00130     let flo$(1)="1,5,C 60,R,N"
00140     if d(1)=1 then !:
            let scr$(1)="Department #:" : let scr$(2)="Account #:" !:
          else !:
            let scr$(1)=" " : let flo$(2)="3,2,C 20,N"
00150     let flo$(2)="3,2,C 20,N" : let flo$(3)="3,20,C 20,N"
00160     if d(2)=1 then !:
            let scr$(3)="Sub-Account:" : let flo$(4)="3,41,C 20,N" !:
          else !:
            let scr$(3)=" " : let flo$(4)="3,41,C 20,N"
00170     let scr$(4)="Description:" !:
          let scr$(5)="Beginning Balance:" !:
          let scr$(6)="Current Balance:" !:
          let scr$(7)="Balance Sheet Ref #:" !:
          let scr$(8)="2nd Balance Sheet:" !:
          let scr$(9)="Income Stmt Ref #:"
00180     let flo$(5)="04,02,C 20,N" : let flo$(6)="05,02,C 20,N" !:
          let flo$(7)="05,36,C 20,N" : let flo$(8)="06,02,C 20,N" !:
          let flo$(9)="06,41,C 20,N"
00190     let flo$(10)="07,02,C 20,N" : let flo$(11)="07,41,C 20,N" !:
          let flo$(12)="08,02,C 20,N" : let flo$(13)="08,41,C 20,N" !:
          let flo$(14)="10,08,C 20,N" : let flo$(15)="10,26,C 20,N" !:
          let flo$(16)="10,46,C 20,N" : let flo$(17)="10,64,C 20,N"
00200     let scr$(10)="2nd Income Stmt:" !:
          let scr$(11)="Fund/Cash Flow Ref#:" !:
          let scr$(12)="2nd Fund/Cash Flow:" !:
          let scr$(13)="Balance This Yr:" !:
          let scr$(14)="Balance Last Yr:" !:
          let scr$(15)="Original Budget:" !:
          let scr$(16)="Revised Budget:"
00210     for j=17 to 29 !:
            let scr$(j)="Period "&str$(j-16)&":" !:
            let flo$(j+1)=str$(j-6)&",2,C 20,N" !:
          next j
00220     let scr$(30)="EOY Bal 2 Yrs Ago:"
00230     let flo$(31)="09,02,C 20,N"
00240     if d(1)=1 then !:
            let fli$(1)="3,15,N 3,UT,N" : let otd$(1)=fli$(1) !:
          else !:
            let fli$(1)="3,15,N 3,PA,N" : let otd$(1)="3,15,PIC(ZZZ),N"
00250     let otd$(2)="3,30,PIC(ZZZZZZ),UT,N"
00260     if d(2)=1 then !:
            let fli$(3)="3,55,N 3,UET,N" : let otd$(3)=fli$(3) !:
          else !:
            let fli$(3)="3,55,N 3,PA,N" : let otd$(3)="3,55,PIC(ZZZ),N"
00270     let fli$(04)="4,15,C 50,UT  ,N" : let otd$(04)=fli$(04)
00280     let otd$(05)="5,21,PIC(---------.##),UT,N" !:
          let otd$(06)="5,54,PIC(---------.##),UT,N" !:
          let otd$(07)="6,23,PIC(ZZZZZ),UT,N" !:
          let otd$(08)="6,62,PIC(ZZZZZ),UT,N" !:
          let otd$(09)="7,23,PIC(ZZZZZ),UT,N" !:
          let otd$(10)="7,62,PIC(ZZZZZ),UT,N" !:
          let otd$(11)="8,23,PIC(ZZZZZ),UT,N" !:
          let otd$(12)="8,62,PIC(ZZZZZ),UT,N" !:
          let otd$(13)="9,23,PIC(---------.##),UT,N"
00290     let fli$(05)="5,21,N 12.2,UT,N" !:
          let fli$(06)="5,54,N 12.2,UT,N" !:
          let fli$(07)="6,23,N 5,QUT,51" !:
          let fli$(08)="6,62,N 5,QUT,52" !:
          let fli$(09)="7,23,N 5,QUT,53" !:
          let fli$(10)="7,62,N 5,QUT,54" !:
          let fli$(11)="8,23,N 5,QUT,55" !:
          let fli$(12)="8,62,N 5,QUT,56" !:
          let fli$(13)="9,23,N 12.2,UT,N"
00300     for j=0 to 12
00310       let fli$(j*4+14)=str$(j+11)&",12,N 12.2,UT,N" !:
            let fli$(j*4+15)=str$(j+11)&",30,N 12.2,UT,N" !:
            let fli$(j*4+16)=str$(j+11)&",50,N 12.2,UT,N" !:
            let fli$(j*4+17)=str$(j+11)&",67,N 12.2,UT,N"
00320       let otd$(j*4+14)=str$(j+11)&",12,PIC(---------.##),UT,N" !:
            let otd$(j*4+15)=str$(j+11)&",30,PIC(---------.##),UT,N" !:
            let otd$(j*4+16)=str$(j+11)&",50,PIC(---------.##),UT,N" !:
            let otd$(j*4+17)=str$(j+11)&",67,PIC(---------.##),UT,N"
00330     next j
00340     let fli$(2)="3,30,N 6,UT  ,N"
00350     if d(1)=0 then let fli$(2)(11:11)="C"
00360     if d(2)=0 then let fli$(2)(12:12)="E"
00370     execute "Free "&env$('Q')&"\GLmstr\ACGLScr.h"&str$(cno)&" -n" ioerr L380
00380 L380: open #20: "Name="&env$('Q')&"\GLmstr\ACGLScr.h"&str$(cno)&",Size=0,RecL=4281",internal,output: write #20,using 'Form POS 1,31*C 15,30*C 20,65*C 18,65*C 30': mat flo$,mat scr$,mat fli$,mat otd$ !:
          close #20: 
00390     let fnacprscr
00400     goto XIT
00410 ! ______________________________________________________________________
00420 ERTN: ! <Updateable Region: ERTN>
00430     let fnerror(program$,err,line,act$,"xit")
00440     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00450     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00460     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00470 ERTN_EXEC_ACT: execute act$ : goto ERTN
00480 ! /region
00490 ! ______________________________________________________________________
00500 XIT: fnend 
00510 ! ______________________________________________________________________
