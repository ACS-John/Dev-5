00010 ! Replace R:\acsGL\Conversion\glstdad_v0_to_v1
00020 ! converts general ledger standard adjustment files to new rln
00030   let fnglstdad_v0_to_v1
00040   def fnglstdad_v0_to_v1
00050     library 'R:\Core\Library': fntop,fnxit, fnerror,fnwait,fnmsgbox,fncno,fngethandle
00060     on error goto ERTN
00070 ! ______________________________________________________________________
00080     dim cnam$*40,cap$*128,message$*40,msgline$(6)*48,response$(5)*1,ref$*12,desc$*30,gln$(10)*12,gla(10)
00090     let fncno(cno)
00100 ! 1: Check for old file; exit if does not exist
00110     if exists("Q:\GLmstr\glstdadj.h"&str$(cno))=0 then goto XIT
00120 ! mat msgline$=("") : msgline$(1)="Warning: this action will replace your" : msgline$(2)="existing Standard Adjustments with data" : msgline$(3)="imported from the DOS version.  Are you sure?" : cap$="Continue?"
00130 ! let fnmsgbox(mat msgline$,resp$,cap$,1)
00140 ! if resp$<>"OK" then goto XIT
00150     open #1: "Name=Q:\GLmstr\glstdadj.h"&str$(cno),internal,input 
00160     open #2: "Name=Q:\GLmstr\glstdad.h"&str$(cno)&",Version=1,Replace,RecL=59",internal,output 
00170 ! 2: Copy old file to new file
00180     do 
00190       read #1,using "FORM C 12,C 30,10*C 12,10*PD 5.2": ref$,desc$,mat gln$,mat gla eof DELETE_FILE
00200       for j=1 to 10
00210         if gla(j)<>0 then 
00220           write #2,using "FORM C 12,C 30,C 12,PD 5.2": ref$,desc$,gln$(j),gla(j)
00230         end if 
00240       next j
00250     loop 
00260 ! 3: Delete old file
00270     execute "FREE Q:\GLmstr\glstdadj.h"&str$(cno)
00280 ! 5: Index new file
00290     execute "Index Q:\GLmstr\glstdad.H"&str$(cno)&" Q:\GLmstr\glstdidx.h"&str$(cno) &" 1 12 Replace,DupKeys"
00291 DELETE_FILE: close #1: 
00292     close #2: 
00300     goto XIT
00310 ! ______________________________________________________________________
00320 ! <Updateable Region: ERTN>
00330 ERTN: let fnerror(cap$,err,line,act$,"xit")
00340     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00350     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00360     print "PROGRAM PAUSE: Type GO and press [Enter] to continue." : print "" : pause : goto ERTN_EXEC_ACT
00370 ERTN_EXEC_ACT: execute act$ : goto ERTN
00380 ! /region
00390 ! ______________________________________________________________________
00400 XIT: let fnxit
00401   fnend 
00410 ! ______________________________________________________________________
