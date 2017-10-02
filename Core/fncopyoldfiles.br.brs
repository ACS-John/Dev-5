00010 ! Replace S:\Core\fnCopyOldFiles
00020 ! copies files from an old system (<=3.x) into a new system (=>4.x) !:
        ! should be used right before conversion !:
        goto CALL_SELF
00030   def library fnCopyoldfiles
00040     library 'S:\Core\Library': fnerror,fncursys$,fngetcd ,fntos,fnacs,fncmdset,fnlbl,fntxt, fncno, fnmsgbox,fnxit
00050     on error goto ERTN
00060 ! ______________________________________________________________________
00070     dim mcd$*256 ! My Current Directory
00080     dim resp$(10)*300
00090     dim ml$(3)*80 ! fnMsgBox Message Lines
00091     dim path_source$*250,path_dest$*250 ! 
00100 ! ______________________________________________________________________
00110     let right=1 : let left=0 : center=2
00120     let fngetcd(mcd$) : let fncno(cno)
00130 ASK_PATHS: ! 
00140     let fntos(sn$="CopyOldFiles") !:
          let mylen=40 : let mypos=mylen+2 : let lc=0
00150     let fnlbl(lc+=1,1,"Old Program Path and Executable:",mylen,right)
00160     let fntxt(lc,mypos,50,250,left,'70') !:
          if exists(":C:\Vol002\wb.exe") then !:
            let resp$(1)="C:\Vol002\wb.exe" else !:
            let resp$(1)="C:\"
00170     let fnlbl(lc+=1,1,"Old Company Number:",mylen,right)
00180     let fntxt(lc,mypos,2,5,left,'number') !:
          let resp$(2)="1"
00190     let lc+=1
00200     let fnlbl(lc+=1,1,"New Program Path and Executable:",mylen,right)
00210     let fntxt(lc,mypos,50,250,left,'70') !:
          let resp$(3)=mcd$&"acsMenu.exe"
00220     let fnlbl(lc+=1,1,"New Company Number:",mylen,right)
00230     let fntxt(lc,mypos,5,5,left,'number') !:
          let resp$(4)="1"
00240     let fnlbl(lc+=1,100,":)")
00250     let fncmdset(2)
00260     let fnacs(sn$,0,mat resp$,ckey)
00270     if ckey=5 then let fnCopyoldfiles=5 : goto XIT !:
          else let fnCopyoldfiles=0
00280     let path_source$=resp$(1)(1:pos(resp$(1),"\",-1)-1) !:
          cno_source=val(resp$(2)) !:
          let path_dest$=resp$(3)(1:pos(resp$(3),"\",-1)-1) !:
          cno_dest=val(resp$(4)) !:
          let sys_source$=fncursys$ !:
          let sys_dest$=fncursys$
00290 ! If EXISTS(PATH_DEST$&"\"&SYS_DEST$&"mstr\Company.h"&STR$(CNO_DEST)) Then !:
          ! Mat ML$(2) !:
          ! Let ML$(1)="Please pick a Company Number that is not being used." !:
          ! Let ML$(2)="Company Number "&STR$(CNO_DEST)&" is currently being used." !:
          ! Let FNMSGBOX(MAT ML$,RESP$,"Error",0) !:
          ! Goto ASK_PATHS
00300     pr "Sy -m Copy "&path_source$&"\"&sys_source$&"mstr\*.h"&str$(cno_source)&" "&path_dest$&"\"&sys_dest$&"mstr\*.h"&str$(cno_dest) ! XXX
00310     goto XIT
00320 ! ______________________________________________________________________
00330 ! <Updateable Region: ERTN>
00340 ERTN: let fnerror(program$,err,line,act$,"xit")
00350     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00360     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00370     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00380 ERTN_EXEC_ACT: execute act$ : goto ERTN
00390 ! /region
00400 ! ______________________________________________________________________
00410 XIT: fnend 
00420 ! ______________________________________________________________________
00430 CALL_SELF: ! 
00440   library program$: fnCopyoldfiles,fnxit
00450   let fnCopyoldfiles
00460   stop 
