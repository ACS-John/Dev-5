00010 !  Replace S:\Core\FlexDir
00020 ! puts a flex grid with a directory inside... kinda a file explorer
00030 ! ______________________________________________________________________
00040   library 'S:\Core\Library': fnopenprn,fngetdir,fnerror,fntos,fnflexadd1,fnacs,fnflexinit1,fngetcd,fnremove2,fngetpp,fnlbl,fntxt,fncmdset,fntop
00050   on error goto ERTN
00060 ! ______________________________________________________________________
00070   dim resp$(10)*255,cap$*128
00080   dim path$(2,1000)*255,prog$(2,1000)*100,ext$(2,1000)*100
00090   dim brfn$(1000)*255,brsfn$(1000)*255,dur$*200,item$(1000)*255
00100 ! ______________________________________________________________________
00110   fntop("S:\Core\FlexDir",cap$="Flex Dir")
00120   fngetcd(dur$)
00130 REFREASH: ! 
00140   mat brfn$(1000) : mat brfn$=("")
00150   if filter$="" then let filter$="*.*"
00160   fngetdir(dur$,mat brfn$," /s ",filter$)
00170   for j=1 to udim(brfn$)
00180     if brfn$(j)="" then mat brfn$(j) : goto OUT_THE_LOOP else !:
            fnremove2(dur$,brfn$(j))
00190     fngetpp(brfn$(j),path$(1,j),prog$(1,j),ext$(1,j))
00200   next j
00210 OUT_THE_LOOP: ! 
00220   fntos(sn$="flexdir") !:
        let mylen=20 : let mypos=mylen+2
00230   fnlbl(1,1,"Filter:",mylen,1)
00240   fntxt(1,mypos,18) !:
        let resp$(1)=filter$
00250   fnlbl(2,1,"Directory:",mylen,1)
00260   fntxt(2,mypos,40,255) !:
        let resp$(2)=dur$
00270   ch$(1)="File Name" : ch$(2)="Ext" : ch$(3)="Path" !:
        mat item$(3) : mat ch$(3) : mat cm$(3) !:
        fnflexinit1("loc-br",3,1,15,90,mat ch$,mat cm$,1)
00280   for j=1 to udim(brfn$)
00290     let item$(1) = prog$(1,j)
00300     let item$(2) = ext$(1,j)
00310     let item$(3) = path$(1,j)
00320     fnflexadd1(mat item$)
00330   next j
00340   fncmdset(102)
00350 L350: let fnacs(sn$,0,mat resp$,ck)
00360   if ck=5 then goto DONE
00370   let filter$=resp$(1) !:
        let dur$=resp$(2)
00380   if ck=6 then goto REFREASH
00390 ! 
00400   goto L350
00410 ! ______________________________________________________________________
00420 DONE: ! 
00430 ! 
00440   stop 
00450 ! ______________________________________________________________________
00460 ! <Updateable Region: ERTN>
00470 ERTN: let fnerror(program$,err,line,act$,"xit")
00480   if uprc$(act$)<>"PAUSE" then goto ERTN_EXEC_ACT
00490   execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00500   pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00510 ERTN_EXEC_ACT: execute act$ : goto ERTN
00520 ! /region
00530 ! ______________________________________________________________________
