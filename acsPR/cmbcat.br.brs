00010 ! Replace S:\acsPR\CmbCat.br
00020 ! creates a screen ace combobox for category records
00030   def library fncmbcat(myline,mypos; addall,container,indexfile$*200)
00040 ! ______________________________________________________________________
00050     library 'S:\Core\Library': fncno,fncombof,fnerror
00060     on error goto Ertn
00070 ! ______________________________________________________________________
00080     dim df$*200,if$*200
00090 ! ______________________________________________________________________
00100     if addall<>1 then addall=0
00110     fncno(cno)
00120     if addall=0 then fen$="Ccat.h[cno]" else !:
            fen$="CcatALL.h[cno]"
00130     if indexfile$="" then if$="[Q]\PRmstr\catindx.h[cno]" else !:
            if$=indexfile$
00140     fncombof(fen$,myline,mypos,43,"[Q]\PRmstr\jccat.h[cno]",1,11,12,25,if$,1+addall,1,"Select from the list of categories. To add a category record, go to the Category File.",container)
00150     indexfile$=""
00160     goto XIT
00170 ! ______________________________________________________________________
00180 ! <Updateable Region: ERTN>
00190 ERTN: fnerror(program$,err,line,act$,"xit")
00200     if lwrc$(act$)<>"pause" then goto ERTN_EXEC_ACT
00210     execute "List -"&str$(line) : pause : goto ERTN_EXEC_ACT
00220     pr "PROGRAM PAUSE: Type GO and press [Enter] to continue." : pr "" : pause : goto ERTN_EXEC_ACT
00230 ERTN_EXEC_ACT: execute act$ : goto ERTN
00240 ! /region
00250 ! ______________________________________________________________________
00260 XIT: fnend 
00270 ! ______________________________________________________________________
